"use strict";

var importer = {};

importer.importHandler = function(e) {
    ga('send','event','import','begin');
    var file = e.target.files[0];
    if (!file) return;

    if ( file.type == "application/zip" ) {
        ga('send','event','import','error', 'zip');
        $("#import_error_stats").text('Zip files are not supported. Only text files can be imported.');
        $("#import_complete").show();
        return;
    }

    utils.logInfo("import: begin");
    importer.startImportProgress();

    var reader = new FileReader();

    var chunkSize = 1 * 1024 * 1024;  // 1 MB per chunk
    var start = 0;
    var stop = chunkSize;
    var numChunks = Math.ceil(file.size / chunkSize);
    var chunkIndex = 0;

    var importState = { 
        lastIncompleteLine: null,
        numParsed: 0,
        numErrors: 0,
        urlsInserted: 0,
        visitsInserted: 0,
        visitsIgnored: 0,
        fileType: null
    };

    reader.onload = function(e) {
        ++chunkIndex;    

        var onProcessedCallback = importer.onImportDone;
        if (stop < file.size) {
            onProcessedCallback = function() {
                var percent = Math.round( (chunkIndex / numChunks) * 100 );
                importer.updateImportProgress(percent);
                utils.logInfo( "import: " + percent.toString() + "%" );

                start = stop;
                stop = stop + chunkSize;

                blob = file.slice(start, stop);
                reader.readAsText(blob, "UTF-8");
            };
        }

        var results = importer.parseChunk(e.target.result, importState);
        if (results.urlCount) {
            importer.saveChunk(results, importState, onProcessedCallback);
        }
        else {
            onProcessedCallback(importState);
        }

    };

    websql.init();

    var blob = file.slice(start, stop);
    reader.readAsText(blob, "UTF-8");
}


importer.splitChunkRegex = function() {
    return new RegExp('^.*(?:[\r\n]+|$)', 'gm');
}


importer.parseChunk = function(text, importState) {
    var lines = text.match( importer.splitChunkRegex() );
    var lineCount = lines.length;

    if (lines[lineCount - 1] == "") {
        --lineCount;    // ignore last line of file, which is empty string
    }

    var results = {
        urls: {},   // maps urls to their visits and title
        urlCount: 0,
    };

    for (var i = 0; i < lineCount; i++) {
        var rawLine = lines[i];
        var isFirstLine = (i == 0);
        var isLastLine = (i == lineCount - 1);

        // If a CR/LF line is split between the two, we can safely ignore the line containing solely a LF
        if (!importState.lastIncompleteLine && rawLine.length == 1 && rawLine == "\n") {
            continue;
        }

        if (isFirstLine && importState.lastIncompleteLine) {
            // prepend the last incomplete line before parsing
            rawLine = importState.lastIncompleteLine + rawLine;
            importState.lastIncompleteLine = null;
        }
        
        var parsedLine = importer.parseLine(rawLine);

        if (!importState.fileType) {
            importState.fileType = parsedLine.fileType;
        }

        if (parsedLine.error) {
            if (isLastLine && parsedLine.error == "MISSING_LINEBREAK") {
                importState.lastIncompleteLine = rawLine;
            }
            else {
                utils.logError("import: " + parsedLine.errorMsg);
                ++importState.numErrors;
            }
        }
        else {
            if (importState.lastIncompleteLine) {
                utils.logWarning("import: unexpected lastIncompleteLine: " + importState.lastIncompleteLine + ", numParsed = " + importState.numParsed);
            }

            ++importState.numParsed;

            if (!results.urls[ parsedLine.url ]) {
                results.urls[ parsedLine.url ] = {
                    "visits": [],
                    "title": null,
                };
                ++results.urlCount;
            }

            var result = results.urls[ parsedLine.url ];
            result.visits.push({ visitTime: parsedLine.visitTime, transition: parsedLine.transition });

            if (result.title == null && parsedLine.title != null) {
                result.title = parsedLine.title;
            }
        }
    }

    return results;
}


importer.saveChunk = function(results, importState, onProcessedCallback) {
    websql.db.transaction(function(tx) {
        var urlIdsFound = 0;
        var seenHostnames = {};     // maps hostnames to root domains

        for (var url in results.urls) {
            var insertCallback = function(url, title) {
                return function(tx, rs) {
                    if (rs.rowsAffected) {
                        ++importState.urlsInserted;
                        tx.executeSql(
                            "INSERT OR IGNORE INTO search_urls (docid, url, title) VALUES (?, ?, ?)", 
                            [rs.insertId, url, title], 
                            websql.onSuccess, 
                            websql.onError
                        );
                    }
                };
            };

            var host = utils.extractHost(url);
            var root_domain;
            if (seenHostnames[ host ]) {
                root_domain = seenHostnames[ host ];
            }
            else {
                root_domain = utils.getDomainPortion(host);
                seenHostnames[ host ] = root_domain;
            }

            var title = results.urls[url].title;
            if (title == "") {
                title = null;
            }

            tx.executeSql(
                "INSERT OR IGNORE INTO urls (url, host, root_domain, title) VALUES (?, ?, ?, ?)",
                [url, (host === undefined ? null : host), (root_domain === undefined ? null : root_domain), title],
                insertCallback(url, title),
                websql.onError
            );

            var getUrlId = function(url) {
                return function(tx, rs) {
                    ++urlIdsFound;
                    results.urls[url].urlid = rs.rows.item(0).urlid;
                    if (urlIdsFound == results.urlCount) {
                        importer.insertVisits(results, importState, onProcessedCallback);
                    }
                };
            };

            tx.executeSql(
                "SELECT urlid FROM urls WHERE url=?", 
                [url], 
                getUrlId(url), 
                websql.onError
            );
        }
    });
}


importer.parseLine = function(lineInput) {
    var result = {
        url: null,
        visitTime: null,
        transition: null,
        error: null,
        errorMsg: null,
        title: null,
        fileType: null
    };

    if (!/[\r\n]+$/.test(lineInput)) {
        result.error = "MISSING_LINEBREAK";
        result.errorMsg = "no linebreaks for line: '" + lineInput + "'";
        return result;
    }

    var line = lineInput.replace(/[\r\n]+$/, '');
    var columns = line.split("\t");

    if (columns.length != 3 && columns.length != 4 && columns.length != 8) {
        result.error = "WRONG_COLUMN_COUNT";
        result.errorMsg = "wrong number of columns (" + columns.length + ") for line: '" + line + "'";
        return result;
    }

    var url;
    var visitTime;
    var transition;
    var title;

    if (columns.length == 3 || columns.length == 4) {
        // 3 cols == "Archived History" format before page title was supported.
        // 4 cols == "Export History" format which includes page title.
        url = columns[0];
        visitTime = columns[1];
        transition = columns[2];
        title = columns.length == 4 ? columns[3] : null;
    }
    else if (columns.length == 8) {
        // 8 cols == "Export Raw Data" format so users can do their own analysis.
        url = columns[0];
        visitTime = columns[3];
        transition = columns[6];
        title = columns[7];
    }

    var errors = [];

    if (!importer.isValidUrl(url)) {
        errors.push("url");
    }

    if (!importer.isValidVisitTime(visitTime)) {
        errors.push("visitTime");
    }

    if (!importer.isValidTransition(transition)) {
        errors.push("transition");
    }

    if (!importer.isValidTitle(title)) {
        errors.push("title");
    }

    if (errors.length) {
        result.error = "INVALID_FIELD";
        result.errorMsg = "Invalid " + errors.join(", ") + " for line '" + line + "'";
        return result;
    }

    var fileType = columns.length + 'col';

    // For cols=8 format, time is already in unix time and transition is already text.
    if (columns.length != 8) {
        // If visitTime begins with a "U", it's already in unix epoch time.
        if ( visitTime.match(/^U/) ) {
            fileType = fileType + '_unix';
            visitTime = parseFloat( visitTime.replace("U","") );
        }
        // Otherwise, it's in windows epoch time.
        else {
            fileType = fileType + '_win';
            visitTime = utils.convertToUnixEpoch(visitTime);
        }
        transition = utils.convertTransitionToText(transition);
    }
    else {
        visitTime = parseFloat(visitTime);
    }

    result.url = url;
    result.visitTime = visitTime;
    result.transition = transition;
    result.title = title;
    result.fileType = fileType;

    return result;
}

importer.isValidUrl = function(url) {
    if (typeof(url) == "string") {
        return url.length > 0;
    }
    return;
}

importer.isValidVisitTime = function(time) {
    if (typeof(time) == "string") {
        return /^U?\d+\.?\d*$/.test(time);
    }
    return;
}

importer.isValidTransition = function(transition) {
    if (typeof(transition) == "string") {
        return /^(?:-?\d+|[a-z_]+)$/i.test(transition);
    }
    return;
}

// Accept anything for title.
importer.isValidTitle = function(title) {
    return true;
}

importer.insertVisits = function(results, importState, onProcessedCallback) {
    utils.logInfo("import: insertVisits called for " + results.urlCount + " url(s)");

    var insertSql = 
        "INSERT OR IGNORE INTO visits (" +
            "urlid, visit_time, visit_date, year, month, month_day, week_day, hour, transition_type" +
        ") VALUES (" +
            "    ?,          ?,          ?,    ?,     ?,         ?,        ?,    ?,               ?" +
        ")"
    ;

    websql.db.transaction(function(tx) {
        var urlCount = 0;

        for (var url in results.urls) {
            ++urlCount;
            var urlInfo = results.urls[url];
            var isLastUrl = (urlCount == results.urlCount);

            for (var i = 0; i < urlInfo.visits.length; i++) {
                var visitInfo = urlInfo.visits[i];
                var visitDate = new Date(visitInfo.visitTime);
                var dateString = utils.dateToString(visitDate);
                var isLastVisit = (i == urlInfo.visits.length - 1);

                var visitCallback = function(importState, isLastUrl, isLastVisit) {
                    return function(tx, rs) {
                        if (rs.rowsAffected) {
                            ++importState.visitsInserted;
                        }
                        else {
                            ++importState.visitsIgnored;
                        }

                        if (isLastUrl && isLastVisit) {
                            onProcessedCallback(importState);
                        }
                    };
                };

                tx.executeSql(
                    insertSql,
                    [
                        urlInfo.urlid, visitInfo.visitTime, dateString, visitDate.getFullYear(), visitDate.getMonth(),
                        visitDate.getDate(), visitDate.getDay(), visitDate.getHours(), visitInfo.transition
                    ],
                    visitCallback(importState, isLastUrl, isLastVisit),
                    websql.onError
                );
            }
        }
    });
}

importer.onImportDone = function(importState) {
    utils.logInfo("import: grand totals: " + importState.numParsed + " parsed, " + importState.numErrors + " errors");
    utils.logInfo("import: grand totals: " 
            + importState.urlsInserted + " urls inserted, " 
            + importState.visitsInserted + " visits inserted, "
            + importState.visitsIgnored  + " visits ignored"
    );

    ga('send','event','import','end','completed');
    ga('send','event','import','type',importState.fileType);
    document.getElementById("import_progress").style.display = "none";
    document.getElementById("import_success_stats").innerHTML = chrome.i18n.getMessage(
        "import_stats", 
        [
            globalPrefs.getNumberDisplay( importState.urlsInserted ), 
            globalPrefs.getNumberDisplay( importState.visitsInserted )
        ]
    );

    if (importState.visitsIgnored) {
        ga('send','event','import','end','ignored');
        document.getElementById("import_ignored_stats").innerHTML = chrome.i18n.getMessage(
            "import_ignored_stats",
            [globalPrefs.getNumberDisplay( importState.visitsIgnored )]
        );
    }

    if (importState.numErrors) {
        ga('send','event','import','end','error');
        document.getElementById("import_error_stats").innerHTML = chrome.i18n.getMessage(
            "import_errors",
            [globalPrefs.getNumberDisplay( importState.numErrors )]
        );
    }

    document.getElementById("import_complete").style.display = "block";
}

importer.startImportProgress = function() {
    document.getElementById('import_progress').style.display = "block";

    // Hide the Import completion div, and empty the stats.
    document.getElementById('import_complete').style.display = "none";
    document.getElementById('import_success_stats').innerHTML = '';
    document.getElementById('import_ignored_stats').innerHTML = '';
    document.getElementById('import_error_stats').innerHTML = '';

    importer.updateImportProgress(0);
}

importer.updateImportProgress = function(percent) {
    document.getElementById("import_percent").innerHTML = chrome.i18n.getMessage("import_progress", [globalPrefs.getNumberDisplay(percent)]);
}



