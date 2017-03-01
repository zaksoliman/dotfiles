"use strict";

var websql = {};
websql.db = null;

websql.onError = function(tx, e) {
    utils.logError(e.message);
};

websql.onTxnError = function(e) {
    utils.logError(e.message);
};

websql.onSuccess = function(tx, rs) {};

websql.init = function() {
    websql.db = openDatabase("history_trends", "1.0", "History Trends Unlimited", 5 * 1024 * 1024);

    // Initialize database
    websql.db.transaction(function(tx) {
        tx.executeSql(
            "CREATE TABLE IF NOT EXISTS urls(" +
                "urlid INTEGER PRIMARY KEY ASC" +
                ",url TEXT" +
                ",host TEXT" +
                ",root_domain TEXT" +
                ",title TEXT" +
            ")",
            [],
            websql.onSuccess,
            websql.onError
        );
        tx.executeSql(
            "CREATE TABLE IF NOT EXISTS visits(" + 
                "visitid INTEGER PRIMARY KEY ASC " +
                ",urlid INTEGER " +
                ",visit_time INTEGER " +
                ",visit_date TEXT " +
                ",year INTEGER " +
                ",month INTEGER " +
                ",month_day INTEGER " +
                ",week_day INTEGER " + 
                ",hour INTEGER " + 
                ",transition_type TEXT" +
            ")",
            [],
            websql.onSuccess,
            websql.onError
        );
        tx.executeSql("CREATE UNIQUE INDEX IF NOT EXISTS urls_url ON urls(url)", [], websql.onSuccess, websql.onError);
        tx.executeSql("CREATE INDEX IF NOT EXISTS urls_host ON urls(host)", [], websql.onSuccess, websql.onError);
        tx.executeSql("CREATE INDEX IF NOT EXISTS urls_root_domain ON urls(root_domain)", [], websql.onSuccess, websql.onError);
        tx.executeSql("CREATE UNIQUE INDEX IF NOT EXISTS visits_urlid_visit_time ON visits(urlid, visit_time)", [], websql.onSuccess, websql.onError);
        tx.executeSql("CREATE INDEX IF NOT EXISTS visits_visit_time ON visits(visit_time)", [], websql.onSuccess, websql.onError);
        tx.executeSql("CREATE INDEX IF NOT EXISTS visits_visit_date ON visits(visit_date)", [], websql.onSuccess, websql.onError);

        // Add title column if it doesn't exist (v1.0.8 and earlier).
        tx.executeSql(
            "SELECT title FROM urls LIMIT 1", 
            [], 
            websql.onSuccess,
            function(tx, error) {
                tx.executeSql("ALTER TABLE urls ADD COLUMN title TEXT", [], websql.onSuccess, websql.onError);
            }
        );

        // "IF NOT EXISTS" does not work with virtual tables in WebSQL's version of SQLite.
        tx.executeSql(
            "SELECT docid FROM search_urls LIMIT 1",
            [],
            websql.onSuccess,
            // Create the table if it does not exist.
            function(tx, error) {
                tx.executeSql(
                    "CREATE VIRTUAL TABLE search_urls USING fts3(url, title, tokenize=simple)",
                    [],
                    // Populate the table if it was just created (supports v1.0.9 and earlier)
                    function(tx, error) {
                        tx.executeSql(
                            "INSERT INTO search_urls (docid, url, title) SELECT urlid, url, title FROM urls",
                            [],
                            websql.onSuccess,
                            websql.onError
                        );
                    },
                    websql.onError
                );
            }
        );
    });
};

websql.initAndSync = function(onSyncComplete) {
    if (!websql.db) {
        websql.updateSyncProgress("Initializing...");
        websql.init();
    }
    websql.updateSyncProgress("Saving history...");
    websql.db.transaction(function(tx) {
        // Begin syncing history
        tx.executeSql(
            "SELECT COUNT(*) AS visit_count, IFNULL(MAX(visit_time),0) AS max_visit_time FROM visits", 
            [], 
            function(tx, rs) {
                var maxVisitTime = rs.rows.item(0).max_visit_time;
                var visit_count = rs.rows.item(0).visit_count;
                utils.logInfo("Starting visit count is " + visit_count);
                websql.syncHistory(maxVisitTime, onSyncComplete);
            }, 
            websql.onError
        );
    });
};

websql.syncHistory = function(maxVisitTime, onSyncComplete) {
    // Track the number of callbacks from chrome.history.getVisits()
    // that we expect to get.  When it reaches zero, we have all results.
    var numRequestsOutstanding = 0;
    var startTime = maxVisitTime + 1;
    utils.logInfo("Syncing visits since " + (new Date(startTime).toString()));

    chrome.history.search({
        'text': '',                // Return every history item....
        'maxResults': 1000000000,  // Have to specify something, so why not a billion?
        'startTime': startTime
    },
    function(historyItems) {
        if (historyItems.length) {
            websql.db.transaction(function(tx) {
                syncUrlsWithTx(tx, historyItems);
            });
        }
        else {
            utils.logInfo("No history items found to sync");
            onSyncComplete();
        }
    });

    var syncUrlsWithTx = function(tx, historyItems) {
        var urls = [];
        var urlsInserted = 0;
        var numItems = historyItems.length;

        for (var i = 0; i < numItems; ++i) {
            var url = historyItems[i].url;
            var title = utils.formatTitle(historyItems[i].title);
            var host = utils.extractHost(url);
            var root_domain = utils.getDomainPortion(host);
            var isLastUrl = (i == numItems - 1);

            if (title == "") {
                title = null;
            }

            var insertCallback = function(url, title) {
                return function(tx, rs) {
                    if (rs.rowsAffected) {
                        ++urlsInserted; 
                        tx.executeSql(
                            "INSERT OR IGNORE INTO search_urls (docid, url, title) VALUES (?, ?, ?)", 
                            [rs.insertId, url, title], 
                            websql.onSuccess, 
                            websql.onError
                        );
                    }
                };
            };
            tx.executeSql(
                "INSERT OR IGNORE INTO urls (url, host, root_domain, title) VALUES (?, ?, ?, ?)", 
                [url, (host === undefined ? null : host), (root_domain === undefined ? null : root_domain), title], 
                insertCallback(url, title),
                websql.onError
            );

            var getUrlId = function(url, isLastUrl, titleFromHistory) {
                return function(tx, rs) {
                    var row = rs.rows.item(0);
                    var urlid = row.urlid;
                    var titleFromDb = row.title;
                    urls.push({ urlid: urlid, url: url });
                    if (titleFromDb != titleFromHistory) {
                        utils.logInfo("Updating title for urlid=" + urlid);
                        tx.executeSql("UPDATE urls SET title=? WHERE urlid=?", [titleFromHistory, urlid], websql.onSuccess, websql.onError);
                        tx.executeSql("UPDATE search_urls SET title=? WHERE docid=?", [titleFromHistory, urlid], websql.onSuccess, websql.onError);
                    }
                    if (isLastUrl) {
                        utils.logInfo("URLS: " + numItems + " found. " + urlsInserted + " inserted.");
                        findVisits(urls);
                    }
                };
            };

            tx.executeSql("SELECT urlid, title FROM urls WHERE url=?", [url], getUrlId(url, isLastUrl, title), websql.onError);
        }
    };
  
    var findVisits = function(urls) {
        for (var i = 0; i < urls.length; i++) {
            var generateProcessVisitsForUrl = function(urls, i) {
                return function(visitItems) {
                    processVisitsForUrl(urls, i, visitItems);
                };
            };
            chrome.history.getVisits({url: urls[i].url}, generateProcessVisitsForUrl(urls, i));
            numRequestsOutstanding++;
        }
    };

    // Callback for chrome.history.getVisits().
    var processVisitsForUrl = function(urls, i, visitItems) {
        urls[i].visitItems = [];

        for (var j = 0; j < visitItems.length; j++) {
            // only keep visits after the maxVisitTime
            if (visitItems[j].visitTime > maxVisitTime) {
                urls[i].visitItems.push(visitItems[j]);
            }
        }
        if (!--numRequestsOutstanding) {
            syncAllVisits(urls);
        }
    };

    var syncAllVisits = function(urls) {
        var insertSql = 
            "INSERT OR IGNORE INTO visits (" +
                "urlid, visit_time, visit_date, year, month, month_day, week_day, hour, transition_type" +
            ") VALUES (" +
                "    ?,          ?,          ?,    ?,     ?,         ?,        ?,    ?,               ?" +
            ")"
        ;

        websql.db.transaction(function(tx) {
            var totalVisits = 0;
            var totalVisitsInserted = 0;

            for (var i = 0; i < urls.length; i++) {
                var urlItem = urls[i];
                totalVisits += urlItem.visitItems.length;
                var isLastUrl = (i == urls.length - 1);

                for (var j = 0; j < urlItem.visitItems.length; j++) {
                    var isLastVisit = (j == urlItem.visitItems.length - 1);

                    var getSuccessCallback = function(isLastUrl, isLastVisit) {
                        return function(tx, rs) {
                            if (rs.rowsAffected) {
                                ++totalVisitsInserted;
                            }
                            if (isLastUrl && isLastVisit) {
                                utils.logInfo("VISITS: " + totalVisits + " found. " + totalVisitsInserted + " inserted.");
                                onSyncComplete();
                            }
                        };
                    };

                    var visitItem = urlItem.visitItems[j];
                    var visitDate = new Date(visitItem.visitTime);
                    var dateString = utils.dateToString(visitDate);

                    tx.executeSql(
                        insertSql,
                        [
                            urlItem.urlid, visitItem.visitTime, dateString, visitDate.getFullYear(), visitDate.getMonth(),
                            visitDate.getDate(), visitDate.getDay(), visitDate.getHours(), visitItem.transition
                        ],
                        getSuccessCallback(isLastUrl, isLastVisit),
                        websql.onError
                    );
                }
            }
        });
    };
};


websql.calcTrends = function(buildDomCallback) {
    websql.updateSyncProgress("Analyzing history... 0%");

    var trends = {
        "historyItems": 0, 
        "visitItems": 0,
        "byTransition": {},
        "byMinute": [],
        "byHour": [],
        "byDay": [],
        "byDayOfMonth": [],
        "byMonth": [],
        "byYear": {},
        "byBusiestDay": {},
        "topDays": [],
        "topUrls": [],
        "topDomains": [],
        "topUniqueVisits": []
    };
  
    // Mass initialization
    for (var i = 0; i < 60; i++)  { trends.byMinute[i] = 0; }
    for (var i = 0; i < 24; i++)  { trends.byHour[i]   = 0; }
    for (var i = 0; i < 7; i++)   { trends.byDay[i]    = 0; }
    for (var i = 1; i <= 31; i++) { trends.byDayOfMonth[i] = 0; }
    for (var i = 0; i < 12; i++)  { trends.byMonth[i]      = 0; }

    var whereInfo = websql.buildWhereClause();
    var whereSql = whereInfo.sql;
    var whereBinds = whereInfo.binds;
    var fromSql = " FROM urls u INNER JOIN visits v ON u.urlid = v.urlid ";

    var numReadRequests;
    var calculationFunctions = [
        // Visit count
        function(tx) {
            var sql = "SELECT COUNT(*) AS visit_count " + fromSql + whereSql;
            tx.executeSql(sql, whereBinds, function(tx, rs) {
                trends.visitItems = rs.rows.item(0).visit_count;
                if (!--numReadRequests) {
                    buildDomCallback(trends);
                }
            });
        },

        // Date counts
        function(tx) {
            var sql = 
                "SELECT v.visit_date, COUNT(*) AS visit_date_count " +
                fromSql +
                whereSql + 
                "GROUP BY v.visit_date ORDER BY COUNT(*) DESC"
            ;
            tx.executeSql(sql, whereBinds, function(tx, rs) {
                var countsExceptToday = [];
                var todayString = utils.dateToString(new Date());

                for (var i = 0; i < rs.rows.length; i++) {
                    var item = rs.rows.item(i);
                    var dateObj = utils.stringToDate(item.visit_date);

                    trends.byBusiestDay[dateObj.toDateString()] = item.visit_date_count;

                    if (i < 100) {
                        trends.topDays.push([
                            dateObj.getTime(),
                            item.visit_date_count
                        ]);
                    }

                    if (item.visit_date != todayString) {
                        countsExceptToday.push(item.visit_date_count);
                    }
                }

                trends.busiestDayMean = utils.calcMean(countsExceptToday);
                trends.busiestDayMedian = utils.calcMedian(countsExceptToday);

                if (!--numReadRequests) {
                    buildDomCallback(trends);
                }
            });
        },

        // Top Domain counts
        function(tx) {
            // As an optimization, if there's no domain filter, we can query the
            // root_domain column. Otherwise, we need to use the host column.
            var column = "root_domain";
            var domainFilter = globalFilters.getDomain();
            if (domainFilter) {
                column = "host";
            }

            // exclude the oddball urls for which we don't handle host/domains.
            // e.g., ip-addresses and file:///
            var excludeNulls = "";
            if (whereSql.length) {
                excludeNulls = " AND u." + column + " IS NOT NULL ";
            }
            else {
                excludeNulls = " WHERE u." + column + " IS NOT NULL ";
            }

            var sql =
                "SELECT u." + column + " AS domain, COUNT(*) AS domain_count " +
                fromSql +
                whereSql +
                excludeNulls +
                "GROUP BY u." + column + " ORDER BY COUNT(*) DESC"
            ;

            // When filtering by domain, we need to preprocess the results to
            // collapse similar domains into one (e.g., if filter='z.com', then
            // collapse 'a.z.com' and 'b.z.com' into 'z.com'). Because SQLite
            // doesn't have built-in support for regexes, we have to do this in JS.
            var hostFunction = function(tx, rs) {
                var domainCounts = {};
                for (var i = 0; i < rs.rows.length; i++) {
                    var item = rs.rows.item(i);
                    var portion = utils.getDomainPortion(item.domain, domainFilter);
                    if (!domainCounts[ portion ]) {
                        domainCounts[ portion ] = 0;
                    }
                    domainCounts[ portion ] = item.domain_count + domainCounts[ portion ];
                }
                var i = 0;
                for (var domain in domainCounts) {
                    if (i > 99) { break; }
                    trends.topDomains.push([ domain, domainCounts[ domain ] ]);
                    i++;
                }
                if (!--numReadRequests) {
                    buildDomCallback(trends);
                }
            };

            // When there is no domain filter, we can use the root_domain column as
            // an optimization, preventing the need for preprocessing.
            var rootDomainFunction = function(tx, rs) {
                for (var i = 0; i < rs.rows.length; i++) {
                    if (i > 99) { break; }
                    var item = rs.rows.item(i);
                    trends.topDomains.push([ item.domain, item.domain_count ]);
                }
                if (!--numReadRequests) {
                    buildDomCallback(trends);
                }
            };

            tx.executeSql(sql, whereBinds, domainFilter ? hostFunction : rootDomainFunction);
        },

        // Unique URLs per Domain counts
        function(tx) {
            // As an optimization, if there's no domain filter, we can query the
            // root_domain column. Otherwise, we need to use the host column.
            var column = "root_domain";
            var domainFilter = globalFilters.getDomain();
            if (domainFilter) {
                column = "host";
            }

            // exclude the oddball urls for which we don't handle host/domains.
            // e.g., ip-addresses and file:///
            var excludeNulls = "";
            if (whereSql.length) {
                excludeNulls = " AND u." + column + " IS NOT NULL ";
            }
            else {
                excludeNulls = " WHERE u." + column + " IS NOT NULL ";
            }

            var sql;
            var callback;

            if (domainFilter) {
                sql =
                    "SELECT u.url, u." + column + " AS domain, COUNT(*) as visit_count " +
                    fromSql +
                    whereSql +
                    excludeNulls +
                    " GROUP BY u.url, u." + column
                ;

                callback = function(tx, rs) {
                    var urlCounts = {};
                    var rowCount = rs.rows.length;
                    for (var i = 0; i < rowCount; i++) {
                        var item = rs.rows.item(i);
                        var domain = utils.getDomainPortion( item.domain, domainFilter );
                        if (!urlCounts[ domain ]) {
                            urlCounts[ domain ] = { url_count: 0, visit_count: 0 };
                        }
                        urlCounts[ domain ].url_count++;
                        urlCounts[ domain ].visit_count += item.visit_count;
                    }

                    var urlCountArray = [];
                    for (var d in urlCounts) {
                        urlCountArray.push({ 
                            domain: d, 
                            url_count: urlCounts[ d ].url_count, 
                            visit_count: urlCounts[ d ].visit_count
                        });
                    }
                    urlCountArray.sort(function(a,b) { return b.url_count - a.url_count; });

                    for (var i = 0; i < urlCountArray.length; i++) {
                        if (i > 99) { break; }
                        var item = urlCountArray[ i ];
                        trends.topUniqueVisits.push([ 
                            item.domain, 
                            item.url_count, 
                            item.visit_count,  
                            item.url_count / item.visit_count
                        ]);
                    }

                    if (!--numReadRequests) {
                        buildDomCallback(trends);
                    }
                };
            }
            else {
                sql = 
                    "SELECT * FROM (" +
                        "SELECT " + 
                            "u." + column + " AS domain, " +
                            "COUNT(DISTINCT u.url) AS url_count, " +
                            "COUNT(*) AS visit_count " +
                        fromSql +
                        whereSql +
                        excludeNulls +
                        "GROUP BY u." + column + " " +
                        "ORDER BY COUNT(DISTINCT u.url) DESC, visit_count DESC" +
                    ") LIMIT 100"
                ;
                callback = function(tx, rs) {
                    var rowCount = rs.rows.length;
                    for (var i = 0; i < rowCount; i++) {
                        var item = rs.rows.item(i);
                        trends.topUniqueVisits.push([ 
                            item.domain, 
                            item.url_count, 
                            item.visit_count,  
                            item.url_count / item.visit_count
                        ]);
                    }

                    if (!--numReadRequests) {
                        buildDomCallback(trends);
                    }
                };
            }

            tx.executeSql(sql, whereBinds, callback);
        },



        // Top URL counts
        function(tx) {
            var sql = 
                "SELECT u.url, COUNT(*) AS url_count " +
                fromSql +
                whereSql +
                "GROUP BY u.url ORDER BY COUNT(*) DESC"
            ;
            tx.executeSql(sql, whereBinds, function(tx, rs) {
                var rowCount = rs.rows.length;
                trends.historyItems = rowCount;
                for (var i = 0; i < rowCount; i++) {
                    if (i > 99) { break; }
                    var item = rs.rows.item(i);
                    trends.topUrls.push([ item.url, item.url_count ]);
                }
                if (!--numReadRequests) {
                    buildDomCallback(trends);
                }
            });
        },

        // Hour counts
        function(tx) {
            var sql = "SELECT v.hour, COUNT(*) as hour_count " + fromSql + whereSql + " GROUP BY v.hour ORDER BY hour";
            tx.executeSql(sql, whereBinds, function(tx, rs) {
                for (var i = 0; i < rs.rows.length; i++) {
                    var item = rs.rows.item(i);
                    trends.byHour[item.hour] = item.hour_count;
                }
                if (!--numReadRequests) {
                    buildDomCallback(trends);
                }
            });
        },

        // Weekday counts
        function(tx) {
            var sql = 
                "SELECT v.week_day, COUNT(*) AS week_day_count " + 
                fromSql +
                whereSql + 
                "GROUP BY v.week_day ORDER BY week_day"
            ;
            tx.executeSql(sql, whereBinds, function(tx, rs) {
                for (var i = 0; i < rs.rows.length; i++) {
                    var item = rs.rows.item(i);
                    trends.byDay[item.week_day] = item.week_day_count;
                }
                if (!--numReadRequests) {
                    buildDomCallback(trends);
                }
            });
        },

        // Monthday counts
        function(tx) {
            var sql =
                "SELECT v.month_day, COUNT(*) AS month_day_count " +
                fromSql +
                whereSql + 
                "GROUP BY v.month_day ORDER BY month_day"
            ;
            tx.executeSql(sql, whereBinds, function(tx, rs) {
                for (var i = 0; i < rs.rows.length; i++) {
                    var item = rs.rows.item(i);
                    trends.byDayOfMonth[item.month_day] = item.month_day_count;
                }
                if (!--numReadRequests) {
                    buildDomCallback(trends);
                }
            });
        },

        // Month counts
        function(tx) {
            var sql = "SELECT v.month, COUNT(*) AS month_count " + fromSql + whereSql + " GROUP BY v.month ORDER BY month";
            tx.executeSql(sql, whereBinds, function(tx, rs) {
                for (var i = 0; i < rs.rows.length; i++) {
                    var item = rs.rows.item(i);
                    trends.byMonth[item.month] = item.month_count;
                }
                if (!--numReadRequests) {
                    buildDomCallback(trends);
                }
            });
        },

        // Year counts
        function(tx) {
            var sql = "SELECT v.year AS year, COUNT(*) AS year_count " + fromSql + whereSql + " GROUP BY v.year ORDER BY v.year";
            tx.executeSql(sql, whereBinds, function(tx, rs) {
                for (var i = 0; i < rs.rows.length; i++) {
                    var item = rs.rows.item(i);
                    trends.byYear[item.year] = item.year_count;
                }
                if (!--numReadRequests) {
                    buildDomCallback(trends);
                }
            });
        },

        // Transition Type counts
        function(tx) {
            var sql =
                "SELECT v.transition_type, COUNT(*) AS transition_type_count " +
                fromSql +
                whereSql +
                "GROUP BY v.transition_type"
            ;
            tx.executeSql(sql, whereBinds, function(tx, rs) {
                for (var i = 0; i < rs.rows.length; i++) {
                    var item = rs.rows.item(i);
                    trends.byTransition[item.transition_type] = item.transition_type_count;
                }
                if (!--numReadRequests) {
                    buildDomCallback(trends);
                }
            });
        }
    ];

    numReadRequests = calculationFunctions.length;

    for (var i = 0; i < numReadRequests; i++) {
        var progressUpdater = function(i) {
            return function() {
                websql.updateSyncProgress("Analyzing history... " + Math.round( ((i+1)/calculationFunctions.length) * 100 ) + "%");
            };
        };
        websql.db.readTransaction( calculationFunctions[i], websql.onTxnError, progressUpdater(i) );
    }
}


websql.buildWhereClause = function() {
    var clauses = [];
    var binds = [];

    var url = globalFilters.getUrl();
    if (url) {
        clauses.push("url = ?");
        binds.push(url);
    }

    var domain = globalFilters.getDomain();
    if (domain) {
        // if the domain filter is present, we must query the urls.host column.
        // urls.domain is only used as an optimization when no domain filter exists.
        clauses.push("(host = ? OR host LIKE '%.' || ?)");
        binds.push(domain, domain);
    }

    var date = globalFilters.getDate();
    if (date) {
        clauses.push("visit_date = ?");
        var d = new Date(date);
        binds.push( utils.dateToString(d) );
    }

    var hour = globalFilters.getHour();
    if (hour) {
        clauses.push("hour = ?");
        binds.push( hour );
    }

    var weekday = globalFilters.getWeekday();
    if (weekday) {
        clauses.push("week_day = ?");
        binds.push( weekday );
    }

    var monthday = globalFilters.getMonthday();
    if (monthday) {
        clauses.push("month_day = ?");
        binds.push( monthday );
    }

    var month = globalFilters.getMonth();
    if (month) {
        clauses.push("month = ?");
        binds.push( month );
    }

    var year = globalFilters.getYear();
    if (year) {
        clauses.push("year = ?");
        binds.push( year );
    }

    var transition = globalFilters.getTransition();
    if (transition) {
        clauses.push("transition_type = ?");
        binds.push( transition );
    }

    var limiter = globalPrefs.getLimiter();
    if (limiter != "limiter_all") {
        clauses.push("visit_time >= ?");
        binds.push( utils.limiterToVisitTime( limiter, (new Date()).getTime() ) );
    }

    var sql = "";
    if (clauses.length) {
        sql = " WHERE " + clauses.join(" AND ") + " ";
    }

    return { sql: sql, binds: binds };
}

websql.updateSyncProgress = function(msg) {
    var element = document.getElementById("waiting_progress");
    if (element) {
        element.innerHTML = msg;
    }
}


