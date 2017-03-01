"use strict";

chrome.browserAction.onClicked.addListener(function() {
    var prefs = new Preferences();
    var page = "trends.html";
    if (prefs.startPage() == "search") {
        page = "search.html";
    }
    chrome.tabs.create({url:chrome.extension.getURL( page )});
});

chrome.runtime.onStartup.addListener(function() {
    utils.logInfo("onStartup sync started");
    websql.initAndSync(function() {
        utils.logInfo("onStartup sync ended");

        var prefs = new Preferences();
        var now   = new Date();
        if ( utils.backupIsNeeded( prefs, now ) ) {
            var backupType = prefs.autoBackupType();
            var backupDate = prefs.lastAutoBackupDate();
            var filename = 'history_autobackup_' + utils.dateToString( now, '' ) + '_' + backupType + '.txt';

            var sqlAndBinds;
            if (backupType == "incremental" && backupDate ) {
                sqlAndBinds = {
                    sql: "WHERE v.visit_time > ?",
                    binds: [ backupDate.getTime() ]
                };
            }

            utils.logInfo("autobackup (" + backupType + ") started");
            ga('send','event','export','auto_' + backupType);

            exporter.exportResults(
                filename,
                'archived',
                sqlAndBinds,
                prefs.autoBackupCompression(),
                function() { 
                    prefs.setLastAutoBackupDate( now ); 
                    utils.logInfo("autobackup (" + backupType + ") ended");
                }
            );
        }
    });
});
