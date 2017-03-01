"use strict";

var exporter = {};

/*
 * exporter.exportResults( filename, format, sqlAndBinds, compressFile, onCompleteCallback )
 *
 * filename: name of exported file
 * format: either 'archived' or 'analysis'
 * sqlAndBinds: An object with two properties: 
 *              - sql: string containing WHERE clause (including WHERE keyword)
 *              - binds: array of bind values for WHERE clause
 * compressFile: boolean indicating if file should be compressed. If true, the filename 
                 extension will be changed to '.zip'.
 * onCompleteCallback: function to call when export is done
 */

exporter.exportResults = function( filename, format, sqlAndBinds, compressFile, onCompleteCallback ) {
    var selectSql = "SELECT u.url, u.host, u.title, u.root_domain, v.visit_time, v.transition_type ";
    var fromSql = " FROM urls u INNER JOIN visits v ON u.urlid = v.urlid ";
    var orderBySql = " ORDER BY u.root_domain, u.host, u.url, v.visit_time";
    var whereSql = '';
    var whereBinds = null;

    if (sqlAndBinds) {
        whereSql = sqlAndBinds.sql;
        whereBinds = sqlAndBinds.binds;
    }

    var sql = selectSql + fromSql + whereSql + orderBySql;
    var exportString;

    websql.db.readTransaction( 
        function(tx) {
            tx.executeSql(sql, whereBinds, function(tx, rs) {
                if (format == "archived") {
                    exportString = exporter.buildArchivedExport(rs.rows);
                }
                else {
                    exportString = exporter.buildAnalysisExport(rs.rows);
                }

                if (compressFile) {
                    utils.logInfo("compression started");
                    var zip = new JSZip();
                    zip.file(filename, exportString);
                    var blob = zip.generate({ 
                        type: "blob",
                        compression: "DEFLATE",
                        compressionOptions: { level: 3 },  // 3 is arbitrary. 1=best-speed .. 9=best-compression
                    });
                    var zipFilename = filename.replace(/\.txt$/, '.zip');
                    utils.logInfo("compression ended");
                    saveAs(blob, zipFilename);
                }
                else {
                    var blob = new Blob([exportString], {type: "text/plain;charset=utf-8"});
                    saveAs(blob, filename);
                }
            });
        },
        websql.onTxnError,
        onCompleteCallback
    );
}

exporter.buildArchivedExport = function(rows) {
    var string = "";
    for (var i = 0; i < rows.length; i++) {
        var item = rows.item(i);
        string += 
            item.url + "\t" + 
            "U" + item.visit_time + "\t" +
            utils.convertTextToTransition(item.transition_type) + "\t" +
            (item.title == null ? "" : item.title) + "\r\n"
        ;
    }
    return string;
}

exporter.buildAnalysisExport = function(rows) {
    var string = "";
    for (var i = 0; i < rows.length; i++) {
        var item = rows.item(i);
        var date = new Date(item.visit_time);
        string += 
            item.url + "\t" + 
            (item.host == null ? "" : item.host) + "\t" + 
            (item.root_domain == null ? "" : item.root_domain) + "\t" + 
            item.visit_time + "\t" + 
            utils.dateToStringLong( date ) + "\t" +
            date.getDay() + "\t" + 
            item.transition_type + "\t" + 
            (item.title == null ? "" : item.title) + "\r\n"
        ;
    }
    return string;
}


