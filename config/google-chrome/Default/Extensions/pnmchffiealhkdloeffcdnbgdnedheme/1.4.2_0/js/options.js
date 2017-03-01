"use strict";

var globalPrefs = new Preferences();

$(document).ready(function(){
    websql.init();

    // Chart Options: "Visits per Day" rolling average checkbox
    $('#enable_rolling_average')
        .prop('checked', globalPrefs.rollingAverage())
        .change( enableRollingAverageHandler )
    ;

    // Chart Options: "Visits per Day" rolling average period
    $('#rolling_average_period')
        .val( globalPrefs.rollingAveragePeriod() )
        .prop( 'disabled', !globalPrefs.rollingAverage() )
        .keyup( function(e) {
            globalPrefs.setRollingAveragePeriod( e.target.value );
        } )
    ;

    // Start Page
    $('#start_page').val( globalPrefs.startPage() );
    $('#start_page').change(function(e) {
        globalPrefs.setStartPage( e.target.value );        
    });

    // Time Display
    $('#time_display').val( globalPrefs.getHourFormat() );
    $('#time_display').change(function(e) {
        globalPrefs.setHourFormat( e.target.value );
    });

    // Transfer History: import file
    $('#import_file').change( importer.importHandler );

    // Transfer History: export history
    $('#export_history').click(function() {
        ga('send','event','export','export_history');
        $('#export_history').after(
            '<img id="export_spinner" style="padding-left: 10px; vertical-align: middle;" src="images/spinner-small.gif" width="24" height="24" />'
        );
        exporter.exportResults(
            'exported_archived_history_' + utils.dateToString(new Date(), '') + '.txt',
            'archived', 
            null, 
            0,  // no compression
            function() { $('#export_spinner').remove(); }
        );
    });

    // Feedbak: FAQ link
    $('#faq').attr('href', chrome.extension.getURL("faq.html"));

    // Auto Backup: Enable checkbox
    $('#enable_autobackup')
        .prop('checked', globalPrefs.autoBackup())
        .change( enableAutoBackupHandler )
    ;

    // Auto Backup: Backup Type
    $('#autobackup_type')
        .val( globalPrefs.autoBackupType() )
        .prop('disabled', !globalPrefs.autoBackup() )
        .change(function(e) {
            globalPrefs.setAutoBackupType( e.target.value );
            $('#autobackup_type_desc').text( getBackupTypeDesc( e.target.value ) );
        })
    ;
    $('#autobackup_type_desc').text( getBackupTypeDesc( globalPrefs.autoBackupType() ) );

    // Auto Backup: Backup interval
    $('#autobackup_interval')
        .val( globalPrefs.autoBackupInterval() )
        .prop('disabled', !globalPrefs.autoBackup() )
        .keyup( function(e) { globalPrefs.setAutoBackupInterval( e.target.value ) } )
    ;

    // Auto Backup: Compression checkbox
    $('#autobackup_compression')
        .prop('checked', globalPrefs.autoBackupCompression())
        .prop( 'disabled', !globalPrefs.autoBackup() )
        .change(function(e) { 
            var checked = e.target.checked;
            globalPrefs.setAutoBackupCompression( checked ); 
        })
    ;
});


function enableRollingAverageHandler(e) {
    var checked = e.target.checked;
    globalPrefs.setRollingAverage(checked);
    globalPrefs.setRollingAveragePeriod( $('#rolling_average_period').val() );
    $('#rolling_average_period').prop('disabled', !checked);
}

function enableAutoBackupHandler(e) {
    var checked = e.target.checked;
    globalPrefs.setAutoBackup(checked);
    globalPrefs.setAutoBackupType( $('#autobackup_type').val() );
    globalPrefs.setAutoBackupInterval( $('#autobackup_interval').val() );
    globalPrefs.setAutoBackupCompression( $('#autobackup_compression').prop('checked') );
    $('#autobackup_type').prop('disabled', !checked);
    $('#autobackup_interval').prop('disabled', !checked);
    $('#autobackup_compression').prop('disabled', !checked);
    // Remove the last autobackup date when turning off autobackup.
    if (!checked) {
        globalPrefs.setLastAutoBackupDate( undefined );
    }
}


function getBackupTypeDesc(value) {
    return value == "full"
        ? "(Exports entire history each time)"
        : "(First backup has everything; subsequent backups have history since last backup)"
    ;
}


