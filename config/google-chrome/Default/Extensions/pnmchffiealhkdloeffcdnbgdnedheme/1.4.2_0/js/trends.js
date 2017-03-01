"use strict";

// History Trends setup
var globalFilters = new Filters();
var globalPrefs = new Preferences();

google.load("visualization", "1", {packages:["corechart", "annotatedtimeline"], language:globalPrefs.getLocale()});
google.setOnLoadCallback(initialize);

function initialize() {
    utils.logInfo("onPageLoad sync started");
    websql.initAndSync(function(){ 
        utils.logInfo("onPageLoad sync ended");
        utils.logInfo("Calculating stats started");
        websql.calcTrends(buildDom);
        function buildDom(trends) {
            utils.logInfo("Calculating stats ended");
            utils.logInfo("Drawing UI started");
            // Several charts must be visible for them to render correctly
            document.getElementById('waiting_container').style.display = 'none';
            document.getElementById('trends_container').style.display = 'block';
            ui.createOverview(trends, 'overview');
            charts.drawHourChart(trends.byHour, 'chart_hour');
            charts.drawDayChart(trends.byDay, 'chart_day');
            charts.drawDayOfMonthChart(trends.byDayOfMonth, 'chart_day_of_month');
            charts.drawMonthChart(trends.byMonth, 'chart_month');
            charts.drawYearChart(trends.byYear, 'chart_year');
            charts.drawTransitionChart(trends.byTransition, 'chart_transition', 'transition_definition');
            charts.drawDygraphChart(trends.byBusiestDay, 'chart_time');
            ui.createFooter();
            utils.logInfo("Drawing UI ended");
        }
    });
}

$(document).ready(function(){
    // Set up dialog box for exporting results
    $( "#export-form" ).dialog({
        autoOpen: false,
        height: 275,
        width: 450,
        modal: true,
        buttons: {
            Export: {
                id: 'export_dialog_button',
                text: 'Export',
                click: function() {
                    ga('send','event','export','raw_data');
                    $('#export_dialog_button').before(
                        '<img id="export_spinner" style="padding-right: 10px; vertical-align: middle;" src="images/spinner-small.gif" width="24" height="24" />'
                    );
                    var whereInfo = websql.buildWhereClause();
                    exporter.exportResults(
                        'exported_analysis_history_' + utils.dateToString(new Date(), '') + '.txt',
                        'analysis', 
                        whereInfo,
                        0,  // no compression
                        function() {
                            $('#export_spinner').remove();
                            $('#export-form').dialog( "close" );
                        }
                    );
                }
            },
            Cancel: function() {
                $( this ).dialog( "close" );
            }
        },
        close: function() {
        },
        open: function(e, ui) {
        }
    });
 
    $( "#export_button" )
        .click(function() {
            $( "#export_error" ).hide();
            $( "#export_more_details" ).attr('href', chrome.extension.getURL("export_details.html"));
            $( "#export-form" ).dialog( "open" );
        })
    ;
});
