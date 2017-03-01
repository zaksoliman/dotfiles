"use strict";

var charts = {};

charts.drawDygraphChart = function(dailyStats, div) {
    var data = [];
    for (var dateString in dailyStats) {
        var aDate = new Date(dateString);
        data.push([aDate, dailyStats[dateString]]);
    }
    // resort data array by ascending dates
    data.sort(function (a,b) { return a[0].valueOf() - b[0].valueOf(); });

    var chartTitle = globalPrefs.rollingAverage() ? "visits_per_day_rolling_avg" : "visits_per_day";

    var gOpts = {
        fill: 50, 
        thickness: 1, 
        pointClickCallback: function (e, point) { 
            ga('send','event','trends','chart_date');
            globalFilters.setDate(point.xval);
            ui.reload(); // js/buildInterface.js
        },
        fillGraph: true,
        fillAlpha: 0.5,
        colors: ['#4684EE'],
        showRangeSelector: true,
        height: 250,
        width: 800,
        title: chrome.i18n.getMessage( chartTitle ),
        titleHeight: 22,
        labels: [ 
            'Date',     // no i18n needed, this text is never displayed
            chrome.i18n.getMessage('number_of_visits')
        ],
        labelsDivWidth: 330,
        interactionModel: Dygraph.Interaction.nonInteractiveModel_,
        xAxisLabelWidth: 75,
        axes: {
            x: {
                axisLabelFormatter: function(d, gran) {
                    if (gran < Dygraph.MONTHLY) {
                        return globalPrefs.getChartDateString(d, "daily");
                    }
                    else if (gran < Dygraph.ANNUAL) {
                        return globalPrefs.getChartDateString(d, "monthly");
                    }
                    else {
                        return globalPrefs.getChartDateString(d, "yearly");
                    }
                },
                valueFormatter: function(ms) {
                    return globalPrefs.getTrendsDateDisplay(ms);
                },
                ticker: function(a, b, pixels, opts, dygraph, vals) {
                    // This is a modified version of Dygraph.dateTicker
                    var chosen = Dygraph.pickDateTickGranularity(a, b, pixels, opts);
                    if (chosen < Dygraph.DAILY) {
                        chosen = Dygraph.DAILY;
                    }

                    if (chosen >= 0) {
                        return Dygraph.getDateAxis(a, b, chosen, opts, dygraph);
                    } 
                    else {
                        // this can happen if self.width_ is zero.
                        return [];
                    }
                }
            },
            y: {
                axisLabelFormatter: function (number, gran) {
                    return globalPrefs.getNumberDisplay(number);
                },
                valueFormatter: function(number) {
                    return globalPrefs.getNumberDisplay(number);
                }
            }
        }
    };

    if (globalPrefs.rollingAverage()) {
        gOpts.showRoller = true;
        gOpts.rollPeriod = globalPrefs.rollingAveragePeriod();
    }

    var g = new Dygraph.GVizChart(document.getElementById(div));
    g.draw(data, gOpts);
}

charts.drawHourChart = function(hourStats, div) {
    var table = new google.visualization.DataTable();
    table.addColumn('string', 'Hour');  // no i18n needed; text not displayed
    table.addColumn('number', chrome.i18n.getMessage('num_visits'));

    for (var i = 0; i < hourStats.length; i++) {
        table.addRow([ 
            { v: i.toString(), f: globalPrefs.getHourDisplay(i) }, 
            { v: hourStats[i], f: globalPrefs.getNumberDisplay(hourStats[i]) },
        ]);
    }
    
    var hourChart = new google.visualization.ColumnChart(document.getElementById(div));
    hourChart.draw(table, {
        width: 800, 
        height: 240, 
        title: chrome.i18n.getMessage('time_of_day'), 
        fontSize: 12.7,
        hAxis: { slantedText: true }
    });
    
    google.visualization.events.addListener(hourChart, 'select', function() {
        ga('send','event','trends','chart_hour');
        globalFilters.setHour(table.getValue(hourChart.getSelection()[0].row, 0));
        ui.reload(); // js/buildInterface.js
    });
}

charts.drawDayChart = function(dayStats, div) {
    var table = new google.visualization.DataTable();
    table.addColumn('string', 'Day of Week');  // no i18n needed; text is not displayed
    table.addColumn('number', chrome.i18n.getMessage('num_visits'));

    for (var i = 0; i < dayStats.length; i++) {
        table.addRow([ 
            { v: i.toString(), f: globalPrefs.getWeekdayDisplay(i) }, 
            { v: dayStats[i],  f: globalPrefs.getNumberDisplay(dayStats[i]) }
        ]);
    }
    
    var dayChart = new google.visualization.ColumnChart(document.getElementById(div));
    dayChart.draw(table, {
        width: 800, height: 240, title: chrome.i18n.getMessage('day_of_week')
    });
    
    google.visualization.events.addListener(dayChart, 'select', function() {
        ga('send','event','trends','chart_weekday');
        globalFilters.setWeekday(table.getValue(dayChart.getSelection()[0].row, 0));
        ui.reload(); // js/buildInterface.js
    });
}


charts.drawDayOfMonthChart = function(dayOfMonthStats, div) {
    var table = new google.visualization.DataTable();
    table.addColumn('string', 'Day of Month');  // no i18n needed; text not displayed
    table.addColumn('number', chrome.i18n.getMessage('num_visits'));

    for (var i = 1; i < dayOfMonthStats.length; i++) {
        table.addRow([ 
            { v: i.toString(),       f: i.toString() },
            { v: dayOfMonthStats[i], f: globalPrefs.getNumberDisplay(dayOfMonthStats[i]) } 
        ]);
    }
    
    var dayOfMonthChart = new google.visualization.ColumnChart(document.getElementById(div));
    dayOfMonthChart.draw(table, {
        width: 800, height: 240, title: chrome.i18n.getMessage('day_of_month'),
        hAxis: { showTextEvery: 2 }
    });
    
    google.visualization.events.addListener(dayOfMonthChart, 'select', function() {
        ga('send','event','trends','chart_monthday');
        globalFilters.setMonthday(table.getValue(dayOfMonthChart.getSelection()[0].row, 0));
        ui.reload(); // js/buildInterface.js
    });
}

charts.drawMonthChart = function(monthStats, div) {
    var table = new google.visualization.DataTable();
    table.addColumn('string', 'Month'); // no i18n needed; text not displayed
    table.addColumn('number', chrome.i18n.getMessage('num_visits'));

    for (var i = 0; i < monthStats.length; i++) {
        table.addRow([ 
            { v: i.toString(),  f: globalPrefs.getMonthDisplay(i) }, 
            { v: monthStats[i], f: globalPrefs.getNumberDisplay(monthStats[i]) }
        ]);
    }
    
    var monthChart = new google.visualization.ColumnChart(document.getElementById(div));
    monthChart.draw(table, {
        width: 800, height: 240, title: chrome.i18n.getMessage('month')
    });
    
    google.visualization.events.addListener(monthChart, 'select', function() {
        ga('send','event','trends','chart_month');
        globalFilters.setMonth(table.getValue(monthChart.getSelection()[0].row, 0));
        ui.reload(); // js/buildInterface.js
    });
}


charts.drawYearChart = function(yearStats, div) {
    var table = new google.visualization.DataTable();
    table.addColumn('string', 'Year'); // no i18n needed; text not displayed
    table.addColumn('number', chrome.i18n.getMessage('num_visits'));

    for (var year in yearStats) {
        table.addRow([ 
            { v: year, f: year }, 
            { v: yearStats[year], f: globalPrefs.getNumberDisplay(yearStats[year]) }
        ]);
    }
    
    var yearChart = new google.visualization.ColumnChart(document.getElementById(div));
    yearChart.draw(table, {
        width: 800, height: 240, title: chrome.i18n.getMessage('year')
    });
    
    google.visualization.events.addListener(yearChart, 'select', function() {
        ga('send','event','trends','chart_year');
        globalFilters.setYear(table.getValue(yearChart.getSelection()[0].row, 0));
        ui.reload(); // js/buildInterface.js
    });
}


charts.drawTransitionChart = function(transitionStats, div, definition_id) {
    var table = new google.visualization.DataTable();
    table.addColumn('string', 'Transition');  // no i18n needed; text not displayed
    table.addColumn('number', chrome.i18n.getMessage('num_visits'));

    for (var key in transitionStats) {
        table.addRow([ 
            { v: key, f: key },
            { v: transitionStats[key], f: globalPrefs.getNumberDisplay(transitionStats[key]) } 
        ]);
    }
    
    var transitionChart = new google.visualization.PieChart(document.getElementById(div));
    transitionChart.draw(table, {
        width: 800, height: 240, title: chrome.i18n.getMessage('transition_type')
    });
    
    google.visualization.events.addListener(transitionChart, 'select', function() {
        ga('send','event','trends','chart_transition');
        globalFilters.setTransition(table.getValue(transitionChart.getSelection()[0].row, 0));
        ui.reload(); // js/buildInterface.js
    });
    
    // Add a link to the definition of transition types
    var definition_div = document.getElementById(definition_id);
    var anchor = document.createElement('a');
    anchor.setAttribute('href', 'http://code.google.com/chrome/extensions/history.html#transition_types');
    anchor.appendChild(document.createTextNode(chrome.i18n.getMessage('define_transitions')));
    definition_div.appendChild(anchor);
}
