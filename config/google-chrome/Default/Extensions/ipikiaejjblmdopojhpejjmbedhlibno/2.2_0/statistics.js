var wordCount;
var totalTimeSpent;
var nationalWpmAverage = 200;
var hourlyrate;
var timeSaved; //in seconds

function _dictIsEmpty(obj) {
    for(var prop in obj) {
        if(obj.hasOwnProperty(prop))
            return false;
    }
}

function _localStorageSetdefault(key, defaultValue) {
	value = localStorage.getItem(key);
	//console.log(value);
	if (value==null || value==undefined || value=='NaN') {
		localStorage.setItem(key, defaultValue.toString());
		return defaultValue;
	}
	else {
		return value;
	}
}

function generateRandomCongrats() {
	var randomInt = Math.floor((Math.random() * 5) + 1);
	switch (randomInt) {
		case 1:
			return "Congratulations!";
			break;
		case 2:
			return "Nice!";
			break;
		case 3:
			return "Awesome!";
			break;
		case 4:
			return "Cool!";
			break;
		case 5:
			return "Wow!";
			break;
	}
}


var waitUntil = function (fn, condition, interval) {
    interval = interval || 100;

    var shell = function () {
            var timer = setInterval(
                function () {
                    var check;

                    try { check = !!(condition()); } catch (e) { check = false; }

                    if (check) {
                        clearInterval(timer);
                        delete timer;
                        fn();
                    }
                },
                interval
            );
        };

    return shell;
};




function secondsToString(seconds)
{

	var numdays = Math.floor(seconds / 86400);
	var numhours = Math.floor((seconds % 86400) / 3600);
	var numminutes = Math.floor(((seconds % 86400) % 3600) / 60);
	var numseconds = ((seconds % 86400) % 3600) % 60;
	if (numdays > 0) {
		return numdays + " days, " + numhours + " hours, " + numminutes + " minutes, " + numseconds + " seconds";
	} else if (numhours > 0) {
		return numhours + " hours, " + numminutes + " minutes, " + numseconds + " seconds";
	} else {
		return numminutes + " minutes, " + numseconds + " seconds";
	}

}

function _calculateTimeSaved() {
	actualTimeSpent = totalTimeSpent/60.0; //actual user time spent in minutes
	expectedTimeSpent = wordCount/nationalWpmAverage; //expected time spent in minutes

	timeSaved = Math.round((expectedTimeSpent-actualTimeSpent)*60); //convert to seconds
	if (timeSaved == NaN) {
		timeSaved = 0;
	}

	//timeSavedString = secondsToString(timeSaved);
	return timeSaved;
}

function _calculateAverageWpm() {
	//average wpm
	if (totalTimeSpent==0) {
		averageWpm = 0;
	}
	else {
		averageWpm = wordCount/(totalTimeSpent/60.0);
	}
	return Math.round(averageWpm);
	//return Math.round(averageWpm).toString()+' WPM';
	
}


function updateHourlyrateValue() {
	hourlyrateString = '$'+hourlyrate+' / hour';
	$('#hourlyrate').html(hourlyrateString);

	//update all other stats that depend on it, in the time worth table
	timeWorthTable = $('#timeworth-table');
	//clear table
	timeWorthTable.children().remove();

	//append header
	timeWorthTable.append('<tr><th>Based on actual Spreed usage:</th></tr>');
	//append stats
	timeWorthTable.append('<tr><td>Your time\'s worth:</td><td></td><td>'+hourlyrateString+'</td></tr>');
	
	
	dayCount = parseInt(_localStorageSetdefault("dayCount", 0));

	//console.log('day count: '+dayCount);
	//extrapolate if not a month yet (have to keep track of days used!), average if more than a month
	if (dayCount<30){
		if (dayCount==0) {
			multiplier = 0;
		}
		else {
			multiplier = 30.0/dayCount;
		}
		//console.log('extrapolated multiplier: '+multiplier);
		hoursPerMonth = multiplier*timeSaved/(60*60);
		extrapolated=true;
	}
	else {
		numMonths = dayCount/30.0;
		//console.log('num months: '+numMonths);
		hoursPerMonth = (timeSaved/(60*60))/numMonths;
	}
	//console.log('hours per month: '+hoursPerMonth);

	//number of hours you've saved in a month
	timeWorthTable.append('<tr><td>Your average number of hours saved a month using Spreed*:</td><td class="middle-column">X</td><td>'+hoursPerMonth.toFixed(2)+'</td></tr>');

	//worth
	worth = hoursPerMonth*hourlyrate;
	timeWorthTable.append('<tr class="border-top"><td>Monthly amount saved using Spreed:</td><td></td><td><span class="big">$'+worth.toFixed(2)+' / month</span></td></tr>');

	//at very end, append notes
	timeWorthTable.append('<tr><td><small>* if you haven\'t used Spreed for more than a month, this number is an extrapolation. Based on national average reading speed of 200 WPM.</small></td></tr>');


	
	
}



$(document).ready(function(){
	//get statistics

	defaultStatsSet = false;

	wordCount = parseInt(_localStorageSetdefault("wordCount", 0));
	totalTimeSpent = parseInt(_localStorageSetdefault("totalTimeSpent", 0));

	defaultStatsSet = true;




	waitUntil(
	  function () {
		initStatistics();
	  },
	  function() {
	    // the code that tests here... (return true if test passes; false otherwise)
	    return !!(defaultStatsSet==true);
	  },
	  50 // amount to wait between checks
	)();

});

function initStatistics() {
	//calculate and write statistics
	statisticsTable = $('#statistics-table');

	//how fast you read compared to average
	averageWpm = _calculateAverageWpm();
	if (averageWpm>250) {
		var readingGreatness = String((averageWpm/200.0).toFixed(1));
		var randomCongrats = generateRandomCongrats();
		var shareStatsUrl = "https://www.facebook.com/dialog/feed?app_id=1420577901545273&display=popup&caption=I can read "+readingGreatness+"x times faster than the average person, using Spreed&link=https://chrome.google.com/webstore/detail/spreed-speed-read-the-web/ipikiaejjblmdopojhpejjmbedhlibno&redirect_uri=https://chrome.google.com/webstore/detail/spreed-speed-read-the-web/ipikiaejjblmdopojhpejjmbedhlibno";
		statisticsTable.before('<div id="congrats-info"><u>'+randomCongrats+' Using Spreed, you read '+readingGreatness+'x faster than the average person!</u><br><div id="share-stats">Share your reading achievement!</div></div>');
		$('#share-stats').click(function() { window.open(shareStatsUrl,"", "width=500, height=350"); });
		
	}

	//time saved
	timeSaved = _calculateTimeSaved();
	timeSavedString = secondsToString(timeSaved);
	statisticsTable.append('<tr><td>Time saved by using Spreed: </td><td>'+timeSavedString+'</td></tr>');
	

	//average wpm
	statisticsTable.append('<tr><td>Your average reading speed: </td><td>'+Math.round(averageWpm).toString()+' WPM</td></tr>');
	

	//total time spent
	totalTimeSpentString = secondsToString(totalTimeSpent);
	statisticsTable.append('<tr><td>Total time spent: </td><td>'+totalTimeSpentString+'</td></tr>');

	//total words read
	statisticsTable.append('<tr><td>Total words read: </td><td>'+wordCount.toString()+' words</td></tr>');



	//populate time worth statistics
	//first init hourly rate slider
	slider = $( "#hourlyrate-slider" );

	hourlyrate = localStorage.getItem("hourlyrate");
	//console.log(hourlyrate);
	if (hourlyrate==null || hourlyrate==undefined) {
		hourlyrate = 8;
	}

	updateHourlyrateValue();

	slider.slider({
	    orientation: "horizontal",
	    max: 30,
	    min: 0,
	    value: hourlyrate, 
	    slide: function( event, ui ) {
	    	if (event.originalEvent) {
	    		//user changed the slider
	    		hourlyrate = ui.value;
	    		localStorage.setItem("hourlyrate",hourlyrate);
	    		updateHourlyrateValue();
	        }
	    }
	});

	slider.css("display", "");

	//set donation listeners
	$('select#donation-recurrence').change(recurrenceChanged);

	

}


function recurrenceChanged () {
	selected = $('select#donation-recurrence').val();
	if (selected === "M") {
		//show monthly stuff
		$('#donation-type').val("_xclick-subscriptions");
		$('#monthly-donation-amount-row').show("fast");
	}
	else if (selected === "O") {
		//show daily stuff
		$('#donation-type').val("_donations");	
		//hide monthly donation amount
		$('#monthly-donation-amount-row').hide("fast");
	}
}

