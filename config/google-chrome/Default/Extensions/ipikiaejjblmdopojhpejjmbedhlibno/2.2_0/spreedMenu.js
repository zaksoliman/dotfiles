spreedThis =function() {
    pushEvent("spreed menu item", "spreed current page");
	//spreed this page. current default is text selector mode
	window.close();

	chrome.runtime.sendMessage({action: "openSpreedFromMenu"}, function(response) {
      //console.log(response.farewell);

    });

}

spreedPasted =function() {
    
	chrome.tabs.create({url: "spreedPaste.html"});
    pushEvent("spreed menu item", "open paste window");
}

showStatistics =function() {
    
	chrome.tabs.create({url: "statistics.html"});
    pushEvent("spreed menu item", "open paste window");
}

showOptions =function() {
    
	chrome.tabs.create({url: "options.html"});
    pushEvent("spreed menu item", "open options window");
}

showHowtouse =function() {
    
	chrome.tabs.create({url: "popup.html"});
    pushEvent("spreed menu item", "open how to use");
}

showDonate =function() {
    
	chrome.tabs.create({url:'https://www.paypal.com/cgi-bin/webscr?cmd=_s-xclick&hosted_button_id=S76JFJWDVUEPQ'});
    pushEvent("spreed menu item", "open donate");
}

$(document).ready(function(){
    //add click listeners
    $('#menu-link-spreedthis').click(spreedThis);
    $('#menu-link-spreedpasted').click(spreedPasted);
    $('#menu-link-statistics').click(showStatistics);
    $('#menu-link-options').click(showOptions);
    $('#menu-link-howtouse').click(showHowtouse);
    $('#menu-link-donate').click(showDonate);

    //active tooltip
    $('.tooltip').tooltipster();
});


