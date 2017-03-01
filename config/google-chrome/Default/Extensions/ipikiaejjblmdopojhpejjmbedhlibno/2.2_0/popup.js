function donateClick() {
	chrome.tabs.create({url:'https://www.paypal.com/cgi-bin/webscr?cmd=_s-xclick&hosted_button_id=S76JFJWDVUEPQ'});


}
function init() {
	document.getElementById('donate-link').addEventListener("click",donateClick,false);
}

document.addEventListener("DOMContentLoaded", init, false);