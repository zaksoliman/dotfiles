var blacklistTextarea;
var disableAllCheckbox;

window.onload=function(){
	//get blacklist
	blacklistString = localStorage.getItem("blacklist");

	blacklistTextarea = document.getElementById("blacklist-textarea");
	blacklistTextarea.value = blacklistString;
	document.getElementById("save-blacklist-button").addEventListener("click", saveBlacklist);

	
	disableAllCheckbox = document.getElementById("disable-button-everywhere");
	updateDisableCheckbox();
	disableAllCheckbox.addEventListener('change', changeDisableCheckbox);


	//set the micro-pausing checkbox
	enablemicropauseCheckbox = document.getElementById("enable-micropause");
	updateMicropauseCheckbox();
	enablemicropauseCheckbox.addEventListener('change', changeMicropauseCheckbox);
};

//set blacklist
function saveBlacklist() {
	blacklistString = blacklistTextarea.value;
	localStorage.setItem("blacklist", blacklistString);
	
	//dispay saved notification
	label = document.getElementById("button-label");
	label.style.cssText = "color:#0f0;"
	label.innerHTML = "Saved";
	setTimeout(function(){
		label.innerHTML = "";
	},3000);
}

//disable spreed button everywhere checkbox changged
function changeDisableCheckbox() {
	if(disableAllCheckbox.checked){
		//checked, so disable blacklisttextarea
		blacklistTextarea.disabled = true;
		localStorage.setItem("blacklistall","true");
	}
	else {
		blacklistTextarea.disabled = false;
		localStorage.setItem("blacklistall","false");
	}
}

function updateDisableCheckbox() {
	blacklistall = localStorage.getItem("blacklistall");
	if (blacklistall == "true") {
		blacklistTextarea.disabled = true;
		disableAllCheckbox.checked = true;
	} else {
		blacklistTextarea.disabled = false;
		disableAllCheckbox.checked = false;
	}
}

// enable micro-pause
function changeMicropauseCheckbox() {
	if (enablemicropauseCheckbox.checked) {
		localStorage.setItem("enablemicropause","true");
	} else {
		localStorage.setItem("enablemicropause","false");
	}
}
function updateMicropauseCheckbox() {
	enablemicropause = localStorage.getItem("enablemicropause");
	if (enablemicropause == undefined) {
		localStorage.setItem("enablemicropause","true");
		enablemicropause = "true";
	}
	if (enablemicropause == "true") {
		enablemicropauseCheckbox.checked = true;
	} else {
		enablemicropauseCheckbox.checked = false;
	}
}