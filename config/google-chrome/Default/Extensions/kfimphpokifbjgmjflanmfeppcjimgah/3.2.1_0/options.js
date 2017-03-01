// Save this script as `options.js`

var runtimeOrExtension = chrome.runtime && chrome.runtime.sendMessage ? 'runtime' : 'extension';



if(window.devicePixelRatio >= 2) {
	document.getElementById("i_1").innerHTML = '<img src="icon1_r.png" width="19px">';
	document.getElementById("i_2").innerHTML = '<img src="icon3_r.png" width="19px">';
	document.getElementById("i_3").innerHTML = '<img src="icon2_r.png" width="19px">';
	document.getElementById("i_4").innerHTML = '<img src="icon4_r.png" width="19px">';
}else{
	document.getElementById("i_1").innerHTML = '<img src="icon1.png" width="19px">';
	document.getElementById("i_2").innerHTML = '<img src="icon3.png" width="19px">';
	document.getElementById("i_3").innerHTML = '<img src="icon2.png" width="19px">';
	document.getElementById("i_4").innerHTML = '<img src="icon4.png" width="19px">';
}
		
// Saves options to localStorage.
function save_options(){
	var badge_type = 1;
	var icon = 1;
	var redirect = 1;
	// var session_key = "";
	var use_ssl = 0;
	var original_fav = 0;

	var badge_type = document.getElementById('badge_type').value;
	var badge_bgcolor = document.getElementById('badge_bgcolor').value;

	var icon_radio = document.getElementsByName('icon');
	for (var i = 0, length = icon_radio.length; i < length; i++){
		if(icon_radio[i].checked){
			icon = icon_radio[i].value;
		}
	}

	var e = document.getElementById("redirect");
	redirect = e.options[e.selectedIndex].value;

	// session_key = document.getElementById('session_key').value;
	
	use_ssl = (document.getElementById('use_ssl').checked) ? 1 : 0;
	original_fav = (document.getElementById('original_fav').checked) ? 1 : 0;
	
	localStorage['badge_type'] = badge_type;
	localStorage['badge_bgcolor'] = badge_bgcolor;
	localStorage['icon'] = icon;
	localStorage['redirect'] = redirect;
	// localStorage['session_key'] = session_key;
	localStorage['use_ssl'] = use_ssl;
	localStorage['original_fav'] = original_fav;

	chrome[runtimeOrExtension].sendMessage({"option_update": true});

//	var status = document.getElementById('status');
//	status.innerHTML = 'Config Saved!';
//	setTimeout(function() {
//		status.innerHTML = '';
//	}, 1500);
}

function select_option(id,option){
    var sel = document.getElementById(id);
    for(var i, j = 0; i = sel.options[j]; j++) {
        if(i.value == option) {
            sel.selectedIndex = j;
            break;
        }
    }
}

function change_badge_bgcolor_select_bgcolor(){
	return; // fugly
    var sel = document.getElementById('badge_bgcolor');
	var selected_color = sel.value;
	sel.style.backgroundColor = selected_color;
	sel.style.color = '#FFF';
}

function restore_options() {
	var use_ssl = localStorage['use_ssl'];
	var badge_type = localStorage['badge_type'];
	var badge_bgcolor = localStorage['badge_bgcolor'];
	var icon = localStorage['icon'];
	var redirect = localStorage['redirect'];
	var original_fav = localStorage['original_fav'];
	
	// var session_key = (localStorage['session_key'] != undefined) ? localStorage['session_key'] : '';

	if(badge_type != undefined){
		select_option('badge_type',badge_type);
	}

	if(badge_bgcolor != undefined){
		select_option('badge_bgcolor',badge_bgcolor);
	}

	if(redirect != undefined){
		select_option('redirect',redirect);
	}

	if(use_ssl == 1){
		document.getElementById('use_ssl').checked = true;
	}

	if(original_fav == undefined || original_fav == 1){
		document.getElementById('original_fav').checked = true;
	}

	if(icon == undefined){
		document.getElementById('icon_1').checked = true;
		return;
	}else{
		document.getElementById('icon_' + icon).checked = true;
	}

	// document.getElementById('session_key').value = session_key;
	change_badge_bgcolor_select_bgcolor();

}
document.addEventListener('DOMContentLoaded', restore_options);
var elements = document.querySelectorAll('.options');

for(var i in elements){
	elements[i].onchange = function(){ save_options(); };
}

document.getElementById('redirect').onchange = function(){save_options();}

document.getElementById('badge_bgcolor').onchange = function(){ change_badge_bgcolor_select_bgcolor(); save_options(); };

// document.getElementById('session_key').onkeyup = function(){ save_options(); };
