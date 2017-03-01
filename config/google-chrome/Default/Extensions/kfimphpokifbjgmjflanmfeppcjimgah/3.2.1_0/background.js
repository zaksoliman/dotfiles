var runtimeOrExtension = chrome.runtime && chrome.runtime.sendMessage ?
                         'runtime' : 'extension';
var base_url = 'https://www.inoreader.com/';
var base_url_nossl = 'http://www.inoreader.com/';
var inoreader_url = /inoreader\.com/;
var ignored_patterns = [/forum\.inoreader\.com/, /translate\.inoreader\.com/, /inoreader\.com\/developers/, /inoreader\.com\/changelog/, /inoreader\.com\/login/, ]
var server_timeout = 30 * 1000; // 30 seconds
var get_server_interval = 300 * 1000; // 5 minutes
var old_data = {};
var old_badge_type;
var connect_timer;
var app_id = '1000001524';
var app_key = 'uxILTlPQilTphfcf4xwBEspL_0EDaUZL';

function init_icon(){
	var icon = (localStorage['icon']) ? localStorage['icon'] : 1;

	chrome.browserAction.setIcon({
		path: {
			"19": "icon"+icon+'.png',
			"38": "icon"+icon+"_r.png"
		}
	});

	console.log('Starting Inoreader Companion...');	

	if(localStorage['badge_type'] != '3'){
		chrome.browserAction.setBadgeText({text: '...'});
		chrome.browserAction.setBadgeBackgroundColor({color: '#EEEEEE'});
		chrome.browserAction.setTitle({title: 'Initializing...'});
	}else{
		chrome.browserAction.setBadgeText({text: ''});
	}
	
	localStorage['badge_type'] = localStorage['badge_type'] || 1;
}

function tab_scan(){
	find_tab(function(tab){
		if(tab === null){							
			console.log('Fetching data from servers');
			get_server_data();				
		}else{
			chrome.tabs.sendMessage(tab.id, {action: "force_counter_refresh"}, function(response) {
				console.log("Forced tab to reload counters",response);
			});  
		}
	});
}

function process_data(data){
	badge_update = false;

	if(localStorage['badge_type'] == '3'){
		chrome.browserAction.setBadgeText({text: ''});
		return true;
	}

	//console.log('Processing data ',data);	

	if(data == null || data.error != undefined){
		chrome.browserAction.setBadgeText({text: 'X'});
		chrome.browserAction.setBadgeBackgroundColor({color: '#DF0101'});
		var err_text = ' ';
		if(data != null && data.error){
			err_text = ' ' + data.error;
		}
		chrome.browserAction.setTitle({title: 'Cannot fetch data! ' + err_text});
		return;
	}

	if (localStorage['badge_type'] == 2 && data.unseen_cnt != old_data.unseen_cn){
		badge_update = true;	
	}
	else if (data.unread_cnt != old_data.unread_cnt){
			 badge_update = true;		
	}
	else if (old_badge_type != localStorage['badge_type']);
			 badge_update = true;
		
	if(badge_update || old_data.error != undefined){
		console.log('Data changed. Updating badge... ');
		
		var cnt = localStorage['badge_type'] == 2 ? parseInt(data.unseen_cnt) : parseInt(data.unread_cnt);
		var badge_bgcolor = (localStorage['badge_bgcolor']) ? localStorage['badge_bgcolor'] : '#0069D6';
		
		
		if(data.max_cnt > 1000){
			if(cnt >= 9999) cnt = "9999";
		}else{
			if(cnt >= 1000) cnt = "999+";
		}

		if(cnt != 0){
			chrome.browserAction.setBadgeText({text: cnt+'' || ''});
		}else{
			chrome.browserAction.setBadgeText({text: ''});
		}

		//chrome.browserAction.setBadgeText({text: cnt+'' || ''});
		chrome.browserAction.setBadgeBackgroundColor({color: badge_bgcolor});
		
		if(localStorage['badge_type'] == 2){
			chrome.browserAction.setTitle({title: 'You have ' + cnt + ' unseen articles in Inoreader'});
		}else{
			chrome.browserAction.setTitle({title: 'You have ' + cnt + ' unread articles in Inoreader'});
		}
	}
	old_data = data;
	old_badge_type = localStorage['badge_type'];

}

function get_server_data() {

	var data = null;

	if(localStorage['badge_type'] == '3'){
		process_data(data);
		return true;
	}

	connect_timer = setTimeout(function(){
        httpRequest.abort();
        chrome.browserAction.setBadgeText({text: ''});
    }, server_timeout);
	var httpRequest = new XMLHttpRequest();
	
	httpRequest.onerror = function(err){ console.log(err); }

	httpRequest.onreadystatechange = function() {
		if(httpRequest.readyState == 4){
			if(httpRequest.status >= 400){
				console.log('Err: ' + httpRequest.status + ' (' + httpRequest.statusText + ')');
			}else if(httpRequest.responseText){
				try{
					if(connect_timer) clearTimeout(connect_timer);
					data = JSON.parse(httpRequest.responseText);
					process_data(data);
				}catch(e){
					console.log(e);
				}
			}else{
				console.log('No data');
			}
		}
	}

	try{
		var session_key = '';
		var url = base_url;
		// if(localStorage['session_key']){
		// 	session_key = '&session_key=' + localStorage['session_key'];
		// 	url = base_url;
		// }
		var badge_type = (localStorage['badge_type']) ? localStorage['badge_type'] : 1;

		console.log('Requesting ' + url + 'api/browser_extensions.php?counters&type=' + badge_type + session_key);
		httpRequest.open('GET', url + 'api/browser_extensions.php?counters=1&AppId=' + app_id + '&AppKey=' + app_key + '&type=' + badge_type + session_key, true);
		httpRequest.send(null);
	}catch(e) {
		console.log(e);
	}	
}

function find_tab(callback_fn){
	chrome.windows.getAll({populate: true}, function(w){
		for(var i in w){
			var TabsCount = w[i].tabs.length;
			while(TabsCount){
				if(inoreader_url.exec(w[i].tabs[TabsCount-1].url)){
					for(var c in ignored_patterns){
						if(ignored_patterns[c].exec(w[i].tabs[TabsCount-1].url)){
							console.log(w[i].tabs[TabsCount-1].url);
							continue;
						}
					}
					callback_fn(w[i].tabs[TabsCount-1]);
					return;
				}
				--TabsCount
			}
		}		
		callback_fn(null);
	});
}

chrome.browserAction.onClicked.addListener(function(tab) {
	find_tab(function(tab){
		if(tab === null){
			localStorage['use_ssl'] = localStorage['use_ssl'] || 0;
			var this_url = (localStorage['use_ssl'] == 1) ? base_url : base_url_nossl;
			chrome.tabs.create({url: this_url});
		}else{
			chrome.tabs.update(tab.id, {selected: true});
		}
	});
});

chrome[runtimeOrExtension].onMessage.addListener(function(msg){
	if(msg.option_update){
		chrome.browserAction.setIcon({
			path: {
				"19": "icon"+(localStorage['icon'] ||  1)+'.png',
				"38": "icon"+(localStorage['icon'] ||  1)+"_r.png"
			}
		});
		get_server_data();
		return;
	}else if(msg.url){
        chrome.tabs.query({active:true, currentWindow:true}, function(active) {
            chrome.tabs.query({currentWindow:true},function(tabs) {
                var idx = 0;
                var ino_tab = (active && active.length) ? active[0].id : 0;
                for(var i in tabs){
                    var t = tabs[i];
                    if(t.active){
                    	idx = t.index;
                    }
                    if(t.openerTabId == ino_tab){
                    	idx = t.index;
                    }
                }
                console.log('Creating new tab ' + (idx+1) + ' with URL ' + msg.url);
				chrome.tabs.create({ url: msg.url, active: false, openerTabId: ino_tab, index: ++idx });
            });
        });
        return;
    }
	process_data(msg);
});

setInterval(function(){ tab_scan(); },get_server_interval);

init_icon();
tab_scan(); 
