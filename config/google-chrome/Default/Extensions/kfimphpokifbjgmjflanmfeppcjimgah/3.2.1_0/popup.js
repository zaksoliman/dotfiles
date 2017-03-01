var base_url = 'https://www.inoreader.com/';
var InoReaderUrl = /inoreader\.(com|ru)(.*)\/*$/;
var b_log = chrome.extension.getBackgroundPage();

var base_link = '';

var activeTab = new Array();
chrome.tabs.query({active: true, currentWindow: true}, function(arrayOfTabs) {
	activeTab = arrayOfTabs[0];
});

var urls = new Array();

if(localStorage['redirect'] == 3){
	urls[0] = '#';
	urls[1] = '#';
	urls[2] = '#';
	urls[3] = '#';
	urls[4] = base_url+'simple_api/?get_user_info&AppId=1000001524&AppKey=uxILTlPQilTphfcf4xwBEspL_0EDaUZL';
}else{
	urls[0] = base_url+'reader/api/0/subscription/list?AppId=1000001524&AppKey=uxILTlPQilTphfcf4xwBEspL_0EDaUZL';
	urls[1] = base_url+'reader/api/0/preference/stream/list?AppId=1000001524&AppKey=uxILTlPQilTphfcf4xwBEspL_0EDaUZL';
	urls[2] = base_url+'reader/api/0/tag/list?AppId=1000001524&AppKey=uxILTlPQilTphfcf4xwBEspL_0EDaUZL&';
	urls[3] = base_url+'reader/api/0/unread-count?AppId=1000001524&AppKey=uxILTlPQilTphfcf4xwBEspL_0EDaUZL&';
	urls[4] = base_url+'simple_api/?AppId=1000001524&AppKey=uxILTlPQilTphfcf4xwBEspL_0EDaUZL&get_user_info';
}

var result = new Array();
var doneCount = 0;

function get_info(folder){
	doneCount = 0;
	if(localStorage.use_ssl == 0){
		base_link += 'http://';
	}else{
		base_link += 'https://';
	}

	base_link += 'www.inoreader.com';
	if(localStorage['redirect'] == 2){
		find_tab(function(tab){
			if(tab === null){
				chrome.tabs.create({url: base_link});
			}else{
				chrome.tabs.update(tab.id, {url: base_link, selected: true});
			}
			window.close();
		});
	}

	html = '<div class="top"><div class="logo_login"><img src=\"logo_r.png\"/ width=\"40px\"></div> <div class="login_text">Loading ...</div><div class="loading_img"><img src=\"load_blue_16x16.gif\"/ ></div></div>';
	$("#tree").html(html);

	$.each(urls, function (i, url) {
		if(url != '#'){
			$.ajax({
			  url: url,
			  dataType: 'json',
			  success: function(json){
						doneCount++;
						result[i]= json;
						if (doneCount == urls.length) {
							process(folder,false);
						}
				},
				error: function(){

					if(window.devicePixelRatio >= 2) {
						html = '<div class="top"><div class="logo_login"><img src=\"logo_r.png\"/ width=\"40px\"></div> <div class="login_text">Please sign into your <a href="javascript://" id=\"logo_login\">Inoreader account</a></div></div>';
					}else{
						html = '<div class="top"><div class="logo_login"><img src=\"logo.png\"/ width=\"40px\"></div> <div class="login_text">Please sign into your <a href="javascript://" id=\"logo_login\">Inoreader account</a></div></div>';
					}
					
					$("#tree").html(html);

					$(".top").click(function() {
						find_tab(function(tab){
							if(tab === null){
								chrome.tabs.create({url: base_link});
							}else{
								chrome.tabs.update(tab.id, {url: base_link, selected: true});
							}
							window.close();
						});
					});

				}
			});
		}else{
			doneCount++;
		}
	});
}

function urlencode(str) {
  str = (str + '').toString();
  return encodeURIComponent(str).replace(/!/g, '%21').replace(/'/g, '%27').replace(/\(/g, '%28').replace(/\)/g, '%29').replace(/\*/g, '%2A').replace(/%20/g, '+');
}

function get_subscribe_info(url){
	if(url == ''){
		url = activeTab.url;
	}
	$.ajax({
	  url: base_url+'api/browser_extensions.php?AppId=1000001524&AppKey=uxILTlPQilTphfcf4xwBEspL_0EDaUZL&fetch_url='+urlencode(url),
	  dataType: 'json',
	  success: function(json){
			draw_feed(json);
		},
		error: function(){
			$("#add_button").html("<span class=\"icon-plus plus_button\"></span>");
			$("#subscribe_links").html('<div class="info">An error occured. Try again later.</div>');
		}
	});
}



function book_page(url){
	if(url == ''){
		url = activeTab.url;
	}
	$.ajax({
	  url: base_url+'reader/api/0/save-web-page?AppId=1000001524&AppKey=uxILTlPQilTphfcf4xwBEspL_0EDaUZL&url='+urlencode(url),
	  success: function(json){
			if(json == 'Error'){
				$("#save_button").html("<span class=\"icon-bookmark book_button\"></span>");
				$("#subscribe_links").html('<div class="info">Sorry, we couldn\'t save this web page.</div>');
			}else{
				$("#save_button").html("<span class=\"icon-bookmark_full book_button_save\"></span>");
			}
		},
		error: function(){
			$("#save_button").html("<span class=\"icon-bookmark book_button\"></span>");
			$("#subscribe_links").html('<div class="info">An error occured. Try again later.</div>');
		}
	});

	$(".save_web").click(function() {
		$("#save_button").html("<img class=\"loading\" src=\"load_blue_16x16.gif\"/>");
		book_page($('#feed_input').val());
	});

}


function add_feed(feed,url){
		$('#icon_'+feed).attr("class",'');
		$('#icon_'+feed).html("<img src=\"load_blue_16x16.gif\"/>");
		$.ajax({
		  url: base_url+'reader/api/0/subscription/quickadd?AppId=1000001524&AppKey=uxILTlPQilTphfcf4xwBEspL_0EDaUZL&quickadd='+url,
		  dataType: 'json',
		  success: function(json){
				$('#icon_'+feed).html("");
				$('#icon_'+feed).attr("class",'icon-checkmark_big add_icon');
				$('#'+feed).attr("class",'added_feed');
			},
			error: function(){
				$("#subscribe_links").html('<div>An error occured. Try again later.</div>');
			}
		});
}

function draw_feed(subsc_feeds){
	z = 0;
	html = "";
	add = 0;
	if(subsc_feeds.discovered_feeds){
		$.each(subsc_feeds.discovered_feeds, function (key, val) {
			encoded_link = urlencode(val.link);
			guid = $.guid++;
			if(val.subscribed == 0){
				html += '<div class="add_feed" id="'+guid+'" data="'+encoded_link+'"><div class="title"><div class="name">'+val.title+'</div><div class="link" title="'+val.link+'">'+val.link+'</div></div><div class="center_icon"><span id="icon_'+guid+'" class="icon-plus add_icon" ></span></div><div class="clear-both"></div></div>';
				add++;
				cur_guid = guid;
				cur_link = encoded_link
			}else{
				html += '<div class="added_feed" id="'+guid+'" data="'+encoded_link+'"><div class="title"><div class="name">'+val.title+'</div><div class="link" title="'+val.link+'">'+val.link+'</div></div><div class="center_icon"><span id="icon_'+guid+'" class="icon-checkmark_big add_icon" ></span></div><div class="clear-both"></div></div>';
			}
			z++;
		});
		html += '<div class="clear-both"></div>';
	}

	if(subsc_feeds && z==0 ){
		html = '<div class="info">No feeds found</div>';
	}

	if(add == 1){
		add_feed(cur_guid,cur_link);
	}

	$(".div_button").html("<span class=\"icon-plus plus_button\"></span>");
	$("#subscribe_links").html(html);

	$(".add_feed").click(function() {
		add_feed($(this).attr('id'),$(this).attr('data'));
	});

	window.scrollTo(0, 0);

	if($(document).height() > 600){
		scroll_workaround = false;
	}

	$('#feed_input').click(function() {
		$(this).select();
	});

	$(".plus_button").click(function() {
		$("#add_button").html("<img class=\"loading\" src=\"load_blue_16x16.gif\"/>");
		get_subscribe_info($('#feed_input').val());
	});

	$(".save_web").click(function() {
		$("#save_button").html("<img class=\"loading\" src=\"load_blue_16x16.gif\"/>");
		book_page($('#feed_input').val());
	});

}


function process(folder,subsc_feeds){
	start = 0;

	if(window.devicePixelRatio >= 2) {
		html = '<div class="top"><div class="logo"><img src=\"logo_r.png\"/ width=\"40px\"></div><input id="feed_searcher" type="text" value="" placeholder="Search my Inoreader"/> <span class="icon-loupe_big icon_search"></span></div>';
	}else{
		html = '<div class="top"><div class="logo"><img src=\"logo.png\"/ width=\"40px\"></div><input id="feed_searcher" type="text" value="" placeholder="Search my Inoreader"/> <span class="icon-loupe_big icon_search"></span></div>';
	}

	ss = ' value="'+activeTab.url+'" placeholder="placeholder"';
	tab_url = activeTab.url;

	if(tab_url == "chrome://extensions/"){
		ss = ' value="" placeholder="Enter URL here..."';
	}

	act_title = activeTab.title;

	if(act_title.length > 40 ){
		act_title = act_title.substring(0, 35)+'...';
	}

	if(tab_url.indexOf("https") != -1 || tab_url.indexOf("http") != -1){
		html += '<div class="feed_add"><div class="txt">Add feeds</div><input id="feed_input" type="text" '+ss+' /> <span class="div_button"><span id="add_button"><span class="icon-plus plus_button"></span></span><div class="clear-both"></div><div class="save_web"><div class="txt_1">Save this page</div><div class="title">'+act_title+'</div> <span class="div_button"><span id="save_button"><span class="icon-bookmark book_button"></span></span></span><div class="clear-both"></div></div></div>';	
	}else{
		html += '<div class="feed_add"><div class="txt">Add feeds</div><input id="feed_input" type="text" '+ss+' /> <span class="div_button"><span class="icon-plus plus_button"></span></div>';
	}



	html += '<div id="subscribe_links"></div>';


// Show only add bar

	if(localStorage['redirect'] == 3){
		$("#tree").html(html);

		$(".logo").click(function() {
			var feed = $(this).attr('data');

			find_tab(function(tab){
				if(tab === null){
					chrome.extension.getBackgroundPage().console.log('create');
					chrome.tabs.create({url: base_link});
				}else{
					chrome.tabs.update(tab.id, {url: base_link, selected: true});
				}
				window.close();
			});
		});
	
		$(".icon_search").click(function() {
			if($('#feed_searcher').val() == ''){
				$('#feed_searcher').focus().effect('highlight',{color: 'red'});
				return true;
			}

			find_tab(function(tab){
				if(tab === null){
					chrome.extension.getBackgroundPage().console.log('create');
					chrome.tabs.create({url: base_link+'/search/'+$('#feed_searcher').val()});
				}else{
					chrome.tabs.update(tab.id, {url: base_link+'/search/'+$('#feed_searcher').val(), selected: true});
				}
				window.close();
			});
		});

		$("#feed_searcher").keyup(function(event){
			if(event.keyCode == 13){
				if($('#feed_searcher').val() == ''){
					$('#feed_searcher').focus().effect('highlight',{color: 'red'});
					return true;
				}

				find_tab(function(tab){
					if(tab === null){
						chrome.extension.getBackgroundPage().console.log('create');
						chrome.tabs.create({url: base_link+'/search/'+$('#feed_searcher').val()});
					}else{
						chrome.tabs.update(tab.id, {url: base_link+'/search/'+$('#feed_searcher').val(), selected: true});
					}
					window.close();
				});
			}
		});


		$(".but_folder").click(function() {
			if(folder == null){
				get_info($(this).attr('id'));
			}else{
				get_info(null);
			}
		});

		$('#feed_input').click(function() {
			$(this).select();
		});

		$(".plus_button").click(function() {
			$("#add_button").html("<img class=\"loading\" src=\"load_blue_16x16.gif\"/>");
			get_subscribe_info($('#feed_input').val());
			
		});

		$(".save_web").click(function() {
			$("#save_button").html("<img class=\"loading\" src=\"load_blue_16x16.gif\"/>");
			book_page($('#feed_input').val());
		});

		$(".inoreader_link").click(function() {

			find_tab(function(tab){
				if(tab === null){
					chrome.extension.getBackgroundPage().console.log('create');
					chrome.tabs.create({url: base_link});
				}else{
					chrome.tabs.update(tab.id, {url: base_link, selected: true});
				}
				window.close();
			});
		});

		return true;
	}

	articles = '';
	articles += '<div id="new_articles">';

	articles += '<div class="new_articles_top">New articles:</div>';

	folder_cnt = new Array();

	folders = new Array();
	sorts = new Array();
	subscriptions = result[0]['subscriptions'];
	unreadcounts = new Array();
	css_folder = '';
	total_cnt = 0;

	$.each(result[3]['unreadcounts'], function (key, val) {
		if(val.count == 0){
			unreadcounts[val.id] = 0;
		}else
		if(val.count > 1000 ){
			unreadcounts[val.id] = val.count;
		}else{
			unreadcounts[val.id] = val.count;
		}
		
	});
	
	$.each(result[2]['tags'], function (key, val) {
		start++;
		if(start > 3){
			value=new Object();
			value.type = 'folder';
			value.id = val.id;
			value.sortid = val.sortid;
			value.title = val.id.replace('user/'+result[4]['id']+'/label/', '');
			value.categories = new Array();
			folders.push(value);
		}
	});

	if(result[4]['tree_ordering'] == 0){
		subscriptions.sort(function(a, b) {
			var x = a.title.toLowerCase(), y = b.title.toLowerCase();
			return x < y ? -1 : x > y ? 1 : 0;
		});

		folders.sort(function(a, b) {
			var x = a.title.toLowerCase(), y = b.title.toLowerCase();
			return x > y ? -1 : x < y ? 1 : 0;
		});

		$.each(folders, function (key, val) {
			subscriptions.unshift(val);
		});

		$.each(subscriptions, function (key, val) {
			if(val.categories.length == 0){
				if(val.type == 'folder'){
					guid = $.guid++;
					total_cnt++;
					child_folders = '';
					total_child_folders = 0;
					if(val.categories){
						$.each(subscriptions, function (_key, _val) {
							$.each(_val.categories, function (k, v) {
								if(v.id == val.id && unreadcounts[_val.id] > 0){
									total_child_folders++;
									f_guid = $.guid++;

									if((localStorage['original_fav'] == undefined || localStorage['original_fav'] == 1) && _val.iconUrl.indexOf("feed.png") == -1 && _val.iconUrl != ''){
										child_folders += '<div class="feed child_feed" data="'+_val.id+'" id="all_'+f_guid+'"><span class="icon16 feed_icon" data="'+_val.id+'"><img class="i_icon16" src="'+_val.iconUrl+'" border="0" alt="" title=""/></span><span class="feed_title feed_title_child" data="'+_val.id+'">'+_val.title+'</span><span title="Mark as reed" class="unread" data="'+_val.id+'" id="un_'+f_guid+'" f_data="'+guid+'">'+unreadcounts[_val.id]+'</span></div>';
									}else{
										child_folders += '<div class="feed child_feed" data="'+_val.id+'" id="all_'+f_guid+'"><span class="icon-rss_negative icon16 feed_icon" data="'+_val.id+'"></span><span class="feed_title feed_title_child" data="'+_val.id+'">'+_val.title+'</span><span title="Mark as reed" class="unread" data="'+_val.id+'" id="un_'+f_guid+'" f_data="'+guid+'">'+unreadcounts[_val.id]+'</span></div>';
									}


								}
							});
						});
					}
					
					if(unreadcounts[val.id]){
						folder_cnt[guid] = unreadcounts[val.id];
						if(total_child_folders > 0){
							articles += '<div class="folder" id="all_'+guid+'"><span id="icon_'+guid+'" data="'+guid+'" class="icon-arrow_right_small icon16 folder_i"></span><span data="'+val.title+'" class="feed_title_1 folder_title">'+val.title+'</span><span class="unread" id="un_'+guid+'" data="'+val.id+'">'+unreadcounts[val.id]+'</span></div>';
						}else{
							articles += '<div class="folder" id="all_'+guid+'"><span data="'+val.id+'" id="icon_'+guid+'" class="icon-tag_full icon16 folder_title"></span><span data="'+val.title+'" class="feed_title_1 folder_title">'+val.title+'</span><span class="unread" id="un_'+guid+'" data="'+val.id+'">'+unreadcounts[val.id]+'</span></div>';
						}

						articles += '<div class="child" id="col_'+guid+'">';

						articles += child_folders;
						articles += '</div>';
					}
				}else{
					if(unreadcounts[val.id] > 0){
						total_cnt++;
						guid = $.guid++;
						if((localStorage['original_fav'] == undefined || localStorage['original_fav'] == 1)  && val.iconUrl.indexOf("feed.png") == -1 && val.iconUrl != ''){
							articles += '<div class="feed" data="'+val.id+'" id="all_'+guid+'"><span class="icon16 feed_icon" data="'+val.id+'"><img class="i_icon16" src="'+val.iconUrl+'" border="0" alt="" title=""/></span><span class="feed_title" data="'+val.id+'">'+val.title+'</span><span title="Mark as reed" class="unread" data="'+val.id+'" id="un_'+guid+'" f_data="">'+unreadcounts[val.id]+'</span></div>';
						}else{
							articles += '<div class="feed" data="'+val.id+'" id="all_'+guid+'"><span class="icon-rss_negative icon16 feed_icon" data="'+val.id+'"></span><span class="feed_title" data="'+val.id+'">'+val.title+'</span><span title="Mark as reed" class="unread" data="'+val.id+'" id="un_'+guid+'" f_data="">'+unreadcounts[val.id]+'</span></div>';
						}
					}
				}			
			}
		});

	}else{
		sort_subscriptions = new Array();
		$.each(subscriptions, function (key, val) {
			sort_subscriptions[val.sortid] = val;
		});

		$.each(folders, function (key, val) {
			sort_subscriptions[val.sortid] = val;
		});
		i = 0;
		$.each(result[1]['streamprefs'], function (key, val) {
			sorts[key] = new Array();
			if(i>0){
				to = val[1].value.length/8;
				f = 0;
				for (z=0;z<to;z++){
					sorts[key][z] = val[1].value.substring(f,(f+8));
					f = f + 8;
				}

			}else{
				to = val[0].value.length/8;
				f = 0;
				for (z=0;z<to;z++){
					sorts[key][z] = val[0].value.substring(f,(f+8));
					f = f + 8;
				}
			}
			i++;
		});

		root = 'user/'+result[4]['id']+'/state/com.google/root';

		$.each(sorts[root], function (key, v) {
			val = sort_subscriptions[v];

			if(val.type == 'folder'){
				if(unreadcounts[val.id] > 0){
					guid = $.guid++;
					total_cnt++;
					child_folders = '';
					total_child_folders = 0;
					if(sorts[val.id]){
						$.each(sorts[val.id], function (_key, _v) {
							_val = sort_subscriptions[_v];
							if(unreadcounts[_val.id] > 0){
								total_child_folders++;
								f_guid = $.guid++;
								
								if((localStorage['original_fav'] == undefined || localStorage['original_fav'] == 1) && _val.iconUrl.indexOf("feed.png") == -1 && _val.iconUrl != ''){
									child_folders += '<div class="feed child_feed" data="'+_val.id+'" id="all_'+f_guid+'"><span class="icon16 feed_icon" data="'+_val.id+'"><img class="i_icon16" src="'+_val.iconUrl+'" border="0" alt="" title=""/></span><span class="feed_title feed_title_child" data="'+_val.id+'">'+_val.title+'</span><span title="Mark as reed" class="unread" data="'+_val.id+'" id="un_'+f_guid+'" f_data="'+guid+'">'+unreadcounts[_val.id]+'</span></div>';
								}else{
									child_folders += '<div class="feed child_feed" data="'+_val.id+'" id="all_'+f_guid+'"><span class="icon-rss_negative icon16 feed_icon" data="'+_val.id+'"></span><span class="feed_title feed_title_child" data="'+_val.id+'">'+_val.title+'</span><span title="Mark as reed" class="unread" data="'+_val.id+'" id="un_'+f_guid+'" f_data="'+guid+'">'+unreadcounts[_val.id]+'</span></div>';
								}
							}
						});
					}

					if(unreadcounts[val.id]){
						folder_cnt[guid] = unreadcounts[val.id];
						if(total_child_folders > 0){
							articles += '<div class="folder" id="all_'+guid+'"><span id="icon_'+guid+'" data="'+guid+'" class="icon-arrow_right_small icon16 folder_i"></span><span data="'+val.title+'" class="feed_title_1 folder_title">'+val.title+'</span><span class="unread" id="un_'+guid+'" data="'+val.id+'">'+unreadcounts[val.id]+'</span></div>';
						}else{
							articles += '<div class="folder" id="all_'+guid+'"><span data="'+val.id+'" id="icon_'+guid+'" class="icon-tag_full icon16 folder_title"></span><span data="'+val.title+'" class="feed_title_1 folder_title">'+val.title+'</span><span class="unread" id="un_'+guid+'" data="'+val.id+'">'+unreadcounts[val.id]+'</span></div>';
						}
						articles += '<div class="child" id="col_'+guid+'">';
						articles += child_folders;
						articles += '</div>';
					}
				}
			}else{
				if(unreadcounts[val.id] > 0){
					total_cnt++;
					guid = $.guid++;

					if((localStorage['original_fav'] == undefined || localStorage['original_fav'] == 1) && val.iconUrl.indexOf("feed.png") == -1 && val.iconUrl != ''){
						articles += '<div class="feed" data="'+val.id+'" id="all_'+guid+'"><span class="icon16 feed_icon" data="'+val.id+'"><img src="'+val.iconUrl+'" class="i_icon16" border="0" alt="" title=""/></span><span class="feed_title" data="'+val.id+'">'+val.title+'</span><span title="Mark as reed" class="unread" data="'+val.id+'" id="un_'+guid+'" f_data="">'+unreadcounts[val.id]+'</span></div>';
					}else{
						articles += '<div class="feed" data="'+val.id+'" id="all_'+guid+'"><span class="icon-rss_negative icon16 feed_icon" data="'+val.id+'"></span><span class="feed_title" data="'+val.id+'">'+val.title+'</span><span title="Mark as reed" class="unread" data="'+val.id+'" id="un_'+guid+'" f_data="">'+unreadcounts[val.id]+'</span></div>';
					}
				}
			}
		});
	}

	if(total_cnt == 0){
		articles = '<div id="new_articles">';
		articles = '<div class="new_articles_top">No new articles</div>';
	}


	articles += '</div>';

	html += articles;


	$("#tree").html(html);

	$(".feed_icon").click(function() {
		var feed = $(this).attr('data');
		feed = feed.replace(/^feed\//,'');
		find_tab(function(tab){
			if(tab === null){
				chrome.extension.getBackgroundPage().console.log('create');
				chrome.tabs.create({url: base_link+'/feed/'+encodeURIComponent(feed)});
			}else{
				chrome.tabs.update(tab.id, {url: base_link+'/feed/'+encodeURIComponent(feed), selected: true});
			}
			window.close();
		});
	});

	$(".feed_title").click(function() {
		var feed = $(this).attr('data');
		feed = feed.replace(/^feed\//,'');
		find_tab(function(tab){
			if(tab === null){
				chrome.extension.getBackgroundPage().console.log('create');
				chrome.tabs.create({url: base_link+'/feed/'+encodeURIComponent(feed)});
			}else{
				chrome.tabs.update(tab.id, {url: base_link+'/feed/'+encodeURIComponent(feed), selected: true});
			}
			window.close();
		});
	});

	var scroll_workaround = true;
	if($(document).height() > 600){
		scroll_workaround = false;
	}

	$(".unread").click(function(){
		id = $(this).attr('id');
		cnt = unread_tab[id];
		all_id = id.replace('un_', '');
		data = $(this).attr('data');
		f_data=$(this).attr('f_data');

		$.ajax({
		  url: base_url+'reader/api/0/mark-all-as-read?AppId=1000001524&AppKey=uxILTlPQilTphfcf4xwBEspL_0EDaUZL&s='+urlencode(data),
		  success: function(){
				if(folder_cnt[f_data]){
					folder_cnt[f_data] = folder_cnt[f_data] - cnt;
					if(folder_cnt[f_data] < 1){
						$('#all_'+f_data).remove();
					}else{
						$('#un_'+f_data).html(folder_cnt[f_data]);
					}
				}

				$('#all_'+all_id).remove();
				$('#col_'+all_id).remove();
				h = $('#tree').height();
				if(h < 600 && !scroll_workaround){
					b = 600 - $(document).height();
					$('#bootom').height(b);
					$('#bootom').show(b);
					scroll_workaround = true;
				}
			},
			error: function(){
			}
		});

	});

	var unread_tab = new Array();

	$(".unread").mouseover(function(){
		feed = $(this).attr('id');
		unread_tab[feed] = $('#'+feed).html();
		$('#'+feed).html('');
		$('#'+feed).attr("class",'unread icon-checkmark_big checked_icon');
	});

	$(".unread").mouseout(function() {
		feed = $(this).attr('id');
		$('#'+feed).attr("class",'unread');
		$('#'+feed).html(unread_tab[feed]);
	});
	
	$(".folder_i").click(function() {
		folder_id = $(this).attr('data');
		if($('#icon_'+folder_id).attr('class') == 'icon-arrow_right_small icon16 folder_i'){
			$('#icon_'+folder_id).attr('class','icon-arrow_down_small icon16 folder_i');
			$('#col_'+folder_id).show();
		}else{
			$('#icon_'+folder_id).attr('class','icon-arrow_right_small icon16 folder_i');
			$('#col_'+folder_id).hide();
		}
		if($(document).height() > 600){
			scroll_workaround = false;
		}
	});


	$(".folder_title").click(function() {
		var feed = $(this).attr('data');

		find_tab(function(tab){
			if(tab === null){
				chrome.extension.getBackgroundPage().console.log('create');
				chrome.tabs.create({url: base_link+'/folder/'+encodeURIComponent(feed)});
			}else{
				chrome.tabs.update(tab.id, {url: base_link+'/folder/'+encodeURIComponent(feed), selected: true});
			}
			window.close();
		});
	});


	$(".logo").click(function() {
		var feed = $(this).attr('data');

		find_tab(function(tab){
			if(tab === null){
				chrome.extension.getBackgroundPage().console.log('create');
				chrome.tabs.create({url: base_link});
			}else{
				chrome.tabs.update(tab.id, {url: base_link, selected: true});
			}
			window.close();
		});
	});



	
	$(".icon_search").click(function() {
		if($('#feed_searcher').val() == ''){
			$('#feed_searcher').focus().effect('highlight',{color: 'red'});
			return true;
		}

		find_tab(function(tab){
			if(tab === null){
				chrome.extension.getBackgroundPage().console.log('create');
				chrome.tabs.create({url: base_link+'/search/'+$('#feed_searcher').val()});
			}else{
				chrome.tabs.update(tab.id, {url: base_link+'/search/'+$('#feed_searcher').val(), selected: true});
			}
			window.close();
		});
	});

	$("#feed_searcher").keyup(function(event){
		if(event.keyCode == 13){
			if($('#feed_searcher').val() == ''){
				$('#feed_searcher').focus().effect('highlight',{color: 'red'});
				return true;
			}

			find_tab(function(tab){
				if(tab === null){
					chrome.extension.getBackgroundPage().console.log('create');
					chrome.tabs.create({url: base_link+'/search/'+$('#feed_searcher').val()});
				}else{
					chrome.tabs.update(tab.id, {url: base_link+'/search/'+$('#feed_searcher').val(), selected: true});
				}
				window.close();
			});
		}
	});


	$(".but_folder").click(function() {
		if(folder == null){
			get_info($(this).attr('id'));
		}else{
			get_info(null);
		}
	});

	$('#feed_input').click(function() {
		$(this).select();
	});

	$(".plus_button").click(function() {
		$("#add_button").html("<img class=\"loading\" src=\"load_blue_16x16.gif\"/>");
		get_subscribe_info($('#feed_input').val());
		
	});

	$(".save_web").click(function() {
		$("#save_button").html("<img class=\"loading\" src=\"load_blue_16x16.gif\"/>");
		book_page($('#feed_input').val());
	});

	$(".inoreader_link").click(function() {

		find_tab(function(tab){
			if(tab === null){
				chrome.extension.getBackgroundPage().console.log('create');
				chrome.tabs.create({url: base_link});
			}else{
				chrome.tabs.update(tab.id, {url: base_link, selected: true});
			}
			window.close();
		});
	});

}

function find_tab(callback_fn){
	var current_id = null;
	chrome.windows.getCurrent(function(win){
		current_id = win.id;
	});

	chrome.windows.getAll({populate: true}, function(w){
		for(var i in w){
			var TabsCount = w[i].tabs.length;
			while(TabsCount){	
				if(InoReaderUrl.exec(w[i].tabs[TabsCount-1].url) && w[i].tabs[TabsCount-1].url.indexOf('forum') === -1 && w[i].tabs[TabsCount-1].url.indexOf('wiki') === -1 && w[i].id == current_id){
					chrome.extension.getBackgroundPage().console.log(w[i].tabs[TabsCount-1]);
					callback_fn(w[i].tabs[TabsCount-1]);
					return;
				}
				--TabsCount
			}
		}
		callback_fn(null);
	});
}

get_info(null);