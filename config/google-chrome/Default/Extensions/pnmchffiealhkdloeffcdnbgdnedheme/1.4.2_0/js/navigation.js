"use strict";

$(document).ready(function(){
    $("#sidebar").append('<ul id="navigation">');
    var items = [
        { name: "Trends",    page: "trends.html" },
        { name: "Search",    page: "search.html" },
        { name: "Options",   page: "options.html" },
        { name: "Changelog", page: "changelog.html" },
    ];
    
    var nav = $("#navigation");
    for (var i = 0; i < items.length; i++) {
        var item = items[i];
        var id = 'nav_' + item.name.toLowerCase();
        var url = chrome.extension.getURL(item.page);
        var is_selected = window.location == url;

        var attrs = '';
        if (is_selected) {
            attrs = ' class="selected"';
        }
        nav.append('<li id="' + id + '"' + attrs + '>' + item.name + '</li>');

        if (!is_selected) {
            var getOnClickCallback = function(name, url) {
                return function() { 
                    if (name == "Trends") {
                        window.sessionStorage.clear();
                    }
                    window.location = url; 
                };
            };
            $('#' + id).click( getOnClickCallback(item.name, url) );
        }
    }
});
