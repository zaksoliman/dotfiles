/* utils.js - a collection of utility methods */
"use strict";

var utils = {};

utils.stripSubDomain = function(host) {
    if (host == public_suffix.get_root_domain(host)) {
        return;
    }
    var matches = host.match(/^[^.]+\.([^.]+\..+)$/);
    return matches ? matches[1] : undefined;
}

/* 
    utils.getActiveTab()
    
    Returns the active tab.  Defaults to 'tab1' if no active tab is set. 
*/

utils.getActiveTab = function() {
    var activeTab = window.sessionStorage.getItem('activeTab');
    if (!activeTab || (activeTab != 'tab1' && activeTab != 'tab2' && activeTab != 'tab3')) {
        activeTab = 'tab1';
        window.sessionStorage.setItem('activeTab', activeTab);
    }
    return activeTab;
}

/*
    utils.getDomainPortion(host, baseDomain)
    
    For the given host, returns a portion of the domain name.
    If baseDomain is empty, returns the root domain.
    If baseDomain is populated, returns the baseDomain plus the next subdomain, if it exists.
    
    Examples:  
        utils.getDomainPortion('www.blog.example.com') returns 'example.com'
        utils.getDomainPortion('www.blog.example.com', 'example.com') returns 'blog.example.com'
        utils.getDomainPortion('www.blog.example.com', 'blog.example.com') returns 'www.blog.example.com'
*/

utils.getDomainPortion = function(host, baseDomain) {
    var domain;
    host = host || '';
    baseDomain = baseDomain || '';
    
    if (baseDomain.length) {
        var regex = new RegExp('[^.]*\\.?' + utils.escapeDots(baseDomain) + '$');
        var matches = host.match(regex);
        domain = matches ? matches[0] : undefined;
    }
    else {
        domain = public_suffix.get_root_domain(host);
    }
    
    return domain;
}


/*
    utils.extractHost(url)
  
    Returns the hostname portion of the given url.
    Example:
        utils.extractHost('http://www.example.com/?x=1') returns 'www.example.com'
 */
utils.extractHost = function(url) {
    var matches = url.match(/^.+?:\/\/(?:[^:@]+:[^@]+@)?([^\/?:]+)/);
    return matches ? matches[1] : undefined;
}

/*
 * utils.formatTitle(title)
 *
 * Replaces tabs and newlines/returns with a single space,
 * then removes consecutive whitespaces.
 *
 */

utils.formatTitle = function(title) {
    if (title == null) {
        return null;
    }

   title = title.replace(/[\t\r\n]/g, ' ');
   title = title.replace(/\s\s+/g, ' ');
    
   return title.trim();
}


utils.escapeDots = function(string) {
    return string.replace(/\./g, '\\.');
}


// Date obj to YYYY-MM-DD
utils.dateToString = function(date, separator) {
    if (separator == null) {
        separator = '-';
    }
    var year = date.getFullYear();
    var month = utils.padZero(date.getMonth() + 1);
    var day = utils.padZero(date.getDate());
    return year + separator + month + separator + day;
}

// Date obj to YYYY-MM-DD HH24:MI:SS.FFF
utils.dateToStringLong = function(date) {
    var year = date.getFullYear();
    var month = utils.padZero(date.getMonth() + 1);
    var day = utils.padZero(date.getDate());
    var hour = utils.padZero(date.getHours());
    var minute = utils.padZero(date.getMinutes());
    var second = utils.padZero(date.getSeconds());
    var millisecond = utils.padZero(date.getMilliseconds(), 3);
    return year + '-' + month + '-' + day + ' ' + hour + ':' + minute + ':' + second + '.' + millisecond;
}

utils.padZero = function(number, length) {
    var numString = number.toString();
    var zero = '0';
    length = length || 2;

    while(numString.length < length) {
        numString = zero + numString;
    }

    return numString;
}

// YYYY-MM-DD to Date obj
utils.stringToDate = function(string) {
    var year = string.substr(0,4);
    var month = parseInt(string.substr(5,2)) - 1;
    var day = string.substr(8,2);
    return new Date(year, month, day);
}

/* Statistics utilities */

utils.calcMedian = function(values) {
    var median;

    if (!values.length) {
        return;
    }
    else if (values.length % 2 == 0) {
        var middle = values.length / 2;
        var lower_middle = values[ middle - 1 ];
        var upper_middle = values[ middle ];
        median = utils.calcMean([lower_middle, upper_middle]);
    }
    else {
        var middle = Math.floor( values.length / 2 );
        median = values[middle];
    }

    return median;
}


utils.calcMean = function(values) {
    if (!values.length) {
        return;
    }
    
    var sum = 0;
    for (var i = 0; i < values.length; i++) {
        sum += values[i];
    }
    return( Math.round(sum/values.length, 0) );
}


/* Time Conversion utilities */

utils.logError = function(msg) {
    console.log( utils.getLogMessage('ERROR', msg) );
}

utils.logWarning = function(msg) {
    console.log( utils.getLogMessage('WARNING', msg) );
}

utils.logInfo = function(msg) {
    console.log( utils.getLogMessage('INFO', msg) );
}

utils.getLogMessage = function(prefix, msg) {
    var date = new Date();
    var dateString = 
        date.getFullYear() + '-' + 
        utils.padZero(date.getMonth() + 1) + '-' +
        utils.padZero(date.getDate()) + ' ' +
        utils.padZero(date.getHours()) + ':' +
        utils.padZero(date.getMinutes()) + ':' +
        utils.padZero(date.getSeconds()) + '.' +
        utils.padZero(date.getMilliseconds(), 3)
    ;
    return(dateString + ": " + prefix + ": " + msg);
}


/* Import/Export Utilities */

// As of 2013-03-02, chrome's transition types were located here:
// https://src.chromium.org/viewvc/chrome/trunk/src/content/public/common/
// in the following files:
//  * page_transition_types_list.h
//  * page_transition_types.cc
utils.convertTransitionToText = function(transition) {
    if (/^-?\d+$/.test(transition)) {
        var types = [
            "link", 
            "typed", 
            "auto_bookmark", 
            "auto_subframe", 
            "manual_subframe", 
            "generated", 
            "auto_toplevel", 
            "form_submit", 
            "reload", 
            "keyword", 
            "keyword_generated" 
        ];
        var typeId = transition & 0xFF;
        if (typeId < types.length) {
            return types[typeId];
        }
        else {
            utils.logWarning("import: unrecognized transition id: " + typeId);
            return "unknown";
        }
    }

    return transition;
}

utils.convertTextToTransition = function(text) {
    var definitions = {
        "link": 0,
        "typed": 1, 
        "auto_bookmark": 2,
        "auto_subframe": 3,
        "manual_subframe": 4,
        "generated": 5,
        "auto_toplevel": 6,
        "form_submit": 7,
        "reload": 8,
        "keyword": 9,
        "keyword_generated": 10 
    };
    return definitions[text];
}

// Converts: milliseconds since 1601-01-01 00:00:00 UTC 
// to:       milliseconds since 1970-01-01 00:00:00 UTC
utils.convertToUnixEpoch = function(visitTime) {
    return( (visitTime - 11644473600000000) / 1000 );
}

// Converts:       milliseconds since 1970-01-01 00:00:00 UTC
// To:             milliseconds since 1601-01-01 00:00:00 UTC 
utils.convertToWindowsEpoch = function(visitTime) {
    return( (visitTime * 1000) + 11644473600000000 );
}


utils.limiterToVisitTime = function( limiter, now_ms ) {
    var offset_ms;
    var one_day_ms = 24 * 60 * 60 * 1000;

    if (limiter == "limiter_all") {
        offset_ms = now_ms;
    }
    else if (limiter == "limiter_365") {
        offset_ms = 365 * one_day_ms;
    }
    else if (limiter == "limiter_180") {
        offset_ms = 180 * one_day_ms;
    }
    else if (limiter == "limiter_90") {
        offset_ms = 90 * one_day_ms;
    }
    else if (limiter == "limiter_30") {
        offset_ms = 30 * one_day_ms;
    }
    else if (limiter == "limiter_7") {
        offset_ms = 7 * one_day_ms;
    }

    return now_ms - offset_ms;
}


/* Search utils */

utils.highlightMatches = function(title, keywords) {
    var newKeywords = keywords.replace('\*','');
    var regex = new RegExp('(^|\\W)(' + newKeywords + ')', 'gi');
    return title.replace(regex, '$1<b>$2</b>');
};

utils.htmlEscape = function(str) {
    return String(str)
            .replace(/&/g, '&amp;')
            .replace(/"/g, '&quot;')
            .replace(/'/g, '&#39;')
            .replace(/</g, '&lt;')
            .replace(/>/g, '&gt;');
}

/* Backup utils */

utils.backupIsNeeded = function(prefs, today) {
    if ( !prefs.autoBackup() ) {
        return false;
    }

    var lastBackupDate = prefs.lastAutoBackupDate();
    var backupInterval = prefs.autoBackupInterval();

    if ( lastBackupDate == undefined ) {
        return true;
    }

    return utils.daysBetweenDates(lastBackupDate, today) >= backupInterval;
}

utils.daysBetweenDates = function(a, b) {
    var msPerDay = 60*60*24*1000;
    // Remove the time portion of each date.
    var a_copy = new Date( a.getFullYear(), a.getMonth(), a.getDate() );
    var b_copy = new Date( b.getFullYear(), b.getMonth(), b.getDate() );
    return Math.floor( Math.abs(b_copy.getTime() - a_copy.getTime()) / msPerDay );
}


