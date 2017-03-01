/*
    All localStorage preferences:
        * hourFormat           => 12, 24
        * rollingAverage       => "true", "false"
        * rollingAveragePeriod => \d+
        * startPage            => 'search' or 'trends'
        * limiter              => 'limiter_all' or 'limiter_\d+'
        * autoBackup           => "true", "false"
        * autoBackupInterval   => \d+
        * autoBackupType       => "full", "incremental"
        * autoBackupCompression => "true", "false"
        * lastAutoBackupDate    => \d+ (ms since epoch)
*/
"use strict";

// The language argument is optional; it's only used by unit tests.
function Preferences( language ) {
    this._locale = language ? language : navigator.language;
    this._generateFormatters();
}

Preferences.prototype._generateFormatters = function() {
    var show12Hour = this.getHourFormat() == 12;
    var locale = this.getLocale();

    this._hourDisplayFormat    = new Intl.DateTimeFormat( locale, {hour: "2-digit", hour12: show12Hour} );
    this._weekdayDisplayFormat = new Intl.DateTimeFormat( locale, {weekday: "long"} );
    this._monthDisplayFormat   = new Intl.DateTimeFormat( locale, {month: "short"} );
    this._trendsDateDisplayFormat = new Intl.DateTimeFormat( locale, {weekday: "short", month: "long", day: "2-digit", year: "numeric"} );
    this._searchTimeStringFormat  = new Intl.DateTimeFormat( locale, {hour12: show12Hour, hour:"2-digit", minute:"2-digit", second:"2-digit"} );
    this._searchDateDisplayFormat = new Intl.DateTimeFormat( locale, {weekday:"long", month:"long", day:"2-digit", year:"numeric"} );

    this._numberDisplayFormat  = new Intl.NumberFormat( locale, {style: "decimal"} );
    this._percentDisplayFormat = new Intl.NumberFormat( locale, {style: "percent", maximumFractionDigits:"0"} );

    this._chartDailyFormat  = new Intl.DateTimeFormat( locale, {month: "short", day: "2-digit"} );
    this._chartMonthlyFormat = new Intl.DateTimeFormat( locale, {month: "short", year: "numeric"} );
    this._chartYearlyFormat   = new Intl.DateTimeFormat( locale, {year: "numeric"} );
};

Preferences.prototype.rollingAverage = function() {
    var enabled = window.localStorage.getItem("rollingAverage");
    return enabled == "true";
};

Preferences.prototype.setRollingAverage = function(value) {
    window.localStorage.setItem("rollingAverage", !!value);
};

Preferences.prototype.rollingAveragePeriod = function() {
    return window.localStorage.getItem("rollingAveragePeriod") || 14;
};

Preferences.prototype.setRollingAveragePeriod = function(value) {
    var number = parseInt(value);
    if (!isNaN(number) && number > 0) {
        window.localStorage.setItem("rollingAveragePeriod", number);
    }
};

Preferences.prototype.startPage = function() {
    return window.localStorage.getItem("startPage") || 'trends';
};

Preferences.prototype.setStartPage = function(value) {
    if (value == "trends" || value == "search") {
        window.localStorage.setItem("startPage", value);
    }
};

Preferences.prototype.getLimiter = function() {
    return window.localStorage.getItem("limiter") || "limiter_all";
};

Preferences.prototype.setLimiter = function(value) {
    if (value == "limiter_all" || value.match("^limiter_\\d+$") != null) {
        window.localStorage.setItem("limiter", value);
    }
};

Preferences.prototype.getLocale = function() {
    return this._locale;
};

Preferences.prototype.getHourFormat = function() {
    var hourFormat = window.localStorage.getItem("hourFormat");

    if (!hourFormat) {
        // Determine whether the user sees 12 or 24 hour time.
        var d = new Date();
        var timeDefault = d.toLocaleTimeString(this._locale);
        var time12Hour  = d.toLocaleTimeString(this._locale, {hour12: true});
        hourFormat = timeDefault == time12Hour ? 12 : 24;
        this.setHourFormat( hourFormat );
    }

    return hourFormat;
};

Preferences.prototype.setHourFormat = function(newFormat) {
    if (newFormat == 12 || newFormat == 24) {
        window.localStorage.setItem("hourFormat", newFormat);
        this._generateFormatters();
    }
};

Preferences.prototype.getHourDisplay = function(hour24) {
    var d = new Date();
    d.setHours( hour24, 0, 0 );
    return this._hourDisplayFormat.format(d);
};

// The baseDate argument is optional. It's only used by unit tests.
Preferences.prototype.getWeekdayDisplay = function(index, baseDate) {
    var d = baseDate ? baseDate : new Date();
    if (d.getDay() != index) {
        d.setDate( d.getDate() + (index - d.getDay()) );
    }
    return this._weekdayDisplayFormat.format(d);
};

Preferences.prototype.getMonthDisplay = function(index) {
    var d = new Date();
    d.setMonth( index, 1 );
    return this._monthDisplayFormat.format(d);
};

Preferences.prototype.getTrendsDateDisplay = function(milliseconds) {
    return this._trendsDateDisplayFormat.format( new Date(milliseconds) );
};

Preferences.prototype.getSearchDateDisplay = function(date) {
    return this._searchDateDisplayFormat.format( date );
};

Preferences.prototype.getSearchTimeString = function(date) {
    return this._searchTimeStringFormat.format(date);
};

Preferences.prototype.getNumberDisplay = function(number) {
    return this._numberDisplayFormat.format( number );
};

Preferences.prototype.getPercentDisplay = function(number) {
    return this._percentDisplayFormat.format( number );
};

Preferences.prototype.getChartDateString = function(date, granularity) {
    if (granularity == "yearly") {
        return this._chartYearlyFormat.format( date );
    }
    else if (granularity == "monthly") {
        return this._chartMonthlyFormat.format( date );
    }
    else {
        return this._chartDailyFormat.format( date );
    }
};

Preferences.prototype.autoBackup = function() {
    var enabled = window.localStorage.getItem("autoBackup");
    return enabled == "true";
};

Preferences.prototype.setAutoBackup = function(value) {
    window.localStorage.setItem("autoBackup", !!value);
};

Preferences.prototype.autoBackupInterval = function() {
    return window.localStorage.getItem("autoBackupInterval") || 7;
};

Preferences.prototype.setAutoBackupInterval = function(value) {
    var number = parseInt(value);
    if (!isNaN(number) && number > 0) {
        window.localStorage.setItem("autoBackupInterval", number);
    }
};

Preferences.prototype.autoBackupType = function() {
    return window.localStorage.getItem("autoBackupType") || 'full';
};

Preferences.prototype.setAutoBackupType = function(value) {
    if (value == "full" || value == "incremental") {
        window.localStorage.setItem("autoBackupType", value);
    }
};

Preferences.prototype.lastAutoBackupDate = function() {
    var milliseconds = window.localStorage.getItem("lastAutoBackupDate");
    if (milliseconds != null) {
        return new Date( parseInt(milliseconds) );
    }
    return;
};

Preferences.prototype.setLastAutoBackupDate = function(date) {
    if (date instanceof Date) {
        window.localStorage.setItem("lastAutoBackupDate", date.getTime());
    }
    else if ( date == undefined ) {
        window.localStorage.removeItem("lastAutoBackupDate");
    }
}

Preferences.prototype.autoBackupCompression = function() {
    var enabled = window.localStorage.getItem("autoBackupCompression");
    return enabled == "true";
};

Preferences.prototype.setAutoBackupCompression = function(value) {
    window.localStorage.setItem("autoBackupCompression", !!value);
};

