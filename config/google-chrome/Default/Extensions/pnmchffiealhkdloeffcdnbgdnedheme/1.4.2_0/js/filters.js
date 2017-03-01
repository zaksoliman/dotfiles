/* All filters:
     * url          (e.g., "http://www.example.com")
     * domain       (e.g., "example.com", or "news.example.com")
     * transition   (e.g., "link")
     * hour         (e.g., 0 - 23)
     * weekday      (e.g., 0 - 6)
     * monthday     (e.g., 1 - 31)
     * month        (e.g., 0-11)
     * date         (e.g., "Mon Jan 28 2013")
     * year         (e.g., "2013")
*/
"use strict";

function Filters() {
    this._filters = JSON.parse(window.sessionStorage.getItem("filters")) || {};
    // Convert milliseconds from string to int
    if (this._filters.date) {
        this._filters.date = parseInt(this._filters.date);
    }
}

/* 
    getAll()
    
    Returns an array of objects for all active filters.  
    Each object has the following attributes:
    * key   => filter name
    * value => filter value
    * displayValue  => The filter value formatted for displaying to the user.
*/

Filters.prototype.getAll = function() {
    var all = [];
    for (var key in this._filters) {
        var value = this._filters[key];
        all.push({ key: key, value: value });
    }
    return all.length ? all : null;
};

/* 
    set(key, value)

    Sets the value for the given filter name.
*/

Filters.prototype.set = function(key, value) {
    this._filters[key] = value;
    this.save();
};

/* 
    get(key)

    Returns the value for the given filter.
*/

Filters.prototype.get = function(key) {
    return (key in this._filters ? this._filters[key] : null);
};

/*
    save()

    Persists the current filter state to session storage
*/

Filters.prototype.save = function() {
    window.sessionStorage.setItem("filters", JSON.stringify( this._filters ));
};

/*
    remove(key)

    Deletes the value for the given filter. Changes are saved automatically.
*/

Filters.prototype.remove = function(key) {
    delete this._filters[key];
    this.save();
};

/*
    removeAll()

    Deletes all filters. Changes are saved automatically.
*/

Filters.prototype.removeAll = function() {
    this._filters = {};
    this.save();
};

/*
    getUrl()
    setUrl(url)

    Gets/sets the url filter.
*/

Filters.prototype.setUrl = function(value) {
    this.set("url", value);
};

Filters.prototype.getUrl = function() {
    return this.get("url");
};

/*
    getDomain()
    setDomain(domain)

    Gets/sets the domain filter.
*/

Filters.prototype.setDomain = function(value) {
    this.set("domain", value);
};

Filters.prototype.getDomain = function() {
    return this.get("domain");
};

/*
    getDate()
    setDate(date)

    Gets/sets the date filter.
*/
Filters.prototype.setDate = function(value) {
    this.set("date", value);
};

Filters.prototype.getDate = function() {
    return this.get("date");
};

/*
    getHour()
    setHour(hour)

    Gets/sets the hour filter.
*/

Filters.prototype.setHour = function(value) {
    this.set("hour", value);
};

Filters.prototype.getHour = function() {
    return this.get("hour");
};

/*
    getWeekday()
    setWeekday(weekday)

    Gets/sets the weekday filter.
*/

Filters.prototype.setWeekday = function(value) {
    this.set("weekday", value);
};

Filters.prototype.getWeekday = function() {
    return this.get("weekday");
};

/*

    getMonthday()
    setMonthday(monthday)

    Gets/sets the monthday filter.
*/

Filters.prototype.setMonthday = function(value) {
    this.set("monthday", value);
};

Filters.prototype.getMonthday = function() {
    return this.get("monthday");
};

/*
    getMonth()
    setMonth(month)

    Gets/sets the month filter.
*/

Filters.prototype.setMonth = function(value) {
    this.set("month", value);
};

Filters.prototype.getMonth = function() {
    return this.get("month");
};

/*
    getTransition()
    setTransition(transition)

    Gets/sets the transition filter.
*/

Filters.prototype.setTransition = function(value) {
    this.set("transition", value);
};

Filters.prototype.getTransition = function() {
    return this.get("transition");
};

/*
    getYear()
    setYear(year)

    Gets/sets the year filter.
*/

Filters.prototype.setYear = function(value) {
    this.set("year", value);
};

Filters.prototype.getYear = function() {
    return this.get("year");
};


