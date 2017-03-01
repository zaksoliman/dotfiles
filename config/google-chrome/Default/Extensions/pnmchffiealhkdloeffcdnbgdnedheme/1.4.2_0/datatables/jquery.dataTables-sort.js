jQuery.extend( jQuery.fn.dataTableExt.oSort, {
    "data-sort-numeric-pre": function ( a ) {
        var x = a.match(/data-sort="*(-?[0-9\.]+)/)[1];
        return parseFloat( x );
    },
 
    "data-sort-numeric-asc": function ( a, b ) {
        return ((a < b) ? -1 : ((a > b) ? 1 : 0));
    },
 
    "data-sort-numeric-desc": function ( a, b ) {
        return ((a < b) ? 1 : ((a > b) ? -1 : 0));
    }
} );

jQuery.extend( jQuery.fn.dataTableExt.oSort, {
    "data-sort-date-pre": function ( a ) {
        var x = a.match(/data-sort="(\d\d\d\d-\d\d-\d\d)"/)[1];
        var y = x.replace(/-/g,'');
        return parseInt( y );
    },
 
    "data-sort-date-asc": function ( a, b ) {
        return ((a < b) ? -1 : ((a > b) ? 1 : 0));
    },
 
    "data-sort-date-desc": function ( a, b ) {
        return ((a < b) ? 1 : ((a > b) ? -1 : 0));
    }
} );
