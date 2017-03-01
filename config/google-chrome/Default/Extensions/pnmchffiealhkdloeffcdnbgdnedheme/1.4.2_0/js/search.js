"use strict";

$(document).ready(function(){
    // Set up dialog box for search tips
    $( "#search_tips" ).dialog({
        autoOpen: false,
        height: 320,
        width: 600,
        modal: true,
        buttons: {
            Ok: function() {
                $( this ).dialog( "close" );
            }
        },
        open: function(e, ui) {
            ga('send','event','search','tips');
        }
    });
    $('#show_search_tips').click( function() { $('#search_tips').dialog( 'open' ); } );

    // Set up delete items confirmation.
    $('#confirm_delete').dialog({
        autoOpen: false,
        resizable: false,
        height: 150,
        modal: true,
        buttons: {
            Delete: function() {
                search.removeSelectedItems();
                search.submit({ disableReset: 1 });
                ga('send','event','delete','visit');
                $(this).dialog('close');
            },
            Cancel: function() {
                $(this).dialog('close');
            }
        }
    });

    $("#keywords").focus();

    // On startup, sync history and display browse results.
    websql.initAndSync(function() {
        $('#waiting_container').hide();
        $('#results').show();
        search.fetchResults();
    });

    // Perform search when clicking the Go button.
    $('#search').click(function() {
        ga('send','event','search','searchbar');
        search.submit();
    });

    // Perform search when pressing entery in keywords field.
    $('#keywords').keyup(function(e) {
        if (e.which == 13) {
            ga('send','event','search','searchbar');
            search.submit();
        }
    });

});

var search = {};

search.prefs      = new Preferences();
search.pageNumber = 1;
search.pageSize   = 100;
search.keywords   = '';
search.isDateSearch   = false;
search.nextPageExists = false;

search.submit = function(options) {
    if (!options || !options.disableReset) {
        search.reset();
    }
    search.keywords = $('#keywords').val();

    // Sync history before every search.
    websql.initAndSync(function() {
        search.fetchResults();
        if (search.keywords.length) {
            search.addClearButton();
        }
    });
};

search.reset = function() {
    search.pageNumber = 1;
    search.keywords   = '';
    search.isDateSearch   = false;
    search.nextPageExists = false;

    return;
};

search.fetchResults = function() {
    // Hide bottom pager row to prevent flickering when table is emptied.
    $('#search_result_controls_bottom').hide();
    $('#results').empty();

    if ( search.keywords.length && search.keywords.match(/^date:\d{4}-\d\d-\d\d$/) ) {
        search.isDateSearch = true;
    }

    var sqlAndBinds = search.getSqlAndBinds();

    websql.db.transaction(function(tx) {
        tx.executeSql(
            sqlAndBinds.sql,
            sqlAndBinds.binds,
            search.drawResults,
            websql.onError
        );
    });

};

search.getSqlAndBinds = function() {
    var sql;
    var binds;
    var offset = search.pageSize * (search.pageNumber - 1);
    var limit  = search.pageSize + 1;  // one more than needed, so we can snoop if a next page exists

    if (search.keywords.length) {
        if (!search.isDateSearch) {
            // SQL for searching by keyword
            sql =
                "SELECT u.url, u.title, v.visitid, v.visit_time FROM urls u INNER JOIN visits v ON u.urlid=v.urlid " +
                "WHERE u.urlid IN (" +
                    "SELECT docid FROM search_urls WHERE search_urls MATCH ?" +
                ") ORDER BY v.visit_time DESC LIMIT ? OFFSET ?"
            ;
            binds = [ 
                search.keywords,
                limit,
                offset 
            ];
        }
        else {
            // SQL for searching by date
            sql = 
                "SELECT u.url, u.title, v.visitid, v.visit_time FROM urls u INNER JOIN visits v ON u.urlid=v.urlid " +
                "WHERE v.visit_date = ? ORDER BY v.visit_time DESC LIMIT ? OFFSET ?"
            ;
            binds = [ 
                search.getDateString(),
                limit, 
                offset 
            ];
        }
    }
    else {
        // SQL for browsing history (by most recent first)
        sql = 
            "SELECT u.url, u.title, v.visitid, v.visit_time FROM urls u INNER JOIN visits v ON u.urlid=v.urlid " +
            "ORDER BY v.visit_time DESC LIMIT ? OFFSET ?"
        ;
        binds = [ limit, offset ];
    }

    return { 
        sql: sql, 
        binds: binds 
    };
};

search.drawResults = function(tx, rs) {

    // Use the pageSize + 1 trick set up by sqlAndBinds() to determine if another page exists.
    var resultCount = rs.rows.length;
    if ( resultCount > search.pageSize ) {
        search.nextPageExists = true;
        resultCount = search.pageSize;
    }
    else {
        search.nextPageExists = false;
    }

    var last_date = '';
    var date_link_index = 0;
    
    for (var i = 0; i < resultCount; i++) {
        var item = rs.rows.item(i);

        // When we reach a new day, display a header row with the full date.
        var dateObj = new Date(item.visit_time);
        var new_date = search.prefs.getSearchDateDisplay( dateObj );
        if (new_date != last_date) {
            ++date_link_index;
            var context = {
                newDate: new_date, 
                dateSearch: utils.dateToString( new Date(item.visit_time) ),
                index: date_link_index
            };
            $('#results').append( $('#template-browse-divider').render( context ) );
            last_date = new_date;
        }

        var title = item.title == null ? item.url : item.title;
        // The title is manually escaped, because highlighting
        // matching terms involves adding HTML bold tags.
        title = utils.htmlEscape(title);

        if (search.keywords.length && !search.isDateSearch) {
            title = utils.highlightMatches(title, search.keywords);
        }

        var row = {
            visitTime: search.prefs.getSearchTimeString(dateObj),
            title_encoded: title,
            url: item.url,
            host: utils.extractHost(item.url),
            visitId: item.visitid,
            index: i+1
        };
        $('#results').append( $('#template-result-row').render(row) );
    }

    if (resultCount) {
        search.drawSearchResultControls();

        // Apply click event to new_day dividers
        $('#results td.new_day a').click( search.dateSearchAction );

        // Apply click event to checkboxes
        $('#results input[type="checkbox"]').click( search.visitCheckboxAction );
    }
    else {
        // no results found
        search.removeSearchResultControls();
        var row = { keywords: search.keywords };
        $('#results').append( $('#template-no-results-row').render( row ) );
    }

    return;
};

search.removeSearchResultControls = function() {
    $('#pager_top').empty();
    $('#pager_bottom').empty();
    $('#search_desc').empty();
    return;
};

search.drawSearchResultControls = function() {
    search.removeSearchResultControls();

    // Draw the "Showing results for..." message.
    if (search.keywords.length) {
        $('#search_desc').append( $('#template-search-desc').render({ text: search.keywords }) );
    }

    var positions = ['top', 'bottom'];
    var is_single_page = search.pageNumber == 1 && !search.nextPageExists;

    // Create the buttons for removing visits and make it disabled.
    for (var i = 0; i < positions.length; i++) {
        var position    = positions[ i ];
        var divSelector = '#pager_' + position;
        var buttonId    = 'remove_items_' + position;
        $(divSelector).append(
            $('#template-button').render({ id: buttonId, text: 'Remove Selected Items' })
        );
        $('#' + buttonId)
            .prop( 'disabled', true )
            .click( function () { $('#confirm_delete').dialog('open'); } );

        // Don't draw the bottom control for single-page results.
        if (position == 'top' && is_single_page) {
            break;
        }
    }
    
    // Don't draw pager buttons for single-page results.
    if (is_single_page) {
        return;
    }

    for (var i = 0; i < positions.length; i++) {
        var position = positions[ i ];
        var divSelector = '#pager_' + position;

        // Older button first, since it floats rightmost
        var buttonId = 'older_' + position;
        $(divSelector).append( 
            $('#template-button').render({ id: buttonId, text: 'Older' }) 
        );
        if (!search.nextPageExists) {
            $('#' + buttonId).attr('disabled',true);
        }
        $('#' + buttonId).click( search.nextPageAction );

        // Newer button last
        buttonId = 'newer_' + position;
        $(divSelector).append( 
            $('#template-button').render({ id: buttonId, text: 'Newer' }) 
        );
        if (search.pageNumber == 1) {
            $('#' + buttonId).attr('disabled',true);
        }
        $('#' + buttonId).click( search.prevPageAction );

        // Bottom controls are hidden before fetching new results,
        // to prevent flickering.
        if (position == 'bottom') {
            $('#search_result_controls_bottom').show();
        }
    }
};


search.addClearButton = function() {
    // Skip if button already exists
    if ( $('#clear_search').length ) {
        return;
    }

    // Add clear button.
    $('#search_controls').append( 
        $('#template-button').render({ id: 'clear_search', text: 'Clear' }) 
    );

    // Install click handler.
    $('#clear_search').click( search.clearSearchAction );

    return;
};


search.nextPageAction = function() {
    ++search.pageNumber;
    search.fetchResults();
};

search.prevPageAction = function() {
    --search.pageNumber;
    search.fetchResults();
};

search.clearSearchAction = function() {
    search.reset();

    // Empty out keywords field
    $('#keywords')
        .val('')
        .focus()
    ;

    // Remove clear button
    $('#clear_search').remove();

    search.removeSearchResultControls();

    search.fetchResults();

    return;
};

search.dateSearchAction = function(e) {
    ga('send','event','search','datelink');
    $('#keywords').val('date:' + $(e.target).attr('data-datesearch'));
    search.submit();

    return;
};

search.removeSelectedItems = function() {
    $('#results input:checked').each(function(index) {
        var visitId = $(this).val();
        websql.db.transaction(function(tx) {
            tx.executeSql(
                'SELECT ' +
                    'u.urlid, ' +
                    'u.url, ' +
                    '(SELECT COUNT(*) FROM (SELECT visitid FROM visits WHERE urlid=u.urlid LIMIT 2)) as visit_count ' +
                'FROM urls u INNER JOIN visits v ON u.urlid=v.urlid ' +
                'WHERE v.visitid=?',
                [ visitId ],
                function(tx, rs) {
                    tx.executeSql('DELETE FROM visits WHERE visitid=?', [visitId], websql.onSuccess, websql.onError);
                    var row = rs.rows.item(0);
                    if (row.visit_count == 1) {
                        tx.executeSql(
                            'DELETE FROM urls WHERE urlid=?', 
                            [row.urlid], 
                            function (tx, rs) {
                                tx.executeSql('DELETE FROM search_urls WHERE url=?', [row.url], websql.onSuccess, websql.onError);
                            },
                            websql.onError
                        );
                    }
                },
                websql.onError
            );
        });
    });
};

search.visitCheckboxAction = function(e) {
    var positions = ['top', 'bottom'];
    for (var i in positions) {
        var selector = '#remove_items_' + positions[i];
        if (e.target.checked) {
            // Enable the remove button when a checkbox is checked.
            $(selector).prop('disabled',false);
        }
        else {
            // Disable the remove button when no more checkboxes are checked.
            if ($('#results input:checked').length == 0) {
                $(selector).prop('disabled',true);
            }
        }
    }
};

search.getDateString = function() {
    return search.keywords.replace('date:','');
};

search.getDateObj = function() {
    return utils.stringToDate( search.getDateString() );
};

