var CS_table=[];function CS_t(){}function getcsid(a,d){return("function"==typeof lp_sha256?lp_sha256:SHA256)(a+""+d)}function getcsinfo(a,d){if(null===a||null===d||"undefined"==typeof a||"undefined"==typeof d)return null;"number"==typeof a&&(a=a.toString());var b=getcsid(a,d);if(null===b)return null;b=CS_table[b];"undefined"==typeof b&&(b=null);return b}
function setcsinfo(a,d,b){if(null===a||null===d||!b||"undefined"==typeof a||"undefined"==typeof d||null===b.docnum||null===b.tabid||null===b.url)return null;"number"==typeof a&&(a=a.toString());a=getcsid(a,d);b.csid&&a&&a!=b.csid&&L("warn: csid!=obj.csid");if(null===b.csid||"undefined"==typeof b.csid)b.csid=a;b.last_ts=(new Date).getTime();"undefined"==typeof CS_table&&(CS_table=[]);CS_table[a]=b;return!0}
function register_new_cs(a,d,b,c,f,e){if(null===a||null===d||!b||"undefined"==typeof a||"undefined"==typeof d)return null;write_history({cmd:"register_new_cs",docnum:d,tabid:a,docstate:f});"undefined"==typeof f&&(f=null);"undefined"==typeof e&&(e=null);"number"==typeof a&&(a=a.toString());var h=0,g=new CS_t;g.docnum=d;g.tabid=a;g.url=b;g.lastfill_aid=0;g.parent_docnum=c?d:g_CS_tops[a];g.docstate=f;g.flags=null;null!==e&&"object"==typeof e&&(g.flags=e,"undefined"!=typeof e.domain&&(g.domain=e.domain,
delete g.flags.domain),"undefined"!=typeof e.origin&&(g.origin=e.origin,delete g.flags.origin));g.start_ts=(new Date).getTime();g_cpwbot&&CPWbot_bg&&(h=CPWbot_bg.score_frame(a,{url:b,docnum:d}));g.importance=h;return setcsinfo(a,d,g)}function delete_cs_for_docnum(a,d){if(null===a||null===d||"undefined"==typeof a||"undefined"==typeof d)return null;"number"==typeof a&&(a=a.toString());var b=!1,c;for(c in CS_table)CS_table.hasOwnProperty(c)&&c==getcsid(a,d)&&(b=!0,delete CS_table[c]);return b}
function delete_cs_for_tab(a){if(null===a||"undefined"==typeof a)return null;"number"==typeof a&&(a=a.toString());var d=!1,b;for(b in CS_table)if(CS_table.hasOwnProperty(b)){var c=CS_table[b];c&&c.tabid==a&&(d=!0,delete CS_table[b])}return d}
function update_cs_docstate(a,d){if(null===a||null===d||"undefined"==typeof a||"undefined"==typeof d)return null;"number"==typeof a&&(a=a.toString());var b=d.docnum;if(null===b||"undefined"==typeof b)return null;var c=d.docstate;if(null===c||"undefined"==typeof c)return null;var f=!1,e=getcsinfo(a,b);e&&(e.docstate=c,f=setcsinfo(a,b,e));return f}
function update_cs_timestamp(a,d){if(null===a||null===d||"undefined"==typeof a||"undefined"==typeof d)return null;"number"==typeof a&&(a=a.toString());var b=d.docnum;if(null===b||"undefined"==typeof b)return null;var c=getcsinfo(a,b);return c?setcsinfo(a,b,c):null}function count_cs_for_tabid(a){if(null===a||"undefined"==typeof a)return-1;"number"==typeof a&&(a=a.toString());var d=0,b,c;for(b in CS_table)CS_table.hasOwnProperty(b)&&(c=CS_table[b],c.tabid===a&&d++);return d}
function dumpinfo_for_tabid(a){if(null!==a&&"undefined"!=typeof a){"number"==typeof a&&(a=a.toString());var d,b,c;console.log("dumping info for tabid="+a);for(b in CS_table)CS_table.hasOwnProperty(b)&&(c=CS_table[b],d=(new Date).getTime(),c.tabid===a&&console.log("DUMPINFO [tabid:"+a+"][docnum:"+c.docnum+"] url="+c.url+" last_ts="+(d-c.last_ts)/1E3+"secs score="+c.importance+" killswitch?="+c.killswitch+" isTop?="+(c.parent_docnum==c.docnum?"true":"false")))}}
function find_docnum_for_tabid_by_url(a,d){if(null===a||null===d||"undefined"==typeof a||"undefined"==typeof d)return null;"number"==typeof a&&(a=a.toString());var b,c,f;for(b in CS_table)if(CS_table.hasOwnProperty(b)&&(f=CS_table[b],f.tabid===a&&f.url==d)){c=f.docnum;break}return c}
function set_killswitch_value(a,d,b){if(null===a||null===d||"undefined"==typeof a||"undefined"==typeof d)return null;"number"==typeof a&&(a=a.toString());var c=getcsinfo(a,d);return c?(c.killswitch=b,setcsinfo(a,d,c)):!1}function get_killswitch_value(a,d){if(null===a||null===d||"undefined"==typeof a||"undefined"==typeof d)return null;"number"==typeof a&&(a=a.toString());var b=getcsinfo(a,d),c=0;b&&(c=parseInt(b.killswitch),isNaN(c)&&(c=0));return c}
function skip_CS_by_score(a,d){if(null===a||null===d)return!0;var b=getcsinfo(a,d);if(!b)return!0;L("skip? [tabid="+a+"][docnum="+d+"] score="+b.importance);return-25>b.importance}function get_top_url(a,d){var b="",c=getcsinfo_top(a,d);if(c&&c.parent_docnum==c.docnum)b=c.url;else for(var f in CS_table)if(CS_table.hasOwnProperty(f)&&(c=CS_table[f],c.tabid===a&&c.parent_docnum==c.docnum)){b=c.url;break}return b}
function get_top_docnum(a,d){var b=0;i;var c=getcsinfo_top(a,d);if(c&&c.parent_docnum==c.docnum)b=c.docnum;else for(var f in CS_table)if(CS_table.hasOwnProperty(f)&&(c=CS_table[f],c.tabid===a&&c.parent_docnum==c.docnum)){b=c.docnum;break}return b}
function update_cs_docflags(a,d){if(null===a||null===d||"undefined"==typeof a||"undefined"==typeof d)return null;"number"==typeof a&&(a=a.toString());var b=d.docnum;if(null===b||"undefined"==typeof b)return null;var c=d.docflags,f=!1,e=getcsinfo(a,b);e&&"object"==typeof c&&(e.flags=c,"undefined"!=typeof c.domain&&(e.domain=c.domain,delete e.flags.domain),"undefined"!=typeof c.origin&&(e.origin=c.origin,delete e.flags.origin),f=setcsinfo(a,b,e));return f}
function getcsinfo_top(a,d){for(var b=null,c=-1,f=getcsinfo(a,d),e=null,h=0;10>h&&f;h++){if((e=getcsinfo(a,f.parent_docnum))&&f.parent_docnum==e.docnum){c=e.docnum;break}if(null==f.parent_docnum)break;f=e}0<=c&&(e=getcsinfo(a,c));e&&!isEmptyObject(e)&&(b=e);return b}
function update_cs_lastfill_aid(a,d){var b=!0;if(null===a||"undefined"==typeof a||"undefined"==typeof d)return!1;"number"==typeof a&&(a=a.toString());var c=get_top_docnum(a),f=getcsinfo(a,c);f&&(f.lastfill_aid=d,b=setcsinfo(a,c,f));return b}
function rebuild_cs_table(a){function d(b){if(0!=b.url.indexOf("chrome:")&&0!=b.url.indexOf("safari:")){var c=gettabid(b),d;for(d in g_CS[c]){var e=d,h=gettaburl(b),g=b.status,k=!1;d&&g_CS_tops[c]==d&&(k=!0);a?(g=getcsinfo(c,e))?(h!=g.url&&console.log(sprintf("[%s][%s] url mismatch %s != %s",c,e,h,g.url)),k!=(g.parent_docnum==g.docnum)&&console.log(sprintf("[%s][%s] isTop mismatch %s != %s ",c,e,h,k,g.parent_docnum==g.docnum))):console.log(sprintf("[%s][%s] seems like this frame exists but no entry found in CS table for it",
c,e)):register_new_cs(c,e,h,k,g,0)}}}get_all_windows({},function(a){var c,b;for(c=0;c<a.length;c++)if(g_ischrome)chrome.tabs.query({windowId:a[c].id},function(a){if(a&&0<a.length)for(var b=0;b<a.length;b++)d(a[b])});else{var e=get_tabs(a[c]);if(e&&"undefined"!=typeof e.length)for(b=0;b<e.length;b++)d(e[b])}})};