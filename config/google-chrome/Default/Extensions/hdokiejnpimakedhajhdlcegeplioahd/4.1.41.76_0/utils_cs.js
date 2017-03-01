function LP_objectSize(a){var b,c=0;if(null==a||"undefined"==typeof a)return 0;if("function"!=typeof a.hasOwnProperty&&"undefined"!=typeof a.length)return a.length;for(b in a)a.hasOwnProperty(b)&&c++;return c}function LP_getComputedStyle(a,b){return b&&a?"undefined"!=typeof a.getComputedStyle?g_isfirefox?a.getComputedStyle(b,null):a.getComputedStyle(b):b.currentStyle:null}
function LP_getloggedin(){return g_isie?init_LPfn()&&LPfn?LPfn.get_loginstate():!1:"undefined"!=typeof g_isloggedin?g_isloggedin:lploggedin}function LP_setloggedin(a){"string"==typeof a&&"0"===a&&(a=!1);if(g_isie)return init_LPfn()&&LPfn?LPfn.set_loginstate(a):!1;if("undefined"!=typeof g_isloggedin)g_isloggedin=a?!0:!1;else if("undefined"!=typeof lploggedin)lploggedin=a?!0:!1;else return!1;return!0}function get_win_self(a){return"undefined"!=typeof g_isfirefoxsdk&&g_isfirefoxsdk?a:a.self}
function LP_get_live_style(a){if(!a||!a.ownerDocument||!g_isie&&!a.ownerDocument.defaultView)return null;var b;if(g_isie&&a.currentStyle)b=a.currentStyle;else try{b=a.ownerDocument.defaultView.getComputedStyle(a,"")}catch(c){"undefined"!=typeof a.currentStyle&&(b=a.currentStyle)}return b}
function LP_elt_get_text(a){if(!a)return"";if("undefined"!=typeof g_isfirefox&&g_isfirefox){if(null!=a.textContent)return a.textContent}else return"undefined"!=typeof a.textContent&&null!=a.textContent?a.textContent:get_innertext(a);return""}function LP_elt_set_text(a,b){if(!a)return!1;"undefined"!=typeof g_isfirefox&&g_isfirefox?a.textContent=b:set_innertext(a,b);return!0}
function parse_zindex(a){if("string"==typeof a&&-1!=a.indexOf("e")){a=""+parseFloat(a);for(var b="",c=a.length-1;0<=c&&"0"==a.charAt(c);c--)b+="9";a=a.substring(0,c+1)+b}return parseInt(a)}
function LP_getAbsolutePos(a,b,c,d){LPCTR("getAbsolutePos");if(!a||!b)return null;if("undefined"==typeof c||null===c)c=!1;if("undefined"==typeof d||null===d)d=!1;var h=null;try{h=typeof b.getBoundingClientRect}catch(p){h=null}if(h&&"undefined"!=h)try{var f=b.getBoundingClientRect(),e,k;e="undefined"==typeof f.width?f.right-f.left:f.width;k="undefined"==typeof f.height?f.bottom-f.top:f.height;var l=LP_get_cached_body_rect(a),h=b=0,g=LP_get_cached_body_style(a);g_bodystyle_relative&&l&&g&&"relative"==
g.position&&!d&&(b=l.left,h=l.top);g=d=0;if(!c){var n="undefined"!=typeof window&&window?window:a.defaultView;if("pageXOffset"in n)var q=n.pageXOffset,r=n.pageYOffset;else{var m;c=1;"undefined"!=typeof g_isie&&g_isie&&"undefined"!=typeof a.querySelector&&"undefined"==typeof a.addEventListener?m=1:(l&&(c=Math.round((l.right-l.left)/a.body.offsetWidth*100)/100),m=c);q=Math.round(a.documentElement.scrollLeft/m);r=Math.round(a.documentElement.scrollTop/m)}d=q;g=r}return{left:f.left+d-b,top:f.top+g-h,
width:e,height:k}}catch(p){return"undefined"!=typeof write_error_to_history&&write_error_to_history(a,"getAbsolutePos",p),null}else return null}
function LP_measureText(a,b,c,d,h){var f=LP_measureTextCacheGet(a,b,d);if(null!=f)return f;var f=a.createElement("span"),e=null;if(null==d&&null==c)return e;null==h&&(h=a.body);null==h&&(h=a.getElementById("hiddenel"));h&&(h.appendChild(f),null!=d&&(f.style.cssText=d),null!=c&&(f.style.fontSize=""+c+"px"),f.style.position="absolute",f.style.left="-1000px",f.style.top="-1000px",LP_elt_set_text(f,b),e={width:f.clientWidth,height:f.clientHeight},LP_measureTextCacheSet(a,b,d,e),h.removeChild(f));return e}
function lp_url_is_lastpass(a){if(null==a)return!1;var b="https://lastpass.com/";"undefined"!=typeof base_url&&(b=base_url);return 0==a.indexOf(b)||0==a.indexOf("https://lastpass.com/")||0==a.indexOf("https://lastpass.eu/")?!0:"undefined"!=typeof g_loosebasematching?(a=lp_gettld_url(a),(new RegExp(a+"/$")).test(base_url)):/^https:\/\/([a-z0-9-]+\.)?lastpass\.(eu|com)\//i.test(a)}
function LP_getElementByIdOrName(a,b,c,d){function h(a){return a&&a.tagName&&"INPUT"==a.tagName.toUpperCase()?!0:!1}"document"==a&&(a=document);a||(a=LP_derive_doc());if(null==a||null==b||0==b.length||"undefined"==typeof a.getElementsByTagName)return null;c=c?c.toUpperCase():"INPUT";c.toLowerCase();for(var f=[],f=c&&"INPUT"!=c&&!g_isie?LP_getElementsByXPath(a,sprintf("*[@id='%s' or @name='%s']",b,b)):LP_getAllInputsByIdOrName(a,b,!0),e=[],k=0;k<f.length;k++)e[e.length]=f[k];var k=e.length,l=a.getElementById(b);
if(null!=l){if(0==k||1==k&&(f[0]==l||null==f[0]))return l;e[e.length]=l}if(LP_name_is_inputidx(a,b)&&(b=LP_getinputidx_from_name(a,b),null!==b&&(l=LP_getElementByIdx(a,b),null!=l)))return l;if(1==k)return e[0];if(0==k)return null;b=[];f=0;k=-1;if("undefined"==typeof Math)return null;for(var l=Math.floor(1E4*Math.random()),g=0;g<e.length;g++)b[g]=0,e[g].tagName.toUpperCase()==c&&(b[g]+=20,h(e[g])&&"hidden"!=e[g].type&&(b[g]+=10)),null!=e[g].style&&"none"!=e[g].style.display&&checkIsDisplayed(a,e[g],
0,null,l,!0)&&(b[g]+=5),h(e[g])&&inputHasLPBackground(e[g])&&(b[g]+=7),null!=e[g].style&&"hidden"!=e[g].style.visibility&&(b[g]+=3),e[g]==g_popupfill_parent&&null!=g_popupfill_parent?b[g]+=5:e[g]==a.g_popupfill_parent&&null!=a.g_popupfill_parent&&(b[g]+=5),d&&e[g].form&&LP_getname(e[g].form)==d&&(b[g]+=20),b[g]>f&&(f=b[g],k=g);return 0<=k?e[k]:null}
function LP_getAllInputsByIdOrName(a,b,c){if(null==a||null==b||0==b.length||"undefined"==typeof a.getElementsByTagName)return null;var d=!0;null!==c&&(d=c);var h=a.getElementsByName(b);c=[];for(var f=0;f<h.length;f++)c[c.length]=h[f];if(d)for(a=a.getElementsByTagName("input"),h=a.length,h>MAX_INPUTS_SOFT&&(h=MAX_INPUTS_SOFT),d=0;d<h;d++)a[d].id==b&&c.push(a[d]);else(b=LP_getElementByIdOrName(a,b))&&c.push(b);return c}
function isEmptyObject(a){"undefined"==typeof Object.keys&&(Object.keys=function(){var a=Object.prototype.hasOwnProperty,c=!{toString:null}.propertyIsEnumerable("toString"),d="toString toLocaleString valueOf hasOwnProperty isPrototypeOf propertyIsEnumerable constructor".split(" "),h=d.length;return function(b){if("object"!==typeof b&&("function"!==typeof b||null===b))throw new TypeError("Object.keys called on non-object");var e=[],f;for(f in b)a.call(b,f)&&e.push(f);if(c)for(f=0;f<h;f++)a.call(b,
d[f])&&e.push(d[f]);return e}}());return 0===Object.keys(a).length}function LP_getname(a,b){if("undefined"!=typeof a&&null!=a){if(b&&"string"==typeof a.id&&""!=a.id)return a.id;if("string"==typeof a.name&&""!=a.name)return a.name;if("string"==typeof a.id)return a.id}return""}
function LP_getWindowWidth(a,b){LPCTR("windowWidth");if(!a)return 0;if(a===LP_derive_doc())return console_warn("ERROR: expected win, got doc"),0;var c=a.innerWidth;if("undefined"==typeof c||null===c||isNaN(c))try{var d=a.document.documentElement;d&&d.clientWidth||(d=a.document.body);d.clientWidth&&(c=parseInt(d.clientWidth),isNaN(c)&&(c=0))}catch(l){c=0}var d=c,h=a.document;if(!h)return 0;var f=LP_get_body_reference(h),e=LP_get_cached_body_style(h);g_bodystyle_relative&&e&&"relative"==e.position&&
(f=h.documentElement);e=h.getElementById("_lpinvis");null==e&&(e=h.createElement("div"),e.id="_lpinvis",e.style.left="0px",e.style.right="0px",e.style.top="0px",e.style.height="0px",e.style.visibility="hidden",e.style.position="absolute",e.setAttribute("aria-hidden","true"),f.appendChild(e));var k=LP_getComputedStyle("undefined"!=typeof window&&window?window:h.defaultView,f);if(!k)return 0;h=parseInt(k.marginLeft);k=parseInt(k.marginRight);0<e.offsetWidth&&4*e.offsetWidth<c&&(d=!(0<h||0<k)||isNaN(h)||
isNaN(k)?e.offsetWidth:e.offsetWidth+k+h);b||f.removeChild(e);return d}
function LP_getWindowHeight(a,b){null==b&&LP_derive_doc();if(!a)return 0;try{var c=parseInt(a.innerHeight);if("undefined"==typeof c||null===c||isNaN(c)||0>=c){if("undefined"!=typeof a.jQuery)return $(a).height();var d=a.document.documentElement;d&&d.clientHeight||(d=a.document.body);return d.clientHeight?(c=parseInt(d.clientHeight),isNaN(c)&&(c=0),c):0}}catch(h){return verbose_log("getWindowHeight failed, "+h.message),0}0>c&&(c=0);return c}
function LP_pos_viewport(a){if(!a)return[0,0];var b=0,c,d=null;a.document&&(d=a.document.documentElement,d||(d=a.document.body));c=a.pageYOffset?parseInt(a.pageYOffset):d&&d.scrollTop?parseInt(d.scrollTop):0;a.pageXOffset?b=parseInt(a.pageXOffset):d&&d.scrollLeft&&(b=parseInt(d.scrollLeft));isNaN(b)&&(b=0);isNaN(c)&&(c=0);return[b,c]}
function LP_getname_or_idx(a,b,c){if(!a&&(a=document,!a))return"";c=LP_getname(b,c);if(""===c||null===c)if("INPUT"==b.tagName||"input"==b.tagName)c=LP_inputidx_to_name(a,LP_getinputidx(a,b));return c}function LP_getinputidx(a,b){if(!a&&(a=document,!a))return"";var c=a.getElementsByTagName("INPUT"),d;for(d=0;d<c.length;d++)if(c[d]==b)return d;return""}var LPMAGICINPUTIDX="input_idx_";function LP_inputidx_to_name(a,b){if(null!==b&&is_valid_input_indexes(a))return LPMAGICINPUTIDX+b}
function LP_name_is_inputidx(a,b){return b&&0==b.indexOf(LPMAGICINPUTIDX)&&b.length>LPMAGICINPUTIDX.length?!0:!1}function LP_getinputidx_from_name(a,b){return is_valid_input_indexes(a)&&0==b.indexOf(LPMAGICINPUTIDX)?b.substr(LPMAGICINPUTIDX.length):null}function LP_getElementByIdx(a,b){var c=null;is_valid_input_indexes(a)&&(c=a.getElementsByTagName("INPUT")[b]);return c}function invalidate_input_indexes(a){if(!a&&(a=document,!a))return;a.g_need_to_recompute_input_index=!0}
function validate_input_indexes(a){if(!a&&(a=document,!a))return;a.g_need_to_recompute_input_index=!1}function is_valid_input_indexes(a){return!0}function LP_get_body_reference(a){return a?"undefined"!=typeof a.body?a.body:a.getElementById("main")?a.getElementById("main"):a.documentElement:null}
function LP_get_cached_body_style(a){if(!a)return null;var b=LP_get_body_reference(a),c=null;"undefined"==typeof a.g_posbodystyle_cache?b&&(c=(c="undefined"!=typeof window&&window?window:a.defaultView)&&"undefined"!=typeof c.getComputedStyle?c.getComputedStyle(b,null):b.currentStyle,a.g_posbodystyle_cache=c):c=a.g_posbodystyle_cache;return c}
function LP_get_cached_body_rect(a){if(!a)return null;var b;b=LP_get_body_reference(a);"undefined"==typeof a.g_posbodyrect_cache&&b?(b=b.getBoundingClientRect(),a.g_posbodyrect_cache=b):b=a.g_posbodyrect_cache;return b}function LP_derive_doc(){var a;return(a="undefined"!=typeof g_isfirefox&&g_isfirefox&&LP?LP.getBrowser().contentDocument:document)?a:null}
function LP_is_inframe(a){if(!a)return!1;try{var b="undefined"!=typeof window&&window?window:a.defaultView;return get_win_self(b)!==b.top}catch(c){return!1}}function LP_pickFieldName(a,b){return a&&b?LP_getname_or_idx(a,b,LP_GETNAME_FAVOR_ID_OVER_NAME):null}
function LP_fieldGetWidth(a){if(!a)return 0;var b={},c=0;if("undefined"!=typeof g_isie&&g_isie){if("undefined"!=typeof a.offsetWidth&&(c=parseInt(a.offsetWidth)),!c)if("undefined"!=typeof a.currentStyle)(b=a.currentStyle)&&(c=parseInt(b.width));else return 0}else if(c=a.style.width.replace(/px/,""),0<c.indexOf("%")&&(c=c.replace(/%/,"")),""==c)try{b=a.ownerDocument.defaultView.getComputedStyle(a,""),c=b.width.replace(/px/,"")}catch(d){"undefined"!=typeof a.currentStyle&&(b=a.currentStyle,c=b.width.replace(/px/,
""))}if("NaN"==c||""===c)c=0;return c}function LP_getActiveElement(a){return a?a.activeElement:null}function LP_docHasFocus(a){return a?!0:null}function is_page_JSON(a){if(!a)return null;if("undefined"!=typeof a.lp_is_page_json)return a.lp_is_page_json;var b=a.body;return b&&(b=b.innerHTML,(b="undefined"!=typeof b.trim?b.trim():b.replace(/^\s*/,""))&&2<b.length&&("{"==b.charAt(0)||"("==b.charAt(0)))?a.lp_is_page_json=!0:a.lp_is_page_json=!1}
function normalize_opacity(a){if("undefined"==typeof a||null===a||"undefined"==typeof Math)return 0;var b=0;0<a&&1>=a?b=Math.floor(100*a):1<a&&100>=a&&(b=a);return b}function set_can_xref(a,b,c){if(!a||!b)return!1;g_isfirefox?a.setAttribute("can_xref_"+b,c):g_can_xref[b]=c;return!0}function get_can_xref(a,b){if(!a||!b)return!1;if(g_isfirefox){var c=a.getAttribute("can_xref_"+b);return"undefined"==typeof c||null===c?!0:c}c=g_can_xref[b];return"undefined"==typeof c||null===c?g_can_xref[b]=!0:c}
function compare_puny_urls(a,b){"undefined"!=typeof punycode&&("string"==typeof a&&-1==a.indexOf("xn--")&&(a=punycode.URLToASCII(a)),"string"==typeof b&&-1==b.indexOf("xn--")&&(b=punycode.URLToASCII(b)));"undefined"!=typeof g_iscasper&&g_iscasper&&("string"==typeof a&&(a=a.replace(/\\/g,"%5C")),"string"==typeof b&&(b=b.replace(/\\/g,"%5C")));"string"==typeof a&&(a="undefined"!=typeof a.trim?a.trim():a.replace(/^\s*/,""),0<=a.indexOf("#")&&(a=a.replace(/#$/,"")));"string"==typeof b&&(b="undefined"!=
typeof b.trim?b.trim():b.replace(/^\s*/,""),0<=b.indexOf("#")&&(b=b.replace(/#$/,"")));return a===b}function LP_getFormSubmit(a){if(!a||"FORM"!=a.tagName.toUpperCase())return null;var b=null;if("function"==typeof a.lpsubmitorig2)return a.lpsubmitorig2;try{b=Object&&"undefined"!=typeof Object.getPrototypeOf?Object.getPrototypeOf(a).submit:a.constructor.prototype.submit}catch(c){verbose&&alert(c.message),b=null}return b}
function LP_set_float(a,b){if(!a||null===b||"undefined"==typeof b)return!1;var c=a.ownerDocument;c||(c=document);var d=0;"undefined"!=typeof g_isie&&g_isie&&("undefined"!=typeof LPfn&&LPfn&&init_LPfn()&&LPfn.getDocumentMode?d=LPfn.getDocumentMode(c):"undefined"!=typeof LP_getDocumentMode?d=LP_getDocumentMode(c):"undefined"!=typeof getDocumentMode&&(d=getDocumentMode(c)));"undefined"!=typeof g_isie&&g_isie&&8>=d?a.style.styleFloat=b:a.style.cssFloat=b;return!0}
function LP_get_float(a){if(!a)return"";var b=a.ownerDocument;b||(b=document);var c=0;"undefined"!=typeof g_isie&&g_isie&&("undefined"!=typeof LPfn&&LPfn&&init_LPfn()&&LPfn.getDocumentMode?c=LPfn.getDocumentMode(b):"undefined"!=typeof LP_getDocumentMode?c=LP_getDocumentMode(b):"undefined"!=typeof getDocumentMode&&(c=getDocumentMode(b)));a=LP_get_live_style(a);return"undefined"!=typeof g_isie&&g_isie&&8>=c?a.styleFloat:a.cssFloat}
function LP_set_style_attr(a,b){if(!a||null===b||"undefined"==typeof b)return!1;var c="undefined"!=typeof document?document:a.ownerDocument,d=0;"undefined"!=typeof g_isie&&g_isie&&("undefined"!=typeof LPfn&&LPfn&&init_LPfn()&&LPfn.getDocumentMode?d=LPfn.getDocumentMode(c):"undefined"!=typeof d&&(d=getDocumentMode(c)));"undefined"!=typeof g_isie&&g_isie&&8>=d?a.style.cssText=b:a.setAttribute("style",b);return!0}
function LP_capitalize(a){if(!a)return"";"number"==typeof a&&"undefined"!=typeof a.toString&&(a=a.toString());return"string"!=typeof a?"":a.charAt(0).toUpperCase()+a.slice(1)}function LP_gettime(){return"undefined"!=typeof Date?(new Date).getTime():0}function LP_set_image_src(a,b){if(!a||"undefined"==typeof b)return!1;g_isie?a.src=b:a.setAttribute("src",b);return!0}
function LP_has_highdef_display(a){if("undefined"!=typeof document&&a===document||null===a)if("undefined"!=typeof window)a=window;else{var b=LP_derive_doc();b&&(a=b.defaultView)}if(!a)return!1;a=a.devicePixelRatio;return"undefined"==typeof a?!1:1.5<=a}function LP_is_browser_url(a){return a&&"string"==typeof a?a.toLowerCase().match(/^(about|chrome|safari|chrome-extension|safari-extension|moz-extension|resource|opera|vivaldi):/)?!0:!1:!1}
function LP_getDocumentMode(a){a||(a=LP_derive_doc());return a?"undefined"!=typeof LPfn&&LPfn&&init_LPfn()&&LPfn.getDocumentMode?LPfn.getDocumentMode(a):"undefined"!=typeof a.documentMode&&null!==a.documentMode?a.documentMode:"undefined"!=typeof g_isie&&g_isie?"undefined"!=typeof a.compatMode&&null!==a.compatMode?"CSS1Compat"==a.compatMode?7:5:5:0:0}
function LP_getQuirksMode(a){a||(a=LP_derive_doc());return a?!g_isie||6<LP_getDocumentMode(a)||"undefined"!=typeof a.compatMode&&null!=a.compatMode&&"CSS1Compat"==a.compatMode?!1:!0:!0};