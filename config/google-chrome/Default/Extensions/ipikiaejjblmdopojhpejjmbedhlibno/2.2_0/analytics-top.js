function pushEvent(category, value) {
	_gaq.push(['_trackEvent', category, value]);
}

var _gaq = _gaq || [];
_gaq.push(['_setAccount', 'UA-35748958-3']);
_gaq.push(['_trackPageview']);



