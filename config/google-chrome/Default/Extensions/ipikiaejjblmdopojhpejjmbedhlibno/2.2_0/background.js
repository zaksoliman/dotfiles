var curTabUrl = "";
var completedFlag = false;

function onInstall() {
    //console.log("Extension Installed");
    chrome.tabs.create({url: "popup.html"});
    localStorage.setItem("selectedText","");
}

function onUpdate() {
    //console.log("Extension Updated");
    chrome.tabs.create({url: "updated.html"});

    localStorage.setItem("notfirsttime",0);
    localStorage.setItem("selectedText","");
}

function getVersion() {
    var details = chrome.app.getDetails();
    return details.version;
}

// Check if the version has changed.
var currVersion = getVersion();
var prevVersion = localStorage['spreed-version']
if (currVersion != prevVersion) {
// Check if we just installed this extension.
if (typeof prevVersion == 'undefined') {
  onInstall();
} else {

  //check if major update (3 or fewer version number)
  tokens = currVersion.split('.');
  if (tokens.length<=3) {
    onUpdate();
  }
}
  localStorage['spreed-version'] = currVersion;
}




var waitUntil = function (fn, condition, interval) {
    interval = interval || 100;

    var shell = function () {
            var timer = setInterval(
                function () {
                    var check;

                    try { check = !!(condition()); } catch (e) { check = false; }

                    if (check) {
                        clearInterval(timer);
                        delete timer;
                        fn();
                    }
                },
                interval
            );
        };

    return shell;
};


function strip(html)
{
   var tmp = document.createElement("DIV");
   tmp.innerHTML = html;
   return tmp.textContent || tmp.innerText || "";
}



function openSpreed() {
  
  //get url
  //getAndStoreUrl();

  //get html
  //getAndStoreHtml();

  setTimeout(function(){setupPopupWindow()},500);
  
}

//TEXT SELECTION HOTKEY
/* The function that finds and returns the selected text */
var funcToInject = function() {
    var selection = window.getSelection();
    return (selection.rangeCount > 0) ? selection.toString() : '';
};

/* This line converts the above function to string
 * (and makes sure it will be called instantly) */
var jsCodeStr = ';(' + funcToInject + ')();';

function openSpreedTextSelector() {
  chrome.tabs.query({ currentWindow: true, active: true }, function (tabs) {
      tab = tabs[0];
      //alert(tab.id);

      chrome.tabs.insertCSS(tab.id, { file: "combined.css" });
      chrome.tabs.executeScript(tab.id, { file: "combined.js" });

  });
}

function autoExtractContent() {
  //get current tab
  localStorage.setItem("selectedText", "");
  chrome.tabs.query({ currentWindow: true, active: true }, function (tabs) {
      tab = tabs[0];
      chrome.tabs.executeScript(tab.id, { file: "jquery.js" });
      chrome.tabs.executeScript(tab.id, { file: "extractor.js" });
      chrome.tabs.executeScript(tab.id, { file: "getPagesSource.js" });

  });
}

function autoExtractContentAndOpenSpreed() {
  autoExtractContent();

  //wait until selected text is set again
  waitUntil(
    function () {
      // the code you want to run here...
      openSpreed();
    },
    function() {
      // the code that tests here... (return true if test passes; false otherwise)
      return !!(localStorage.getItem('selectedText') !== '');
    },
    50 // amount to wait between checks
  )();
  
}

chrome.commands.onCommand.addListener(function(cmd) {
    if (cmd === 'open-spreed') {

        //alert('open spreed hotkey pressed');
        /* Inject the code into all frames of the active tab */
        chrome.tabs.executeScript({
            code: jsCodeStr,
            allFrames: true   //  <-- inject into all frames, as the selection 
                              //      might be in an iframe, not the main page
        }, function(selectedTextPerFrame) {
            if (chrome.runtime.lastError) {
                /* Report any error */
                //alert('Try right clicking to spreed the text. Error: ' + chrome.runtime.lastError.message);
                console.log('Try right clicking to spreed the text. Error: ' + chrome.runtime.lastError.message);
            }

           if ((selectedTextPerFrame[0].length > 0)
                    && (typeof(selectedTextPerFrame[0]) === 'string')) {

                //alert('something selected');
                /* The results are as expected */
                //alert('Selected text: ' + selectedTextPerFrame[0]);
                //alert(selectedTextPerFrame[0].length);
                localStorage.setItem("openMode", "3");
                localStorage.setItem("selectedText", selectedTextPerFrame[0]);
				        setupPopupWindow();
            } 
            
            else if (selectedTextPerFrame[0].length == 0) {
                //nothing selected, we want to auto content extract
                localStorage.setItem("openMode", "1");
                autoExtractContentAndOpenSpreed();
            }


        });
    }

    else if (cmd == 'open-spreed-text-selector') {

      openSpreedTextSelector();
      
    } 

});




function url_domain(data) {
  var    a      = document.createElement('a');
         a.href = data;
  return a.hostname;
}
//insert overlay
chrome.tabs.onUpdated.addListener(function(tabId, changeInfo, tab) {

    

  if (tab.url != curTabUrl) //reset flag if on a new page
    completedFlag = false;

  //alert(changeInfo.status);

  //if (changeInfo.status=="complete" && curTabUrl != tab.url) {
  if (changeInfo.status=="complete" && completedFlag == false) {
    curTabUrl = tab.url;
    completedFlag = true;
    //alert('tab url switched: '+tabId); //ok, this works
    //alert(changeInfo.status);

    //called twice... but i dealt with it in overlay.js

    //set current extension url
    extensionUrl = chrome.extension.getURL("/");
    localStorage.setItem("extensionUrl", extensionUrl);
    //alert(extensionUrl);

    //set blacklistall flag to false, if it doesn't exist
    if (localStorage.getItem("blacklistall")==null) {
      localStorage.setItem("blacklistall", "false");
    }

    //get blacklist
    blacklistString = localStorage.getItem("blacklist");
    if (blacklistString==null) {
      //set default blacklist sites
      blacklistString = "www.mail.google.com, www.facebook.com, www.youtube.com, www.mail.yahoo.com";
      localStorage.setItem("blacklist",blacklistString);
    }
    //alert(blacklistString);
    
    //check that current domain isn't in blacklist
    onBlacklist = false;

    if (tab.url.substring(0,4) == 'http') {
      localStorage.setItem("curUrl", tab.url);
      localStorage.setItem("curTitle", tab.title);
      
    }

    curDomain = url_domain(tab.url);
    
    //if curDomain doesn't have www, add
    if (curDomain.substring(0,4) != 'www.') {
      curDomain  = "www."+curDomain;
    }
    localStorage.setItem("curDomain", curDomain);

    
    //check if current domain is in blacklist
    blacklistSites = blacklistString.split(',');
    for (var i=0; i<blacklistSites.length; i++) {
      curBlacklistedSite = blacklistSites[i].trim();
      if (curBlacklistedSite==curDomain) {
        onBlacklist = true;
      }
    }
    
    blacklistall = localStorage.getItem("blacklistall");

    //if not, insert overlay js and relevant libraries
    if (onBlacklist == false && blacklistall == 'false' && tab.url.substring(0,4) == 'http' && tab.url.substring(0,5) != 'https' && tab.url.substring(tab.url.length-3, tab.url.length) != 'pdf') {

      chrome.tabs.executeScript(tabId, { file: "overlay.js" });
      chrome.tabs.executeScript(tabId, { file: "vex.combined.min.js" });
      chrome.tabs.insertCSS(tab.id, { file: "overlay.css" });
      chrome.tabs.insertCSS(tab.id, { file: "vex.css" });
      chrome.tabs.insertCSS(tab.id, { file: "vex-theme-plain.css" });
    }

    completedFlag = false;
  }
});


chrome.runtime.onMessage.addListener(
  function(request, sender, sendResponse) {

    //console.log(sender.tab ? "from a content script:" + sender.tab.url : "from the extension");
    //alert('message received');
  
    if (request.action == "divSelect") {
      if (request.allText.length>0) {
        localStorage.setItem("openMode", "2");
        localStorage.setItem("selectedText",request.allText);
        //test = localStorage.getItem("allText");
        //alert(test);
        //sendResponse({farewell: "goodbye"});
        
      }
    }

    else if (request.action == "getSource") {
      //get page html
      //localStorage.setItem("htmlSource",request.source);
      //alert('htmlSource');
      //alert(localStorage.getItem("htmlSource"));
      htmlSource = request.source;
      //alert(htmlSource);
      localStorage.setItem("htmlSource",htmlSource);

      localStorage.setItem("curTitle",request.title); // update the title that's extracted, since this is called on article extraction

      //send to api in background
      //not yet implemented

    }

    else if (request.action == "openSpreedFromButton") {
      localStorage.setItem("openMode", "0");
      autoExtractContentAndOpenSpreed();
    }

    else if (request.action == "getCurDomain") {
      sendResponse({
        curDomain: localStorage.getItem("curDomain"),
        extensionUrl: localStorage.getItem("extensionUrl")  ,
        curUrl: localStorage.getItem("curUrl")
    });
    }

    else if (request.action == "addToBlacklist") {
      blacklistString = localStorage.getItem("blacklist");
      blacklistString += ", "+request.curDomain;
      localStorage.setItem("blacklist",blacklistString);
    }

    else if (request.action == "openSpreedTextSelector") {
      localStorage.setItem("openMode", "5");
      openSpreedTextSelector();
    }

    else if (request.action == "openSpreedTextSelectorFromMenu") {
      localStorage.setItem("openMode", "7");
      openSpreedTextSelector();
    }

    else if (request.action == "openSpreedWithPasted") {
      localStorage.setItem("openMode", "6");
      localStorage.setItem("selectedText", request.pastedText);
      setTimeout(function(){setupPopupWindow()},0);
    }

    else if (request.action == "extractor") { //the end point for all auto content extractor calls
      htmlStripped = strip(request.html);
      localStorage.setItem("selectedText", htmlStripped);
    }

    else if (request.action == "openSpreedFromMenu") {
      localStorage.setItem("openMode", "8");
      autoExtractContentAndOpenSpreed();
    }

    else if (request.action == "autoExtractContent") {
      autoExtractContent();
    }

    else if (request.action == "getExtractedContent") {
      //alert(localStorage.getItem("speed"));

      sendResponse({
            extratedContent: localStorage.getItem("selectedText")
      });

    }

    else if (request.action == "getWPMSpeed") {
      wpm = localStorage.getItem("speed");
      if (wpm==null) {
        wpm = 400;
      }
      sendResponse({
            wpm: wpm
      });
    }

});


chrome.browserAction.onClicked.addListener(function (tab) { //Fired when User Clicks icon
    //launch menu


});


