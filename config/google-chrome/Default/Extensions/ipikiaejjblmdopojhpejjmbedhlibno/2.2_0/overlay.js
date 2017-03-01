var overlay;
var removeOverlay;
var offX;
var offY;
var vexDialog;

var extensionUrl;
var contentWordCount;

//content functions
function isEmpty(str) {
    return (!str || 0 === str.length);
}

function _removeEmpty(splitText) {
  var newSplitText = new Array();
  
  for (var i=0; i<splitText.length; i=i+1) {
    token = splitText[i];
    if (!isEmpty(token)) {
      newSplitText.push(token);
    }
  }
  return newSplitText;
}

function _combineNumbers(theSplitText) {
  
  newList = Array();
  var i=0;
  while (i<theSplitText.length) {
    curChar = theSplitText[i];
    if (theSplitText[i].match(/[0-9]/)) {
      curChar = "";
      while (theSplitText[i].match(/[0-9]/)) {
        curChar+=theSplitText[i];
        i+=1;
      }
      newList.push(curChar);

    }
    else {
      newList.push(curChar);
      i+=1;
    }
  }
  //console.log(newList);
  return newList;
}

function _combineEnglishWords(theSplitText) {
  //console.log('combine english');
  
  newList = Array();
  var i=0;
  while (i<theSplitText.length) {
    curChar = theSplitText[i];
    if (theSplitText[i].match(/[a-zA-Z]/)) {
      curChar = "";
      while (theSplitText[i].match(/[a-zA-Z]/)) {
        curChar+=theSplitText[i];
        i+=1;
      }
      newList.push(curChar);

    }
    else {
      newList.push(curChar);
      i+=1;
    }
  }
  
  return newList;
}

function _splitSelectedText(selectedText) {

  //detect chinese or japanese
  if(selectedText.match(/[\u3400-\u9FBF]{10,}/)) {
    //naive: don't split numbers, or english words
    var theSplitText = selectedText.split('');

    theSplitText = _combineNumbers(theSplitText);
    theSplitText = _combineEnglishWords(theSplitText);
  }
  else {
    //some stuff for scientific notations?
    selectedText = selectedText.replace(/([\w]+)([\[][\w]+)+/g,'$1 $2');
    selectedText = selectedText.replace(/([\w]+)([\-\=\/])([\w]+)+/g,'$1$2 $3');
    selectedText = selectedText.replace(/([\w]+)([\-\=\/]\[)([\w]+)+/g,'$1$2 $3');

    //split on whitespace
    theSplitText = selectedText.split(/[\s]+/);
  }

  theSplitText = _removeEmpty(theSplitText);
  return theSplitText;
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

function closeVexDialogAndHideButton() {
  vex.close(vexDialog.data().vex.id);
  overlay.style.visibility='hidden';
}

function okClick() {
  //add domain to blacklist
  chrome.runtime.sendMessage({
      action: "addToBlacklist",
      curDomain: curDomain
  });
  closeVexDialogAndHideButton();
}

function cancelClick() {
  closeVexDialogAndHideButton();
}

removeOverlay = function() {

  curDomain = localStorage.getItem("curDomain");
  extensionUrl = localStorage.getItem("extensionUrl");

  //open dialog to add or not this domain to blacklist
    
  vexDialog = vex.open({
    content: '<form class="vex-dialog-form"> \
    <div class="vex-dialog-message">Never show Spreed button on <i>'+curDomain+'</i> again? \
    <br>Or click <a href="'+extensionUrl+'options.html" target="_blank">here</a> to disable the Spreed button on all sites.</div>\
    <div class="vex-dialog-buttons"> \
    <input type="button" value="OK" id="ok-button" class="vex-dialog-button-primary vex-dialog-button vex-first"> \
    <input type="button" value="Just this time" id="cancel-button" class="vex-dialog-button-secondary vex-dialog-button vex-last"> \
    </div> \
    </form>',
    className: 'vex-theme-plain'

  });

  okButton = document.getElementById('ok-button');
  okButton.addEventListener("click", okClick);

  okButton = document.getElementById('cancel-button');
  okButton.addEventListener("click", cancelClick);


}



function updateOverlay() {
  //get time estimate. steps:
    //submit html to api
    //get content, striped of html tags
    //get chunk size, and wpm speed
    //split into tokens
    //calculate time estimate steps
}

function divMove(e){

  var div = document.getElementById('spreed-overlay');
  div.style.position = 'absolute';
  div.style.top = (e.clientY-offY) + 'px';
  div.style.left = (e.clientX-offX) + 'px';

}

function mouseUp(e)
{
  console.log('');
  window.removeEventListener('mousemove', divMove, true);
}

function mouseDown(e){

  var div = document.getElementById('spreed-overlay');
  offY= e.clientY-parseInt(div.offsetTop);
  offX= e.clientX-parseInt(div.offsetLeft);
  window.addEventListener('mousemove', divMove, true);
}

function getTimeToReadString(timeToRead) {
  timeToReadString = "";
  if (timeToRead<1) {
    timeToReadString += "< 1";
  }
  else {
    timeToReadString += Math.round(timeToRead).toString();
  }
  timeToReadString += " min";
  return timeToReadString;
}


temp = document.getElementById("spreed-overlay");
//console.log('spreed overlay:' + temp);
if (temp==null) {



  //run content extractor
  
  chrome.runtime.sendMessage({
      action: "autoExtractContent"
  });


  //send message to get current domain, and extension url
  extensionUrl = null;
  curUrl = null;
  chrome.runtime.sendMessage({
      action: "getCurDomain"
  }, function(response) {
    //console.log(response.curDomain);
    localStorage.setItem("curDomain", response.curDomain);
    localStorage.setItem("extensionUrl", response.extensionUrl);

    curUrl = response.curUrl;
    extensionUrl = response.extensionUrl;
    //console.log("extensionUrl: "+extensionUrl);
  });

  waitUntil(
    function () {
      /*
      if (localStorage.getItem("prevUrl")==curUrl) {
        return;
      }
      else {
        localStorage.setItem("prevUrl", curUrl);
      }*/
      
      //console.log('loading overlay');
      //create overlay
      overlay = document.createElement("div");
      overlay.id = 'spreed-overlay';


      //give extractor enough time to get content word count
      setTimeout(continueInit, 1000);
    },
    function() {
      // the code that tests here... (return true if test passes; false otherwise)
      return !!(curUrl !== null);
    },
    50 // amount to wait between checks
  )(); 
  

}

function continueInit() {

  //get word count
  contentWordCount = null;

  chrome.runtime.sendMessage({
      action: "getExtractedContent"
  }, function(response) {
    
    extratedContent = response.extratedContent;
    splitWords = _splitSelectedText(extratedContent);
    contentWordCount = splitWords.length;
  });

  //get user's wpm speed
  timeToRead = null;
  chrome.runtime.sendMessage({
      action: "getWPMSpeed"
  }, function(response) {
    wpm = response.wpm;
    timeToRead = contentWordCount/wpm;
    //console.log(timeToRead);
  });
  


  //wait until we have extensionUrl
  waitUntil(
    function () {
      
      
      //close overlay button
      closeOverlay = document.createElement("img");
      //closeOverlay.src = "https://dl.dropboxusercontent.com/u/162121/spreed/close_icon.png";
      closeOverlay.src = extensionUrl+"close_icon.png";
      closeOverlayCss = 'float:left; \
      margin-bottom:3px; \
      position:absolute; \
      left:0px; \
      top:0px; \
      cursor:pointer;';
      closeOverlay.style.cssText = closeOverlayCss;
      overlay.appendChild(closeOverlay);

      
      //time to spreed
      spreedTimeEstimate = document.createElement("a");
      //spreedTimeEstimate.src = 'https://dl.dropboxusercontent.com/u/162121/spreed/book24.png';
      
      //console.log(timeToRead);
      timeToReadString = getTimeToReadString(timeToRead);
      //console.log(timeToReadString);
      spreedTimeEstimate.innerHTML = timeToReadString+' to Spreed';
      

      //spreedTimeEstimate.innerHTML = "test";
      overlay.appendChild(spreedTimeEstimate);


      //console.log(closeOverlay);

      //listeners

      closeOverlay.addEventListener("click", removeOverlay);


      spreedTimeEstimate.style.cursor = 'pointer';
      spreedTimeEstimate.onclick = function() {
          //send message to open spreed
          chrome.runtime.sendMessage({
              action: "openSpreedFromButton"
          });
      };

      //movable. Eric reports a bug: it doesn't detach...
      /*
      overlay.addEventListener('mousedown', mouseDown, false);
      window.addEventListener('mouseup', mouseUp, false);
      */


      //get time estimate
      updateOverlay();

      //attach overlay to body
      document.getElementsByTagName("body")[0].appendChild(overlay);

      //make it move with scroll
      overlayObj = $("#spreed-overlay");
      $(window).scroll(function(){

          if($(window).scrollTop()>0){
              overlayObj.css('position', 'fixed');
              
          } else {
              overlayObj.css('position', 'absolute');

          }
      });

    },
    function() {
      // the code that tests here... (return true if test passes; false otherwise)
      return !!(extensionUrl !== null && timeToRead !== null);
    },
    50 // amount to wait between checks
  )();
}




$(window).load(function(){







});

