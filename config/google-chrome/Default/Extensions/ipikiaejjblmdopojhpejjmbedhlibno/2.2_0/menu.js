function callbackTestFunction(obj) { //works
  localStorage.setItem("openMode", "4");
  localStorage.setItem("selectedText", obj.selectionText);
  setupPopupWindow();
}

function setupPopupWindow() {
  //alert(localStorage.getItem("selectedText")); 
  //window width height
    var width = 800;
    var height = 430;

    //detect OS
    var OSName="Unknown OS";
    if (navigator.appVersion.indexOf("Win")!=-1) OSName="Windows";
    if (navigator.appVersion.indexOf("Mac")!=-1) OSName="MacOS";
    if (navigator.appVersion.indexOf("X11")!=-1) OSName="UNIX";
    if (navigator.appVersion.indexOf("Linux")!=-1) OSName="Linux";
    //console.log(OSName);
    if (OSName=="Windows") {
      width = 800;
      height = 500;
    }

    //console.log('default width: '+width);
    //console.log('default height: '+height);

    if (localStorage.getItem("width")> 0 && localStorage.getItem("height")>0) {
      width = localStorage.getItem("width");
      height = localStorage.getItem("height");
      //console.log('saved width: '+width);
      //console.log('saved height: '+height)
    }

    width = parseInt(width);
    height = parseInt(height);
  popupwindow("app.html", "", width, height);
}

function popupwindow(url, title, w, h) {
  var left = (screen.width/2)-(w/2);
  var top = (screen.height/2)-(h/2);
  
  return window.open(url, title, 'toolbar=no, location=no, directories=no, status=no, menubar=no, scrollbars=no, resizable=no, copyhistory=no, width='+w+', height='+h+', top='+top+', left='+left);
} 

// Create selection menu
var contexts = ["selection"];
for (var i = 0; i < contexts.length; i++) {
  var context = contexts[i];
  var title = "Spreed selected text";
  var id = chrome.contextMenus.create({"title": title, "contexts":[context],
                                       "onclick": callbackTestFunction});
  //console.log("'" + context + "' item:" + id);
}

