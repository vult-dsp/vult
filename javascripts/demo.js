function resizeEditor(){
   var screen = window.innerHeight;
   var main = document.getElementById("main");
   main.style.height = (screen)+"px";
   main.style.top = 0+"px";
   main.width = "100%";

   var top = document.getElementById("top-section");
   var bottom = document.getElementById("bottom-section");
   var middle = document.getElementById("middle-section");
   var side = document.getElementById("side-section");
   console.log("Screen: " + screen);
   console.log("Bottom: " + bottom.clientHeight);
   console.log("Top: " + top.clientHeight);
   var new_height = (screen - top.clientHeight - bottom.clientHeight) +"px";
   var editor_div = document.getElementById("editor");
   var editor = ace.edit("editor");

   middle.style.height = new_height;
   side.style.height = new_height;
   editor_div.style.height = new_height;
   editor_div.style.top = (0) + "px";
   middle.style.position = "relative";
   editor_div.style.position = "relative";
   editor.resize();

}

var expanded = false;

function expandCollapse() {
   var middle = document.getElementById("middle-section");
   var side = document.getElementById("side-section");
   var examples = document.getElementById("examples");

   if(expanded){
      middle.style.marginLeft = "33px";
      side.style.width = "33px";
      examples.style.visibility = "hidden";
      expanded = false;

   }
   else {
      middle.style.marginLeft = "90px";
      side.style.width = "90px";
      examples.style.visibility = "visible";
      expanded = true;
   }

}

function initDemo () {
   makeEditorBox('editor','100%',"450px");
   loadPreset(0);
   window.onbeforeunload = function() {
      return 'Are you sure you want to navigate away? Any changes to the code will be lost.';
   }
   var prev_onload = window.onload;
   window.onload = function() {
      prev_onload();
      resizeEditor();
   };
   window.onresize=function(){resizeEditor()};
}