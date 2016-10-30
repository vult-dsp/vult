function makeEditorBox(name,width,height){
   var editor_div = document.getElementById(name);
   var editor = ace.edit(name);
   editor.setTheme("ace/theme/clouds");
   editor.getSession().setMode("ace/mode/vult");
   var height_px = editor.session.getLength()*20;
   editor_div.style.height = height;
   editor_div.style.width = width;
   editor_div.style.fontSize='14px';
   editor.resize();
}

function makeCodeBox(name,width,mode){
   var editor_div = document.getElementById(name);
   var editor = ace.edit(name);
   editor.setTheme("ace/theme/clouds");
   editor.getSession().setMode(mode);
   editor.setReadOnly(true);
   editor.setHighlightActiveLine(false);
   editor.session.setOption("useWorker", false);
   var height_px = editor.session.getLength()*20;
   editor_div.style.height = height_px+"px";
   editor_div.style.width = width;
   editor_div.style.fontSize='14px';
   editor.resize();
}

function setEditorFontSize(name, size) {
   var editor_div = document.getElementById(name);
   editor_div.style.fontSize=size+'px';
}

var all_vult_editors = document.getElementsByClassName("vult_code");
for(var i=0;i<all_vult_editors.length;i=i+1){
   makeCodeBox(all_vult_editors[i].id,"650px","ace/mode/vult");
}

var all_c_editors = document.getElementsByClassName("c_code");
for(var i=0;i<all_c_editors.length;i=i+1){
   makeCodeBox(all_c_editors[i].id,"650px","ace/mode/c_cpp");
}


