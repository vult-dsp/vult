function makeEditor(name,width,mode){
   var editor_div = document.getElementById(name);
   var editor = ace.edit(name);
   editor.setTheme("ace/theme/monokai");
   editor.getSession().setMode(mode);
   editor.setReadOnly(true);
   editor.setHighlightActiveLine(false);
   var height_px = editor.session.getLength()*20;
   editor_div.style.height = height_px+"px";
   editor_div.style.width = width;
   editor_div.style.fontSize='14px';
   var text = editor.getValue();
   editor.resize();
}

var all_vult_editors = document.getElementsByClassName("vult_code");
for(var i=0;i<all_vult_editors.length;i=i+1){
   makeEditor(all_vult_editors[i].id,"700px","ace/mode/vult");
}

var all_c_editors = document.getElementsByClassName("c_code");
for(var i=0;i<all_c_editors.length;i=i+1){
   makeEditor(all_c_editors[i].id,"700px","ace/mode/c_cpp");
}