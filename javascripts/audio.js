
function setAudioStatus(msg){
   var label = document.getElementById("audio_status");
   label.textContent = "Audio: "+msg;
}

function setCodeStatus(msg){
   var label = document.getElementById("code_status");
   label.textContent = "Code: "+msg;
}

var audioContext = null;
var audioStatus = false;
var vult_object = null;
var control_values = [];
var generated_code = "";

function sendNoteOn(note,velocity){
    if(vult_object!=null){
        if(vult_object.liveNoteOn!=null){
            vult_object.liveNoteOn(note,velocity);
        }
    }
    console.log("noteOn("+note+","+velocity+")");
}

function sendNoteOff(note){
    if(vult_object!=null){
        if(vult_object.liveNoteOff!=null){
            vult_object.liveNoteOff(note);
        }
    }
    console.log("noteOff("+note+")");
}

function sendControlChange(control,value){
    control_values[control]=value;
    if(vult_object!=null){
        if(vult_object.liveControlChange!=null){
            vult_object.liveControlChange(control,value);
        }
    }
    console.log("controlChange("+control+","+value+")");
}

function restoreControls(){
    for(var i=0; i<128; i++){
        var value = control_values[i];
        if(value){
            sendControlChange(i,value);
        }
    }
}

function updateProgram(){
    var editor = ace.edit("editor");
    var new_vult_object = null;
    try{
        generated_code = jscode(editor.getValue()) + " new vultProcess()";
        //console.log(generated_code);
        new_vult_object = eval(generated_code);
    }
    catch(err) {
        setCodeStatus("Error when compiling");
    }
    if(new_vult_object){
        vult_object = new_vult_object;
        restoreControls();
        setCodeStatus("Ok");
    }
    else setCodeStatus("Error when compiling");
}

function getGeneratedProgram(){
    return generated_code;
}

var effect = null;
function setAudioOn(){

    if(audioContext==null && audioStatus==false){
        audioContext = new AudioContext();
        source = audioContext.createBufferSource();

        effect = (function() {
            var node = audioContext.createScriptProcessor(0, 1, 1);
            node.onaudioprocess = function(e) {
                var input = e.inputBuffer.getChannelData(0);
                var output = e.outputBuffer.getChannelData(0);
                for (var i = 0; i < e.inputBuffer.length; i++) {
                    if(vult_object){
                        if(vult_object.liveProcess){
                            output[i] = vult_object.liveProcess(input[i]);
                        }
                    }
                    else{
                        output[i] = input[i];
                    }
                }
            }
          return node;
        })();

        oscillator = audioContext.createOscillator();
        oscillator.frequency = 440;
        oscillator.connect(effect);
        effect.connect(audioContext.destination);
        oscillator.start(0);
        setAudioStatus("On");
        audioStatus=true;
        //console.log("Turning audio On");
        updateProgram();
    }
    else if(audioContext!=null && audioStatus){
        //console.log("Updating");
        updateProgram();
    }
}

function setAudioOff(){
    if(audioContext){
        effect.disconnect();
        if(audioContext.close)
            audioContext.close();
        audioContext = null;
        setAudioStatus("Off");
        audioStatus=false;
    }
}



// All the presets
var template  = "";
var volume1   = "";
var phasedist = "";
var synt1     = "";


function loadPreset(n){
    console.log("Loading "+n)
    var code;
    switch(n){
        case 0:
            code = template;
            break;
        case 1:
            code = volume1;
            break;
        case 2:
            code = phasedist;
            break;
        case 3:
            code = synt1;
            break;
        default:
            code = template;
            break;
    }
    var editor = ace.edit("editor");
    editor.setValue(code);
    editor.clearSelection();
    editor.gotoLine(0);
}

var request = new XMLHttpRequest();
request.onreadystatechange = function() {
  if (request.readyState != 4) return; // Not there yet
  if (request.status != 200) {
    return;
  }
  var resp = request.responseText;
  example_files = JSON.parse(resp);
  template = example_files.files['template.vult'].content;
  volume1 = example_files.files['volume.vult'].content;
  phasedist = example_files.files['phasedist.vult'].content;
  synt1 = example_files.files['synth1.vult'].content;
  loadPreset(0);
}
request.open('GET', 'https://api.github.com/gists/77bc427f231a1d5b7d8a', true);
request.send();
