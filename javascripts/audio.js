
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
        if(vult_object.noteOn!=null){
            vult_object.noteOn(note,velocity);
        }
    }
    console.log("noteOn("+note+","+velocity+")");
}

function sendNoteOff(note){
    if(vult_object!=null){
        if(vult_object.noteOff!=null){
            vult_object.noteOff(note);
        }
    }
    console.log("noteOff("+note+")");
}

function sendControlChange(control,value){
    control_values[control]=value;
    if(vult_object!=null){
        if(vult_object.controlChange!=null){
            vult_object.controlChange(control,value);
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
        generated_code = jscode(editor.getValue());
        new_vult_object = eval(generated_code);
    }
    catch(err) {
        setCodeStatus("Error when compiling");
    }
    if(new_vult_object){
        vult_object = new_vult_object;
        setCodeStatus("Ok");
    }
    else setCodeStatus("Error when compiling");
}

function getGeneratedProgram(){
    return generated_code;
}

function setAudioOn(){

    if(audioContext==null && audioStatus==false){
        audioContext = new AudioContext();
        source = audioContext.createBufferSource();

        var effect = (function() {
            var node = audioContext.createScriptProcessor(0, 1, 1);
            node.onaudioprocess = function(e) {
                var input = e.inputBuffer.getChannelData(0);
                var output = e.outputBuffer.getChannelData(0);
                for (var i = 0; i < e.inputBuffer.length; i++) {
                    if(vult_object){
                        if(vult_object.process){
                            output[i] = vult_object.process(input[i]);
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
        audioContext.close();
        audioContext = null;
        setAudioStatus("Off");
        audioStatus=false;
    }
}

var template = "/* You can use a this template to start a program */\n\n// Main processing function\n// 'input' is by default a sine wave at 440 Hz\nfun process(input){\n   return input;\n}\n\n// Called when a note On is received\nfun noteOn(note,velocity){\n}\n\n// Called when a note Off is received\nfun noteOff(note){\n}\n\n// Called when a control changes\nfun controlChange(control,value){\n}\n\n// Called on initialization to define initial values\nfun default(){\n}\n";
var volume1 = "/* A simple volume control */\n\n// Used to soften the transitions of controls\nfun smooth(input){\n   mem x;\n   x = x+(input-x)*0.005;\n   return x;\n}\n// Main processing function\n// 'input' is by default a sine wave at 440 Hz\nfun process(input){\n   mem volume; // the value is set in 'controlChange'\n   return input*smooth(volume);\n}\n\n// Called when a note On is received\nfun noteOn(note,velocity){\n}\n\n// Called when a note Off is received\nfun noteOff(note){\n}\n\n// Called when a control changes\nfun controlChange(control,value){\n   mem volume;\n   // Control 30 defines the volume\n   if(control==30) volume = value/127;\n}\n\n// Called on initialization to define initial values\nfun default(){\n mem volume = 0;\n}\n";
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
        default:
            code = template;
            break;
    }
    var editor = ace.edit("editor");
    editor.setValue(code);
    editor.clearSelection();
    editor.gotoLine(0);
}
