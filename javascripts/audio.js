
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
        restoreControls();
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

// All the presets
var template  = "/* You can use a this template to start a program */\n\n// Main processing function\n// 'input' is by default a sine wave at 440 Hz\nfun process(input){\n   return input;\n}\n\n// Called when a note On is received\nfun noteOn(note,velocity){\n}\n\n// Called when a note Off is received\nfun noteOff(note){\n}\n\n// Called when a control changes\nfun controlChange(control,value){\n}\n\n// Called on initialization to define initial values\nfun default(){\n}\n";
var volume1   = "/* A simple volume control */\n\n// Used to soften the transitions of controls\nfun smooth(input){\n   mem x;\n   x = x+(input-x)*0.005;\n   return x;\n}\n// Main processing function\n// 'input' is by default a sine wave at 440 Hz\nfun process(input){\n   mem volume; // the value is set in 'controlChange'\n   return input*smooth(volume);\n}\n\n// Called when a note On is received\nfun noteOn(note,velocity){\n}\n\n// Called when a note Off is received\nfun noteOff(note){\n}\n\n// Called when a control changes\nfun controlChange(control,value){\n   mem volume;\n   // Control 30 defines the volume\n   if(control==30) volume = value/127;\n}\n\n// Called on initialization to define initial values\nfun default(){\n mem volume = 0;\n}\n";
var phasedist = "/* Phase distortion oscillator */\n\n// Used to soften the transitions of controls\nfun smooth(input){\n   mem x;\n   x = x+(input-x)*0.005;\n   return x;\n}\n\n// Returns true every time the input value changes\nfun change(x):bool {\n    mem pre_x;\n    val v:bool = pre_x!=x;\n    pre_x = x;\n    return v;\n}\n\n// Converts the MIDI note to increment rate at a 44100 sample rate\nfun pitchToRate(d) return 8.1758*exp(0.0577623*d)/44100;\n\nfun phasor(pitch,reset){\n    mem rate,phase;\n    if(change(pitch))\n        rate = pitchToRate(pitch);\n    phase = if reset then 0 else (phase + rate) % 1;\n    return phase;\n}\n\n// Main processing function\nfun process(input){\n   mem volume,detune; // values set in 'controlChange'\n   mem pitch;\n   mem pre_phase1;\n   // Implements the resonant filter simulation as shown in\n   // http://en.wikipedia.org/wiki/Phase_distortion_synthesis\n   val phase1 = phasor(pitch,false);\n   val comp   = 1 - phase1;\n   val reset  = (pre_phase1 - phase1) > 0.5;\n   pre_phase1 = phase1;\n   val phase2 = phasor(pitch+smooth(detune)*32,reset);\n   val sine  = sin(2*3.14159265359*phase2);\n   return smooth(volume)*(sine*comp);\n}\n\n// Called when a note On is received\nfun noteOn(note,velocity){\n    mem pitch = note;\n}\n\n// Called when a note Off is received\nfun noteOff(note){\n}\n\n// Called when a control changes\nfun controlChange(control,value){\n   mem volume;\n   mem detune;\n   // Control 30 defines the volume\n   if(control==30) volume = value/127;\n   if(control==31) detune = value/127;\n}\n\n// Called on initialization to define initial values\nfun default(){\n   mem volume = 0;\n   mem pitch = 45;\n   mem detune = 0;\n}\n";

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
        default:
            code = template;
            break;
    }
    var editor = ace.edit("editor");
    editor.setValue(code);
    editor.clearSelection();
    editor.gotoLine(0);
}
