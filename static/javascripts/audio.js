function setAudioStatus(msg) {
   var label = document.getElementById("audio_status");
   label.textContent = "Audio: " + msg;
}

function setCodeStatus(msg) {
   var label = document.getElementById("code_status");
   label.textContent = "Code: " + msg;
}

var audioContext = null;
var audioStatus = false;
var vult_object = null;
var control_values = [];
var generated_code = "";

function sendNoteOn(note, velocity, channel) {
   if (vult_object != null) {
      if (vult_object.liveNoteOn != null) {
         vult_object.liveNoteOn(note, velocity, channel);
      }
   }
   console.log("noteOn(" + note + "," + velocity + "," + channel + ")");
}

function sendNoteOff(note, channel) {
   if (vult_object != null) {
      if (vult_object.liveNoteOff != null) {
         vult_object.liveNoteOff(note, channel);
      }
   }
   console.log("noteOff(" + note + "," + channel + ")");
}

function sendControlChange(control, value, channel) {
   control_values[control] = value;
   if (vult_object != null) {
      if (vult_object.liveControlChange != null) {
         vult_object.liveControlChange(control, value, channel);
      }
   }
   console.log("controlChange(" + control + "," + value + "," + channel + ")");
}

function restoreControls() {
   for (var i = 0; i < 128; i++) {
      var value = control_values[i];
      if (value) {
         sendControlChange(i, value);
      }
   }
}

function updateProgram() {
   var editor = ace.edit("editor");
   var new_vult_object = null;
   try {
      generated_code = jscode(editor.getValue()) + " new vultProcess()";
      console.log(generated_code);
      new_vult_object = eval(generated_code);
   } catch (err) {
      console.log("Bad error");
      setCodeStatus("Error when compiling: " ^ err);
   }
   if (new_vult_object) {
      vult_object = new_vult_object;
      restoreControls();
      setCodeStatus("Ok");
   } else setCodeStatus("Error when compiling " + generated_code);
}

function getGeneratedProgram() {
   return generated_code;
}

var effect = null;

function setAudioOn() {

   if (audioContext == null && audioStatus == false) {
      audioContext = new AudioContext();
      source = audioContext.createBufferSource();

      effect = (function () {
         var node = audioContext.createScriptProcessor(0, 1, 1);
         node.onaudioprocess = function (e) {
            var input = e.inputBuffer.getChannelData(0);
            var output = e.outputBuffer.getChannelData(0);
            for (var i = 0; i < e.inputBuffer.length; i++) {
               if (vult_object) {
                  if (vult_object.liveProcess) {
                     output[i] = vult_object.liveProcess(input[i]);
                  }
               } else {
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
      audioStatus = true;
      //console.log("Turning audio On");
      updateProgram();
   } else if (audioContext != null && audioStatus) {
      //console.log("Updating");
      updateProgram();
   }
}

function setAudioOff() {
   if (audioContext) {
      effect.disconnect();
      if (audioContext.close)
         audioContext.close();
      audioContext = null;
      setAudioStatus("Off");
      audioStatus = false;
   }
}



// All the presets
var template = "";
var volume1 = "";
var phasedist = "";
var synth1 = "";
var synth2 = "";
var delay = "";
var premapped = "";
var demo1 = "";


function loadPreset(n) {
   console.log("Loading " + n)
   var code;
   switch (n) {
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
         code = synth1;
         break;
      case 4:
         code = synth2;
         break;
      case 5:
         code = delay;
         break;
      case 6:
         code = premapped;
         break;
      case 7:
         code = demo1;
         break;
      default:
         print("Not found")
         code = template;
         break;
   }
   var editor = ace.edit("editor");
   editor.setValue(code);
   editor.clearSelection();
   editor.gotoLine(0);
}

function loadFile(url, set_fun) {
   var request = new XMLHttpRequest();
   request.onreadystatechange = function () {
      if (request.readyState != 4) return; // Not there yet
      if (request.status != 200) {
         return;
      }
      var resp = request.responseText;
      set_fun(resp);
   }
   request.open('GET', url, true);
   request.send();
}

loadFile('https://raw.githubusercontent.com/modlfo/vult/master/examples/web/template.vult', function (txt) {
   template = txt;
   loadPreset(0);
});
loadFile('https://raw.githubusercontent.com/modlfo/vult/master/examples/web/delay.vult', function (txt) {
   delay = txt;
});
loadFile('https://raw.githubusercontent.com/modlfo/vult/master/examples/web/phasedist.vult', function (txt) {
   phasedist = txt;
});
loadFile('https://raw.githubusercontent.com/modlfo/vult/master/examples/web/synth1.vult', function (txt) {
   synth1 = txt;
});
loadFile('https://raw.githubusercontent.com/modlfo/vult/master/examples/web/synth2.vult', function (txt) {
   synth2 = txt;
});
loadFile('https://raw.githubusercontent.com/modlfo/vult/master/examples/web/volume.vult', function (txt) {
   volume1 = txt;
});
loadFile('https://raw.githubusercontent.com/modlfo/vult/master/examples/web/premaped.vult', function (txt) {
   premapped = txt;
});
loadFile('https://raw.githubusercontent.com/modlfo/vult/master/examples/web/demo1.vult', function (txt) {
   demo1 = txt;
});