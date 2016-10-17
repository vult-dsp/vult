
function setMidiStatus(msg){
   var label = document.getElementById("midi_status");
   label.textContent = "MIDI: "+msg;

}

var midiCount = 0;
var updateCC = null;

function midiMessageReceived( ev ) {
  var cmd = ev.data[0] >> 4;
  var channel = ev.data[0] & 0xf;
  var noteNumber = ev.data[1];
  var velocity = ev.data[2];


  if ( cmd==8 || ((cmd==9)&&(velocity==0)) ) {
    // Note on
    sendNoteOff(noteNumber,channel);
  } else if (cmd == 9) {
    sendNoteOn(noteNumber,velocity,channel);
  } else if (cmd == 11) {
    sendControlChange(noteNumber,velocity,channel);
    if(updateCC!=null){
      updateCC(noteNumber,velocity);
    }
  } else if (cmd == 14) {
    // Pitch weel
  } else if ( cmd == 10 ) {

  }
  midiCount = midiCount+1;
  setMidiStatus("Received "+midiCount);
}

// Keeps the main midi object
var midiAccess = null;

function onMIDIInit( midi ) {
  midiAccess = midi;
  for (var input of midiAccess.inputs.values()){
    input.onmidimessage = midiMessageReceived;
    console.log("Opening port "+input.name);
    setMidiStatus("On");
  }
}

function onMIDISystemError( err ) {
  //document.getElementById("synthbox").className = "error";
  console.log( "MIDI not initialized - error encountered:" + err.code );
}

//init: start up MIDI
window.addEventListener('load', function() {
  if(navigator.requestMIDIAccess)
    navigator.requestMIDIAccess().then( onMIDIInit, onMIDISystemError );

});


// UI elements
nx.onload = function() {
  nx.colorize("#DBBB2D"); // sets accent (default)
  nx.colorize("border", "#222222");
  nx.colorize("fill", "#6E6E6E");
  keyboard_input.octaves = 5;
  keyboard_input.init();

  CC30.on('*',function(data){ sendControlChange(30,Math.floor(127*data.value))});
  CC31.on('*',function(data){ sendControlChange(31,Math.floor(127*data.value))});
  CC32.on('*',function(data){ sendControlChange(32,Math.floor(127*data.value))});
  CC33.on('*',function(data){ sendControlChange(33,Math.floor(127*data.value))});
  CC34.on('*',function(data){ sendControlChange(34,Math.floor(127*data.value))});
  CC35.on('*',function(data){ sendControlChange(35,Math.floor(127*data.value))});
  CC36.on('*',function(data){ sendControlChange(36,Math.floor(127*data.value))});
  CC37.on('*',function(data){ sendControlChange(37,Math.floor(127*data.value))});
  CC38.on('*',function(data){ sendControlChange(38,Math.floor(127*data.value))});
  CC39.on('*',function(data){ sendControlChange(39,Math.floor(127*data.value))});
  CC40.on('*',function(data){ sendControlChange(40,Math.floor(127*data.value))});
  CC41.on('*',function(data){ sendControlChange(41,Math.floor(127*data.value))});

  keyboard_input.on('*',function(data){
    if(data.on != 0){
      sendNoteOn(data.note-24,100,0);
    }
    else
    {
      sendNoteOff(data.note-24,0);
    }
  });

  updateCC = function(ctrl,value){
    var fixed = value /127;
    switch(ctrl){
      case 30: CC30.set({ value:fixed }); break;
      case 31: CC31.set({ value:fixed }); break;
      case 32: CC32.set({ value:fixed }); break;
      case 33: CC33.set({ value:fixed }); break;
      case 34: CC34.set({ value:fixed }); break;
      case 35: CC35.set({ value:fixed }); break;
      case 36: CC36.set({ value:fixed }); break;
      case 37: CC37.set({ value:fixed }); break;
      case 38: CC38.set({ value:fixed }); break;
      case 39: CC39.set({ value:fixed }); break;
      case 40: CC40.set({ value:fixed }); break;
      case 41: CC41.set({ value:fixed }); break;
      default: break;
    }
  }

}


