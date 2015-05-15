
function setMidiStatus(msg){
   var label = document.getElementById("midi_status");
   label.textContent = "MIDI: "+msg;

}

var midiCount = 0;

function midiMessageReceived( ev ) {
  var cmd = ev.data[0] >> 4;
  var channel = ev.data[0] & 0xf;
  var noteNumber = ev.data[1];
  var velocity = ev.data[2];


  if ( cmd==8 || ((cmd==9)&&(velocity==0)) ) {
    // Note on
    sendNoteOn(noteNumber,velocity);
  } else if (cmd == 9) {
    sendNoteOff(noteNumber);

  } else if (cmd == 11) {
    sendControlChange(noteNumber,velocity);
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
