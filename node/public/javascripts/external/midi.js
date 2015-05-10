
page1 = true;
page2 = true;
lock = false;
function midiMessageReceived( ev ) {
  var cmd = ev.data[0] >> 4;
  var channel = ev.data[0] & 0xf;
  var noteNumber = ev.data[1];
  var velocity = ev.data[2];



  if (channel == 9)
    return
  if ( cmd==8 || ((cmd==9)&&(velocity==0)) ) { // with MIDI, note on with velocity zero is the same as note off
    if(vult_object && lock==false){
      lock = true;
      vult_object.live__process_noteOff(vult_object.context,noteNumber);
      lock = false;
    }
  } else if (cmd == 9) {
    if(vult_object && lock==false){
      lock = true;
      vult_object.live__process_noteOn(vult_object.context,noteNumber);
      lock = false;
    }
  } else if (cmd == 11) {
    if(vult_object){
      if(noteNumber==40)
        page1 = true;
      if(noteNumber==42)
        page1 = false;
      if(noteNumber==44)
        page2 = true;
      if(noteNumber==45)
        page2 = false;
      if(noteNumber>=30 && noteNumber<=33 && page1==true){
        switch(noteNumber-30){
          case 0:
            vult_object.context.param1_in = (velocity/127);
            break;
          case 1:
            vult_object.context.param2_in = (velocity/127);
            break;
          case 2:
            vult_object.context.param3_in = (velocity/127);
            break;
          case 3:
            vult_object.context.param4_in = (velocity/127);
            break;
        }
      }
      if(noteNumber>=30 && noteNumber<=33 && page1==false){
        switch(noteNumber-30){
          case 0:
            vult_object.context.param5_in = (velocity/127);
            break;
          case 1:
            vult_object.context.param6_in = (velocity/127);
            break;
          case 2:
            vult_object.context.param7_in = (velocity/127);
            break;
          case 3:
            vult_object.context.param8_in = (velocity/127);
            break;
        }
      }
      if(noteNumber>=34 && noteNumber<=37 && page2==true){
        switch(noteNumber-34){
          case 0:
            vult_object.context.param9_in = (velocity/127);
            break;
          case 1:
            vult_object.context.param10_in = (velocity/127);
            break;
          case 2:
            vult_object.context.param11_in = (velocity/127);
            break;
          case 3:
            vult_object.context.param12_in = (velocity/127);
            break;
        }
      }
      if(noteNumber>=34 && noteNumber<=37 && page2==false){
        switch(noteNumber-34){
          case 0:
            vult_object.context.param13_in = (velocity/127);
            break;
          case 1:
            vult_object.context.param14_in = (velocity/127);
            break;
          case 2:
            vult_object.context.param15_in = (velocity/127);
            break;
          case 3:
            vult_object.context.param16_in = (velocity/127);
            break;
        }
      }
    }
    //controller( noteNumber, velocity/127.0);
  } else if (cmd == 14) {
    // pitch wheel
    //pitchWheel( ((velocity * 128.0 + noteNumber)-8192)/8192.0 );
  } else if ( cmd == 10 ) {  // poly aftertouch
    //polyPressure(noteNumber,velocity/127)
  }
  console.log("" + ev.data[0] + " " + ev.data[1] + " " + ev.data[2])
  if(vult_object){
    //console.log(vult_object.context)
  }
}

var selectMIDI = null;
var midiAccess = null;
var midiIn = null;

function onMIDIInit( midi ) {
  midiAccess = midi;
  for (var input of midiAccess.inputs.values()){
    if(input.name=="Port1"){
      input.onmidimessage = midiMessageReceived;
      console.log(input)
    }
  }
}

function onMIDISystemError( err ) {
  //document.getElementById("synthbox").className = "error";
  console.log( "MIDI not initialized - error encountered:" + err.code );
}

//init: start up MIDI
window.addEventListener('load', function() {
  if (navigator.requestMIDIAccess)
    navigator.requestMIDIAccess().then( onMIDIInit, onMIDISystemError );

});