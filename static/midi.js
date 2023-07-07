var context = new AudioContext(),
    oscillators = {};

WebMidi.enable().then(onEnabled)
.catch(err => alert(err));

function onEnabled() {
  WebMidi.inputs.forEach(function(input) {console.log(input.manufacturer, input.name)});
  var myInput = WebMidi.getInputByName("WORLDE MIDI");

//   const myInput = WebMidi.inputs[0];
//
  myInput.addListener("noteon", function (e) {
  var newNote = new Note(e.note.number);
  var miFreq = (Math.pow(2, ((newNote.number - 69) / 12)) * 440);
  // tocaNota(miFreq, newNote.duration);
  tocaNota(miFreq);
  // console.log(e.note);
});

  myInput.addListener("noteoff", function (e2) {
  // console.log(e2.note);
  var newNote = new Note(e2.note.number);
  var miFreq = (Math.pow(2, ((newNote.number - 69) / 12)) * 440);
  paraNota(miFreq);
});

WebMidi.addListener('pitchbend', function(e) {
  console.log("Pitch value: " + e.value);
});


function tocaNota (frequency) {
    oscillators[frequency] = context.createOscillator();
    oscillators[frequency].frequency.value = frequency;
    oscillators[frequency].connect(context.destination);
    oscillators[frequency].start(context.currentTime);
    // oscillators[frequency].stop(context.currentTime + dur);
    // oscillators[frequency].disconnect();
  }

function paraNota (frequency) {
  oscillators[frequency].stop(context.currentTime);
  oscillators[frequency].disconnect();
}

}
