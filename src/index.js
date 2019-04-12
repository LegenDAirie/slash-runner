import './main.css';
import { Elm } from './Main.elm';

var app = Elm.Main.init({
  node: document.querySelector('main'),
  flags: { width: window.innerWidth, height: window.innerHeight }
})

function onKeyDown(e) {
  var canvas = document.getElementsByTagName('canvas')[0];
  if (canvas && canvas.webkitRequestFullScreen && e.code === "KeyF"){
    canvas.webkitRequestFullScreen();
  }
}

document.onkeydown  = onKeyDown;

app.ports.writeLevelData.subscribe(function(levelDataJson) {
  // console.log('level data json', levelDataJson);
  console.log('save level data');
  localStorage.setItem('levelData', levelDataJson);
});

app.ports.fetchLevelData.subscribe(function(levelNumber) {

  var levelData = localStorage.getItem('levelData');
    // Parse JSON string into object
    var actual_JSON = JSON.parse(levelData);
    app.ports.receiveLevelData.send(actual_JSON);
});

app.ports.getGamePadState.subscribe(function(playerNumber) {
  var gamePads = navigator.getGamepads ? navigator.getGamepads() : []
  var gamePad = gamePads[playerNumber]
  var releventButtons = {
    gamepadConnected: gamePad !== null,
    up: gamePad ? gamePad.buttons[12].pressed : false,
    left: gamePad ? gamePad.buttons[14].pressed : false,
    right: gamePad ? gamePad.buttons[15].pressed : false,
    down: gamePad ? gamePad.buttons[13].pressed : false,
    jump: gamePad ? gamePad.buttons[0].pressed : false,
    dash: gamePad ? gamePad.buttons[2].pressed : false
  }
  app.ports.receiveGamePadState.send(releventButtons)
});

window.addEventListener("gamepadconnected", function(e) {
  var gp = navigator.getGamepads()[e.gamepad.index];
});

window.addEventListener("gamepaddisconnected", function(e) {
});



// function loadJSON(callback) {
//
//    var xobj = new XMLHttpRequest();
//    xobj.overrideMimeType("application/json");
//    xobj.open('GET', '../levels/level1.json', true); // Replace 'my_data' with the path to your file
//    xobj.onreadystatechange = function () {
//          if (xobj.readyState == 4 && xobj.status == "200") {
//            // Required use of an anonymous callback as .open will NOT return a value but simply returns undefined in asynchronous mode
//            callback(xobj.responseText);
//          }
//    };
//    xobj.send(null);
// }
