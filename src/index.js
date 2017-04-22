require('./main.css');

var Elm = require('./Main.elm');

var root = document.getElementById('root');

var app = Elm.Main.embed(root);

app.ports.writeLevelData.subscribe(function(levelDataJson) {
  // console.log('level data json', levelDataJson);
  postJSON(levelDataJson);
});

app.ports.fetchLevelData.subscribe(function(levelNumber) {

  loadJSON(function(response) {
   // Parse JSON string into object
     var actual_JSON = JSON.parse(response);
     app.ports.receiveLevelData.send(actual_JSON);
  });
});

function loadJSON(callback) {

   var xobj = new XMLHttpRequest();
   xobj.overrideMimeType("application/json");
   xobj.open('GET', '../levels/level1.json', true); // Replace 'my_data' with the path to your file
   xobj.onreadystatechange = function () {
         if (xobj.readyState == 4 && xobj.status == "200") {
           // Required use of an anonymous callback as .open will NOT return a value but simply returns undefined in asynchronous mode
           callback(xobj.responseText);
         }
   };
   xobj.send(null);
}



function postJSON(levelDataJson) {
  console.log('begin');
  var http = new XMLHttpRequest();
  var params = "text=stuff";
  http.open("POST", "http://localhost:3000", true);

  http.setRequestHeader("Content-type", "application/json");
  // http.setRequestHeader("Access-Control-Allow-Origin",  "http://localhost:3000/");
  // http.setRequestHeader("Connection", "close");

  http.onreadystatechange = function() {
      console.log('onreadystatechange');
      if (http.readyState == 4 && http.status == 200) {
          alert(http.responseText);
      }
      else {
          console.log('readyState=' + http.readyState + ', status: ' + http.status);
      }
  }

  console.log('sending...')
  http.send(params);
  console.log('end');
}
