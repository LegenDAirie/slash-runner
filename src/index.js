require('./main.css');

var Elm = require('./Main.elm');

var root = document.getElementById('root');

var app = Elm.Main.embed(root);

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
