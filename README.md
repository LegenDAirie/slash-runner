# Todo
* No generic vectors, custom types for specific things(these vectors and intVectors are driving me nuts already)
* maybe separate things from how they are rendered???
  - See if this idea breaks down with opaque types.
* GamePlatform and the game map are different concepts. Make a game map that owns things and the locations they are at.
  - `GamePlatform` does not need its own file.
* `Dict IntVector GamePlatform.Platform` is really just the game map.(Platforms, layout, enemies spawning points?)
* `Tuple.second Coordinates.gridSquareSize` is not straightforward... `gridSquareWidth` might be.
* Collision needs an overhaul
* All types should live in there home file
* Should GameFeel be stored in NormalPlay's model or stay in Main????
