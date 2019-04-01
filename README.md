# Todo
* No generic vectors, custom types for specific things(these vectors and intVectors are driving me nuts already)
* maybe separate game things from how they are rendered???
* GamePlatform and the game map are different concepts. Make a game map that owns things and the locations they are at.
* GamePlatform doesn't need to be it's own file.
* `Dict IntVector GamePlatform.Platform` is really just the game map.(Platforms, layout, but not enemies)
* `Tuple.second Coordinates.gridSquareSize` is not straightforward... `gridSquareWidth` might be
