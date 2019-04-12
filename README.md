# Todo
* No generic vectors, custom types for specific things(these vectors and intVectors are driving me nuts already)
* maybe separate things from how they are rendered???
  - See if this idea breaks down with opaque types.
* GamePlatform and the game map are different concepts. Make a game map that owns things and the locations they are at.
  - `GamePlatform` does not need its own file.
* `Dict IntVector GamePlatform.Platform` is really just the game map.(Platforms, layout, enemies spawning points?)
* Collision needs an overhaul
* All types should live in there home file
* Should GameFeel be stored in NormalPlay's model or stay in Main????
* MouseHelpers, should it be its own module???

## Little clean up tasks
* Get window width and height from flags not initial Cmd
* `Tuple.second Coordinates.gridSquareSize` is not straightforward... `gridSquareWidth` might be.
* Rename Direction in the Player Module to PlayerFacing.
* Rename Tick to Step
* Move rendering to it's own file or Folder(I think)
* Move decoding and encoding to it's own module(I think)

## Refactor order (This will solve a lot of the todos above)
1. Coordinates need to be a new way of representing the game map
2. V2 needs to becomes a way of representing location, velocities, sizes
3. GamePlatform needs to be used in or become a new GameMap or something
4. Using the two above Collision needs an overhaul

(Will soon be true)
In this game there is a distinction between the GameMap and free space.

### The GameMap represents static game data ...
* Platform placement
* Enemy spawn locations
* Item placement(If I decide to add Items)
* Initial player spawn location.

The GameMap also how level data stored and saved.
