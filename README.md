# Ideas
* Dashing doesn't end when the player slows to a stop. Dashing should last for a set number of frames
* PlayerState could be changed with helper functions specific to that state. almost like an opeque type
* frameCount should be a part of PlayerState to keep PlayerState change and frame set in sync
* PlayerState shouldn't be a maybe, Other could be used in place of a non PersistantState
* Pull collision out into a routine of some kind
* numbers should be wrapped in some kind of type to keep in the bounds of what javascript can understand. Don't want super large, super small infinity or NaN

#### potential update pipeline to ensure the right state gets the right update
```elm
  updatedPlayer
    |> updatePlayerFromButtonState
    |> updatePlayerLocation
    |> collision
    |> updatePlayerstateFromCollision
```
