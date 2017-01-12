module SpriteSheet exposing (..)


import Utility exposing (..)
import Array exposing (..)


type alias Sprite =
  { sheet : String
  , animations : Array SpriteAnimation
  , currentAnimation : SpriteAnimation
  , size : Vector
  , currentFrame : Vector
  }


type alias SpriteAnimation =
  { frames : Int
  , isLoop : Bool
  }


advanceAnimation : Float -> Sprite -> Sprite
advanceAnimation speed sprite =
  if sprite.currentFrame.x == (sprite.currentAnimation.frames - 1)
    then 
      if sprite.currentAnimation.isLoop
        then
          { sprite | currentFrame = {x = 0, y = sprite.currentFrame.y}}
      else
          enterAnimation 0 sprite
    else
      { sprite | currentFrame = {x = sprite.currentFrame.x + 1, y = sprite.currentFrame.y}}


enterAnimation : Int -> Sprite -> Sprite
enterAnimation row sprite =
  { sprite 
    | currentFrame = {x = 0, y = row}
    , currentAnimation = Array.get row sprite.animations |> unsafe
  }


getDrawRectangle : Sprite -> Vector
getDrawRectangle sprite =
  { x = sprite.currentFrame.x * sprite.size.x
  , y = sprite.currentFrame.y * sprite.size.y
  }