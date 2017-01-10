module SpriteEngine exposing (..)

import Html exposing (Html)
import Html.Attributes exposing (style)
import Utility exposing (..)
import SpriteSheet exposing (..)

drawStaticSprite : String -> Coordinate -> Vector -> Int -> Html msg
drawStaticSprite image position size rotation =
    Html.div
        [ Html.Attributes.style
            [ ("background-image", "url(" ++ toString image ++ ")")
            , ("background-size", "contain")
            , ("background-repeat", "no-repeat")
            , ( "position", "absolute" )
            , ( "left", toString position.x ++ "px" )
            , ( "top", toString position.y ++ "px" )
            , ( "width", toString size.x ++ "px" )
            , ( "height", toString size.y ++ "px" )
            , ( "transform", "rotate(" ++ toString rotation ++ "deg)" )
            ]
        ]
        []


drawSprite : Sprite -> Coordinate -> Vector -> Int -> Html msg
drawSprite sprite position size rotation = 
    let
        spriteFramePosition = getDrawRectangle sprite
    in
        Html.div
            [ Html.Attributes.style
                [ ("background-image", "url(" ++ toString sprite.sheet ++ ")")
                , ("background-size", "cover")
                , ("background-repeat", "no-repeat")
                , ("background-position", 
                    toString (spriteFramePosition.x * -1) ++ "px " ++ 
                    toString (spriteFramePosition.y * -1) ++ "px")
                , ( "position", "absolute" )
                , ( "left", toString position.x ++ "px" )
                , ( "top", toString position.y ++ "px" )
                , ( "width", toString size.x ++ "px" )
                , ( "height", toString size.y ++ "px" )
                , ( "transform", "rotate(" ++ toString rotation ++ "deg)" )
                ]
            ]
            []


drawFrame : Vector -> List (Html msg) -> Html msg
drawFrame size sprites =
    Html.div
        [ Html.Attributes.style
            [ ( "position", "absolute" )
            , ( "width", toString size.x ++ "px" )
            , ( "height", toString size.y ++ "px" )
            , ( "left", "20px" )
            , ( "top", "20px" )
            ]
        ]
        sprites
