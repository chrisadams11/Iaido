module SpriteEngine exposing (..)

import Html exposing (Html)
import Html.Attributes exposing (style)
import Utility exposing (..)


type alias Sprite msg =
    Html msg


drawSprite : String -> Coordinate -> Vector -> Int -> Sprite msg
drawSprite image position size rotation =
    Html.img
        [ Html.Attributes.src image 
        , Html.Attributes.style
            [ ( "position", "absolute" )
            , ( "left", toString position.x ++ "px" )
            , ( "top", toString position.y ++ "px" )
            , ( "width", toString size.x ++ "px" )
            , ( "height", toString size.y ++ "px" )
            , ( "transform", "rotate(" ++ toString rotation ++ "deg)" )
            ]
        ]
        []


drawFrame : Vector -> List (Sprite msg) -> Html msg
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
