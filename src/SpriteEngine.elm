module SpriteEngine exposing (..)

import Html exposing (Html)
import Html.Attributes exposing (style)
import Utility exposing (..)
import SpriteSheet exposing (..)

drawStaticSprite : String -> Coordinate -> Vector -> Int -> Bool -> Html msg
drawStaticSprite image position size rotation flip =
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
        , if flip then flipHorizontal else style []
        ]
        []


drawSprite : Sprite -> Coordinate -> Int -> Bool -> Html msg
drawSprite sprite position rotation flip= 
    let
        spriteFramePosition = getDrawRectangle sprite
    in
        Html.div
            [ Html.Attributes.style
                [ ("background-image", "url(" ++ toString sprite.sheet ++ ")")
                , ("background-repeat", "no-repeat")
                , ("background-position", 
                    toString (spriteFramePosition.x * -1) ++ "px " ++ 
                    toString (spriteFramePosition.y * -1) ++ "px")
                , ( "position", "absolute" )
                , ( "left", toString position.x ++ "px" )
                , ( "top", toString position.y ++ "px" )
                , ( "width", toString sprite.size.x ++ "px" )
                , ( "height", toString sprite.size.y ++ "px" )
                , ( "transform", "rotate(" ++ toString rotation ++ "deg)" )
                ]
            , if flip then flipHorizontal else style []
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


flipHorizontal : Html.Attribute msg
flipHorizontal =
    style
        [ ("-moz-transform", "scaleX(-1)")
        , ("-o-transform", "scaleX(-1)")
        , ("-webkit-transform", "scaleX(-1)")
        , ("transform", "scaleX(-1)")
        , ("filter", "FlipH")
        , ("-ms-filter", "FlipH")
        ]