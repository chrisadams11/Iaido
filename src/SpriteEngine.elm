module SpriteEngine exposing (..)

import Html exposing (Html)
import Html.Attributes exposing (style)
import Utility exposing (..)
import Sprite exposing (..)


type DrawOption 
    = DrawOption_FlipHorizontal
    | DrawOption_FlipVertical


drawStaticSprite : String -> Coordinate -> Vector -> Int -> List DrawOption -> Html msg
drawStaticSprite image position size rotation options =
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
        , if List.member DrawOption_FlipHorizontal options 
            then flipHorizontal 
            else style []
        ]
        []


drawSprite : Sprite -> Coordinate -> Int -> List DrawOption -> Html msg
drawSprite sprite position rotation options = 
    let
        spriteState = getSpriteDrawState sprite
    in
        Html.div
            [ Html.Attributes.style
                [ ("background-image", "url(" ++ toString sprite.sheetFile ++ ")")
                , ("background-repeat", "no-repeat")
                , ("background-position", 
                    toString (spriteState.drawPos.x * -1) ++ "px " ++ 
                    toString (spriteState.drawPos.y * -1) ++ "px")
                , ( "position", "absolute" )
                , ( "left", toString position.x ++ "px" )
                , ( "top", toString position.y ++ "px" )
                , ( "width", toString spriteState.drawSize.x ++ "px" )
                , ( "height", toString spriteState.drawSize.y ++ "px" )
                , ( "transform", "rotate(" ++ toString rotation ++ "deg)" )
                ]
            , if List.member DrawOption_FlipHorizontal options 
                then flipHorizontal 
                else style []
            ]
            []


drawEmpty : Html msg
drawEmpty = Html.div [] []


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