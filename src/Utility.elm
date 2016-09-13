module Utility exposing (..)

import Html exposing (Html, div)
import Html.Attributes exposing (style)
import List

type alias PlayerID =
    Int



--Common definition of a vector


type alias Vector =
    { x : Int, y : Int }


zeroVector : Vector
zeroVector =
    Vector 0 0


reverseVector : Vector -> Vector
reverseVector v =
    scaleVector v -1


scaleVector : Vector -> Int -> Vector
scaleVector v n =
    Vector (v.x * n) (v.y * n)


filterMaybe : List (Maybe a) -> List a
filterMaybe list =
    List.foldr
        (\elem acc ->
            case elem of
                Nothing ->
                    acc

                Just elem ->
                    elem :: acc
        )
        []
        list


findFirst : (a -> Bool) -> List a -> Maybe a
findFirst filter l =
    List.map (\e -> if filter e then Just e else Nothing) l
    |> filterMaybe |> List.head


--Performs a list.Head operation, crashes if the list is empty.


unsafeHead : List a -> a
unsafeHead list =
    unsafe (List.head list)



--Performs a List.Tail operation, crashes if the list is empty.


unsafeTail : List a -> List a
unsafeTail list =
    unsafe (List.tail list)



--Forces a value from a Maybe, crashes if the maybe is nothing.
--Useful in instances where it is contextually impossible to get
--"Nothing" out of an operation, such as a List.head when the list
--was already checked to be non-empty.


unsafe : Maybe a -> a
unsafe x =
    case x of
        Just safe ->
            safe

        Nothing ->
            Debug.crash "unsafe operation failed!"



--Tests if a list contains a specific element using the
-- '==' comparator


listContains : List a -> a -> Bool
listContains l e =
    List.any (\b -> e == b) l



--Removes all instances of element 'e' from list 'l'


listRemove : List a -> a -> List a
listRemove l e =
    List.filter (\b -> e /= b) l


divFloatWrapper : Html msg -> Html msg
divFloatWrapper elem =
    div
        [ style
            [ ( "position", "absolute" )
            , ( "top", "0" )
            , ( "left", "0" )
            , ( "width", "1600px" )
            , ( "height", "900px" )
            ]
        ]
        [ elem ]

listUpdate : (a -> Bool) -> List a -> a -> List a
listUpdate identifier l e =
    e
        :: (List.filter
                (\i -> not (identifier i))
                l
           )