module Globals where

--Player ID's are just integers, could change later
type alias PlayerID = Int


--Common definition of a vector
type alias Vector = {x:Int, y:Int}

filterMaybe : List Maybe a -> List a
filterMaybe list = List.foldr (\elem acc -> case elem of Nothing -> acc Just elem -> elem :: acc) [] list


--Performs a list.Head operation, crashes if the list is empty.
unsafeHead : List a -> a
unsafeHead list = unsafe (List.head list)


--Performs a List.Tail operation, crashes if the list is empty.
unsafeTail : List a -> List a
unsafeTail list = unsafe (List.tail list)


--Forces a value from a Maybe, crashes if the maybe is nothing.
--Useful in instances where it is contextually impossible to get
--"Nothing" out of an operation, such as a List.head when the list
--was already checked to be non-empty.
unsafe : Maybe a -> a
unsafe x = case x of
  Just safe -> safe
  Nothing   -> Debug.crash "unsafe operation failed!"
