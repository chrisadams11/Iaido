-------------------------------------------------------------------------------
--------------------------------VIEW-------------------------------------------
--The view encapsulates all rendering logic that shows our game state to the  -
--player. This section is entirely self-contained, and is only referenced once-
--outside of itself; in the main function. Because of this constraint, our    -
--game state becomes completely independent of how it is rendered, and the    -
--engine that renders it is entirely hot-swappable.                           -
-------------------------------------------------------------------------------

--The view function renders the given game state within the given window.
view : (Int,Int) -> Game -> Element
view (w,h) {state,tileList, unitSet, player1} = render { scene | objects <- [] }
