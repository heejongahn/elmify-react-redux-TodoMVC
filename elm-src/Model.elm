module Model exposing (..)

type alias Todo =
  { id : Int
  , completed : Bool
  , text : String
  }

type alias Model = [ Todo ]
