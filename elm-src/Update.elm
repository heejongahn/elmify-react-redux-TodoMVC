module Update exposing (Msg(..), update)

-- Standard Library
import List as L
-- Local imports
import Model exposing (..)

type alias Text = String
type alias Id = Int

type alias Msg
  = AddTodo Text
  | DeleteTodo Id
  | EditTodo Id Text
  | CompleteTodo Id
  | CompleteAll
  | ClearCompleted

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    AddTodo newText ->
      let newId = (L.foldr getMaxId -1 model.todos) + 1
          newTodo = { id = newId, completed = False, text = newText }
          newModel = newTodo :: model
      in
          ( newModel, Cmd.none )

    DeleteTodo targetId ->
      let newModel = L.filter (\todo -> todo.id /= targetId) model
      in
          ( newModel, Cmd.none )

    EditTodo targetId newText ->
      let newModel = L.map (\todo ->
          if todo.id /= targetId
            then todo
            else { todo | text = newText }
          ) model
      in
          ( newModel, Cmd.none )

    CompleteTodo targetId ->
      let newModel = L.map (\todo ->
          if todo.id /= targetId
            then todo
            else { todo | completed = !todo.completed }
          ) model
      in
          ( newModel, Cmd.none )

    CompleteAll ->
      let newModel = L.map (\todo -> { todo | completed = True } ) model
      in
          ( newModel, Cmd.none )

    ClearCompleted ->
      let newModel = L.filter (\todo -> !todo.completed) model
      in
          ( newModel, Cmd.none )


getMaxId : Todo -> Id -> Id
getMaxId todo id = max (odostodo.id, id)
