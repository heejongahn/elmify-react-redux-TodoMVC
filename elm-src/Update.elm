module Update exposing (Msg(..), update)

-- Standard Library
import List as L
-- Local imports
import Model exposing (..)

type alias Text = String
type alias Id = Int

type alias Msg
  = ChangeNewText Text
  | AddTodo
  | DeleteTodo Id
  | EditTodo Id Text
  | CompleteTodo Id
  | CompleteAll
  | ClearCompleted

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    ChangeNewText text ->
      ( { model | newText = text }, Cmd.none )
    AddTodo ->
      let newId = (L.foldr getMaxId -1 model.todos) + 1
          newTodo = { id = newId, completed = False, text = model.newText }
          newTodos = newTodo :: model.todos
      in
          ( { model | todos = newTodos, newText = "" }, Cmd.none )

    DeleteTodo targetId ->
      let newTodos = L.filter (\todo -> todo.id /= targetId) model.todos
      in
          ( { model | todos = newTodos }, Cmd.none )

    EditTodo targetId newText ->
      let newTodos = L.map (\todo ->
          if todo.id /= targetId
            then todo
            else { todo | text = newText }
          ) model.todos
      in
          ( { model | todos = newTodos }, Cmd.none )

    CompleteTodo targetId ->
      let newTodos = L.map (\todo ->
          if todo.id /= targetId
            then todo
            else { todo | completed = !todo.completed }
          ) model.todos
      in
          ( { model | todos = newTodos }, Cmd.none )

    CompleteAll ->
      let newTodos = L.map (\todo -> { todo | completed = True } ) model.todos
      in
          ( { model | todos = newTodos }, Cmd.none )

    ClearCompleted ->
      let newTodos = L.filter (\todo -> !todo.completed) model.todos
      in
          ( { model | todos = newTodos }, Cmd.none )


getMaxId : Todo -> Id -> Id
getMaxId todo id = max (todo.id, id)
