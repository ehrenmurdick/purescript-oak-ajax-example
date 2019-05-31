module Main (main) where

import Prelude
  ( Unit
  , bind
  , const
  , map
  , not
  , mempty
  , show
  , (>>>)
  , (<>)
  , ($)
  )
import Style as S
import Oak.Html.Events (onClick, onInput)
import Oak.Html.Attribute ( style, value, type_, checked )
import Oak.Debug ( DebugMsg(..), debugApp )
import Data.Array (snoc, updateAt, mapWithIndex)
import Data.Show (class Show)
import Data.Either (Either(..))
import Data.Maybe
import Effect (Effect)
import Oak
  ( runApp
  , App
  , createApp
  )
import Oak.Html
  ( Html
  , div
  , text
  , button
  , section
  , ul
  , li
  , input
  )
import Oak.Document
  ( appendChildNode
  , getElementById
  )
import Oak.Ajax
  ( get
  , delete
  , post
  , AjaxError
  , put
  )


type Model =
  { todos :: Array Todo
  , form :: {
      text :: String,
      done :: Boolean
    }
  }

type Todo =
  { id :: Int
  , text :: String
  , done :: Boolean
  }

type Form =
  { text :: String
  , done :: Boolean
  }


type Index = Array Todo

data Msg
  = Get
  | Input String
  | Got Response
  | Post
  | Toggle Int Todo

data Response
  = Index (Either AjaxError Index)
  | PostR (Either AjaxError Todo)

instance showMsg :: Show Msg where
  show msg =
    case msg of
      Get -> "Get"
      Post -> "Post"
      Got r -> "Got " <> show r
      Input str -> "Input " <> str
      Toggle _ t -> "Toggle: " <> t.text


instance showResponse :: Show Response where
  show resp =
    case resp of
      Index (Right _) -> "Index"
      Index e -> "Index " <> show e
      PostR (Right d) -> "Post " <> show d
      PostR e -> "Post " <> show e


view :: Model -> Html Msg
view model =
  div [ style S.container ]
    [ section [ style $ S.big <> S.section ]
      [ ul [ style S.list ]
        ((mapWithIndex showTodo model.todos) <>
        [ div [ style S.big ]
          [ input [ onInput Input, value model.form.text ] []
          , button [ onClick Post ] [ text "create" ]
          ]
        ])
      ]
    ]

showTodo :: Int -> Todo -> Html Msg
showTodo i todo =
  li []
    [ input
      [ style S.checkbox
      , type_ "checkbox"
      , checked todo.done
      , onClick (Toggle i todo)
      ] []
    , text todo.text
    ]


urlFor :: String -> String
urlFor str = "http://localhost:3000/todos" <> str


postBody :: Model -> Maybe Form
postBody model =
  Just $ { text: model.form.text, done: false }


ignoreResponse :: Either AjaxError String -> Effect Unit
ignoreResponse a = mempty


next :: Msg -> Model -> (Msg -> Effect Unit) -> Effect Unit
next msg mod h =
  case msg of
    Got _ -> mempty
    Input _ -> mempty
    Toggle _ t -> do
      put
        (Just (t { done = not t.done }))
        (urlFor ("/" <> show t.id))
        ignoreResponse
    Get -> do
      get
        (urlFor "")
        (Index >>> Got >>> h)
    Post -> do
      post
        (postBody mod)
        (urlFor "")
        (PostR >>> Got >>> h)

update :: Msg -> Model -> Model
update msg model =
  case msg of
    Get -> model
    Post -> model
    Toggle i todo -> model {
      todos = fromMaybe model.todos (updateAt i (todo { done = not todo.done }) model.todos)
    }
    Got (Index (Right todos)) -> model { todos = todos }
    Got (PostR (Right todo)) ->
      model
        { todos = snoc model.todos todo
        , form { text = "" }
        }
    Got _ -> model
    Input str -> model { form { text = str } }


init :: Model
init =
  { todos: []
  , form: { text: "", done: false }
  }

app :: App Msg Model
app = createApp
  { init: init
  , view: view
  , update: update
  , next: next
  }

main :: Effect Unit
main = do
  rootNode <- runApp (debugApp app) (Just (Wrap Get))
  container <- getElementById "app"
  appendChildNode container rootNode
