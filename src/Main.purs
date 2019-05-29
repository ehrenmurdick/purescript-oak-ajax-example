module Main (main) where

import Prelude
  ( Unit
  , bind
  , map
  , mempty
  , show
  , (<>)
  , ($)
  )
import Style as S
import Oak.Html.Events (onClick, onInput)
import Oak.Html.Attribute ( style, value )
import Oak.Debug ( debugApp )
import Data.Show (class Show)
import Data.Either (Either(..))
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
  )


type Model =
  { todos :: Array Todo
  , form :: {
      text :: String
    }
  }

type Todo =
  { id :: Int
  , text :: String
  }

type Index = Array Todo

data Msg
  = Get
  | Input String
  | Got Response

data Response
  = Index (Either AjaxError Index)

instance showMsg :: Show Msg where
  show msg =
    case msg of
      Get -> "Get"
      Got r -> "Got " <> show r
      Input str -> "Input"


instance showResponse :: Show Response where
  show resp =
    case resp of
      Index (Right _) -> "Index"
      Index e -> "Index " <> show e


view :: Model -> Html Msg
view model =
  div [ style S.container ]
    [ section [ style S.section ]
      [ div [] [ button [ onClick (Get) ] [ text "Get Todos" ] ]
      ]
    , section [ style $ S.big <> S.section ]
      [ ul [ style S.list ]
        (map showTodo model.todos)
      ]
    , section [ style $ S.big <> S.section ]
      [ div [ style S.big ]
        [ input [ onInput Input, value model.form.text ] []
        ]
      ]
    ]

showTodo :: Todo -> Html Msg
showTodo todo =
  li [] [ text todo.text ]


urlFor :: String -> String
urlFor str = "http://localhost:3000/todos" <> str


next :: Msg -> Model -> (Msg -> Effect Unit) -> Effect Unit
next msg mod h =
  case msg of
    Got _ -> mempty
    Input _ -> mempty
    Get -> get Index (urlFor "") (map h Got)

update :: Msg -> Model -> Model
update msg model =
  case msg of
    Get -> model
    Got (Index (Right todos)) -> model { todos = todos }
    Got _ -> model
    Input str -> model { form { text = str } }


init :: Model
init =
  { todos: []
  , form: { text: "" }
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
  rootNode <- runApp (debugApp app)
  container <- getElementById "app"
  appendChildNode container rootNode
