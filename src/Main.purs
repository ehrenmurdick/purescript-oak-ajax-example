module Main (main) where

import Prelude
  ( Unit
  , bind
  , mempty
  , show
  , (<>)
  , ($)
  )
import Style as S
import Oak.Html.Events (onClick)
import Oak.Html.Attribute ( style )
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
  )
import Oak.Document
  ( appendChildNode
  , getElementById
  )
import Oak.Ajax
  ( get
  , delete
  , AjaxError
  )


type Model = { message :: String }

type Response = { text :: String }
type DelResp  = { }

data Msg
  = Get String
  | Delete String
  | GetResult (Either AjaxError Response)
  | DelResult (Either AjaxError DelResp)


instance showMsg :: Show Msg where
  show msg =
    case msg of
      Get       url -> "Get "       <> url
      Delete    url -> "Delete "    <> url
      GetResult a   -> "GetResult " <> show a
      DelResult a   -> "DelResult " <> show a

view :: Model -> Html Msg
view model =
  div [ style S.container ]
    [ section [ style S.section ]
      [ div [] [ button [ onClick (Get "1") ] [ text "get 1" ] ]
      , div [] [ button [ onClick (Get "2") ] [ text "get 2" ] ]
      , div [] [ button [ onClick (Get "3") ] [ text "get 3" ] ]
      ]
    , section [ style S.section ]
      [ div [] [ button [ onClick (Delete "1") ] [ text "del 1" ] ]
      , div [] [ button [ onClick (Delete "2") ] [ text "del 2" ] ]
      , div [] [ button [ onClick (Delete "3") ] [ text "del 3" ] ]
      ]
    , section [ style $ S.big <> S.section ]
      [ div [ style S.big ] [ text model.message ]
      ]
    ]


urlFor :: String -> String
urlFor str = "http://localhost:3000/greetings/" <> str

next :: Msg -> Model -> (Msg -> Effect Unit) -> Effect Unit
next msg mod h =
  case msg of

    GetResult _ -> mempty

    DelResult _ -> mempty

    Get str -> get GetResult (urlFor str) h

    Delete str -> delete DelResult (urlFor str) h

update :: Msg -> Model -> Model
update msg model =
  case msg of
    Get url                    -> model { message = "getting " <> url <> "..." }
    Delete url                 -> model { message = "deleting " <> url <> "..." }
    (GetResult (Left e))       -> model { message = show e }
    (GetResult (Right result)) -> model { message = result.text }
    (DelResult (Left e))       -> model { message = show e }
    (DelResult (Right result)) -> model { message = "done." }


init :: Model
init =
  { message: ""
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
