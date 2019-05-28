module Main (main) where

import Prelude
  ( Unit
  , bind
  , mempty
  , show
  , (<>)
  )
import Oak.Html.Events (onClick)
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
  )
import Oak.Document
  ( appendChildNode
  , getElementById
  )
import Oak.Ajax
  ( get
  , AjaxError
  )


type Model = { message :: String }

type Response = { text :: String }

data Msg
  = Get String
  | GetResult (Either AjaxError Response)


instance showMsg :: Show Msg where
  show msg =
    case msg of
      Get url -> "Get " <> url
      GetResult (Left e) -> "GetResult Left " <> show e
      GetResult (Right r) -> "GetResult Right " <> show r

view :: Model -> Html Msg
view model =
  div []
    [ div [] [ button [ onClick (Get "1.json") ] [ text "get 1" ] ]
    , div [] [ button [ onClick (Get "2.json") ] [ text "get 2" ] ]
    , div [] [ button [ onClick (Get "3.json") ] [ text "get 3" ] ]
    , div [] [ text model.message ]
    ]


next :: Msg -> Model -> (Msg -> Effect Unit) -> Effect Unit
next msg mod h =
  case msg of

    (GetResult _) -> mempty
    -- mempty is a "do nothing" effect

    Get url -> get GetResult url h
    -- send a get request to "/1.json"
    -- and decode the result into the GetResult message
    -- Left AjaxError if it failed
    -- Right a where a is the response type from the server
    --   if it was successful

update :: Msg -> Model -> Model
update msg model =
  case msg of
    Get url                    -> model { message = "getting " <> url <> "..." }
    (GetResult (Left e))       -> model { message = show e }
    (GetResult (Right result)) -> model { message = result.text }


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
