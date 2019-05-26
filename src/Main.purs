module Main (main) where

import Prelude
  ( Unit
  , bind
  , mempty
  , unit
  , show
  )
import Oak.Html.Events (onClick)
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
  = Get
  | GetResult (Either AjaxError Response)


view :: Model -> Html Msg
view model =
  div []
    [ div [] [ button [ onClick Get ] [ text "get" ] ]
    , div [] [ text model.message ]
    ]


next :: Msg -> Model -> (Msg -> Effect Unit) -> Effect Unit
next msg mod h =
  case msg of

    (GetResult _) -> mempty
    -- mempty is a "do nothing" effect

    Get -> get GetResult "1.json" h
    -- send a get request to "/1.json"
    -- and decode the result into the GetResult message
    -- Left AjaxError if it failed
    -- Right a where a is the response type from the server
    --   if it was successful

update :: Msg -> Model -> Model
update msg model =
  case msg of
    Get                        -> model
    (GetResult (Left e))       -> model { message = show e }
    (GetResult (Right result)) -> model { message = result.text }


init :: Unit -> Model
init _ =
  { message: ""
  }

app :: App Model Msg Unit
app = createApp
  { init: init
  , view: view
  , update: update
  , next: next
  }

main :: Effect Unit
main = do
  rootNode <- runApp app unit
  container <- getElementById "app"
  appendChildNode container rootNode
