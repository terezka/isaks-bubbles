module Main exposing (..)

import Browser
import Browser.Events
import Random
import Html as H
import Html.Attributes as HA
import Json.Decode as D


main =
  Browser.element
    { init = \() -> init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


type alias Model =
  { bubble : Coordinates
  , mouse : Coordinates
  , color : Color
  }


type alias Coordinates =
  { x : Int, y : Int }


type Color
  = Purple
  | Blue
  | LightBlue
  | Turquois
  | Yellow
  | Orange


init : ( Model, Cmd Msg )
init =
  ( Model (Coordinates 0 0) (Coordinates 0 0) Purple
  , newBubble
  )


newBubble : Cmd Msg
newBubble =
  Random.generate identity (Random.map2 OnNewBubble randomColor randomCoordinates)

randomColor : Random.Generator Color
randomColor =
  Random.uniform Purple [Purple, Blue, LightBlue, Turquois, Yellow, Orange]


randomCoordinates : Random.Generator Coordinates
randomCoordinates =
  Random.map2 Coordinates (Random.int 1 100) (Random.int 1 100)


type Msg
  = OnNewBubble Color Coordinates
  | OnMouseMove Coordinates
  | OnMouseClick Coordinates


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    OnNewBubble color coords ->
      ( { model | bubble = coords, color = color }, Cmd.none )

    OnMouseMove coords ->
      ( { model | mouse = coords }, Cmd.none )

    OnMouseClick coords ->
      ( model, newBubble )


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ Browser.Events.onMouseMove (D.map OnMouseMove mouseCoordsDecoder)
    , Browser.Events.onClick (D.map OnMouseClick mouseCoordsDecoder)
    ]


mouseCoordsDecoder : D.Decoder Coordinates
mouseCoordsDecoder =
  D.map2 (\x y -> Coordinates (round x) (round y))
    (D.field "pageX" D.float)
    (D.field "pageY" D.float)


view : Model -> H.Html Msg
view model =
  H.div
    [ HA.style "width" "500px"
    , HA.style "height" "500px"
    , HA.style "position" "relative"
    , HA.style "margin" "50px"
    ]
    [ viewBubble model ]


viewBubble : Model -> H.Html Msg
viewBubble model =
  H.div
    [ HA.style "position" "absolute"
    , HA.style "left" (String.fromInt model.bubble.x ++ "%")
    , HA.style "top" (String.fromInt model.bubble.y ++ "%")
    , HA.style "transform" "translate(-50%, 50%)"
    , HA.style "width" "100px"
    , HA.style "height" "100px"
    ]
    [ H.div
        [ HA.style "background" (toCssColor model.color)
        , HA.style "border-radius" "100%"
        , HA.style "position" "absolute"
        , HA.style "border-radius" "100%"
        , HA.style "width" "100px"
        , HA.style "height" "100px"
        ]
        []
    , H.div
        [ HA.style "background" "white"
        , HA.style "position" "absolute"
        , HA.style "left" "60px"
        , HA.style "top" "20px"
        , HA.style "border-radius" "100%"
        , HA.style "width" "20px"
        , HA.style "height" "20px"
        ]
        []
    ]


toCssColor : Color -> String
toCssColor color =
  case color of
    Purple -> bubblePurple
    Blue -> bubbleBlue
    LightBlue -> bubbleLightBlue
    Turquois -> bubbleTurqouise
    Yellow -> bubbleYellow
    Orange -> bubbleOrange


bubblePurple = "rgb(227, 108, 253)"
bubbleBlue = "rgb(0, 88, 251)"
bubbleLightBlue = "rgb(46, 192, 251)"
bubbleTurqouise = "rgb(20, 247, 239)"
bubbleYellow = "rgb(249, 224, 51)"
bubbleOrange = "rgb(250, 161, 67)"