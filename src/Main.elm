module Main exposing (main)

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
  | OnKeyPress


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    OnNewBubble color coords ->
      ( { model | bubble = Debug.log "New" coords, color = color }, Cmd.none )

    OnMouseMove coords ->
      ( { model | mouse = coords }, Cmd.none )

    OnMouseClick coords ->
      ( model, newBubble )

    OnKeyPress ->
      ( model, newBubble )


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ Browser.Events.onMouseMove (D.map OnMouseMove mouseCoordsDecoder)
    , Browser.Events.onKeyDown (D.succeed OnKeyPress)
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
    [ HA.style "width" "100%"
    , HA.style "height" "100vh"
    , HA.style "position" "relative"
    , HA.style "padding" "50px"
    , HA.style "box-sizing" "border-box"
    ]
    [ H.div
        [ HA.style "width" "100%"
        , HA.style "height" "100%"
        , HA.style "position" "relative"
        ]
        [ H.h1
            [ HA.style "font-family" "monospace"
            , HA.style "position" "absolute"
            , HA.style "user-select" "none"
            ]
            [ H.text "Isak's Bubble Game!" ]
        , viewBubble model ]
    ]


viewBubble : Model -> H.Html Msg
viewBubble model =
  H.div
    [ HA.style "position" "absolute"
    , HA.style "left" (String.fromInt model.bubble.x ++ "%")
    , HA.style "top" (String.fromInt model.bubble.y ++ "%")
    , HA.style "transform" "translate(-50%, -50%)"
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


bubblePurple = "rgba(227, 108, 253, 0.9)"
bubbleBlue = "rgba(0, 88, 251, 0.9)"
bubbleLightBlue = "rgba(46, 192, 251, 0.9)"
bubbleTurqouise = "rgba(20, 247, 239, 0.9)"
bubbleYellow = "rgba(249, 224, 51, 0.9)"
bubbleOrange = "rgba(250, 161, 67, 0.9)"