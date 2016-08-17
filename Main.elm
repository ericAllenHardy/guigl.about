import Html exposing (Html, div, button, text, img)
import Html.App as App
import Html.Events exposing (onMouseDown, onMouseUp)
import Html.Attributes exposing (src, style)
import Array exposing (Array, fromList, get, set, indexedMap)
import Array 
import Time exposing (Time)

import Guigl
import Guigl exposing (guigl, Guigl, Transform, Name, ImagePath)

main = 
    App.program 
    { init = (init, Cmd.none) 
    , view = view
    , update = update
    , subscriptions = subscriptions
    }
 

-- TYPES --
type alias Model = 
  { guigls : Array Guigl
  }
 
type Msg 
  = Alert Int Guigl.Msg
  | Broadcast Guigl.Msg


-- CONSTANTS --
baseRadius : Float
baseRadius = 30.0

ubaldino : Guigl
ubaldino =
  Guigl.init "ubaldino" "/models/ubaldino.png" 
             "mortal enemy and teammate"
             updateUbaldino

walusneaki : Guigl
walusneaki = 
  Guigl.init "walusneaki" "/models/walusneaki.png"
             "the edgy one"
             updateWalusneaki

blooch : Guigl
blooch =
  Guigl.init "blooch" "/models/blooch.png"
             "the token love interest"
             updateBlooch

derburg : Guigl
derburg =
  Guigl.init "derburg" "/models/derburg.png"
             "???? a new challenger approaches"
             updateDerburg


updateUbaldino : Transform -> Time -> Transform
updateUbaldino tf dt =
  let frequency = 10
      magnitude = 30
      timeUp = tf.timeUp + frequency * dt
  in { tf 
     | rotation = magnitude * sin timeUp 
     , timeUp = timeUp
     }


updateWalusneaki : Transform -> Time -> Transform
updateWalusneaki tf dt =
  let frequency = 0.02
      magnitude = 20
      timeUp = tf.timeUp + frequency * dt
  in { tf 
     | xOffset = round <| (magnitude * cos timeUp) / (1 + (sin timeUp)^2)
     , yOffset = round <| (magnitude * sin timeUp * cos timeUp) 
                          / (1 + (sin timeUp)^2) 
     , timeUp = timeUp
     }


updateBlooch : Transform -> Time -> Transform
updateBlooch tf dt =
  tf

updateDerburg : Transform -> Time -> Transform
updateDerburg tf dt = 
  tf

-- INIT --
init : Model 
init = 
  Model <| fromList [ guigl
                    , ubaldino
                    , walusneaki
                    , blooch
                    , derburg
                    ]

-- UPDATE --
arrUpdate : (a -> a) -> Int -> Array a -> Array a
arrUpdate f i arr =
  case get i arr of
    Nothing -> 
      arr 
    Just x -> 
      set i (f x) arr


update : Msg -> Model -> (Model, Cmd Msg)
update msg {guigls} = 
  case msg of
    Alert i gMsg -> 
      (Model << arrUpdate (Guigl.update gMsg) i <| guigls, Cmd.none)
    Broadcast gMsg ->
      (Model << Array.map (Guigl.update gMsg) <| guigls, Cmd.none)

-- SUBSCRIPTIONS --
subscriptions : a -> Sub Msg 
subscriptions =
  always << Sub.map Broadcast <| Guigl.subscriptions ()  

-- VIEW --
view : Model -> Html Msg
view {guigls} = 
  div [ style [ "background-image" => "url(/models/art1.jpg)"
              , "height" => "100%"
              ] 
      ]
    <| Array.foldr (::) [] (indexedMap viewGuigl guigls)

(=>) = (,)

viewGuigl : Int -> Guigl -> Html Msg
viewGuigl i guigl =
  div [ style [ "display" => "inline-block" ]
      ] 
    [ App.map (Alert i) <| Guigl.view guigl ]
