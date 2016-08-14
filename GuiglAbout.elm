module GuiglAbout exposing (
  Guigl, Msg, update, init, view
  )  

import Html exposing (Html, div, button, text, img)
import Html.App as App
import Html.Events exposing (onMouseEnter, onMouseLeave)
import Html.Attributes exposing (style, src, alt, height, width)
import Time exposing (Time)
import AnimationFrame exposing (diffs)

main = 
    App.program 
    { init = (guigl, Cmd.none)
    , view = view
    , update = update'
    , subscriptions = subscriptions
    }
 
-- TYPES --
type alias Name = String
type alias ImagePath = String

type alias Transform =
  { radius : Int
  , rotation : Float
  }

type Activeness
  = Active
  | Inactive

type alias Guigl = 
  { name : Name
  , icon : ImagePath
  , desc : String
  , tf : Transform
  , active : Activeness
  , updater : Transform -> Time -> Transform
  }  

type Msg 
  = Activate
  | Deactivate
  | Tick Time 

-- CONSTANTS --
rotationRate : Int
rotationRate = 20

maxRadius : Int 
maxRadius = 100
minRadius : Int
minRadius = 50

guigl = 
  init "guigl" "/models/guigl.png" "our protagonist" updateGuigl

updateGuigl : Transform -> Time -> Transform
updateGuigl tf dt =
  let rotationRate = (*) 15 -- 0.1
  in {tf | rotation = tf.rotation + rotationRate dt }
  
baseTransform : Transform
baseTransform = 
  Transform minRadius 0

-- FUNCTIONS --


init : Name -> ImagePath -> String -> (Transform -> Time -> Transform) -> Guigl
init n p d uf = 
  Guigl n p d baseTransform Inactive uf

update' : Msg -> Guigl -> (Guigl, Cmd Msg)
update' msg guigl =
  (update msg guigl, Cmd.none)

update : Msg -> Guigl -> Guigl
update msg guigl = 
  case msg of 
    Activate ->
      { guigl 
      | tf = (let tf = guigl.tf in { tf | radius = maxRadius })
      , active = Active
      }
    Deactivate -> 
      { guigl
      -- | tf = (let tf = guigl.tf in { tf | radius = minRadius })
      | tf = baseTransform
      , active = Inactive
      }
    Tick dt -> 
      case guigl.active of
        Inactive -> 
          guigl
        Active -> 
          { guigl
          | tf = guigl.updater guigl.tf dt
          }

subscriptions : Guigl -> Sub Msg
subscriptions g = 
  diffs Tick

(=>) = (,)

view : Guigl -> Html Msg
view guigl =
  div []
    [ div [] [ text guigl.name ]
    , img [ src guigl.icon
          , alt guigl.name
          , height (2 * guigl.tf.radius)
          , width (round (2.4 * toFloat guigl.tf.radius))
          --, rotate (guigl.rotation |> toString) ++"deg"
          , style (let rot = "rotate(" ++ toString guigl.tf.rotation ++ "deg)"
                   in [ "-webkit-transform" => rot  -- Safari
                      , "-moz-transform" => rot -- Firefox 3.6 Firefox 4 */
                      , "-ms-transform" => rot -- IE9 
                      , "-o-transform" => rot -- Opera 
                      , "transform" => rot -- W3
                      ])
          , onMouseEnter Activate
          , onMouseLeave Deactivate
          ] []
    , div [] [ text guigl.desc ]
    ]
