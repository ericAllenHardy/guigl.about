module GuiglAbout exposing (
  Guigl, Msg, update, init, view
  )  

import Html exposing (Html, h1, div, button, text, img)
import Html.App as App
import Html.Events exposing (onMouseEnter, onMouseLeave)
import Html.Attributes exposing (style, title, src, alt, height, width, align)
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
maxRadius = 70
minRadius : Int
minRadius = 50

baseTransform : Transform
baseTransform = 
  Transform minRadius 0


guigl = 
  init "guigl" "/models/guigl.png" 
       "our brave protagonist" 
       updateGuigl

updateGuigl : Transform -> Time -> Transform
updateGuigl tf dt =
  let rotationRate = (*) 15 -- 0.1 is a sane amount
  in {tf | rotation = tf.rotation + rotationRate dt } --beware overflow
  

-- INIT --
init : Name -> ImagePath -> String -> (Transform -> Time -> Transform) -> Guigl
init n p d uf = 
  Guigl n p d baseTransform Inactive uf


-- UPDATE --
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


-- SUBSCRIPTIONS -- 
subscriptions : Guigl -> Sub Msg
subscriptions g = 
  diffs Tick


-- VIEW --
(=>) = (,)

px : Int -> String
px n = 
  toString n ++ "px"

view : Guigl -> Html Msg
view guigl =
  let guiglWidth = round <| 2.4 * toFloat guigl.tf.radius
      guiglHeight = 2 * guigl.tf.radius
  in 
  div [ style [ "width" => (px << round <| 1.2 * toFloat guiglWidth)
              , "position" => "relative"
              , "left" => px 5
              ] 
      ]
    [ h1 [ {-align "left"-} ] [ text guigl.name ]
    -- , div [] [ text guigl.desc ]
    , img [ src guigl.icon
          , alt guigl.name
          , title guigl.desc
          , height guiglHeight 
          , width guiglWidth 
          , style (let rot = "rotate(" ++ toString guigl.tf.rotation ++ "deg)"
                   in [ "-webkit-transform" => rot  -- Safari
                      , "-moz-transform" => rot -- Firefox 3.6 Firefox 4 */
                      , "-ms-transform" => rot -- IE9 
                      , "-o-transform" => rot -- Opera 
                      , "transform" => rot -- W3
                     -- , "position" => "relative"
                     -- , "left" => px 10
                      ])
          , align "center"
          , onMouseEnter Activate
          , onMouseLeave Deactivate
          ] []
    ]
