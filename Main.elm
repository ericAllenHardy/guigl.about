import Html exposing (div, button, text, img)
import Html.App as Html
import Html.Events exposing (onMouseDown, onMouseUp)
import Html.Attributes exposing (src)
import List exposing (foldl, reverse)

main = 
    App.Program 
    { init = init 
    , view = view
    , update = update
    , substriptions = subscriptions
    }
 
type alias Name = String
type alias ImagePath = String

type alias Angle = Float
type alias Posn = 
  { x : Float, y : Float }

type alias Model = 
  { guigls : List Guigl
  }

type alias GuiglRender = 
  { angle : Angle
  , radius : Float
  , posn : Posn
  , guigl : Guigl
  }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
  case msg of
    Grow i -> 
      (map (growGuigl i) (guigls model), Cmd.none)
    Shrink i -> 
      (map (shrinkGuigl i) (guigls model), Cmd.none)

baseRadius : Float
baseRadius = 30.0

unfold : (a -> b -> (c, b)) -> b -> List a -> List c 
unfold mut seed xs = go mut seed xs []
  where go _ [] ys = reverse ys
        go seed (x :: xs) ys = 
          let (y, seed') = mut x seed
          in go seet xs y :: ys
            
{--
init : List {Name ImagePath} -> List Guigl
init icons = unfold placeGuigls initPosn
  where initPosn = Posn 0 0
        placeGuigls {name, icon} {x, y} = 
          let 
--}
  

renderGuigl : Guigl -> Int -> Html Msg
renderGuigl guigl i = 
  img [ src ( icon guigl )
      -- , attribute "data-rotate" ( angle guigl )
      , hieght ( 2 * radius guigl )
      , width ( 2 * radius guigl )
      ]   
    [ onMouseDown (Grow i) 
    , onMouseUp (Shrink i)
    ]


