-- Read more about this program in the official Elm guide:
-- https://guide.elm-lang.org/architecture/effects/http.html

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode exposing (..)
import Json.Decode.Extra exposing ((|:))
main =
  Html.program
    { init = init "cats"
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

-- MODEL

type alias Model =
  { topic : String
  , gifUrl : String
  , partyStatus : String
  , plate :  List (List Case)
  , move : Move
  , tmpLigne : List (List String)
  }

type alias Case = 
  { piece : String,
    player : String
  }

type alias Ligne = 
  { l0 : String,
    l1 : String,
    l2 : String,
    l3 : String,
    l4 : String,
    l5 : String,
    l6 : String,
    l7 : String,
    l8 : String
  }

type alias Col = 
  { c0 : Ligne,
    c1 : Ligne
  }

type alias Move = 
  { startLine: Int,
    startRow : Int,
    endLine: Int,
    endRow : Int
  }

init : String -> (Model, Cmd Msg)
init topic =
  ( Model topic "waiting.gif" "lol" generatePlate initMove [[""]]-- On définit le model lors du premier chargement de la page
  , initPlayer "Boulet" "Blanc" -- Et on charge tout de suite un premier gif
  )

generatePlate : List (List Case)
generatePlate = 
  [
   [ Case "Null" "Conard",
    Case "Null" "Conard",
    Case "Null" "Null",
    Case "PionN" "Conard",
    Case "PionN" "Conard",
    Case "PionN" "Conard",
    Case "Null" "Conard",
    Case "Null" "Conard",
    Case "Null" "Conard"
    ],
    [ Case "Null" "Conard",
    Case "Null" "Conard",
    Case "Null" "Null",
    Case "Null" "Conard",
    Case "PionN" "Conard",
    Case "Null" "Conard",
    Case "Null" "Conard",
    Case "Null" "Conard",
    Case "Null" "Conard"
    ],
    [ Case "Null" "Conard",
    Case "Null" "Conard",
    Case "Null" "Null",
    Case "Null" "Conard",
    Case "PionB" "Conard",
    Case "Null" "Conard",
    Case "Null" "Conard",
    Case "Null" "Conard",
    Case "Null" "Conard"
    ],
    [ Case "PionN" "Conard",
    Case "Null" "Conard",
    Case "Null" "Null",
    Case "Null" "Conard",
    Case "PionB" "Conard",
    Case "Null" "Conard",
    Case "Null" "Conard",
    Case "Null" "Conard",
    Case "PionN" "Conard"
    ],
    [ Case "PionN" "Conard",
    Case "PionN" "Conard",
    Case "PionB" "Null",
    Case "PionB" "Conard",
    Case "RoiB" "Conard",
    Case "PionB" "Conard",
    Case "PionB" "Conard",
    Case "PionN" "Conard",
    Case "PionN" "Conard"
    ],
    [ Case "PionN" "Conard",
    Case "Null" "Conard",
    Case "Null" "Null",
    Case "Null" "Conard",
    Case "PionB" "Conard",
    Case "Null" "Conard",
    Case "Null" "Conard",
    Case "Null" "Conard",
    Case "PionN" "Conard"
    ],
    [ Case "Null" "Conard",
    Case "Null" "Conard",
    Case "Null" "Null",
    Case "Null" "Conard",
    Case "PionB" "Conard",
    Case "Null" "Conard",
    Case "Null" "Conard",
    Case "Null" "Conard",
    Case "Null" "Conard"
    ],
    [ Case "Null" "Conard",
    Case "Null" "Conard",
    Case "Null" "Null",
    Case "Null" "Conard",
    Case "PionN" "Conard",
    Case "Null" "Conard",
    Case "Null" "Conard",
    Case "Null" "Conard",
    Case "Null" "Conard"
    ],
    [ Case "Null" "Conard",
    Case "Null" "Conard",
    Case "Null" "Null",
    Case "PionN" "Conard",
    Case "PionN" "Conard",
    Case "PionN" "Conard",
    Case "Null" "Conard",
    Case "Null" "Conard",
    Case "Null" "Conard"
    ]
  ]

-- UPDATE


type Msg
  = MorePlease
  | NewGif (Result Http.Error String)
  | InitPlayerMsg (Result Http.Error String)
  | UpdateStatus
  | NewPartyStatus (Result Http.Error String)
  | CaseClick Int Int



update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    -- GIF
    MorePlease ->
      (model, getRandomGif model.topic)

    NewGif (Ok newUrl) ->
      (Model model.topic newUrl model.partyStatus model.plate initMove  [[""]], Cmd.none)

    NewGif (Err _) ->
      (model, Cmd.none)

    -- INIT PLAYER 
    InitPlayerMsg (Ok plateUpdate) ->
      (Model model.topic model.gifUrl model.partyStatus model.plate initMove (plateUpdateDecoder plateUpdate), Cmd.none)

    InitPlayerMsg (Err _) ->
      (model, Cmd.none)

    -- STATUS PARTY
    UpdateStatus ->
      (model, getPartyStatus "Conard" "blanc" )

    NewPartyStatus (Ok partyStatus) ->
      (Model model.topic model.gifUrl partyStatus model.plate initMove  [[""]], Cmd.none)

    NewPartyStatus (Err _) ->
      (model, Cmd.none)

    -- ONCLICK 
    CaseClick indexLine indexRow ->
      ( Model model.topic model.gifUrl model.partyStatus model.plate (moveUpdate model.move indexLine indexRow)  [[""]], Cmd.none)


-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ h2 [] [text model.topic]
    , button [ onClick MorePlease,  class "btn btn-default"] [ text "More Please!" ] -- Quand on clique sur le bouton, on fait appel a MorePlease qui va ré-effectuer la même procédure que NewGif
    , br [] []
    , img [src model.gifUrl] []
    , br [] []
    , button [ onClick UpdateStatus,  class "btn btn-default" ] [ text "Get Status" ]
    , p[][text model.partyStatus]
    , table[] (List.indexedMap viewPlate model.plate)
    ]


viewPlate : Int -> List Case -> Html Msg
viewPlate indexLine plate = 
  tr[id (toString indexLine)](List.indexedMap (viewCase indexLine) plate )

    
viewCase : Int -> Int -> Case -> Html Msg
viewCase indexRow indexLine {piece , player} =
  if ((indexRow == 0 || indexRow == 8) && (indexLine == 0 || indexLine == 8)) then
    if (piece /= "Null") then
      td[id ((toString indexRow)++(toString indexLine)), class "corner", onClick (CaseClick indexRow indexLine)]
      [
        img [src ("img/"++piece++".png")][]
      ]
    else
      td[id ((toString indexRow)++(toString indexLine)), class "corner", onClick (CaseClick indexRow indexLine)]
      []
  else
    if (piece /= "Null") then
      td[id ((toString indexRow)++(toString indexLine)), class "floor", onClick (CaseClick indexRow indexLine)]
      [
        img [src ("img/"++piece++".png")][]
      ]
    else
      td[id ((toString indexRow)++(toString indexLine)), class "floor", onClick (CaseClick indexRow indexLine)]
      []
  
-- SUBSCRIPTIONS --------------------

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

-- HTTP ----------------------------

-- Récupération du Statu de la party
getPartyStatus : String -> String -> Cmd Msg
getPartyStatus playerName team = 
  let 
    --url = "http://localhost:64385/"++playerName++"/"++team++"/"
    url = "http://demo1416923.mockable.io/fonctionnelOP"
  in
    Http.send NewPartyStatus (Http.get url decodePartyStatus)

-- Decode Json qui permet de récupèrer un élement (image_url dans data) dans du JSON
decodePartyStatus : Json.Decode.Decoder String  
decodePartyStatus =
  Json.Decode.at ["partyState", "Plate"] Json.Decode.string

-- A Supprimer
getRandomGif : String -> Cmd Msg 
getRandomGif topic =
  let
    url =
      "https://api.giphy.com/v1/gifs/random?api_key=dc6zaTOxFJmzC&tag=" ++ topic --On récupère un gif random avec comme paramètre le topic (par défaut "chat") 
  in
    Http.send NewGif (Http.get url decodeGifUrl) -- Et la on fait appel a la cmd qui va effectuer pour de vrai la requette HTTP

-- A supprimer
decodeGifUrl : Json.Decode.Decoder String  -- Decode Json qui permet de récupèrer un élement (image_url dans data) dans du JSON
decodeGifUrl =
  Json.Decode.at ["data", "image_url"] Json.Decode.string

-- Initialisation du double-click move
initMove : Move
initMove =
 Move -1 -1 -1 -1

initPlayer : String -> String -> Cmd Msg
initPlayer pseudo team =
  let 
    url = 
      "http://demo1416923.mockable.io/listPlate"
  in 
    Http.send InitPlayerMsg (Http.getString url)

caseDecoder : Json.Decode.Decoder (List(String))
caseDecoder = 
  Json.Decode.at ["plate"] (Json.Decode.list Json.Decode.string)

plateUpdateDecoder : String -> List (List String)
plateUpdateDecoder stringPlate =
  let 
    d = Json.Decode.decodeString (Json.Decode.list (Json.Decode.list Json.Decode.string)) stringPlate
  in
    case d of
        Ok x ->  x
        Err msg -> [[""]]

-- (Col(Ligne "0" "0" "0" "0" "0" "0" "0" "0" "0")
--caseDecoder : Json.Decode.Decoder Ligne
--caseDecoder = 
--  succeed Ligne
--    |: (field "l0" string)
--    |: (field "l1" string)s
--    |: (field "l2" string)
--    |: (field "l3" string)
--    |: (field "l4" string)
--    |: (field "l5" string)
--    |: (field "l6" string)
--    |: (field "l7" string)
--    |: (field "l8" string)
--  
--colDecoder : Json.Decode.Decoder Col
--colDecoder = 
--  succeed  Col
--    |: (field "c0" caseDecoder)
--    |: (field "c1" caseDecoder)

  

-- En cas de double-click move, update du Model-Move 
moveUpdate : Move -> Int -> Int -> Move -- Réussir a passer le move du model et à le modifier a travers cette fonction // TODO
moveUpdate move indexLine indexRow =
  if move.startLine == -1 && move.startRow == -1 then
    Move indexLine indexRow move.endLine move.endRow 
  else if move.endLine == -1 && move.endRow == -1 then
    Move move.startLine move.startRow indexLine indexRow 
  else
    Move indexLine indexRow -1 -1
