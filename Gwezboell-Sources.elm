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
  , plate :  List (List String)
  , joueur1 : Player
  , joueur2 : Player
  , winner : Player
  , move : Move
  }

type alias Move = 
  { startLine: Int,
    startRow : Int,
    endLine: Int,
    endRow : Int
  }

type alias Player =
  { couleur : String,
    pseudo : String,
    tour : Int
}

type alias GameStatus =
  { plate : List(List String),
    joueur1 : Player,
    joueur2 : Player,
    partieGagnee : Player
  }



init : String -> (Model, Cmd Msg)
init topic =
  ( Model topic "waiting.gif" generatePlate initEmptyPlayer initEmptyPlayer initEmptyPlayer initMove -- On définit le model lors du premier chargement de la page
  , initPlayer "Boulet" "Blanc" -- Et on charge tout de suite un premier gif
  )

generatePlate : List (List String)
generatePlate = 
  [["Null","Null","Null","PionN","Null","PionN","Null","Null","Null"],["Null","Null","Null","Null","PionN","Null","Null","Null","Null"],["Null","Null","Null","Null","PionB","Null","Null","Null","Null"],["PionN","Null","Null","Null","PionB","Null","Null","Null","PionN"],["PionN","PionN","PionB","PionB","RoiB","PionB","PionB","PionN","PionN"],["PionN","Null","Null","Null","PionB","Null","Null","Null","PionN"],["Null","Null","Null","Null","PionB","Null","Null","Null","Null"],["Null","Null","Null","Null","PionN","Null","Null","Null","Null"],["Null","Null","Null","PionN","PionN","PionN","Null","Null","Null"]]

-- UPDATE


type Msg
  = MorePlease
  | NewGif (Result Http.Error String)
  | InitPlayerMsg (Result Http.Error GameStatus)
  | PlayTurn 
  | NewPartyStatus (Result Http.Error String)
  | CaseClick Int Int




update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    -- GIF
    MorePlease ->
      (model, getRandomGif model.topic)

    NewGif (Ok newUrl) ->
      (Model model.topic newUrl model.plate  model.joueur1 model.joueur2 model.winner initMove, Cmd.none)

    NewGif (Err _) ->
      (model, Cmd.none)

    -- INIT PLAYER 
    InitPlayerMsg (Ok plateUpdate) ->
      (Model model.topic model.gifUrl plateUpdate.plate plateUpdate.joueur1 plateUpdate.joueur2 plateUpdate.partieGagnee initMove, Cmd.none)

    InitPlayerMsg (Err _) ->
      (model, Cmd.none)

    -- PLAYER Play 
    PlayTurn ->
      (model, (getPlayResult model model.move))

    NewPartyStatus (Ok partyStatus) ->
      (Model model.topic model.gifUrl  model.plate model.joueur1 model.joueur2 model.winner initMove, Cmd.none)

    NewPartyStatus (Err _) ->
      (model, Cmd.none)

    -- ONCLICK 
    CaseClick indexLine indexRow ->
      ( Model model.topic model.gifUrl model.plate model.joueur1 model.joueur2 model.winner (moveUpdate model.move indexLine indexRow), Cmd.none)


-- VIEW


view : Model -> Html Msg
view model =
  div [class "bigBrother"]
    [ h1 [] [text "Gwezboell Fonctionnel"]
    ,table[class "scores"][
      tr[][
        td[][
          h2 [] [text "Player 1"]
          ,h3 [] [text (model.joueur1.pseudo++" - "++model.joueur1.couleur++" - "++toString(model.joueur1.tour))]
        ]
        ,
        td[class "spacer"][text "= = = = VS = = = ="]
        ,td[][
          h2 [] [text "Player 2"]
          , h3 [] [text (model.joueur2.pseudo++" - "++model.joueur2.couleur++" - "++toString(model.joueur2.tour))]
        ]
      ],
      tr[][
        td[][]
        ,td[class "spacer"][text "= = = = WINNER = = = ="]
        ,td[][]
      ],
      tr[][
        td[][]
        ,td[class "spacer"][text model.winner.pseudo]
        ,td[][]
      ]
    ]
    , button [ onClick MorePlease,  class "btn btn-default"] [ text "More Please!" ] -- Quand on clique sur le bouton, on fait appel a MorePlease qui va ré-effectuer la même procédure que NewGif
    , br [] []
    , img [src model.gifUrl] []
    , br [] []
    , button [ onClick PlayTurn,  class "btn btn-default" ] [ text ("Play Move : "++toString(model.move.startLine+1)++","++toString(model.move.startRow+1)++" || "++toString(model.move.endLine+1)++","++toString(model.move.endRow+1)) ]
    , table[] (List.indexedMap viewPlate model.plate)
    ]


viewPlate : Int -> List String -> Html Msg
viewPlate indexLine plate = 
  tr[id (toString indexLine)](List.indexedMap (viewCase indexLine) plate )

    
viewCase : Int -> Int -> String -> Html Msg
viewCase indexRow indexLine piece  =
  if ((indexRow == 0 || indexRow == 8) && (indexLine == 0 || indexLine == 8)) then
    if (piece /= "Null") then
      td[id ((toString indexRow)++(toString indexLine)), class "corner case", onClick (CaseClick indexRow indexLine)]
      [
        img [src ("img/"++piece++".png")][]
      ]
    else
      td[id ((toString indexRow)++(toString indexLine)), class "corner case", onClick (CaseClick indexRow indexLine)]
      []
  else
    if (piece /= "Null") then
      td[id ((toString indexRow)++(toString indexLine)), class "floor case", onClick (CaseClick indexRow indexLine)]
      [
        img [src ("img/"++piece++".png")][]
      ]
    else
      td[id ((toString indexRow)++(toString indexLine)), class "floor case", onClick (CaseClick indexRow indexLine)]
      []
  
-- SUBSCRIPTIONS --------------------

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

-- HTTP ---
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

initEmptyPlayer : Player
initEmptyPlayer =
 Player "Blanc" "Link" 0 

initPlayer : String -> String -> Cmd Msg
initPlayer pseudo team =
  let 
    --"http://demo1416923.mockable.io/listPlate2/"++pseudo++"/"++team
    --"http://demo1416923.mockable.io/listPlate2"
    url = 
      "http://localhost:8080/Joueur2/Noir/"
  in 
    Http.send InitPlayerMsg (Http.get url gameUpdateDecoder)


getPlayResult : Model -> Move -> Cmd Msg
getPlayResult model move = 
  let 
    --url = "http://demo1416923.mockable.io/listPlate2/"++toString(move.startLine)++":"++toString(move.startRow)++":"++toString(move.endLine)++":"++toString(move.endRow)
    --"http://demo1416923.mockable.io/listPlate3"
    url = "http://localhost:8080/"++ model.joueur2.pseudo++"/"++model.joueur2.couleur++"/"++toString(move.startLine+1)++":"++toString(move.startRow+1)++":"++toString(move.endLine+1)++":"++toString(move.endRow+1)
  in
     Http.send InitPlayerMsg (Http.get url gameUpdateDecoder)


-- Premier decoder avec seulement le plateau
plateUpdateDecoder : String -> List (List String)
plateUpdateDecoder stringPlate =
  let 
    d = Json.Decode.decodeString (field "Plateau" (Json.Decode.list (Json.Decode.list Json.Decode.string))) stringPlate
  in
    case d of
        Ok x ->  x
        Err msg -> [[""]]
--
-- New Decoder avec tous les champs --
gameUpdateDecoder : Decoder GameStatus
gameUpdateDecoder =
  Json.Decode.succeed GameStatus
    |: (field "Plateau" (Json.Decode.list(Json.Decode.list Json.Decode.string)))
    |: (field "Joueur1" playerDecode)
    |: (field "Joueur2" playerDecode)
    |: (field "PartieGagnee" playerDecode)

playerDecode : Decoder Player
playerDecode =
  Json.Decode.succeed Player
    |: (field "Couleur" Json.Decode.string)
    |: (field "Pseudo" Json.Decode.string)
    |: (field "Tour" Json.Decode.int) 


-- En cas de double-click move, update du Model-Move 
moveUpdate : Move -> Int -> Int -> Move -- Réussir a passer le move du model et à le modifier a travers cette fonction // TODO
moveUpdate move indexLine indexRow =
  if move.startLine == -1 && move.startRow == -1 then
    Move indexLine indexRow move.endLine move.endRow 
  else if move.endLine == -1 && move.endRow == -1 then
    Move move.startLine move.startRow indexLine indexRow 
  else
    Move indexLine indexRow -1 -1
