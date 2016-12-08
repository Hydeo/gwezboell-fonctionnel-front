-- Read more about this program in the official Elm guide:
-- https://guide.elm-lang.org/architecture/effects/http.html

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode



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
  , plate :  List (List Square)
  }

type alias Square = 
  { piece : String,
    player : String
  }

init : String -> (Model, Cmd Msg)
init topic =
  ( Model topic "waiting.gif" "lol" generatePlate -- On définit le model lors du premier chargement de la page
  , getRandomGif topic -- Et on charge tout de suite un premier gif
  )

generatePlate : List (List Square)
generatePlate = 
  [
   [ Square "Roi" "Conard",
    Square "Pion" "Conard",
    Square "Null" "Null",
    Square "Pion" "Conard",
    Square "Pion" "Conard"
    ],
    [ Square "Roi" "Conard",
    Square "Pion" "Conard",
    Square "Null" "Null",
    Square "Pion" "Conard",
    Square "Pion" "Conard"
    ]
  ]

-- UPDATE


type Msg
  = MorePlease
  | NewGif (Result Http.Error String)
  | UpdateStatus
  | NewPartyStatus (Result Http.Error String)


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    -- GIF
    MorePlease ->
      (model, getRandomGif model.topic)

    NewGif (Ok newUrl) ->
      (Model model.topic newUrl model.partyStatus model.plate, Cmd.none)

    NewGif (Err _) ->
      (model, Cmd.none)

    -- STATUS
    UpdateStatus ->
      (model, getPartyStatus "Conard" "blanc" )

    NewPartyStatus (Ok partyStatus) ->
      (Model model.topic model.gifUrl partyStatus model.plate, Cmd.none)

    NewPartyStatus (Err _) ->
      (model, Cmd.none)


-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ h2 [] [text model.topic]
    , button [ onClick MorePlease ] [ text "More Please!" ] -- Quand on clique sur le bouton, on fait appel a MorePlease qui va ré-effectuer la même procédure que NewGif
    , br [] []
    , img [src model.gifUrl] []
    , br [] []
    , button [ onClick UpdateStatus ] [ text "Get Status" ]
    , p[][text model.partyStatus]
    , table[] [(viewPlate model.plate)]
    ]


--iteratePlate List (List String) -> List String
--iteratePlate plate =

viewPlate : List (List Square) -> Html Msg
viewPlate plate = 
   viewPlate (List.map viewSquare List.head::plate)

iteratePlate : List(List(Html Msg)) -> Html Msg
iteratePlate itPlate = 
  iteratePlate (List.map iteratePlate List.head::itPlate)

  
viewSquare : Square -> Html Msg
viewSquare {piece , player} =
  div[]
    [
      text piece, text player
    ]




  
-- SUBSCRIPTIONS --------------------


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none



-- HTTP ----------------------------

getPartyStatus : String -> String -> Cmd Msg
getPartyStatus playerName team = 
  let 
    --url = "http://localhost:64385/"++playerName++"/"++team++"/"
    url = "http://demo1416923.mockable.io/fonctionnelOP"
  in
    Http.send NewPartyStatus (Http.get url decodePartyStatus)

decodePartyStatus : Decode.Decoder String  -- Decode Json qui permet de récupèrer un élement (image_url dans data) dans du JSON
decodePartyStatus =
  Decode.at ["data", "Plate"] Decode.string


getRandomGif : String -> Cmd Msg 
getRandomGif topic =
  let
    url =
      "https://api.giphy.com/v1/gifs/random?api_key=dc6zaTOxFJmzC&tag=" ++ topic --On récupère un gif random avec comme paramètre le topic (par défaut "chat") 
  in
    Http.send NewGif (Http.get url decodeGifUrl) -- Et la on fait appel a la cmd qui va effectuer pour de vrai la requette HTTP


decodeGifUrl : Decode.Decoder String  -- Decode Json qui permet de récupèrer un élement (image_url dans data) dans du JSON
decodeGifUrl =
  Decode.at ["data", "image_url"] Decode.string