import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)


main =
  Html.beginnerProgram { model = model, view = view, update = update }


-- MODEL

type alias Model =
  { name : String
  , password : String
  , passwordAgain : String
  }

model : Model
model =
  Model "" "" ""

type Couleur = Blanc | Noir |Incolore

type alias Player = { pseud : String, team : Couleur}

one : Player
one = 
  {pseud = "Ken" , team = Blanc}

two : Player
two = 
  {pseud = "Ryu" , team = Noir}

playerList : List Player
playerList = 
  [one,two]


getColorPlayer : Couleur -> List Player -> List Player
getColorPlayer couleur playerList = 
  case couleur of 
    Blanc ->
      List.filter (\playerList -> playerList.team == Blanc) playerList

    Noir ->
      List.filter (\playerList -> playerList.team == Noir) playerList

    Incolore ->
      List.filter (\playerList -> playerList.team == Incolore) playerList

getPlayercolor : List Player -> String
getPlayercolor playerList = 
  case playerList of 
    [] -> "Empty"
    head :: rest ->
      head.pseud ++ (getPlayercolor rest)








-- UPDATE

type Msg
    = Name String
    | Password String
    | PasswordAgain String

update : Msg -> Model -> Model
update msg model =
  case msg of
    Name name ->
      { model | name = name }

    Password password ->
      { model | password = password }

    PasswordAgain password ->
      { model | passwordAgain = password }


-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ input [ type_ "text", placeholder "Name", onInput Name ] []
    , input [ type_ "password", placeholder "Password", onInput Password ] []
    , input [ type_ "password", placeholder "Re-enter Password", onInput PasswordAgain ] []
    , viewValidation model
    , div [ style [] ] [ text (getPlayercolor playerList) ]
    ]

viewValidation : Model -> Html msg
viewValidation model =
  let
    (color, message) =
      if model.password == model.passwordAgain then
        ("green", "OK")
      else
        ("red", "Passwords do not match!")
  in
    div [ style [("color", color)] ] [ text message ]