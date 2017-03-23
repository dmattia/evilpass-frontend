-- Read more about this program in the official Elm guide:
-- https://guide.elm-lang.org/architecture/effects/http.html

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode
import Json.Encode as Encode

main =
  Html.program
    { init = (model, Cmd.none)
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

type alias Model =
  { username : String
  , email : String
  , password : String
  , result : String
  , loading : Bool
  , errorStatus : Status
  }

type Status
  = Success
  | Neutral
  | Error

model : Model
model =
  Model "" "" "" "No response yet" False Neutral

type Msg
  = ReceiveResponse (Result Http.Error String)
  | AttemptLogin
  | UsernameUpdated String
  | EmailUpdated String
  | PasswordUpdated String

-- Model Updaters
updateUsername : String -> Model -> Model
updateUsername newValue model =
  { model | username = newValue }

updateEmail : String -> Model -> Model
updateEmail newValue model =
  { model | email = newValue }

updatePassword : String -> Model -> Model
updatePassword newValue model =
  { model | password = newValue }

startLoading : Model -> Model
startLoading model =
  { model | loading = True }

endLoading : Model -> Model
endLoading model =
  { model | loading = False }

markResultAsError : Model -> Model
markResultAsError model =
  { model | errorStatus = Error }

markResultAsSuccess : Model -> Model
markResultAsSuccess model =
  { model | errorStatus = Success }

setResult : String -> Model -> Model
setResult newValue model =
  { model | result = newValue }

updateModel : Msg -> Model -> Model
updateModel msg model =
  case msg of
    UsernameUpdated new ->
      model
        |> updateUsername new

    EmailUpdated new ->
      model
        |> updateEmail new
      
    PasswordUpdated new ->
      model
        |> updatePassword new

    ReceiveResponse (Ok response) ->
      case response of
        "Dat is a nice set o' credentials you got there" ->
          model
            |> setResult response
            |> markResultAsSuccess
            |> endLoading
        _ ->
          model
            |> setResult response
            |> markResultAsError
            |> endLoading

    ReceiveResponse (Err err) ->
      model
        |> setResult (toString err)
        |> markResultAsError
        |> endLoading
      
    AttemptLogin ->
      model
        |> startLoading

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  let
    newModel = updateModel msg model
  in
    case msg of
      AttemptLogin ->
        (newModel, checkCredentials (jsonify model))
  
      _ ->
        (newModel, Cmd.none)

view : Model -> Html Msg
view model =
  div [ class "container white" ]
    [ div [ class "black-text" ]
      [ h2 [ class "red-text" ] [ text "Register an Account" ]
      , input [ placeholder "username", onInput UsernameUpdated ] []
      , br [] []
      , input [ placeholder "email", onInput EmailUpdated ] []
      , br [] []
      , input [ placeholder "password", type_ "password", onInput PasswordUpdated ] []
      , br [] []
      , loginBtn model
      , br [] []
      , resultSpan model
      ]
    ]

resultSpan : Model -> Html Msg
resultSpan model =
  case model.errorStatus of
    Success ->
      h3 [ class "green-text" ] [ text model.result ]
    Neutral ->
      span [] [ text model.result ]
    Error ->
      h3 [ class "red-text" ] [ text model.result ]

loginBtn : Model -> Html Msg
loginBtn model =
  if model.loading then
    div [ class "preloader-wrapper small active" ]
      [ div [ class "spinner-layer spinner-green-only" ]
        [ div [ class "circle-clipper left" ]
          [ div [ class "circle" ] []
          ]
        , div [ class "gap-patch" ]
          [ div [ class "circle" ] []
          ]
        , div [ class "circle-clipper right" ]
          [ div [ class "circle" ] []
          ]
        ]
      ]
  else
    button [ class "btn btn-large", onClick AttemptLogin ] [ text "Login" ]

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none
  
jsonify : Model -> Http.Body
jsonify model =
    Http.jsonBody <| Encode.object 
      [ ("username", Encode.string model.username)
      , ("email", Encode.string model.email)
      , ("password", Encode.string model.password)
      ]
  
checkCredentials : Http.Body -> Cmd Msg
checkCredentials json =
  let
    url =
      --"http://localhost:8080/register"
      --"https://v5q9hoapkj.execute-api.us-east-1.amazonaws.com/dev/register"
      "https://pass-word.herokuapp.com/register"
  in
    Http.send ReceiveResponse (Http.post url json decoder)

decoder : Decode.Decoder String
decoder =
  Decode.at ["message"] Decode.string

