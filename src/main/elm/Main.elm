import Html exposing (..)
-- import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode
-- import Json.Encode as Encode

main : Program Never Model Msg
main =
  Html.program
    { init = start
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

url : String -> String
url action = "http://localhost:9000/member/" ++ action

-- MODEL

type alias Member =
  { id : Int
  , name : String
  , email : String
  }

type alias Model =
  { count : Int
  , message : String
  }

start : (Model, Cmd Msg)
start =
  ( Model 0 "No message"
  , Cmd.none
  )

-- UPDATE

type Msg
  = GetMemberCount
  | MemberCountReceived (Result Http.Error Int)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GetMemberCount ->
      (model, getMemberCount)

    MemberCountReceived (Ok newCount) ->
      ( { model | count = newCount }, Cmd.none)

    MemberCountReceived (Err error) ->
      ( { model | message = toString error }, Cmd.none)

-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ h2 [] [text ("Member Count = " ++ toString model.count) ]
    , button [ onClick GetMemberCount ] [ text "Update Member Count" ]
    , hr [] []
    , text model.message
    ]

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none

-- HTTP

getMemberCount : Cmd Msg
getMemberCount =
    Http.send MemberCountReceived (Http.get (url "count") Decode.int)

memberDecoder : Decode.Decoder Member
memberDecoder =
  Decode.map3 Member
    (Decode.field "id" Decode.int)
    (Decode.field "name" Decode.string)
    (Decode.field "email" Decode.string)
