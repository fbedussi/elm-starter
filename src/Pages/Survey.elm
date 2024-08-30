module Pages.Survey exposing (Model, Msg, init, update, view)

import Html exposing (Html, button, div, input, label, text)
import Html.Attributes exposing (checked, type_)
import Html.Events exposing (onClick)
import Http
import Json.Decode exposing (Decoder, int, string, succeed)
import Json.Decode.Pipeline exposing (optional, required)


type alias Model =
    { likeElm : Bool
    , name : String
    , data : CallStatus SomeData
    }


type Msg
    = SetLike Bool
    | LoadData String
    | DataLoaded (Result Http.Error SomeData)


type CallStatus a
    = Idle
    | Loading
    | Data a
    | Error String


type alias SomeData =
    { one : String
    , two : Int
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetLike likeElm ->
            ( { model | likeElm = likeElm }, Cmd.none )

        LoadData id ->
            ( { model | data = Loading }, callGetData id )

        DataLoaded (Ok data) ->
            ( { model | data = Data data }, Cmd.none )

        DataLoaded (Err error) ->
            ( { model | data = Error (getErrorMsg error) }, Cmd.none )


getErrorMsg : Http.Error -> String
getErrorMsg httpError =
    case httpError of
        Http.BadUrl url ->
            "Bad url: " ++ url

        Http.Timeout ->
            "Call timed out"

        Http.NetworkError ->
            "Network error"

        Http.BadStatus status ->
            "Server error" ++ String.fromInt status

        Http.BadBody _ ->
            "Bad body"


callGetData : String -> Cmd Msg
callGetData id =
    Http.get
        { url = "http://someserver.com/" ++ id
        , expect = Http.expectJson DataLoaded someDataDecoder
        }


someDataDecoder : Decoder SomeData
someDataDecoder =
    succeed SomeData
        |> required "one" string
        |> optional "two" int 0


view : Model -> Html Msg
view model =
    div []
        [ div [] [ text ("Hi, " ++ model.name) ]
        , label []
            [ input [ type_ "checkbox", checked model.likeElm, not model.likeElm |> SetLike |> onClick ] []
            , text "Do you like Elm?"
            ]
        , button [ onClick (LoadData "foo") ] [ text "Load some data!" ]
        , div []
            [ div []
                [ text "call result" ]
            , showData model.data
            ]
        ]


showData : CallStatus SomeData -> Html Msg
showData callResult =
    case callResult of
        Loading ->
            div [] [ text "Loading..." ]

        Data data ->
            div [] [ text ("one: " ++ data.one ++ ", two: " ++ String.fromInt data.two) ]

        Error errorMsg ->
            div [] [ text errorMsg ]

        _ ->
            div [] []


init : Bool -> String -> Model
init initialLikeElm name =
    { likeElm = initialLikeElm
    , name = name
    , data = Idle
    }
