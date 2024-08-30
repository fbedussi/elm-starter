port module Main exposing (Route(..), main, viewHeader)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (classList, href)
import Pages.Counter
import Pages.Survey
import Url exposing (Url)
import Url.Parser exposing ((</>))


type alias Model =
    { navigationKey : Nav.Key
    , route : Maybe Route
    , page : Page
    , userName : String
    }


type Route
    = CounterRoute
    | SurveyRoute String


type Page
    = CounterPage Pages.Counter.Model
    | SurveyPage Pages.Survey.Model
    | NotFoundPage


parseUrl : Url -> Maybe Route
parseUrl =
    Url.Parser.oneOf
        [ Url.Parser.map CounterRoute Url.Parser.top
        , Url.Parser.map SurveyRoute (Url.Parser.s "survey" </> Url.Parser.string)
        ]
        |> Url.Parser.parse


view : Model -> Browser.Document Msg
view model =
    let
        content =
            case model.page of
                CounterPage counterModel ->
                    Pages.Counter.view counterModel
                        |> Html.map GotCounterMsg

                SurveyPage surveyModel ->
                    Pages.Survey.view surveyModel
                        |> Html.map GotSurveyMsg

                NotFoundPage ->
                    text "Sorry, I did't find this page"
    in
    { title = getTitle model.route
    , body =
        [ viewHeader model.route model.userName
        , main_ []
            [ content ]
        , footer []
            [ text "Elm SPA boilerplate"
            ]
        ]
    }


getTitle : Maybe Route -> String
getTitle route =
    case route of
        Just CounterRoute ->
            "Elm SPA boilerplate - Counter"

        Just (SurveyRoute _) ->
            "Elm SPA boilerplate - Survay"

        _ ->
            "Elm SPA boilerplate"


viewHeader : Maybe Route -> String -> Html msg
viewHeader activeRoute userName =
    let
        isActive : Maybe Route -> Route -> Bool
        isActive routeA routeB =
            case ( routeA, routeB ) of
                ( Just CounterRoute, CounterRoute ) ->
                    True

                ( Just (SurveyRoute _), SurveyRoute _ ) ->
                    True

                ( _, _ ) ->
                    False
    in
    header []
        [ nav []
            [ ul []
                [ navLink (isActive activeRoute CounterRoute) { url = "/", label = "Counter" }
                , navLink (isActive activeRoute (SurveyRoute "")) { url = "/survey/" ++ userName, label = "Survey " ++ userName }
                ]
            ]
        ]


navLink : Bool -> { a | url : String, label : String } -> Html msg
navLink isActive { url, label } =
    a [ href url, classList [ ( "active", isActive ) ] ] [ text label ]


type Msg
    = ClickedLink Browser.UrlRequest
    | PerformUrlChange String
    | ChangedUrl Url
    | GotCounterMsg Pages.Counter.Msg
    | GotSurveyMsg Pages.Survey.Msg
    | GotUserName String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedLink (Browser.Internal url) ->
            ( model, Url.toString url |> sendUrlChangeRequest )

        ClickedLink (Browser.External string) ->
            ( model, Nav.load string )

        PerformUrlChange urlString ->
            ( model, Nav.pushUrl model.navigationKey urlString )

        ChangedUrl url ->
            let
                { route, page, cmd } =
                    processUrl 0 url
            in
            ( { model | route = route, page = page }
            , cmd
            )

        GotCounterMsg counterMsg ->
            case model.page of
                CounterPage oldCounterModel ->
                    let
                        ( newCounterModel, counterCmd ) =
                            Pages.Counter.update counterMsg oldCounterModel
                    in
                    ( { model | page = CounterPage newCounterModel }, Cmd.map GotCounterMsg counterCmd )

                _ ->
                    ( model, Cmd.none )

        GotSurveyMsg surveyMsg ->
            case model.page of
                SurveyPage oldSurveyModel ->
                    let
                        ( newSurveyModel, surveyCmd ) =
                            Pages.Survey.update surveyMsg oldSurveyModel
                    in
                    ( { model | page = SurveyPage newSurveyModel }, Cmd.map GotSurveyMsg surveyCmd )

                _ ->
                    ( model, Cmd.none )

        GotUserName userName ->
            ( { model | userName = userName }, Cmd.none )


port sendUrlChangeRequest : String -> Cmd msg


port performUrlChange : (String -> msg) -> Sub msg


port getUserName : (String -> msg) -> Sub msg


type alias Flags =
    Int


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = ClickedLink
        , onUrlChange = ChangedUrl
        }


init : Flags -> Url -> Nav.Key -> ( Model, Cmd Msg )
init initialCounter url navigationKey =
    let
        { route, page, userName, cmd } =
            processUrl initialCounter url
    in
    ( { navigationKey = navigationKey
      , route = route
      , page = page
      , userName = userName
      }
    , cmd
    )


processUrl : Int -> Url -> { route : Maybe Route, page : Page, userName : String, cmd : Cmd msg }
processUrl initialCounter url =
    let
        route =
            parseUrl url

        defaultName =
            "no-name"

        ( page, cmd, userName ) =
            case route of
                Just CounterRoute ->
                    let
                        ( model, counterCmd ) =
                            Pages.Counter.init initialCounter
                    in
                    ( CounterPage model, counterCmd, defaultName )

                Just (SurveyRoute name) ->
                    ( SurveyPage (Pages.Survey.init False name), Cmd.none, name )

                Nothing ->
                    ( NotFoundPage, Cmd.none, defaultName )
    in
    { route = route, page = page, userName = userName, cmd = cmd }


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        baseSubscriptions =
            [ getUserName GotUserName
            , performUrlChange PerformUrlChange
            ]
    in
    case model.page of
        CounterPage counterModel ->
            Sub.batch
                (baseSubscriptions ++ [ Pages.Counter.subscriptions counterModel |> Sub.map GotCounterMsg ])

        _ ->
            Sub.batch baseSubscriptions
