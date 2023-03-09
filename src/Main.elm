module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (Html, button, div, h1, input, text)
import Html.Attributes exposing (value)
import Html.Events exposing (onClick)
import Maybe.Extra
import Random
import Task
import Time
import Unguarded exposing (Loot, lootGeneratorForLevel)



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { seed : Int, level : Int, loot : List Loot }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { seed = 42, level = 1, loot = [] }, Task.perform NewTime Time.now )



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- UPDATE


type Msg
    = NewTime Time.Posix
    | IncreaseLevel
    | DecreaseLevel
    | Generate
    | NewLoot (List Loot)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewTime posix ->
            ( { model | seed = Time.posixToMillis posix }, Cmd.none )

        IncreaseLevel ->
            ( { model | level = min maxLevel (model.level + 1) }, Cmd.none )

        DecreaseLevel ->
            ( { model | level = max minLevel (model.level - 1) }, Cmd.none )

        Generate ->
            ( model
            , Random.generate NewLoot (lootGeneratorForLevel model.level)
            )

        NewLoot loot ->
            ( { model | loot = loot }, Cmd.none )


minLevel =
    1


maxLevel =
    8



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "BFRPG Unguarded Treasure Generator" ]
        , div []
            [ viewLevel model.level
            , button [ onClick IncreaseLevel ] [ text "+" ]
            , button [ onClick DecreaseLevel ] [ text "-" ]
            ]
        , button [ onClick Generate ] [ text "Generate" ]
        , div [] (viewLoots model.loot)
        , div [] [ text ("Total: " ++ viewTotal model.loot ++ " gp") ]
        ]


viewLevel : Int -> Html Msg
viewLevel level =
    let
        levelText =
            if level == 8 then
                String.fromInt level ++ "+"

            else
                String.fromInt level
    in
    text ("Level: " ++ levelText)


viewLoots : List Loot -> List (Html Msg)
viewLoots lootList =
    List.map viewLoot lootList


viewLoot : Loot -> Html Msg
viewLoot loot =
    div [] [ text loot.description ]


viewTotal : List Loot -> String
viewTotal lootList =
    lootList
        |> List.map (\loot -> loot.value)
        |> List.sum
        |> String.fromFloat
