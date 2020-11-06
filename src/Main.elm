module Main exposing (..)

-- Press buttons to increment and decrement a counter.
--
-- Read how it works:
--     https://guide.elm-lang.org/architecture/buttons.html
--
--
-- If Elm's syntax looks weird to you, read this first:
--     https://guide.elm-lang.org/core_language.html
-- and here's a reference that might be handy:
--     https://elm-lang.org/docs/syntax
--

import Browser
import Html exposing (Html, br, button, div, text)
import Html.Events exposing (onClick)



-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Item =
    { prevVal : Int
    , operation : String
    , val : Int
    }


type alias Model =
    List Item


init : Model
init =
    [ { prevVal = 0
      , operation = "start"
      , val = 0
      }
    ]



-- UPDATE


type Msg
    = Increment
    | Decrement
    | Square
    | Undo


update : Msg -> Model -> Model
update msg model =
    let
        last_val =
            Maybe.withDefault { prevVal = 0, operation = "start", val = 0 } (List.head model)
    in
    case msg of
        Increment ->
            { prevVal = last_val.val
            , operation = "+ 1 ="
            , val = last_val.val + 1
            }
                :: model

        Decrement ->
            { prevVal = last_val.val
            , operation = "- 1 ="
            , val = last_val.val - 1
            }
                :: model

        Square ->
            { prevVal = last_val.val
            , operation = "^2 ="
            , val = last_val.val ^ 2
            }
                :: model

        Undo ->
            if List.length model > 1 then
                List.drop 1 model

            else
                model



-- VIEW


print_operation : Item -> Html msg
print_operation item =
    text (String.fromInt item.prevVal ++ " " ++ item.operation ++ " " ++ String.fromInt item.val)


view : Model -> Html Msg
view model =
    let
        printed_model =
            List.intersperse (br [] [])
                (List.map print_operation model)
    in
    let
        currentItem =
            Maybe.withDefault { prevVal = 0, operation = "start", val = 0 } (List.head model)
    in
    div []
        [ button [ onClick Decrement ] [ text "-" ]
        , div []
            [ text
                (String.fromInt currentItem.val)
            ]
        , button [ onClick Increment ] [ text "+" ]
        , button [ onClick Square ] [ text "**" ]
        , button [ onClick Undo ] [ text "undo" ]
        , div [] [ br [] [] ]
        , div [] printed_model
        ]
