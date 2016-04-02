module PedChart where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, on, targetValue)
import Html.Events.Extra exposing (onInput)

import String
import PedChartUtils exposing (..)

-- MODEL

type alias Med =
  { for : String
  , every : Int
  , max : String
  , childML: Float
  , infantML : Maybe Float
  }

type alias Model = (Med, Med)

init : Model
init =
  ( { for = "Tylenol(Acetominophen)"
    , every = 4
    , max = "4 doses in 24 hours"
    , childML = 0
    , infantML = Just 0 }
  , { for = "Motrin, Advil, Ibuprofen"
    , every = 6
    , max = "40 mg/kg in 24 hours"
    , childML = 0
    , infantML = Just 0 } )


-- UPDATE

type Action = NoOp | Calc String

update : Action -> Model -> Model
update action model =
  case action of
    NoOp ->
      model

    Calc strWeight ->
      let
        lbs = String.toFloat strWeight
                |> Result.toMaybe
                |> Maybe.withDefault 0

        tylenol = fst model
        motrin = snd model
      in
        ( { tylenol | childML = tylenolML lbs, infantML = tylenolInfantML lbs }
        , { motrin | childML = motrinML lbs, infantML = motrinInfantML lbs }
        )

-- VIEW

medView : Signal.Address Action -> Med -> Html
medView address med =
  let
    infantDose =
      case med.infantML of
        Just value ->
          (toString value) ++ " ml"

        Nothing ->
          "N/A"

  in
    div [ ]
    [ h2 [] [ text med.for ]
    , span [] [ text ("Infant Dosage: " ++ infantDose) ]
    , br [] []
    , span [] [ text ("Dosage: " ++ (toString med.childML) ++ " ml") ]
    , br [] []
    , br [] []
    , span [] [ text ("Given every " ++ (toString med.every) ++ " hours") ]
    , br [] []
    , span [] [ text ("Max: " ++ med.max) ]
    ]


view : Signal.Address Action -> Model -> Html
view address model =
  let
    tylenol = fst model
    motrin = snd model
  in
  div []
    [ input
      [ type' "text"
      , placeholder "weight in lbs"
      , onInput address Calc ]
      []
    , medView address tylenol
    , medView address motrin
    ]

