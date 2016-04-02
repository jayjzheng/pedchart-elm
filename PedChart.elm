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
  , url : String
  , every : Int
  , max : String
  , childML: Float
  , infantML : Maybe Float
  }

type alias Model = (Med, Med)

init : Model
init =
  ( { for = "Tylenol"
    , url = "http://www.tylenol.com/children-infants/safety/dosage-charts"
    , every = 4
    , max = "4 doses in 24 hours"
    , childML = 0
    , infantML = Just 0 }
  , { for = "Motrin"
    , url = "http://www.motrin.com/children-infants/dosing-charts?icid=home|tout|1"
    , every = 6
    , max = "4 doses in 24 hours"
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
    div [ class "col-xs-12 col-md-4" ]
    [ a [ href med.url, target "_blank" ] [ h3 [] [ text med.for ] ]
    , h4 [] [ text ("Child Dosage: " ++ (toString med.childML) ++ " ml") ]
    , h4 [] [ text ("Infant Dosage: " ++ infantDose) ]
    , span [] [ text ("Given every " ++ (toString med.every) ++ " hours") ]
    , br [] []
    , span [] [ text ("Max: " ++ med.max) ]
    ]

inputView : Signal.Address Action -> Html
inputView address =
  div [ class "col-xs-12 col-sm-8" ]
  [ input
    [ class "form-control"
    , type' "text"
    , placeholder "Weight in lbs"
    , onInput address Calc ]
    []
  ]


disclaimerView : Html
disclaimerView =
  div
    [ class "col-xs-12" ]
    [ text "Disclaimer: This site is solely for reference. Always ask your doctor for the right dose."]


view : Signal.Address Action -> Model -> Html
view address model =
  let
    tylenol = fst model
    motrin = snd model
  in
  div [ class "container" ]
    [ div [ class "row", style [("padding-top", "40px"), ("padding-bottom", "10px")] ]
      [ inputView address ]

    , div [ class "row" ]
      [ medView address tylenol
      , medView address motrin
      ]

    , div [ class "row", style [("padding-top", "60px")] ]
      [ disclaimerView ]
    ]

