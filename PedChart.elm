module PedChart where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, on, targetValue)

import String

-- MODEL

type alias Med =
  { for : String
  , every : Int
  , max : String
  , childML: Float
  , infantML : Float
  }

type alias Model = (Med, Med)

init : Model
init =
  ( { for = "Tylenol(Acetominophen)"
    , every = 4
    , max = "4 doses in 24 hours"
    , childML = 0
    , infantML = 0 }
  , { for = "Motrin, Advil, Ibuprofen"
    , every = 6
    , max = "40 mg/kg in 24 hours"
    , childML = 0
    , infantML = 0 } )


-- UPDATE

type Action = NoOp | Calc String

update : Action -> Model -> Model
update action model =
  case action of
    NoOp ->
      model

    Calc strWeight ->
      let
        weight = String.toFloat strWeight
                  |> Result.toMaybe
                  |> Maybe.withDefault 0

        kgs = weight * 0.453592

        twoDecimal num =
          if toFloat(round (num * 100)) / 100 >= 20 then 20 else toFloat(round (num * 100)) / 100

        oneDecimal fn num =
          if toFloat(fn (num * 10)) / 10 >= 20 then 20 else toFloat(fn (num * 10)) / 10

        tylenol = fst model
        motrin = snd model
      in
        ( { tylenol| childML = oneDecimal floor (kgs * 15 * 2.5 / 80), infantML = oneDecimal round (kgs * 15 * 0.8 / 80) }
        , { motrin | childML = oneDecimal round (kgs * 10 * 5 / 100), infantML = twoDecimal (kgs * 10 * 1.25 / 50) }
        )

-- VIEW

medView : Signal.Address Action -> Med -> Html
medView address med =
  div [ ]
  [ h2 [] [ text med.for ]
  , span [] [ text ("Dosage: " ++ (toString med.childML) ++ " ml") ]
  , br [] []
  , span [] [ text ("Infant Dosage: " ++ (toString med.infantML) ++ " ml") ]
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
      , on "input" targetValue (Signal.message address << Calc) ]
      []
    , medView address tylenol
    , medView address motrin
    ]

