module PedChartUtils where

tylenolML : Float -> Float
tylenolML lbs =
  let
    dose = (lbs2kgs lbs) * 15 * 2.5 / 80
  in
    oneDeci dose 20 floor


tylenolInfantML : Float -> Maybe Float
tylenolInfantML lbs =
  let
    dose = (lbs2kgs lbs) * 15 * 0.8 / 80
  in
    if lbs < 30
      then Just (oneDeci dose 20 ceiling)
      else Nothing


motrinML : Float -> Float
motrinML lbs =
  let
    dose = (lbs2kgs lbs) * 10 * 5 / 100
  in
    oneDeci dose 20 round


motrinInfantML : Float -> Maybe Float
motrinInfantML lbs =
  let
    dose = (lbs2kgs lbs) * 10 * 1.25 / 50
  in
    if lbs < 30
      then Just (twoDeci dose 20 ceiling)
      else Nothing


lbs2kgs : Float -> Float
lbs2kgs lbs =
  lbs * 0.453592


twoDeci : Float -> Float -> (Float -> Int) -> Float
twoDeci num max fn =
  let
    res = toFloat(fn (num * 100)) / 100
  in
    if res >= max then max else res


oneDeci : Float -> Float -> (Float -> Int) -> Float
oneDeci num max fn =
  let
    res = toFloat(fn (num * 10)) / 10
  in
    if res >= max then max else res
