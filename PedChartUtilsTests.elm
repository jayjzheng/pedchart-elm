module PedChartUtilsTests where

import PedChartUtils exposing (..)

import Graphics.Element exposing (Element)
import ElmTest exposing (..)
import List
import String


testCasesData =
  [ { input = 11.0, tChild = 2.3, tInfant = Just 0.8, mChild = 2.5, mInfant = Just 1.25 }
  , { input = 13.2, tChild = 2.8, tInfant = Just 0.9, mChild = 3.0, mInfant = Just 1.5 }
  , { input = 15.4, tChild = 3.2, tInfant = Just 1.1, mChild = 3.5, mInfant = Just 1.75 }
  , { input = 17.6, tChild = 3.8, tInfant = Just 1.2, mChild = 4.0, mInfant = Just 2.0 }
  , { input = 19.8, tChild = 4.2, tInfant = Just 1.4, mChild = 4.5, mInfant = Just 2.25 }
  , { input = 22.0, tChild = 4.6, tInfant = Just 1.5, mChild = 5.0, mInfant = Just 2.5 }
  , { input = 24.2, tChild = 5.0, tInfant = Just 1.7, mChild = 5.5, mInfant = Just 2.75 }
  , { input = 26.4, tChild = 5.5, tInfant = Just 1.8, mChild = 6.0, mInfant = Just 3.0 }
  , { input = 28.6, tChild = 6.0, tInfant = Just 2.0, mChild = 6.5, mInfant = Just 3.25 }
  , { input = 30.8, tChild = 6.5, tInfant = Nothing, mChild = 7.0, mInfant = Nothing }
  , { input = 33.0, tChild = 7.0, tInfant = Nothing, mChild = 7.5, mInfant = Nothing }
  , { input = 35.2, tChild = 7.5, tInfant = Nothing, mChild = 8.0, mInfant = Nothing }
  , { input = 37.4, tChild = 8.0, tInfant = Nothing, mChild = 8.5, mInfant = Nothing }
  , { input = 39.7, tChild = 8.5, tInfant = Nothing, mChild = 9.0, mInfant = Nothing }
  , { input = 41.9, tChild = 9.0, tInfant = Nothing, mChild = 9.5, mInfant = Nothing }
  ]

makeTest fn (desc, input, exp) =
  test desc ( assertEqual exp (fn input) )


testDesc title lbs =
  title ++ " " ++ (toString lbs) ++ " lbs"


tylenoChildTests =
  let
    testcases =
      List.map (\t -> (testDesc "tylenol child" t.input, t.input, t.tChild) ) testCasesData
  in
    List.map (makeTest tylenolML) testcases


tylenoInfantTests =
  let
    testcases =
      List.map (\t -> (testDesc "tylenol infant" t.input, t.input, t.tInfant) ) testCasesData
  in
    List.map (makeTest tylenolInfantML) testcases


motrinChildTests =
  let
    testcases =
      List.map (\t -> (testDesc "motrin child" t.input, t.input, t.mChild) ) testCasesData
  in
    List.map (makeTest motrinML) testcases


motrinInfantTests =
  let
    testcases =
      List.map (\t -> (testDesc "motrin child" t.input, t.input, t.mInfant) ) testCasesData
  in
    List.map (makeTest motrinInfantML) testcases


tests =
  suite "PedChartUtils Tests"
    ( tylenoInfantTests
    ++ tylenoChildTests
    ++ motrinInfantTests
    ++ motrinChildTests
    )


main : Element
main =
  elementRunner tests
