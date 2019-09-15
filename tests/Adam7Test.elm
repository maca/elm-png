module Adam7Test exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)


import Dict

import Adam7 exposing (passDimensions)
import Matrix exposing (Dimensions)


suite : Test
suite =
  describe "adam7 functions"
    [ describe "get pass dimensions"
      [ test8x8passDimensions
      , test33x47passDimensions
      , test1x1passDimensions
      , test0x0passDimensions
      ]
    ]


test8x8passDimensions =
  let
      dimensions = { width = 8, height = 8 }
  in
  describe "pass dimensions for 8x8 image"
    [ test "returns pass 0 dimensions for 8x8 image" <|
        always <| Expect.equal { width = 1, height = 1 }
               <| passDimensions 0 dimensions

    , test "returns pass 1 dimensions for 8x8 image" <|
        always <| Expect.equal { width = 1, height = 1 }
               <| passDimensions 1 dimensions

    , test "returns pass 2 dimensions for 8x8 image" <|
        always <| Expect.equal { width = 2, height = 1 }
               <| passDimensions 2 dimensions

    , test "returns pass 3 dimensions for 8x8 image" <|
        always <| Expect.equal { width = 2, height = 2 }
               <| passDimensions 3 dimensions

    , test "returns pass 4 dimensions for 8x8 image" <|
        always <| Expect.equal { width = 4, height = 2 }
               <| passDimensions 4 dimensions

    , test "returns pass 5 dimensions for 8x8 image" <|
        always <| Expect.equal { width = 4, height = 4 }
               <| passDimensions 5 dimensions

    , test "returns pass 6 dimensions for 8x8 image" <|
        always <| Expect.equal { width = 8, height = 4 }
               <| passDimensions 6 dimensions

    , test "returns nothing for invalid pass for 8x8 image" <|
        always <| Expect.equal { width = 0, height = 0 }
               <| passDimensions 7 dimensions
    ]


test33x47passDimensions =
  let
      dimensions = { width = 33, height = 47 }
  in
  describe "pass dimensions for 33x47 image"
    [ test "returns pass 0 dimensions for 33x47 image" <|
        always <| Expect.equal { width = 5, height = 6 }
               <| passDimensions 0 dimensions

    , test "returns pass 1 dimensions for 33x47 image" <|
        always <| Expect.equal { width = 4, height = 6 }
               <| passDimensions 1 dimensions

    , test "returns pass 2 dimensions for 33x47 image" <|
        always <| Expect.equal { width = 9, height = 6 }
               <| passDimensions 2 dimensions

    , test "returns pass 3 dimensions for 33x47 image" <|
        always <| Expect.equal { width = 8, height = 12 }
               <| passDimensions 3 dimensions

    , test "returns pass 4 dimensions for 33x47 image" <|
        always <| Expect.equal { width = 17, height = 12 }
               <| passDimensions 4 dimensions

    , test "returns pass 5 dimensions for 33x47 image" <|
        always <| Expect.equal { width = 16, height = 24 }
               <| passDimensions 5 dimensions

    , test "returns pass 6 dimensions for 33x47 image" <|
        always <| Expect.equal { width = 33, height = 23 }
               <| passDimensions 6 dimensions

    , test "returns nothing for invalid pass for 33x47 image" <|
        always <| Expect.equal { width = 0, height = 0 }
               <| passDimensions 7 dimensions
    ]


test1x1passDimensions =
  let
      dimensions = { width = 1, height = 1 }
  in
  describe "pass dimensions for 1x1 image"
    [ test "returns pass 0 dimensions for 1x1 image" <|
        always <| Expect.equal { width = 1, height = 1 }
               <| passDimensions 0 dimensions

    , test "returns pass 1 dimensions for 1x1 image" <|
        always <| Expect.equal { width = 0, height = 1 }
               <| passDimensions 1 dimensions

    , test "returns pass 2 dimensions for 1x1 image" <|
        always <| Expect.equal { width = 1, height = 0 }
               <| passDimensions 2 dimensions

    , test "returns pass 3 dimensions for 1x1 image" <|
        always <| Expect.equal { width = 0, height = 1 }
               <| passDimensions 3 dimensions

    , test "returns pass 4 dimensions for 1x1 image" <|
        always <| Expect.equal { width = 1, height = 0 }
               <| passDimensions 4 dimensions

    , test "returns pass 5 dimensions for 1x1 image" <|
        always <| Expect.equal { width = 0, height = 1 }
               <| passDimensions 5 dimensions

    , test "returns pass 6 dimensions for 1x1 image" <|
        always <| Expect.equal { width = 1, height = 0 }
               <| passDimensions 6 dimensions

    , test "returns nothing for invalid pass for 1x1 image" <|
        always <| Expect.equal { width = 0, height = 0 }
               <| passDimensions 7 dimensions
    ]


test0x0passDimensions =
  let
      dimensions = { width = 0, height = 0 }
  in
  describe "pass dimensions for 0x0 image"
    [ test "returns pass 0 dimensions for 0x0 image" <|
        always <| Expect.equal { width = 0, height = 0 }
               <| passDimensions 0 dimensions

    , test "returns pass 1 dimensions for 0x0 image" <|
        always <| Expect.equal { width = 0, height = 0 }
               <| passDimensions 0 dimensions

    , test "returns pass 2 dimensions for 0x0 image" <|
        always <| Expect.equal { width = 0, height = 0 }
               <| passDimensions 2 dimensions

    , test "returns pass 3 dimensions for 0x0 image" <|
        always <| Expect.equal { width = 0, height = 0 }
               <| passDimensions 3 dimensions

    , test "returns pass 4 dimensions for 0x0 image" <|
        always <| Expect.equal { width = 0, height = 0 }
               <| passDimensions 4 dimensions

    , test "returns pass 5 dimensions for 0x0 image" <|
        always <| Expect.equal { width = 0, height = 0 }
               <| passDimensions 5 dimensions

    , test "returns pass 6 dimensions for 0x0 image" <|
        always <| Expect.equal { width = 0, height = 0 }
               <| passDimensions 6 dimensions

    , test "returns nothing for invalid pass for 0x0 image" <|
        always <| Expect.equal { width = 0, height = 0 }
               <| passDimensions 7 dimensions
    ]
