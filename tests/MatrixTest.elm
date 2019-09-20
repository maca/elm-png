module MatrixTest exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)

import Array

import Matrix exposing (Dimensions, positionFromIndex)



suite : Test
suite =
  describe "matrix"
    [ describeGet
    , describeSet
    , describeIndexToPositionFor3x3Matrix
    , describeFromArray
    , describeFoldLines
    ]


describeGet =
  let
      matrix =
        Array.initialize 64 identity
          |> Matrix.fromArray { width = 8, height = 8 }
  in
  describe "get value for a 8x8 matrix"
    [ test "get at (0, 0)" <| \_ ->
      Matrix.get ( 0, 0 ) matrix |> Expect.equal (Just 0)

    , test "get at (0, 1)" <| \_ ->
      Matrix.get ( 0, 1 ) matrix |> Expect.equal (Just 8)

    , test "get at (1, 1)" <| \_ ->
      Matrix.get ( 1, 1 ) matrix |> Expect.equal (Just 9)

    , test "get at (7, 7)" <| \_ ->
      Matrix.get ( 7, 7 ) matrix |> Expect.equal (Just 63)
    ]


describeSet =
  let
      matrix =
        Array.repeat 64 0
          |> Matrix.fromArray { width = 8, height = 8 }

      expectation position updated =
        Matrix.get position updated |> Expect.equal (Just 1)
  in
  describe "set value for a 8x8 matrix"
    [ test "get at (0, 0)" <| \_ ->
      let
          position = ( 0, 0)
      in
      expectation position <| Matrix.set position 1 matrix

    , test "get at (0, 1)" <| \_ ->
      let
          position = ( 0, 1)
      in
      expectation position <| Matrix.set position 1 matrix

    , test "get at (1, 1)" <| \_ ->
      let
          position = ( 1, 1)
      in
      expectation position <| Matrix.set position 1 matrix

    , test "get at (7, 7)" <| \_ ->
      let
          position = ( 7, 7)
      in
      expectation position <| Matrix.set position 1 matrix
    ]


describeIndexToPositionFor3x3Matrix =
  let
      dimensions = { width = 3, height = 3 }
  in
      describe "get position from index for 3x3 matrix"
        [ test "get position for 0" <| \_ ->
          positionFromIndex dimensions 0 |> Expect.equal ( 0, 0 )

        , test "get position for 1" <| \_ ->
          positionFromIndex dimensions 1 |> Expect.equal ( 1, 0 )

        , test "get position for 5" <| \_ ->
          positionFromIndex dimensions 5 |> Expect.equal ( 2, 1 )

        , test "get position for 8" <| \_ ->
          positionFromIndex dimensions 8 |> Expect.equal ( 2, 2 )
        ]


describeIndexToPositionFor3x5Matrix =
  let
      dimensions = { width = 3, height = 5 }
  in
      describe "get position from index for 3x5 matrix"
        [ test "get position for 0" <| \_ ->
          positionFromIndex dimensions 0 |> Expect.equal ( 0, 0 )

        , test "get position for 7" <| \_ ->
          positionFromIndex dimensions 7 |> Expect.equal ( 1, 2 )

        , test "get position for 11" <| \_ ->
          positionFromIndex dimensions 11 |> Expect.equal ( 2, 3 )

        , test "get position for 14" <| \_ ->
          positionFromIndex dimensions 14 |> Expect.equal ( 2, 4 )
        ]


describeFromArray =
  let
      array9 = Array.repeat 9 Nothing
  in
  describe "initialize matrix from array"
    [ test "initializes when array has the correct length" <| \_ ->
        Matrix.fromArray { width = 3, height = 3 } array9
          |> Matrix.toArray
          |> Expect.equal array9

    , test "truncates when array exceeds length" <| \_ ->
        Array.repeat 10 Nothing
          |> Matrix.fromArray { width = 3, height = 3 }
          |> Matrix.toArray
          |> Expect.equal array9
    ]


describeFoldLines =
  let
      matrix =
        Array.initialize 9 identity
          |> Matrix.fromArray { width = 3, height = 3 }
  in
  describe "fold over matrix lines"
    [ test "folds lines over to a list" <| \_ ->
        Matrix.foldLines Array.toList matrix
          |> Expect.equal [[ 0, 1, 2 ], [ 3, 4, 5 ], [ 6, 7, 8 ]]
    ]
