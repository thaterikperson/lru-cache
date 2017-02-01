module Tests exposing (..)

import Dict exposing (Dict)
import Expect
import LruCache exposing (LruCache(..))
import Test exposing (Test, describe, test)
import Test.Runner.Html


main : Test.Runner.Html.TestProgram
main =
    Test.Runner.Html.run cacheTests


empty10 : LruCache comparable v
empty10 =
    (LruCache.empty 10)


oneToOne : LruCache Int String
oneToOne =
    LruCache.insert 1 "1" empty10


cacheTests : Test
cacheTests =
    describe "All Tests"
        [ getTests
        , insertTests
        ]


getTests : Test
getTests =
    describe "Get Tests"
        [ test "Get Nothing from non-existent key" <|
            \() ->
                let
                    ( value, _ ) =
                        LruCache.get 1 empty10
                in
                    Expect.equal Nothing value
        ]


insertTests : Test
insertTests =
    describe "Insert Tests"
        [ test "Insert into empty has one object" <|
            \() ->
                let
                    contents =
                        case oneToOne of
                            LruCache c ->
                                c.contents
                in
                    Expect.equal 1 (Dict.size contents)
        , test "Insert existing key keeps same size" <|
            \() ->
                let
                    contents =
                        case LruCache.insert 1 "2" oneToOne of
                            LruCache c ->
                                c.contents
                in
                    Expect.equal 1 (Dict.size contents)
        , test "Insert existing key uses new value" <|
            \() ->
                let
                    cache =
                        LruCache.insert 1 "2" oneToOne

                    value =
                        cache
                            |> LruCache.get 1
                            |> Tuple.first
                            |> Maybe.withDefault ""
                in
                    Expect.equal "2" value
        , test "Inserting two elements with size 1 retains 1 element" <|
            \() ->
                let
                    cache =
                        (LruCache.empty 1)
                            |> LruCache.insert 1 "1"
                            |> LruCache.insert 2 "2"

                    contents =
                        case cache of
                            LruCache c ->
                                c.contents
                in
                    Expect.equal 1 (Dict.size contents)
        ]
