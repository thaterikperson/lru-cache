module LruCache exposing (..)

{-| An Elm LRU Cache with a Dict like interface.

# Type
@docs LruCache

# Functions
@docs empty, insert, get

-}

import Dict exposing (Dict)


{-| LruCache has a key and value type. The key type
must be a `comparable` but the value type can be any type.
-}
type LruCache comparable v
    = LruCache
        { size : Int
        , contents : Dict comparable v
        , lastAccessed : Dict comparable Int
        , counter : Int
        }


{-| Initialize a new LruCache with a maximum size.
-}
empty : Int -> LruCache comparable v
empty maxSize =
    LruCache { size = maxSize, contents = Dict.empty, lastAccessed = Dict.empty, counter = 0 }


{-| Insert a key and value into the LruCache. `key` must be a `comparable`
but `v` can be of any type. Inserting an exisinting key will overwrite
the exisinting value.
-}
insert : comparable -> v -> LruCache comparable v -> LruCache comparable v
insert key value (LruCache cache) =
    let
        ( contents, accessed ) =
            if cache.size >= Dict.size cache.contents then
                let
                    key =
                        cache.lastAccessed
                            |> Dict.toList
                            |> List.sortBy Tuple.second
                            |> List.head
                            |> Maybe.andThen (\( k, _ ) -> Just k)
                in
                    case key of
                        Nothing ->
                            ( cache.contents, cache.lastAccessed )

                        Just k ->
                            ( Dict.remove k cache.contents
                            , Dict.remove k cache.lastAccessed
                            )
            else
                ( cache.contents, cache.lastAccessed )
    in
        LruCache
            { cache
                | contents = Dict.insert key value contents
                , lastAccessed = Dict.insert key cache.counter accessed
                , counter = cache.counter + 1
            }


{-| Fetch an item from the LruCache by its key.
-}
get : comparable -> LruCache comparable v -> ( Maybe v, LruCache comparable v )
get key (LruCache cache) =
    let
        value =
            Dict.get key cache.contents

        ( accessed, counter ) =
            case value of
                Nothing ->
                    ( cache.lastAccessed, cache.counter )

                _ ->
                    ( Dict.update key (\_ -> Just cache.counter) cache.lastAccessed
                    , cache.counter + 1
                    )

        updated =
            { cache
                | counter = counter
                , lastAccessed = accessed
            }
    in
        ( value, LruCache updated )
