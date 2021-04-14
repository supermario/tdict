module TDict exposing (empty)

import TDict.Internal as Internal exposing (TDict)
import Task exposing (Task)



-- Build


{-| empty : Dict k v
-}
empty : Task () (TDict comparable v)
empty =
    Internal.newRef_


{-| singleton : comparable -> v -> Dict comparable v
-}
singleton : comparable -> v -> Task () (TDict comparable v)
singleton key value =
    empty |> Task.andThen (insert key value)


{-| insert : comparable -> v -> Dict comparable v -> Dict comparable v
-}
insert : comparable -> v -> TDict comparable v -> Task () (TDict comparable v)
insert key value tdict =
    tdict |> Internal.insert key value


{-| update : comparable -> (Maybe v -> Maybe v) -> Dict comparable v -> Dict comparable v
-}
update : comparable -> (Maybe v -> Maybe v) -> TDict comparable v -> Task () (TDict comparable v)
update key alter tdict =
    tdict
        |> get key
        |> Task.andThen
            (\mv ->
                case alter mv of
                    Just x ->
                        tdict |> Internal.insert key x

                    Nothing ->
                        tdict |> Internal.remove key
            )


{-| remove : comparable -> Dict comparable v -> Dict comparable v
-}
remove : comparable -> TDict comparable v -> Task () (TDict comparable v)
remove key tdict =
    tdict |> Internal.remove key



-- Query
-- isEmpty : Dict k v -> Bool
-- member : comparable -> Dict comparable v -> Bool
-- get : comparable -> Dict comparable v -> Maybe v


get : comparable -> TDict comparable v -> Task () (Maybe v)
get key tdict =
    tdict |> Internal.get key



-- size : Dict k v -> Int
-- Lists
-- keys : Dict k v -> List k
-- values : Dict k v -> List v
-- toList : Dict k v -> List ( k, v )
-- fromList : List ( comparable, v ) -> Dict comparable v
-- Transform
-- map : (k -> a -> b) -> Dict k a -> Dict k b
-- foldl : (k -> v -> b -> b) -> b -> Dict k v -> b
-- foldr : (k -> v -> b -> b) -> b -> Dict k v -> b
-- filter : (comparable -> v -> Bool) -> Dict comparable v -> Dict comparable v
-- partition : (comparable -> v -> Bool) -> Dict comparable v -> ( Dict comparable v, Dict comparable v )
-- Combine
-- union : Dict comparable v -> Dict comparable v -> Dict comparable v
-- intersect : Dict comparable v -> Dict comparable v -> Dict comparable v
-- diff : Dict comparable a -> Dict comparable b -> Dict comparable a
-- merge : (comparable -> a -> result -> result) -> (comparable -> a -> b -> result -> result) -> (comparable -> b -> result -> result) -> Dict comparable a -> Dict comparable b -> result -> result
