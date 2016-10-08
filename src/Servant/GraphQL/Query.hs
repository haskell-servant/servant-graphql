module Servant.GraphQL.Query where

import Data.GraphQL.AST
import qualified Data.Map as Map
import qualified Data.Text as T

data GraphQLQuery
  = Leaf
  | Err
  | NameFn (Name -> GraphQLQuery)
  | ArgFn ([Argument] -> GraphQLQuery)
  | Fields (Map.Map T.Text GraphQLQuery)

nodeAsQuery :: Node -> GraphQLQuery
nodeAsQuery (Node _ _ _ sels) = selectionSetAsQuery sels
  where
    getFieldFns :: [Field] -> Name -> GraphQLQuery
    getFieldFns ((Field _ n _ _ sset):r) nameArg
      | n == nameArg = selectionSetAsQuery sset
      | otherwise    = getFieldFns r nameArg
    getFieldFns [] _ = Leaf

    -- We don't yet implement fragments, so here we just ignore them
    onlyFields :: SelectionSet -> [Field]
    onlyFields (SelectionField f : r) = f : onlyFields r
    onlyFields (_ : r)                = onlyFields r
    onlyFields []                     = []

    selectionSetAsQuery :: SelectionSet -> GraphQLQuery
    selectionSetAsQuery [] = Leaf
    selectionSetAsQuery ss = NameFn . getFieldFns $ onlyFields ss
