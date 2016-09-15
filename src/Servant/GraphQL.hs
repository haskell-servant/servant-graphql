module Servant.GraphQL where

import qualified Data.Map as Map
import Data.Aeson
import qualified Data.Text as T
import Servant

data GraphQL
  -- This directly returns a value
  = Val (Handler Value)
  -- This needs an argument.
  | Fn (Value -> GraphQL)
  -- This has fields
  | Fields (Map.Map T.Text GraphQL)

newtype Accepts = Accepts [T.Text]

class HasName a where
  hasName :: Proxy a -> T.Text

class HasGraphQL a where
   hasGraphQL :: a -> GraphQL

instance (HasName a, ToJSON a) => HasGraphQL (Handler a) where
   hasGraphQL x = Fields $ Map.fromList [(n, Val $ toJSON <$> x)]
     where
       n = hasName (Proxy :: Proxy a)

instance (FromJSON a, HasGraphQL r) => HasGraphQL (a -> r ) where
   hasGraphQL fn = Fn $ \x -> hasGraphQL $ fn (forceResult $ fromJSON x)
      where
        forceResult (Success a) = a
        forceResult _ = error "bother later"

{-
Example:

{
  human(id: "1000") {
    name
    height
  }
}

Becomes (roughly)

\g -> case g of
  Fields m -> case Map.lookup "human" m of
    Fn fn -> case fn 1000 of
      Fields m' -> case (Map.lookup "name" m', Map.lookup "height" m' of)
        (Val nameE, Val heightE) -> <return>
    _ -> badQuery
  _ -> badQuery
  -}
