{-# LANGUAGE PolyKinds                  #-}

module Servant.GraphQL where

import Control.Applicative
import qualified Data.Map as Map
import Data.Aeson
import qualified Data.Text as T
import Servant hiding (route)
import Servant.Server.Internal hiding (route)
import GHC.TypeLits
import Data.GraphQL.AST
import Data.String.Conversions (cs, (<>))
import Network.HTTP.Types

import           Servant.API.ContentTypes    (AcceptHeader (..),
                                              AllCTRender (..),
                                              AllCTUnrender (..),
                                              AllMime,
                                              canHandleAcceptH)

type GraphQL api = GraphQLT api Handler

newtype Accepts = Accepts [T.Text]

data RequestSchema = RequestSchema
    { schemaQueries :: [FieldDefinition]
    , schemaMutations :: [FieldDefinition]
    , schemaTypes :: [TypeDefinition]
    } deriving Show

instance Monoid RequestSchema where
    mempty = RequestSchema [] [] []
    mappend a b = RequestSchema
        { schemaQueries = schemaQueries a <> schemaQueries b
        , schemaMutations = schemaMutations a <> schemaMutations b
        , schemaTypes = schemaTypes a <> schemaTypes b
        }

class HasName a where
    hasName :: Proxy a -> T.Text

class HasGraphQL api where
    type GraphQLT api (m :: * -> *) :: *

    schema :: Proxy api -> RequestSchema

    handleOperation :: proxy api
                    -> OperationDefinition
                    -> GraphQLT api Handler
                    -> Maybe (Handler Data.Aeson.Value)

class GraphQLValue a where
    responseType :: Proxy a -> Type
    typeDefinition :: Proxy a -> [TypeDefinition]

instance GraphQLValue T.Text where
    responseType _ = TypeNamed $ NamedType "String"
    typeDefinition _ = []

instance GraphQLValue String where
    responseType _ = TypeNamed $ NamedType "String"
    typeDefinition _ = []

instance GraphQLValue Bool where
    responseType _ = TypeNamed $ NamedType "Boolean"
    typeDefinition _ = []

-- * Instances

-- | A server for @a ':<|>' b@ first tries to match the request against the route
--   represented by @a@ and if it fails tries @b@. You must provide a request
--   handler for each route.
--
-- > type MyApi = "books" :> Get '[JSON] [Book] -- GET /books
-- >         :<|> "books" :> ReqBody Book :> Post '[JSON] Book -- POST /books
-- >
-- > server :: Server MyApi
-- > server = listAllBooks :<|> postBook
-- >   where listAllBooks = ...
-- >         postBook book = ...
instance (HasGraphQL a, HasGraphQL b) => HasGraphQL (a :<|> b) where

  type GraphQLT (a :<|> b) m = GraphQLT a m :<|> GraphQLT b m

  schema Proxy = (schema pa) <> (schema pb)
      where pa = Proxy :: Proxy a
            pb = Proxy :: Proxy b

  handleOperation _ op (a :<|> b) = handleOperation pa op a
                                <|> handleOperation pb op b
    where
      pa :: Proxy a
      pa = Proxy
      pb :: Proxy b
      pb = Proxy

-- | If you use 'Capture' in one of the endpoints for your API,
-- this automatically requires your server-side handler to be a function
-- that takes an argument of the type specified by the 'Capture'.
-- This lets servant worry about getting it from the field arguments and turning
-- it into a value of the type you specify.
--
-- You can control how it'll be converted from 'Text' to your type
-- by simply providing an instance of 'FromJSON' for your type.
--
-- Example:
--
-- > type MyApi = "books" :> Capture "isbn" Text :> Get '[JSON] Book
-- >
-- > server :: Server MyApi
-- > server = getBook
-- >   where getBook :: Text -> Handler Book
-- >         getBook isbn = ...
instance (KnownSymbol capture, FromJSON a, HasGraphQL api, GraphQLValue a)
  => HasGraphQL (Capture capture a :> api) where

  type GraphQLT (Capture capture a :> api) m =
      a -> GraphQLT api m

  schema Proxy =
      case schema (Proxy :: Proxy api) of
        RequestSchema {..} -> RequestSchema
            { schemaQueries = map addArgument schemaQueries
            , schemaMutations = map addArgument schemaMutations
            , ..
            }

   where argName = cs $ symbolVal (Proxy :: Proxy capture)
         addArgument (FieldDefinition p as t) = FieldDefinition p (a':as) t
         a' = InputValueDefinition argName (responseType (Proxy :: Proxy a)) Nothing

  handleOperation _ op fn =

-- | Make sure the incoming request starts with @"/path"@, strip it and
-- pass the rest of the request path to @api@.
instance (KnownSymbol path, HasGraphQL api) => HasGraphQL (path :> api) where

  type GraphQLT (path :> api) m = GraphQLT api m

  schema Proxy =
      case schema (Proxy :: Proxy api) of
        RequestSchema {..} -> RequestSchema
            { schemaQueries = map prefixPath schemaQueries
            , schemaMutations = map prefixPath schemaMutations
            , ..
            }

   where pathPiece = cs $ symbolVal (Proxy :: Proxy path)
         prefixPath (FieldDefinition p as t) = FieldDefinition (pathPiece <> "/" <> p) as t


instance forall k1 (ctypes :: [*]) (a :: *) (method :: k) (status :: Nat). ( ReflectMethod method, KnownNat status, GraphQLValue a
         ) => HasGraphQL (Verb method status ctypes a) where

  type GraphQLT (Verb method status ctypes a) m = m a

  schema Proxy =
      if method == methodGet && status == ok200
         then RequestSchema
            [(FieldDefinition "GET" [] (responseType (Proxy :: Proxy a)))]
            []
            (typeDefinition (Proxy :: Proxy a))
            else mempty -- Todo: Handle mutations
                where method = reflectMethod (Proxy :: Proxy method)
                      status = toEnum . fromInteger $ natVal (Proxy :: Proxy status)

-- | If you use @'QueryParam' "author" Text@ in one of the endpoints for your API,
-- this automatically requires your server-side handler to be a function
-- that takes an argument of type @'Maybe' 'Text'@.
--
-- This lets servant worry about looking it up in the query string
-- and turning it into a value of the type you specify, enclosed
-- in 'Maybe', because it may not be there and servant would then
-- hand you 'Nothing'.
--
-- You can control how it'll be converted from 'Text' to your type
-- by simply providing an instance of 'FromHttpApiData' for your type.
--
-- Example:
--
-- > type MyApi = "books" :> QueryParam "author" Text :> Get '[JSON] [Book]
-- >
-- > server :: Server MyApi
-- > server = getBooksBy
-- >   where getBooksBy :: Maybe Text -> Handler [Book]
-- >         getBooksBy Nothing       = ...return all books...
-- >         getBooksBy (Just author) = ...return books by the given author...
instance (KnownSymbol sym, FromHttpApiData a, HasGraphQL api, GraphQLValue a)
  => HasGraphQL (QueryParam sym a :> api) where

  type GraphQLT (QueryParam sym a :> api) m =
      Maybe a -> GraphQLT api m

  schema Proxy =
      case schema (Proxy :: Proxy api) of
        RequestSchema {..} -> RequestSchema
            { schemaQueries = map addArgument schemaQueries
            , schemaMutations = map addArgument schemaMutations
            , ..
            }

   where argName = cs $ symbolVal (Proxy :: Proxy sym)
         addArgument (FieldDefinition p as t) = FieldDefinition p (a':as) t
         a' = InputValueDefinition argName (responseType (Proxy :: Proxy a)) Nothing



serveGraphQL = undefined

{-
    instance (HasName a, ToJSON a) => HasGraphQL (Handler a) where
        hasGraphQL x = Fields $ Map.fromList [(n, Val $ toJSON <$> x)]
            where
                n = hasName (Proxy :: Proxy a)

instance (FromJSON a, HasGraphQL r) => HasGraphQL (a -> r) where
    hasGraphQL fn = Fn $ \x -> hasGraphQL $ fn (forceResult $ fromJSON x)
        where
            forceResult (Success a) = a
            forceResult _ = error "bother later"

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
