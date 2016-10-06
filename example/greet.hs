{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

import           Data.Aeson
import           Data.Monoid
import           Data.Proxy
import           Data.Text
import           GHC.Generics
import           Network.Wai
import           Network.Wai.Handler.Warp

import           Servant
import           Servant.GraphQL
import Data.GraphQL.AST

-- * Example

-- | A greet message data type
data Greet = Greet { _msg :: Text, _name :: Text, _capital :: Bool }
  deriving (Generic, Show)

instance FromJSON Greet
instance ToJSON Greet

-- API specification
type TestApi =
       -- GET /hello/:name?capital={true, false}  returns a Greet as JSON
       "hello" :> Capture "name" Text :> QueryParam "capital" Bool :> Get '[JSON] Greet

       -- POST /greet with a Greet as JSON in the request body,
       --             returns a Greet as JSON
--  :<|> "greet" :> ReqBody '[JSON] Greet :> Post '[JSON] Greet

  :<|> "greet" :> Capture "greetid" Text :> Delete '[JSON] Text

testApi :: Proxy TestApi
testApi = Proxy

instance GraphQLValue Greet where
    responseType Proxy = TypeNamed $ NamedType "Greet"
    typeDefinition Proxy =
        [ TypeDefinitionObject $ ObjectTypeDefinition
            "Greet"
            []
            [ FieldDefinition "msg" [] $ TypeNamed $ NamedType "String"
            , FieldDefinition "name" [] $ TypeNamed $ NamedType "String"
            , FieldDefinition "capital" [] $ TypeNamed $ NamedType "Bool"
            ]
        ]

-- Server-side handlers.
--
-- There's one handler per endpoint, which, just like in the type
-- that represents the API, are glued together using :<|>.
--
-- Each handler runs in the 'Handler' monad.
{-
server :: GraphQL TestApi
server = helloH :<|> postGreetH :<|> deleteGreetH

  where helloH name Nothing = helloH name (Just False)
        helloH name (Just False) = return $ Greet ("Hello, " <> name) name False
        helloH name (Just True) = return $ Greet ("Hello, " <> toUpper name) name True

        postGreetH = return

        deleteGreetH _ = return NoContent
-}
server = undefined

-- Turn the server into a WAI app. 'serve' is provided by servant,
-- more precisely by the Servant.Server module.
test :: Application
test = serveGraphQL testApi server

-- Run the server.
--
-- 'run' comes from Network.Wai.Handler.Warp
runTestServer :: Port -> IO ()
runTestServer port = run port test

-- Put this all to work!
main :: IO ()
--main = runTestServer 8001
main = print $ schema testApi
