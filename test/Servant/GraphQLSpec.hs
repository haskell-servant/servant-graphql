module Servant.GraphQLSpec (spec) where

import GHC.Generics (Generic)
import Test.Hspec
import Servant
import Servant.GraphQL
import Data.GraphQL.AST

spec :: Spec
spec = do
  schemaSpec

schemaSpec :: Spec
schemaSpec = describe "schema" $ do

  it "contains type definitions for API response types" $ do
    schemaTypes (schema testApi)
      `shouldContain` typeDefinition (Proxy :: Proxy Character)

  it "contains queries for Get endpoints " $ do
    length (schemaQueries $ schema testApi) `shouldBe` 1

-- * Types

data Character = Character
  { name :: String
  , appearsIn :: [Episode]
  } deriving (Eq, Show, Read, Generic)

instance GraphQLValue Character where
    responseType _ = TypeNamed $ NamedType "Character"
    typeDefinition _ = []

newtype Episode = Episode { getEpisode :: String }
  deriving (Eq, Show, Read, Generic)

-- * API

type TestApi =
       "hello" :> Capture "name" String :> Get '[JSON] Character

testApi :: Proxy TestApi
testApi = Proxy
