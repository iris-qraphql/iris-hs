{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main
  ( main,
  )
where

import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Iris.App
  ( App (..),
    mkApp,
    runApp,
  )
import Data.Iris.App.Internal.Resolving (resultOr)
import Data.Iris.App.NamedResolvers
  ( NamedResolverFunction,
    RootResolverValue,
    enum,
    getArgument,
    list,
    object,
    queryResolvers,
    ref,
    refs,
  )
import Language.Iris
  ( parseSchema,
  )
import Language.Iris.Types.Internal.AST
  ( QUERY,
    Schema,
    VALID,
  )
import Web.Scotty ( body, post, raw, scotty )
import Relude hiding (ByteString)

resolverDeities :: Monad m => RootResolverValue e m
resolverDeities =
  queryResolvers
    [ ( "Query",
        const $
          object
            [ ("deity", ref "Deity" <$> getArgument "id"),
              ("deities", pure $ refs "Deity" ["zeus", "morpheus"])
            ]
      ),
      ("Deity", deityResolver)
    ]

deityResolver :: Monad m => NamedResolverFunction QUERY e m
deityResolver "zeus" =
  object
    [ ("name", pure "Zeus"),
      ("power", pure $ list [])
    ]
deityResolver _ =
  object
    [ ("name", pure "Morpheus"),
      ("power", pure $ list [enum "Shapeshifting"])
    ]

getSchema :: IO (Schema VALID)
getSchema = LBS.readFile "examples/app/src/deities.gql"  >>= resultOr (fail . show) pure . parseSchema

irisApp :: IO (App e IO)
irisApp  = (`mkApp` resolverDeities) <$> getSchema 

api :: LBS.ByteString -> IO LBS.ByteString
api req = irisApp >>= (`runApp` req)

main :: IO ()
main = scotty 3000 $ post "/api" $ raw =<< (liftIO . api =<< body)
