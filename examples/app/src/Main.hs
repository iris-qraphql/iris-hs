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
    refMap,
    refs,
    variant,
  )
import Language.Iris
  ( parseSchema,
  )
import Language.Iris.Types.Internal.AST
  ( QUERY,
    Schema,
    VALID,
  )
import Relude hiding (ByteString)
import Web.Scotty (body, post, raw, scotty)

resolverDeities :: Monad m => RootResolverValue e m
resolverDeities =
  queryResolvers
    [ ( "Query",
        const $
          object
            [ ("deity", ref "Deity" <$> getArgument "id"),
              ( "deities",
                pure $
                  refMap
                    "Deity"
                    [ ("ze3", "zeus"),
                      ("cr1", "cronos"),
                      ("z34", "morpheus"),
                      ("z42", "kraken")
                    ]
              )
            ]
      ),
      ("Deity", deityResolver),
      ("God", godResolver),
      ("Deity.Titan", titanResolver),
      ("Deity.Unknown", const $ object [])
    ]

deityResolver :: Monad m => NamedResolverFunction QUERY e m
deityResolver name
  | name `elem` ["zeus", "morpheus"] = variant "God" name
  | name `elem` ["cronos", "gaea"] = variant "Deity.Titan" name
  | otherwise = variant "Deity.Unknown" ""

godResolver :: Monad m => NamedResolverFunction QUERY e m
godResolver "zeus" =
  object
    [ ("name", pure "Zeus"),
      ("power", pure $ list []),
      ("lifespan", pure $ enum "Immortal")
    ]
godResolver _ =
  object
    [ ("name", pure "Morpheus"),
      ("power", pure $ list [enum "Shapeshifting"]),
      ("lifespan", pure $ enum "Immortal")
    ]

titanResolver :: Monad m => NamedResolverFunction QUERY e m
titanResolver "cronos" = object [("name", pure "Cronos")]
titanResolver _ = object [("name", pure "Gaea")]

getSchema :: IO (Schema VALID)
getSchema = LBS.readFile "examples/app/src/deities.gql" >>= resultOr (fail . show) pure . parseSchema

irisApp :: IO (App e IO)
irisApp = (`mkApp` resolverDeities) <$> getSchema

api :: LBS.ByteString -> IO LBS.ByteString
api req = irisApp >>= (`runApp` req)

main :: IO ()
main = scotty 3000 $ post "/api" $ raw =<< (liftIO . api =<< body)
