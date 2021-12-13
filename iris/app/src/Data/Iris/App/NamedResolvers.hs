module Data.Iris.App.NamedResolvers
  ( ref,
    object,
    variant,
    list,
    refs,
    enum,
    queryResolvers,
    getArgument,
    NamedResolverFunction,
    RootResolverValue,
    ResultBuilder,
  )
where

import qualified Data.HashMap.Lazy as HM
import Data.Iris.App.Internal.Resolving.Resolver (LiftOperation, Resolver, getArgument)
import Data.Iris.App.Internal.Resolving.RootResolverValue (RootResolverValue (..))
import Data.Iris.App.Internal.Resolving.Types
  ( NamedResolver (..),
    NamedResolverRef (..),
    NamedResolverResult (..),
    ObjectTypeResolver (..),
    ResolverMap,
    ResolverValue (..),
    mkList,
  )
import Data.Mergeable.Utils (empty)
import Language.Iris.Types.Internal.AST
  ( FieldName,
    QUERY,
    TypeName,
    ValidValue,
  )

-- PUBLIC

-- fields
enum :: TypeName -> ResolverValue m
enum x = ResObject (Just x) (ObjectTypeResolver empty)

list :: [ResolverValue m] -> ResolverValue m
list = mkList

ref :: Applicative m => TypeName -> ValidValue -> ResolverValue m
ref typeName = ResRef . pure . NamedResolverRef typeName

refs :: Applicative m => TypeName -> [ValidValue] -> ResolverValue m
refs typeName = mkList . map (ref typeName)

type NamedResolverFunction o e m = ValidValue -> Resolver o e m (ResultBuilder o e m)

-- types
object :: (LiftOperation o, Monad m) => [(FieldName, Resolver o e m (ResolverValue (Resolver o e m)))] -> Resolver o e m (ResultBuilder o e m)
object = pure . Object

variant :: (LiftOperation o, Monad m) => TypeName -> ValidValue -> Resolver o e m (ResultBuilder o e m)
variant tName = pure . Union tName

queryResolvers :: Monad m => [(TypeName, NamedResolverFunction QUERY e m)] -> RootResolverValue e m
queryResolvers = NamedResolversValue . mkResolverMap

-- INTERNAL
data ResultBuilder o e m
  = Object [(FieldName, Resolver o e m (ResolverValue (Resolver o e m)))]
  | Union TypeName ValidValue

mkResolverMap :: (LiftOperation o, Monad m) => [(TypeName, NamedResolverFunction o e m)] -> ResolverMap (Resolver o e m)
mkResolverMap = HM.fromList . map packRes
  where
    packRes :: (LiftOperation o, Monad m) => (TypeName, ValidValue -> Resolver o e m (ResultBuilder o e m)) -> (TypeName, NamedResolver (Resolver o e m))
    packRes (typeName, value) =
      ( typeName,
        NamedResolver
          typeName
          ( fmap mapValue
              . value
          )
      )
      where
        mapValue (Object x) = NamedObjectResolver (ObjectTypeResolver $ HM.fromList x)
        mapValue (Union name x) = NamedUnionResolver (NamedResolverRef name x)
