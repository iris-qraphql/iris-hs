{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Iris.Schema.Schema
  ( internalSchema,
  )
where

import Language.Iris.Schema.DSL (dsl)
import Language.Iris.Types.Internal.AST.TypeSystem (Schema)

internalSchema :: Schema s
internalSchema =
  [dsl|

"""
Directs the executor to skip this field or fragment when the `if` argument is true.
"""
directive @skip(if: Boolean) 
  on FIELD | FRAGMENT_SPREAD | INLINE_FRAGMENT

"""
Directs the executor to include this field or fragment only when the `if` argument is true.
"""
directive @include(if: Boolean) 
  on FIELD | FRAGMENT_SPREAD | INLINE_FRAGMENT

"""
Marks an element of a GraphQL schema as no longer supported.
"""
directive @deprecated(reason: String?) on FIELD_DEFINITION 

scalar Boolean
scalar Int
scalar Float
scalar String
scalar ID

resolver __Schema = {
  types: [__Type]
  queryType: __Type
  mutationType: __Type?
  subscriptionType: __Type?
  directives: [__Directive]
}

resolver __TypeFields = {
  name: String
  description: String?
}

resolver __Type 
  | __TypeFields
    = Scalar {
        name: String
        description: String?
      } 
    | Series {
        name: String
        description: String?
        parameters: [__TypeRef] 
      }
    | ADT {
        role: __Role?
        name: String
        description: String?
        guard: String?
        variants(includeDeprecated: Boolean = false): [__Variant] 
      }

data __Role = DATA {} | RESOLVER {} 

resolver __Variant = {
  name: String
  namespace: String?
  fields(includeDeprecated: Boolean = false): [__Field]?
}

data __TypeRef = {
  name: String
  required: Boolean 
  parameters: [__TypeRef]
}

resolver __Field = {
  name: String
  description: String?
  type: __TypeRef
  args: [__Argument]?
  deprecation: String?
}

resolver __Argument = {
  name: String
  description: String?
  type: __TypeRef
  defaultValue: String?
}

resolver __Directive = {
  name: String
  description: String?
  locations: [__DirectiveLocation]
  args: [__Argument]
}

data __DirectiveLocation 
  = QUERY {}
  | MUTATION {}
  | SUBSCRIPTION {}
  | FIELD {}
  | FRAGMENT_DEFINITION {}
  | FRAGMENT_SPREAD {}
  | INLINE_FRAGMENT {}
  | SCHEMA {}
  | SCALAR {}
  | DATA {}
  | RESOLVER {}
  | FIELD_DEFINITION {}
  | ARGUMENT_DEFINITION {}

resolver Query = {
  __type(name: String): __Type?
  __schema: __Schema
}
|]
