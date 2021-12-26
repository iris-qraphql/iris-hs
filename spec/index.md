# Iris specs

Composite language of GraphQl and Haskell

## Scalars

Iris scalars are identical to graphql scalars

## No Enums

you can use data types with no fields for it.

```gql
data City = Athens {} | Sparta {}
```

## Types

## types vs type variants

- Type is standalone type element and containing one or multiple variants.
- variant can be:
  - reference of another type with single variant
  - collection if fields with corresponding tag. this variant will exist inside the type and can't be referenced by another types

### roles

- data:

  - can only strict types
  - can’t have arguments
  - represents just JSON values
  - fields field cannot be selected. It means that client will get its value as if it was GraphQL JSON Scalar (but typed).

- resolver:
  - can use data and resolver types
  - can have arguments
  - are like graphql types
  - fields can be selected.

```gql
data Address = {
  name: String
}

resolver User = {
   address(format: String): Address
   friends: [User]
}
```

for example type `User` can use type `Address` however, type `Address` can't.

value `"A"` and `{ type: "A" }` are equals

- server will always serialize `resolver` types as `{ type: "A" }`.
- server will always serialize `data` types as `"A"`

### \_\_typename

- on inputs user always should be provide \_\_typename
- on outputs \_\_typename will be always automatically selected

## Unions

same way as with object we have data and resolver unions.

```gql
data AB = A | B

type CD = C | D
```

### closed unions

union variants can be also enclosed inside by the Union Type. for example

```gql
resolver Deity
  = Morpheus {
    name: String
    shapes : String
  }
  | Iris {
    name: String
  }
```

type `A` and `B` will exist only inside `MyType` and will get `ID` as `Deity.Morpheus` and `Deity.Iris`

```gql
fragment Morpheus on Deity.Morpheus {
  name
  shapes
}
```

### default values

default values for data types complicates validation process. therefore we only allow default values for argument types.

for example :

```gql
resolver Type {
  field(max: Int = 10): [String]
}
```

### Type guards

since we don't have GraphQL interfaces we provide type guards.

```gql
data U | GuardType = A | B
```

## Wrappers

### Maybe

like in Haskell, every type is required, for optional types we use operator `?`.

for example:

- required type: `Type`
- optional type: `Type?`

### List

like GraphQL list.

#### Named Lists

named lists are list with specific behavior

```gql
list Set

resolver Query {
  ids: Set<Int>
}
```

### Map

```gql
resolver User {
  name: String
}

resolver Query {
  users: Map<Int,User>
}
```

### No Explicit Schema Definition

in graphql you can define schema, with custom types as query. However in iris for simplicity only Types with corresponding names : `Query`, `Mutation`, `Subscription` will be selected as corresponding operations.

## Introspection

### Schema

```gql
scalar Boolean
scalar Int
scalar Float
scalar String
scalar ID

resolver __Schema {
  types: [__Type]
  queryType: __Type
  mutationType: __Type?
  subscriptionType: __Type?
  directives: [__Directive]
}

resolver __TypeFields {
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

resolver __Variant {
  name: String
  namespace: String?
  fields(includeDeprecated: Boolean = false): [__Field]?
}

data __TypeRef {
  name: String
  required: Boolean
  parameters: [__TypeRef]
}

resolver __Field {
  name: String
  description: String?
  type: __TypeRef
  args: [__Argument]?
  deprecation: String?
}

resolver __Argument {
  name: String
  description: String?
  type: __TypeRef
  defaultValue: String?
}

resolver __Directive {
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

resolver Query {
  __type(name: String): __Type?
  __schema: __Schema
}
```

### Query

```gql
{ __schema {
    types {
      name
      description
      ...ADT
      ...List
    }
  }
}

fragment ADT on __Type.ADT {
  role
  guard
  variants {
    name
    namespace
    fields {
      name
      description
      type
      args { 
        ...Argument 
      }
      deprecation
    }
  }
}

fragment List on __Type.Series {
  parameters
}

fragment Argument on __Argument {
  name
  description
  type
  defaultValue
}
```
