# Iris specs

Composite language of GraphQl and Haskell

## Type System

### Types, variants and typeVariants

- **Type**: a standalone entity and containing one or multiple variants.
- **VariantType**: a type with a singe variant
- **Variant**: a variant can be either a reference of VariantType or collection of fields with corresponding tag. this variant will exist inside the type and can't be referenced by another types

in following schema, variants `God` and `Titan` will exist only inside `Deity` and will get `__typename` as `Deity.God` and `Deity.Titan`

```gql
<role> God = { name: String }
# role: resolver | data
<role> Deity
  = God
  | Titan {
      name: String
    }
```

#### \_\_typename

every type (variant) has field \_\_typename.

value `"A"` and `{ __typename: "A" }` are equals

- server will always serialize `resolver` and `data` types as `{ __typename: "A" }`.
- in every selection, field `__typename` will be always automatically selected
- on input unions user must provide `__typename`

### Data types

properties:

- can only strict types
- can’t have arguments
- represents just JSON values
- fields field cannot be selected. It means that client will get its value as if it was GraphQL JSON Scalar (but typed).

**data** as a generalization of enums, scalars and input types

```gql
# GQL enum with data
data City = Athens {} | Sparta {}

# GQL input object with data
data Deity = { name: String }

# GQL scalar with data
data Natural = Int
```

in addition, data types can also provide input unions and type safe scalar values to client

### Resolver types

properties:

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

#### Type guards

since we don't have GraphQL interfaces we provide type guards.

```gql
data U | GuardType = A | B
```

**resolver** as generalization of types, unions and interfaces

```gql
## GQL type
resolver A = { a: Int? }

## GQL Union
resolver X
  | A ## GQL interface
  = X1 { a: Int }
  | X2 { a: Int? , b: Float }
```

```gql
fragment Morpheus on X.X1 {
  name
}
```

## Wrappers

### Maybe

like in Haskell, every type is required, for optional types we use operator `?`.

for example:

- required type: `Type`
- optional type: `Type?`

### (Named) Lists

named lists are list with specific behavior

```gql
list Set

resolver Query {
  ids: Set<Int>
}
```

## Additional Restrictions

### No Explicit Schema Definition

in graphql you can define schema, with custom types as query. However in iris for simplicity only Types with corresponding names : `Query`, `Mutation`, `Subscription` will be selected as corresponding operations.

### default values can be used only on ArgumentsDefinition

default values for data types complicates validation process. therefore we only allow default values for argument types.

for example :

```gql
resolver Type {
  field(max: Int = 10): [String]
}
```
