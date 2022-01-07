# Iris specs

Composite language of GraphQl and Haskell

## Type System

### Types vs type variants

- Type is standalone type element and containing one or multiple variants.
- variant can be:
  - reference of another type with single variant
  - collection if fields with corresponding tag. this variant will exist inside the type and can't be referenced by another types

#### inline variants

union variants can be also defined inside the type definition. for example

```gql
# role: resolver | data
<role> Deity
  = Morpheus {
    name: String
    shapes : String
  }
  | Iris {
    name: String
  }
```

type `A` and `B` will exist only inside `MyType` and will get `ID` as `Deity.Morpheus` and `Deity.Iris`

#### \_\_typename

for example type `User` can use type `Address` however, type `Address` can't.

value `"A"` and `{ type: "A" }` are equals

- server will always serialize `resolver` types as `{ type: "A" }`.
- server will always serialize `data` types as `"A"`

- on inputs user always should be provide \_\_typename
- on outputs \_\_typename will be always automatically selected

### Data types

properties:

- can only strict types
- can’t have arguments
- represents just JSON values
- fields field cannot be selected. It means that client will get its value as if it was GraphQL JSON Scalar (but typed).

__data__ as a generalization of enums, scalars and input types

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

#### Type guards

since we don't have GraphQL interfaces we provide type guards.

```gql
data U | GuardType = A | B
```


__resolver__ as generalization of types, unions and interfaces

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
fragment Morpheus on Deity.Morpheus {
  name
  shapes
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
