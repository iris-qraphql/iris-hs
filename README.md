# Iris

The motivation of Iris is to combine the flexibility of the GraphQL query language with the formalism and strength of the Haskell type system.

the Language attempts to substitute various entities of the GQL language (such as input, scalar, enum, object, enum, interface, and wrapping types) with small but more unified and powerful alternatives (such as `resolver`, `data`, and `wrapping` types).

The types defined by Iris can be converted into the standard GQL language that can be used by GraphQL clients. However, these converted types have additional annotations that provide additional information (like JSDoc) that can be used by code-gen to generate suitable types for them.

for documentations see [spec](https://github.com/nalchevanidze/iris/tree/main/spec/index.md)

language is in experimental phase, so any feedback or proposal is welcome!

## Example

### Schema

### Iris Schema

```gql
data Lifespan 
  = Immortal {} 
  | Limited { max: Int? }

resolver God = {
  name: String
  lifespan: Lifespan
}

resolver Deity
  = God
  | Titan { name: String } # __typename = Deity.Titan 

resolver Query = {
  deities(lifespan: Lifespan?): [Deity]
}
```

#### Iris client Query

```gql
{
  deities (lifespan: Immortal{} ) {
    ... on God {
      name
      lifespan
    }
    ... on Deity.Titan {
      name
    }
  }
}
```

### iris response

```json
{
  "data": {
    "deities": [
      {
        "__typename": "God",
        "lifespan":  "Immortal",
        "name": "Zeus"
      },
      {
        "__typename": "Deity.Titan",
        "name": "Cronos"
      },
      {
        "__typename": "God",
        "lifespan": "Immortal",
        "name": "Morpheus"
      }
    ]
  }
}
```

## Using Iris App by GQL Client

_Note: This is only a draft that has not yet been implemented._

### Conversion of the Iris schema into a GQL schema

The iris app provides GQL introspection that converts the types defined above into the following GQL schema.

- __resolver types__: the resolver type is decomposed into object and union types, where we define a new object type `Deity_Titan` for the inline variant `Deity.Titan`.

- __data types__: in iris, the user can pass data values as input or retrieve json values in a query. Therefore, we have represented data type values as custom scalar values in the generated GQL schema. Since scalar values do not reveal the underlying data type, information about the Iris types is lost in this way. To compensate that, the scalar types we generate are provided with iris type definition annotations (in our example as JSDoc) in the description, which can be parsed by the `code-gens` to generate appropriate types for them.

  Nevertheless, the approach also brings its limitations. The GQL language itself cannot guarantee type safety of the scalar input values. Therefore, each input value should be re-declared as a variable and passed as JSON by the host language, which uses generated types to check its validity (see variable `$lifespan` in query).

```gql

"""
@typedef {{ max: number, __typename: "Lifespan_Limited" }} Lifespan_Limited
@typedef {("Immortal" | Lifespan_Limited) } Lifespan
"""
scalar Lifespan

type God {
  name: String!
  lifespan: Lifespan!
}

type Deity_Titan { 
  name: String!
}

type Deity
  = God
  | Deity_Titan

resolver Query = {
  deities(lifespan: Lifespan): [Deity!]!
}
```

query

```gql
query ($lifespan: Lifespan) {
  deities (lifespan: $lifespan ) {
    ... on God {
      name
      power
      lifespan
    }
    # ... on Deity.Titan 
    ... on Deity_Titan {
      name
    }
  }
}
```

## About

### The name

> _Iris was the Greek goddess – or, better yet, personification – of the rainbow, and a messenger for the gods._
>
> _[Greek Mythology](https://www.greekmythology.com/Other_Gods/Iris/iris.html)_

### Team

Iris is written and maintained by [_nalchevanidze_](https://github.com/nalchevanidze)

## Roadmap

- support GQL spec transformation
- implement js parser
- implement js client
- implement js Playground
- implement ts code-gen plugin for iris
