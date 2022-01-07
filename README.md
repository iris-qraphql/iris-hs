# Iris

The motivation of Iris is to combine the flexibility of the GraphQL query language with the formalism and strength of the Haskell type system.

the Language attempts to substitute various entities of the GQL language (such as input, scalar, enum, object, enum, interface, and wrapping types) with small but more unified and powerful alternatives (such as `resolver`, `data`, and `wrapping` types).

The types defined by Iris can be converted into the standard GQL language that can be used by GraphQL clients. However, these converted types have additional annotations that provide additional information (like JSDoc) that can be used by code genes to generate suitable types for them.

for documentations see [spec](https://github.com/nalchevanidze/iris/tree/main/spec/index.md)

language is in experimental phase, so any feedback or proposal is welcome!

## Example

### Schema

```gql
list Set

data Lifespan 
  = Immortal {} 
  | Limited { max: Int? }

data Power
  = Shapeshifting {}
  | Thunderbolt {}

resolver God = {
  name: String
  power: Set<Power>
  lifespan: Lifespan
}

resolver Deity
  = God
  | Titan { name: String } # exists only inside Deity
  | Unknown {} # exists only inside of Deity`

resolver Query = {
  deities(lifespan: Lifespan?): [Deity]
}
```

### Query

```gql
{
  deities (lifespan: Immortal ) {
    ... on God {
      name
      power
      lifespan
    }
    ... on Deity.Titan {
      name
    }
  }
}
```

### returns

```json
{
  "data": {
    "deities": [
      {
        "__typename": "God",
        "lifespan": {
          "__typename": "Immortal"
        },
        "power": [],
        "name": "Zeus"
      },
      {
        "__typename": "Deity.Titan",
        "name": "Cronos"
      },
      {
        "__typename": "God",
        "lifespan": {
          "__typename": "Immortal"
        },
        "power": [
          {
            "__typename": "Shapeshifting"
          }
        ],
        "name": "Morpheus"
      },
      {
        "__typename": "Deity.Unknown"
      }
    ]
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

- support lists extensions in Haskell
- finish spec
- implement js parser
- implement js client
- implement js Playground
