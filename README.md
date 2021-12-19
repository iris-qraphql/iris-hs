# Iris

a Language influenced by GraphQL and Haskell

for documentations see `spec/index.md`

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
