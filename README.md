# Iris

a Language influenced by GraphQL and Haskell

for documentations see `spec/index.md`

language is in experimental phase, so any feedback or proposal is welcome!

## Example

### Schema

```gql
# lists with specific properties
list Set

data Lifespan 
  = Immortal {} 
  | Limited { max: Int? }

data Power
  = Shapeshifting {}
  | Thunderbolt {}

resolver God {
  name: String
  power: Set<Power>
}

resolver Deity
  = God
  | Titan { name: String } # exists only inside of scope `Deity`
  | Monsters {} # exists only inside of scope `Deity`

resolver Query {
  deities(lifespan: Family?): [Deity]
}
```

### Query

```gql
{
  deities(lifespan: Immortal )
  {
    ...on God {
      name
      power
    }
    # accessing fields of scoped type variants
    ...on Deity.Titan {
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
        "name": "Zeus",
        "power": [
          {
            "__typename": "Thunderbolt"
          }
        ]
      },
      {
        "__typename": "Deity.Titan",
        "name": "Cronos"
      }
    ]
  }
}
```
