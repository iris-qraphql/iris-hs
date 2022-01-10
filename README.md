# Iris

The motivation of Iris is to combine the flexibility of the GraphQL query language with the formalism and strength of the Haskell type system.

the Language attempts to substitute various entities of the GQL language (such as input, scalar, enum, object, enum, interface, and wrapping types) with small but more unified and powerful alternatives (such as `resolver`, `data`, and `wrapping` types).

The types defined by Iris can be converted into the standard GQL language that can be used by GraphQL clients. However, these converted types have additional annotations that provide additional information (like JSDoc, see Exposing Scalar Type Definition) that can be used by code-gen to generate suitable types for them.

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
        "lifespan": "Immortal",
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

- **resolver types**: the resolver type is decomposed into object and union types, where we define a new object type `Deity_Titan` for the inline variant `Deity.Titan`.

- **data types**: in iris, the user can pass data values as input or retrieve json values in a query. Therefore, we have represented data type values as custom scalar values in the generated GQL schema. Since scalar values do not reveal the underlying data type, information about the Iris types is lost in this way. To compensate that, the scalar types we generate are provided with iris type definition annotations (in our example as JSDoc) in the description, which can be parsed by the `code-gens` to generate appropriate types for them.

  Nevertheless, the approach also brings its limitations. The GQL language itself cannot guarantee type safety of the scalar input values. Therefore, each input value should be re-declared as a variable and passed as JSON by the host language, which uses generated types to check its validity (see variable `$lifespan` in query).

```gql
"""
@typedef {{ __typename: "Lifespan.Limited", max: ?number }} Lifespan_Limited
@type {("Immortal" | Lifespan_Limited)}
"""
scalar Lifespan

type God {
  name: String!
  lifespan: Lifespan!
}

# represents inline variant Deity.Titan
type Deity_Titan {
  name: String!
}

union Deity = God | Deity_Titan

type Query {
  deities(lifespan: Lifespan): [Deity!]!
}
```

based on the schema `ApolloCLI` generates corresponding typescript types for the `queries/GetDeities.ts`.

```ts
// queries/GetDeities.ts

import { gql } from "@apollo/client";

export const GET_DEITIES = gql`
  query GetDeities($lifespan: Lifespan!) {
    deities(lifespan: $lifespan) {
      ... on God {
        name
        lifespan
      }
      ... on Deity_Titan {
        name
      }
    }
  }
`;
```

```ts
// queries/__generated__/GetDeities.ts

import { Lifespan } from "__generated__/globalTypes";

export interface GetDeities_deities_God {
  __typename: "God";
  name: string;
  lifespan: Lifespan;
}

export interface GetDeities_deities_Deity_Titan {
  __typename: "Deity_Titan";
  name: string;
}

export type GetDeities_deities =
  | GetDeities_deities_God
  | GetDeities_deities_Deity_Titan;

export interface GetDeities {
  deities: GetDeities_deities[];
}

export interface GetDeitiesVariables {
  lifespan: Lifespan;
}
```

```ts
// __generated__/globalTypes.ts

export type Lifespan_Limited = {
  __typename: "Lifespan.Limited";
  max: number | undefined;
};

export type Lifespan = "Immortal" | Lifespan_Limited;
```

if you use React, you can end with the component:

```tsx
const Deities = () => {
  const { loading, error, data } = useQuery(GET_DEITIES, {
    variables: { lifespan: "Immortal" },
  });

  return <div>...</div>;
};
```

## Exposing Scalar Type Definition

## using JSDoc in Description

Using GQL scalar descriptions for JSDoc type definitions.

```gql
"""
@type{("Athens" | "Ithaca")}
"""
scalar City
```

## using JSON Schema as Introspection field

For example, for the introspection of the scalar type "Lifespan" the field "jsonSchema" (see example below) can be provided, which can be used by Code-Gens.

```json
{
  "name": "Lifespan",
  "kind": "SCALAR",
  "jsonSchema": {
    "$schema": "http://json-schema.org/draft-04/schema#",
    "title": "Lifespan",
    "oneOf": [
      {
        "enum": ["Immortal"]
      },
      {
        "type": "object",
        "properties": {
          "__typename": { "enum": ["Limited"] },
          "max": {
            "type": "integer"
          }
        }
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

- implement js (parser / server / client / playground)
- implement ts code-gen plugin for iris
- tuples and maps
