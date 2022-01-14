# Why GraphQL needs Typed Scalars

## Motivation

One of the fundamental strengths of GraphQL is that you have control over the depth of the structure by selecting fields in your query. For example, if we have the type user that has users as friends, we can query those friends, those friends' friends, etc. The query determines the depth of this data structure without getting into an infinite loop of the graph database.

__schema__

```graphql
type User {
  name: String!
  friends: [User!]!
}

type Query {
  user(name: String!): User
}
```

__query__

```graphql
{
  user {
    name
    friends {
      name
      friends{
        name
      }
    }
  }
}
```

This limitation makes GraphQL very attractive for graph databases and recursive data types with independent resolvers, so you don't get run in a loop. Nevertheless, this design can hinder some cases; let's explore some of them.

### Introspection Type

Every time you use the GQL playground (or client), your IDE presents readable documentation and validates queries for you based on introspection. The introspection is the query to the API itself about its schema, where each type is represented with type `__Type.`

```graphql
type __Type {
  kind: __TypeKind!
  name: String
  description: String
  # must be non-null for OBJECT and INTERFACE, otherwise null.
  fields(includeDeprecated: Boolean = false): [__Field!]
  # must be non-null for OBJECT and INTERFACE, otherwise null.
  interfaces: [__Type!]
  # must be non-null for INTERFACE and UNION, otherwise null.
  possibleTypes: [__Type!]
  # must be non-null for ENUM, otherwise null.
  enumValues(includeDeprecated: Boolean = false): [__EnumValue!]
  # must be non-null for INPUT_OBJECT, otherwise null.
  inputFields: [__InputValue!]
  # must be non-null for NON_NULL and LIST, otherwise null.
  ofType: __Type
  # may be non-null for custom SCALAR, otherwise null.
  specifiedByURL: String
}
```

The type `__Type` references itself with the field `ofType` to represent wrapped types like `Lists` and `NonNull,` which may be wrapped several times until we reach the named type. However, there is a problem: how do we know how many levels to select until we finally get the NamedType? The answer is that we don't. However, we can speculate that the probability of someone defining the type reference with nine levels of wrapping is close to zero. Therefore, popular GraphQL clients use this assumption and write their queries with eight levels of nesting (see fragment `TypeRef`).

```graphql
fragment TypeRef on __Type {
  kind
  name
  ofType {
    kind
    name
    ofType {
      kind
      name
      ofType {
        kind
        name
        ofType {
          kind
          name
          ofType {
            kind
            name
            ofType {
              kind
              name
              ofType {
                kind
                name
              }
            }
          }
        }
      }
    }
  }
}
```

Hypothetically, we could create a schema with type `[[[[[User]!]!]!]!` where we could break clients. Since this scenario is doubtful, it was never a big issue in GraphQL. However, there are other recursive types where this issue can be challenging.

### Tree Types

The most common case where the strength of GraphQL becomes a pain for the developer is when dealing with Tree Types. They can have hundreds or even thousands of nesting levels before reaching the leaf nodes, which sometimes makes them impossible to query. There are several tree types, but we will only consider one instance: the RichText. Let's assume the following case. We want to create `RichText` in our `WebApp` where we use GraphQL `BFF`.

```graphql
enum RichTextNodeType {
  Label
  Paragraph
  Image
}

type RichTextNode {
  type: RichTextNodeType!
  src: String
  text: String
  children: [RichTextNode!]
}
```

For this case, the solution presented above (see TypeRef) is no longer applicable, as it can have hundreds of nesting levels depending on the content.

## Solution in GraphQL

The straightforward solution to this problem is to represent `RichText` by scalar `JSON.` However, the client knows nothing about the type and cannot statically check the correctness of the code. To improve this, type generators (e.g., `Apollo`) provide type mapping to map scalar names to specific types. However, this works well when server and client are packages in the same Monorepo and use the same language. If we target third-party clients, we need to define the library "@types/rich-text" and publish it on `npm` for them.

```yaml
// apollo.config.yaml

config:
  scalars:
    Node: import('@types/rich-text').RichTextNode
```

However, this approach has the following problems:

- What if we want to target different languages (Java, TS, Flow, Elm ... )? Should we manually provide a type definitions library for each particular language? Even if we do that, we have to maintain each of them to introduce updates in the data types.
- are we sure as a client that the published types library is not outdated?
- are we sure we have the correct version of the type definitions for the API?
- The validity of the values is not checked by GraphQL automatically, but the developer has to check it manually.
- Never the less, in Apollo Codegen, we have to map library types to scalar types by hand.

A general solution in GraphQL is typed scalars (which we have in `Iris` as `data` types). A typed scalar will represent JSON values without getting its dedicated resolvers. GraphQL compiler will only check if the values match type definitions and will not automatically resolve their fields. That way, we would not run into the loop but still have type safety guaranteed by the compiler.

One attempt of solving this problem in GraphQL is to provide type annotations with `JSDoc` in the scalar description, where a type generator could parse annotations and generate corresponding types. In addition, a server with the directive `@JSDoc` could use these annotations to validate scalar (inputs/outputs) values.

graphql-schema

```graphql
enum RichTextNodeType = {
  Label
  Paragraph
  Image
}

"""
@type {{  
  type: RichTextNodeType,
  src: ?string,
  text: ?string,
  children: ?RichTextNode[]
  }}
"""
scalar @JSDoc RichTextNode
```

generated-code-typescript

```ts
// __generated__/globalTypes.ts

export type RichTextNodeType = "Label" | "Paragraph" | "Image"

export type RichTextNode = {
  type: RichTextNodeType,
  src: string | undefined,
  text: string | undefined,
  children: RichTextNode[] | undefined
}
```
