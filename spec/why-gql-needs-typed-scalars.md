# Why GraphQL needs Typed Scalars

## Problem

One of the fundamental ideas of GraphQL is that you have control over the fields and depth of structure you query. for example, if we have the type user that has users as friends. we can query these friends and friends and their ... Friends. The depth of this data structure can be determined with query without running into an infinite loop of Graph database.

schema

```graphql
type User {
  name: String!
  friends: [User!]!
}

type Query {
  user(name: String!): User
}
```

query

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

This limitation makes GraphQL very attractive for graph databases and recursive data types that have their own independent resolvers, so you don't get caught in a loop.

Still, this design can be a hindrance in some cases, let's explore some of them.

## Introspection Type

The first example is the most common in the GraphQL universe. Every time you use the GQL playground (or client), your IDE presents readable documentation and validates queries for you based on introspection .

The introspection is the query to the API itself about its own schema, where each type is represented with type `__Type`.

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


the type `__Type` references itself with the field `ofType` to represent wrapped types like `Lists` and `NonNull`, which may be wrapped several times until we reach the named type. However, there is a problem: how do we know how many levels to select until we finally reach the NamedType? The answer is that we don't. However, we can speculate that the probability of someone defining the type reference with 9 levels of wrapping is close to zero. Therefore, popular GraphQL clients use this assumption and write their queries with 8 levels of nesting (see fragment `TypeRef`).

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

hypothetically, however, I could create a schema with type `[[[[[User]!]!]!]!` where I could break clients. Since the use of this type is very unlikely, it has never been a big issue in the GraphQL world. However, we have other recursive type scenarios where this condition can be reached quite quickly.

## Tree Types

The most common case where the strength of GraphQL becomes a pain for the developer is when dealing with Tree Types.They can have hundreds or even thousands of nesting levels before we reach the leaf nodes, which sometimes makes them impossible to query. There are several types of tree types, but we will only consider the one instance of it the RichText. Let's assume the following case, we want to create RichText in our `WebApp` where we use GraphQL BFF.

```graphql
enum RichTextNodeType = {
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

For this kind of cases, the solution presented above (see TypeRef) is no longer applicable, as it can have hundreds of nesting levels depending on the content of the editor.

## Solution in GraphQL

the straightforward solution to this problem is to represent `RichText` by scalar with type `JSON`. However, the client knows nothing about the type and cannot statically check that the code dealing with the value is correct. To improve this, type generators (e.g. Apollo) provide type mapping where we can map scalar names to specific types. However, this only works well if we have internal knowledge of the application, or best if we have bff and application in the same Monorepo and use only one language to program the entire application. we can define the library "@types/richtext" with the appropriate types, use them in BFF and publish library them to clients.


```yaml
// apollo.config.yaml

config:
  scalars:
    Node: import('@types/richtext').RichTextNode
```

However, this approach has following problems:

- What if I want different target languages (Java, TS, Flow, Elm ... ), should I manually provide type definition library for each particular target language? then  i have to maintain each of them , If I want to introduce updates in the data types
- am i sure as a client that published types library is not outdated?
- validity of the values is not checked by the GraphQL automatically, but developer has to check it manually.

in best case, we should have typed scalars (which we have in `Iris` as `data` types). typed scalar will represent just JSON value and will not get its dedicated resolver. GraphQL compiler would only check if value is matches to type definition and will not try to automatically resolve its fields. that way we would not run into infinite loop but still have type safety guaranteed by compiler.

one attempt of solving this problem in GraphQL is to provide type annotations with `JSDoc` in scalar description. a type generator could parse annotations and generate corresponding types. in addition,  directive `@JSDoc` could parse types from the description and automatically validate scalar (inputs/outputs) values.

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
  children: ?[RichTextNode!]
  }}
"""
scalar @JSDoc RichTextNode
```