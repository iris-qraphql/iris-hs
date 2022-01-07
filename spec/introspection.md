# Introspection

## Schema

```gql
resolver __Schema {
  types: [__Type]
  queryType: __Type
  mutationType: __Type?
  subscriptionType: __Type?
  directives: [__Directive]
}

resolver __TypeGuard {
  name: String
  description: String?
}

resolver __Type
  | __TypeGuard
    = List {
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
