Transforming from `CamelCase` to `snake_case` is not as simple as it sounds.
To do it correctly, we would need to statically analyze all the symbols, and make sure that we're not changing any function names that come from packages.
We could take it even further, and change all the symbols in the packages to be consistently named using one case convention.

We could also make other changes, for example, going from pipes `%>%` to standard calls.

This would make it possible for two developers to share the same codebase and work in different case style conventions.
