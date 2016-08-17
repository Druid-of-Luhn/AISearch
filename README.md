# AISearch

A Haskell toolkit of common AI search algorithms. A type class `Problem` is defined to standardise the form of implementations. Search algorithms extend `Problem` and problem implementations are defined as instances of them.

So far, the following algorithms have been implemented:

- Depth First Search (DFS)
- A\*

Using existing algorithms, the following problems have been implemented:

- Travel problem (get from point A to point B)
- 8/15-tiles problem

## Usage

Compile with `stack build` and run with `stack exec AISearch-exe -- <problem>`, passing it input on stdin.

For example:

```
stack build
stack exec AISearch-exe -- 8tiles < data/8tiles.txt
```

## License

Copyright © Billy Brown 2016

See LICENSE file for more information.
