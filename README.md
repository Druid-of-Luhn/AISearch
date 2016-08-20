# AISearch

A Haskell toolkit of common AI search algorithms. A type class `Problem` is defined to standardise the form of implementations. Search algorithms extend `Problem` and problem implementations are defined as instances of them.

So far, the following algorithms have been implemented:

- Breadth First Search (BFS)
- Depth First Search (DFS)
- A\*

Using existing algorithms, the following problems have been implemented:

- Travel problem (get from point A to point B) (`travel-A*`, `travel-bfs`, `travel-dfs`)
- 8/15-tiles problem (`8tiles`)

## Usage

Compile with `stack build` and run with `stack exec AISearch-exe -- <problem>`, passing it input on stdin.

For example:

```
stack build
stack exec AISearch-exe -- 8tiles < examples/8tiles.txt
```

## Help

Help files for the different problems that have been implemented are available in the "help/" directory, and are accessible by passing the `--help` flag after the problem name: `stack exec AISearch-exe -- travel-bfs --help`.

## License

Copyright © Billy Brown 2016

See LICENSE file for more information.
