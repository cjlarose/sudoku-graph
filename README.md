# Sudoku graph

Suggests hints for sudoku puzzles by operating on the corresponding [Sudoku graph](https://en.wikipedia.org/wiki/Sudoku_graph).

## Run

```sh
npm install -g spago
spago run
```

The program will ask for the puzzle on stdin. Puzzles are encoded as a single line, top-left entry to bottom-right. For example,

```
4...3.......6..8..........1....5..9..8....6...7.2........1.27..5.3....4.9........
```

## Test

```sh
spago test
```
