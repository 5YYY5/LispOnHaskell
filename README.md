# LispOnHaskell

A small Lisp interpreter implemented in Haskell.

## Run

- Build: `stack build`
- Interactive REPL: `stack exec my-project-exe -- repl`
- Execute file: `stack exec my-project-exe -- file test/test`
- Shortcut (file path directly): `stack exec my-project-exe -- test/test`

If you start without args (`stack exec my-project-exe`), the app asks you to choose REPL or file mode.

## Tests

- Run all tests: `stack test`
- The test suite validates core operations and executes sample programs from:
  - `test/test`
  - `test/user-functions.lisp`
