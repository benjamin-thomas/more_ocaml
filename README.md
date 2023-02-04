# Reading the book : More OCaml, by John Whitington

## Setup

The library `more` may be installed with:

```bash
opam install more-ocaml
```

## Dev workflow n°1

1. Launch the `utop` wrapper at:
    `./manage/dev/utop`

It auto-reloads on file changes

2. Launch this background worker:
    `foreman start -f Procfile.dev`

It kills the child REPL on file changes (file must be tracked by git).

3. To kill the `utop` wrapper, run:
    `kill $(cat .utop.ppid)`

And maybe `reset` if the terminal happens to be wonky.

---

An alternative could be to combine steps 2 and 3 in one go, like this:

```bash
# Run first, pane 2
./manage/dev/utop ; reset

# Run second, pane 1
foreman start -f Procfile.dev ; kill $(cat .utop.ppid)
```

## Dev workflow n°2

Run the inline tests, as such:

```
dune runtest -w
```
