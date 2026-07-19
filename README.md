# yaml-sweep

A YAML configuration engine that turns a single YAML file into many
configuration variants via *sweep expansion*. On top of plain YAML it
supports four custom tags — `!prod`, `!key`/`!zip_<name>`, `!include`,
and `!inherit` — that declaratively describe how a config should fan
out into a list of concrete configurations.

Built entirely on [HsYAML](https://hackage.haskell.org/package/HsYAML).
The public API returns `Data.YAML.Node Pos` values, so every mapping
key retains its source position through the whole sweep pipeline,
which makes error messages point back to the originating line/column
even after multiple files have been merged.

## Concepts

### Sweeps

A *sweep* is a list of alternative values for a single field. yaml-sweep
expands every combination of sweep points into one output variant per
combination.

- **`!prod [a, b, c]`** — an independent sweep point. Multiple
  `!prod` markers multiply independently into a cartesian product.

  ```yaml
  traces:
    samples: !prod [1, 5, 10]
    max_rounds: !prod [5, 20]
  ```
  produces 6 variants (3 × 2). See [examples/02-prod](examples/02-prod),
  [examples/03-prod-cartesian](examples/03-prod-cartesian).

- **`!zip_<name> [a, b, c]`** — an index-linked sweep point. All
  `!zip_<name>` markers that share a scope name advance in lockstep by
  index, so the number of variants for that scope equals the (common)
  length of the lists, not their cartesian product.

  ```yaml
  attribution:
    - drop_threshold: !zip_th [0.5, 0.8, 0.9]
      drop_exponent:  !zip_th [0.5, 0.8, 0.9]
  ```
  produces 3 variants (`{0.5,0.5}`, `{0.8,0.8}`, `{0.9,0.9}`).
  See [examples/05-zip-same-element](examples/05-zip-same-element).

### Scopes

A scope is the binding that links `!zip_<name>` markers together. There
are two kinds:

- **Named (explicit) scope** — declared with `!key <name>` as a key in
  a mapping. The scope is visible across the entire subtree under that
  mapping, including sibling and nested list elements. All `!zip_<name>`
  markers referring to the same named scope must list the same number of
  values, otherwise expansion fails with `inconsistent lengths`.

  ```yaml
  !key th:
  attribution:
    - drop_threshold: !zip_th [0.5, 0.8]
    - drop_threshold: !zip_th [0.5, 0.8]
  ```
  Both list elements share the same `th` value per variant. See
  [examples/07-zip-key-cross-element](examples/07-zip-key-cross-element),
  [examples/13-mixed-scopes](examples/13-mixed-scopes),
  [examples/14-nested-two-keys](examples/14-nested-two-keys),
  [examples/15-nested-key-scopes](examples/15-nested-key-scopes).

- **Local (implicit) scope** — a `!zip_<name>` tag used without a
  matching `!key <name>` declaration. The scope is local the 
  nearest enclosing array element: markers inside *the same* element
  sharing the same name are index-linked, while markers in *different*
  sibling elements form independent local scopes (even with the same
  name) and may have different lengths.

  See [examples/05-zip-same-element](examples/05-zip-same-element) (local scope within one element),
  [examples/06-zip-different-elems](examples/06-zip-different-elems) (same name, two independent local
  scopes of different lengths), [examples/08-nested-zip](examples/08-nested-zip) (a local scope
  spanning two nesting levels, bubbling to the nearest common array).

### Bubbling

When a sweep marker resolves to multiple values, the resulting
expansion *bubbles up* as elements of the nearest enclosing array of the mapping
that contains the marker. A `!prod` inside a list element therefore
duplicates that element; a `!prod` outside any array produces multiple
top-level documents. Each array acts as a boundary — sweeps do not
  propagate further outward than their nearest enclosing array. See
  [examples/04-bubble-one-level-up](examples/04-bubble-one-level-up).

### File inclusion and inheritance

- **`!include <file>`** — a scalar tag that inlines another YAML file
  at the tag position. File resolution is relative to the directory of
  the referencing file. Includes are spliced verbatim into the
  surrounding structure (e.g. as a list element or a mapping value).

  ```yaml
  traces:
    - !include base.yaml
  ```
  See [examples/16-include](examples/16-include),
  [examples/20-include-local-in-array](examples/20-include-local-in-array),
  [examples/21-include-plain-alongside-sweeps](examples/21-include-plain-alongside-sweeps).

- **`!inherit`** — a mapping tag that loads a base file (named by the
  `base:` key) and deep-merges the remaining fields of the `!inherit`
  mapping on top of it. Local values override base values recursively;
  on collision the override's source position wins. Inheritance chains
  are supported (a base file may itself `!inherit`).

  ```yaml
  --- !inherit
  base: base.yaml
  traces:
    max_rounds: 20   # overrides base.yaml's max_rounds
  ```
  See [examples/17-inherit](examples/17-inherit).

### File boundaries

`!include` and `!inherit` both establish a *file boundary*: scopes
declared in the outer file do not leak into the included/inherited
file, and vice-versa. An included file may declare its own `!key` and
`!zip` scopes that are fully resolved inside the include before
splicing. However, an included/inherited file is **not** allowed to
emit top-level keys of its own — its content must be a single value
  that gets spliced into the parent structure. See
  [examples/18-include-file-barrier](examples/18-include-file-barrier) (independent scope inside an
  include), [examples/19-include-with-top-level-key](examples/19-include-with-top-level-key) (rejected: top
  level keys in included file).

## Quick start

```haskell
import Yaml.Sweep (loadConfigValue)

main :: IO ()
main = do
  result <- loadConfigValue "config.yaml"
  case result of
    Left err -> putStrLn $ "Error: " <> show err
    Right variants -> do
      putStrLn $ "Generated " <> show (length variants) <> " configurations"
      -- Each variant is a Data.YAML.Node Pos.
```

## API

```haskell
-- | Load a YAML config file, resolve !include/!inherit, expand !prod
--   and !zip_<name> sweep points, and return one 'Data.YAML.Node Pos'
--   per resulting configuration variant. Mapping keys keep their
--   source positions.
loadConfigValue :: FilePath -> IO (Either Err [Data.YAML.Node Pos])

-- | A structured error: (message, source position, source file).
type Err = (Text, Pos, FilePath)
```

HsYAML's encoder requires positionless nodes, so the bundled CLI
(`yaml-sweep-cli`) strips the `Pos` annotations before serializing;
the library API itself keeps them.

## Pipeline

`loadConfigValue` runs four passes:

1. **Loader** (`Yaml.Sweep.Loader`) — recursively loads every file
   reachable via `!include`/`!inherit`, with cycle detection, and
   caches the parsed `Node Pos` tree of each.
2. **Parser** (`Yaml.Sweep.Parser`) — converts the HsYAML node tree
   into a `ConfigExpr` tree, recognising the custom tags and resolving
   `!inherit` via deep-merge.
3. **Expander** (`Yaml.Sweep.Expander`) — resolves scopes and expands
   `!prod` / `!zip_<name>` into the final list of `Node Pos` variants.

## Errors

All errors are returned as `Err = (Text, Pos, FilePath)` via
`renderErr`, which formats them as `file:line:col: message`. The
examples directory contains one directory per error condition showing
the offending config and the expected message:

| Example | Error |
| --- | --- |
| [09-key-different-lengths-error](examples/09-key-different-lengths-error) | `inconsistent lengths` for markers in the same named scope |
| [10-key-shadowing-error](examples/10-key-shadowing-error) | `shadowing` — a `!key` redeclared inside its own subtree |
| [11-double-key-definition](examples/11-double-key-definition) | `double definition` — `!key` declared twice in the same scope |
| [19-include-with-top-level-key](examples/19-include-with-top-level-key) | `Top level keys in included file is not allowed` |
| [22-prod-empty-list-error](examples/22-prod-empty-list-error) | `!prod with empty list` |
| [23-zip-empty-scope-name-error](examples/23-zip-empty-scope-name-error) | `!zip_ with empty scope name` |
| [24-zip-empty-list-error](examples/24-zip-empty-list-error) | `!zip_<name> with empty list` |
| [25-key-empty-name-error](examples/25-key-empty-name-error) | `!key with empty name` |
| [26-inherit-no-base-error](examples/26-inherit-no-base-error) | `!inherit requires a 'base' key` |
| [27-yaml-parse-error](examples/27-yaml-parse-error) | `YAML parse error` (from HsYAML) |
| [28-empty-yaml-document-error](examples/28-empty-yaml-document-error) | `empty YAML document` |
| [29-include-file-not-found-error](examples/29-include-file-not-found-error) | `file not found` |
| [30-include-cycle-error](examples/30-include-cycle-error) | `Cycle detected` |
| [31-zip-over-zip-boundary-error](examples/31-zip-over-zip-boundary-error) | `Zip over zip boundary is not supported` |

## Examples

The `examples/` directory contains numbered, self-contained cases.
Each case has a `config.yaml` and either a `result.yaml` (the expected
expansion, using `---` document separators when there are multiple
variants) or an `error.txt` (the expected error message). They double
as the test corpus for the library.

| Example | Demonstrates |
| --- | --- |
| [01-simple](examples/01-simple) | Plain config, one variant |
| [02-prod](examples/02-prod) | Single `!prod` sweep point |
| [03-prod-cartesian](examples/03-prod-cartesian) | Multiple `!prod` markers multiply into a cartesian product |
| [04-bubble-one-level-up](examples/04-bubble-one-level-up) | A list is a boundary; `!prod` duplicates its element |
| [05-zip-same-element](examples/05-zip-same-element) | Local zip scope within a single list element |
| [06-zip-different-elems](examples/06-zip-different-elems) | Same zip name in different elements → independent local scopes |
| [07-zip-key-cross-element](examples/07-zip-key-cross-element) | Named `!key` scope linking sibling list elements |
| [08-nested-zip](examples/08-nested-zip) | Local zip scope bubbling across nesting levels |
| [12-different-keys-product](examples/12-different-keys-product) | Two independent scopes in one element → cartesian product |
| [13-mixed-scopes](examples/13-mixed-scopes) | A named outer scope combined with local inner scopes |
| [14-nested-two-keys](examples/14-nested-two-keys) | Nested named scopes driving outer and inner expansion |
| [15-nested-key-scopes](examples/15-nested-key-scopes) | Nested scopes with plain fields alongside sweeps |
| [16-include](examples/16-include) | `!include` inlining files |
| [17-inherit](examples/17-inherit) | `!inherit` with deep-merge overrides |
| [18-include-file-barrier](examples/18-include-file-barrier) | File boundary isolates scope declarations |
| [20-include-local-in-array](examples/20-include-local-in-array) | Include resolves its own local sweeps before splicing |
| [21-include-plain-alongside-sweeps](examples/21-include-plain-alongside-sweeps) | Plain include spliced verbatim into each outer variant |

## License

BSD-3-Clause
