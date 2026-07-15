# yaml-sweep

A YAML configuration engine with sweep expansion, file inclusion, and inheritance.

## Features

- **`!include`** — inline another YAML file at the tag position, with cycle detection
- **`!inherit`** — load a base file and deep-merge with local overrides (supports recursive chains)
- **`!prod [a, b, c]`** — expand cartesian-product sweep points across the config
- **`!zip_<name> [a, b, c]`** — index-linked sweeps: all markers with the same name advance together

## Quick Start

```haskell
import Yaml.Sweep (loadConfigValue)

main :: IO ()
main = do
  result <- loadConfigValue "config.yaml"
  case result of
    Left err -> putStrLn $ "Error: " <> err
    Right sweepResults -> do
      putStrLn $ "Generated " <> show (length sweepResults) <> " configurations"
      -- Each SweepResult has srValue (Aeson Value) and srName (derived short label)
```

### Example YAML

```yaml
traces:
  samples: !prod [1, 5, 10]
  max_rounds: !prod [5, 20]
```

This produces 6 configurations (3 × 2) with derived names like `samp1_maxR5`.

```yaml
attribution:
  - drop_threshold: !zip_threshold [0.5, 0.8, 0.9]
    drop_exponent:  !zip_exponent  [0.5, 0.8, 0.9]
```

This produces 3 configurations (index-linked, not 9).

### File Inclusion

```yaml
# config.yaml
traces:
  - !include base.yaml
```

### Inheritance with Deep-Merge

```yaml
# config.yaml
--- !inherit
base: base.yaml
traces:
  max_rounds: 20   # overrides base.yaml's max_rounds
```

## API

```haskell
data SweepResult = SweepResult
  { srValue :: Value  -- ^ Expanded Aeson Value
  , srName  :: Text   -- ^ Derived short label
  }

loadConfigValue :: FilePath -> IO (Either String [SweepResult])
```

## License

BSD-3-Clause
