module Main (main) where

import Yaml.Sweep
import Data.Aeson
import Data.Text qualified as T
import Data.Yaml qualified as Yaml
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec
import Data.Text.Encoding qualified as TE

main :: IO ()
main = hspec spec

parseYaml :: Text -> IO Value
parseYaml t = case Yaml.decodeEither' (TE.encodeUtf8 t) of
  Left err -> expectationFailure ("Invalid expected YAML: " <> show err) >> return Null
  Right v -> return v

checkYaml :: Text -> [Text] -> Expectation
checkYaml input expecteds =
  withSystemTempDirectory "config-test" $ \dir -> do
    let path = dir </> "config.yaml"
    writeFile path (T.unpack input)
    result <- loadConfigValue path
    case result of
      Left err -> expectationFailure $ "Expected success, got error: " <> err
      Right actualResults -> do
        let actualValues = map srValue actualResults
        expectedValues <- mapM parseYaml expecteds
        actualValues `shouldMatchList` expectedValues

checkYamlError :: Text -> String -> Expectation
checkYamlError input expectedError =
  withSystemTempDirectory "config-test" $ \dir -> do
    let path = dir </> "config.yaml"
    writeFile path (T.unpack input)
    result <- loadConfigValue path
    case result of
      Right _ -> expectationFailure "Expected error, got success"
      Left err -> err `shouldContain` expectedError

spec :: Spec
spec = do
  describe "loadConfigValue + expandExpr" $ do
    
    it "parses simple YAML without sweeps" $
      checkYaml
        (T.unlines
          [ "traces:"
          , "  mode: post_hoc"
          , "  samples: 1"
          ])
        [ T.unlines
          [ "traces:"
          , "  mode: post_hoc"
          , "  samples: 1"
          ]
        ]

    it "expands !prod into multiple results" $
      checkYaml
        (T.unlines
          [ "traces:"
          , "  samples: !prod [1, 3]"
          ])
        [ T.unlines [ "traces:\n  samples: 1" ]
        , T.unlines [ "traces:\n  samples: 3" ]
        ]

    it "expands multiple !prod as cartesian product" $
      checkYaml
        (T.unlines
          [ "traces:"
          , "  samples: !prod [1, 2]"
          , "  max_rounds: !prod [5, 10]"
          ])
        [ T.unlines [ "traces:\n  samples: 1\n  max_rounds: 5" ]
        , T.unlines [ "traces:\n  samples: 1\n  max_rounds: 10" ]
        , T.unlines [ "traces:\n  samples: 2\n  max_rounds: 5" ]
        , T.unlines [ "traces:\n  samples: 2\n  max_rounds: 10" ]
        ]

    it "!zip with !key is cross-element (config-level)" $
      checkYaml
        (T.unlines
          [ "!key th:"
          , "attribution:"
          , "  - drop_threshold: !zip_th [0.5, 0.8]"
          , "  - drop_threshold: !zip_th [0.5, 0.8]"
          ])
        [ T.unlines [ "attribution:\n  - drop_threshold: 0.5\n  - drop_threshold: 0.5" ]
        , T.unlines [ "attribution:\n  - drop_threshold: 0.8\n  - drop_threshold: 0.8" ]
        ]

    it "!zip without !key is local (element-level, sum not product)" $
      checkYaml
        (T.unlines
          [ "attribution:"
          , "  - drop_threshold: !zip_th [0.5, 0.8]"
          , "  - drop_threshold: !zip_th [0.5, 0.8]"
          ])
        [ T.unlines 
          [ "attribution:"
          , "  - drop_threshold: 0.5"
          , "  - drop_threshold: 0.8"
          , "  - drop_threshold: 0.5"
          , "  - drop_threshold: 0.8"
          ]
        ]

    it "!zip with !key links same-name zips across elements" $
      checkYaml
        (T.unlines
          [ "!key th:"
          , "attribution:"
          , "  - drop_threshold: !zip_th [0.5, 0.8]"
          , "  - drop_threshold: !zip_th [0.5, 0.8]"
          ])
        [ T.unlines [ "attribution:\n  - drop_threshold: 0.5\n  - drop_threshold: 0.5" ]
        , T.unlines [ "attribution:\n  - drop_threshold: 0.8\n  - drop_threshold: 0.8" ]
        ]

    it "!zip in same element are linked (implicit local scope)" $
      checkYaml
        (T.unlines
          [ "attribution:"
          , "  - a: !zip_x [1, 2]"
          , "    b: !zip_x [10, 20]"
          ])
        [ T.unlines 
          [ "attribution:"
          , "  - a: 1"
          , "    b: 10"
          , "  - a: 2"
          , "    b: 20"
          ]
        ]

    it "different lengths OK for local scopes in different elements" $
      checkYaml
        (T.unlines
          [ "attribution:"
          , "  - t: !zip_t [0.5, 0.8]"
          , "  - t: !zip_t [0.5, 0.8, 0.9]"
          ])
        [ T.unlines 
          [ "attribution:"
          , "  - t: 0.5"
          , "  - t: 0.8"
          , "  - t: 0.5"
          , "  - t: 0.8"
          , "  - t: 0.9"
          ]
        ]

    it "!key with different lengths across elements is an error" $
      checkYamlError
        (T.unlines
          [ "!key lr:"
          , "experiments:"
          , "  - lr: !zip_lr [0.1, 0.01, 0.001]"
          , "  - lr: !zip_lr [0.1, 0.01]"
          ])
        "inconsistent lengths"

    it "!key shadowing is an error" $
      checkYamlError
        (T.unlines
          [ "!key a:"
          , "list:"
          , "  - !key a:"
          , "    key1: !zip_a [1, 2]"
          ])
        "shadowing"

    it "mixed global+local scopes with different field order across elements" $
      checkYaml
        (T.unlines
          [ "!key a:"
          , "items:"
          , "  - y: !zip_a [1, 2]"
          , "    x: !zip_b [10, 20]"
          , "  - x: !zip_b [30, 40]"
          , "    y: !zip_a [1, 2]"
          ])
        [ T.unlines
          [ "items:"
          , "  - y: 1"
          , "    x: 10"
          , "  - y: 1"
          , "    x: 20"
          , "  - x: 30"
          , "    y: 1"
          , "  - x: 40"
          , "    y: 1"
          ]
        , T.unlines
          [ "items:"
          , "  - y: 2"
          , "    x: 10"
          , "  - y: 2"
          , "    x: 20"
          , "  - x: 30"
          , "    y: 2"
          , "  - x: 40"
          , "    y: 2"
          ]
        ]

    it "BFS: zip in nested field links to sibling zip in same mapping" $
      checkYaml
        (T.unlines
          [ "list1:"
          , "  - list2:"
          , "      - key21: !zip_a [1, 2]"
          , "    key1: !zip_a [1, 2]"
          ])
        [ T.unlines
          [ "list1:"
          , "  - key1: 1"
          , "    list2:"
          , "      - key21: 1"
          , "  - key1: 2"
          , "    list2:"
          , "      - key21: 2"
          ]
        ]

    it "nested BFS with two !key scopes and position-aware array merge" $
      checkYaml
        (T.unlines
          [ "!key A:"
          , "list1:"
          , "  - dict2:"
          , "      !key B:"
          , "      list3:"
          , "        - a1: !zip_A [11, 12]"
          , "        - a2: !zip_A [21, 22]"
          , "          b2: !zip_B [28, 29]"
          , "        - b3: !zip_B [38, 39]"
          ])
        [ T.unlines
          [ "list1:"
          , "  - dict2:"
          , "      list3:"
          , "        - a1: 11"
          , "        - a2: 21"
          , "          b2: 28"
          , "        - b3: 38"
          , "  - dict2:"
          , "      list3:"
          , "        - a1: 11"
          , "        - a2: 21"
          , "          b2: 29"
          , "        - b3: 39"
          ]
        , T.unlines
          [ "list1:"
          , "  - dict2:"
          , "      list3:"
          , "        - a1: 12"
          , "        - a2: 22"
          , "          b2: 28"
          , "        - b3: 38"
          , "  - dict2:"
          , "      list3:"
          , "        - a1: 12"
          , "        - a2: 22"
          , "          b2: 29"
          , "        - b3: 39"
          ]
        ]

    it "nested !key scopes: outer synchronizes across elements, inner expands locally" $
      checkYaml
        (T.unlines
          [ "!key model:"
          , "experiments:"
          , "  - model: !zip_model [gpt4, claude]"
          , "    temp: !zip_model [0.0, 0.7]"
          , "    !key eval:"
          , "    benchmarks:"
          , "      - name: !zip_eval [mmlu, gsm8k, humaneval]"
          , "        metric: !zip_eval [accuracy, exact_match, pass_at_1]"
          , "      - shots: !zip_eval [5, 8, 0]"
          , "  - model: !zip_model [gpt4, claude]"
          , "    endpoint: !zip_model [/v1/chat, /v1/messages]"
          , "    task: safety_eval"
          , "    max_tokens: 512"
          ])
        [ T.unlines
          [ "experiments:"
          , "  - model: gpt4"
          , "    temp: 0.0"
          , "    benchmarks:"
          , "      - name: mmlu"
          , "        metric: accuracy"
          , "      - shots: 5"
          , "  - model: gpt4"
          , "    temp: 0.0"
          , "    benchmarks:"
          , "      - name: gsm8k"
          , "        metric: exact_match"
          , "      - shots: 8"
          , "  - model: gpt4"
          , "    temp: 0.0"
          , "    benchmarks:"
          , "      - name: humaneval"
          , "        metric: pass_at_1"
          , "      - shots: 0"
          , "  - model: gpt4"
          , "    endpoint: /v1/chat"
          , "    task: safety_eval"
          , "    max_tokens: 512"
          ]
        , T.unlines
          [ "experiments:"
          , "  - model: claude"
          , "    temp: 0.7"
          , "    benchmarks:"
          , "      - name: mmlu"
          , "        metric: accuracy"
          , "      - shots: 5"
          , "  - model: claude"
          , "    temp: 0.7"
          , "    benchmarks:"
          , "      - name: gsm8k"
          , "        metric: exact_match"
          , "      - shots: 8"
          , "  - model: claude"
          , "    temp: 0.7"
          , "    benchmarks:"
          , "      - name: humaneval"
          , "        metric: pass_at_1"
          , "      - shots: 0"
          , "  - model: claude"
          , "    endpoint: /v1/messages"
          , "    task: safety_eval"
          , "    max_tokens: 512"
          ]
        ]

  describe "!include & !inherit behavior checks" $ do
    it "resolves !include correctly" $
      withSystemTempDirectory "config-test" $ \dir -> do
        writeFile (dir </> "base.yaml") "mode: post_hoc\nsamples: 1\n"
        writeFile (dir </> "config.yaml") $ T.unpack $ T.unlines
          [ "traces:"
          , "  - !include base.yaml"
          ]
        result <- loadConfigValue (dir </> "config.yaml")
        case result of
          Left err -> expectationFailure err
          Right actual -> do
            let actualValues = map srValue actual
            expectedValues <- mapM parseYaml [T.unlines ["traces:\n  - mode: post_hoc\n    samples: 1"]]
            actualValues `shouldMatchList` expectedValues

    it "resolves !inherit correctly" $
      withSystemTempDirectory "config-test" $ \dir -> do
        writeFile (dir </> "base.yaml") "traces:\n  mode: post_hoc\n  samples: 1\n"
        writeFile (dir </> "config.yaml") $ T.unpack $ T.unlines
          [ "--- !inherit"
          , "base: base.yaml"
          , "traces:"
          , "  samples: 2"
          ]
        result <- loadConfigValue (dir </> "config.yaml")
        case result of
          Left err -> expectationFailure err
          Right actual -> do
            let actualValues = map srValue actual
            expectedValues <- mapM parseYaml [T.unlines ["traces:\n  mode: post_hoc\n  samples: 2"]]
            actualValues `shouldMatchList` expectedValues

    it "!include: outer !key scope does not leak into included file" $
      withSystemTempDirectory "config-test" $ \dir -> do
        writeFile (dir </> "inner.yaml") "val: !zip_a [1, 2]\n"
        writeFile (dir </> "config.yaml") $ T.unpack $ T.unlines
          [ "!key a:"
          , "outer: !zip_a [10, 20]"
          , "items:"
          , "  - !include inner.yaml"
          ]
        result <- loadConfigValue (dir </> "config.yaml")
        case result of
          Right _ -> expectationFailure "Expected file barrier error, got success"
          Left err -> err `shouldContain` "Zip over zip boundary"

    it "!include: sweep in included file is blocked by file barrier" $
      withSystemTempDirectory "config-test" $ \dir -> do
        writeFile (dir </> "inner.yaml") $ T.unpack $ T.unlines
          [ "!key b:"
          , "x: !zip_b [1, 2]"
          , "y: !zip_b [10, 20]"
          ]
        writeFile (dir </> "config.yaml") $ T.unpack $ T.unlines
          [ "items:"
          , "  - !include inner.yaml"
          ]
        result <- loadConfigValue (dir </> "config.yaml")
        case result of
          Right _ -> expectationFailure "Expected file barrier error, got success"
          Left err -> err `shouldContain` "Zip over zip boundary"

    it "!include: local sweep inside array resolves within file barrier" $
      withSystemTempDirectory "config-test" $ \dir -> do
        writeFile (dir </> "inner.yaml") $ T.unpack $ T.unlines
          [ "items:"
          , "  - val: !zip_x [1, 2]"
          ]
        writeFile (dir </> "config.yaml") $ T.unpack $ T.unlines
          [ "outer: hello"
          , "data:"
          , "  - !include inner.yaml"
          ]
        result <- loadConfigValue (dir </> "config.yaml")
        case result of
          Left err -> expectationFailure err
          Right actual -> do
            let actualValues = map srValue actual
            expectedValues <- mapM parseYaml
              [ T.unlines ["outer: hello\ndata:\n  - items:\n      - val: 1\n      - val: 2"]
              ]
            actualValues `shouldMatchList` expectedValues

    it "!include: plain values work alongside outer sweeps" $
      withSystemTempDirectory "config-test" $ \dir -> do
        writeFile (dir </> "inner.yaml") "val: fixed\n"
        writeFile (dir </> "config.yaml") $ T.unpack $ T.unlines
          [ "!key a:"
          , "x: !zip_a [1, 2]"
          , "items:"
          , "  - !include inner.yaml"
          ]
        result <- loadConfigValue (dir </> "config.yaml")
        case result of
          Left err -> expectationFailure err
          Right actual -> do
            let actualValues = map srValue actual
            expectedValues <- mapM parseYaml
              [ T.unlines ["x: 1\nitems:\n  - val: fixed"]
              , T.unlines ["x: 2\nitems:\n  - val: fixed"]
              ]
            actualValues `shouldMatchList` expectedValues
