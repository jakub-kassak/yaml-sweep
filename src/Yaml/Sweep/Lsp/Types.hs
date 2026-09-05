module Yaml.Sweep.Lsp.Types
  ( SrcSpan (..),
    SrcLoc (..),
    JumpTarget (..),
    JumpMap,
    InheritFieldDiff (..),
    InheritDiff (..),
    spanFromPos,
    spanContainsPos,
    posToLspPosition,
    lspPositionToPos,
    spanToLspRange,
    findTagSpanOnLine,
    locToLspLocation,
    lineAt,
  )
where

import Data.Char (isAlphaNum)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.YAML (Pos (..))
import Language.LSP.Protocol.Types
  ( Location (..),
    Position (..),
    Range (..),
    filePathToUri,
  )
import Yaml.Sweep.Types (Path)

data SrcSpan = SrcSpan
  { spanStartLine :: Int,
    spanStartCol :: Int,
    spanEndLine :: Int,
    spanEndCol :: Int
  }
  deriving (Show, Eq, Ord)

data SrcLoc = SrcLoc
  { locFile :: FilePath,
    locSpan :: SrcSpan
  }
  deriving (Show, Eq, Ord)

data JumpTarget = JumpTarget
  { jtTargetFile :: FilePath,
    jtTargetSpan :: SrcSpan
  }
  deriving (Show, Eq)

type JumpMap = Map.Map (FilePath, SrcSpan) JumpTarget

data InheritFieldDiff
  = FieldOverridden
      { ifdPath :: Path,
        ifdBaseFile :: FilePath,
        ifdBasePos :: Pos,
        ifdBaseVal :: Text,
        ifdOverrideFile :: FilePath,
        ifdOverridePos :: Pos,
        ifdOverrideValuePos :: Pos,
        ifdOverrideVal :: Text
      }
  | FieldAdded
      { ifdPath :: Path,
        ifdOverrideFile :: FilePath,
        ifdOverridePos :: Pos,
        ifdOverrideValuePos :: Pos,
        ifdOverrideVal :: Text
      }
  | FieldInherited
      { ifdPath :: Path,
        ifdBaseFile :: FilePath,
        ifdBasePos :: Pos,
        ifdBaseVal :: Text,
        ifdOverrideFile :: FilePath
      }
  deriving (Show, Eq)

data InheritDiff = InheritDiff
  { idBasePath :: FilePath,
    idBaseFile :: FilePath,
    idOverrideFile :: FilePath,
    idInheritPos :: Pos,
    idBaseKeyPos :: Maybe Pos,
    idBaseValuePos :: Maybe Pos,
    idFields :: Map Path InheritFieldDiff
  }
  deriving (Show, Eq)

spanFromPos :: Pos -> Text -> SrcSpan
spanFromPos Pos {..} text = SrcSpan {..}
  where
    spanStartLine = posLine
    spanStartCol = posColumn
    lineBreaks = T.count "\n" text
    spanEndLine = spanStartLine + lineBreaks
    spanEndCol
      | lineBreaks == 0 = spanStartCol + T.length text
      | otherwise = T.length (T.takeWhileEnd (/= '\n') text)

spanContainsPos :: SrcSpan -> Pos -> Bool
spanContainsPos SrcSpan {..} Pos {..}
  | posLine < spanStartLine || posLine > spanEndLine = False
  | posLine == spanStartLine && posLine == spanEndLine =
      posColumn >= spanStartCol && posColumn < spanEndCol
  | posLine == spanStartLine = posColumn >= spanStartCol
  | posLine == spanEndLine = posColumn < spanEndCol
  | otherwise = True

posToLspPosition :: Text -> Pos -> Position
posToLspPosition source Pos {..} = sourcePosition source posLine posColumn

lspPositionToPos :: Text -> Position -> Pos
lspPositionToPos source Position {..} = Pos {..}
  where
    posLine = fromIntegral _line + 1
    posColumn = charOffsetFromUtf16 line (fromIntegral _character)
    posByteOffset = 0
    posCharOffset = 0
    line = fromMaybe "" (lineAt source posLine)

charOffsetFromUtf16 :: Text -> Int -> Int
charOffsetFromUtf16 line targetUnits = go 0 0 (T.unpack line)
  where
    go col _ [] = col
    go col units (char : rest)
      | units >= targetUnits = col
      | otherwise =
          let charUnits = if ord char > 0xFFFF then 2 else 1
           in go (col + 1) (units + charUnits) rest

spanToLspRange :: Text -> SrcSpan -> Range
spanToLspRange source SrcSpan {..} = Range {..}
  where
    _start = sourcePosition source spanStartLine spanStartCol
    _end = sourcePosition source spanEndLine spanEndCol

findTagSpanOnLine :: Text -> Int -> Text -> Maybe SrcSpan
findTagSpanOnLine source lineNumber target = do
  line <- lineAt source lineNumber
  findAt 0 Nothing Nothing False line Nothing
  where
    targetWords = T.words target
    findAt column previous quote escaped remaining found = case T.uncons remaining of
      Nothing -> found
      Just (char, rest)
        | Just activeQuote <- quote ->
            if
              | activeQuote == '"' && escaped -> findAt (column + 1) (Just char) quote False rest found
              | activeQuote == '"' && char == '\\' -> findAt (column + 1) (Just char) quote True rest found
              | activeQuote == '\'' && char == '\'' -> case T.uncons rest of
                  Just ('\'', afterEscape) -> findAt (column + 2) (Just char) quote False afterEscape found
                  _ -> findAt (column + 1) (Just char) Nothing False rest found
              | char == activeQuote -> findAt (column + 1) (Just char) Nothing False rest found
              | otherwise -> findAt (column + 1) (Just char) quote False rest found
        | char == '#' && isCommentStart previous -> found
        | (char == '"' || char == '\'') && validTokenStart previous -> findAt (column + 1) (Just char) (Just char) False rest found
        | otherwise ->
            let nextFound = case matchTarget targetWords remaining of
                  Just width
                    | validTokenStart previous -> Just (singleLineSpan lineNumber column (column + width))
                  _ -> found
             in findAt (column + 1) (Just char) Nothing False rest nextFound

locToLspLocation :: Text -> SrcLoc -> Location
locToLspLocation source SrcLoc {..} = Location {..}
  where
    _uri = filePathToUri locFile
    _range = spanToLspRange source locSpan

sourcePosition :: Text -> Int -> Int -> Position
sourcePosition source lineNumber column = Position {..}
  where
    _line = fromIntegral (max 0 (lineNumber - 1))
    _character = fromIntegral (utf16Length (T.take (max 0 column) line))
    line = fromMaybe "" (lineAt source lineNumber)

utf16Length :: Text -> Int
utf16Length = T.foldl' addCodeUnits 0
  where
    addCodeUnits total char
      | ord char > 0xFFFF = total + 2
      | otherwise = total + 1

lineAt :: Text -> Int -> Maybe Text
lineAt source lineNumber
  | lineNumber < 1 = Nothing
  | otherwise = listToMaybe (drop (lineNumber - 1) (T.splitOn "\n" source))

matchTarget :: [Text] -> Text -> Maybe Int
matchTarget [] _ = Nothing
matchTarget wordsToMatch source = do
  width <- matchWords wordsToMatch source
  guard (hasTokenBoundary (T.drop width source))
  pure width

matchWords :: [Text] -> Text -> Maybe Int
matchWords [] _ = Just 0
matchWords [word] source = T.stripPrefix word source $> T.length word
matchWords (word : rest) source = do
  suffix <- T.stripPrefix word source
  let spacing = T.takeWhile isHorizontalSpace suffix
  guard (not (T.null spacing))
  restWidth <- matchWords rest (T.drop (T.length spacing) suffix)
  pure (T.length word + T.length spacing + restWidth)

hasTokenBoundary :: Text -> Bool
hasTokenBoundary source = case T.uncons source of
  Nothing -> True
  Just (char, _) -> not (isAlphaNum char || char == '_' || char == '-')

isHorizontalSpace :: Char -> Bool
isHorizontalSpace char = char == ' ' || char == '\t'

validTokenStart :: Maybe Char -> Bool
validTokenStart Nothing = True
validTokenStart (Just char) = isHorizontalSpace char || char `elem` (":,-[{}]?" :: String)

isCommentStart :: Maybe Char -> Bool
isCommentStart Nothing = True
isCommentStart (Just char) = isHorizontalSpace char

singleLineSpan :: Int -> Int -> Int -> SrcSpan
singleLineSpan spanStartLine spanStartCol spanEndCol = SrcSpan {..}
  where
    spanEndLine = spanStartLine
