module Teem.Parser
  (readNrrdHeader
  , Value (..)
  , Result (..) -- export Trifecta result type
  ) where

import           Control.Applicative
import           Control.Monad       (void)
import           Data.Char           (toLower)
import qualified Data.Map            as M
import           Text.Trifecta

data Space = LPS | RPS | RAS | LAS
  deriving (Show, Eq)

type Tuple3 = (Double, Double, Double)

data SpaceDirections
  = StructuralSpace Tuple3 Tuple3 Tuple3
  | DWISpace Tuple3 Tuple3 Tuple3
  deriving (Eq, Show)

type Key = String

data Value
  = VDataType String
  | VDimension Integer
  | VSpace Space
  | VSizes [Integer]
  | VSpaceDirections SpaceDirections
  | VKinds [String]
  | VEndian String
  | VEncoding String
  | VSpaceOrigin Tuple3
  | VGradientDirection Tuple3
  | VDefault String
  deriving (Show, Eq)

type KVPs = M.Map Key Value

skipComments :: Parser [String]
skipComments = many (char '#' *> manyTill anyChar newline)

skipNrrdMagic :: Parser ()
skipNrrdMagic = void $ string "NRRD000" *> anyChar *> newline

eol :: Parser ()
eol = choice $ map (try . (spaces' *>)) [void newline, eof]
  where
    spaces' = many $ char ' '

parseDouble :: Parser Double
parseDouble = toDbl <$> integerOrDouble
      where
        toDbl (Left i) = fromIntegral i
        toDbl (Right d) = d

parseTuple3 :: Parser Tuple3
parseTuple3 = (,,)
  <$> (bo *> parseDouble <* sep)
  <*> (parseDouble <* sep)
  <*> (parseDouble <* bc)
  where
    bo = token $ char '('
    bc = token $ char ')'
    sep = token $ char ','

parseGradientDirection :: Parser Tuple3
parseGradientDirection = (,,)
                         <$> (parseDouble <* spaces)
                         <*> (parseDouble <* spaces)
                         <*> (parseDouble <* spaces)

parseSpaceDirections :: Parser SpaceDirections
parseSpaceDirections = do
  choice [ try parseDWI, try parseStrct ]
  where
     parseStrct = StructuralSpace <$> parseTuple3 <*> parseTuple3 <*> parseTuple3
     parseDWI = DWISpace <$> parseTuple3 <*> parseTuple3 <*>
      (parseTuple3 <* string "none" <* spaces)

readSpace :: String -> Space
readSpace s = case (map toLower s) of
  "lps" -> LPS
  "left-posterior-superior" -> LPS
  "rps" -> RPS
  "right-anterior-superior" -> RAS
  "las" -> LAS
  "left-anterior-posterior" -> LAS
  _ -> error "Invalid space"

parseKey :: Parser String
parseKey = try twoWords <|> try oneWord
  where
    oneWord = some (alphaNum <|> char '_' <|> char '-')
    twoWords = do
          w1 <- oneWord
          char ' '
          w2 <- oneWord
          return $ w1++" "++w2

parseKVP :: Parser (Key, Value)
parseKVP = do
  key <- parseKey
  _ <- token $ char ':'
  skipOptional $ token $ char '='
  let parseStr = manyTill anyChar eol
  val <- case key of
           "type" -> VDataType <$> parseStr
           "dimension" -> VDimension <$> natural
           "space" -> VSpace . readSpace <$> parseStr
           "sizes" -> VSizes <$> some natural
           "space directions" -> VSpaceDirections <$> parseSpaceDirections
           "kinds" -> VKinds . words . unwords . words <$> parseStr
           "endian" -> VEndian <$> parseStr
           "encoding" -> VEncoding <$> parseStr
           "space origin" -> VSpaceOrigin <$> parseTuple3
           _ -> if take 14 key == "DWMRI_gradient"
             then VGradientDirection <$> parseGradientDirection
             else VDefault <$> parseStr
  return (key, val)

parseHeader :: Parser KVPs
parseHeader = fmap M.fromList $ skipNrrdMagic *> some (skipComments *> parseKVP)

readNrrdHeader :: FilePath -> IO (Result KVPs)
readNrrdHeader nrrd = parseFromFileEx parseHeader nrrd
