{-# LANGUAGE
    GeneralizedNewtypeDeriving
  , OverloadedStrings
  #-}

module Data.Css2.Rule where

import Data.String
import qualified Data.Text as T
import Data.Word
import Data.Char
import Data.Attoparsec.Text
import Text.Read (readMaybe)
import Control.Applicative
import Control.Monad


newtype Rule = Rule { getRule :: T.Text }
  deriving (Show, Eq, IsString)


data NumberUnit
  = Px
  | Em
  | Rem
  | In
  | Cm
  | Percent
  | OtherUnit T.Text
  deriving (Show, Eq)

parseNumberUnit :: Parser NumberUnit
parseNumberUnit =
      token "px" Px
  <|> token "em" Em
  <|> token "rem" Rem
  <|> token "in" In
  <|> token "cm" Cm
  <|> token "%" Percent
  <|> anyToken
  where
    token s s' = s' <$ string s
    anyToken = (OtherUnit . T.pack) <$> many1 (satisfy isToken')
    isToken' c = isAlphaNum c


data Value
  = Hex
      { hexRed   :: Word8
      , hexGreen :: Word8
      , hexBlue  :: Word8
      , hexAlpha :: Maybe Word8
      }
  | Rgb
      { rgbRed   :: Word8
      , rgbGreen :: Word8
      , rgbBlue  :: Word8
      , rgbAlpha :: Maybe Word8
      }
  | Hsl
      { hslRed   :: Word8
      , hslGreen :: Word8
      , hslBlue  :: Word8
      , hslAlpha :: Maybe Word8
      }
  | Number
      { numberScalar :: Double
      , numberUnit   :: Maybe NumberUnit
      }
  | Url
      { urlLocation :: T.Text
      }
  | Other T.Text
  deriving (Show, Eq)


--  foo : bar ; baz : qux
parseRules :: Parser [(Rule, Value)]
parseRules = flip sepBy1 (char ';') $ do
  skipSpace
  r <- parseRule
  skipSpace
  void $ char ':'
  skipSpace
  v <- parseValue
  skipSpace
  pure (r,v)



parseRule :: Parser Rule
parseRule = (Rule . T.pack) <$> many1 (satisfy isToken)

-- N.B. Shashh is NOT a DOM library / validator.
parseValue :: Parser Value
parseValue =
  parseHex <|> parseRgb <|> parseHsl <|> parseNumber <|> parseUrl <|> parseOther
  where
    parseHex :: Parser Value
    parseHex = do
      void (char '#')
      six <|> three
      where
        six = Hex <$> parseWord8 <*> parseWord8 <*> parseWord8 <*> parseMaybe parseWord8
        three = do
          r <- satisfy isHex
          g <- satisfy isHex
          b <- satisfy isHex
          a <- parseMaybe (satisfy isHex)
          pure $ Hex (fromChar r * 16)
                     (fromChar g * 16)
                     (fromChar b * 16)
                     (((*) 16 . fromChar) <$> a)

    parseRgb :: Parser Value
    parseRgb = noAlpha <|> withAlpha
      where
        noAlpha :: Parser Value
        noAlpha = do
          void $ string "rgb("
          skipSpace
          r <- parseWord8'
          skipSpace
          void $ char ','
          skipSpace
          g <- parseWord8'
          skipSpace
          void $ char ','
          skipSpace
          b <- parseWord8'
          skipSpace
          void $ char ')'
          pure $ Rgb r g b Nothing

        withAlpha :: Parser Value
        withAlpha = do
          void $ string "rgba("
          skipSpace
          r <- parseWord8'
          skipSpace
          void $ char ','
          skipSpace
          g <- parseWord8'
          skipSpace
          void $ char ','
          skipSpace
          b <- parseWord8'
          skipSpace
          void $ char ','
          skipSpace
          a <- parseWord8'
          skipSpace
          void $ char ')'
          pure $ Rgb r g b (Just a)

    parseHsl :: Parser Value
    parseHsl = noAlpha <|> withAlpha
      where
        noAlpha :: Parser Value
        noAlpha = do
          void $ string "hsl("
          skipSpace
          h <- parseWord8'
          skipSpace
          void $ char ','
          skipSpace
          s <- parseWord8'
          skipSpace
          void $ char ','
          skipSpace
          l <- parseWord8'
          skipSpace
          void $ char ')'
          pure $ Hsl h s l Nothing

        withAlpha :: Parser Value
        withAlpha = do
          void $ string "hsla("
          skipSpace
          h <- parseWord8'
          skipSpace
          void $ char ','
          skipSpace
          s <- parseWord8'
          skipSpace
          void $ char ','
          skipSpace
          l <- parseWord8'
          skipSpace
          void $ char ','
          skipSpace
          a <- parseWord8'
          skipSpace
          void $ char ')'
          pure $ Hsl h s l (Just a)

    parseNumber :: Parser Value
    parseNumber = Number <$> double <*> (skipSpace >> parseMaybe parseNumberUnit)

    parseUrl :: Parser Value
    parseUrl = do
      void $ string "url(" >> satisfy isQuote
      u <- (Url . T.pack) <$> many1 (satisfy isUrl)
      void $ satisfy isQuote >> char ')'
      pure u
      where
        isQuote :: Char -> Bool
        isQuote c = c == '\'' || c == '"'
        isUrl :: Char -> Bool
        isUrl c = isAscii c || not (isSpace c)

    parseOther :: Parser Value
    parseOther = (Other . T.pack) <$> many1 (satisfy isOther)
      where
        isOther c = isAlphaNum c

    -- 255
    parseWord8' :: Parser Word8
    parseWord8' = do
      xs <- many1 (satisfy isDigit)
      case xs of
        []         -> fail "empty Word8"
        [x]        -> pure $ read [x]
        [x,x']     -> pure $ read [x,x']
        [x,x',x''] -> case readMaybe [x,x',x''] of
                        Nothing -> fail "failed parse"
                        Just y  -> pure y
        _          -> fail "too many digits"

    -- ff
    parseWord8 :: Parser Word8
    parseWord8 = do
      [hx1, hx2] <- replicateM 2 (satisfy isHex)
      pure $ (fromChar hx1 * 16) + fromChar hx2

    fromChar :: Char -> Word8
    fromChar c
      | isDigit c = read [c]
      | toLower c `elem` ("abcdef" :: String) =
        case toLower c of
          'a' -> 10
          'b' -> 11
          'c' -> 12
          'd' -> 13
          'e' -> 14
          'f' -> 15
          _   -> error "not hexadecimal"
      | otherwise = error "not hexadecimal"

    isHex :: Char -> Bool
    isHex c = isDigit c || elem (toLower c) ("abcdef" :: String)


isToken :: Char -> Bool
isToken c =
     isAlphaNum c
  || c == '_'
  || c == '-'


parseMaybe :: Parser a -> Parser (Maybe a)
parseMaybe p = (Just <$> p) <|> pure Nothing

