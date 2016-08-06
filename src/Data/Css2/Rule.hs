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
      Hex <$> parseWord8 <*> parseWord8 <*> parseWord8 <*> parseMaybe parseWord8

    parseRgb :: Parser Value
    parseRgb = noAlpha <|> withAlpha
      where
        noAlpha :: Parser Value
        noAlpha = do
          void $ string "rgb("
          r <- parseWord8'
          void $ char ','
          g <- parseWord8'
          void $ char ','
          b <- parseWord8'
          void $ char ')'
          pure $ Rgb r g b Nothing

        withAlpha :: Parser Value
        withAlpha = do
          void $ string "rgba("
          r <- parseWord8'
          void $ char ','
          g <- parseWord8'
          void $ char ','
          b <- parseWord8'
          void $ char ','
          a <- parseWord8'
          void $ char ')'
          pure $ Rgb r g b (Just a)

    parseHsl :: Parser Value
    parseHsl = noAlpha <|> withAlpha
      where
        noAlpha :: Parser Value
        noAlpha = do
          void $ string "hsl("
          h <- parseWord8'
          void $ char ','
          s <- parseWord8'
          void $ char ','
          l <- parseWord8'
          void $ char ')'
          pure $ Hsl h s l Nothing

        withAlpha :: Parser Value
        withAlpha = do
          void $ string "hsla("
          h <- parseWord8'
          void $ char ','
          s <- parseWord8'
          void $ char ','
          l <- parseWord8'
          void $ char ','
          a <- parseWord8'
          void $ char ')'
          pure $ Hsl h s l (Just a)

    parseNumber :: Parser Value
    parseNumber = Number <$> double <*> parseMaybe parseNumberUnit

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
      where
        -- FIXME: use a hex parser dummy
        fromChar :: Num a => Char -> a
        fromChar c | c == '0' = 0
                   | c == '1' = 1
                   | c == '2' = 2
                   | c == '3' = 3
                   | c == '4' = 4
                   | c == '5' = 5
                   | c == '6' = 6
                   | c == '7' = 7
                   | c == '8' = 8
                   | c == '9' = 9
                   | toLower c == 'a' = 10
                   | toLower c == 'b' = 11
                   | toLower c == 'c' = 12
                   | toLower c == 'd' = 13
                   | toLower c == 'e' = 14
                   | toLower c == 'f' = 15


    isHex :: Char -> Bool
    isHex c = isDigit c || elem (toLower c) ("abcdef" :: String)


isToken :: Char -> Bool
isToken c =
     isAlphaNum c
  || c == '_'
  || c == '-'


parseMaybe :: Parser a -> Parser (Maybe a)
parseMaybe p = (Just <$> p) <|> pure Nothing

