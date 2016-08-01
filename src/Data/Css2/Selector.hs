{-# LANGUAGE
    OverloadedStrings
  , GeneralizedNewtypeDeriving
  #-}

module Data.Css2.Selector where

import Data.String
import qualified Data.Text as T
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import           Data.HashSet (HashSet)
import qualified Data.HashSet as HS

import Data.Char
import Data.Attoparsec.Text
import Control.Applicative
import Control.Monad (void)



newtype Element = Element { getElement :: T.Text }
  deriving (Show, Eq, IsString)

newtype Ident = Ident { getIdent :: T.Text }
  deriving (Show, Eq, IsString)

newtype Class = Class { getClass :: T.Text }
  deriving (Show, Eq, IsString)

data Universal = Universal
  deriving (Show, Eq)

data Attribute
  = HasAttribute                 T.Text
  | HasAttributeValue            T.Text T.Text
  | HasAttributeContainingWord   T.Text T.Text -- ^ Space-separated
  | HasAttributeStartingWith     T.Text T.Text
  | HasAttributeStartingWithDash T.Text T.Text
  | HasAttributeEndingWith       T.Text T.Text
  | HasAttributeContaining       T.Text T.Text
  deriving (Show, Eq)

-- TODO: Enumerate standard pseudoclasses and pseudoelements for optimization
newtype PseudoClass = PseudoClass { getPseudoClass :: T.Text }
  deriving (Show, Eq, IsString)

newtype PseudoElement = PseudoElement { getPseudoElement :: T.Text }
  deriving (Show, Eq, IsString)


-- div#foo.bar.baz[foo~=bar][foo^=bar]:after

data Specifier = Specifier
  { specifierNode       :: Maybe (Either Universal Element)
  , specifierIdent      :: Maybe Ident
  , specifierClasses    :: [Class]
  , specifierAttributes :: [Attribute]
  , specifierPseudo     :: Maybe (Either PseudoClass PseudoElement)
  } deriving (Show, Eq)


data Css2SelectorSingle
  = Descendant           Specifier Css2SelectorSingle
  | Child                Specifier Css2SelectorSingle
  | HasImmediatelyBefore Specifier Css2SelectorSingle
  | HasBefore            Specifier Css2SelectorSingle
  | Nil
  deriving (Show, Eq)


newtype Css2Selector = Css2Selector { getCss2Selector :: [Css2SelectorSingle] }
  deriving (Show, Eq)


parseCss2Selector :: Parser Css2Selector
parseCss2Selector = Css2Selector <$> sepBy parseCss2SelectorSingle (char ',')


parseCss2SelectorSingle :: Parser Css2SelectorSingle
parseCss2SelectorSingle = do
  skipSpace
  ms <- parseMaybe parseSpecifier
  case ms of
    Nothing -> pure Nil
    Just s  -> do
      f <- do
        skipSpace
        let parseChild = do
              void $ string ">"
              skipSpace
              pure $ Child s
            parseImmediatelyBefore = do
              void $ string "+"
              skipSpace
              pure $ HasImmediatelyBefore s
            parseBefore = do
              void $ string "~"
              skipSpace
              pure $ HasBefore s
            parseDescendant =
              pure $ Descendant s
        parseChild <|> parseImmediatelyBefore <|> parseBefore <|> parseDescendant
      f <$> parseCss2SelectorSingle


parseSpecifier :: Parser Specifier
parseSpecifier = do
  n  <- parseMaybe $ eitherP parseUniversal parseElement
  i  <- parseMaybe parseIdent
  cs <- many parseClass
  as <- many parseAttribute
  p  <- parseMaybe $ eitherP parsePseudoClass parsePseudoElement
  case (n,i,cs,as,p) of
    (Nothing, Nothing, [], [], Nothing) -> fail "No specifier"
    _ -> pure $ Specifier n i cs as p
  where
    parseElement       = Element <$> parseToken
    parseIdent         = Ident   <$> (char '#' >> parseToken)
    parseClass         = Class   <$> (char '.' >> parseToken)
    parseUniversal     = Universal <$ char '*'
    parsePseudoClass   = PseudoClass   <$> (char ':'    >> parsePseudoToken)
    parsePseudoElement = PseudoElement <$> (string "::" >> parsePseudoToken)
    parseAttribute = do
      void $ char '['
      a <-  parseAttributeContainingWord
        <|> parseAttributeStartingWith
        <|> parseAttributeStartingWithDash
        <|> parseAttributeEndingWith
        <|> parseAttributeContaining
        <|> parseAttributeValue
        <|> parseAttributeRaw
      void $ char ']'
      pure a
      where
        parseAttributeRaw =
          HasAttribute <$> parseToken
        parseAttributeRelation :: T.Text -> Parser (T.Text, T.Text)
        parseAttributeRelation r = do
          k <- parseToken
          void $ string r
          v <- parseToken
          pure (k,v)
        parseAttributeValue =
          (uncurry HasAttributeValue)            <$> parseAttributeRelation "="
        parseAttributeContainingWord =
          (uncurry HasAttributeContainingWord)   <$> parseAttributeRelation "~="
        parseAttributeStartingWith =
          (uncurry HasAttributeStartingWith)     <$> parseAttributeRelation "^="
        parseAttributeStartingWithDash =
          (uncurry HasAttributeStartingWithDash) <$> parseAttributeRelation "|="
        parseAttributeEndingWith =
          (uncurry HasAttributeEndingWith)       <$> parseAttributeRelation "$="
        parseAttributeContaining =
          (uncurry HasAttributeContaining)       <$> parseAttributeRelation "*="


parseToken :: Parser T.Text
parseToken = T.pack <$> many1 (satisfy isToken)
  where
    isToken :: Char -> Bool
    isToken c =
         isAlphaNum c
      || c == '_'
      || c == '-'


parsePseudoToken :: Parser T.Text
parsePseudoToken = T.pack <$> many1 (satisfy isPseudoToken)
  where
    isPseudoToken :: Char -> Bool
    isPseudoToken c =
         isAlphaNum c
      || c == '_'
      || c == '-'
      || c == '('
      || c == ')'


parseMaybe :: Parser a -> Parser (Maybe a)
parseMaybe p =
  (Just <$> p) <|> pure Nothing
