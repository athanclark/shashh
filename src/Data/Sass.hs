{-# LANGUAGE
    OverloadedStrings
  #-}

module Data.Sass where

import Data.String
import qualified Data.Text as T

import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import           Data.HashSet (HashSet)
import qualified Data.HashSet as HS



newtype Element = Element { getElement :: T.Text }
  deriving (Show, Eq, IsString)

newtype Ident = Ident { getIdent :: T.Text }
  deriving (Show, Eq, IsString)

newtype Class = Class { getClass :: T.Text }
  deriving (Show, Eq, IsString)

data Universal = Universal
  deriving (Show, Eq)

data Attributes
  = HasAttribute                 (HashSet T.Text)
  | HasAttributeValues           (HashMap T.Text T.Text)
  | HasAttributeContainingWord   (HashMap T.Text T.Text) -- ^ Space-separated
  | HasAttributeStartingWith     (HashMap T.Text T.Text)
  | HasAttributeStartingWithDash (HashMap T.Text T.Text)
  | HasAttributeEndingWith       (HashMap T.Text T.Text)
  | HasAttributeContaining       (HashMap T.Text T.Text)
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
  , specifierAttributes :: Attributes
  , specifierPseudo     :: Maybe (Either PseudoClass PseudoElement)
  } deriving (Show, Eq)


data Selector
  = Both                 Specifier Selector
  | Descendant           Specifier Selector
  | Child                Specifier Selector
  | HasImmediatelyBefore Specifier Selector
  | HasBefore            Specifier Selector
  | Nil
  deriving (Show, Eq)
