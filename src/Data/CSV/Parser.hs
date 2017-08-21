{-# LANGUAGE ScopedTypeVariables #-}

module Data.CSV.Parser where

import           Control.Applicative     (Alternative, (<|>), liftA3, optional)
import           Data.CharSet            (CharSet)
import qualified Data.CharSet as CharSet (fromList, insert)
import           Data.List.NonEmpty      (NonEmpty ((:|)), some1)
import           Data.Functor            (void, ($>), (<$>))
import           Data.Separated          (pesaratedBy)
import           Text.Parser.Char        (CharParsing, char, notChar, noneOfSet, oneOfSet, string)
import           Text.Parser.Combinators (between, choice, eof, many, notFollowedBy, option, sepEndBy, skipOptional, try)

import           Data.CSV.CSV            (CSV (CSV), FinalRecord (FinalRecord), Records (Records))
import           Data.CSV.Field          (Field (UnquotedF, QuotedF), MonoField (MonoField))
import           Data.CSV.Record         (NonEmptyRecord (MultiFieldNER, SingleFieldNER), Record (Record, fields))
import           Data.NonEmptyString     (NonEmptyString)
import           Text.Between            (Between (Between))
import           Text.Newline            (Newline (CR, CRLF, LF))
import           Text.Quote              (Escaped (SeparatedByEscapes), Quote (SingleQuote, DoubleQuote), Quoted (Quoted), quoteChar)

singleQuote, doubleQuote, backslash, comma, pipe, tab :: Char
singleQuote = '\''
doubleQuote = '"'
backslash = '\\'
comma = ','
pipe = '|'
tab = '\t'

sepByNonEmpty :: Alternative m => m a -> m sep -> m (NonEmpty a)
sepByNonEmpty p sep = (:|) <$> p <*> many (sep *> p)

sepEndByNonEmpty :: Alternative m => m a -> m sep -> m (NonEmpty a)
sepEndByNonEmpty p sep = (:|) <$> p <*> ((sep *> sepEndBy p sep) <|> pure [])

singleQuotedField, doubleQuotedField :: CharParsing m => m (Field String a String)
singleQuotedField = quotedField SingleQuote
doubleQuotedField = quotedField DoubleQuote

quoted :: CharParsing m => Quote -> m (Escaped a) -> m (Quoted a)
quoted q p =
  let c = char (quoteChar q)
  in  (fmap (Quoted q)) (between c c p)

quotedField :: CharParsing m => Quote -> m (Field String a String)
quotedField quote =
  let qc = quoteChar quote
      escape = escapeQuote quote
      noescape = many (notChar qc) `sepByNonEmpty` escape
  in  QuotedF <$> spaced (quoted quote (SeparatedByEscapes <$> noescape))

escapeQuote :: CharParsing m => Quote -> m Char
escapeQuote q =
  let c = quoteChar q
  in  try (string (two c)) $> c

two :: a -> [a]
two a = [a,a]

unquotedField :: CharParsing m => Char -> (m Char -> m (f Char)) -> m (Field a (f Char) b)
unquotedField sep combinator = UnquotedF <$> combinator (fieldChar sep)

fieldChar :: CharParsing m => Char -> m Char
fieldChar sep = noneOfSet (newlineOr sep)

newlineOr :: Char -> CharSet
newlineOr c = CharSet.insert c newlines

newlines :: CharSet
newlines = CharSet.fromList "\r\n"

newline :: CharParsing m => m Newline
newline =
  CRLF <$ try (string "\r\n")
    <|> CR <$ char '\r'
    <|> LF <$ char '\n'

generalisedField :: CharParsing m => (m Char -> m (f Char)) -> Char -> m (Field String (f Char) String)
generalisedField combinator sep =
  choice [
    try singleQuotedField
  , try doubleQuotedField
  , unquotedField sep combinator
  ]

field :: CharParsing m => Char -> m (Field String String String)
field1 :: CharParsing m => Char -> m (Field String NonEmptyString String)
field = generalisedField many
field1 = generalisedField some1

monoField :: CharParsing m => Char -> m (MonoField String String)
monoField = fmap MonoField . field

horizontalSpace :: CharParsing m => m Char
horizontalSpace = choice (fmap char [' ', '\t'])

spaced :: CharParsing m => m a -> m (Between String a)
spaced p =
  let s = many horizontalSpace
  in liftA3 Between s p s

record :: CharParsing m => Char -> m (Record String String)
record sep =
  Record <$> ((MonoField <$> field sep) `sepEndByNonEmpty` char sep)

separatedValues :: CharParsing m => Char -> m (CSV String NonEmptyString String)
separatedValues sep =
  CSV sep <$> values sep <*> ending sep

values :: CharParsing m => Char -> m (Records String String)
values sep =
  Records <$> try (record sep <* notFollowedBy eof) `pesaratedBy` newline

ending :: CharParsing m => Char -> m (FinalRecord String NonEmptyString String)
ending sep = (FinalRecord <$> (optional (nonEmptyRecord sep))) <* eof

nonEmptyRecord :: CharParsing m => Char -> m (NonEmptyRecord String NonEmptyString String)
nonEmptyRecord sep =
  try (MultiFieldNER <$> monoField sep <* char sep <*> (fields <$> record sep))
  <|> SingleFieldNER <$> field1 sep

