{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative (liftA2)
import Control.Lens
import Data.Functor.Alt
import Data.Semigroup
import Data.Text (Text)
import System.Exit (exitFailure)

import Data.Sv

config :: SvConfig
config = defaultConfig & (headedness .~ Unheaded)

file :: FilePath
file = "test/commastring.csv"

data CommaString =
  CommmaString Int Int Double Text Double
  deriving (Eq, Ord, Show)

commastringDecode :: FieldDecode' Text CommaString
commastringDecode =
  CommmaString <$> int <*> int <*> double <*> text <*> double
  <!> CommmaString <$> int <*> int <*> double <*> (liftA2 (\s t -> s <> "," <> t) text text) <*> double

main :: IO ()
main = do
  v <- decodeFromFile commastringDecode (Just config) file
  case v of
    AccFailure e -> do
      putStrLn "Failed to parse and decode commastring.csv:"
      print e
      exitFailure
    AccSuccess a ->
      print a
