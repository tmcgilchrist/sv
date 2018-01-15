import Data.ByteString (ByteString)
import Data.Text (Text)
import Text.Trifecta (TokenParsing, CharParsing, integer, parseFromFile, string)
import System.Exit (exitFailure)

import Data.Sv hiding (integer, parser, string)
import Text.Babel (Textual)

file :: FilePath
file = "test/concat.csv"

type Name = Text
type Age = Int
data Person = Person Name Age deriving Show

person :: FieldDecode' ByteString Person
person = Person <$> utf8 <*> int

type Stock = Int
-- TODO use a harder type than Integer
newtype Cost = Cost Integer deriving Show
data Item = Item Name Stock Cost deriving Show

cost :: TokenParsing m => m Cost
cost = string "$" *> fmap Cost integer

item :: FieldDecode' ByteString Item
item = Item <$> utf8 <*> int <*> trifecta cost

sv2 :: (CharParsing m, Textual s) => Separator -> Headedness -> m (Sv s, Sv s)
sv2 sep h = (,) <$> separatedValues sep h <*> separatedValues sep h

data PeopleAndItems = PeopleAndItems [Person] [Item] deriving Show

parser :: CharParsing m => m (Sv ByteString, Sv ByteString)
parser = sv2 comma Headed

main :: IO ()
main = do
  d <- parseFromFile parser file
  case d of
    Nothing -> exitFailure
    Just (s1,s2) -> do
      let result = PeopleAndItems <$> decode person s1 <*> decode item s2
      case result of
        AccFailure e -> do
          print e
          exitFailure
        AccSuccess a -> print a