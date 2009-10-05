module MapLoader (readMap) where
import Control.Monad
import System.IO
import Types
import Graphics.Rendering.OpenGL

import Text.ParserCombinators.Parsec.Perm
import Text.ParserCombinators.Parsec.Prim
import Text.ParserCombinators.Parsec.Char
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Combinator
import qualified Text.ParserCombinators.Parsec.Token as P

data PlanetParse = PlanetName String | PlanetPos (Vector3 GLdouble) | Mass GLdouble | Radius GLdouble | TexName String
data ShipParse = ShipPosition (Vector3 GLdouble) | ShipVelocity (Vector3 GLdouble)

wspaces = many $ satisfy (\s -> if s == ' ' || s == '\t' then True else False)

glDoubleParser :: GenParser Char st GLdouble
glDoubleParser = do
  f <- P.float $ P.makeTokenParser emptyDef
  return (fromRational $ toRational $ abs f) <?> "floating point value expected"

-- Parses (a,b,c) where each a,b,c is a floating point
vectorParser = do
  char '('
  a <- glDoubleParser
  char ','
  b <- glDoubleParser
  char ','
  c <- glDoubleParser
  char ')'
  return (Vector3 a b c) <?> "expected (a,b,c) where a is a float"

-- Parser key = value
keyVal k kParser kCon = do
  wspaces >> string k >> wspaces >> string "=" >> wspaces
  val <- kParser
  wspaces
  return $ kCon val

-- Parses Planet: key1 = value1 key2 = value2 etc.. (space seperated)
planetParser :: GenParser Char st (IO Planet)
planetParser = do
  string "Planet:" >> wspaces <?> "keyword Planet:"
  p <- permute (buildPlanet <$$> keyVal "Name" (many1 alphaNum) PlanetName
                       <||> keyVal "Texture" (many1 alphaNum) TexName
                       <||> keyVal "Mass" glDoubleParser Mass
                       <||> keyVal "Radius" glDoubleParser Radius
                       <||> keyVal "Position" vectorParser PlanetPos)
  return p
  where
    buildPlanet (PlanetName n) (TexName t) (Mass m) (Radius r) (PlanetPos p) = do
      --texture <- createTexture tex (True, True) `catch` (\_ -> (putStrLn "Error reading texture: " ++  tex) >> return Nothing)
      let texture = Nothing
      return Planet {planetName = n, planetTexture = texture, planetMass = m, planetRadius = r, planetPos = p}

shipParser :: GenParser Char st Ship
shipParser = do
  string "Ship:" >> wspaces <?> "keyword Ship:"
  p <- permute (buildPlanet <$$> keyVal "Position" vectorParser ShipPosition
                       <||> keyVal "Velocity" vectorParser ShipVelocity)
  return p
  where
    buildPlanet (ShipPosition p) (ShipVelocity v) = Ship {shipPos = p, shipVelocity = v, shipTrail = []}

eol = wspaces >> char '\n' >> wspaces

-- One planet per line
worldParser :: GenParser Char st (IO World)
worldParser = do
  s <- shipParser <?> "Ship Info Expected"
  eol
  p <- planetParser `sepBy1` eol <?> "At least one planet expected"
  return $ do
    p' <- sequence p
    return (p', [], [s])


readMap fileName = do
  f <- readFile fileName
  case runParser worldParser 0 fileName f of
    Left err -> fail (show err)
    Right ps -> ps


