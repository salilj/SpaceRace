module MapParser (readMap) where
import Control.Monad
import System.IO
import Types
import Texture
import Graphics.Rendering.OpenGL

import Text.ParserCombinators.Parsec.Perm
import Text.ParserCombinators.Parsec.Prim
import Text.ParserCombinators.Parsec.Char
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Combinator
import qualified Text.ParserCombinators.Parsec.Token as P

data PlanetParse = PlanetName String | PlanetPos (Vector3 GLdouble) | Mass GLdouble | Radius GLdouble | TexName String
data ShipParse = ShipPosition (Vector3 GLdouble) | ShipVelocity (Vector3 GLdouble) | ShipFuel GLdouble

glDoubleParser :: GenParser Char st GLdouble
glDoubleParser = do
  f <- P.naturalOrFloat $ P.makeTokenParser emptyDef
  case f of
    Left l -> return (fromIntegral $ abs l) 
    Right r -> return (fromRational $ toRational $ abs r)
  <?> "expected"


-- Parses (a,b,c) where each a,b,c is a GLdouble
vectorParser = do
  char '(' >> spaces
  a <- glDoubleParser
  char ',' >> spaces
  b <- glDoubleParser
  char ',' >> spaces
  c <- glDoubleParser
  char ')'
  return (Vector3 a b c) <?> "(<number>,<number>,<number>)"

-- Parser key = value
keyVal k kParser kCon = do
  spaces >> string k >> spaces >> string "=" >> spaces
  val <- kParser
  char ';'
  spaces
  return $ kCon val

-- Parses Planet: key1 = value1 key2 = value2 etc.. (space seperated)
planetParser :: GenParser Char st (IO Planet)
planetParser = do
  string "Planet:" >> spaces <?> "keyword Planet:"
  p <- permute (buildPlanet <$$> keyVal "Name" (many1 $ noneOf [';']) PlanetName
                       <||> keyVal "Texture" (many1 $ noneOf [';']) TexName
                       <||> keyVal "Mass" glDoubleParser Mass
                       <||> keyVal "Radius" glDoubleParser Radius
                       <||> keyVal "Position" vectorParser PlanetPos)
  spaces
  return p
  where
    buildPlanet (PlanetName n) (TexName t) (Mass m) (Radius r) (PlanetPos p) = do
      texture <- createTexture t (True, True) `catch` (\e -> (putStrLn $ "Error: " ++ show e ++ " while reading texture: " ++ t) >> return Nothing)
      return Planet {planetName = n, planetTexture = texture, planetMass = m, planetRadius = r, planetPos = p}

shipParser :: GenParser Char st Ship
shipParser = do
  string "Ship:" >> spaces <?> "keyword Ship:"
  p <- permute (buildPlanet <$$> keyVal "Position" vectorParser ShipPosition
                       <||> keyVal "Velocity" vectorParser ShipVelocity
                       <||> keyVal "Fuel" glDoubleParser ShipFuel)
  spaces
  return p
  where
    buildPlanet (ShipPosition p) (ShipVelocity v) (ShipFuel f)= Ship {shipPos = p, shipVelocity = v, shipTrail = [], shipFuel=f}


-- A ship, followed by at least one planet
worldParser :: GenParser Char st (IO World)
worldParser = do
  s <- shipParser <?> "Ship Info"
  p <- ((many1 planetParser) <?> "At least one planet")
  return $ do
    p' <- sequence p
    return (p', [], [s])


readMap fileName = do
  f <- readFile fileName
  case runParser worldParser 0 fileName f of
    Left err -> fail (show err)
    Right ps -> ps


