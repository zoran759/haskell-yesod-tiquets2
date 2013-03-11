{-# LANGUAGE PackageImports, OverloadedStrings, QuasiQuotes #-}
-- module Util.UtilPaisos (països, nomPaís) where

-- import "shakespeare-text" Text.Shakespeare.Text
import Prelude
import Data.Text as T
import Data.Tuple (swap)
import Data.List as L
import Data.Map as M
import Text.Printf (printf)
import Control.Monad
import System.IO
import System.Environment (getProgName, getArgs)   -- a Linux System.Environment.UTF8
import System.Exit (exitSuccess, exitWith, ExitCode(..))

x |> f = f x

aeroports :: String -> [(Text, Text, Text, Text, Text)]
aeroports contingut = L.lines contingut
           |> L.tail
           |> L.map (processa . T.init . T.pack)


processa :: Text -> (Text, Text, Text, Text, Text)
processa lin = fn $ T.split (==',') lin


fn :: [Text] -> (Text, Text, Text, Text, Text)
fn c = (codi, poble, provincia, aeroport, país)
   where
        codi = st(c!!0)
        poble = st(c!!1)
        provincia = st(c!!2)
        aeroport = st(c!!3)
        país = st(c!!4)

st x = let y = T.strip x
       in case T.uncons y of
            Just ('"',r1) -> case T.uncons r1 of
                               Just ('[',r2) -> T.init $ T.init r2
                               Just ('\xA0',_) -> ""
                               Just ( _,r2) -> if T.last r1 == '"' then T.init r1 else r1
                               _ -> y
            _ -> y
             
test :: String -> IO ()             
test term = do
     contingut <- readFile $ "docs/aeroports-"++ term ++".csv"
     forM_ (aeroports contingut) $ \(codi, poble, provincia, aeroport, país) -> do
          printf "%s %s %s %s %s\n" (T.unpack codi) (T.unpack poble) (T.unpack provincia) (T.unpack aeroport) (T.unpack país)
{-

test2 :: String -> IO ()
test2 país = do
     contingut <- readFile $ "docs/aeroports-"++ país ++".csv"
     forM_ (aeroports contingut) $ \(codi, poble, provincia, aeroport, país) -> do
          printf "%s %s\n" (T.unpack codi) (T.unpack nom)

test3 :: String -> IO ()
test3 país = do
     contingut <- readFile $ "docs/aeroports-"++ país ++".csv"
     forM_ (aeroports contingut) $ \(codi, nom) -> do
          let scodi = T.unpack codi
              snom = T.unpack  nom
          case snom of
               hd:_ -> printf "%s %d %04x %s\n" scodi (L.length snom) hd snom
               _ -> printf "%s %d\n" scodi (L.length snom)
               -}               
          
main = do
        args <- getArgs
        let ús = "entreu codi país\n"
        case args of
             [arg] -> do
                     let codi = (T.unpack . T.strip . T.pack) arg
                     test codi
             _ -> putStrLn ús