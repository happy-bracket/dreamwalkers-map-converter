{-# LANGUAGE RecordWildCards #-}

module Main where

import           Data.Maybe
import           System.Environment
import           Text.XML.Light.Cursor
import           Text.XML.Light.Input
import           Text.XML.Light.Output
import           Text.XML.Light.Types

extName :: Element -> String
extName = qName . elName

remGid :: Content -> Content
remGid (Elem Element{..}) = Elem $ Element elName (convertAttrs elAttribs) elContent elLine
  where
    convertAttrs = filter ((/=) "gid" . qName . attrKey)
remGid el = el

remGids :: Cursor -> Cursor
remGids cur = let gidRemovedCur = modifyContent remGid cur in
  case nextDF gidRemovedCur of
  Just next -> remGids next
  Nothing   -> gidRemovedCur

remTilesets :: Cursor -> Cursor
remTilesets cur@Cur{current=Elem el} | extName el == "tileset" = case removeGoRight cur of
  Just right -> remTilesets right
  Nothing    -> let (Just rem) = removeGoUp cur in rem
remTilesets cur = case nextDF cur of
  Just next -> remTilesets next
  Nothing   -> cur

convert :: String -> Maybe String
convert tiledMap = (ppContent . current . root . remGids . root . remTilesets . fromElement) <$> parseXMLDoc tiledMap

main :: IO ()
main = do
  (input : output : _) <- getArgs
  tiledMap <- readFile input
  case convert tiledMap of
    Just dwMap -> writeFile output dwMap
    Nothing    -> putStrLn "malformed input map"
  return ()
