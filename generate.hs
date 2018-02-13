#!/usr/bin/env stack
-- stack --resolver lts-10.2 script --package turtle --package here --package text --package attoparsec --package foldl

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

import qualified Control.Foldl        as Fold
import           Data.Attoparsec.Text as A
import           Data.List            (nub)
import           Data.Maybe           (catMaybes)
import           Data.String.Here
import qualified Data.Text            as T
import           Data.Text.Lazy.IO    (writeFile)
import           Prelude              hiding (FilePath, writeFile)
import           Turtle               hiding (nub)

-- | The tachyons source directory.
tachyonsSource :: FilePath
tachyonsSource = "./node_modules/tachyons/src/"

-- | Distribution directory.
distDir :: FilePath
distDir = "./src/Halogen/CSS/Tachyons"

-- | Module prefix
modulePrefix :: Text
modulePrefix = "Halogen.CSS.Tachyons"

main = do
  sh (do file <- find (suffix ".css") tachyonsSource; parseFile file)
  echo ""
  echo "==============================================================="
  echo "Done!"
  echo "==============================================================="

parseFile :: FilePath -> Shell ()
parseFile path = do
  let name    = toModuleName path
  let modName = modulePrefix <> "." <> name
  let dPath   = distDir </> (fromString . T.unpack) name <.> "purs"
  let header  = headTemplate modName
  lines <- fold (input path) Fold.list
  let classList = nub $ concat $ catMaybes $ map (getClassname) lines
  let content   = map classTemplate classList
  case content of
    [] -> liftIO $ print $ "! Skipping " <> name <> " no usefull statements found"
    _ -> do
         liftIO $ print $ "Exporting " <> name
         output dPath (pure $ unsafeTextToLine $ header)
         sh (do line <- select (textToLines $ T.concat content); append dPath (pure line))

--getClassname :: Line -> [Text]
getClassname l =
  case parseOnly parser (lineToText l) of
    Left _   -> Nothing :: Maybe [Text]
    Right [] -> Nothing :: Maybe [Text]
    Right a  -> Just a
  where parser = A.sepBy (parseClassname <|> parseRootClassname) (A.char ',')

parseClassname :: A.Parser Text
parseClassname = lexeme $ do
  A.char '.'
  A.takeTill $ (=='{') <||> (==':') <||> (==' ') <||> (== ',')

parseRootClassname :: A.Parser Text
parseRootClassname = lexeme $ do
  A.char '-'
  A.char '-'
  A.takeTill $ (==':') <||> (==' ')

-- | Cosume whitespace tab and line-ending.
lexeme :: A.Parser a -> A.Parser a
lexeme p = A.skipSpace *> p <?> "lexeme"

-- | Transform a filepath to a modulename.
toModuleName :: FilePath -> Text
toModuleName p = T.concat $ map T.toTitle $ splitted $ format fp $ basename p
 where splitted = T.split ((=='_') <||> (=='-'))

-- | A lifted ('&&').
(<&&>) :: Applicative f => f Bool -> f Bool -> f Bool
(<&&>) = liftA2 (&&)
{-# INLINE (<&&>) #-}

-- | A lifted ('||').
(<||>) :: Applicative f => f Bool -> f Bool -> f Bool
(<||>) = liftA2 (||)
{-# INLINE (<||>) #-}

-- | Convert a Either into a Maybe.
eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe (Left _)  = Nothing
eitherToMaybe (Right b) = Just b

-- -----------------------------------------------------------------------------
-- Templates

-- | Head template for file head.
headTemplate :: Text -> Text
headTemplate moduleName = T.pack [i|module ${T.unpack moduleName} where

import Halogen.HTML (ClassName(..))
|]

-- | Class template.
classTemplate :: Text -> Text
classTemplate klass = T.pack [i|${functionName klass} :: ClassName
${functionName klass} = ClassName "${className klass}"

|]
  where functionName t = T.unpack $ T.replace "-" "_" t
        className t = T.unpack t
