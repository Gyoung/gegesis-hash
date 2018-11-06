{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import           Universum

import           Crypto.Hash (Digest, hashlazy)
import           Crypto.Hash.Algorithms (SHA256)
import           Data.ByteString.Lazy.Char8 as L8
import           System.IO (stdin)
import           Text.JSON.Canonical (JSValue, parseCanonicalJSON,
                     renderCanonicalJSON)

main :: IO ()
main = do
  f <- parseArgs
  val <- readFile2 f
  print (blakeHash2 val)

parseArgs :: IO (Maybe FilePath)
parseArgs = getArgs >>= \case
  ["-"] -> pure Nothing
  [f] -> pure (Just f)
  _ -> die "usage: genesis-hash  INFILE.json" >> exitFailure




readJSON :: Maybe FilePath -> IO JSValue
readJSON mf = do
  bs <- case mf of
    Just f  -> L8.readFile f
    Nothing -> L8.hGetContents stdin
  case parseCanonicalJSON bs of
    Right v -> pure v
    Left e  -> die e >> exitFailure

blakeHash :: JSValue -> Digest SHA256
blakeHash = hashlazy . renderCanonicalJSON


readFile2 :: Maybe FilePath -> IO L8.ByteString
readFile2 mf = do
    case mf of
      Just f  -> L8.readFile f
      Nothing -> L8.hGetContents stdin

blakeHash2 :: L8.ByteString -> Digest SHA256
blakeHash2 = hashlazy