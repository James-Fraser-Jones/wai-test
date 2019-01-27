{-# LANGUAGE OverloadedStrings #-}

module Main where

--------------------------------------------------------------------------------

import qualified Data.Text as T
import qualified Data.ByteString.Char8 as B

import Data.Functor
import Data.List
import Data.List.Split (splitOn)

import System.Directory (doesFileExist)

import Network.Wai                                --(web server <--> web app/framework) interface... so this is just a terribly basic web application?
import Network.HTTP.Types (status200, status404)  --useful http types
import Network.Wai.Handler.Warp (run)             --warp fast low level web server

--------------------------------------------------------------------------------

type Extension = String

--------------------------------------------------------------------------------

application :: Application
application request respond = do
   let rawFilePath = getPath request
       filePath = convertFilePath rawFilePath
   fileExists <- doesFileExist filePath
   let response = if fileExists then successResponse filePath else errorResponse
   respond response

getPath :: Request -> FilePath
getPath = tail . B.unpack . rawPathInfo

getExtension :: FilePath -> Extension
getExtension = last . splitOn "."

convertFilePath :: FilePath -> FilePath
convertFilePath "" = convertFilePath "index"
convertFilePath filePath = "public/" ++ filePath ++ if not ('.' `elem` filePath) then ".html" else ""

errorResponse :: Response
errorResponse = responseLBS status404 [("Content-Type", "text/plain")] "Oh Noes! File Not Found! Error: 404"

successResponse :: FilePath -> Response
successResponse path = responseFile status200 [("Content-Type", mimeType path)] path Nothing

mimeType :: FilePath -> B.ByteString
mimeType filePath = B.pack $ case getExtension filePath of
  "html" -> "text/html"
  "css" -> "text/css"
  "txt" -> "text/plain"
  _ -> "text/plain"

runWarp :: Int -> Application -> IO ()
runWarp port app = putStrLn ("Listening on port: " ++ show port) >> run port app

main :: IO ()
main = runWarp 3000 application
