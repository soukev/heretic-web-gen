-- these rules are here because of Aeson so it's simpler to derive classes
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
module Lib where

import System.Environment
import System.Exit
import System.Directory
import System.FilePath.Posix(takeBaseName)
import Control.Monad
import CMarkGFM
import GHC.Generics
import Data.Maybe
import qualified Data.Text as T
import Data.Time
import Data.List(sortOn)
import Data.Ord(Down(..))
import Data.Aeson as Ae (ToJSON(..), FromJSON(..), encodeFile, decodeFileStrict)
import Text.Regex.Posix
-- modules
import GenHF


data Article = Article
  {
    -- | file name
    file :: String,
    -- | title to show in index file
    title :: String,
    -- | date when created
    created :: String,
    -- | date when updated
    updated :: String,
    -- |
    extra :: Bool
  } deriving (Show, Generic, ToJSON, FromJSON)

newtype Articles
  = Articles {articles :: [Article]}
  deriving (Show, Generic, ToJSON, FromJSON)


test :: Article
test = Article
  {
    file="oof.html",
    title="test",
    created="2021-02-23",
    updated="432",
    extra=True
  }

{-
 The 'currTime' function returns formatted current time as %Y-%m-%d.
-}
currTime :: IO String
currTime = do formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S"
     <$> getZonedTime

{-
 Finds first H1 title in .md file and returns it without md formatting
-}
getTitleFromLines :: [String] -> String
getTitleFromLines [] = ""
getTitleFromLines (x:xs)
  | x =~ ("^#.*" :: String) = drop 2 x
  | otherwise = getTitleFromLines xs

{-
  The 'getArticleFromFile' function creates instace of type Article from md file
-}
getArticleFromFile :: String-> String -> IO (Maybe Article)
getArticleFromFile src f = do
  exists <- if src == "" then doesFileExist f else doesFileExist (src ++ "/" ++ f)
  if exists && T.isSuffixOf (T.pack ".md") (T.pack f) then do
    file_cont <- if src == "" then readFile f else readFile (src ++ "/" ++ f)
    let ls = lines file_cont
    let t = getTitleFromLines ls
    ct <- currTime
    return $ Just $ Article (takeBaseName f ++ ".html") t ct "" False
  else
    -- return $ Article "" "" "" "" False
    return Nothing

{-
 The 'articlesFilter' function filters Nothing out of list.
-}
articlesFilter :: [Maybe Article] -> [Article]
articlesFilter [] = []
articlesFilter (x:xs) =
  case x of
    Nothing -> articlesFilter xs
    Just a -> a:articlesFilter xs


-- articleDate :: Article -> ZonedTime
-- articleDate a = parseTimeOrError True defaultTimeLocale "%Y-%m-%d" $ created a

-- convert md to html and return it as string, packing and unpacking fun
htmlFromFileCont :: String -> String
htmlFromFileCont = T.unpack . commonmarkToHtml [] [] . T.pack

{-
  The 'parseFile' function passes source file in md to be processed
  and converted to html.
  Function check if file exists and if file extention is '.md'.
  Args: Source directory -> Destionation directory -> File name
  Returns IO()
-}
parseFile :: String -> String -> String -> IO ()
parseFile src dest f = do 
    a <- if src == "" then doesFileExist f else doesFileExist (src ++ "/" ++ f) -- exists
    when (a && T.isSuffixOf (T.pack ".md") (T.pack f))
        $ do 
        h <- readFile (dest ++ "/header.html.conf")
        writeFile (dest ++ "/html/" ++ takeBaseName f ++ ".html") h
        json <- decodeFileStrict (dest ++ "/conf.json") :: IO (Maybe Articles)
        let articleDate = case json of
                Nothing -> ""
                Just a ->  "<div class='date'>" ++ getArticleDate(articles a) (takeBaseName f ++ ".html")++ "</div>"
        let fuck = "oof"
        s <- if src == "" then readFile f else readFile (src ++ "/" ++ f)
        appendFile
            (dest ++ "/html/" ++ takeBaseName f ++ ".html")
            ("<article>\n"
            ++ htmlFromFileCont s ++ "\n" ++ articleDate ++ "</article>\n")
        readFile (dest ++ "/footer.html.conf")
            >>=
            \ foot
                -> appendFile (dest ++ "/html/" ++ takeBaseName f ++ ".html") foot -- file doesn't exist or doesn't have '.md' suffix

tests :: Article -> IO()
tests a = print $ title a

{-
 So we can print only date without time
 TODO Make less clunky and maybe options for date formates
-}
onlyDate :: String -> String
onlyDate d = d =~ ("[0-9]+-[0-9]+-[0-9]+" :: String) :: String


{-
 The 'genIndexArticleLinks' function generates string from list of Articles in html format.
 Expects Articles to be sorted.
-}
genIndexArticleLinks :: [Article] -> String
genIndexArticleLinks [] = ""
genIndexArticleLinks (a:as) = "<div class=\"articlelink\">\n<a href=\"" ++ file a ++ "\">\n"
  ++ onlyDate (created a) ++ " "
  ++ title a ++ "\n</a>\n</div>" ++ genIndexArticleLinks as


{-
 The 'genIndex' function generates index.html file, takes header, appends content of body
 and appends footer. Result is written to file.
 Expects that header.html.conf and footer.html.conf exists in dest folder.
-}
genIndex :: String -> [Article] -> IO()
genIndex dest as = do
  -- Rewrite file and print header
  readFile (dest ++ "/header.html.conf") >>= \h ->
    writeFile (dest ++ "/html/index.html") h
  appendFile (dest ++ "/html/index.html") ("<div class=\"articles\">\n" ++ genIndexArticleLinks (sortOn (Down . created) as) ++ "</div>")
  -- append footer
  readFile (dest ++ "/footer.html.conf") >>= \foot ->
    appendFile (dest ++ "/html/index.html") foot


rebuildEachFile :: String -> String -> IO [()]
rebuildEachFile src dest = do
  conts <- getDirectoryContents src
  json <- decodeFileStrict (dest ++ "/conf.json") :: IO (Maybe Articles)
  case json of
    Nothing -> print ("Couldn't find conf.json file" :: String) >> exit
    Just a -> do
      let arts = articles a
      genIndex dest arts
      copyFile (dest ++ "/style.css.conf") (dest ++ "/html/css/style.css")
      mapM (parseFile src dest) conts


{-
 The 'forEachFile' function passes each file to parse content, creates conf.json and generates index.html.

                          -- IMPORTANT --
 This function causes all files to be rewritten and creates new dates for articles.
 This function is only good for generating website for first time or for clean rewrite.
-}
forEachFile :: String -> String -> IO [()]
forEachFile src dest = do
  conts <- getDirectoryContents src
  res <- mapM (getArticleFromFile src) conts -- get all articles
  let filtered = articlesFilter res
  mapM_ tests filtered
  let jsonArticles = Articles filtered
  encodeFile (dest ++ "/conf.json") jsonArticles
  genIndex dest filtered
  mapM (parseFile src dest) conts -- get everything from directory and map it to parseFile

{-
  The 'writeFileIfMissing' function copies 'content' to file on 'path'
  if file is missing, otherwise it does nothing
-}
writeFileIfMissing :: String -> String -> IO ()
writeFileIfMissing path content = do
  exists <- doesFileExist path
  if exists then
    return()
  else
    void(writeFile path content)



forceNew :: String -> String -> String -> IO ()
forceNew web_title src dst = do
  -- create directories
  createDirectoryIfMissing True (dst ++ "/html")
  createDirectoryIfMissing True (dst ++ "/html/css")
  createDirectoryIfMissing True (dst ++ "/html/img")
  -- copy header, footer and css to dest dir
  writeFileIfMissing (dst ++ "/header.html.conf") $ gHeader web_title
  writeFileIfMissing (dst ++ "/footer.html.conf") gFooter
  writeFileIfMissing (dst ++ "/style.css.conf") gCSS
  copyFile (dst ++ "/style.css.conf") (dst ++ "/html/css/style.css")
  -- convert all md files
  void(forEachFile src dst)

getPathFromRegTuple :: (String, String, String) -> String
getPathFromRegTuple (x, _, _) = x

getFileFromRegTuple :: (String, String, String) -> String
getFileFromRegTuple (_, x, _) = x

splitPathAndFile :: String -> (String, String, String)
splitPathAndFile article_path = article_path =~ ("[a-zA-Z0-9]+.md$" :: String) :: (String, String, String)

addNewArticle :: String -> String -> IO ()
addNewArticle article_path dst = do
  let res = splitPathAndFile article_path
  -- print res
  let src = getPathFromRegTuple res
  let f = getFileFromRegTuple res
  art <- getArticleFromFile src f

  case art of
    Nothing -> print ("Couldn't find or open file" :: String) >> exit
    Just art_val -> do
      json <- decodeFileStrict (dst ++ "/conf.json") :: IO (Maybe Articles)
      case json of
        Nothing -> do
          encodeFile (dst ++ "/conf.json") (Articles [art_val])
          genIndex dst [art_val]
        Just json_val -> do
          let arts = json_val
          let appendArts = articles arts ++ [art_val]
          encodeFile (dst ++ "/conf.json") (Articles appendArts)
          genIndex dst appendArts
      parseFile src dst f


isArticleInConf :: [Article] -> String -> Bool
isArticleInConf [] _ = False
isArticleInConf (a:as) f
  | f == file a = True
  | otherwise = isArticleInConf as f

getArticleDate :: [Article] -> String -> String
getArticleDate [] _ = ""
getArticleDate (a:as) f
  | f == file a = if updated a /= "" then
                      onlyDate (created a) ++ " (Updated: " ++ onlyDate (updated a) ++ ")"
                    else
                      onlyDate (created a :: String)
  | otherwise = getArticleDate as f

updateArticleConf :: [Article] -> String -> String -> [Article]
updateArticleConf [] _ _ = []
updateArticleConf (a:as) f d
  | f == file a = Article
                      (file a)
                      (title a)
                      (created a)
                      d
                      (extra a) : as
  | otherwise = a : updateArticleConf as f d

updateArticle :: String -> String -> IO()
updateArticle article_path dst = do
  let res = splitPathAndFile article_path
  let src = getPathFromRegTuple res
  let f = getFileFromRegTuple res
  art <- getArticleFromFile src f

  case art of
    Nothing -> print ("Couldn't find or open file." :: String) >> exit
    Just art_val -> do
      json <- decodeFileStrict (dst ++ "/conf.json") :: IO (Maybe Articles)
      case json of
        Nothing -> putStrLn "conf.json missing" >> exitWith (ExitFailure 3)
        Just json_val -> do
          let h = takeBaseName f ++ ".html"
          let arts = articles json_val
          if isArticleInConf arts h then do
            d <- currTime
            let up_arts = updateArticleConf arts h d
            encodeFile (dst ++ "/conf.json") (Articles up_arts)
            parseFile src dst f
          else
            putStrLn "File is not in conf.json" >> exitWith (ExitFailure 4)

updateFile :: String -> String -> IO()
updateFile article_path dst = do
  let res = splitPathAndFile article_path
  let src = getPathFromRegTuple res
  let f = getFileFromRegTuple res
  copyFile (dst ++ "/style.css.conf") (dst ++ "/html/css/style.css")
  parseFile src dst f


-- wrong ARG message and exit
usage :: String -> IO()
usage a
  | a == "-fn" = putStrLn "Force build new project.\nUsage: herewg -fn \"html_title\" src_dir dest_dir"
  | a == "-a" = putStrLn "Add new article to existing project.\nUsage: herewg -a file_path dest_dir"
  | a == "-u" = putStrLn "Update article in existing project.\nUsage: herewg -u file_path dest_dir"
  | a == "-rbf" = putStrLn "Rebuild specific article. Useful when article md file is outside your main dir.\nUsage: herewg -rbf file_path dest_dir"
  | a == "-rbd" = putStrLn "Rebuild all articles from specific directory. Useful if header or footer was changed.\nUsage: herewg -rbd src_dir dest_dir"
  | otherwise = putStr "herewg usage:\n\
                       \ -fn \"title\" src_dir dest_dir      Build new project.\n\
                       \ -a file_path dest_dir             Add new article to existing project.\n\
                       \ -u file_path dest_dir             Update article in existing project.\n\
                       \ -rbf file_path dest_dir           Rebuild article. Useful when article md file is outside your main dir\n\
                       \ -rbd src_dir dest_dir             Rebuild all articles from directory. Useful when header or footer has been changed.\n"

exit :: IO a
exit    = exitSuccess

-- MAIN
generatorMain :: IO ()
generatorMain = do
  args <- getArgs
  case length args of
    0 -> usage "" >> exit
    _-> do
      case head args of
        "-fn" -> if length args == 4 then forceNew (args !! 1) (args !! 2) (args !! 3) else usage (head args) >> exit
        "-a" -> if length args == 3 then addNewArticle (args !! 1) (args !! 2) else usage (head args) >> exit
        "-u" -> if length args == 3 then updateArticle (args !! 1) (args !! 2) else usage (head args) >> exit
        "-rbf" -> if length args == 3 then updateFile (args !! 1) (args !! 2) else usage (head args) >> exit
        "-rbd" -> if length args == 3 then void(rebuildEachFile (args !! 1) (args !! 2)) else usage (head args) >> exit
        _ -> usage (head args) >> exit
