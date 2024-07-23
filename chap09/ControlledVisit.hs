import Control.Exception (bracket)
import Control.Monad (forM, liftM)
import GHC.IO.IOMode (IOMode (ReadMode))
import System.Directory (Permissions (searchable), getDirectoryContents, getModificationTime, getPermissions)
import Prelude hiding (traverse)
import System.FilePath ((</>))

data Info = Info
  { infoPath :: FilePath,
    infoPerms :: Maybe Permissions,
    infoSize :: Maybe Integer
  }
  deriving (Eq, Ord, Show)

maybeIO :: IO a -> IO (Maybe a)
maybeIO act = handle (\_ -> return Nothing) (Just `liftM` act)

getInfo path = do
  perms <- maybeIO (getPermissions path)
  size <- maybeIO (bracket (openFile path ReadMode) hClose hFileSize)
  modified <- maybeIO (getModificationTime path)
  return (Info path perms size modified)

traverse :: ([Info] -> [Info]) -> FilePath -> IO [Info]
traverse order path = do
  names <- getUsefulContents path
  contents <- mapM getInfo (path : map (path </>) names)
  liftM concat $ forM (order contents) $ \info -> do
    if isDirectory info && infoPath info /= path
      then traverse order (infoPath info)
      else return [info]

getUsefulContents :: FilePath -> IO [String]
getUsefulContents path = do
  names <- getDirectoryContents path
  return (filter (`notElem` [".", ".."]) names)

isDirectory :: Info -> Bool
isDirectory = maybe False searchable . infoPerms

traverseVerbose order path = do
  names <- getDirectoryContents path
  let usefulNames = filter (`notElem` [".", ".."]) names
  contents <- mapM getEntryName ("" : usefulNames)
  recursiveContents <- mapM recurse (order contents)
  return (concat recursiveContents)
  where
    getEntryName name = getInfo (path </> name)
    isDirectory info = case infoPerms info of
      Nothing -> False
      Just perms -> searchable perms
    recurse info = do
      if isDirectory info && infoPath info /= path
        then traverseVerbose order (infoPath info)
        else return [info]
