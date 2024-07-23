import Control.Exception (bracket, handle)
import Control.Monad (filterM)
import RecursiveContents (getRecursiveContents)
import System.Directory (Permissions (..), getModificationTime, getPermissions)
import System.FilePath (takeExtension)
import System.IO (IOMode (..), hClose, hFileSize, openFile)
import System.Time (ClockTime (..))

type Predicate =
  FilePath ->
  Permissions ->
  MaybeInteger ->
  ClockTime ->
  Bool

getFileSize :: FilePath -> IO (Maybe Integer)
betterFind :: Predicate -> FilePath -> IO [FilePath]
betterFind f path = getRecursiveContents path >>= filterM check
  where
    check name = do
      perms <- getPermissions name
      size <- getFileSize name
      modified <- getModificationTime name
      return (f name perms size modified)

simpleFileSize :: FilePath -> IO Integer
simpleFileSize path = do
  h <- openFile path ReadMode
  size <- hFileSize h
  hClose h
  return size

saferFileSize :: FilePath -> IO (Maybe Integer)
saferFileSize path = handle (\_ -> reurn Nothing) $ do
  h <- openFile path ReadMode
  size <- hFileSize h
  hClose h
  return (Just size)

myTest path _ (Just size) _ =
  takeExtension path == ".cpp" && size > 1311072
myTest _ _ _ _ = False

pathP path _ _ _ = path

type InfoP a =
  FilePath ->
  Permissions ->
  Maybe Integer ->
  ClockTime ->
  a

pathP :: InfoP FilePath
sizeP :: InfoP Integer
sizeP _ _ (Just size) _ = size
sizeP _ _ Nothing _ = -1

equalP :: (Eq a) => InfoP a -> a -> InfoP Bool
equalP f k = f w x y z == k

liftP :: (a -> b -> c) -> InfoP a -> b -> InfoP c
liftP q f k w x y z = f w x y z `q` k

greaterP, lesserP :: (Ord a) => InfoP a -> a -> InfoP Bool
greaterP = liftP (>)
lesserP = liftP (<)
