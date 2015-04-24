import System.Directory
import Control.Monad
import System.FilePath.Posix
import System.IO.Error
--import Control.Exception.catch

srcPath = "/home/igor/workspace/app-test/src"

isDODD :: String -> Bool
isDODD f = f == "." || f == ".."

listFiles :: [FilePath] -> IO [FilePath]
listFiles = filterM doesFileExist

listDirs :: [FilePath] -> IO [FilePath]
listDirs = filterM doesDirectoryExist

joinFN :: String -> String -> FilePath
joinFN str1 str2 = joinPath [str1, str2]

dir :: FilePath -> IO [FilePath]
dir path = do 
	setCurrentDirectory path
	cd <- getCurrentDirectory
	allfiles <- getDirectoryContents cd
	dirss <- listDirs (filter (not.isDODD) allfiles)
	dirs <- (mapM (return.(joinFN path)) dirss ) 
	filess <- listFiles allfiles
	files <- (mapM (return.(joinFN path)) filess ) 
	subfiles <- (mapM dir dirs >>= return.concat)
	return (files ++ subfiles)

getLength :: FilePath -> IO Int
getLength path = do
	files <- dir path
	return (length files)
	
getLength2 :: FilePath -> IO Int
getLength2 path = do
	files <- mainFunc path
	return (length files)

readfile :: FilePath -> IO String	
readfile path = do
	str <- readFile path
	return str

rus :: [Char]
rus = ['а'..'я'] ++ ['А'..'Я']

containRus :: String -> Bool
containRus str = length (filter (\x -> rusChar x) str) > 0

rusChar :: Char -> Bool
rusChar ch = length( filter (\x -> x) [x == ch | x <- rus]) > 0

containQuote :: String -> Bool
containQuote str = elem '"' str

containQuote2 :: String -> Bool
containQuote2 str = elem '\'' str

mainFunc :: FilePath -> IO [FilePath]
mainFunc path = do
	filess <- dir path
	files <- (filterM filterFunc filess)
	return files

filterFunc :: FilePath -> IO Bool
filterFunc path = do 
	str <- readFile path
	return (filterWords str)

filterWords :: String -> Bool
filterWords str = length (filter (\x -> (containRus x &&( containQuote x || containQuote2 x))) (words str)) > 0

printFiles files = do
	filess <- mainFunc files
	z <- mapM print filess
	return z
