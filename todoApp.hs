import           Control.Exception
import           Data.List
import           System.Directory
import           System.Environment
import           System.IO

dispatch :: String -> [String] -> IO ()
dispatch "add"       = add
dispatch "view"      = view
dispatch "remove"    = remove
dispatch "bump"      = bump
dispatch wrongInput  = failMessage wrongInput
disptach _           = fullBlownError


bump :: [String] -> IO ()
bump [fileName, numberString] = do
  contents <- readFile fileName
  let todoTasks = lines contents
      number = read numberString
      newTodoItems = unlines $ (todoTasks !! number) : delete (todoTasks !! number) todoTasks
  bracketOnError (openTempFile "." "temp")
    (\(tempName, tempHandle) -> do
      hClose tempHandle
      removeFile tempName)
    (\(tempName, tempHandle) -> do
      hPutStr tempHandle newTodoItems
      hClose tempHandle
      removeFile fileName
      renameFile tempName fileName)



fullBlownError :: IO ()
fullBlownError =
  putStrLn "Invalid input, please try again."


failMessage :: String -> [String] -> IO ()
failMessage msg _ =
  putStrLn $ "I'm sorry I don't recognize command " ++ msg


add :: [String] -> IO ()
add [fileName, todoItem] = appendFile fileName (todoItem ++ "\n")


view :: [String] -> IO ()
view [fileName] = do
  contents <- readFile fileName
  let todoTasks = lines contents
      numberedTasks = zipWith (\n line -> show n ++ " - " ++ line )
                              [0..] todoTasks
  putStr $ unlines numberedTasks


remove :: [String] -> IO ()
remove [fileName, numberString] = do
  contents <- readFile fileName
  let todoTasks = lines contents
      numberedTasks = zipWith (\n line -> show n ++ " - " ++ line )
                              [0..] todoTasks
      number = read numberString
      newTodoItems = unlines $ delete (todoTasks !! number) todoTasks
  bracketOnError (openTempFile "." "temp")
    (\(tempName, tempHandle) -> do
      hClose tempHandle
      removeFile tempName)
    (\(tempName, tempHandle) -> do
      hPutStr tempHandle newTodoItems
      hClose tempHandle
      removeFile fileName
      renameFile tempName fileName)
  putStrLn "These are your remaining Todo items: "
  mapM_ putStrLn $ zipWith (\n line -> show n ++ " - " ++ line)
                           [0..] $ lines newTodoItems

main = do
  (command:argList) <- getArgs
  dispatch command argList
