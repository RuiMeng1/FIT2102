import           Control.Concurrent
import           Control.Exception
import           Control.Monad        (forever)
import           Data.List            (isPrefixOf)
import           GHC.Exception
import           System.Console.ANSI
import           System.Environment   (getArgs)
import           System.Exit
import           System.FilePath.Glob (glob)
import           System.FSNotify
import           System.IO
import           Test.DocTest         (doctest)

main :: IO ()
main = do
  e <- parseArgs <$> getArgs
  case e of
    Left msg -> putStrLn msg >> exitFailure
    Right getFiles -> getFiles >>= \files -> do
      runDoctest files
      watch forkDoctest "src"

parseArgs :: [String] -> Either String (IO [String])
parseArgs [] = Right requiredFiles
parseArgs ["s"] = Right allFiles
parseArgs ["supplementary"] = Right allFiles
parseArgs args = Left $ "Invalid arguments: " ++ unwords args ++ ".\nValid arguments: supplementary, s"

requiredFiles :: IO [String]
requiredFiles = glob "src/*.hs"

allFiles :: IO [String]
allFiles = (++) <$> requiredFiles <*> glob "src/supplementary/*.hs"

resetScreen :: IO ()
resetScreen = setSGR [Reset] >> clearScreen >> setCursorPosition 0 0

colorMsg :: Color -> String -> IO ()
colorMsg c s = do
  setSGR [SetColor Foreground Vivid c]
  putStrLn $
    s
      ++ "\ESC[0m" -- resets the colour, because the following doesn't seem to always work.
  setSGR [Reset]

runDoctest :: [String] -> IO ()
runDoctest files = do
  resetScreen
  colorMsg Cyan $ "testing " ++ show files ++ "..."
  finally
    ( ( do
          doctest files
          colorMsg Green $ "Tests Passed for " ++ show files
      )
        `catch` (\(SomeException _) -> colorMsg Red "Error from doctest!")
    )
    (colorMsg Cyan "waiting for file change...")

forkDoctest :: Event -> IO ()
forkDoctest (Modified f _ _) = do
  id <- forkIO $ runDoctest [f]
  return ()
forkDoctest _ = return () -- ignore new files etc

conf =
  WatchConfig
    { confDebounce = Debounce 10,
      confUsePolling = True,
      confPollInterval = 1000
    }

watch :: Action -> FilePath -> IO a
watch action targetDir =
  withManagerConf conf $ \mgr -> do
    -- start a watching job (in the background)
    watchDir
      mgr -- manager
      targetDir -- directory to watch
      (const True) -- predicate
      action -- action

    -- sleep forever (until interrupted)
    forever $ threadDelay 1000000
