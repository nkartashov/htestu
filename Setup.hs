import Distribution.Verbosity
import Distribution.Simple
import Distribution.Simple.Setup
import Distribution.Simple.Utils

import System.FilePath.Posix
import System.Directory
import System.Process

import Data.Maybe
import Data.Function
import Data.List
import Control.Monad

main = defaultMainWithHooks simpleUserHooks
  { preBuild = \args flags -> buildTestU args flags >> preBuild simpleUserHooks args flags}

testUPath = "cbits/testu"
buildPath = "dist/build"

configureScript = "configure"

callFromEnvironment :: Verbosity -> [String] -> IO ()
callFromEnvironment verbosity = rawSystemExit verbosity "env"

checkProgramIsInPath :: Verbosity -> FilePath -> IO Bool
checkProgramIsInPath verbosity program = findProgramLocation verbosity program >>= return . isJust

neededPrograms = ["make", "gcc"]

notPresentErrorMessage program = program ++ " not present in path"

findProgramOrDie :: Verbosity -> String -> [(String, FilePath)] -> IO FilePath
findProgramOrDie verbosity program suppliedPrograms = do
  let programExec = case filter ((== program) . fst) suppliedPrograms of
                          [x] -> snd x
                          _ -> program
  found <- checkProgramIsInPath verbosity programExec
  if found then return programExec
           else die $ programExec ++ " not found in path"

takeUniquePaths :: [(String, FilePath)] -> [(String, FilePath)]
takeUniquePaths = map head . (groupBy ((==) `on` fst)) . sortBy (compare `on` fst)

buildTestU :: Args -> BuildFlags -> IO ()
buildTestU _ flags  = do
  let verbosity = fromFlag $ buildVerbosity flags
  currentDirectory <- getCurrentDirectory
  let fullTestUPath = currentDirectory </> testUPath
  setCurrentDirectory fullTestUPath
  let prefix = "--prefix=" ++ (currentDirectory </> buildPath)
  let buildPrograms = takeUniquePaths $ buildProgramPaths flags
  rawSystemExit verbosity (fullTestUPath </> configureScript) [prefix]
  makeExec <- findProgramOrDie verbosity "make" buildPrograms
  gccExec <- findProgramOrDie verbosity "gcc" buildPrograms
  callFromEnvironment verbosity [makeExec, "CC=" ++ gccExec]
  callFromEnvironment verbosity [makeExec, "install"]
  setCurrentDirectory currentDirectory
