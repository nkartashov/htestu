import Distribution.Verbosity
import Distribution.Simple
import Distribution.Simple.Setup
import Distribution.Simple.Utils

import System.FilePath.Posix
import System.Directory
import System.Process

main = defaultMainWithHooks simpleUserHooks
  { preBuild = \args flags -> buildTestU args flags >> preBuild simpleUserHooks args flags}

testUPath = "cbits/testu"
buildPath = "dist/build"

configureScript = "configure"

callFromEnvironment :: Verbosity -> [String] -> IO ()
callFromEnvironment verbosity = rawSystemExit verbosity "env"

buildTestU :: Args -> BuildFlags -> IO ()
buildTestU _ flags  = do
  let verbosity = fromFlag $ buildVerbosity flags
  currentDirectory <- getCurrentDirectory
  let fullTestUPath = currentDirectory </> testUPath
  setCurrentDirectory fullTestUPath
  let prefix = "--prefix=" ++ (currentDirectory </> buildPath)
  rawSystemExit verbosity (fullTestUPath </> configureScript) [prefix]
  callFromEnvironment verbosity ["make", "CC=/usr/local/bin/gcc-4.9"]
  callFromEnvironment verbosity ["make", "install"]
  setCurrentDirectory currentDirectory
