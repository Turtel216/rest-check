module Cli where

import Options.Applicative

-- | CLI Options
data Options = Options
  { configFiles :: [FilePath] -- ^ Multiple config files
  , verbose :: Bool           -- ^ Verbose output
  , quiet :: Bool             -- ^ Minimal output (only failures)
  , noColor :: Bool           -- ^ Disable colored output 
  , concurrent :: Bool        -- ^ Run files concurrently 
  , timeout :: Maybe Int      -- ^ Request timneout in seconds
  , dryRun :: Bool            -- ^ Parse config wihtout executing
  } deriving (Show)

-- | Parser for CLI options
optionsParser :: Parser Options
optionsParser = Options
  <$> some (argument str
        ( metavar "CONFIG_FILES..."
       <> help "One or more config files to process" ))
  <*> switch
        ( long "verbose"
       <> short 'v'
       <> help "Enable verbose output" )
  <*> switch
        ( long "quiet"
       <> short 'q'
       <> help "Only show failures" )
  <*> switch
        ( long "no-color"
       <> help "Disable colored output" )
  <*> switch
        ( long "concurrent"
       <> short 'c'
       <> help "Process multiple files concurrently" )
  <*> optional (option auto
        ( long "timeout"
       <> short 't'
       <> metavar "SECONDS"
       <> help "Request timeout in seconds" ))
  <*> switch
        ( long "dry-run"
       <> short 'n'
       <> help "Parse config files without executing requests" )

-- | Full parser with help text
optsInfo :: ParserInfo Options
optsInfo = info (optionsParser <**> helper)
  ( fullDesc
 <> progDesc "Run REST API checks from CONFIG_FILES"
 <> header "restcheck - a declarative REST API testing tool" )

-- | Color handling
data Colors = Colors
  { cGreen :: String
  , cRed   :: String
  , cYellow :: String
  , cBlue :: String
  , cReset :: String
  }

mkColors :: Bool -> Colors
mkColors noCol
  | noCol = Colors "" "" "" "" ""
  | otherwise = Colors "\x1b[32m" "\x1b[31m" "\x1b[33m" "\x1b[34m" "\x1b[0m"
