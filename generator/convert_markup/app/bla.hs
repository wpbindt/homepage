import Options.Applicative

data Options =
    ConvertSingle SingleInput SingleOutput
    | ConvertDir FilePath FilePath
    deriving Show

data SingleInput = 
    StdIn
    | FileInput FilePath
    deriving Show

data SingleOutput = 
    StdOut
    | FileOutput FilePath
    deriving Show

inp :: Parser FilePath
inp =
    strOption
        ( long "input"
          <> short 'i'
          <> metavar "FILE"
          <> help "Input file"
        )

out :: Parser FilePath
out =
    strOption
        ( long "output"
          <> short 'o'
          <> metavar "FILE"
          <> help "Output file"
        )

pInputFile :: Parser SingleInput
pInputFile = InputFile <$> parser
    where
        parser =
            strOption
                ( long "input"
                  <> short 'i'
                  <> metavar "FILE"
                  <> help "Input file"
                )

pOutputFile :: Parser SingleOutput
pOutputFile = OutputFile <$> parser
    where
        parser =
            strOption
                ( long "output"
                  <> short 'o'
                  <> metavar "FILE"
                  <> help "Output file"
                )

pConvertSingle :: Parser Options
pConvertSingle = ConvertSingle <$> pInputFile <*> pOutputFile

