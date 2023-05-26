module Plugin.FileIO.FileIO (allSnippets, loadSnippet, snippetsOfType ) where
import Plugin.Types  (Snippet(..), SnippetMetaData (..))


sampleData :: [Snippet]
sampleData = [
  Snippet "MySnippet" ["hello <#Title#>", "bye <#Title#>"] (SnippetMetaData {fileType = "haskell" }),
  Snippet "Andris Snippet" ["bye <#Title#>"] (SnippetMetaData {fileType = "haskell" }),
  Snippet "Bashibash Snippet" ["bye <#Title#>"] (SnippetMetaData {fileType = "bash" })
  ]

allSnippets :: IO [Snippet]
allSnippets = return sampleData

snippetsOfType :: String -> IO [Snippet]
snippetsOfType ft =
  filter (\snippet -> (fileType . meta) snippet == ft) <$> allSnippets


loadSnippet :: String -> IO Snippet
loadSnippet n =  head . filter (\(Snippet name _ _) -> name == n ) <$> allSnippets

