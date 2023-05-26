module Plugin.FileIO.FileIO (allSnippets, loadSnippet, snippetsOfType ) where
import Plugin.Types  (Snippet(..), SnippetMetaData (..))


sampleData :: [Snippet]
sampleData = [
  Snippet "MySnippet" ["hello <#Title#>", "bye <#Title#>"] (SnippetMetaData {fileType = "haskell" }), 
  Snippet "Andris Snippet" ["bye <#Title#>"] (SnippetMetaData {fileType = "haskell" }),
  Snippet "Bashibash Snippet" ["bye <#Title#>"] (SnippetMetaData {fileType = "bash" })
  ]

allSnippets :: [Snippet]
allSnippets = sampleData

snippetsOfType :: String -> [Snippet]
snippetsOfType ft = filter (\snippet -> (fileType . meta) snippet == ft) allSnippets


loadSnippet :: String -> IO Snippet
loadSnippet n = do
    let s = filter (\(Snippet name _ _) -> name == n ) allSnippets
    pure $ head s

