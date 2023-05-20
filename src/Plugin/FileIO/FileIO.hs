module Plugin.FileIO.FileIO (allSnippets, loadSnippet ) where
import Plugin.Types  (Snippet(..))


sampleData :: [Snippet]
sampleData = [Snippet "MySnippet" ["hello <#Title#>"], Snippet "Andris Snippet" ["bye <#Title#>"]]

allSnippets :: [Snippet]
allSnippets = sampleData

loadSnippet :: String -> IO Snippet
loadSnippet n = do
    let s = filter (\(Snippet name _) -> name == n ) allSnippets
    pure $ head s
