module Plugin.Examples where
import Plugin.Types (Snippet(..), SnippetMetaData (..), Placeholder (..), PlaceholderState (..))

mySnippet = Snippet "mySnippet" [
      "asdfasdf <#greeting#> wie gehts <#goodbye#> asdfasdf", 
      "asdfwejlasdfiij", 
      "<#goodbye#>"
     ] 
     (SnippetMetaData ["haskell"])

qs = ("<#", "#>")

placeholdersInSnippet = [Placeholder "greeting" (Just "hello world"), Placeholder "goodbye" (Just "bye world")]
state = PS mySnippet qs placeholdersInSnippet


