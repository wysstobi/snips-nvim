# Snippet Plugin for Neovim

[[_TOC_]]

## About the Project
This project is part of the module "fprod" at the FHNW in Burgg/Windisch.
The plugin makes it possible to easily save text or code snippets and to plot 
them at a given place if needed. 
For this purpose, a simple snippet management is provided, 
which offers the possibility to select among the available snippets. 
The snippets are persisted in external files.


## Build With
- [Neovim](https://neovim.io/)

## Getting started

### Prerequisites

Make sure `neovim` is installed.

### Installation

Install using your favorite plugin manager ([vim-plug](https://github.com/junegunn/vim-plug))

```vimL
Plug 'neovimhaskell/nvim-hs.vim'
```

## Usage
1. Clone this Git Repository
2. Create a file that is loaded by neovim on startup an add the following content:
```vimL
call nvimhs#start('PATH/TO/YOUR/REPO', 'snippet-plugin', [])
```
3. Run `stack build` in your repository's folder
4. Test the installation: run `:echo FprodTeam()`, should print the names of the team

## Acknowledgments
* [Haskell plugin backend for neovim](https://hackage.haskell.org/package/nvim-hs)
* [nvim-hs.vim](https://github.com/neovimhaskell/nvim-hs.vim)


## Contact
- Tobias Wyss - tobias.wyss@students.fhw.ch
- Raphael Lüthy - tobias.wyss@students.fhw.ch
- Andri Wild - andri.wild@students.fhnw.ch 
