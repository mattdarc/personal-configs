set runtimepath^=~/.vim runtimepath+=~/.vim/after
let &packpath = &runtimepath

call plug#begin('~/.vim/plugged')
Plug 'junegunn/vim-easy-align'
Plug 'junegunn/fzf'
Plug 'junegunn/fzf.vim'
Plug 'rafi/awesome-vim-colorschemes'
Plug 'tpope/vim-fugitive'
Plug 'airblade/vim-gitgutter'
Plug 'vim-airline/vim-airline'
Plug 'aklt/plantuml-syntax'
Plug 'neoclide/coc.nvim', {'branch': 'release'}
Plug 'jackguo380/vim-lsp-cxx-highlight'
Plug 'rhysd/vim-clang-format'

" Couldn't get by without this
Plug 'liuchengxu/vim-which-key', { 'on': ['WhichKey', 'WhichKey!'] }
" Commenting
Plug 'preservim/nerdcommenter'
Plug 'preservim/nerdtree'
Plug 'qpkorr/vim-bufkill'
Plug 'luochen1990/rainbow'

" Syntax enhancements
Plug 'octol/vim-cpp-enhanced-highlight'
Plug 'rust-lang/rust.vim'
Plug 'vimwiki/vimwiki'
call plug#end()

"set to 0 if you want to enable it later via :RainbowToggle
let g:rainbow_active = 1

syntax enable
filetype plugin indent on

" clang-format style
let g:clang_format#code_style = 'llvm'
autocmd FileType *.c,*.cpp,*.hpp,*.cc,*.h ClangFormatAutoEnable

" disable comment continue
autocmd FileType * setlocal formatoptions-=cro

" Syntax highlighting for vim-cpp-enhanced-highlight
let g:cpp_class_scope_highlight = 1
let g:cpp_experimental_template_highlight = 1

" full features for rust.vim
filetype plugin indent on

:silent call system('mkdir -p $HOME/.vim/{undo,swapfiles,backups}')
if has('persistent_undo')
    set undodir=$HOME/.vim/undo
    set undofile
endif

set directory=$HOME/.vim/swapfiles//
set backupdir=$HOME/.vim/backups//
set tabstop     =4
set softtabstop =4
set shiftwidth  =4
set expandtab
set autoindent
set smartindent
set cindent
set cursorline
set textwidth=100
set colorcolumn=100
set formatoptions-=t
set ignorecase
set smartcase
set hlsearch
set timeoutlen=500

set autochdir
set nohlsearch
set nu
colorscheme gruvbox
set bg=dark

let g:rustfmt_autosave = 1

abbreviate eph edit %:p:h

" File explorer settings
" let g:netrw_banner = 0
" let g:netrw_liststyle = 3
" let g:netrw_browse_split = 4
" let g:netrw_winsize = 25
" let g:netrw_wiw = 1
" augroup AutoDeleteNetrwHiddenBuffers
"   au!
"   au FileType netrw setlocal bufhidden=wipe
" augroup end

autocmd BufWritePre *
    \ if '<afile>' !~ '^scp:' && !isdirectory(expand('<afile>:h')) |
        \ call mkdir(expand('<afile>:h'), 'p') |
    \ endif

let s:path = expand('<sfile>:p:h')
execute 'source '.s:path.'/autocorrect.vimrc'
execute 'source '.s:path.'/fzf.vimrc'
execute 'source '.s:path.'/keybindings.vimrc'
execute 'source '.s:path.'/coc.vimrc'
execute 'source '.s:path.'/airline.vimrc'

if filereadable(expand("~/.vimrc"))
    source ~/.vimrc
endif
