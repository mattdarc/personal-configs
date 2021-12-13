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
Plug 'nvim-treesitter/nvim-treesitter'
Plug 'nvim-orgmode/orgmode'
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

set nohlsearch
set number relativenumber
colorscheme gruvbox
set bg=dark

let g:rustfmt_autosave = 1

abbreviate eph edit %:p:h

" function! json_format#format() abort
"     let source = join(getline(1, '$'), "\n")
"     return s:system(printf('echo %s | jq -M .', source))
" endfunction

autocmd BufWritePre *
    \ if '<afile>' !~ '^scp:' && !isdirectory(expand('<afile>:h')) |
        \ call mkdir(expand('<afile>:h'), 'p') |
    \ endif

let s:path = expand('<sfile>:p:h')
function! s:source_relative (file)
    execute 'source '.s:path.'/'.a:file
endfunction

call s:source_relative ('autocorrect.vimrc')
call s:source_relative ('fzf.vimrc')
call s:source_relative ('keybindings.vimrc')
call s:source_relative ('coc.vimrc')
call s:source_relative ('airline.vimrc')
call s:source_relative ('org.vimrc')

if filereadable(expand("~/.vimrc"))
    source ~/.vimrc
endif

" OCaml setup
" let g:opamshare = substitute(system('opam config var share'),'\n$','','''')
" execute "set rtp+=" . g:opamshare . "/merlin/vim"
