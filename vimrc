call plug#begin('~/.vim/plugged')
Plug 'junegunn/vim-easy-align'
Plug 'junegunn/fzf'
Plug 'junegunn/fzf.vim'
Plug 'scrooloose/nerdtree', { 'on':  'NERDTreeToggle' }
Plug 'rafi/awesome-vim-colorschemes'
Plug 'tpope/vim-fugitive'
Plug 'airblade/vim-gitgutter'
Plug 'vim-airline/vim-airline'
Plug 'dense-analysis/ale'

" Couldn't get by without this
Plug 'liuchengxu/vim-which-key', { 'on': ['WhichKey', 'WhichKey!'] }

" Commenting
Plug 'preservim/nerdcommenter'

" Syntax enhancements
Plug 'octol/vim-cpp-enhanced-highlight'
Plug 'rust-lang/rust.vim'
call plug#end()

syntax enable
filetype plugin indent on

" Syntax highlighting for vim-cpp-enhanced-highlight
let g:cpp_class_scope_highlight = 1
let g:cpp_experimental_template_highlight = 1

" Status lines for ALE: Set this. Airline will handle the rest.
let g:airline#extensions#ale#enabled = 1

" full features for rust.vim
filetype plugin indent on

:silent call system('mkdir -p $HOME/.vim/{undo,swapfiles,backups}')
if has('persistent_undo')
    set undodir=$HOME/.vim/undo
    set undofile
endif
set directory=$HOME/.vim/swapfiles//
set backupdir=$HOME/.vim/backups//

" tabstop:          Width of tab character
" softtabstop:      Fine tunes the amount of white space to be added
" shiftwidth        Determines the amount of whitespace to add in normal mode
" expandtab:        When this option is enabled, vi will use spaces instead of tabs
set tabstop     =4
set softtabstop =4
set shiftwidth  =4
set expandtab
set autoindent
set smartindent
set cursorline
set textwidth=90

" disable comment continue 
autocmd FileType * setlocal formatoptions-=cro

" set autochdir
set nohlsearch
set nu
colorscheme gruvbox

let g:rustfmt_autosave = 1

" Bind space as the leader key
let g:mapleader = "\<Space>"
let g:maplocalleader = ','
nnoremap <silent> <leader>      :<c-u>WhichKey '<Space>'<CR>
nnoremap <silent> <localleader> :<c-u>WhichKey  ','<CR>
set timeoutlen=500

nnoremap <silent> <leader>tt :NERDTreeToggle<CR>
nnoremap <silent> <leader>tf :NERDTreeFocus<CR>
nnoremap <silent> <C-w>t :vert :term<CR>
nnoremap <silent> <leader>fc :Files %:p:h<CR>
nnoremap <silent> <leader>ff :Files<CR>
nnoremap <silent> <leader>fg :Rg<CR>
nmap <silent> <C-k> <Plug>(ale_previous_wrap)
nmap <silent> <C-j> <Plug>(ale_next_wrap)
nnoremap <silent> <leader>fs :w<CR>
nnoremap <silent> <leader>fS :w!<CR>

" air-line fallbacks       
let g:airline_powerline_fonts = 1

if !exists('g:airline_symbols')
    let g:airline_symbols = {}
endif

" unicode symbols
let g:airline_left_sep = '»'
let g:airline_left_sep = '▶'
let g:airline_right_sep = '«'
let g:airline_right_sep = '◀'
let g:airline_symbols.linenr = '␊'
let g:airline_symbols.linenr = '␤'
let g:airline_symbols.linenr = '¶'
let g:airline_symbols.branch = '⎇'
let g:airline_symbols.paste = 'ρ'
let g:airline_symbols.paste = 'Þ'
let g:airline_symbols.paste = '∥'
let g:airline_symbols.whitespace = 'Ξ'

" airline symbols
let g:airline_left_sep = ''
let g:airline_left_alt_sep = ''
let g:airline_right_sep = ''
let g:airline_right_alt_sep = ''
let g:airline_symbols.branch = ''
let g:airline_symbols.readonly = ''
let g:airline_symbols.linenr = ''
