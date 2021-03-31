set runtimepath^=~/.vim runtimepath+=~/.vim/after
let &packpath = &runtimepath

call plug#begin('~/.vim/plugged')
Plug 'junegunn/vim-easy-align'
Plug 'junegunn/fzf'
Plug 'junegunn/fzf.vim'
Plug 'scrooloose/nerdtree', { 'on':  'NERDTreeToggle' }
Plug 'rafi/awesome-vim-colorschemes'
Plug 'tpope/vim-fugitive'
Plug 'airblade/vim-gitgutter'
Plug 'vim-airline/vim-airline'
Plug 'aklt/plantuml-syntax'
" Plug 'dense-analysis/ale'
Plug 'dag/vim-fish'
Plug 'neoclide/coc.nvim', {'branch': 'release'}
Plug 'jackguo380/vim-lsp-cxx-highlight'
Plug 'rhysd/vim-clang-format'

" Couldn't get by without this
Plug 'liuchengxu/vim-which-key', { 'on': ['WhichKey', 'WhichKey!'] }
" Commenting
Plug 'preservim/nerdcommenter'

" Syntax enhancements
Plug 'octol/vim-cpp-enhanced-highlight'
Plug 'rust-lang/rust.vim'
Plug 'vimwiki/vimwiki'
call plug#end()

syntax enable
filetype plugin indent on

" clang-format style
let g:clang_format#code_style = 'llvm'
autocmd FileType *.c,*.cpp,*.hpp,*.cc,*.h ClangFormatAutoEnable

" FZF config
let g:fzf_preview_window = ['up:60%', 'ctrl-/']
let g:fzf_layout = {'window': {'width': 0.9, 'height': 0.9}}

" TODO Need to remove any binary files (slx, slxt, etc...)
" P4rg for searching files in a changelist
command! -bang -nargs=* P4Rg
            \ call fzf#vim#grep(
            \   'rg --column --line-number --no-heading --color=always --smart-case -- '.shellescape(<q-args>).' '
            \   .'$(p4 opened | sed '.shellescape('s/\/\/mw\/\(mwpdb\|B.*\)\/\(.*\)#.*/\1/g', 1)
            \   .'| sed '.shellescape('s/%40/@/g', 1)
            \   .'| grep -v '.shellescape('"\.\(slx\|slxt\|mat\)$"')
            \   .')',1 ,fzf#vim#with_preview(g:fzf_preview_window[0], g:fzf_preview_window[1]), <bang>0)
" , {'options': '--prompt P4Rg>'}

command! -bang -nargs=* LinesWithPreview
            \ call fzf#vim#grep(
            \   'rg --with-filename --column --line-number --no-heading --color=always --smart-case . '.fnameescape(expand('%')), 1,
            \   fzf#vim#with_preview(g:fzf_preview_window[0], g:fzf_preview_window[1]), <bang>0)
nnoremap / :LinesWithPreview<CR>

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
set cindent
set cursorline
set textwidth=100
set colorcolumn=100

" disable comment continue
autocmd FileType * setlocal formatoptions-=cro

" set autochdir
set nohlsearch
set nu
colorscheme gruvbox
set bg=dark

let g:rustfmt_autosave = 1

" Bind space as the leader key
let g:mapleader = "\<Space>"
let g:maplocalleader = ','
nnoremap <silent> <leader>      :<c-u>WhichKey '<Space>'<CR>
nnoremap <silent> <localleader> :<c-u>WhichKey  ','<CR>
set timeoutlen=500

nnoremap <silent> <leader>tt :NERDTreeToggle<CR>
nnoremap <silent> <leader>tf :NERDTreeFocus<CR>
nnoremap <silent> <C-w>t :vsplit\|terminal<CR>
nnoremap <silent> <leader>ff :Files<CR>
nnoremap <silent> <leader>gr :Rg<CR>
nnoremap <silent> <leader>gp :P4Rg<CR>
nnoremap <silent> / :LinesWithPreview<CR>
nnoremap <silent> <leader>b :Buffers<CR>
nnoremap <silent> <leader>cw :let _s=@/<Bar>:%s/\s\+$//e<Bar>:let @/=_s<Bar><CR>

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

" Reasonable terminal behavior
augroup TermGroup
    autocmd!
    autocmd TermEnter * setlocal listchars= nonumber norelativenumber
    autocmd TermOpen * startinsert
augroup END
tnoremap <silent> <Esc> <C-\><C-n>:q<CR>
tnoremap <silent> <C-w>l <C-\><C-n><C-w>l
tnoremap <silent> <C-w>h <C-\><C-n><C-w>h
tnoremap <silent> <C-w>j <C-\><C-n><C-w>j
tnoremap <silent> <C-w>k <C-\><C-n><C-w>k

" ZML configuration
augroup ZMLGroup
    autocmd!
    autocmd BufRead *.zml setlocal formatoptions-=t
augroup END

" coc.nvim configuration
" TextEdit might fail if hidden is not set.
set hidden

" Some servers have issues with backup files, see #649.
set nobackup
set nowritebackup

" Give more space for displaying messages.
set cmdheight=1

" Having longer updatetime (default is 4000 ms = 4 s) leads to noticeable
" delays and poor user experience.
set updatetime=300

" Don't pass messages to |ins-completion-menu|.
set shortmess+=c

" Always show the signcolumn, otherwise it would shift the text each time
" diagnostics appear/become resolved.
if has("patch-8.1.1564")
    " Recently vim can merge signcolumn and number column into one
    set signcolumn=number
else
    set signcolumn=yes
endif

" Toggle comments with a single key
nmap <silent> ; <Plug>NERDCommenterInvert
vmap <silent> ; <Plug>NERDCommenterInvert

 "Use tab for trigger completion with characters ahead and navigate.
 "NOTE: Use command ':verbose imap <tab>' to make sure tab is not mapped by
 "other plugin before putting this into your config.

function! s:check_back_space() abort
    let col = col('.') - 1
    return !col || getline('.')[col - 1]  =~# '\s'
endfunction

nmap <silent> <C-p> <Plug>(coc-diagnostic-prev)
nmap <silent> <C-n> <Plug>(coc-diagnostic-next)

" GoTo code navigation.
nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gy <Plug>(coc-type-definition)
nmap <silent> gi <Plug>(coc-implementation)
nmap <silent> gr <Plug>(coc-references)

" Symbol renaming.
nmap <silent> <leader>rn <Plug>(coc-rename)

" Formatting selected code.
xmap <leader>fc  <Plug>(coc-format-selected)
nmap <silent> <leader>fc  <Plug>:ClangFormat<CR>

" Use K to show documentation in preview window.
nnoremap <silent> K :call <SID>show_documentation()<CR>

function! s:show_documentation()
    if (index(['vim','help'], &filetype) >= 0)
        execute 'h '.expand('<cword>')
    elseif (coc#rpc#ready())
        call CocActionAsync('doHover')
    else
        execute '!' . &keywordprg . " " . expand('<cword>')
    endif
endfunction

" Highlight the symbol and its references when holding the cursor.
autocmd CursorHold * silent call CocActionAsync('highlight')

" Add `:OR` command for organize imports of the current buffer.
command! -nargs=0 OR   :call     CocAction('runCommand', 'editor.action.organizeImport')

" Add (Neo)Vim's native statusline support.
" NOTE: Please see `:h coc-status` for integrations with external plugins that
" provide custom statusline: lightline.vim, vim-airline.
set statusline^=%{coc#status()}%{get(b:,'coc_current_function','')}

if filereadable(expand("~/.vimrc"))
    source ~/.vimrc
endif
