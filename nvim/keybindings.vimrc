" Bind space as the leader key
let g:mapleader = "\<Space>"
let g:maplocalleader = ','
nnoremap <silent> <leader>      :<c-u>WhichKey '<Space>'<CR>
nnoremap <silent> <localleader> :<c-u>WhichKey  ','<CR>

function s:vimrc()
    return '$HOME/.config/nvim/init.vim'
endfunction

nnoremap <silent> <leader>Q :qa<CR>
nnoremap <silent> <leader>q :q<CR>
nnoremap <silent> <leader>fs :w<CR>
nnoremap <silent> <leader>fS :w!<CR>
nnoremap <silent> <C-w>t :split\|terminal<CR>
nnoremap <silent> <leader>ff :Files<CR>
nnoremap <silent> <leader>gr :Rg<CR>
nnoremap <silent> <leader>gp :P4Rg<CR>
nnoremap <silent> <leader>ss :LinesWithPreview<CR>
nnoremap <silent> <leader>b :Buffers<CR>
nnoremap <silent> <leader>xdw :let _s=@/<Bar>:%s/\s\+$//e<Bar>:let @/=_s<Bar><CR>
nnoremap <silent> <leader>fed :execute 'edit '.<SID>vimrc()<CR>
nnoremap <silent> <leader>feR :execute 'source '.<SID>vimrc() \| echo 'config reloaded!'<CR>
nnoremap <silent> <leader>t :NERDTreeToggle<CR>

" Reasonable terminal behavior
augroup TermGroup
    autocmd!
    autocmd TermEnter * setlocal listchars= nonumber norelativenumber
    autocmd TermOpen * startinsert
augroup END
tnoremap <silent> <C-w>l <C-\><C-n><C-w>l
tnoremap <silent> <C-w>h <C-\><C-n><C-w>h
tnoremap <silent> <C-w>j <C-\><C-n><C-w>j
tnoremap <silent> <C-w>k <C-\><C-n><C-w>k

" Toggle comments with a single key
nmap <silent> ; <Plug>NERDCommenterInvert
vmap <silent> ; <Plug>NERDCommenterInvert
