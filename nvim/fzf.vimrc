" FZF config
let g:fzf_preview_window = ['up:60%', 'ctrl-/']
let g:fzf_layout = {'window': {'width': 0.9, 'height': 0.9}}

command! -bang -nargs=* LinesWithPreview
            \ call fzf#vim#grep(
            \   'rg --with-filename --column --line-number --no-heading --color=always --smart-case . '.fnameescape(expand('%')), 1,
            \   fzf#vim#with_preview(g:fzf_preview_window[0], g:fzf_preview_window[1]), <bang>0)

