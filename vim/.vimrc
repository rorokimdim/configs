"""IMPORTANT. Do this first to setup vim-plug
" 1. Follow instructions in https://github.com/junegunn/vim-plug#installation.
"    (put the plug.vim file in ~/.vim/autoload directory)
"""

"""IMPORTANT. For python stuff, make sure to install the following.
" 1. flake8: `pip install flake8`
" (2. neovim: `pip install neovim`)
"""

"""IMPORTANT. Do this after finishing the above steps.
" 1. Open vim/nvim and run :PlugInstall
"""

let mapleader = ","
let maplocalleader = "\\"
set nocompatible
set clipboard=unnamed
filetype off

"""Start of Plug stuff
call plug#begin('~/.vim/plugged')
Plug 'ctrlpvim/ctrlp.vim'
Plug 'easymotion/vim-easymotion'
Plug 'flazz/vim-colorschemes'
Plug 'francoiscabrol/ranger.vim'
Plug 'guns/vim-sexp'
Plug 'haya14busa/incsearch-easymotion.vim'
Plug 'haya14busa/incsearch.vim'
Plug 'itchyny/vim-haskell-indent'
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'
Plug 'kien/rainbow_parentheses.vim'
Plug 'mklabs/split-term.vim'
Plug 'neovimhaskell/haskell-vim'
Plug 'ntpeters/vim-better-whitespace'
Plug 'rbgrouleff/bclose.vim'
Plug 'rizzatti/dash.vim'
Plug 'terryma/vim-multiple-cursors'
Plug 'tpope/vim-dispatch'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-sexp-mappings-for-regular-people'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'vim-syntastic/syntastic'
Plug 'Olical/conjure'
call plug#end()
"""End of Plug stuff

filetype plugin indent on
syntax on

set tabstop=4
set expandtab
set shiftwidth=4
set tw=120
set mouse=a
set ic
set incsearch
set ruler
set nofoldenable

"""Color scheme
colorscheme jellybeans
let g:lightline = {
\ 'colorscheme': 'jellybeans',
\ }

"""Setup backup/swap dir
set backup
set swapfile
set backupdir=~/.vim-tmp
set directory=~/.vim-tmp

match ErrorMsg '\%>100v.\+'
match ErrorMsg '\s\+\%#\@<!$'

"""Ranger
let g:ranger_map_keys = 0
let g:ranger_command_override = 'ranger --cmd "set show_hidden=true"'
nnoremap `` :RangerWorkingDirectoryExistingOrNewTab<CR>

"""Syntastic
let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 0
let g:syntastic_check_on_open = 0
let g:syntastic_check_on_wq = 0
highlight SyntasticError guibg=#2f0000
highlight SyntasticErrorSign guifg=white guibg=red
highlight SyntasticErrorLine guifg=white guibg=red
highlight SyntasticWarningLine guifg=white guibg=red
let g:syntastic_enable_signs = 1
nnoremap <leader>en :silent! lnext<CR>
nnoremap <leader>ep :silent! lprev<CR>

"""Ale
"Only run on save
let g:ale_lint_on_text_changed = 'never'
let g:ale_lint_on_insert_leave = 0

"""Rainbow parentheses everywhere
let g:rbpt_colorpairs = [
    \ ['brown',       'RoyalBlue3'],
    \ ['Darkblue',    'SeaGreen3'],
    \ ['darkgray',    'DarkOrchid3'],
    \ ['darkgreen',   'firebrick3'],
    \ ['darkcyan',    'RoyalBlue3'],
    \ ['darkred',     'SeaGreen3'],
    \ ['darkmagenta', 'DarkOrchid3'],
    \ ['brown',       'firebrick3'],
    \ ['gray',        'RoyalBlue3'],
    \ ['white',       'SeaGreen3'],
    \ ['darkmagenta', 'DarkOrchid3'],
    \ ['Darkblue',    'firebrick3'],
    \ ['darkgreen',   'RoyalBlue3'],
    \ ['darkcyan',    'SeaGreen3'],
    \ ['darkred',     'DarkOrchid3'],
    \ ['red',         'firebrick3'],
    \ ]
autocmd VimEnter * RainbowParenthesesToggle
autocmd Syntax * RainbowParenthesesLoadRound
autocmd Syntax * RainbowParenthesesLoadSquare
autocmd Syntax * RainbowParenthesesLoadBraces

"""Autocompletion
" Use 'Enter'-key to accept a suggestion.
"""
inoremap <expr> <cr> ((pumvisible())?("\<C-y>"):("\<cr>"))
autocmd InsertLeave * if pumvisible() == 0 | pclose | endif

"In insert/command mode use emacs keys
inoremap <C-a> <ESC>I
inoremap <C-e> <ESC>A
inoremap <C-b> <Left>
inoremap <C-h> <Left>
inoremap <C-f> <Right>
inoremap <C-l> <Right>

cnoremap <C-a> <ESC>I
cnoremap <C-e> <ESC>A
cnoremap <C-b> <Left>
cnoremap <C-h> <Left>
cnoremap <C-l> <Right>

"""Easymotion
map <Leader> <Plug>(easymotion-prefix)
nmap <Leader>, <Plug>(easymotion-overwin-f)
map  <Leader>f <Plug>(easymotion-bd-f)
nmap <Leader>f <Plug>(easymotion-overwin-f)
map <Leader>/ <Plug>(incsearch-easymotion-/)
map <Leader>? <Plug>(incsearch-easymotion-?)
map <Leader>l <Plug>(easymotion-lineforward)
map <Leader>j <Plug>(easymotion-j)
map <Leader>k <Plug>(easymotion-k)
map <Leader>h <Plug>(easymotion-linebackward)
let g:EasyMotion_startofline = 0 " keep cursor column when JK motion

"""C++
let g:syntastic_cpp_compiler = 'clang++'
let g:syntastic_cpp_compiler_options = ' -std=c++17 -stdlib=libc++'

"""Python
"(Remember to pip install flake8!)
let g:syntastic_python_checkers = ['flake8']

"""Tabs
nnoremap <leader>tn :silent! tabn<CR>
nnoremap <leader>tp :silent! tabp<CR>
nnoremap <leader>to :silent! tabnew<CR>

"""Fzf
nnoremap \\ :silent! Buffers<CR>
nnoremap \f :silent! Files<CR>
nnoremap \l :silent! Lines<CR>
nnoremap \s :silent! Ag<CR>

"""Unhighlight last search on escape
nnoremap <Esc> :nohlsearch<CR><Esc>

"""Highlight rows which are too long
highlight OverLength ctermbg=red ctermfg=white guibg=#592929
match OverLength /\%161v.\+/

"""Set highlight color for search
highlight Search cterm=NONE ctermfg=black ctermbg=white

"""Disable auto-wrapping long lines
set formatoptions-=t

"""Custom functions
func! DeleteTrailingWS()
    exe "normal mz"
    %s/\s\+$//ge
    exe "normal 'z"
endfunc
autocmd BufWrite * :call DeleteTrailingWS()

"With cursor on line call :Underline
function! s:Underline(chars)
    let chars = empty(a:chars) ? '-' : a:chars
    let nr_columns = virtcol('$') - 1
    let uline = repeat(chars, (nr_columns / len(chars)) + 1)
    put =strpart(uline, 0, nr_columns)
endfunction
command! -nargs=? Underline call s:Underline(<q-args>)

set hidden

"Special indent configuration for javascript
autocmd filetype javascript set sw=2
autocmd filetype javascript set ts=2
autocmd filetype javascript set sts=2

"Special indent configuration for css
autocmd filetype css set sw=2
autocmd filetype css set ts=2
autocmd filetype css set sts=2

"Special indent configuration for html
autocmd filetype html set sw=2
autocmd filetype html set ts=2
autocmd filetype html set sts=2

"Custom commands
command! Config execute ":e ~/.vimrc"
command! Reload execute "source ~/.vimrc"

"Auto create non-existent dirs when creating a new file
function s:MkNonExDir(file, buf)
    if empty(getbufvar(a:buf, '&buftype')) && a:file!~#'\v^\w+\:\/'
        let dir=fnamemodify(a:file, ':h')
        if !isdirectory(dir)
            call mkdir(dir, 'p')
        endif
    endif
endfunction

augroup BWCCreateDir
    autocmd!
    autocmd BufWritePre * :call s:MkNonExDir(expand('<afile>'), +expand('<abuf>'))
augroup END
