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
filetype off

"""Start of Plug stuff
call plug#begin('~/.vim/plugged')
Plug 'ctrlpvim/ctrlp.vim'
Plug 'dense-analysis/ale', { 'for': 'clojure' }
Plug 'easymotion/vim-easymotion'
Plug 'ervandew/supertab'
Plug 'flazz/vim-colorschemes'
Plug 'guns/vim-sexp', { 'for': 'clojure' }
Plug 'haya14busa/incsearch-easymotion.vim'
Plug 'haya14busa/incsearch.vim'
Plug 'itchyny/vim-haskell-indent'
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'
Plug 'kien/rainbow_parentheses.vim'
Plug 'liquidz/vim-iced', {'for': 'clojure'}
Plug 'liquidz/vim-iced-coc-source', {'for': 'clojure'}
Plug 'liquidz/vim-iced-function-list', {'for': 'clojure', 'on': 'IcedBrowseFunction'}
Plug 'liquidz/vim-iced-project-namespaces', {'for': 'clojure', 'on': 'IcedBrowseNamespace'}
Plug 'mklabs/split-term.vim'
Plug 'ndmitchell/ghcid', { 'rtp': 'plugins/nvim' }
Plug 'neoclide/coc.nvim', {'branch': 'release'}
Plug 'neovimhaskell/haskell-vim'
Plug 'ntpeters/vim-better-whitespace'
Plug 'rizzatti/dash.vim'
Plug 'scrooloose/nerdtree'
Plug 'terryma/vim-multiple-cursors'
Plug 'tpope/vim-dispatch'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-sexp-mappings-for-regular-people', { 'for': 'clojure' }
Plug 'tpope/vim-surround'
Plug 'vhdirk/vim-cmake'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'vim-syntastic/syntastic'
Plug 'autozimu/LanguageClient-neovim', {
    \ 'branch': 'next',
    \ 'do': 'bash install.sh',
    \ }
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

"""Setup backup/swap dir
set backup
set swapfile
set backupdir=~/.vim-tmp
set directory=~/.vim-tmp

match ErrorMsg '\%>100v.\+'
match ErrorMsg '\s\+\%#\@<!$'

"""Syntastic
let g:syntastic_mode_map = { 'passive_filetypes': ['cpp'] }
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
let g:ale_linters = {'clojure': ['clj-kondo']}

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

"""LanguageClient
" See https://github.com/autozimu/LanguageClient-neovim
"""
nnoremap <F5> :call LanguageClient_contextMenu()<CR>
map <Leader>lk :call LanguageClient#textDocument_hover()<CR>
map <Leader>lg :call LanguageClient#textDocument_definition()<CR>
map <Leader>lr :call LanguageClient#textDocument_rename()<CR>
map <Leader>lf :call LanguageClient#textDocument_formatting()<CR>
map <Leader>lb :call LanguageClient#textDocument_references()<CR>
map <Leader>la :call LanguageClient#textDocument_codeAction()<CR>
map <Leader>ls :call LanguageClient#textDocument_documentSymbol()<CR><Paste>

hi link ALEError Error
hi Warning term=underline cterm=underline ctermfg=Yellow gui=undercurl guisp=Gold
hi link ALEWarning Warning
hi link ALEInfo SpellCap
let g:ale_set_highlights = 0
let g:ale_sign_error = 'x'
let g:ale_sign_warning = 'w'
let g:ale_lint_on_enter = 0
let g:LanguageClient_diagnosticsSignsMax = 0
let g:LanguageClient_diagnosticsEnable=0

"""Autocompletion
" Use 'Enter'-key to accept a suggestion.
"""
inoremap <expr> <cr> ((pumvisible())?("\<C-y>"):("\<cr>"))
let g:SuperTabDefaultCompletionType = "<C-X><C-O>"
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

"""Haskell
let g:ghcid_command = 'ghcid -c "stack ghci --no-test"'
let g:haskell_indent_disable = 1  " Use vim-haskell-indent instead
autocmd Filetype haskell setlocal expandtab tabstop=2 shiftwidth=2 softtabstop=2

"""Tabs
nnoremap <leader>tn :silent! tabn<CR>
nnoremap <leader>tp :silent! tabp<CR>
nnoremap <leader>to :silent! tabnew<CR>

"""NERDTree
nnoremap \n :NERDTreeToggle<CR>

"""Fzf
nnoremap \\ :silent! Buffers<CR>
nnoremap \f :silent! Files<CR>
nnoremap \l :silent! Lines<CR>
nnoremap \s :silent! Ag<CR>

"""Unhighlight last search on escape
nnoremap <Esc> :nohlsearch<CR><Esc>

"""Highlight rows which are too long
highlight OverLength ctermbg=red ctermfg=white guibg=#592929
match OverLength /\%101v.\+/

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

"Shorcuts for clojure filetype
autocmd VimEnter *.clj,*.cljc call iced#nrepl#auto_connect()
autocmd filetype clojure map <leader>cc <Plug>(iced_connect)
autocmd filetype clojure map <leader>cj <Plug>(iced_jack_in)
autocmd filetype clojure map <leader>ei <Plug>(iced_eval)<Plug>(sexp_inner_element)
autocmd filetype clojure map <leader>ee <Plug>(iced_eval)<Plug>(sexp_outer_list)
autocmd filetype clojure map <leader>en <Plug>(iced_eval_ns)
autocmd filetype clojure map <leader>et <Plug>(iced_eval_outer_top_list)
autocmd filetype clojure map <leader>er <Plug>(iced_eval_repl_visual)
autocmd filetype clojure map <leader>ep <Plug>(iced_print_last)
autocmd filetype clojure map <leader>eu <Plug>(iced_undef)
autocmd filetype clojure map <leader>eq <Plug>(iced_interrupt)
autocmd filetype clojure map <leader>eQ <Plug>(iced_interrupt_all)
autocmd filetype clojure map <leader>hd <Plug>(iced_document_open)
autocmd filetype clojure map <leader>hh <Plug>(iced_clojuredocs_open)
autocmd filetype clojure map <leader>hs <Plug>(iced_source_show)
autocmd filetype clojure map <leader>ss <Plug>(iced_stdout_buffer_open)
autocmd filetype clojure map <leader>sl <Plug>(iced_stdout_buffer_clear)
autocmd filetype clojure map <leader>sq <Plug>(iced_stdout_buffer_close)
autocmd filetype clojure map <leader>jb <Plug>(iced_def_back)
autocmd filetype clojure map <leader>jd <Plug>(iced_def_jump)
autocmd filetype clojure map <leader>jl <Plug>(iced_jump_to_let)
autocmd filetype clojure map <leader>== <Plug>(iced_format)
autocmd filetype clojure map <leader>=G <Plug>(iced_format_all)

"Shorcuts for cpp filetype
autocmd filetype cpp map <leader>cc :Dispatch! make -C build<CR>
autocmd filetype cpp map <leader>r :Term ./build/main<CR>
command! -nargs=+ Cppman silent! call system("tmux split-window cppman " . expand(<q-args>))
autocmd filetype cpp nnoremap <silent><buffer> H <Esc>:Cppman <cword><CR>
autocmd filetype cpp nnoremap <silent> K :call <SID>show_documentation()<CR>
autocmd filetype cpp setlocal signcolumn=yes

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

"Auto create non-existent dirs when creating a new file
function s:MkNonExDir(file, buf)
    if empty(getbufvar(a:buf, '&buftype')) && a:file!~#'\v^\w+\:\/'
        let dir=fnamemodify(a:file, ':h')
        if !isdirectory(dir)
            call mkdir(dir, 'p')
        endif
    endif
endfunction

function! s:show_documentation()
  if (index(['vim','help'], &filetype) >= 0)
    execute 'h '.expand('<cword>')
  else
    call CocAction('doHover')
  endif
endfunction

augroup BWCCreateDir
    autocmd!
    autocmd BufWritePre * :call s:MkNonExDir(expand('<afile>'), +expand('<abuf>'))
augroup END
