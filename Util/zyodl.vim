
"" A Vim syntax highlighting file for Doc/Zsh/*.yo

" To try this, run:
"     cd Doc/Zsh && vim --cmd "source ./.vimrc" zle.yo
" (This sources the file <Doc/Zsh/.vimrc>.)
"
" To install this permanently:
" 1. Copy this file to ~/.vim/syntax/zyodl.vim
" 2. Create ~/.vim/filetype.vim as explained in ":help new-filetype" case C.
" 3. Add the following command to ~/.vim/filetype.vim:
"      autocmd BufRead,BufNewFile **/Doc/Zsh/*.yo setfiletype zyodl

"" Test case:
"   texinode()()()()
"   chapter(foo)
"   vindex(foo) 
"   foo tt(foo) var(foo) bf(foo) em(foo) foo
"   xitem(foo)
"   item(foo)(foo)
"   sitem(foo)(foo foo)
"   example(print *.c+LPAR()#q:s/#%+LPAR()#b+RPAR()s+LPAR()*+RPAR().c/'S${match[1]}.C'/+RPAR())
"   ifzman(zmanref(zshmisc))ifnzman(noderef(Redirection))
"   LPAR()foo 42 foo+RPAR()
"
"   chapter(foo (foo) foo) # nested parentheses
"   sitem(foo)(foo tt(foo) foo) # nested underline

if exists("b:current_syntax")
  finish
endif

"" Syntax groups:
syn clear
syn cluster zyodlInline contains=zyodlTt,zyodlVar,zyodlBold,zyodlEmph,zyodlCond
syn region zyodlTt      start="\<tt("      end=")" contains=zyodlSpecial
syn region zyodlVar     start="\<var("     end=")" contains=zyodlSpecial
syn region zyodlBold    start="\<bf("      end=")" contains=zyodlSpecial
syn region zyodlEmph    start="\<em("      end=")" contains=zyodlSpecial
syn region zyodlIndex   start="\<.index("  end=")" contains=zyodlSpecial
syn match  zyodlSpecial "+\?\<\(LPAR\|RPAR\|PLUS\)()"
syn match  zyodlNumber  "\d\+"
syn region zyodlItem    start="\<xitem(" end=")" contains=zyodlSpecial,@zyodlInline
syn region zyodlItem    start="\<item("  end=")" contains=zyodlSpecial,@zyodlInline
syn region zyodlExample start="\<example(" end=")" contains=zyodlSpecial
syn region zyodlTitle   start="\<\(chapter\|subsect\|sect\)(" end=")" contains=zyodlSpecial,@zyodlInline
syn match  zyodlTitle   "^texinode(.*$"

syn region zyodlCond    start="\<\(ifzman\|ifnzman\)(" end=")" contains=zyodlRef,zyodlSpecial,@zyodlInline
syn region zyodlRef     start="\<\(zmanref\|noderef\)(" end=")"

syn keyword zyodlKeyword sitem nextgroup=zyodlSItemArg1
syn region zyodlSItemArg1 oneline start="(" end=")" contains=zyodlSpecial,@zyodlInline nextgroup=zyodlSItemArg2 contained
syn region zyodlSItemArg2 start="(" end=")" contains=zyodlSpecial,@zyodlInline contained

"" Highlight groups:
hi def link zyodlTt Constant
hi def link zyodlVar Identifier
" Not ':hi def link zyodlBold Bold' since there's no such group.
hi def zyodlBold gui=bold cterm=bold
hi def link zyodlEmph Type
hi def link zyodlIndex Comment
hi def link zyodlSpecial Special
hi def link zyodlNumber Number
hi def link zyodlItem Keyword
hi def link zyodlExample String
hi def link zyodlTitle Title
hi def link zyodlCond Conditional
hi def link zyodlRef Include
hi def link zyodlSItemArg1 Macro
hi def link zyodlSItemArg2 Underlined

let b:current_syntax = "zyodl"

