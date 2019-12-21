"" A Vim syntax highlighting file for Test/*.ztst
"
" See ../Util/zyodl.vim for installation instructions.
" Also, it's recommended to 'setlocal conceallevel=3 concealcursor=nc'.
"
" See B01cd.ztst for cases we cover

" TODO: Some zsh syntax isn't highlighted, e.g., «{ cd $0 }» doesn't highlight either 'cd' or '$0'
"   Apparently because the $VIMRUNTIME/syntax/zsh.vim zshBrackets group is defined as 'contains=TOP'?
"   https://bugs.debian.org/947120
" TODO: ZTST_unimplemented ZTST_skip aren't recognized everywhere
"   I haven't found yet a legitimate use where they aren't highlighted, but
"   they aren't highlighted in theoretical cases such as (( ++ZTST_skip )).
"   (This example is theoretical because those variables are string-typed.)

"" Boilerplate:
if exists("b:current_syntax")
  finish
endif
let s:cpo_save = &cpo
set cpo&vim

"" Syntax groups:
syn clear

syn include @zsh                   syntax/zsh.vim

syn match  ztstPayload             /^\s\+\zs.*/ contains=@zsh

syn match  ztstExitCode            /^\d\+\|^-/                nextgroup=ztstFlags
syn match  ztstFlags               /[.dDq]*:/       contained nextgroup=ztstTestName contains=ztstColon
syn match  ztstColon               /:/              contained
syn region ztstTestName            start=// end=/$/ contained 

syn match  ztstInputMarker         /^</                       nextgroup=ztstInput
syn region ztstInput               start=// end=/$/ contained

syn match  ztstOutputPattern       /^[*]>/                    nextgroup=ztstOutput   contains=ztstOutputPatternSigil,ztstOutputPatternMarker
syn match  ztstOutputPatternSigil  /[*]/            contained
syn match  ztstOutputPatternMarker /[>]/            contained conceal
syn match  ztstOutputLiteral       /^>/                       nextgroup=ztstOutput
syn region ztstOutput              start=// end=/$/ contained

syn match  ztstErrputPattern       /^[*][?]/                  nextgroup=ztstErrput   contains=ztstErrputPatternSigil,ztstErrputPatternMarker
syn match  ztstErrputPatternSigil  /[*]/            contained
syn match  ztstErrputPatternMarker /[?]/            contained conceal
syn match  ztstErrputLiteral       /^[?]/                     nextgroup=ztstErrput
syn region ztstErrput              start=// end=/$/ contained

syn match  ztstFrequentExplanationMarker /^F:/                nextgroup=ztstFrequentExplanation
syn region ztstFrequentExplanation start=// end=/$/ contained

syn match  ztstDirective           /^%.*/

syn match  ztstComment             /^#.*/

syn keyword ztstSpecialVariable ZTST_unimplemented ZTST_skip ZTST_testdir containedin=@zsh 

"" Highlight groups:
hi def link ztstExitCode                  Number
hi def link ztstFlags                     Normal
hi def link ztstColon                     Ignore
hi def link ztstTestName                  Title
hi def link ztstInput                     Normal
hi def link ztstInputMarker               Ignore
hi def link ztstOutput                    String
hi def link ztstOutputPatternSigil        Type
hi def link ztstOutputPatternMarker       Ignore
hi def link ztstOutputLiteral             Ignore
hi def link ztstErrput                    Identifier
hi def link ztstErrputPatternSigil        Type
hi def link ztstErrputPatternMarker       Ignore
hi def link ztstErrputLiteral             Ignore
hi def link ztstDirective                 Statement
hi def link ztstComment                   Comment
hi def link ztstFrequentExplanation       PreProc
hi def link ztstFrequentExplanationMarker Ignore
hi def link ztstSpecialVariable           Underlined

"" Boilerplate:
let b:current_syntax = "ztst"
let &cpo = s:cpo_save
unlet s:cpo_save
