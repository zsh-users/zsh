# THE Z SHELL (ZSH)
## Version

This is version 5.10 of the shell.  This is a security and feature release.
There are several visible improvements since 5.9, as well as bug fixes.
All zsh installations are encouraged to upgrade as soon as possible.

Note in particular the changes highlighted under "Incompatibilities since
5.9" below.  See NEWS for more information.

## Installing Zsh
The instructions for compiling zsh are in the file [INSTALL](https://github.com/zsh-users/zsh/blob/master/INSTALL). You should
also **check the file** [**MACHINES**](https://github.com/zsh-users/zsh/blob/master/MACHINES) in the top directory to see if there
are any special instructions for your particular architecture.

Note in particular the zsh/newuser module that guides new users through
setting basic shell options without the administrator's intervention.  This
is **turned on** by default. See the section AUTOMATIC NEW USER CONFIGURATION
in [INSTALL](https://github.com/zsh-users/zsh/blame/master/INSTALL) for configuration information.

## Features
Zsh is a shell with lots of features.  For a list of some of these, see the
file [FEATURES](https://github.com/zsh-users/zsh/blob/master/FEATURES), and for the latest changes see [NEWS](https://github.com/zsh-users/zsh/blob/master/NEWS).  For more
details, see the documentation.

## Incompatibilities since 5.9
The line editor's default keymap is now the *emacs* keymap regardless of the
value of the environment variables `$VISUAL` and `$EDITOR`.  This only affects you
if your `$VISUAL` or `$EDITOR` environment variable is set to a value that
contains the string *vi*.  To get the previous behaviour, add
```Zsh
    bindkey -v
```
> or, if your `$VISUAL` and `$EDITOR` environment variables vary,
```Zsh
    if [[ ${VISUAL} == *vi* || ${EDITOR} == *vi* ]]; then
        bindkey -v
    else
        bindkey -e
    fi
```
to your *.zshrc* file.  These snippets are compatible with previous
versions of the shell.

The `ERR_EXIT` and `ERR_RETURN` options were refined to be more self-
consistent and better aligned with the POSIX-2017 specification of
`set -e`:
- Function calls or anonymous functions prefixed with `!` now never
  trigger exit or return. Negated function calls or anonymous
  functions used to trigger exit or return if ERR_EXIT or ERR_RETURN
  was set and the function call or anonymous function returned a
  zero exit status. 
- Example:
```Zsh
      setopt ERR_EXIT
      f() { true }
      ! f
      echo "This is printed only since 5.10."
```
- The `always` command now ignores `ERR_EXIT` and `ERR_RETURN`, as other
    complex commands do, if its exit status comes from a command
    executed while the option is ignored. 
- Example:
```Zsh
      setopt ERR_EXIT
      { false && true } always { echo "This was and still is printed." }
      echo "This is printed only since 5.10."
```
- Function calls, anonymous functions, and the `eval`, `.`, and
 `source` builtins now never ignore ERR_EXIT and ERR_RETURN on
  their own. These commands used to ignore ERR_EXIT and ERR_RETURN
  if their result came from a complex command (if, for, ...) whose
  result came from a command executed while the option is
  ignored. 
- Example:
```Zsh
      setopt ERR_EXIT
      f() { if true; then false && true; fi }
      f
      echo "This is printed only prior to 5.10."
```
- The `&&` and `||` operators now always ignore `ERR_RETURN` in their
  left operand. Until this version, the operators failed to ignored
  `ERR_RETURN` in their left operand if they were executed as part of
  a function call or an anonymous function that was itself executed
  in a context where `ERR_RETURN` is ignored. Example:
```Zsh
      setopt ERR_RETURN
      f() { { false; echo "This is printed only since 5.10." } || true }
      if f; then true; fi
```
*PCRE* support is now **PCRE2**.

Parameter names may begin with a **"."** and follow a relaxed implementation
of ksh namespace syntax.  Expansion of such parameters must use braces,
that is, in ${.param.name} form.  Parameters so named are excluded from
`typeset` and `set` output unless explicitly listed in `typeset` arguments
or matched by a pattern with `typeset -m`.

Interpretation of exclusion-patterns following alternation-patterns has
been rationalised.  This means for example that `[[ ab = (|a*)~^(*b) ]]`
is true where previously it was false.

Improvements to handling of terminal colors and attributes in prompts
may change the behavior of some prompt sequences, most notably in
cases where `esq=${(%)...}` is used to capture an escape sequence.

The `which` and `functions` commands output function definitions in a
format independent of the `MULTI_FUNC_DEF` option.

Math context no longer interprets a leading underscore as part of a
numeric constant.

Nul and characters greater than `\x77` are correctly handled by `read -d`.

Return values of `sysopen` from the zsh/system module have been updated
to be more similar to other commands in that module.

Tied parameters created with the zsh/db/gdbm module may not be re-tied
as locals in nested function scope.  This prevents database corruption
when a function scope ends.

Incompatibilities between 5.8.1 and 5.9
---------------------------------------

compinit: A "y" response to the "Ignore ... and continue?" prompt removes
insecure elements from the set of completion functions, where previously
it ignored the compaudit result and included all elements.

Build-time change: The default value of the --enable-gdbm configure
argument has changed from "yes" to "no".  Thus, the zsh/db/gdbm module will
not be built unless --enable-gdbm is passed explicitly.

vcs_info quilt: The value of the 'quiltcommand' style used to be taken for the
name of an external command.  Now it may also be a shell function.  Normal
command word precedence rules apply, so if you have a function and a command
with the same name, the function will be used.

The "function" reserved word, used to define functions, gained a new -T option.
That affects syntaxes such as:

1. "function -T { ... }".  It used to define a function named "-T".  It
now defines and executes an anonymous function with single-level tracing
enabled --- same as "function f { ... }; functions -T f; f", but without
naming the function.

2. "function -T foo { ... }".  It used to define two functions, named "-T"
and "foo" (see the MULTI_FUNC_DEF option).  It now defines a function
"foo" with tracing enabled.

3. "function -- { ... }".  It used to define a function named "--".  It
now defines and executes an anonymous function.  The "--" is taken to be
an end-of-options guard (same as "ls --").

The sh-compatible function definition syntax, "f() { ... }", is unchanged.

The time-out (-t) value given to zsh/system's `zsystem flock` command is
now limited to 2^30-1 seconds (= a little over 34 years).

zstyle: For background, recall that the zstyle builtin associates styles with
values for particular contexts, and when a context (such as ':foo:bar:baz') is
matched by multiple patterns (such as ':foo:*' and ':foo:bar:*'), the style's
value for the more specific of the patterns is used.  In zsh 5.8 and earlier
the determination of which pattern is "more specific" used semantics slightly
different to those the documentation promised.  The implementation was changed
to match the documentation.  The upshot of this is that if you set a single
style in multiple contexts, zsh 5.9 may use the values set for a pattern other
than the one zsh 5.8 used.  For example, if you do
    zstyle ':foo:bar:*'   style value1
    zstyle ':foo:*:baz:*' style value2
and the style is looked up under a context that both patterns match (e.g.,
:foo:bar:baz:qux), zsh 5.9 will use value2 -- which is consistent with the
documentation of both 5.8 and 5.9 -- but zsh 5.8 will use value1.  If this
affects you, make the implied colons in the first pattern explicit, as in:
    zstyle ':foo:bar:*:*' style value1
    zstyle ':foo:*:baz:*' style value2
This will use value1 in both 5.8 and 5.9.

Elements of the region_highlight array have gained a fourth space-separated
field.  Code written against 5.9 that sets the new field may break under 5.8:
for example, the element "0 20 bold memo=foo", which is valid under 5.9, would
not work under 5.8.  (Under the hood, 5.8 does not recognize the space as
terminating the highlighting specification.)  On the other hand, code that does
not set the new, fourth field will continue to work under both 5.8 and 5.9.
(As it happens, adding a comma after "bold" will make both 5.8 and 5.9 do the
right thing, but this should be viewed as an unsupported hack.)

The XTRACE option is now disabled while running user-defined completion
widgets.  See NEWS.

emulate sh: When zsh emulates sh, the final command in a pipeline is now run in
a subshell.  This differs from the behavior in the native (zsh) mode, but is
consistent with most other sh implementations.

The export and readonly builtins now ignore the -p option when there are
operands given and POSIX_BUILTINS is enabled. This more closely matches the
behaviour of bash and ksh.

getopts now calculates OPTIND in a similar manner to other shells when the
POSIX_BUILTINS option is enabled.

Ignored-signal traps are now inherited by subshells when the POSIX_TRAPS
option is enabled.

emulate sh: Inf and NaN are now treated as parameter names in arithmetic
context when zsh is emulating sh.

The ${name:offset:length} expansion syntax now behaves more similarly to
other shells in that the offset and length are applied as array indices
prior to scalar conversion in e.g. "${*:0:2}".

The optimization for the =(<<<foo) construct with no command, which
creates a temporary file with contents "foo", now adds a newline byte
after "foo" for consistency with the behaviour of the <<< redirection
everywhere else.

Incompatibilities between 5.7.1 and 5.8.1
-----------------------------------------

The history expansion !:1:t2 used to be interpreted such that the 2
was a separate character added after the history expansion.  Now
it is an argument to the :t modifier.

For example

% echo /my/interesting/path
% echo !:1:t2

used to echo "path2", but now echoes "interesting/path".

The behaviour of :h has similarly changed.

The behaviour has also changed in forms such as ${foo:t2) and *(:t2),
but in those cases the previous behaviour was not meaningful.

The vcs_info function VCS_INFO_quilt-dirfind now returns a string value
by setting $REPLY.  Previously it printed the value to standard output.
This only affects you if you override that function in your dotfiles.

The cd and chdir builtins no longer interpret operands like -1 and +2 as
stack entries when POSIX_CD is enabled.

Dropping privileges with `unsetopt privileged` may fail (with an error
message) on some older and uncommon platforms due to library dependency
changes made in the course of fixing CVE-2019-20044.  Please report this
to the zsh-workers mailing list if your system is affected.  See NEWS for
more.

PROMPT_SUBST expansion is no longer performed on arguments to prompt-
expansion sequences such as %F.

Incompatibilities between 5.6.2 and 5.7.1
-----------------------------------------

1) vcs_info git: The gen-unapplied-string hook receives the patches in
order (next to be applied first).  This is consistent with the hg
backend and with one of two contradictory claims in the documentation
(the other one has been corrected).  In zsh through 5.6.2, the patches
were passed in reverse order, next to be applied being last in the
array.

The gen-applied-string hook is unaffected; it still receives the patches in
reverse order, from last applied to first applied.

2) The option NO_UNSET now also applies when reading values from
variables without a preceding '$' sign in shell arithmetic expansion
and in the double-parentheses and 'let' arithmetic commands.

3) _alternative now correctly handles the same (...) action syntax as
_arguments; this may necessitate quoting/escaping in calls to _alternative
and _regex_arguments that wasn't previously required.  See
https://zsh.org/workers/48414 (commit zsh-5.8-348-g2c000ee7b) for details
and an example.

Incompatibilities between 5.5.1 and 5.6.2
-----------------------------------------

The completion helper _remote_files, typically used after a hostname
with scp-style completion, now uses remote-files instead of files as a
tag.  This makes it easier to restrict completions with the tag-order
style.

Incompatibilities between 5.4.2 and 5.5.1
-----------------------------------------

1) The default build-time maximum nested function depth has been
decreased from 1000 to 500 based on user experience.  However,
it can now be changed at run time via the variable FUNCNEST.
If you previously configured the shell to set a different value,
or to remove the check, this is now reflected in the default
value of the variable.

2) The syntax

foo=([key]=value)

can be used to set elements of arrays and associative arrays.  In the
unlikely event that you need to set an array by matching files using a
pattern that starts with a character range followed by '=', you need to
quote the '=', e.g.:

foo=([aeiou]\=vowel)

This is only required for array values contained within parentheses;
command line expansion for normal arguments has not changed.

3) The syntax

[[ -o foo ]]

where foo is not the name of a shell option (with optional underscores
and optional "no" prefix) used to be treated as a syntax error, i.e.,
the enclosing command line or file were aborted.  It now emits a warning
and returns a non-zero exit code.  For further details, see the
documentation of the -o switch in the chapter "Conditional Expressions"
in the zshmisc(1) manual.


Incompatibilities between 5.3.1 and 5.4.2
-----------------------------------------

1) The default behaviour of code like the following has changed:

  alias foo='noglob foo'
  foo() { print function body; }

When this is encountered in a start-up file, or other place where input
was read line by line, "foo" is in command position and is expanded as
an alias before the function definition takes place.  In previous
versions of the shell, this caused two functions "noglob" and "foo" to
be defined.  Any expansion of an alias in a function definition is
nearly always an unintended effect, as well as hard to detect, so has
been made an error.  (The option setting NO_MULTI_FUNC_DEF turned this
case into an error, but did not help with other cases and is off by
default.)  The alternative, of not expanding the alias, was rejected as
it was more difficult to achieve in the parser and also would silently
change the shell's behaviour between versions.  A new option,
ALIAS_FUNC_DEF, has been added, which can be set to make the shell
behave as in previous versions.  It is in any case recommended to use
the "function" keyword, as aliases are not expanded afterwards.

2) It was an undocumented, and largely useless, feature that a function
autoloaded with an absolute path was searched for along the normal fpath
(as if the leading / was missing) and, if found, loaded under the full
name including the leading slash.  This has been replaced with the more
useful feature that the function is searched for only at the given
absolute path; the name of the function is the base name of the file.
Note that functions including a non-leading / behave as before,
e.g. if `dir/name' is found anywhere under a directory in $fpath it is
loaded as a function named `dir/name'.

3) vcs_info: When neither a set-patch-format nor a gen-applied-string
(resp. gen-unapplied-string) hook is set, vcs_info now '%'-escapes the
applied-string (resp. unapplied-string) before interpolating it into the
patch-format string, to prevent literal `%' signs in the interpolated
value from being interpreted as prompt escape sequences.  If you use
${vcs_info_msg_0_} in a context other than the shell prompt, you may need
to undo the escaping with:

    print -v vcs_info_msg_0_ -Pr -- "${vcs_info_msg_0_}"

This is also needed if $vcs_info_msg_0_ is used to set $psvar.

4) functions executed by ZLE widgets no longer have their standard input
closed, but redirected from /dev/null instead. That still guards
against user defined widgets inadvertently reading from the tty device,
and addresses the antisocial behaviour of running a command with its
stdin closed.

5) [New between 5.4.1 and 5.4.2] In previous versions of the shell, the
following code:

    () { setopt err_return; false; echo 'oh no' } && true

printed "oh no", as the ERR_RETURN behaviour was suppressed when
a function was executed on the left hand side of an "&&" list.  This was
undocumented and inconvenient as it is generally more useful to consider
execution within a function in isolation from its environment.  The shell
now returns from the function on executing `false'.  (This is general
to all functions; an anonymous function is shown here for compactness.)


Incompatibilities between 5.0.8 and 5.3
----------------------------------------

1) In character classes delimited by "[" and "]" within patterns, whether
used for filename generation (globbing) or other forms of pattern
matching, it used not to be possible to quote "-" when used for a range,
or "^" and "!" when used for negating a character set.  The characters can
now be quoted by any of the standard shell means, but note that
the "[" and "]" must not be quoted.  For example,

  [[ $a = ['a-z'] ]]

matches if the variable a contains just one of the characters "a", "-"
or "z" only.  Previously this would have matched any lower case ASCII
letter.  Note therefore the useful fact that

  [[ $a = ["$cset"] ]]

matches any character contained in the variable "cset".  A consequence
of this change is that variables that should have active ranges need
(with default zsh options) to be indicated explicitly, e.g.

  cset="a-z"
  [[ b = [${~cset}] ]]

The "~" causes the "-" character to be active.  In sh emulation the
"~" is unnecessary in this example and double quotes must be used to
suppress the range behaviour of the "-".

2) The first argument to 'repeat' is now evaluated as an arithmetic
expression.  It was always documented to be an arithmetic expression, but
until now the decimal integer at the start of the value was used and the
remainder of the value discarded.  This could lead to different behaviour
if the argument contains non-numeric characters, or if the argument has
leading zeroes and the OCTAL_ZEROES option is set.

3) For some time the shell has had a POSIX_TRAPS option which determines
whether the EXIT trap has POSIX behaviour (the trap is only run at shell
exit) or traditional zsh behaviour (the trap is run once and discarded
when the enclosing function or shell exits, whichever happens first).
The use of this option has now been made "sticky" on the EXIT trap ---
in other words, the setting of the option at the point where the trap is
set now determines whether the trap has POSIX or traditional zsh
behaviour.  This means that changing the option after the trap was set
no longer has any effect.

Other aspects of EXIT trap handling have not changed --- there is still
only one EXIT trap at any point in a programme, so it is not generally
useful to combine POSIX and non-POSIX behaviour in the same script.

4) There was an undocumented feature dating from the early days of zsh
that glob qualifiers consisting only of the digits 0 to 7 were treated
as an octal file mode to "and" with the modes of files being tested.
This has been removed in order to be more sensitive to syntax errors.
The "f" qualifier has for many years been the documented way of testing
file modes; it allows the "and" test ("*(f+1)" is the documented
equivalent of "*(1)") as well as many other forms.

5) The completion helper function _arguments now escapes both backslashes
and colons in the values of option arguments when populating the $opt_args
associative array.  Previously, colons were escaped with a backslash but
backslashes were not themselves escaped with a backslash, which lead to
ambiguity: '-x foo\:bar' (one argument with a backslashed colon) and
'-x foo\\ bar' (two arguments, and the first one ends in a backslash) would
both set $opt_args[-x] to the same value.  This example assumes the -x
option's spec declared two arguments, as in:
    _arguments : -x:foo:${action}:bar:$action

For the more common case of non-repeatable options that take a single
argument, completion functions now have to unescape not only colons but
also backslashes when obtaining the option's argument from $opt_args.

6) Previously, if the function command_not_found_handler was run
in place of a command-not-found error, and the function returned
non-zero status, zsh set the status to 127 and printed an error message
anyway.  Now, the status from the handler is retained and no additional
message is printed.  The main reasons for this change are that it was not
possible to return a non-zero status to the parent shell from a command
executed as a replacement, and the new implementation is more consistent
with other shells.

7) The output of "typeset -p" (and synonyms) now takes into account the
function scope and export state of each parameter.  Exported parameters
are output as "export" commands unless the parameter is also local, and
other parameters not local to the scope are output with the "-g" option.
Previously, only "typeset" commands were output, never using "-g".

8) At spelling-correction prompt ($SPROMPT), where the choices offered are
[nyae], previously <Enter> would be accepted to mean [N] and <Space> and
<Tab> would be accepted to mean [Y].  Now <Space> and <Tab> are invalid
choices: typing either of them remains at the prompt.

9) The $ary[i,j] subscript syntax to take a slice of an array behaves
differently when both i and j are larger than the number of elements in
the array.  When i == j, such a slice always yields an empty array, and
when i < j it always yields an array of one empty string element.  The
following example illustrates how this differs from past versions.

     nargs() { print $# }
     a=(one two)
     for i in 1 2 3 4; do
      for j in 1 2 3 4 5; do
       print -n "$i $j => "
       nargs "${(@)a[i,j]}"
      done
     done
     
     5.2       |  5.3 **
     ----------+----------
     1 1 => 1  |  1 1 => 1
     1 2 => 2  |  1 2 => 2
     1 3 => 2  |  1 3 => 2
     1 4 => 2  |  1 4 => 2
     1 5 => 2  |  1 5 => 2
     2 1 => 0  |  2 1 => 0
     2 2 => 1  |  2 2 => 1
     2 3 => 1  |  2 3 => 1
     2 4 => 1  |  2 4 => 1
     2 5 => 1  |  2 5 => 1
     3 1 => 0  |  3 1 => 0
     3 2 => 0  |  3 2 => 0
     3 3 => 0  |  3 3 => 0
     3 4 => 0  |  3 4 => 1   **
     3 5 => 0  |  3 5 => 1   **
     4 1 => 0  |  4 1 => 0
     4 2 => 0  |  4 2 => 0
     4 3 => 0  |  4 3 => 0
     4 4 => 1  |  4 4 => 0   **
     4 5 => 1  |  4 5 => 1

The behaviour of the parameter flag (P) has changed when it appears
in a nested parameter group, in order to make it more useful in
such cases.  A (P) in the outermost parameter group behaves as
before.  See NEWS for more.

The default behaviour when text is pasted into an X Windows terminal has
changed significantly (unless you are using a very old terminal emulator
that doesn't support this mode).  Now, the new "bracketed paste mode"
treats all the pasted text as literal characters.  This means, in
particular, that a newline is simply inserted as a visible newline; you
need to hit Return on the keyboard to execute the pasted text in one go.
See the description of zle_bracketed_paste in the zshparams manual for
more.  "unset zle_bracketed_paste" restores the previous behaviour.

As noted in NEWS, the builtins declare, export, float, integer, local,
readonly and typeset now have corresponding reserved words that provide
true assignment semantics instead of an approximation by means of normal
command line arguments.  It is hoped that this additional consistency
provides a more natural interface.  However, compatibility with older
versions of zsh can be obtained by turning off the reserved word
interface, exposing the builtin interface:

  disable -r declare export float integer local readonly typeset

This is also necessary in the unusual eventuality that the builtins are
to be overridden by shell functions, since reserved words take
precedence over functions.

10) For compatibility with other shells, the syntax

array=([index]=value)

can be used with both associative arrays and normal arrays.  In the
unlikely event that you wish to create an array with an entry
matching a file whose name consists of one of a range of characters
matched as a [...] expression, followed by an equal sign, followed
by arbitrary other characters, it is now necessary to quote the equals
sign.

Incompatibilities between 5.0.7 and 5.0.8
-----------------------------------------

Various arithmetic operations have changed, in particular with respect
to the choice of integer or floating point operations.  The new
behaviour is intended to be more consistent, but is not compatible with
the old.

1) Previously, the modulus operation, `%', implicitly converted the
operation to integer and output an integer result, even if one
or both of the arguments were floating point.  Now, the C math
library fmod() operator is used to implement the operation where
one of the arguments is floating point.  For example:

Old behaviour:

% print $(( 5.5 % 2 ))
1

New behaviour:

% print $(( 5.5 % 2 ))
1.5


2) Previously, assignments to variables assigned the correct type to
variables declared as floating point or integer, but this type was
not propagated to the value of the expression, as a C programmer
would naturally expect.  Now, the type of the variable is propagated
so long as the variable is declared as a numeric type (however this
happened, e.g. the variable may have been implicitly typed by a
previous assignment).  For example:

Old behaviour:

% integer var
% print $(( var = 5.5 / 2.0 ))
2.75
% print $var
2

New behaviour:

% integer var
% print $(( var = 5.5 / 2.0 ))
2
% print $var
2


3) Previously, the FORCE_FLOAT option only forced the use of floating
point in arithmetic expressions for integer constants, i.e. numbers
typed directly into the expression, but not for variables.  Hence
an operation involving only integer variables (or string variables
containing integers) was not forced to be performed with floating point
arithmetic.  Now, operations involving variables are also forced to
floating point.  For example:

Old behaviour:

% unsetopt FORCE_FLOAT
% print $(( 1 / 2 ))
0
% integer i=1 j=2
% print $(( i / j ))
0
% setopt FORCE_FLOAT
% print $(( 1 / 2 ))
0.5
% print $(( i / j ))
0

New behaviour:

% unsetopt FORCE_FLOAT
% print $(( 1 / 2 ))
0
% integer i=1 j=2
% print $(( i / j ))
0
% setopt FORCE_FLOAT
% print $(( 1 / 2 ))
0.5
% print $(( i / j ))
0.5


4) The _git completion used to offer both local and remote heads under the
tag 'heads'.  The tag has now been split into 'heads-local' and
'heads-remote' in all contexts that existed in 5.0.7.  The --fixup/--squash
context still uses the tag 'heads' (but this may change in a future release).


Incompatibilities between 5.0.2 and 5.0.5
-----------------------------------------

The "zshaddhistory" hook mechanism documented in the zshmisc manual page
has been upgraded so that a hook returning status 2 causes a history
line to be saved on the internal history list but not written to the
history file.  Previously any non-zero status return would cause
the line not to be saved on the history at all.  It is recommended
to use status 1 for this (indeed most shell users would naturally do
so).

Incompatibilities between 5.0.0 and 5.0.2
-----------------------------------------

In 5.0.0, the new "sticky" emulation feature was applied to functions
explicitly declared within an expression following `emulate ... -c', but
did not apply to functions marked for autoload in that expression.  This
was not documented and experience suggests it was inconvenient, so in
5.0.2 autoloads also have the sticky property.

In other words,

  emulate zsh -c 'func() { ... }'

behaves the same way in 5.0.0 and 5.0.2, with the function func always being
run in native zsh emulation regardless of the current option settings.
However,

  emulate zsh -c 'autoload -Uz func'

behaves differently: in 5.0.0, func was loaded with the options in
effect at the point where it was first run, and subsequently run with
whatever options were in effect at that point; in 5.0.2, func is loaded
with native zsh emulation options and run with those same options.  This
is now the recommended way of ensuring a function is loaded and run with
a consistent set of options.

Note that the command `autoload -z' has never affected the options
applied when the function is loaded or run, only the effect of the
KSH_AUTOLOAD option at the point the function is loaded.

Possible incompatibilities between 4.2 and 5.0
----------------------------------------------

Here are some incompatibilities in the shell since the 4.2 series of
releases.  It is hoped most users will not be adversely affected by these.

In previous releases of the shell, builtin commands and precommand
modifiers that did not accept options also did not recognize the
argument "--" as marking the end of option processing without being
considered an argument.  This was not documented and was incompatible
with other shells.  All such commands now handle this syntax.

The configuration option --enable-lfs to enable large file support has
been replaced by autoconf's standard --enable-largefile mechanism.
As this is usually used whenever necessary, this won't usually
be noticeable; however, anyone configuring with --disable-lfs
should configure with --disable-largefile instead.

The configuration option --with-curses-terminfo has been replaced
by the option --with-term-lib="LIBS" where LIBS is a space-separated
list of libraries to search for termcap and curses features.

The option SH_WORD_SPLIT, used in Bourne/Korn/Posix shell compatibility
mode, has been made more like other shells in the case of substitutions of
the form ${1+"$@"} (a common trick used to work around problems in older
Bourne shells) or any of the related forms with the + replaced by - or =
with an optional colon preceding.  Previously, with SH_WORD_SPLIT in
effect, this expression would cause splitting on all white space in the
shell arguments.  (This was always regarded as a bug but was long-standing
behaviour.)  Now it is treated identically to "$@".  The same change
applies to expressions with forced splitting such as ${=1+"$@"}, but
otherwise the case where SH_WORD_SPLIT is not set is unaffected.

Debug traps (`trap ... DEBUG' or the function TRAPDEBUG) now run by default
before the command to which they refer instead of after.  This is almost
always the right behaviour for the intended purpose of debugging and is
consistent with recent versions of other shells.  The option
DEBUG_BEFORE_CMD can be unset to revert to the previous behaviour.

Previously, process substitutions of the form =(...), <(...) and >(...)
were only handled if they appeared as separate command arguments.
(However, the latter two forms caused the current argument to be
terminated and a new one started even if they occurred in the middle of
a string.)  Now all three may be followed by other strings, and the
latter two may also be preceded by other strings.  Remaining
limitations on their use (to reduce incompatibilities to a minimum)
are documented in the zshexpn.1 manual.

In previous versions of the shell it was possible to use index 0 in an
array or string subscript to refer to the same element as index 1 if the
option KSH_ARRAYS was not in effect.  This was a limited approximation to
the full KSH_ARRAYS handling and so was not very useful.  In this version
of the shell, this behaviour is only provided when the option
KSH_ZERO_SUBSCRIPT is set.  Note that despite the name this does not provide
true compatibility with ksh or other shells and KSH_ARRAYS should still be
used for that purpose.  By default, the option is not set; an array
subscript that evaluates to 0 returns an empty string or array element and
attempts to write to an array or string range including only a zero
subscript are treated as an error.  Writes to otherwise valid ranges that
also include index zero are allowed; hence for example the assignment
  array[(R)notfound,(r)notfound]=()
(where the string "notfound" does not match an element in $array) sets the
entire array to be empty, as in previous versions of the shell.
KSH_ZERO_SUBSCRIPT is irrelevant when KSH_ARRAYS is set.  Also as in previous
versions, attempts to write to non-existent elements at the end of an array
cause the array to be suitably extended.  This difference means that, for
example
  array[(R)notfound]=(replacement)
is an error if KSH_ZERO_SUBSCRIPT is not set (new behaviour), while
  array[(r)notfound]=(replacement)
causes the given value to be appended to the array (same behaviour as
previous versions).

The "exec" precommand modifier now takes various options for compatibility
with other shells.  This means that whereas "exec -prog" previously
tried to execute a command name "-prog", it will now report an error
in option handling.  "exec -- -prog" will execute "-prog".  If
the option EQUALS is set, as it is by default in zsh's native mode,
"exec =-prog" behaves the same way in all versions of zsh provided
the command can be found.

The "unset" builtin now does not regard the unsetting of non-existent
variables as an error, so can still return status 0 (depending on the
handling of other arguments).  This appears to be the standard shell
behaviour.

The variable BAUD is no longer set automatically by the shell.
In previous versions it was set to the baud rate reported by
the terminal driver in order to initialise the line editor's
compensation mechanism for slow baud rates.  However, the baud
rate so reported is very rarely related to the limiting speed of
screen updates on modern systems.  Users who need the compensation
mechanism should set BAUD to an appropriate rate by hand.

The variable HOME is no longer set by the shell if zsh is emulating any
other shell at startup; it must be present in the environment or set
subsequently by the user.  It is valid for the variable to be unset.

If the shell starts in a mode where it is emulating another shell
(typically because the base name of the shell was "sh" or another known
shell), the "repeat" syntax is not available by default, to avoid clashes
with external commands, but the "ulimit" command is available by default.
"limit", "sched" and "unlimit" are not available by default in such modes:
this has been the case for many versions but is now documented for the
first time.  (Users should note that emulation modes are not designed for
backwards compatibility with previous versions of zsh, but to maximise
compatibility with other shells, hence it is not safe to assume emulation
modes will behave consistently between zsh versions.)

Parameter substitutions in the form ${param//#%search/replace} match
against "search" anchored at both ends of the parameter value.  Previously
this syntax would have matched against "%search", anchored only at the head
of the value.  The form ${param//#$search/replace} where the value
$search starts with "%" considers the "%" to be part of the search
string as before.

Configure attempts to decide if multibyte characters are supported by the
system and if so sets the effect of --enable-multibyte, unless
--disable-multibyte was passed on the command line.  When
--enable-multibyte is in effect, the MULTIBYTE shell option is on by
default; this causes many operations to recognise characters in the current
locale.  (Most typically this is used for a UTF-8 character set but the
shell will work with any character set provided by the system where
individual octets are either US ASCII characters or have the top bit set.)
Older versions of the shell always assumed a character was one byte; this
remains the case if --disable-multibyte is in effect or if the MULTIBYTE
option is unset.  In some places the width of characters will be taken into
account where previously a raw string length was used; this is transparent
in calculations of screen position, but also occurs, for example, in
calculations of padding width.  Note that MULTIBYTE is not automatically
set when emulating Bourne- and POSIX-style shells; for interactive use of
these emulations it may be necessary to set it by hand.  Note also that the
option COMBINING_CHARS is not set by default due to difficulties detecting
the ability of the terminal to display combining characters correctly; MAC
users in particular will probably wish to set this option.

Zsh has previously been lax about whether it allows octets with the
top bit set to be part of a shell identifier.  Older versions of the shell
assumed all such octets were allowed in identifiers, however the POSIX
standard does not allow such characters in identifiers.  The older
behaviour is still obtained with --disable-multibyte in effect.
With --enable-multibyte in effect (see previous paragraph) there are three
possible cases:
  MULTIBYTE option unset:  only ASCII characters are allowed; the
    shell does not attempt to identify non-ASCII characters at all.
  MULTIBYTE option set, POSIX_IDENTIFIERS option unset: in addition
    to the POSIX characters, any alphanumeric characters in the
    local character set are allowed.  Note that scripts and functions that
    take advantage of this are non-portable; however, this is in the spirit
    of previous versions of the shell.  Note also that the options must
    be set before the shell parses the script or function; setting
    them during execution is not sufficient.
  MULITBYTE option set, POSIX_IDENTIFIERS set:  only ASCII characters
    are allowed in identifiers even though the shell will recognise
    alphanumeric multibyte characters.

The sched builtin now keeps entries in time order.  This means that
after adding an entry the index of an existing entry used for deletion
may change, if that entry had a later time than the new entry.  However,
deleting a entry with a later time will now never change the index of an
entry with an earlier time, which could happen with the previous method.

The completion style pine-directory must now be set to use completion
for PINE mailbox folders; previously it had the default ~/mail.  This
change was necessary because otherwise recursive directories under
~/mail were searched by default, which could be a considerable unnecessary
hit for anyone not using PINE.  The previous default can be restored with:
  zstyle ':completion:*' pine-directory ~/mail

The completion style fake-files now allows patterns as directories,
for example the value '/home/*:.snapshot' is now valid.  This will
only cause problems in the unlikely event that a directory in the style
has a pattern character in it.

The default maximum function depth (configurable with
--enable-max-function-depth) has been decreased to 1000 from 4096.  The
previous value was observed to be small enough that crashes still occurred
on some fairly common PC configurations.  This change is only likely to
affect some highly specialised uses of the shell.

The variables HISTCHARS and histchars now reject any attempt to
set non-ASCII characters for history or comments.  Multibyte characters
have never worked and the most consistent change was to restrict the
set to portable characters only.

Writers of add-on modules should note that the API has changed
significantly to allow user control of individual features provided by
modules.  See the documentation for zmodload -F and
Etc/zsh-development-guide, in that order.

Documentation
-------------

There are a number of documents about zsh in this distribution:

Doc/Zsh/*.yo	The master source for the zsh documentation is written in
		yodl.  Yodl is a document language written by Karel Kubat.
		It is not required by zsh but it is a nice program so you
		might want to get it anyway, especially if you are a zsh
		developer.  It can be downloaded from
		https://fbb-git.github.io/yodl/

Doc/zsh*.1	Man pages in nroff format.  These will be installed
		by "make install.man" or "make install".  By default,
		these will be installed in /usr/local/man/man1, although
		you can change this with the --mandir option to configure
		or editing the user configuration section of the top level
		Makefile.

Doc/zsh.texi	Everything the man pages have, but in texinfo format.  These
		will be installed by "make install.info" or "make install".
		By default, these will be installed in /usr/local/info,
		although you can change this with the --infodir option to
		configure or editing the user configuration section of the
		top level Makefile.  Version 4.0 or above of the
		Texinfo tools are recommended for processing this file.

Also included in the distribution are:

Doc/intro.ms	An introduction to zsh in troff format using the ms
		macros.  This document explains many of the features
		that make zsh more equal than other shells.
		Unfortunately this is based on zsh-2.5 so some examples
		may not work without changes but it is still a good
		introduction.

For more information, see the website, as described in the META-FAQ.

If you do not have the necessary tools to process these documents, PDF,
Info and DVI versions are available in the separate file zsh-doc.tar.gz at
the archive sites listed in the META-FAQ.

The distribution also contains a Perl script in Utils/helpfiles which
can be used to extract the descriptions of builtin commands from the
zshbuiltins manual page.  See the comments at the beginning of the
script about its usage.  The files created by this script can be used
by example function run-help located in the subdirectory Functions/Misc to
show information about zsh builtins and run `man' on external commands.
For this the shell variable HELPDIR should point to a directory containing
the files generated by the helpfiles script.  run-help should be
unaliased before loading the run-help function.  After that this function
will be executed by the run-help ZLE function which is by default bound
to ESC-h in emacs mode.

Examples
--------

Examples of zsh startup files are located in the subdirectory
StartupFiles.  Examples of zsh functions and scripts are located in
the subdirectory Functions.  Examples of completion control commands
(compctl) are located in the file Misc/compctl-examples.

Zsh FTP Sites, Web Pages, and Mailing Lists
-------------------------------------------

The current list of zsh FTP sites, web pages, and mailing lists can be
found in the META-FAQ.  A copy is included in this distribution and is
available separately at any of the zsh FTP sites.

Common Problems and Frequently Asked Questions
----------------------------------------------

Zsh has a list of Frequently Asked Questions (FAQ) maintained by Peter
Stephenson <pws@zsh.org>.  It covers many common problems encountered
when building, installing, and using zsh.  A copy is included in this
distribution in Etc/FAQ and is available separately at any of the zsh
ftp sites.

Zsh Maintenance and Bug Reports
-------------------------------

Zsh is currently maintained by the members of the zsh-workers mailing list
and coordinated by Peter Stephenson <coordinator@zsh.org>.  Please send
any feedback and bugs reports to <zsh-workers@zsh.org>.

Reports are most helpful if you can reproduce the bug starting zsh with
the -f option.  This skips the execution of local startup files except
/etc/zshenv.  If a bug occurs only when some options set try to locate
the option which triggers the bug.

There is a script "reporter" in the subdirectory Util which will print out
your current shell environment/setup.  If you cannot reproduce the bug
with "zsh -f", use this script and include the output from sourcing this
file.  This way, the problem you are reporting can be recreated.

The known bugs in zsh are listed in the file Etc/BUGS.  Check this as
well as the Frequently Asked Questions (FAQ) list before sending a bug
report.  Note that zsh has some features which are not compatible with
sh but these are not bugs.  Most of these incompatibilities go away
when zsh is invoked as sh or ksh (e.g. using a symbolic link).

If you send a bug report to the list and are not a subscriber, please
mention this in your message if you want a response.

If you would like to contribute to the development and maintenance of zsh,
then you should join the zsh-workers mailing list (check the META-FAQ
for info on this).  You should also read the "zsh-development-guide"
located in the subdirectory Etc.

Contributors
------------

The people who have contributed to this software project are listed
in Etc/CONTRIBUTORS.
