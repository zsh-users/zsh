/*
 * text.c - textual representations of syntax trees
 *
 * This file is part of zsh, the Z shell.
 *
 * Copyright (c) 1992-1997 Paul Falstad
 * All rights reserved.
 *
 * Permission is hereby granted, without written agreement and without
 * license or royalty fees, to use, copy, modify, and distribute this
 * software and to distribute modified versions of this software for any
 * purpose, provided that the above copyright notice and the following
 * two paragraphs appear in all copies of this software.
 *
 * In no event shall Paul Falstad or the Zsh Development Group be liable
 * to any party for direct, indirect, special, incidental, or consequential
 * damages arising out of the use of this software and its documentation,
 * even if Paul Falstad and the Zsh Development Group have been advised of
 * the possibility of such damage.
 *
 * Paul Falstad and the Zsh Development Group specifically disclaim any
 * warranties, including, but not limited to, the implied warranties of
 * merchantability and fitness for a particular purpose.  The software
 * provided hereunder is on an "as is" basis, and Paul Falstad and the
 * Zsh Development Group have no obligation to provide maintenance,
 * support, updates, enhancements, or modifications.
 *
 */

#include "zsh.mdh"
#include "text.pro"

static char *tptr, *tbuf, *tlim;
static int tsiz, tindent, tnewlins, tjob;

static void
dec_tindent(void)
{
    DPUTS(tindent == 0, "attempting to decrement tindent below zero");
    if (tindent > 0)
	tindent--;
}

/* add a character to the text buffer */

/**/
static void
taddchr(int c)
{
    *tptr++ = c;
    if (tptr == tlim) {
	if (!tbuf) {
	    tptr--;
	    return;
	}
	tbuf = realloc(tbuf, tsiz *= 2);
	tlim = tbuf + tsiz;
	tptr = tbuf + tsiz / 2;
    }
}

/* add a string to the text buffer */

/**/
static void
taddstr(char *s)
{
    int sl = strlen(s);
    char c;

    while (tptr + sl >= tlim) {
	int x = tptr - tbuf;

	if (!tbuf)
	    return;
	tbuf = realloc(tbuf, tsiz *= 2);
	tlim = tbuf + tsiz;
	tptr = tbuf + x;
    }
    if (tnewlins) {
	memcpy(tptr, s, sl);
	tptr += sl;
    } else
	while ((c = *s++))
	    *tptr++ = (c == '\n' ? ' ' : c);
}

/**/
static void
taddlist(Estate state, int num)
{
    if (num) {
	while (num--) {
	    taddstr(ecgetstr(state, EC_NODUP, NULL));
	    taddchr(' ');
	}
	tptr--;
    }
}

/* add a newline, or something equivalent, to the text buffer */

/**/
static void
taddnl(void)
{
    int t0;

    if (tnewlins) {
	taddchr('\n');
	for (t0 = 0; t0 != tindent; t0++)
	    taddchr('\t');
    } else
	taddstr("; ");
}

/* get a permanent textual representation of n */

/**/
mod_export char *
getpermtext(Eprog prog, Wordcode c, int start_indent)
{
    struct estate s;

    if (!c)
	c = prog->prog;

    useeprog(prog);		/* mark as used */

    s.prog = prog;
    s.pc = c;
    s.strs = prog->strs;

    tindent = start_indent;
    tnewlins = 1;
    tbuf = (char *)zalloc(tsiz = 32);
    tptr = tbuf;
    tlim = tbuf + tsiz;
    tjob = 0;
    if (prog->len)
	gettext2(&s);
    *tptr = '\0';
    freeeprog(prog);		/* mark as unused */
    untokenize(tbuf);
    return tbuf;
}

/* get a representation of n in a job text buffer */

/**/
char *
getjobtext(Eprog prog, Wordcode c)
{
    static char jbuf[JOBTEXTSIZE];

    struct estate s;

    if (!c)
	c = prog->prog;

    useeprog(prog);		/* mark as used */
    s.prog = prog;
    s.pc = c;
    s.strs = prog->strs;

    tindent = 0;
    tnewlins = 0;
    tbuf = NULL;
    tptr = jbuf;
    tlim = tptr + JOBTEXTSIZE - 1;
    tjob = 1;
    gettext2(&s);
    *tptr = '\0';
    freeeprog(prog);		/* mark as unused */
    untokenize(jbuf);
    return jbuf;
}

/*
 * gettext2() shows one way to walk through the word code without
 * recursion. We start by reading a word code and executing the
 * action for it. Some codes have sub-structures (like, e.g. WC_FOR)
 * and require something to be done after the sub-structure has been
 * handled. For these codes a tstack structure which describes what
 * has to be done is pushed onto a stack. Codes without sub-structures
 * arrange for the next structure being taken from the stack so that
 * the action for it is executed instead of the one for the next
 * word code. If the stack is empty at this point, we have handled
 * the whole structure we were called for.
 */

typedef struct tstack *Tstack;

struct tstack {
    Tstack prev;
    wordcode code;
    int pop;
    union {
	struct {
	    LinkList list;
	} _redir;
	struct {
	    char *strs;
	    Wordcode end;
	} _funcdef;
	struct {
	    Wordcode end;
	} _case;
	struct {
	    int cond;
	    Wordcode end;
	} _if;
	struct {
	    int par;
	} _cond;
	struct {
	    Wordcode end;
	} _subsh;
    } u;
};

static Tstack tstack, tfree;

static Tstack
tpush(wordcode code, int pop)
{
    Tstack s;

    if ((s = tfree))
	tfree = s->prev;
    else
	s = (Tstack) zalloc(sizeof(*s));

    s->prev = tstack;
    tstack = s;
    s->code = code;
    s->pop = pop;

    return s;
}

/**/
static void
gettext2(Estate state)
{
    Tstack s, n;
    int stack = 0;
    wordcode code;

    while (1) {
	if (stack) {
	    if (!(s = tstack))
		return;
	    if (s->pop) {
		tstack = s->prev;
		s->prev = tfree;
		tfree = s;
	    }
	    code = s->code;
	    stack = 0;
	} else {
	    s = NULL;
	    code = *state->pc++;
	}
	switch (wc_code(code)) {
	case WC_LIST:
	    if (!s) {
		s = tpush(code, (WC_LIST_TYPE(code) & Z_END));
		stack = 0;
	    } else {
		if (WC_LIST_TYPE(code) & Z_ASYNC) {
		    taddstr(" &");
		    if (WC_LIST_TYPE(code) & Z_DISOWN)
			taddstr("|");
		}
		if (!(stack = (WC_LIST_TYPE(code) & Z_END))) {
		    if (tnewlins)
			taddnl();
		    else
			taddstr((WC_LIST_TYPE(code) & Z_ASYNC) ? " " : "; ");
		    s->code = *state->pc++;
		    s->pop = (WC_LIST_TYPE(s->code) & Z_END);
		}
	    }
	    if (!stack && (WC_LIST_TYPE(s->code) & Z_SIMPLE))
		state->pc++;
	    break;
	case WC_SUBLIST:
	    if (!s) {
                if (!(WC_SUBLIST_FLAGS(code) & WC_SUBLIST_SIMPLE) &&
                    wc_code(*state->pc) != WC_PIPE)
                    stack = -1;
		if (WC_SUBLIST_FLAGS(code) & WC_SUBLIST_NOT)
		    taddstr(stack ? "!" : "! ");
		if (WC_SUBLIST_FLAGS(code) & WC_SUBLIST_COPROC)
		    taddstr(stack ? "coproc" : "coproc ");
		s = tpush(code, (WC_SUBLIST_TYPE(code) == WC_SUBLIST_END));
	    } else {
		if (!(stack = (WC_SUBLIST_TYPE(code) == WC_SUBLIST_END))) {
		    taddstr((WC_SUBLIST_TYPE(code) == WC_SUBLIST_OR) ?
			    " || " : " && ");
		    s->code = *state->pc++;
		    s->pop = (WC_SUBLIST_TYPE(s->code) == WC_SUBLIST_END);
		    if (WC_SUBLIST_FLAGS(s->code) & WC_SUBLIST_NOT)
			taddstr("! ");
		    if (WC_SUBLIST_FLAGS(s->code) & WC_SUBLIST_COPROC)
			taddstr("coproc ");
		}
	    }
	    if (stack < 1 && (WC_SUBLIST_FLAGS(s->code) & WC_SUBLIST_SIMPLE))
		state->pc++;
	    break;
	case WC_PIPE:
	    if (!s) {
		tpush(code, (WC_PIPE_TYPE(code) == WC_PIPE_END));
		if (WC_PIPE_TYPE(code) == WC_PIPE_MID)
		    state->pc++;
	    } else {
		if (!(stack = (WC_PIPE_TYPE(code) == WC_PIPE_END))) {
		    taddstr(" | ");
		    s->code = *state->pc++;
		    if (!(s->pop = (WC_PIPE_TYPE(s->code) == WC_PIPE_END)))
			state->pc++;
		}
	    }
	    break;
	case WC_REDIR:
	    if (!s) {
		state->pc--;
		n = tpush(code, 1);
		n->u._redir.list = ecgetredirs(state);
	    } else {
		getredirs(s->u._redir.list);
		stack = 1;
	    }
	    break;
	case WC_ASSIGN:
	    taddstr(ecgetstr(state, EC_NODUP, NULL));
	    if (WC_ASSIGN_TYPE2(code) == WC_ASSIGN_INC) taddchr('+');
	    taddchr('=');
	    if (WC_ASSIGN_TYPE(code) == WC_ASSIGN_ARRAY) {
		taddchr('(');
		taddlist(state, WC_ASSIGN_NUM(code));
		taddstr(") ");
	    } else {
		taddstr(ecgetstr(state, EC_NODUP, NULL));
		taddchr(' ');
	    }
	    break;
	case WC_SIMPLE:
	    taddlist(state, WC_SIMPLE_ARGC(code));
	    stack = 1;
	    break;
	case WC_SUBSH:
	    if (!s) {
		taddstr("(");
		tindent++;
		taddnl();
		n = tpush(code, 1);
		n->u._subsh.end = state->pc + WC_SUBSH_SKIP(code);
		/* skip word only use for try/always */
		state->pc++;
	    } else {
		state->pc = s->u._subsh.end;
		dec_tindent();
		taddnl();
		taddstr(")");
		stack = 1;
	    }
	    break;
	case WC_CURSH:
	    if (!s) {
		taddstr("{");
		tindent++;
		taddnl();
		n = tpush(code, 1);
		n->u._subsh.end = state->pc + WC_CURSH_SKIP(code);
		/* skip word only use for try/always */
		state->pc++;
	    } else {
		state->pc = s->u._subsh.end;
		dec_tindent();
		taddnl();
		taddstr("}");
		stack = 1;
	    }
	    break;
	case WC_TIMED:
	    if (!s) {
		taddstr("time");
		if (WC_TIMED_TYPE(code) == WC_TIMED_PIPE) {
		    taddchr(' ');
		    tindent++;
		    tpush(code, 1);
		} else
		    stack = 1;
	    } else {
		dec_tindent();
		stack = 1;
	    }
	    break;
	case WC_FUNCDEF:
	    if (!s) {
		Wordcode p = state->pc;
		Wordcode end = p + WC_FUNCDEF_SKIP(code);

		taddlist(state, *state->pc++);
		if (tjob) {
		    taddstr(" () { ... }");
		    state->pc = end;
		    stack = 1;
		} else {
		    taddstr(" () {");
		    tindent++;
		    taddnl();
		    n = tpush(code, 1);
		    n->u._funcdef.strs = state->strs;
		    n->u._funcdef.end = end;
		    state->strs += *state->pc;
		    state->pc += 3;
		}
	    } else {
		state->strs = s->u._funcdef.strs;
		state->pc = s->u._funcdef.end;
		dec_tindent();
		taddnl();
		taddstr("}");
		stack = 1;
	    }
	    break;
	case WC_FOR:
	    if (!s) {
		taddstr("for ");
		if (WC_FOR_TYPE(code) == WC_FOR_COND) {
		    taddstr("((");
		    taddstr(ecgetstr(state, EC_NODUP, NULL));
		    taddstr("; ");
		    taddstr(ecgetstr(state, EC_NODUP, NULL));
		    taddstr("; ");
		    taddstr(ecgetstr(state, EC_NODUP, NULL));
		    taddstr(")) do");
		} else {
		    taddlist(state, *state->pc++);
		    if (WC_FOR_TYPE(code) == WC_FOR_LIST) {
			taddstr(" in ");
			taddlist(state, *state->pc++);
		    }
		    taddnl();
		    taddstr("do");
		}
		tindent++;
		taddnl();
		tpush(code, 1);
	    } else {
		dec_tindent();
		taddnl();
		taddstr("done");
		stack = 1;
	    }
	    break;
	case WC_SELECT:
	    if (!s) {
		taddstr("select ");
		taddstr(ecgetstr(state, EC_NODUP, NULL));
		if (WC_SELECT_TYPE(code) == WC_SELECT_LIST) {
		    taddstr(" in ");
		    taddlist(state, *state->pc++);
		}
		tindent++;
		taddnl();
		tpush(code, 1);
	    } else {
		dec_tindent();
		taddnl();
		taddstr("done");
		stack = 1;
	    }
	    break;
	case WC_WHILE:
	    if (!s) {
		taddstr(WC_WHILE_TYPE(code) == WC_WHILE_UNTIL ?
			"until " : "while ");
		tindent++;
		tpush(code, 0);
	    } else if (!s->pop) {
		dec_tindent();
		taddnl();
		taddstr("do");
		tindent++;
		taddnl();
		s->pop = 1;
	    } else {
		dec_tindent();
		taddnl();
		taddstr("done");
		stack = 1;
	    }
	    break;
	case WC_REPEAT:
	    if (!s) {
		taddstr("repeat ");
		taddstr(ecgetstr(state, EC_NODUP, NULL));
		taddnl();
		taddstr("do");
		tindent++;
		taddnl();
		tpush(code, 1);
	    } else {
		dec_tindent();
		taddnl();
		taddstr("done");
		stack = 1;
	    }
	    break;
	case WC_CASE:
	    if (!s) {
		Wordcode end = state->pc + WC_CASE_SKIP(code);

		taddstr("case ");
		taddstr(ecgetstr(state, EC_NODUP, NULL));
		taddstr(" in");

		if (state->pc >= end) {
		    if (tnewlins)
			taddnl();
		    else
			taddchr(' ');
		    taddstr("esac");
		    stack = 1;
		} else {
		    tindent++;
		    if (tnewlins)
			taddnl();
		    else
			taddchr(' ');
		    taddstr("(");
		    code = *state->pc++;
		    taddstr(ecgetstr(state, EC_NODUP, NULL));
		    state->pc++;
		    taddstr(") ");
		    tindent++;
		    n = tpush(code, 0);
		    n->u._case.end = end;
		    n->pop = (state->pc - 2 + WC_CASE_SKIP(code) >= end);
		}
	    } else if (state->pc < s->u._case.end) {
		dec_tindent();
		switch (WC_CASE_TYPE(code)) {
		case WC_CASE_OR:
		    taddstr(" ;;");
		    break;

		case WC_CASE_AND:
		    taddstr(";&");
		    break;

		default:
		    taddstr(";|");
		    break;
		}
		if (tnewlins)
		    taddnl();
		else
		    taddchr(' ');
		taddstr("(");
		code = *state->pc++;
		taddstr(ecgetstr(state, EC_NODUP, NULL));
		state->pc++;
		taddstr(") ");
		tindent++;
		s->code = code;
		s->pop = ((state->pc - 2 + WC_CASE_SKIP(code)) >=
			  s->u._case.end);
	    } else {
		dec_tindent();
		switch (WC_CASE_TYPE(code)) {
		case WC_CASE_OR:
		    taddstr(" ;;");
		    break;

		case WC_CASE_AND:
		    taddstr(";&");
		    break;

		default:
		    taddstr(";|");
		    break;
		}
		dec_tindent();
		if (tnewlins)
		    taddnl();
		else
		    taddchr(' ');
		taddstr("esac");
		stack = 1;
	    }
	    break;
	case WC_IF:
	    if (!s) {
		Wordcode end = state->pc + WC_IF_SKIP(code);

		taddstr("if ");
		tindent++;
		state->pc++;

		n = tpush(code, 0);
		n->u._if.end = end;
		n->u._if.cond = 1;
	    } else if (s->pop) {
		stack = 1;
	    } else if (s->u._if.cond) {
		dec_tindent();
		taddnl();
		taddstr("then");
		tindent++;
		taddnl();
		s->u._if.cond = 0;
	    } else if (state->pc < s->u._if.end) {
		dec_tindent();
		taddnl();
		code = *state->pc++;
		if (WC_IF_TYPE(code) == WC_IF_ELIF) {
		    taddstr("elif ");
		    tindent++;
		    s->u._if.cond = 1;
		} else {
		    taddstr("else");
		    tindent++;
		    taddnl();
		}
	    } else {
		s->pop = 1;
		dec_tindent();
		taddnl();
		taddstr("fi");
		stack = 1;
	    }
	    break;
	case WC_COND:
	    {
		static char *c1[] = {
		    "=", "!=", "<", ">", "-nt", "-ot", "-ef", "-eq",
		    "-ne", "-lt", "-gt", "-le", "-ge", "=~"
		};

		int ctype;

		if (!s) {
		    taddstr("[[ ");
		    n = tpush(code, 1);
		    n->u._cond.par = 2;
		} else if (s->u._cond.par == 2) {
		    taddstr(" ]]");
		    stack = 1;
		    break;
		} else if (s->u._cond.par == 1) {
		    taddstr(" )");
		    stack = 1;
		    break;
		} else if (WC_COND_TYPE(s->code) == COND_AND) {
		    taddstr(" && ");
		    code = *state->pc++;
		    if (WC_COND_TYPE(code) == COND_OR) {
			taddstr("( ");
			n = tpush(code, 1);
			n->u._cond.par = 1;
		    }
		} else if (WC_COND_TYPE(s->code) == COND_OR) {
		    taddstr(" || ");
		    code = *state->pc++;
		    if (WC_COND_TYPE(code) == COND_AND) {
			taddstr("( ");
			n = tpush(code, 1);
			n->u._cond.par = 1;
		    }
		}
		while (!stack) {
		    switch ((ctype = WC_COND_TYPE(code))) {
		    case COND_NOT:
			taddstr("! ");
			code = *state->pc++;
			if (WC_COND_TYPE(code) <= COND_OR) {
			    taddstr("( ");
			    n = tpush(code, 1);
			    n->u._cond.par = 1;
			}
			break;
		    case COND_AND:
			n = tpush(code, 1);
			n->u._cond.par = 0;
			code = *state->pc++;
			if (WC_COND_TYPE(code) == COND_OR) {
			    taddstr("( ");
			    n = tpush(code, 1);
			    n->u._cond.par = 1;
			}
			break;
		    case COND_OR:
			n = tpush(code, 1);
			n->u._cond.par = 0;
			code = *state->pc++;
			if (WC_COND_TYPE(code) == COND_AND) {
			    taddstr("( ");
			    n = tpush(code, 1);
			    n->u._cond.par = 1;
			}
			break;
		    case COND_MOD:
			taddstr(ecgetstr(state, EC_NODUP, NULL));
			taddchr(' ');
			taddlist(state, WC_COND_SKIP(code));
			stack = 1;
			break;
		    case COND_MODI:
			{
			    char *name = ecgetstr(state, EC_NODUP, NULL);

			    taddstr(ecgetstr(state, EC_NODUP, NULL));
			    taddchr(' ');
			    taddstr(name);
			    taddchr(' ');
			    taddstr(ecgetstr(state, EC_NODUP, NULL));
			    stack = 1;
			}
			break;
		    default:
			if (ctype < COND_MOD) {
			    /* Binary test: `a = b' etc. */
			    taddstr(ecgetstr(state, EC_NODUP, NULL));
			    taddstr(" ");
			    taddstr(c1[ctype - COND_STREQ]);
			    taddstr(" ");
			    taddstr(ecgetstr(state, EC_NODUP, NULL));
			    if (ctype == COND_STREQ ||
				ctype == COND_STRNEQ ||
				ctype == COND_REGEX)
				state->pc++;
			} else {
			    /* Unary test: `-f foo' etc. */ 
			    char c2[4];

			    c2[0] = '-';
			    c2[1] = ctype;
			    c2[2] = ' ';
			    c2[3] = '\0';
			    taddstr(c2);
			    taddstr(ecgetstr(state, EC_NODUP, NULL));
			}
			stack = 1;
			break;
		    }
		}
	    }
	    break;
	case WC_ARITH:
	    taddstr("((");
	    taddstr(ecgetstr(state, EC_NODUP, NULL));
	    taddstr("))");
	    stack = 1;
	    break;
	case WC_TRY:
	    if (!s) {
		taddstr("{");
		tindent++;
		taddnl();
		n = tpush(code, 0);
		state->pc++;
		/* this is the end of the try block alone */
		n->u._subsh.end = state->pc + WC_CURSH_SKIP(state->pc[-1]);
	    } else if (!s->pop) {
		state->pc = s->u._subsh.end;
		dec_tindent();
		taddnl();
		taddstr("} always {");
		tindent++;
		taddnl();
		s->pop = 1;
	    } else {
		dec_tindent();
		taddnl();
		taddstr("}");
		stack = 1;
	    }
	    break;
	case WC_END:
	    stack = 1;
	    break;
	default:
	    DPUTS(1, "unknown word code in gettext2()");
	    return;
	}
    }
}

/**/
void
getredirs(LinkList redirs)
{
    LinkNode n;
    static char *fstr[] =
    {
	">", ">|", ">>", ">>|", "&>", "&>|", "&>>", "&>>|", "<>", "<",
	"<<", "<<-", "<<<", "<&", ">&", NULL /* >&- */, "<", ">"
    };
    taddchr(' ');
    for (n = firstnode(redirs); n; incnode(n)) {
	Redir f = (Redir) getdata(n);

	switch (f->type) {
	case REDIR_WRITE:
	case REDIR_WRITENOW:
	case REDIR_APP:
	case REDIR_APPNOW:
	case REDIR_ERRWRITE:
	case REDIR_ERRWRITENOW:
	case REDIR_ERRAPP:
	case REDIR_ERRAPPNOW:
	case REDIR_READ:
	case REDIR_READWRITE:
	case REDIR_HERESTR:
	case REDIR_MERGEIN:
	case REDIR_MERGEOUT:
	case REDIR_INPIPE:
	case REDIR_OUTPIPE:
	    if (f->varid) {
		taddchr('{');
		taddstr(f->varid);
		taddchr('}');
	    } else if (f->fd1 != (IS_READFD(f->type) ? 0 : 1))
		taddchr('0' + f->fd1);
	    taddstr(fstr[f->type]);
	    if (f->type != REDIR_MERGEIN && f->type != REDIR_MERGEOUT)
		taddchr(' ');
	    if (f->type == REDIR_HERESTR &&
		(f->flags & REDIRF_FROM_HEREDOC)) {
		/*
		 * Strings that came from here-documents are converted
		 * to here strings without quotation, so add that
		 * now.  If tokens are present we need to do double quoting.
		 */
		if (!has_token(f->name)) {
		    taddchr('\'');
		    taddstr(quotestring(f->name, NULL, QT_SINGLE));
		    taddchr('\'');
		} else {
		    taddchr('"');
		    taddstr(quotestring(f->name, NULL, QT_DOUBLE));
		    taddchr('"');
		}
	    } else
		taddstr(f->name);
	    taddchr(' ');
	    break;
#ifdef DEBUG
	case REDIR_CLOSE:
	    DPUTS(1, "BUG: CLOSE in getredirs()");
	    taddchr(f->fd1 + '0');
	    taddstr(">&- ");
	    break;
	default:
	    DPUTS(1, "BUG: unknown redirection in getredirs()");
#endif
	}
    }
    tptr--;
}
