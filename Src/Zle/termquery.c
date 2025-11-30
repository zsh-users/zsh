/*
 * termquery.c - terminal feature probes
 *
 * This file is part of zsh, the Z shell.
 *
 * Copyright (c) 2025 Oliver Kiddle
 * All rights reserved.
 *
 * Permission is hereby granted, without written agreement and without
 * license or royalty fees, to use, copy, modify, and distribute this
 * software and to distribute modified versions of this software for any
 * purpose, provided that the above copyright notice and the following
 * two paragraphs appear in all copies of this software.
 *
 * In no event shall Oliver Kiddle or the Zsh Development Group be liable
 * to any party for direct, indirect, special, incidental, or consequential
 * damages arising out of the use of this software and its documentation,
 * even if Oliver Kiddle and the Zsh Development Group have been advised of
 * the possibility of such damage.
 *
 * Oliver Kiddle and the Zsh Development Group specifically disclaim any
 * warranties, including, but not limited to, the implied warranties of
 * merchantability and fitness for a particular purpose.  The software
 * provided hereunder is on an "as is" basis, and Oliver Kiddle and the
 * Zsh Development Group have no obligation to provide maintenance,
 * support, updates, enhancements, or modifications.
 *
 */

#include "zle.mdh"
#include "termquery.pro"

/* On a 9600 serial link, 0.2 seconds is about the minimum to safely work.
 * 0.5s should be generous enough. We only incur this delay on a terminal
 * that doesn't respond to device attributes requests. */
#define TIMEOUT -51L

#define TAG     (1 << 7)
#define SEQ     (TAG | (1 << 6))

/* tags */
#define T_BEGIN    0x80  /* group start */
#define T_END      0x81  /* group end */
#define T_OR       0x82  /* alternation (within group) */
#define T_REPEAT   0x83  /* repeat preceding block */
#define T_NUM      0x84  /* decimal number, defaults to 0 if absent */
#define T_HEX      0x85  /* hexadecimal digit used as part of a number */
#define T_HEXCH    0x86  /* hexadecimal digit that is thrown away */
#define T_WILDCARD 0x87  /* match any character */
#define T_RECORD   0x88  /* start text capture */
#define T_CAPTURE  0x89  /* end text capture */
#define T_DROP     0x91  /* drop input + restart without matching a sequence */
#define T_CONTINUE 0x92  /* when matching don't go back to first state */
#define T_NEXT     0x94  /* advance to next stored number */

typedef const unsigned char seqstate_t;

/* macros for entries in the parse state table */
#define OR  "\x82"
#define NUM "\x84"
#define HEX "\x85"
#define HEXCH "\x86"
#define WILDCARD "\x87"
#define RECORD "\x88"
#define CAPTURE "\x89"
#define EITHER(group) "\x80" group "\x81"
#define OPT(opt) EITHER( opt OR EITHER() )
#define REPEAT(r) EITHER( r ) "\x83" /* would use OPT() but done in C code */
/* Sequence completion tags and other actions need to precede the state
 * where the final character is matched. This macro just reverses the
 * order so that they come in a more logical order. */
#define MATCH(ch, arg) arg ch
#define DROP  "\x91"
#define CONTINUE "\x92"
#define NEXT "\x94"
#define DA    "\xc0"
#define COLOR "\xc1"
#define KITTY "\xc2"
#define TINFO "\xc3"
#define XTID  "\xc4"
#define XTVER "\xc5"
#define CLIP  "\xc6"

/* Deterministic finite state automata for parsing terminal response sequences.
 * - alternatives decided by the first character (no back-tracking)
 * - matching literal characters is 7-bit ASCII only
 */
#define QUERY_STATES \
    "\033" \
    EITHER( /* default terminal colours */ \
	"]1" NUM ";" \
	EITHER( "rgb:" \
	    HEX OPT( HEX ) OPT( HEXCH HEXCH ) "/" \
	    HEX OPT( HEX ) OPT( HEXCH HEXCH ) "/" \
	    HEX OPT( HEX ) OPT( HEXCH HEXCH ) \
	OR "#" \
	    HEX HEX NEXT HEX HEX NEXT HEX HEX ) \
	EITHER( \
	    MATCH("\033", COLOR CONTINUE ) \
	    MATCH("\\", DROP) /* urxvt 9.31 has bug: it omits the backslash */ \
	OR  MATCH("\007", COLOR ) ) \
    OR "P" /* DCS */ \
	EITHER( /* terminal name and version */ \
	    ">|" RECORD \
	    REPEAT( \
		CAPTURE EITHER( MATCH("(", XTID CONTINUE) \
		    OR MATCH(" ", XTID CONTINUE) ) RECORD \
	    OR \
		CAPTURE OPT( ")" ) "\033" MATCH( "\\", XTVER ) \
	    OR \
		WILDCARD \
	    ) \
	OR /* 24-bit colour support */ \
	    EITHER( \
		"0+r" OPT( "524742" ) /* mlterm responds without numbers */ \
		"\033" MATCH("\\", DROP) /* kitty does 24-bit but 0 => no */ \
	    OR  "1+r524742=" REPEAT( HEXCH HEXCH ) /* hex encoded bytes */ \
		"\033" MATCH("\\", TINFO) )) /* any value => truecolor */ \
    OR /* keyboard protocol and device attributes */ \
	"[?" \
	REPEAT( NUM \
	    EITHER( ";" \
	    OR MATCH("u", KITTY ) \
	    OR MATCH("c", DA) )))

#define OSC52_STATES \
    "\033]52;" EITHER("p" OR "c") ";" RECORD \
    REPEAT( \
	CAPTURE EITHER( "\033" MATCH("\\", CLIP) OR MATCH("\007", CLIP) ) \
    OR WILDCARD )

static char *EXTVAR  = ".term.extensions";
static char *IDVAR   = ".term.id";
static char *VERVAR  = ".term.version";
static char *COLORVAR[]  = { ".term.fg", ".term.bg", ".term.cursor" };
static char *MODEVAR = ".term.mode";
static char *WAITVAR = ".term.querywait";

/* Query sequences
 * using ESC\\ as ST instead of BEL because the bell was emitted on
 * old xterm. */

/* Terminal default colors. Probably best that these are queried first
 * because tmux will need to pass these on. */
#define TQ_BGCOLOR "\033]11;?\033\\"
#define TQ_FGCOLOR "\033]10;?\033\\"
#define TQ_CURSOR  "\033]12;?\033\\"

/* Kitty / fixterms keyboard protocol which allows wider support for keys
 * and modifiers. This clears the screen in terminology. */
#define TQ_KITTYKB "\033[?u"

/* Query for 24-bit color support, This is an XTTERMCAP sequence which with
 * some terminals allows terminfo entries to be retrieved. */
#define TQ_RGB "\033P+q524742\033\\"

/* Query terminal name and version which is useful with terminals that
 * lie in $TERM so that they work without terminfo entries. */
#define TQ_XTVERSION "\033[>0q"

/* Device attributes, Response isn't especially interesting but we get a
 * response from most terminals so by putting this last we can short-circuit
 * the time delay waiting for less well-supported responses that might never
 * come.
 * Following two spaces + CR works to clear fragments of sequences that
 * appeared. GNU screen is an example terminal that needs this. */
#define TQ_DA "\033[c  \r"

static seqstate_t*
find_branch(seqstate_t* pos)
{
    int nested = 0;
    seqstate_t* cur = pos + 1;

    for (; *cur && (nested || (*cur != T_END && *cur != T_OR)); cur++) {
	if (*cur == T_BEGIN)
	    nested++;
	else if (*cur == T_END)
	    nested--;
    }
    return cur;
}

static seqstate_t*
find_matching(seqstate_t* pos, int direction)
{
    int nested = 1;
    seqstate_t* cur = pos + direction;

    for (; *cur && nested; cur += direction) {
	if (*cur == T_BEGIN && !(nested += direction)) {
	    break; /* going backward, stop on begin */
	} else if (*cur == T_END)
	    nested -= direction;
    }
    return cur;
}

static void
probe_terminal(const char *tquery, seqstate_t *states,
	void (*handle_seq) (int seq, int *numbers, int len,
	    char *capture, int clen, void *output), void *output)
{
    size_t blen = 256, nlen = 16;
    char *buf = zhalloc(blen);
    char *start = buf, *current = buf, *illgotten = buf;
    size_t record = 0, capture = 0;
    int *numbers = hcalloc(nlen * sizeof(int));
    int *num = numbers;
    int finish = 0, number = 0;
    int ch;
    struct ttyinfo ti, torig;
    struct value vbuf;
    Value v = getvalue(&vbuf, &WAITVAR, 0);
    long timeout = v ? -1 - getintvalue(v) : TIMEOUT;

    if (timeout == -1)
	timeout = -((long)1 << (sizeof(int)*8-11))*100;

    seqstate_t *curstate = states;

    gettyinfo(&ti);
    memcpy(&torig, &ti, sizeof(torig));
#ifdef HAS_TIO
    ti.tio.c_lflag &= (~ECHO & ~ICANON);
    ti.tio.c_iflag &= ~ICRNL;
#else
    ti.sgttyb.sg_flags &= ~ECHO;
    ti.sgttyb.sg_flags |= CBREAK;
#endif
    settyinfo(&ti);

    write_loop(SHTTY, tquery, strlen(tquery));
    notify_pwd(); /* unrelated to the function's main purpose */

    while (!finish && *curstate) {
	int consumed = 0; /* whether an input token has been matched */
	int branches = 1; /* count of untried paths encountered */
	int triedstart = curstate == states; /* current char tried at start */
	unsigned char action = 0, sequence = 0;

	if (illgotten < current) {
	    ch = *illgotten++;
	} else {
	    if (current == buf + blen) {
		current = hrealloc(buf, blen, blen * 2);
		illgotten = current + (illgotten - buf);
		start = current + (start - buf);
		buf = current;
		current = buf + blen;
		memset(current, 0, blen);
		blen *= 2;
	    }
            ch = getbyte(timeout, 0, 1);
	    if (errflag) {
		errflag = 0;
		break;
	    }
	    if (ch == EOF)
		break;
	    *current++ = ch;
	    illgotten = current;
	}

	while (!consumed && branches >= 1 && *curstate) {
	    int increment = 0, base = 1, tryhere = 0;

	    do {
		switch (*curstate) {
		case T_BEGIN:
		    branches++;
		    curstate++;
		    break;
		case T_END:
		    branches--;
		    sequence = action = 0;
		    curstate++;
		    break;
		case T_OR:
		    curstate = find_matching(curstate, 1);
		    break;
		case T_REPEAT:
		    sequence = action = 0;
		    if (branches > 1) {
			branches--;
			curstate++;
		    } else {
			branches++;
			curstate = find_matching(curstate - 1, -1);
		    }
		    break;
		case T_NUM:
		    if (!(tryhere = (ch >= '0' && ch <= '9')))
			curstate++;
		    break;
		case T_RECORD:
		    record = current - buf - 1;
		    curstate++;
		    break;
		case T_CAPTURE:
		    capture = current - buf - 1;
		    curstate++;
		    break;
		case T_DROP:
		case T_CONTINUE:
		case T_NEXT:
		    action |= *curstate;
		    curstate++;
		    break;
		default:
		    if ((*curstate & SEQ) == SEQ) {
			sequence = *curstate;
			curstate++;
		    } else {
			tryhere = 1;
		    }
		}
	    } while (!tryhere);

	    switch (*curstate) {
	    case T_HEX:
		if (ch >= '0' && ch <= '9') {
		    increment = ch - '0';
		} else if (ch >= 'a' && ch <= 'f') {
		    increment = ch - 'a' + 10;
		} else if (ch >= 'A' && ch <= 'F') {
		    increment = ch - 'A' + 10;
		} else
		    break;
		consumed = number = 1;
		base = 16;
		if (action & 4)  /* NEXT was used */
		    ++num;
		break;
	    case T_HEXCH:
		consumed = (ch >= '0' && ch <= '9') ||
		    (ch >= 'a' && ch <= 'f') ||
		    (ch >= 'A' && ch <= 'F');
		if (consumed && number) {
		    ++num;
		    number = 0;
		}
		break;
	    case T_NUM:
		if (ch >= '0' && ch <= '9') {
		    increment = ch - '0';
		    base = 10;
		    consumed = number = 1;
		    curstate--; /* allow repetition */
		}
		break;
	    case T_WILDCARD:
		consumed = 1;
		if (number) {
		    ++num;
		    number = 0;
		}
		break;
	    default:
		if (!(*curstate & TAG) && (consumed = (*curstate == ch)) &&
			number)
		{
		    ++num;
		    number = 0;
		}
		break;
	    }
	    if (num == numbers + nlen) {
		numbers = hrealloc((char *) numbers, nlen * sizeof(int),
			sizeof(int) * nlen * 2);
		memset(numbers + nlen, 0, nlen * sizeof(int));
		num = numbers + nlen; /* restore relative position */;
		nlen *= 2;
	    }
	    if (number)
		*num = *num * base + increment;

	    /* if it didn't match, move to the next OR */
	    if (!consumed && branches > 1) {
		sequence = action = 0;
		for (; branches > 1; branches--) {
		    curstate = find_branch(curstate);
		    if (*curstate == T_OR) {
			curstate++;
			break;
		    /* repeated group can match zero times */
		    } else if (curstate[1] == T_REPEAT)
			break;
		}
	    }
	    /* Retry character at the start if it is the only buffered
	     * character and was tried from a later state. */
	    if (!consumed && branches <= 1) {
		if (triedstart || start + 1 != current)
		    break;
		branches = 1;
		curstate = states;
		triedstart = 1;
		memset((num = numbers), 0, nlen * sizeof(int));
		sequence = action = number = 0;
	    }
	}

	if (!consumed) {
	    illgotten = ++start;
	    curstate = states; /* return to first state */
	    memset((num = numbers), 0, nlen * sizeof(int));
	    number = 0;
	} else {
	    if (sequence && !(finish = sequence == SEQ))
		handle_seq(sequence & ~SEQ, numbers, num - numbers,
			buf + record, capture - record, output);

	    if ((sequence || (action & 1)) &&
		(current = start) && /* drop input from sequence */
		(!(action & 2)))
	    {
		curstate = states;
		memset((num = numbers), 0, nlen * sizeof(int));
		number = 0;
	    } else { /* CONTINUE */
		while (*++curstate == T_END)
		    ;
	    }
	}
    }

    /* put back any type-ahead text */
    if (current > buf)
	ungetbytes(buf, current - buf);

    settyinfo(&torig);
}

static unsigned memo_cursor;

static void
handle_color(int bg, int red, int green, int blue)
{
    char *colour;

    switch (bg) {
	case 0:  /* foreground color */
	    memo_term_color &= ~TXT_ATTR_FG_MASK;
	    memo_term_color |= TXT_ATTR_FG_24BIT | (zattr) ((((red << 8)
		    + green) << 8) + blue) << TXT_ATTR_FG_COL_SHIFT;
	    break;
	case 1:  /* background color */
	    memo_term_color &= ~TXT_ATTR_BG_MASK;
	    memo_term_color |= TXT_ATTR_BG_24BIT | (zattr) ((((red << 8)
		    + green) << 8) + blue) << TXT_ATTR_BG_COL_SHIFT;
	    /* scale by Rec.709 coefficients for lightness */
	    setsparam(MODEVAR, ztrdup(
		    0.2126f * red + 0.7152f * green + 0.0722f * blue <= 127 ?
		    "dark" : "light"));
	    break;
        case 2:  /* cursor color */
	    memo_cursor = (red << 24) | (green << 16) | (blue << 8);
	    break;
    }

    colour = zalloc(8);
    sprintf(colour, "#%02x%02x%02x", red, green, blue);
    setsparam(COLORVAR[bg], colour);
}

/* roughly corresponding feature names */
static const char *features[] =
	{ "bg", "fg", "cursorcolor", "modkeys-kitty", "truecolor", "id" };
static const char *queries[] =
	{ TQ_BGCOLOR, TQ_FGCOLOR, TQ_CURSOR, TQ_KITTYKB, TQ_RGB, TQ_XTVERSION, TQ_DA };

static void
handle_query(int sequence, int *numbers, int len, char *capture, int clen,
	UNUSED(void *output))
{
    char **feat;

    switch (sequence) {
	case 1: /* default colour */
	    if (len == 4)
		handle_color(numbers[0], numbers[1], numbers[2], numbers[3]);
	    break;
	case 2: /* kitty keyboard */
	    feat = zshcalloc(2 * sizeof(char *));
	    *feat = ztrdup(features[3]);
	    assignaparam(EXTVAR, feat, ASSPM_WARN|ASSPM_AUGMENT);
	    break;
	case 3: /* truecolor */
	    feat = zshcalloc(2 * sizeof(char *));
	    *feat = ztrdup(features[4]);
	    assignaparam(EXTVAR, feat, ASSPM_WARN|ASSPM_AUGMENT);
	    break;
	case 4: /* id */
	    setsparam(IDVAR, ztrduppfx(capture, clen));
	    break;
	case 5: /* version */
	    setsparam(VERVAR, ztrduppfx(capture, clen));
	    break;
    }
}

/**/
void
query_terminal(void) {
    char tquery[sizeof(TQ_BGCOLOR TQ_FGCOLOR TQ_CURSOR TQ_KITTYKB TQ_RGB TQ_XTVERSION TQ_DA)];
    char *tqend = tquery;
    static seqstate_t states[] = QUERY_STATES;
    char **f, **flist = getaparam(EXTVAR);
    size_t i;

    for (f = flist; f && *f; f++)
	if (!strcmp(*f, "-query"))
	    return; /* disable all queries */

    for (i=0; i < sizeof(queries)/sizeof(*queries); i++) {
	int last = i >= sizeof(features)/sizeof(*features);
	int found = (last && tqend == tquery);
	int enable = 0;
	char *cterm;

	/* skip if the query or corresponding feature is already in the list */
	for (f = flist; !last && !found && f && *f; f++) {
	    /* just i=3(TQ_KITTYKB) is disabled by default */
	    enable = i == 3 && strpfx("query-", *f) && !strcmp(*f + 6, features[i]);
	    found = enable || !strcmp(*f + (**f == '-'), features[i]) ||
		(strpfx("-query-", *f) && !strcmp(*f + 7, features[i]));
	}
	if (found ? !enable : i == 3)
	    continue;
	/* if termcap indicates 24-bit color, assume support - even
	 * though this is only based on the initial $TERM
	 * failing that, check $COLORTERM */
	if (i == 4 && (tccolours == 1 << 24 ||
		((cterm = getsparam("COLORTERM")) &&
		    (!strcmp(cterm, "truecolor") ||
			!strcmp(cterm, "24bit")))))
	    handle_query(3, NULL, 0, NULL, 0, NULL);
	else
	    struncpy(&tqend, (char *) queries[i], /* collate escape sequences */
		sizeof(tquery) - (tqend - tquery));
    }

    if (tqend != tquery) /* unless nothing left after filtering */
	probe_terminal(tquery, states, &handle_query, NULL);
}

static char*
base64_encode(const char *src, size_t len) {
    static const char* base64_table =
	"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";

    const unsigned char *end = (unsigned char *)src + len;
    const unsigned char *in = (unsigned char *)src;
    char *ret = zhalloc(1 + 4 * ((len + 2) / 3)); /* 4 bytes out for 3 in */
    char *cur = ret;

    for (; end - in > 0; in += 3, cur += 4) {
	unsigned int n = *in << 16;
	cur[3] = end - in > 2 ? base64_table[(n |= in[2]) & 0x3f] : '=';
	cur[2] = end - in > 1 ? base64_table[((n |= in[1]<<8) >> 6) & 0x3f] : '=';
	cur[1] = base64_table[(n >> 12) & 0x3f];
	cur[0] = base64_table[n >> 18];
    }
    *cur = '\0';

    return ret;
}

static char*
base64_decode(const char *src, size_t len)
{
    int i = 0;
    unsigned int n;
    char *buf = hcalloc((3 * len) / 4 + 1);
    char *b = buf;
    char c;

    while (len && (c = src[i]) != '=') {
	n = isdigit(c) ? c - '0' + 52 :
	    islower(c) ? c - 'a' + 26 :
	    isupper(c) ? c - 'A' :
	    (c == '+') ? 62 :
	    (c == '/') ? 63 : 0;
	if (i % 4)
	    *b++ |= n >> (2 * (3 - (i % 4)));
	if (++i >= len)
	    break;
	*b = n << (2 * (i % 4));
    }
    return buf;
}

static void
handle_paste(UNUSED(int sequence), UNUSED(int *numbers), UNUSED(int len),
	char *capture, int clen, void *output)
{
    *(char**) output = base64_decode(capture, clen);
}

static char*
url_encode(const char* path, size_t *ulen)
{
    char *url = zhalloc(strlen(path) * 3 + 1); /* worst case length triples */
    const char *in = path;
    char *out = url;

    for (; *in; in++) {
        /* In theory, space can be encoded as '+' but not all terminals
	 * handled that and %20 works reliably.
	 * ':' as a Windows drive letter should also not be encoded */
        if (isalnum(*in) || strchr("-._~/", *in))
            *out++ = *in; /* untouched in encoding */
        else
            out += sprintf(out, "%%%02X", *in); /* otherwise %HH */
    }

    *out = '\0';
    *ulen = out - url;
    return url;
}

/**/
char *
system_clipget(char clip)
{
    static seqstate_t osc52[] = OSC52_STATES;
    char seq[] = "\033]52;.;?\033\\";
    char *contents = NULL;
    seq[5] = clip;
    probe_terminal(seq, osc52, &handle_paste, &contents);
    return contents;
}

/**/
void
system_clipput(char clip, char *content, size_t clen)
{
    char *encoded = base64_encode(content, clen);
    fprintf(shout, "\033]52;%c;%s\033\\", clip, encoded);
}

/**/
static int
extension_enabled(const char *class, const char *ext, unsigned clen, int def)
{
    char **e, **elist = getaparam(EXTVAR);

    for (e = elist; e && *e; e++) {
	int negate = (**e == '-');
	if (strncmp(*e + negate, class, clen))
	    continue;

	if (!*(*e + negate + clen) || !strcmp(*e + negate + clen + 1, ext))
	    return !negate;
    }
    return def;
}


struct extension {
    char *key, *seq[2];
    int class, enabled;
};

static const struct extension editext[] = {
    { "bracketed-paste", { NULL, NULL}, 0, 1 },
    { "integration-prompt", { "\033]133;B\033\\" }, 11, 1 },
#if 0
    { "modkeys-kitty", { "\033[=5u", "\033[=0u" }, 7, 0 },
#endif
    { "modkeys-xterm", { "\033[>4;1m", "\033[>4m" }, 7, 0 }
};

static void
collate_seq(int sindex, int dir)
{
    char seq[256];
    char *pos = seq;
    int max = sizeof(editext) / sizeof(*editext);
    int i;
    char **bracket;
    char **e, **elist = getaparam(EXTVAR);

    for (i = dir > 0 ? 0 : max - 1; i >= 0 && i < max; i += dir) {
	int enabled = editext[i].enabled;
	if (i && !editext[i].seq[sindex])
	    continue;
	for (e = elist; e && *e; e++) {
	    int negate = (**e == '-');
	    if (negate != enabled)
		continue;
	    if ((editext[i].class &&
                !strncmp(*e + negate, editext[i].key, editext[i].class) &&
		!*(*e + negate + editext[i].class)) ||
		!strcmp(*e + negate + editext[i].class,
                    editext[i].key + editext[i].class))
	    {
                enabled = !negate;
		break;
	    }

	}
        if (enabled) {
	    if (i)
		strucpy(&pos, editext[i].seq[sindex]);
	    else if ((bracket = getaparam("zle_bracketed_paste")) &&
		    arrlen(bracket) == 2)
		strucpy(&pos, bracket[sindex]);
	}
    }
    write_loop(SHTTY, seq, pos - seq);
}

/**/
void
start_edit(void)
{
    collate_seq(0, 1);
}

/**/
void
end_edit(void)
{
    collate_seq(1, -1);
}

/**/
const char **
prompt_markers(void)
{
    static unsigned int aid = 0;
    static char pre[] = "\033]133;A;cl=m;aid=zZZZZZZ\033\\"; /* before the prompt */
    static const char PR[] = "\033]133;P;k=i\033\\";   /* primary (PS1) */
    static const char SE[] = "\033]133;P;k=s\033\\";   /* secondary (PS2) */
    static const char RI[] = "\033]133;P;k=r\033\\";   /* right (RPS1,2) */
    static const char *markers[] = { PR, SE, RI };
    static const char *nomark[] = { NULL, NULL, NULL, NULL };

    if (!extension_enabled("integration", "prompt", 11, 1))
	return nomark;

    if (!aid) {
	/* hostname and pid should uniquely identify a shell instance */
	char *h = getsparam("HOST");
	aid = (h ? hasher(h) : 0) ^ getpid();
	if (!aid) aid = 1; /* unlikely but just to be safe */
	/* base64 not required but it is safe, convenient and compact */
	h = base64_encode((const char *)&aid, sizeof(aid));
	memcpy(pre + 13, h, 6);
    }

    return markers;
}

/**/
void
mark_output(int start)
{
    static const char START[] = "\033]133;C\033\\";
    static const char END[] = "\033]133;D\033\\";
    if (extension_enabled("integration", "output", 11, 1))
	write_loop(SHTTY, start ? START : END,
		(start ? sizeof(START) : sizeof(END)) - 1);
}

/**/
void
notify_pwd(void)
{
    char *url;
    size_t ulen;

    if (!extension_enabled("integration", "pwd", 11, 1))
	return;

    url = url_encode(pwd, &ulen);
    /* only "localhost" seems to be much use here as the host */
    write_loop(SHTTY, "\033]7;file://localhost", 20);
    write_loop(SHTTY, url, ulen);
    write_loop(SHTTY, "\033\\", 2);
}

static unsigned int *cursor_forms;
static unsigned int cursor_enabled_mask;

static void
match_cursorform(const char *teststr, unsigned int *cursor_form)
{
    static const struct {
	const char *name;
	unsigned char value, mask;
    } shapes[] = {
	{ "none", 0, 0xff },
	{ "underline", CURF_UNDERLINE, CURF_SHAPE_MASK },
	{ "bar", CURF_BAR, CURF_SHAPE_MASK },
	{ "block", CURF_BLOCK, CURF_SHAPE_MASK },
	{ "blink",  CURF_BLINK, CURF_STEADY },
	{ "steady",  CURF_STEADY, CURF_BLINK },
	{ "hidden", CURF_HIDDEN, 0 }
    };

    *cursor_form = 0;
    while (*teststr) {
	size_t s;
	int found = 0;

	if (strpfx("color=#", teststr)) {
	    char *end;
	    teststr += 7;
	    zlong col = zstrtol(teststr, &end, 16);
            if (end - teststr == 4) {
		unsigned int red = col >> 8;
		unsigned int green = (col & 0xf0) >> 4;
		unsigned int blue = (col & 0xf);
		*cursor_form &= 0xff; /* clear color */
		*cursor_form |= CURF_COLOR |
		    ((red << 4 | red) << CURF_RED_SHIFT) |
		    ((green << 4 | green) << CURF_GREEN_SHIFT) |
		    ((blue << 4 | blue) << CURF_BLUE_SHIFT);
		found = 1;
	    } else if (end - teststr == 6) {
		*cursor_form |= (col << 8) | CURF_COLOR;
		found = 1;
	    }
	    teststr = end;
	}
	for (s = 0; !found && s < sizeof(shapes) / sizeof(*shapes); s++) {
	    if (strpfx(shapes[s].name, teststr)) {
		teststr += strlen(shapes[s].name);
		*cursor_form &= ~shapes[s].mask;
		*cursor_form |= shapes[s].value;
		found = 1;
	    }
	}
	if (!found) /* skip an unknown component */
	    teststr = strchr(teststr, ',');
	if (!teststr || *teststr != ',')
	    break;
	teststr++;
    }
}

/**/
void
zle_set_cursorform(void)
{
    char **atrs = getaparam("zle_cursorform");
    static int setup = 0;
    size_t i;
    static const char *contexts[] = {
	"edit:",
	"command:",
	"insert:",
	"overwrite:",
	"pending:",
	"regionstart:",
	"regionend:",
        "visual:"
    };

    if (!cursor_forms)
	cursor_forms = zalloc(CURC_DEFAULT * sizeof(*cursor_forms));
    memset(cursor_forms, 0, CURC_DEFAULT * sizeof(*cursor_forms));
    cursor_forms[CURC_INSERT] = CURF_BAR;
    cursor_forms[CURC_PENDING] = CURF_UNDERLINE;

    for (; atrs && *atrs; atrs++) {
	if (strpfx("region:", *atrs)) {
	    match_cursorform(*atrs + 7, &cursor_forms[CURC_REGION_END]);
	    cursor_forms[CURC_REGION_START] = cursor_forms[CURC_REGION_END];
	    continue;
	}
	for (i = 0; i < sizeof(contexts) / sizeof(*contexts); i++) {
	    if (strpfx(contexts[i], *atrs)) {
		match_cursorform(*atrs + strlen(contexts[i]), &cursor_forms[i]);
		break;
	    }
	}
    }

    if (!setup || trashedzle) {
	cursor_enabled_mask = 0;
	setup = 1;
	if (!extension_enabled("cursor", "shape", 6, 1))
	    cursor_enabled_mask |= CURF_SHAPE_MASK | CURF_BLINK | CURF_STEADY;
	if (!extension_enabled("cursor", "color", 6, 1))
	    cursor_enabled_mask |= CURF_COLOR_MASK;
    }
}

/**/
void
free_cursor_forms(void)
{
    if (cursor_forms)
	zfree(cursor_forms, CURC_DEFAULT * sizeof(*cursor_forms));
    cursor_forms = 0;
}

/**/
void
cursor_form(void)
{
    char seq[31];
    char *s = seq;
    unsigned int want, changed;
    static unsigned int state = CURF_DEFAULT;
    enum cursorcontext context = CURC_DEFAULT;

    if (!cursor_forms)
	return;

    if (trashedzle) {
	;
    } else if (!insmode) {
	context = CURC_OVERWRITE;
    } else if (vichgflag == 2) {
	context = CURC_PENDING;
    } else if (region_active) {
        if (invicmdmode()) {
	    context = CURC_VISUAL;
	} else {
	    context = mark > zlecs ? CURC_REGION_START : CURC_REGION_END;
	}
    } else
	context = invicmdmode() ? CURC_COMMAND : (vichgflag ? CURC_INSERT : CURC_EDIT);
    want = (context == CURC_DEFAULT) ? CURF_DEFAULT : cursor_forms[context];
    if (!(changed = (want ^ state) & ~cursor_enabled_mask))
	return;

    if (changed & CURF_HIDDEN)
	 tcout(want & CURF_HIDDEN ? TCCURINV : TCCURVIS);
    if (changed & CURF_SHAPE_MASK) {
	char c = '0';
	switch (want & CURF_SHAPE_MASK) {
	    case CURF_BAR: c += 2;
	    case CURF_UNDERLINE: c += 2;
	    case CURF_BLOCK:
		c += 2 - !!(want & CURF_BLINK);
		changed &= ~(CURF_BLINK | CURF_STEADY);
	}
	s += sprintf(s, "\033[%c q", c);
    }
    if (changed & (CURF_BLINK | CURF_STEADY)) {
	s += sprintf(s, "\033[?12%c", (want & CURF_BLINK) ? 'h' : 'l');
    }
    if (changed & CURF_COLOR_MASK) {
	if (!(want & CURF_COLOR_MASK))
	    want = memo_cursor | (want & 0xff);
	s += sprintf(s, "\033]12;rgb:%02x00/%02x00/%02x00\033\\",
		want >> CURF_RED_SHIFT, (want >> CURF_GREEN_SHIFT) & 0xff,
		(want >> CURF_BLUE_SHIFT) & 0xff);
    }
    if (s - seq)
	write_loop(SHTTY, seq, s - seq);
    state = want;
}
