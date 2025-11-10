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
static char *BGVAR   = ".term.bg";
static char *FGVAR   = ".term.fg";
static char *MODEVAR = ".term.mode";

/* Query sequences
 * using ESC\\ as ST instead of BEL because the bell was emitted on
 * old xterm. */

/* Terminal default colors. Probably best that these are queried first
 * because tmux will need to pass these on. */
#define TQ_BGCOLOR "\033]11;?\033\\"
#define TQ_FGCOLOR "\033]10;?\033\\"

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

    seqstate_t *curstate = states;

    gettyinfo(&ti);
    memcpy(&torig, &ti, sizeof(torig));
#ifdef HAS_TIO
    ti.tio.c_lflag &= (~ECHO & ~ICANON & ~ISIG);
    ti.tio.c_iflag &= ~ICRNL;
#else
    ti.sgttyb.sg_flags &= ~ECHO;
    ti.sgttyb.sg_flags |= CBREAK;
#endif
    settyinfo(&ti);

    fputs(tquery, shout);
    fflush(shout);

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
	    if ((ch = getbyte(TIMEOUT, 0, 1)) == EOF)
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

static void
handle_color(int bg, int red, int green, int blue)
{
    char *colour;

    switch (bg) {
	case 1: /* background color */
	    /* scale by Rec.709 coefficients for lightness */
	    setsparam(MODEVAR, ztrdup(
		    0.2126f * red + 0.7152f * green + 0.0722f * blue <= 127 ?
		    "dark" : "light"));
	  /* fall-through */
	case 0:
	    colour = zalloc(8);
	    sprintf(colour, "#%02x%02x%02x", red, green, blue);
	    setsparam(bg ? BGVAR : FGVAR, colour);
	    break;
	default: break;
    }
}

/* roughly corresponding feature names */
static const char *features[] =
	{ "bg", "fg", "modkeys-kitty", "truecolor", "id" };
static const char *queries[] =
	{ TQ_BGCOLOR, TQ_FGCOLOR, TQ_KITTYKB, TQ_RGB, TQ_XTVERSION, TQ_DA };

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
	    *feat = ztrdup(features[2]);
	    assignaparam(EXTVAR, feat, ASSPM_WARN|ASSPM_AUGMENT);
	    break;
	case 3: /* truecolor */
	    feat = zshcalloc(2 * sizeof(char *));
	    *feat = ztrdup(features[3]);
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
    char tquery[sizeof(TQ_BGCOLOR TQ_FGCOLOR TQ_KITTYKB TQ_RGB TQ_XTVERSION TQ_DA)];
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
	char *cterm;

	/* skip if the query or corresponding feature is already in the list */
	for (f = flist; !last && !found && f && *f; f++)
	    found = !strcmp(*f + (**f == '-'), features[i]) ||
		(!strncmp(*f, "-query-", 7) && !strcmp(*f + 7, features[i]));
	if (found)
	    continue;
	/* if termcap indicates 24-bit color, assume support - even
	 * though this is only based on the initial $TERM
	 * failing that, check $COLORTERM */
	if (i == 3 && (tccolours == 1 << 24 ||
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

/**/
char *
system_clipget(char clip)
{
    static seqstate_t osc52[] = OSC52_STATES;
    char seq[] = "\033]52;.;?\033\\";
    char *contents;
    seq[5] = clip;
    probe_terminal(seq, osc52, &handle_paste, &contents);
    return contents;
}

/**/
void
system_clipput(char clip, char *content, size_t clen)
{
    char *encoded = base64_encode(content, clen);
    fprintf(shout, "\033]52;%c;%s\a", clip, encoded);
}
