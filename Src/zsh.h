/*
 * zsh.h - standard header file
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

#define trashzle()      trashzleptr()
#define zleread(X,Y,H)  zlereadptr(X,Y,H)
#define spaceinline(X)  spaceinlineptr(X)
#define gotword()       gotwordptr()
#define refresh()       refreshptr()

#define compctlread(N,A,O,R) compctlreadptr(N,A,O,R)

/* A few typical macros */
#define minimum(a,b)  ((a) < (b) ? (a) : (b))

/* math.c */
typedef int LV;

/* Character tokens are sometimes casted to (unsigned char)'s.         * 
 * Unfortunately, some compilers don't correctly cast signed to        * 
 * unsigned promotions; i.e. (int)(unsigned char)((char) -1) evaluates * 
 * to -1, instead of 255 like it should.  We circumvent the troubles   * 
 * of such shameful delinquency by casting to a larger unsigned type   * 
 * then back down to unsigned char.                                    */

#ifdef BROKEN_SIGNED_TO_UNSIGNED_CASTING
# define STOUC(X)	((unsigned char)(unsigned short)(X))
#else
# define STOUC(X)	((unsigned char)(X))
#endif

/* Meta together with the character following Meta denotes the character *
 * which is the exclusive or of 32 and the character following Meta.     *
 * This is used to represent characters which otherwise has special      *
 * meaning for zsh.  These are the characters for which the imeta() test *
 * is true: the null character, and the characters from Meta to Marker.  */

#define Meta		((char) 0x83)

/* Note that the fourth character in DEFAULT_IFS is Meta *
 * followed by a space which denotes the null character. */

#define DEFAULT_IFS	" \t\n\203 "

/* Character tokens */
#define Pound		((char) 0x84)
#define String		((char) 0x85)
#define Hat		((char) 0x86)
#define Star		((char) 0x87)
#define Inpar		((char) 0x88)
#define Outpar		((char) 0x89)
#define Qstring	        ((char) 0x8a)
#define Equals		((char) 0x8b)
#define Bar	      	((char) 0x8c)
#define Inbrace	        ((char) 0x8d)
#define Outbrace	((char) 0x8e)
#define Inbrack	        ((char) 0x8f)
#define Outbrack	((char) 0x90)
#define Tick		((char) 0x91)
#define Inang		((char) 0x92)
#define Outang		((char) 0x93)
#define Quest		((char) 0x94)
#define Tilde		((char) 0x95)
#define Qtick		((char) 0x96)
#define Comma		((char) 0x97)
#define Snull		((char) 0x98)
#define Dnull		((char) 0x99)
#define Bnull		((char) 0x9a)
#define Nularg		((char) 0x9b)

#define INULL(x)	(((x) & 0xfc) == 0x98)

/* Marker used in paramsubst for rc_expand_param */
#define Marker		((char) 0x9c)

/* chars that need to be quoted if meant literally */

#define SPECCHARS "#$^*()=|{}[]`<>?~;&\n\t \\\'\""

enum {
    NULLTOK,		/* 0  */
    SEPER,
    NEWLIN,
    SEMI,
    DSEMI,
    AMPER,		/* 5  */
    INPAR,
    OUTPAR,
    DBAR,
    DAMPER,
    OUTANG,		/* 10 */
    OUTANGBANG,
    DOUTANG,
    DOUTANGBANG,
    INANG,
    INOUTANG,		/* 15 */
    DINANG,
    DINANGDASH,
    INANGAMP,
    OUTANGAMP,
    AMPOUTANG,		/* 20 */
    OUTANGAMPBANG,
    DOUTANGAMP,
    DOUTANGAMPBANG,
    TRINANG,
    BAR,		/* 25 */
    BARAMP,
    INOUTPAR,
    DINPAR,
    DOUTPAR,
    AMPERBANG,		/* 30 */
    SEMIAMP,
    DOUTBRACK,
    STRING,
    ENVSTRING,
    ENVARRAY,		/* 35 */
    ENDINPUT,
    LEXERR,

    /* Tokens for reserved words */
    BANG,	/* !         */
    DINBRACK,	/* [[        */
    INBRACE,    /* {         */	/* 40 */
    OUTBRACE,   /* }         */
    CASE,	/* case      */
    COPROC,	/* coproc    */
    DO,		/* do        */
    DONE,	/* done      */ /* 45 */
    ELIF,	/* elif      */
    ELSE,	/* else      */
    ZEND,	/* end       */
    ESAC,	/* esac      */
    FI,		/* fi        */ /* 50 */
    FOR,	/* for       */
    FOREACH,	/* foreach   */
    FUNC,	/* function  */
    IF,		/* if        */
    NOCORRECT,	/* nocorrect */ /* 55 */
    REPEAT,	/* repeat    */
    SELECT,	/* select    */
    THEN,	/* then      */
    TIME,	/* time      */
    UNTIL,	/* until     */ /* 60 */
    WHILE	/* while     */
};

/* Redirection types.  If you modify this, you may also have to modify *
 * redirtab in parse.c and getredirs() in text.c and the IS_* macros   *
 * below.                                                              */

enum {
    WRITE,		/* > */
    WRITENOW,		/* >| */
    APP,		/* >> */
    APPNOW,		/* >>| */
    ERRWRITE,		/* &>, >& */
    ERRWRITENOW,	/* >&| */
    ERRAPP,		/* >>& */
    ERRAPPNOW,		/* >>&| */
    READWRITE,		/* <> */
    READ,		/* < */
    HEREDOC,		/* << */
    HEREDOCDASH,	/* <<- */
    HERESTR,		/* <<< */
    MERGEIN,		/* <&n */
    MERGEOUT,		/* >&n */
    CLOSE,		/* >&-, <&- */
    INPIPE,		/* < <(...) */
    OUTPIPE		/* > >(...) */
};

#define IS_WRITE_FILE(X)      ((X)>=WRITE && (X)<=READWRITE)
#define IS_APPEND_REDIR(X)    (IS_WRITE_FILE(X) && ((X) & 2))
#define IS_CLOBBER_REDIR(X)   (IS_WRITE_FILE(X) && ((X) & 1))
#define IS_ERROR_REDIR(X)     ((X)>=ERRWRITE && (X)<=ERRAPPNOW)
#define IS_READFD(X)          (((X)>=READWRITE && (X)<=MERGEIN) || (X)==INPIPE)
#define IS_REDIROP(X)         ((X)>=OUTANG && (X)<=TRINANG)

/* Flags for input stack */
#define INP_FREE      (1<<0)	/* current buffer can be free'd            */
#define INP_ALIAS     (1<<1)	/* expanding alias or history              */
#define INP_HIST      (1<<2)	/* expanding history                       */
#define INP_CONT      (1<<3)	/* continue onto previously stacked input  */
#define INP_ALCONT    (1<<4)	/* stack is continued from alias expn.     */

/* Flags for metafy */
#define META_REALLOC	0
#define META_USEHEAP	1
#define META_STATIC	2
#define META_DUP	3
#define META_ALLOC	4
#define META_NOALLOC	5
#define META_HEAPDUP	6
#define META_HREALLOC	7


/**************************/
/* Abstract types for zsh */
/**************************/

typedef struct linknode  *LinkNode;
typedef struct linklist  *LinkList;
typedef struct hashnode  *HashNode;
typedef struct hashtable *HashTable;

typedef struct reswd     *Reswd;
typedef struct alias     *Alias;
typedef struct param     *Param;
typedef struct cmdnam    *Cmdnam;
typedef struct shfunc    *Shfunc;
typedef struct builtin   *Builtin;
typedef struct nameddir  *Nameddir;
typedef struct module    *Module;

typedef struct process   *Process;
typedef struct job       *Job;
typedef struct value     *Value;
typedef struct varasg    *Varasg;
typedef struct cond      *Cond;
typedef struct cmd       *Cmd;
typedef struct pline     *Pline;
typedef struct sublist   *Sublist;
typedef struct list      *List;
typedef struct comp      *Comp;
typedef struct redir     *Redir;
typedef struct complist  *Complist;
typedef struct heap      *Heap;
typedef struct heapstack *Heapstack;
typedef struct histent   *Histent;
typedef struct forcmd    *Forcmd;
typedef struct autofn    *AutoFn;

typedef struct asgment  *Asgment;


/********************************/
/* Definitions for linked lists */
/********************************/

/* linked list abstract data type */

struct linknode {
    LinkNode next;
    LinkNode last;
    void *dat;
};

struct linklist {
    LinkNode first;
    LinkNode last;
};

/* Macros for manipulating link lists */

#define addlinknode(X,Y) insertlinknode(X,(X)->last,Y)
#define uaddlinknode(X,Y) uinsertlinknode(X,(X)->last,Y)
#define empty(X)     ((X)->first == NULL)
#define nonempty(X)  ((X)->first != NULL)
#define firstnode(X) ((X)->first)
#define getaddrdata(X) (&((X)->dat))
#define getdata(X)   ((X)->dat)
#define setdata(X,Y) ((X)->dat = (Y))
#define lastnode(X)  ((X)->last)
#define nextnode(X)  ((X)->next)
#define prevnode(X)  ((X)->last)
#define peekfirst(X) ((X)->first->dat)
#define pushnode(X,Y) insertlinknode(X,(LinkNode) X,Y)
#define incnode(X) (X = nextnode(X))
#define gethistent(X) (histentarr+((X)%histentct))


/********************************/
/* Definitions for syntax trees */
/********************************/

/* struct list, struct sublist, struct pline, etc.  all fit the form *
 * of this structure and are used interchangably. The ptrs may hold  *
 * integers or pointers, depending on the type of the node.          */

/* Generic node structure for syntax trees */
struct node {
    int ntype;			/* node type */
};

#define N_LIST    0
#define N_SUBLIST 1
#define N_PLINE   2
#define N_CMD     3
#define N_REDIR   4
#define N_COND    5
#define N_FOR     6
#define N_CASE    7
#define N_IF      8
#define N_WHILE   9
#define N_VARASG 10
#define N_AUTOFN 11
#define N_COUNT  12

/* values for types[4] */

#define NT_EMPTY 0
#define NT_NODE  1
#define NT_STR   2
#define NT_LIST  4
#define NT_ARR   8

#define NT_TYPE(T) ((T) & 0xff)
#define NT_N(T, N) (((T) >> (8 + (N) * 4)) & 0xf)
#define NT_SET(T0, T1, T2, T3, T4) \
    ((T0) | ((T1) << 8) | ((T2) << 12) | ((T3) << 16) | ((T4) << 20))
#define NT_HEAP   (1 << 30)

/* tree element for lists */

struct list {
    int ntype;			/* node type */
    int type;
    Sublist left;
    List right;
};

/* These are control flags that are passed *
 * down the execution pipeline.            */
#define Z_TIMED	(1<<0)	/* pipeline is being timed                   */
#define Z_SYNC	(1<<1)	/* run this sublist synchronously       (;)  */
#define Z_ASYNC	(1<<2)	/* run this sublist asynchronously      (&)  */
#define Z_DISOWN (1<<3)	/* run this sublist without job control (&|) */

/* tree element for sublists */

struct sublist {
    int ntype;			/* node type */
    int type;
    int flags;			/* see PFLAGs below */
    Pline left;
    Sublist right;
};

#define ORNEXT  10		/* || */
#define ANDNEXT 11		/* && */

#define PFLAG_NOT     1		/* ! ... */
#define PFLAG_COPROC 32		/* coproc ... */

/* tree element for pipes */

struct pline {
    int ntype;			/* node type */
    int type;
    Cmd left;
    Pline right;
};

#define END	0		/* pnode *right is null                     */
#define PIPE	1		/* pnode *right is the rest of the pipeline */

/* tree element for commands */

struct cmd {
    int ntype;			/* node type                    */
    int type;
    int flags;			/* see CFLAGs below             */
    int lineno;			/* lineno of script for command */
    union {
	List list;		/* for SUBSH/CURSH/SHFUNC       */
	Forcmd forcmd;
	struct casecmd *casecmd;
	struct ifcmd *ifcmd;
	struct whilecmd *whilecmd;
	Sublist pline;
	Cond cond;
	AutoFn autofn;
	void *generic;
    } u;
    LinkList args;		/* command & argmument List (char *'s)   */
    LinkList redir;		/* i/o redirections (struct redir *'s)   */
    LinkList vars;		/* param assignments (struct varasg *'s) */
};

/* cmd types */
#define SIMPLE   0
#define SUBSH    1
#define CURSH    2
#define ZCTIME   3
#define FUNCDEF  4
#define CFOR     5
#define CWHILE   6
#define CREPEAT  7
#define CIF      8
#define CCASE    9
#define CSELECT 10
#define COND    11
#define CARITH  12
#define AUTOFN  13

/* flags for command modifiers */
#define CFLAG_EXEC	(1<<0)	/* exec ...    */

/* tree element for redirection lists */

struct redir {
    int ntype;			/* node type */
    int type;
    int fd1, fd2;
    char *name;
};

/* tree element for conditionals */

struct cond {
    int ntype;		/* node type                     */
    int type;		/* can be cond_type, or a single */
			/* letter (-a, -b, ...)          */
    void *left, *right;
};

#define COND_NOT    0
#define COND_AND    1
#define COND_OR     2
#define COND_STREQ  3
#define COND_STRNEQ 4
#define COND_STRLT  5
#define COND_STRGTR 6
#define COND_NT     7
#define COND_OT     8
#define COND_EF     9
#define COND_EQ    10
#define COND_NE    11
#define COND_LT    12
#define COND_GT    13
#define COND_LE    14
#define COND_GE    15

struct forcmd {			/* for/select */
/* Cmd->args contains list of words to loop thru */
    int ntype;			/* node type                          */
    int inflag;			/* if there is an in ... clause       */
    char *name;			/* initializer or parameter name      */
    char *condition;		/* arithmetic terminating condition   */
    char *advance;		/* evaluated after each loop          */
    List list;			/* list to look through for each name */
};

struct casecmd {
/* Cmd->args contains word to test */
    int ntype;			/* node type       */
    char **pats;
    List *lists;		/* list to execute */
};


/*  A command like "if foo then bar elif baz then fubar else fooble"  */
/*  generates a tree like:                                            */
/*                                                                    */
/*  struct ifcmd a = { next =  &b,  ifl = "foo", thenl = "bar" }      */
/*  struct ifcmd b = { next =  &c,  ifl = "baz", thenl = "fubar" }    */
/*  struct ifcmd c = { next = NULL, ifl = NULL, thenl = "fooble" }    */

struct ifcmd {
    int ntype;			/* node type */
    List *ifls;
    List *thenls;
};

struct whilecmd {
    int ntype;			/* node type                           */
    int cond;			/* 0 for while, 1 for until            */
    List cont;			/* condition                           */
    List loop;			/* list to execute until condition met */
};

/* node for autoloading functions */

struct autofn {
    int ntype;			/* node type                             */
    Shfunc shf;			/* the shell function to define	         */
};

/* The number of fds space is allocated for  *
 * each time a multio must increase in size. */
#define MULTIOUNIT 8

/* A multio is a list of fds associated with a certain fd.       *
 * Thus if you do "foo >bar >ble", the multio for fd 1 will have *
 * two fds, the result of open("bar",...), and the result of     *
 * open("ble",....).                                             */

/* structure used for multiple i/o redirection */
/* one for each fd open                        */

struct multio {
    int ct;			/* # of redirections on this fd                 */
    int rflag;			/* 0 if open for reading, 1 if open for writing */
    int pipe;			/* fd of pipe if ct > 1                         */
    int fds[MULTIOUNIT];	/* list of src/dests redirected to/from this fd */
};

/* variable assignment tree element */

struct varasg {
    int ntype;			/* node type                             */
    int type;			/* nonzero means array                   */
    char *name;
    char *str;			/* should've been a union here.  oh well */
    LinkList arr;
};

/* lvalue for variable assignment/expansion */

struct value {
    int isarr;
    Param pm;		/* parameter node                      */
    int inv;		/* should we return the index ?        */
    int a;		/* first element of array slice, or -1 */
    int b;		/* last element of array slice, or -1  */
};

/* structure for foo=bar assignments */

struct asgment {
    struct asgment *next;
    char *name;
    char *value;
};

#define MAX_ARRLEN    262144


/********************************************/
/* Defintions for job table and job control */
/********************************************/

/* size of job table */
#define MAXJOB 50

/* entry in the job table */

struct job {
    pid_t gleader;		/* process group leader of this job  */
    pid_t other;		/* subjob id or subshell pid         */
    int stat;                   /* see STATs below                   */
    char *pwd;			/* current working dir of shell when *
				 * this job was spawned              */
    struct process *procs;	/* list of processes                 */
    LinkList filelist;		/* list of files to delete when done */
    int stty_in_env;		/* if STTY=... is present            */
    struct ttyinfo *ty;		/* the modes specified by STTY       */
};

#define STAT_CHANGED	(1<<0)	/* status changed and not reported      */
#define STAT_STOPPED	(1<<1)	/* all procs stopped or exited          */
#define STAT_TIMED	(1<<2)	/* job is being timed                   */
#define STAT_DONE	(1<<3)	/* job is done                          */
#define STAT_LOCKED	(1<<4)	/* shell is finished creating this job, */
                                /*   may be deleted from job table      */
#define STAT_NOPRINT	(1<<5)	/* job was killed internally,           */
                                /*   we don't want to show that         */
#define STAT_INUSE	(1<<6)	/* this job entry is in use             */
#define STAT_SUPERJOB	(1<<7)	/* job has a subjob                     */
#define STAT_SUBJOB	(1<<8)	/* job is a subjob                      */
#define STAT_CURSH	(1<<9)	/* last command is in current shell     */
#define STAT_NOSTTY	(1<<10)	/* the tty settings are not inherited   */
				/* from this job when it exits.         */

#define SP_RUNNING -1		/* fake status for jobs currently running */

struct timeinfo {
    long ut;                    /* user space time   */
    long st;                    /* system space time */
};

#define JOBTEXTSIZE 80

/* node in job process lists */

struct process {
    struct process *next;
    pid_t pid;                  /* process id                       */
    char text[JOBTEXTSIZE];	/* text to print when 'jobs' is run */
    int status;			/* return code from waitpid/wait3() */
    struct timeinfo ti;
    struct timeval bgtime;	/* time job was spawned             */
    struct timeval endtime;	/* time job exited                  */
};

struct execstack {
    struct execstack *next;

    LinkList args;
    pid_t list_pipe_pid;
    int nowait;
    int pline_level;
    int list_pipe_child;
    int list_pipe_job;
    char list_pipe_text[JOBTEXTSIZE];
    int lastval;
    int noeval;
    int badcshglob;
    pid_t cmdoutpid;
    int cmdoutval;
    int trapreturn;
    int noerrs;
    int subsh_close;
    char *underscore;
};

struct heredocs {
    struct heredocs *next;
    Redir rd;
};

struct dirsav {
    int dirfd, level;
    char *dirname;
    dev_t dev;
    ino_t ino;
};

/*******************************/
/* Definitions for Hash Tables */
/*******************************/

typedef void *(*VFunc) _((void *));
typedef void (*FreeFunc) _((void *));

typedef unsigned (*HashFunc)       _((char *));
typedef void     (*TableFunc)      _((HashTable));
typedef void     (*AddNodeFunc)    _((HashTable, char *, void *));
typedef HashNode (*GetNodeFunc)    _((HashTable, char *));
typedef HashNode (*RemoveNodeFunc) _((HashTable, char *));
typedef void     (*FreeNodeFunc)   _((HashNode));

/* type of function that is passed to *
 * scanhashtable or scanmatchtable    */
typedef void     (*ScanFunc)       _((HashNode, int));

typedef void (*PrintTableStats) _((HashTable));

/* hash table for standard open hashing */

struct hashtable {
    /* HASHTABLE DATA */
    int hsize;			/* size of nodes[]  (number of hash values)   */
    int ct;			/* number of elements                         */
    HashNode *nodes;		/* array of size hsize                        */

    /* HASHTABLE METHODS */
    HashFunc hash;		/* pointer to hash function for this table    */
    TableFunc emptytable;	/* pointer to function to empty table         */
    TableFunc filltable;	/* pointer to function to fill table          */
    AddNodeFunc addnode;	/* pointer to function to add new node        */
    GetNodeFunc getnode;	/* pointer to function to get an enabled node */
    GetNodeFunc getnode2;	/* pointer to function to get node            */
				/* (getnode2 will ignore DISABLED flag)       */
    RemoveNodeFunc removenode;	/* pointer to function to delete a node       */
    ScanFunc disablenode;	/* pointer to function to disable a node      */
    ScanFunc enablenode;	/* pointer to function to enable a node       */
    FreeNodeFunc freenode;	/* pointer to function to free a node         */
    ScanFunc printnode;		/* pointer to function to print a node        */

#ifdef HASHTABLE_INTERNAL_MEMBERS
    HASHTABLE_INTERNAL_MEMBERS	/* internal use in hashtable.c                */
#endif
};

/* generic hash table node */

struct hashnode {
    HashNode next;		/* next in hash chain */
    char *nam;			/* hash key           */
    int flags;			/* various flags      */
};

/* The flag to disable nodes in a hash table.  Currently  *
 * you can disable builtins, shell functions, aliases and *
 * reserved words.                                        */
#define DISABLED	(1<<0)

/* node in shell reserved word hash table (reswdtab) */

struct reswd {
    HashNode next;		/* next in hash chain        */
    char *nam;			/* name of reserved word     */
    int flags;			/* flags                     */
    int token;			/* corresponding lexer token */
};

/* node in alias hash table (aliastab) */

struct alias {
    HashNode next;		/* next in hash chain       */
    char *nam;			/* hash data                */
    int flags;			/* flags for alias types    */
    char *text;			/* expansion of alias       */
    int inuse;			/* alias is being expanded  */
};

/* is this alias global */
#define ALIAS_GLOBAL	(1<<1)

/* node in command path hash table (cmdnamtab) */

struct cmdnam {
    HashNode next;		/* next in hash chain */
    char *nam;			/* hash data          */
    int flags;
    union {
	char **name;		/* full pathname for external commands */
	char *cmd;		/* file name for hashed commands       */
    }
    u;
};

/* flag for nodes explicitly added to *
 * cmdnamtab with hash builtin        */
#define HASHED		(1<<1)

/* node in shell function hash table (shfunctab) */

struct shfunc {
    HashNode next;		/* next in hash chain     */
    char *nam;			/* name of shell function */
    int flags;			/* various flags          */
    List funcdef;		/* function definition    */
};

/* node in builtin command hash table (builtintab) */

typedef int (*HandlerFunc) _((char *, char **, char *, int));
#define NULLBINCMD ((HandlerFunc) 0)

struct builtin {
    HashNode next;		/* next in hash chain                                 */
    char *nam;			/* name of builtin                                    */
    int flags;			/* various flags                                      */
    HandlerFunc handlerfunc;	/* pointer to function that executes this builtin     */
    int minargs;		/* minimum number of arguments                        */
    int maxargs;		/* maximum number of arguments, or -1 for no limit    */
    int funcid;			/* xbins (see above) for overloaded handlerfuncs      */
    char *optstr;		/* string of legal options                            */
    char *defopts;		/* options set by default for overloaded handlerfuncs */
};

#define BUILTIN(name, flags, handler, min, max, funcid, optstr, defopts) \
    { NULL, name, flags, handler, min, max, funcid, optstr, defopts }
#define BIN_PREFIX(name, flags) \
    BUILTIN(name, flags | BINF_PREFIX, NULLBINCMD, 0, 0, 0, NULL, NULL)

/* builtin flags */
/* DISABLE IS DEFINED AS (1<<0) */
#define BINF_PLUSOPTS		(1<<1)	/* +xyz legal */
#define BINF_R			(1<<2)	/* this is the builtin `r' (fc -e -) */
#define BINF_PRINTOPTS		(1<<3)
#define BINF_ADDED		(1<<4)	/* is in the builtins hash table */
#define BINF_FCOPTS		(1<<5)
#define BINF_TYPEOPT		(1<<6)
#define BINF_ECHOPTS		(1<<7)
#define BINF_MAGICEQUALS	(1<<8)  /* needs automatic MAGIC_EQUAL_SUBST substitution */
#define BINF_PREFIX		(1<<9)
#define BINF_DASH		(1<<10)
#define BINF_BUILTIN		(1<<11)
#define BINF_COMMAND		(1<<12)
#define BINF_EXEC		(1<<13)
#define BINF_NOGLOB		(1<<14)
#define BINF_PSPECIAL		(1<<15)

#define BINF_TYPEOPTS   (BINF_TYPEOPT|BINF_PLUSOPTS)

struct module {
    char *nam;
    int flags;
    void *handle;
    LinkList deps;
};

#define MOD_BUSY    (1<<0)

/* node used in parameter hash table (paramtab) */

struct param {
    HashNode next;		/* next in hash chain */
    char *nam;			/* hash data          */
    int flags;			/* PM_* flags         */

    /* the value of this parameter */
    union {
	void *data;		/* used by special parameter functions    */
	char **arr;		/* value if declared array   (PM_ARRAY)   */
	char *str;		/* value if declared string  (PM_SCALAR)  */
	long val;		/* value if declared integer (PM_INTEGER) */
    } u;

    /* pointer to function to set value of this parameter */
    union {
	void (*cfn) _((Param, char *));
	void (*ifn) _((Param, long));
	void (*afn) _((Param, char **));
    } sets;

    /* pointer to function to get value of this parameter */
    union {
	char *(*cfn) _((Param));
	long (*ifn) _((Param));
	char **(*afn) _((Param));
    } gets;

    /* pointer to function to unset this parameter */
    void (*unsetfn) _((Param, int));

    int ct;			/* output base or field width            */
    char *env;			/* location in environment, if exported  */
    char *ename;		/* name of corresponding environment var */
    Param old;			/* old struct for use with local         */
    int level;			/* if (old != NULL), level of localness  */
};

/* flags for parameters */

/* parameter types */
#define PM_SCALAR	0	/* scalar                                     */
#define PM_ARRAY	(1<<0)	/* array                                      */
#define PM_INTEGER	(1<<1)	/* integer                                    */

#define PM_TYPE(X) (X & (PM_SCALAR|PM_INTEGER|PM_ARRAY))

#define PM_LEFT		(1<<2)	/* left justify and remove leading blanks     */
#define PM_RIGHT_B	(1<<3)	/* right justify and fill with leading blanks */
#define PM_RIGHT_Z	(1<<4)	/* right justify and fill with leading zeros  */
#define PM_LOWER	(1<<5)	/* all lower case                             */

/* The following are the same since they *
 * both represent -u option to typeset   */
#define PM_UPPER	(1<<6)	/* all upper case                             */
#define PM_UNDEFINED	(1<<6)	/* undefined (autoloaded) shell function      */

#define PM_READONLY	(1<<7)	/* readonly                                   */
#define PM_TAGGED	(1<<8)	/* tagged                                     */
#define PM_EXPORTED	(1<<9)	/* exported                                   */
#define PM_UNIQUE	(1<<10)	/* remove duplicates                          */
#define PM_SPECIAL	(1<<11) /* special builtin parameter                  */
#define PM_DONTIMPORT	(1<<12)	/* do not import this variable                */
#define PM_RESTRICTED	(1<<13) /* cannot be changed in restricted mode       */
#define PM_UNSET	(1<<14)	/* has null value                             */

/* node for named directory hash table (nameddirtab) */

struct nameddir {
    HashNode next;		/* next in hash chain               */
    char *nam;			/* directory name                   */
    int flags;			/* see below                        */
    char *dir;			/* the directory in full            */
    int diff;			/* strlen(.dir) - strlen(.nam)      */
};

/* flags for named directories */
/* DISABLED is defined (1<<0) */
#define ND_USERNAME	(1<<1)	/* nam is actually a username       */


/* flags for controlling printing of hash table nodes */
#define PRINT_NAMEONLY		(1<<0)
#define PRINT_TYPE		(1<<1)
#define PRINT_LIST		(1<<2)

/* flags for printing for the whence builtin */
#define PRINT_WHENCE_CSH	(1<<3)
#define PRINT_WHENCE_VERBOSE	(1<<4)
#define PRINT_WHENCE_SIMPLE	(1<<5)
#define PRINT_WHENCE_FUNCDEF	(1<<6)
#define PRINT_WHENCE_WORD	(1<<7)

/***********************************/
/* Definitions for history control */
/***********************************/

/* history entry */

struct histent {
    char *text;			/* the history line itself          */
    char *zle_text;		/* the edited history line          */
    time_t stim;		/* command started time (datestamp) */
    time_t ftim;		/* command finished time            */
    short *words;		/* Position of words in history     */
				/*   line:  as pairs of start, end  */
    int nwords;			/* Number of words in history line  */
    int flags;			/* Misc flags                       */
};

#define HIST_OLD	0x00000001	/* Command is already written to disk*/
#define HIST_READ	0x00000002	/* Command was read back from disk*/

/* Parts of the code where history expansion is disabled *
 * should be within a pair of STOPHIST ... ALLOWHIST     */

#define STOPHIST (stophist += 4);
#define ALLOWHIST (stophist -= 4);

#define HISTFLAG_DONE   1
#define HISTFLAG_NOEXEC 2
#define HISTFLAG_RECALL 4


/******************************************/
/* Definitions for programable completion */
/******************************************/

/* Nothing special. */
#define IN_NOTHING 0
/* In command position. */
#define IN_CMD     1
/* In a mathematical environment. */
#define IN_MATH    2
/* In a condition. */
#define IN_COND    3
/* In a parameter assignment (e.g. `foo=bar'). */
#define IN_ENV     4


/******************************/
/* Definition for zsh options */
/******************************/

/* Possible values of emulation */

#define EMULATE_CSH  (1<<1) /* C shell */
#define EMULATE_KSH  (1<<2) /* Korn shell */
#define EMULATE_SH   (1<<3) /* Bourne shell */
#define EMULATE_ZSH  (1<<4) /* `native' mode */

/* option indices */

enum {
    OPT_INVALID,
    ALLEXPORT,
    ALWAYSLASTPROMPT,
    ALWAYSTOEND,
    APPENDHISTORY,
    AUTOCD,
    AUTOLIST,
    AUTOMENU,
    AUTONAMEDIRS,
    AUTOPARAMKEYS,
    AUTOPARAMSLASH,
    AUTOPUSHD,
    AUTOREMOVESLASH,
    AUTORESUME,
    BADPATTERN,
    BANGHIST,
    BAREGLOBQUAL,
    BEEP,
    BGNICE,
    BRACECCL,
    BSDECHO,
    CDABLEVARS,
    CHASELINKS,
    CLOBBER,
    COMPLETEALIASES,
    COMPLETEINWORD,
    CORRECT,
    CORRECTALL,
    CSHJUNKIEHISTORY,
    CSHJUNKIELOOPS,
    CSHJUNKIEQUOTES,
    CSHNULLGLOB,
    EQUALS,
    ERREXIT,
    EXECOPT,
    EXTENDEDGLOB,
    EXTENDEDHISTORY,
    FLOWCONTROL,
    FUNCTIONARGZERO,
    GLOBOPT,
    GLOBASSIGN,
    GLOBCOMPLETE,
    GLOBDOTS,
    GLOBSUBST,
    HASHCMDS,
    HASHDIRS,
    HASHLISTALL,
    HISTALLOWCLOBBER,
    HISTBEEP,
    HISTIGNOREDUPS,
    HISTIGNORESPACE,
    HISTNOFUNCTIONS,
    HISTNOSTORE,
    HISTREDUCEBLANKS,
    HISTVERIFY,
    HUP,
    IGNOREBRACES,
    IGNOREEOF,
    INTERACTIVE,
    INTERACTIVECOMMENTS,
    KSHARRAYS,
    KSHAUTOLOAD,
    KSHGLOB,
    KSHOPTIONPRINT,
    LISTAMBIGUOUS,
    LISTBEEP,
    LISTTYPES,
    LOCALOPTIONS,
    LOGINSHELL,
    LONGLISTJOBS,
    MAGICEQUALSUBST,
    MAILWARNING,
    MARKDIRS,
    MENUCOMPLETE,
    MONITOR,
    MULTIOS,
    NOMATCH,
    NOTIFY,
    NULLGLOB,
    NUMERICGLOBSORT,
    OVERSTRIKE,
    PATHDIRS,
    POSIXBUILTINS,
    PRINTEIGHTBIT,
    PRINTEXITVALUE,
    PRIVILEGED,
    PROMPTBANG,
    PROMPTCR,
    PROMPTPERCENT,
    PROMPTSUBST,
    PUSHDIGNOREDUPS,
    PUSHDMINUS,
    PUSHDSILENT,
    PUSHDTOHOME,
    RCEXPANDPARAM,
    RCQUOTES,
    RCS,
    RECEXACT,
    RESTRICTED,
    RMSTARSILENT,
    RMSTARWAIT,
    SHFILEEXPANSION,
    SHGLOB,
    SHINSTDIN,
    SHOPTIONLETTERS,
    SHORTLOOPS,
    SHWORDSPLIT,
    SINGLECOMMAND,
    SINGLELINEZLE,
    SUNKEYBOARDHACK,
    UNSET,
    VERBOSE,
    XTRACE,
    USEZLE,
    OPT_SIZE
};

#undef isset
#define isset(X) (opts[X])
#define unset(X) (!opts[X])

#define interact (isset(INTERACTIVE))
#define jobbing  (isset(MONITOR))
#define islogin  (isset(LOGINSHELL))

/***********************************************/
/* Defintions for terminal and display control */
/***********************************************/

/* tty state structure */

struct ttyinfo {
#ifdef HAVE_TERMIOS_H
    struct termios tio;
#else
# ifdef HAVE_TERMIO_H
    struct termio tio;
# else
    struct sgttyb sgttyb;
    int lmodes;
    struct tchars tchars;
    struct ltchars ltchars;
# endif
#endif
#ifdef TIOCGWINSZ
    struct winsize winsize;
#endif
};

/* defines for whether tabs expand to spaces */
#if defined(HAVE_TERMIOS_H) || defined(HAVE_TERMIO_H)
#define SGTTYFLAG       shttyinfo.tio.c_oflag
#else   /* we're using sgtty */
#define SGTTYFLAG       shttyinfo.sgttyb.sg_flags
#endif
# ifdef TAB3
#define SGTABTYPE       TAB3
# else
#  ifdef OXTABS
#define SGTABTYPE       OXTABS
#  else
#define SGTABTYPE       XTABS
#  endif
# endif

/* flags for termflags */

#define TERM_BAD	0x01	/* terminal has extremely basic capabilities */
#define TERM_UNKNOWN	0x02	/* unknown terminal type */
#define TERM_NOUP	0x04	/* terminal has no up capability */
#define TERM_SHORT	0x08	/* terminal is < 3 lines high */
#define TERM_NARROW	0x10	/* terminal is < 3 columns wide */

/* interesting termcap strings */

#define TCCLEARSCREEN   0
#define TCLEFT          1
#define TCMULTLEFT      2
#define TCRIGHT         3
#define TCMULTRIGHT     4
#define TCUP            5
#define TCMULTUP        6
#define TCDOWN          7
#define TCMULTDOWN      8
#define TCDEL           9
#define TCMULTDEL      10
#define TCINS          11
#define TCMULTINS      12
#define TCCLEAREOD     13
#define TCCLEAREOL     14
#define TCINSLINE      15
#define TCDELLINE      16
#define TCNEXTTAB      17
#define TCBOLDFACEBEG  18
#define TCSTANDOUTBEG  19
#define TCUNDERLINEBEG 20
#define TCALLATTRSOFF  21
#define TCSTANDOUTEND  22
#define TCUNDERLINEEND 23
#define TC_COUNT       24

#define tccan(X) (tclen[X])

#define TXTBOLDFACE   0x01
#define TXTSTANDOUT   0x02
#define TXTUNDERLINE  0x04
#define TXTDIRTY      0x80

#define txtisset(X)  (txtattrmask & (X))
#define txtset(X)    (txtattrmask |= (X))
#define txtunset(X)  (txtattrmask &= ~(X))

#define TXTNOBOLDFACE	0x10
#define TXTNOSTANDOUT	0x20
#define TXTNOUNDERLINE	0x40

#define txtchangeisset(X)	(txtchange & (X))
#define txtchangeset(X, Y)	(txtchange |= (X), txtchange &= ~(Y))

/****************************************/
/* Definitions for the %_ prompt escape */
/****************************************/

#define cmdpush(X) if (!(cmdsp >= 0 && cmdsp < 256)) {;} else cmdstack[cmdsp++]=(X)
#ifdef DEBUG
# define cmdpop()  if (cmdsp <= 0) { \
			fputs("BUG: cmdstack empty\n", stderr); \
			fflush(stderr); \
		   } else cmdsp--
#else
# define cmdpop()   if (cmdsp <= 0) {;} else cmdsp--
#endif

#define CS_FOR          0
#define CS_WHILE        1
#define CS_REPEAT       2
#define CS_SELECT       3
#define CS_UNTIL        4
#define CS_IF           5
#define CS_IFTHEN       6
#define CS_ELSE         7
#define CS_ELIF         8
#define CS_MATH         9
#define CS_COND        10
#define CS_CMDOR       11
#define CS_CMDAND      12
#define CS_PIPE        13
#define CS_ERRPIPE     14
#define CS_FOREACH     15
#define CS_CASE        16
#define CS_FUNCDEF     17
#define CS_SUBSH       18
#define CS_CURSH       19
#define CS_ARRAY       20
#define CS_QUOTE       21
#define CS_DQUOTE      22
#define CS_BQUOTE      23
#define CS_CMDSUBST    24
#define CS_MATHSUBST   25
#define CS_ELIFTHEN    26
#define CS_HEREDOC     27
#define CS_HEREDOCD    28
#define CS_BRACE       29
#define CS_BRACEPAR    30

/*********************
 * Memory management *
 *********************/

#ifndef DEBUG
# define HEAPALLOC	do { int nonlocal_useheap = global_heapalloc(); do

# define PERMALLOC	do { int nonlocal_useheap = global_permalloc(); do

# define LASTALLOC	while (0); \
			if (nonlocal_useheap) global_heapalloc(); \
			else global_permalloc(); \
		} while(0)

# define LASTALLOC_RETURN \
			if ((nonlocal_useheap ? global_heapalloc() : \
			     global_permalloc()), 0) {;} else return
#else
# define HEAPALLOC	do { int nonlocal_useheap = global_heapalloc(); \
			alloc_stackp++; do

# define PERMALLOC	do { int nonlocal_useheap = global_permalloc(); \
			alloc_stackp++; do

# define LASTALLOC	while (0); alloc_stackp--; \
			if (nonlocal_useheap) global_heapalloc(); \
			else global_permalloc(); \
		} while(0)

# define LASTALLOC_RETURN \
			if ((nonlocal_useheap ? global_heapalloc() : \
			    global_permalloc()),alloc_stackp--,0){;}else return
#endif

/****************/
/* Debug macros */
/****************/

#ifdef DEBUG
# define DPUTS(X,Y) if (!(X)) {;} else dputs(Y)
# define MUSTUSEHEAP(X) if (useheap) {;} else \
		fprintf(stderr, "BUG: permanent allocation in %s\n", X), \
		fflush(stderr)
#else
# define DPUTS(X,Y)
# define MUSTUSEHEAP(X)
#endif

/**************************/
/* Signal handling macros */
/**************************/

/* These used in the sigtrapped[] array */

#define ZSIG_TRAPPED	(1<<0)
#define ZSIG_IGNORED	(1<<1)
#define ZSIG_FUNC	(1<<2)

/****************/
/* Entry points */
/****************/

/* compctl entry point pointers */

typedef int (*CompctlReadFn) _((char *, char **, char *, char *));

/* ZLE entry point pointers */

typedef void (*ZleVoidFn) _((void));
typedef void (*ZleVoidIntFn) _((int));
typedef unsigned char * (*ZleReadFn) _((char *, char *, int));
