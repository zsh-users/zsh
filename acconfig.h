
/***** begin user configuration section *****/

/* Define this to be the location of your password file */
#define PASSWD_FILE "/etc/passwd"

/* Define this to be the name of your NIS/YP password *
 * map (if applicable)                                */
#define PASSWD_MAP "passwd.byname"

/* Define to 1 if you want user names to be cached */
#define CACHE_USERNAMES 1

/* Define to 1 if system supports job control */
#define JOB_CONTROL 1

/* Define this if you use "suspended" instead of "stopped" */
#define USE_SUSPENDED 1
 
/* The default history buffer size in lines */
#define DEFAULT_HISTSIZE 30

/* The default editor for the fc builtin */
#define DEFAULT_FCEDIT "vi"

/* The default prefix for temporary files */
#define DEFAULT_TMPPREFIX "/tmp/zsh"


/***** end of user configuration section            *****/
/***** shouldn't have to change anything below here *****/
@TOP@

/* The global file to source absolutely first whenever zsh is run; *
 * if undefined, don't source anything                             */
#undef GLOBAL_ZSHENV

/* The global file to source whenever zsh is run; *
 * if undefined, don't source anything            */
#undef GLOBAL_ZSHRC

/* The global file to source whenever zsh is run as a login shell; *
 * if undefined, don't source anything                             */
#undef GLOBAL_ZLOGIN

/* The global file to source whenever zsh is run as a login shell, *
 * before zshrc is read; if undefined, don't source anything       */
#undef GLOBAL_ZPROFILE

/* The global file to source whenever zsh was run as a login shell.  *
 * This is sourced right before exiting.  If undefined, don't source *
 * anything                                                          */
#undef GLOBAL_ZLOGOUT

/* Define to 1 if compiler could initialise a union */
#undef HAVE_UNION_INIT

/* Define to 1 if compiler incorrectly cast signed to unsigned */
#undef BROKEN_SIGNED_TO_UNSIGNED_CASTING

/* Define to 1 if compiler supports variable-length arrays */
#undef HAVE_VARIABLE_LENGTH_ARRAYS

/* Define if your system defines TIOCGWINSZ in sys/ioctl.h.  */
#undef GWINSZ_IN_SYS_IOCTL

/* Define to 1 if you have NIS */
#undef HAVE_NIS

/* Define to 1 if you have NISPLUS */
#undef HAVE_NIS_PLUS

/* Define to 1 if you have RFS superroot directory. */
#undef HAVE_SUPERROOT

/* Define to 1 if you need to use the native getcwd */
#undef USE_GETCWD

/* Define to the path of the /dev/fd filesystem */
#undef PATH_DEV_FD

/* Define if sys/time.h and sys/select.h cannot be both included */
#undef TIME_H_SELECT_H_CONFLICTS

/* Define to be the machine type (microprocessor class or machine model) */
#undef MACHTYPE

/* Define to be the name of the operating system */
#undef OSTYPE

/* Define to 1 if ANSI function prototypes are usable.  */
#undef PROTOTYPES

/* Define to be location of utmp file. */
#undef PATH_UTMP_FILE

/* Define to be location of utmpx file. */
#undef PATH_UTMPX_FILE

/* Define to be location of wtmp file. */
#undef PATH_WTMP_FILE

/* Define to be location of wtmpx file. */
#undef PATH_WTMPX_FILE

/* Define to 1 if struct utmp is defined by a system header */
#undef HAVE_STRUCT_UTMP

/* Define to 1 if struct utmpx is defined by a system header */
#undef HAVE_STRUCT_UTMPX

/* Define if your system's struct utmp has a member named ut_host.  */
#undef HAVE_STRUCT_UTMP_UT_HOST

/* Define if your system's struct utmpx has a member named ut_host.  */
#undef HAVE_STRUCT_UTMPX_UT_HOST

/* Define if your system's struct utmpx has a member named ut_xtime.  */
#undef HAVE_STRUCT_UTMPX_UT_XTIME

/* Define if your system's struct utmpx has a member named ut_tv.  */
#undef HAVE_STRUCT_UTMPX_UT_TV

/* Define if your system's struct dirent has a member named d_ino.  */
#undef HAVE_STRUCT_DIRENT_D_INO

/* Define if your system's struct dirent has a member named d_stat.  */
#undef HAVE_STRUCT_DIRENT_D_STAT

/* Define if your system's struct direct has a member named d_ino.  */
#undef HAVE_STRUCT_DIRECT_D_INO

/* Define if your system's struct direct has a member named d_stat.  */
#undef HAVE_STRUCT_DIRECT_D_STAT

/* Define if your system's struct sockaddr_in6 has a member named sin6_scope_id.  */
#undef HAVE_STRUCT_SOCKADDR_IN6_SIN6_SCOPE_ID

/* Define to be a string corresponding the vendor of the machine */
#undef VENDOR

/* Define to limit job table size */
#undef MAXJOB
#undef NEED_LINUX_TASKS_H

/* Define if your system defines `struct winsize' in sys/ptem.h.  */
#undef WINSIZE_IN_PTEM

/* Define to 1 if you want to debug zsh */
#undef DEBUG

/* Define to 1 if you want to use zsh's own memory allocation routines */
#undef ZSH_MEM

/* Define to 1 if you want to debug zsh memory allocation routines */
#undef ZSH_MEM_DEBUG

/* Define to 1 if you want to turn on warnings of memory allocation errors */
#undef ZSH_MEM_WARNING

/* Define to 1 if you want to turn on memory checking for free() */
#undef ZSH_SECURE_FREE

/* Define to 1 if you want to get debugging information on internal *
 * hash tables.  This turns on the `hashinfo' builtin.              */
#undef ZSH_HASH_DEBUG

/* Undefine this if you don't want to get a restricted shell *
 * when zsh is exec'd with basename that starts with r.      *
 * By default this is defined.                               */
#undef RESTRICTED_R

/* Define for Maildir support */
#undef MAILDIR_SUPPORT

/* Define for function depth limits */
#undef MAX_FUNCTION_DEPTH

/* Define if you want locale features.  By default this is defined. */
#undef CONFIG_LOCALE

/* Define to 1 if your termcap library has the ospeed variable */
#undef HAVE_OSPEED
/* Define to 1 if you have ospeed, but it is not defined in termcap.h */
#undef MUST_DEFINE_OSPEED

/* Define to 1 if tgetent() accepts NULL as a buffer */
#undef TGETENT_ACCEPTS_NULL

/* Define to what tgetent() returns on success (0 on HP-UX X/Open curses) */
#define TGETENT_SUCCESS 1

/* Define to 1 if you use POSIX style signal handling */
#undef POSIX_SIGNALS

/* Define to 1 if you use BSD style signal handling (and can block signals) */
#undef BSD_SIGNALS

/* Define to 1 if you use SYS style signal handling (and can block signals) */
#undef SYSV_SIGNALS

/* Define to 1 if you have no signal blocking at all (bummer) */
#undef NO_SIGNAL_BLOCKING

/* Define to `unsigned int' if <sys/types.h> or <signal.h> doesn't define */
#undef sigset_t

/* Define to 1 if struct timezone is defined by a system header */
#undef HAVE_STRUCT_TIMEZONE

/* Define to 1 if there is a prototype defined for brk() on your system */
#undef HAVE_BRK_PROTO

/* Define to 1 if there is a prototype defined for sbrk() on your system */
#undef HAVE_SBRK_PROTO

/* Define to 1 if there is a prototype defined for ioctl() on your system */
#undef HAVE_IOCTL_PROTO

/* Define to 1 if there is a prototype defined for mknod() on your system */
#undef HAVE_MKNOD_PROTO

/* Define to 1 if select() is defined in <sys/socket.h>, ie BeOS R4.51*/
#undef SELECT_IN_SYS_SOCKET_H

/* Define to 1 if system has working FIFO's */
#undef HAVE_FIFOS

/* Define to 1 if struct rlimit uses quad_t */
#undef RLIM_T_IS_QUAD_T

/* Define to 1 if struct rlimit uses long long */
#undef RLIM_T_IS_LONG_LONG

/* Define to 1 if rlimit uses unsigned */
#undef RLIM_T_IS_UNSIGNED

/* Define to the type used in struct rlimit */
#undef rlim_t

/* Define to 1 if RLIMIT_AIO_MEM is present (whether or not as a macro) */
#undef HAVE_RLIMIT_AIO_MEM

/* Define to 1 if RLIMIT_AIO_OPS is present (whether or not as a macro) */
#undef HAVE_RLIMIT_AIO_OPS

/* Define to 1 if RLIMIT_AS is present (whether or not as a macro) */
#undef HAVE_RLIMIT_AS

/* Define to 1 if RLIMIT_LOCKS is present (whether or not as a macro) */
#undef HAVE_RLIMIT_LOCKS

/* Define to 1 if RLIMIT_MEMLOCK is present (whether or not as a macro) */
#undef HAVE_RLIMIT_MEMLOCK

/* Define to 1 if RLIMIT_NPROC is present (whether or not as a macro) */
#undef HAVE_RLIMIT_NPROC

/* Define to 1 if RLIMIT_NOFILE is present (whether or not as a macro) */
#undef HAVE_RLIMIT_NOFILE

/* Define to 1 if RLIMIT_PTHREAD is present (whether or not as a macro) */
#undef HAVE_RLIMIT_PTHREAD

/* Define to 1 if RLIMIT_RSS is present (whether or not as a macro) */
#undef HAVE_RLIMIT_RSS

/* Define to 1 if RLIMIT_SBSIZE is present (whether or not as a macro) */
#undef HAVE_RLIMIT_SBSIZE

/* Define to 1 if RLIMIT_TCACHE is present (whether or not as a macro) */
#undef HAVE_RLIMIT_TCACHE

/* Define to 1 if RLIMIT_VMEM is present (whether or not as a macro) */
#undef HAVE_RLIMIT_VMEM

/* Define to 1 if RLIMIT_VMEM and RLIMIT_RSS both exist and are equal */
#undef RLIMIT_VMEM_IS_RSS

/* Define to 1 if RLIMIT_VMEM and RLIMIT_AS both exist and are equal */
#undef RLIMIT_VMEM_IS_AS

/* Define to 1 if /bin/sh does not interpret \ escape sequences */
#undef SH_USE_BSD_ECHO

/* Define to 1 if system has working link() */
#undef HAVE_LINK

/* Define to 1 if kill(pid, 0) doesn't return ESRCH, ie BeOS R4.51 */
#undef BROKEN_KILL_ESRCH

/* Define to 1 if sigsuspend() is broken, ie BeOS R4.51 */
#undef BROKEN_POSIX_SIGSUSPEND

/* Define to 1 if getpwnam() is faked, ie BeOS R4.51 */
#undef GETPWNAM_FAKED

/* Define to 1 if tcsetpgrp() doesn't work, ie BeOS R4.51 */
#undef BROKEN_TCSETPGRP

/* Define to 1 if an underscore has to be prepended to dlsym() argument */
#undef DLSYM_NEEDS_UNDERSCORE

/* Define to 1 if multiple modules defining the same symbol are OK */
#undef DYNAMIC_NAME_CLASH_OK

/* The extension used for dynamically loaded modules */
#undef DL_EXT

/* Define to 1 if you want to use dynamically loaded modules */
#undef DYNAMIC

/* Define to 1 if you want to use dynamically loaded modules on AIX */
#undef AIXDYNAMIC

/* Define to 1 if you want to use dynamically loaded modules on HPUX 10 */
#undef HPUXDYNAMIC

/* Define to `unsigned long' if <sys/types.h> doesn't define. */
#undef ino_t

/*
 * Definitions used when a long is less than eight byte, to try to
 * provide some support for eight byte operations.
 *
 * Note that ZSH_64_BIT_TYPE, OFF_T_IS_64_BIT, INO_T_IS_64_BIT do *not* get
 * defined if long is already 64 bits, since in that case no special handling
 * is required.
 */
/* Define to 1 if long is 64 bits */
#undef LONG_IS_64_BIT

/* Define to a 64 bit integer type if there is one, but long is shorter */
#undef ZSH_64_BIT_TYPE

/* Define to an unsigned variant of ZSH_64_BIT_TYPE if that is defined */
#undef ZSH_64_BIT_UTYPE

/* Define to 1 if off_t is 64 bit (for large file support) */
#undef OFF_T_IS_64_BIT

/* Define to 1 if ino_t is 64 bit (for large file support) */
#undef INO_T_IS_64_BIT

/* Define to 1 if h_errno is not defined by the system */
#undef USE_LOCAL_H_ERRNO

/* Define if you have the termcap boolcodes symbol.  */
#undef HAVE_BOOLCODES

/* Define if you have the termcap numcodes symbol.  */
#undef HAVE_NUMCODES

/* Define if you have the termcap strcodes symbol.  */
#undef HAVE_STRCODES

/* Define if you have the terminfo boolnames symbol.  */
#undef HAVE_BOOLNAMES

/* Define if you have the terminfo numnames symbol.  */
#undef HAVE_NUMNAMES

/* Define if you have the terminfo strnames symbol.  */
#undef HAVE_STRNAMES

/* Define if term.h chokes without curses.h */
#undef TERM_H_NEEDS_CURSES_H

/* Define to the base type of the third argument of accept */
#undef SOCKLEN_T
