/*
 * attr.c - extended attributes (xattr) manipulation
 *
 * This file is part of zsh, the Z shell.
 *
 * Copyright (c) 2009 Mikael Magnusson
 * All rights reserved.
 *
 * Permission is hereby granted, without written agreement and without
 * license or royalty fees, to use, copy, modify, and distribute this
 * software and to distribute modified versions of this software for any
 * purpose, provided that the above copyright notice and the following
 * two paragraphs appear in all copies of this software.
 *
 * In no event shall Mikael Magnusson or the Zsh Development Group be liable
 * to any party for direct, indirect, special, incidental, or consequential
 * damages arising out of the use of this software and its documentation,
 * even if Andrew Main and the Zsh Development Group have been advised of
 * the possibility of such damage.
 *
 * Mikael Magnusson and the Zsh Development Group specifically disclaim any
 * warranties, including, but not limited to, the implied warranties of
 * merchantability and fitness for a particular purpose.  The software
 * provided hereunder is on an "as is" basis, and Mikael Magnusson and the
 * Zsh Development Group have no obligation to provide maintenance,
 * support, updates, enhancements, or modifications.
 *
 */

#include "attr.mdh"
#include "attr.pro"

#include <sys/types.h>
#include <sys/xattr.h>

static int
bin_getattr(char *nam, char **argv, UNUSED(Options ops), UNUSED(int func))
{
    int ret = 0;
    int len, slen;
    char value[256];

    unmetafy(*argv, &slen);
    unmetafy(*(argv+1), NULL);
    if (listxattr(*argv, NULL, 0
#ifdef XATTR_EXTRA_ARGS
		  , 0
#endif
		  ) > 0) {
        if (0 < (len = getxattr(*argv, *(argv+1), value, 255
#ifdef XATTR_EXTRA_ARGS
				, 0, 0
#endif
				))) {
            if (len < 256) {
                value[len] = '\0';
                if (*(argv+2))
                    setsparam(*(argv+2), metafy(value, len, META_DUP));
                else
                    printf("%s\n", value);
            }
        } else if (len < 0) {
            zwarnnam(nam, "%s: %e", metafy(*argv, slen, META_NOALLOC), errno);
            ret = 1;
        }
    }
    return ret;
}

static int
bin_setattr(char *nam, char **argv, UNUSED(Options ops), UNUSED(int func))
{
    int ret = 0, slen;

    unmetafy(*argv, &slen);
    unmetafy(*(argv+1), NULL);
    unmetafy(*(argv+2), NULL);
    if (setxattr(*argv, *(argv+1), *(argv+2), strlen(*(argv+2)), 0
#ifdef XATTR_EXTRA_ARGS
						     , 0
#endif
		 )) {
        zwarnnam(nam, "%s: %e", metafy(*argv, slen, META_NOALLOC), errno);
        ret = 1;
    }
    return ret;
}

static int
bin_delattr(char *nam, char **argv, UNUSED(Options ops), UNUSED(int func))
{
    int ret = 0, slen;

    unmetafy(*argv, &slen);
    unmetafy(*(argv+1), NULL);
    if (removexattr(*argv, *(argv+1)
#ifdef XATTR_EXTRA_ARGS
		    , 0
#endif
		    )) {
        zwarnnam(nam, "%s: %e", metafy(*argv, slen, META_NOALLOC), errno);
        ret = 1;
    }
    return ret;
}

static int
bin_listattr(char *nam, char **argv, UNUSED(Options ops), UNUSED(int func))
{
    int ret = 0;
    int len, slen;
    char value[256];

    unmetafy(*argv, &slen);
    if (0 < (len = listxattr(*argv, value, 256
#ifdef XATTR_EXTRA_ARGS
		  , 0
#endif
			     ))) {
        if (len < 256) {
            char *p = value;
            if (*(argv+1))
                setsparam(*(argv+1), metafy(value, len, META_DUP));
            else while (p < &value[len]) {
                printf("%s\n", p);
                p += strlen(p) + 1;
            }
        }
    } else if (len < 0) {
        zwarnnam(nam, "%s: %e", metafy(*argv, slen, META_NOALLOC), errno);
        ret = 1;
    }
    return ret;
}

/* module paraphernalia */

static struct builtin bintab[] = {
    BUILTIN("zgetattr", 0, bin_getattr, 2, 3, 0, NULL, NULL),
    BUILTIN("zsetattr", 0, bin_setattr, 3, 3, 0, NULL, NULL),
    BUILTIN("zdelattr", 0, bin_delattr, 2, 2, 0, NULL, NULL),
    BUILTIN("zlistattr", 0, bin_listattr, 1, 2, 0, NULL, NULL),
};

static struct features module_features = {
    bintab, sizeof(bintab)/sizeof(*bintab),
    NULL, 0,
    NULL, 0,
    NULL, 0,
    0
};

/**/
int
setup_(UNUSED(Module m))
{
    return 0;
}

/**/
int
features_(Module m, char ***features)
{
    *features = featuresarray(m, &module_features);
    return 0;
}

/**/
int
enables_(Module m, int **enables)
{
    return handlefeatures(m, &module_features, enables);
}

/**/
int
boot_(UNUSED(Module m))
{
    return 0;
}

/**/
int
cleanup_(Module m)
{
    return setfeatureenables(m, &module_features, NULL);
}

/**/
int
finish_(UNUSED(Module m))
{
    return 0;
}
