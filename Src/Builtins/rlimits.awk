#
# rlimits.awk: {g,n}awk script to generate rlimits.h
#
# NB: On SunOS 4.1.3 - user-functions don't work properly, also \" problems
# Without 0 + hacks some nawks compare numbers as strings
#
BEGIN {limidx = 0}

/^[\t ]*(#[\t ]*define[\t _]*RLIMIT_[A-Z_]*[\t ]*[0-9][0-9]*|RLIMIT_[A-Z_]*,[\t ]*|_*RLIMIT_[A-Z_]*[\t ]*=[\t ]*[0-9][0-9]*,[\t ]*)/ {
    limindex = index($0, "RLIMIT_")
    limtail = substr($0, limindex, 80)
    split(limtail, tmp)
    limnam = substr(tmp[1], 8, 20)
    limnum = tmp[2]
    # in this case I assume GNU libc resourcebits.h
    if (limnum == "") {
	limnum = limidx++
	limindex = index($0, ",")
	limnam = substr(limnam, 1, limindex-1)
    }
    if (limnum == "=") {
	if (tmp[3] ~ /^[0-9]/) {
	    limnum = tmp[3] + 0
	} else {
	    limnum = limidx++
	}
	limindex = index($0, ",")
	limnam = substr(limnam, 1, limindex-1)
    }
    limrev[limnam] = limnum
    if (lim[limnum] == "") {
	lim[limnum] = limnam
	if (limnum ~ /^[0-9]*$/) {
	    if (limnam == "AIO_MEM") { msg[limnum] = "Maiomemorylocked" }
	    if (limnam == "AIO_OPS") { msg[limnum] = "Naiooperations" }
	    if (limnam == "AS")      { msg[limnum] = "Maddressspace" }
	    if (limnam == "CORE")    { msg[limnum] = "Mcoredumpsize" }
	    if (limnam == "CPU")     { msg[limnum] = "Tcputime" }
	    if (limnam == "DATA")    { msg[limnum] = "Mdatasize" }
	    if (limnam == "FSIZE")   { msg[limnum] = "Mfilesize" }
	    if (limnam == "LOCKS")   { msg[limnum] = "Nmaxfilelocks" }
	    if (limnam == "MEMLOCK") { msg[limnum] = "Mmemorylocked" }
	    if (limnam == "NOFILE")  { msg[limnum] = "Ndescriptors" }
	    if (limnam == "NPROC")   { msg[limnum] = "Nmaxproc" }
	    if (limnam == "OFILE")   { msg[limnum] = "Ndescriptors" }
	    if (limnam == "PTHREAD") { msg[limnum] = "Nmaxpthreads" }
	    if (limnam == "RSS")     { msg[limnum] = "Mresident" }
	    if (limnam == "SBSIZE")  { msg[limnum] = "Msockbufsize" }
	    if (limnam == "STACK")   { msg[limnum] = "Mstacksize" }
	    if (limnam == "TCACHE")  { msg[limnum] = "Ncachedthreads" }
	    if (limnam == "VMEM")    { msg[limnum] = "Mvmemorysize" }
	    if (limnam == "SIGPENDING") { msg[limnum] = "Nsigpending" }
	    if (limnam == "MSGQUEUE") { msg[limnum] = "Nmsgqueue" }
	    if (limnam == "NICE") { msg[limnum] = "Nnice" }
	    if (limnam == "RTPRIO") { msg[limnum] = "Nrt_priority" }
        }
    }
}
/^[\t ]*#[\t ]*define[\t _]*RLIM_NLIMITS[\t ]*[0-9][0-9]*/ {
    limindex = index($0, "RLIM_")
    limtail = substr($0, limindex, 80)
    split(limtail, tmp)
    nlimits = tmp[2]
}
# in case of GNU libc
/^[\t ]*RLIM_NLIMITS[\t ]*=[\t ]*RLIMIT_NLIMITS/ {
    if(!nlimits) { nlimits = limidx }
}
/^[\t _]*RLIM(IT)?_NLIMITS[\t ]*=[\t ]*[0-9][0-9]*/ {
    limindex = index($0, "=")
    limtail = substr($0, limindex, 80)
    split(limtail, tmp)
    nlimits = tmp[2]
}

END {
    if (limrev["MEMLOCK"] != "") {
        irss = limrev["RSS"]
        msg[irss] = "Mmemoryuse"
    }
    ps = "%s"

    printf("%s\n%s\n\n", "/** rlimits.h                              **/", "/** architecture-customized limits for zsh **/")
    printf("#define ZSH_NLIMITS %d\n\nstatic char const *recs[ZSH_NLIMITS] = {\n", 0 + nlimits)

    for (i = 0; i < 0 + nlimits; i++)
	if (msg[i] == "")
            printf("\t%c%s%c,\n", 34, lim[i], 34)
	else
	    printf("\t%c%s%c,\n", 34, substr(msg[i], 2, 30), 34)
    print "};"
    print ""
    print "static int limtype[ZSH_NLIMITS] = {"
    for (i = 0; i < 0 + nlimits; i++) {
	if (msg[i] == "")
	    limtype = "UNKNOWN"
	else {
	    limtype = substr(msg[i], 1, 1)
	    if(limtype == "M") { limtype = "MEMORY" }
	    if(limtype == "N") { limtype = "NUMBER" }
	    if(limtype == "T") { limtype = "TIME" }
	}
	printf("\tZLIMTYPE_%s,\n", limtype)
    }
    print "};"

    exit(0)
}
