BEGIN {nkeydefs = 0}

/^[\t ]*#[\t ]*define[\t _]*KEY_[A-Z0-9_]*[\t ]/ {
    keyindex = index($0, "KEY_")
    keytail = substr($0, keyindex, 80)
    split(keytail, tmp)
    keynam = substr(tmp[1], 5, 30)
    if (keynam != "MIN" && keynam != "MAX" && keynam != "CODE_YES") {
	name[nkeydefs++] = keynam
    }
}

END {
    printf("static const struct zcurses_namenumberpair keypad_names[] = {\n")
    for (i = 0; i < 0 + nkeydefs; i++)
        printf("    {\"%s\", KEY_%s},\n", name[i], name[i])
    printf("    {NULL, 0}\n")
    printf("};\n")
}
