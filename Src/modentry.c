#include "zsh.mdh"

int boot_ _((Module));
int cleanup_ _((Module));
int modentry _((int boot, Module m));

/**/
int
modentry(int boot, Module m)
{
    if (boot)
	return boot_(m);
    else
	return cleanup_(m);
}
