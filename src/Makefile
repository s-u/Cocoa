#
# ${R_HOME}/src/modules/aqua/Makefile

all: cocoa.so

cocoa.lo: cocoa.m
	gcc -c cocoa.m -o cocoa.lo -fno-common

cglue.lo: cglue.c
	gcc -c cglue.c -o cglue.lo -fno-common

cocoa.so: cocoa.lo cglue.lo
	gcc -bundle -flat_namespace -undefined suppress -o $@ cocoa.lo cglue.lo -framework Cocoa

clean:
	rm -f *.lo *.o *.so *~
