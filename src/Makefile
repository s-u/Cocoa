all: cocoa.so

cocoa.lo: cocoa.m
	gcc -c cocoa.m -o cocoa.lo -fno-common -I$(R_HOME)/../Headers $(PKG_CFLAGS)

cglue.lo: cglue.c
	gcc -c cglue.c -o cglue.lo -fno-common $(PKG_CFLAGS)

cocoa.so: cocoa.lo cglue.lo
	gcc -bundle -flat_namespace -undefined suppress -o $@ cocoa.lo cglue.lo -framework Cocoa -framework R $(PKG_LDFLAGS)

clean:
	rm -f *.lo *.o *.so *~
