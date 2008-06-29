#import <Foundation/Foundation.h>
#ifdef __APPLE__
#import <Cocoa/Cocoa.h>
#include <objc/objc.h>
#include <objc/objc-runtime.h>
#endif
#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>

#ifndef __APPLE__
#define objc_getClass objc_get_class
#define objc_lookUpClass objc_lookup_class
#endif

#ifdef DEBUG
#define Dprintf Rprintf
#else
#define Dprintf(...)
#endif

NSAutoreleasePool * pool = NULL;

void InitializeCocoa()
{
  /* need a auto release pool for allocating objects
   *
   * BUT inside R.app it's very dangerous to create a new pool, because it could be
   * destroyed whenever R.app releases its pools. This is normally ok, but since the
   * use is likely to relay on the persistence of objects as ruled by R it's much safer
   * to let it be and let R.app manage the pools */
	if (!objc_lookUpClass("RController"))
		pool = [[NSAutoreleasePool alloc] init];

    /* Gaining access to the shared application.  This is a very important step because it initializes
     * the Cocoa envionment.  This is required to do things like put up Cocoa alert dialogs.  
     */
#ifdef __APPLE_
    [NSApplication sharedApplication];
#endif
}

void DeInitializeCocoa()
{
  /* deallocate the autorelease pool if neccessary. */
    if (pool)
        [pool release];
}

/* send release msg to the corresponding Obj-C object as soon as it's no longer needed in R */
static void idObjCfinalizer(SEXP obj)
{
	if (TYPEOF(obj)==LISTSXP)
		obj=CAR(obj);
    if (TYPEOF(obj)==EXTPTRSXP) {
        id o = R_ExternalPtrAddr(obj);
        if (o) [o release];
    }
}

/* convert Obj-C object into Obj-C reference in R - note that the object automatically receives a retain message */
static SEXP ObjC2SEXP(id obj)
{
	SEXP class;
    SEXP sref = R_MakeExternalPtr((void*) obj, R_NilValue, R_NilValue);
    SEXP robj = CONS(sref, R_NilValue);
	SET_TAG(robj, install("ref"));
    [obj retain];
	PROTECT(class = allocVector(STRSXP, 1));
	SET_STRING_ELT(class, 0, mkChar("ObjCid"));
    SET_CLASS(robj, class);
	UNPROTECT(1);
	R_RegisterCFinalizer(sref, idObjCfinalizer); /* should we use Ex and onExit=TRUE? */
    return robj;
}

static SEXP ObjCclass2SEXP(id obj)
{ /* we make a reference, but we don't retain it and don't register a finalizer */
	SEXP class, sref = R_MakeExternalPtr((void*) obj, R_NilValue, R_NilValue);
    SEXP robj = CONS(sref, R_NilValue);
	SET_TAG(robj, install("ref"));
	PROTECT(class = allocVector(STRSXP, 2));
	SET_STRING_ELT(class, 0, mkChar("ObjCclass"));
	SET_STRING_ELT(class, 1, mkChar("ObjCid"));
	SET_CLASS(robj, class);
	UNPROTECT(1);
	return robj;
}

/* convert Obj-C reference in R into Obj-C object */
static id SEXP2ObjC(SEXP ref)
{
	if (TYPEOF(ref)==LISTSXP)
		ref=CAR(ref);
    if (TYPEOF(ref)!=EXTPTRSXP)
        error("Obj-C reference must be an external pointer.");
    return (id) R_ExternalPtrAddr(ref);
}

/* the following two functions are useful only in ObjR where all SEXPs are also ObjC objects - they allow the mapping of native objects into ObjCid objects and back */

SEXP id_deref(SEXP ref) {
  if (TYPEOF(ref)==LISTSXP)
    ref=CAR(ref);
  if (TYPEOF(ref)!=EXTPTRSXP)
    error("Obj-C reference must be an external pointer.");
  return (SEXP) R_ExternalPtrAddr(ref);
}

SEXP id_ref(SEXP obj) {
  SEXP class;
  SEXP sref = R_MakeExternalPtr((void*) obj, R_NilValue, R_NilValue);
  SEXP robj = CONS(sref, R_NilValue);
  SET_TAG(robj, install("ref"));
  PROTECT(class = allocVector(STRSXP, 1));
  SET_STRING_ELT(class, 0, mkChar("ObjCid"));
  SET_CLASS(robj, class);
  UNPROTECT(1);
  return robj;
}


/* selectors are kept in a global hash table, so they are never deallocated
 * At any rate, it's not an object, so we cannot retain it */
static SEXP SEL2SEXP(SEL sel)
{
	SEXP class, sref = R_MakeExternalPtr((void*) sel, R_NilValue, R_NilValue);
    SEXP robj = CONS(sref, R_NilValue);
	SET_TAG(robj, install("ref"));
	PROTECT(class = allocVector(STRSXP, 1));
	SET_STRING_ELT(class, 0, mkChar("ObjCsel"));
	SET_CLASS(robj, class);
	UNPROTECT(1);
	return robj;
}

static SEL SEXP2SEL(SEXP ref)
{
	if (!inherits(ref, "ObjCsel"))
		error("object is not a selector.");
	if (TYPEOF(ref)==LISTSXP)
		ref=CAR(ref);
    if (TYPEOF(ref)!=EXTPTRSXP)
        error("Obj-C selector must be an external pointer.");
    return (SEL) R_ExternalPtrAddr(ref);
}

/* the string can be either STRSXP (then the first CHARSXP is used) or CHARSXP itself */
static SEL selectorFromRString(SEXP s)
{
    const char *c;
    NSString *sname;
    SEL res;
    
    if (TYPEOF(s)!=STRSXP && TYPEOF(s)!=CHARSXP)
        error("Obj-C selector name must be a string.");
    if (TYPEOF(s)!=CHARSXP)
        s = STRING_ELT(s, 0);
    c = CHAR(s);
    /* we use manual allocation to make sure we don't run into some allocation pool problems */
    sname = [[NSString alloc] initWithUTF8String:c];
    res = NSSelectorFromString(sname);
    [sname release];
    return res;
}

#if 0
/* this one is not really used ... */
static Class classFromRString(SEXP s)
{
    const char *c;
    NSString *sname;
    Class res;

    if (TYPEOF(s)!=STRSXP && TYPEOF(s)!=CHARSXP)
        error("Obj-C class name must be a string.");
    if (TYPEOF(s)!=CHARSXP)
        s = STRING_ELT(s, 0);
    c = CHAR(s);
    /* we use manual allocation to make sure we don't run into some allocation pool problems */
    sname = [[NSString alloc] initWithUTF8String:c];
    res = NSClassFromString(sname);
    [sname release];
    return res;
}
#endif

static id classObjectFromRString(SEXP s)
{
    if (TYPEOF(s)!=STRSXP && TYPEOF(s)!=CHARSXP)
        error("Obj-C class name must be a string.");
    if (TYPEOF(s)!=CHARSXP)
        s = STRING_ELT(s, 0);
	
    return objc_getClass(CHAR(s));
}

/*====== .External functions - those are called from the R code - */

SEXP ObjCclass(SEXP par)
{
	id obj;
	SEXP arg;
	
	par = CDR(par);
	arg = CAR(par);
	par = CDR(par);
	obj = classObjectFromRString(arg);
	if (!obj) return R_NilValue;
	return ObjCclass2SEXP(obj);
}

SEXP ObjCselector(SEXP par)
{
	SEXP arg = CADR(par);
	SEL s = selectorFromRString(arg);
	if (!s) return R_NilValue;
	return SEL2SEXP(s);
}

#define rvCast(type) (((type *)rv)[0])
#define rvNext(type) rvPtr[0] += sizeof(type)
#define rvNextN(type,N) rvPtr[0] += (N * sizeof(type))

static SEXP parseReturnValue(char **rvPtr, const char **mrtPtr)
{
	char *rv = rvPtr[0];
	const char *mrt = mrtPtr[0];
	Dprintf(" ptr = %p, mrt = '%s'\n", rvPtr[0], mrt);
	/* modifiers: r = const, [nNoORV] - method-related */
	while (*mrt == 'r') mrt++;
	mrtPtr[0] = mrt + 1; /* the default is to process 1 char. setting it here ensures that we don't get infinite loops */
	switch (*mrt) {
		case '#': /* class */
			rvNext(id);
			return ObjCclass2SEXP(rvCast(id));
		case '@': /* id */
			rvNext(id);			
			return ObjC2SEXP(rvCast(id));
		case ':': /* SEL */
		{
			SEL s = rvCast(SEL);
			rvNext(SEL);
			return SEL2SEXP(s);
		}
		case '{': /* structure */
		{
		    char sName[64]; /* structure name */
		    char *cc = sName;
			mrt++;
		    while (*mrt && *mrt != '=') *(cc++) = *(mrt++);
		    *cc = 0;
			mrt++; /* skip = */
			SEXP head = R_NilValue, tail = R_NilValue;
			Dprintf(" - parsing structure: '%s':\n", sName);
			while (*mrt && *mrt != '}') {
				mrtPtr[0] = mrt;
				SEXP elt = parseReturnValue(rvPtr, mrtPtr);
				SEXP link = CONS(elt, R_NilValue);
				if (head == R_NilValue)
					head = tail = link;
				else {
					SETCDR(tail, link);
					tail = link;
				}
				mrt = mrtPtr[0];
			}
			if (*mrt == '}') mrt++;
			Dprintf("   <end of structure, mrt='%s'>\n", mrt);
			mrtPtr[0] = mrt;
			setAttrib(head, install("struct.name"), mkString(sName));
			return head;
		}
			/* FIXME: we could try to convert some known ones ... */
		case 'l': /* long */
		case 'L': /* unsigned long */
		case 'q': /* long long */
		case 'Q': /* unsigned long long */
		case '*': /* char * */
		case '(': /* union */
		case 'b': /* bit field (bnnn, nnn=number of bits) */
		case '^': /* pointer to type (^type) */
		case '?': /* unknown */
			warning("Note: ObjC return type '%s' is not implemented yet.\n", mrt);
		case 'v': /* void */
			return R_NilValue; /* UNIMPLEMENTED */
		case 'i': /* signed int */
		case 'I': /* unsigned int */	
			rvNext(int);
			return ScalarInteger(rvCast(int));
		case 's': /* short */
			rvNext(short);
			return ScalarInteger((int)rvCast(short int));
		case 'S': /* unsigned short */
			rvNext(short);
			return ScalarInteger((int)rvCast(unsigned short));
		case 'f': /* float */
			rvNext(float);
			return ScalarReal((double)rvCast(float));
		case 'd': /* double */
			rvNext(double);
			return ScalarReal(rvCast(double));
		case 'c': /* char */
			rvNext(char);
			return ScalarInteger((int)rvCast(char));
		case 'C': /* char */
			rvNext(char);
			return ScalarInteger((int)rvCast(unsigned char));
		case '[':
		{
			const char *c = mrt+1;
			while (*c>='0' && *c<='9') c++;
			if (*c && c[1]==']') { /* 1d array */
				char szb[32];
				long n = 0;
				mrtPtr[0] = c + 2;
				memcpy(szb, mrt+1, c-mrt-1);
				szb[c-mrt-1]=0;
				n = atol(szb);
				switch (*c) {
					case 'i':
					case 'I':
					{
						SEXP r = allocVector(INTSXP, n);
						rvNextN(int, n);
						memcpy(INTEGER(r), rv, sizeof(int) * n);
						return r;
					}
					case 'f':
					{
						SEXP r = allocVector(REALSXP, n);
						if (n > 0) {
							float *f = (float*) rv;
							rvNextN(float, n);
							int i = 0;
							while (i < n) {
								REAL(r)[i] = f[i];
								i++;
							}
						}
						return r;
					}							
					case 'd':
					{
						SEXP r = allocVector(REALSXP, n);
						rvNextN(double, n);
						return r;
					}
					default:
						warning("Note: ObjC array return type '%s' is not implemented yet.\n", mrt);
				}
			} else
				warning("Note: ObjC multi-dimensional return type '%s' is not implemented yet.\n", mrt);
			break;
		}				   
	}
	return R_NilValue;
}

#define retArg(type, value) { atPtr[0] = at + 1; ((type*)parval)[0] = (type) value; return sizeof(type); }

int convertArgument(SEXP arg, const char **atPtr, void *parval, int *pCount, void **tmp) 
{
	BOOL isConst = NO;
	const char *at = *atPtr;
	if (*at == 'r') { at++; isConst = YES; }
	switch (*at) {
		case 'i':
		case 'I':
			retArg(int, asInteger(arg));
		case 's':
		case 'S':
			retArg(short, asInteger(arg));
		case 'l':
		case 'L':
			retArg(long, asInteger(arg));
		case 'd':
			retArg(double, asReal(arg));
		case 'f':
			retArg(float, asReal(arg));
		case 'B':
			retArg(BOOL, asLogical(arg));
		case ':':
		{
			if (TYPEOF(arg) == STRSXP)
				retArg(SEL, selectorFromRString(arg))
			error("invalid selector");
		}
		case '#':
		{
			if (TYPEOF(arg) == STRSXP)
				retArg(id, classObjectFromRString(arg))
			error("invalid class");
		}
		case '@':
		{
			if (isString(arg)) {
				if (LENGTH(arg) < 1) retArg(id, @"") else
					retArg(id, [NSString stringWithUTF8String:CHAR(STRING_ELT(arg, 0))]);
			} else {
				if (TYPEOF(arg) == LISTSXP) {
					if (TYPEOF(CAR(arg))!=EXTPTRSXP)
						error("invalid object (ObjC object expected)");
					arg = CAR(arg);
				}
				if (TYPEOF(arg) == EXTPTRSXP) {
					retArg(id, SEXP2ObjC(arg));
					//} else if (TYPEOF(arg) == S4SXP) {
				} else error("invalid object (ObjC object expected)");
			}
		}
		case '*':
		{
			// FIXME: we should allow RAWSXP as char *
			if (!isString(arg) || LENGTH(arg) < 1)
				error("char * argument must be a string");
			if (!isConst)
				error("strings can only be passed as (const char *) arguments");
			retArg(char*, CHAR(STRING_ELT(arg,0)));
		}
		case '[':
		{
			Dprintf("array type '%s'\n", at);
			error("I'm sorry, array types are not yet supported as arguments.");
		}
		case '{':
		{
			int struct_len = 0;
			if (TYPEOF(arg) != LISTSXP)
				error("structures must be made of pairlists (for now)");
			while (*at && *at != '=') at++; /* skip structure name */
			if (*at == '=') at++;
			int i = 0;
			while (*at != '}') {
				Dprintf("parsing struct element %d: '%s' (parval=%p)\n", at, parval);
				atPtr[0] = at;
				int len = convertArgument(CAR(arg), atPtr, parval, pCount, tmp);
				// FIXME: check tmp and potential overflow
				arg = CDR(arg);
				parval = ((char*)parval) + len;
				at = atPtr[0];
				struct_len += len;
				if (*at == ',') at++;
			}
			Dprintf("end of structure, len = %d (parval=%p)\n", struct_len, parval);
			atPtr[0] = at;
			return struct_len;
			/* Dprintf("structure type '%s'\n", at);
			 error("I'm sorry, strcture types are not yet supported as arguments."); */
		}
		case '(':
		{
			Dprintf("union type '%s'\n", at);
			error("I'm sorry, union types are not yet supported as arguments.");
		}
		case '^':
		{
			Dprintf("pointer type '%s'\n", at);
			error("I'm sorry, pointers are not yet supported as arguments.");
		}
		case '?':
			if (TYPEOF(arg) != EXTPTRSXP)
				error("only external pointers can be used as functional arguments");
			retArg(void*, EXTPTR_PTR(arg));
		default:
			error("unsupported argument type '%c'", *at);
	}
	error("unknown argument type '%c'", *at);
}

SEXP ObjCsendMsg(SEXP par)
{
    int argPos=2; /* arg[0] = self, arg[1] = _cmd (SEL) */
    id obj;
    NSInvocation *invocation;
    NSMethodSignature *ms;
    SEL sel;
	const char *mrt;
	char staticRetBuf[128]; /* static return buffer */
	char *retBuf = (char*)staticRetBuf; /* return buffer - either static or dynamically allocated */
    SEXP x, target;
    par = CDR(par);
	target = CAR(par);
	par = CDR(par);
	if (TYPEOF(target)==LISTSXP) target=CAR(target);
    if (TYPEOF(target)!=EXTPTRSXP && TYPEOF(target)!=CHARSXP && (TYPEOF(target)!=STRSXP || LENGTH(target)<1))
        error("target must be Obj-C object reference or a class name.");
    if (TYPEOF(target)!=EXTPTRSXP)
        obj = classObjectFromRString(target);
    else
        obj = SEXP2ObjC(target);

    if (!obj)
        error("cannot find target class or object.");
    
    /* NSLog(@" object = %@", obj); */

    x = CAR(par); par = CDR(par);
    if (TYPEOF(x)!=STRSXP || LENGTH(x)<1)
        error("selector must be a string.");

    sel = selectorFromRString(x);
    /* NSLog(@" selector (%@) addr = %x", NSStringFromSelector(sel), sel); */
    
    ms = [obj methodSignatureForSelector: sel];
    if (!ms)
		error("cannot find method signature for the selector");
#ifdef DEBUG
    { 
		Dprintf("%s", [[NSString stringWithFormat:@" (\"%s\") \"%@\" args[%d]: ", [ms methodReturnType], NSStringFromSelector(sel), [ms numberOfArguments]] UTF8String]);
		int q = 0, qq = [ms numberOfArguments]; while (q < qq) Dprintf("\"%s\" ", [ms getArgumentTypeAtIndex:q++]);
		Dprintf("\n");
	}
#endif
    invocation = [NSInvocation invocationWithMethodSignature: ms];
    [invocation setSelector: sel];
    [invocation setTarget: obj];
    while (par && (TYPEOF(par)!=NILSXP)) {
		int pCounter = 0;
		void *tmp=0;
        SEXP arg = CAR(par);
        char parval[256];
		void *parptr = &parval;
		
		const char *at = [ms getArgumentTypeAtIndex: argPos];
		const char **atp = &at;
		convertArgument(arg, atp, &parval, &pCounter, &tmp);
		if (tmp) parptr = tmp;
		par = CDR(par);
		Dprintf(" arg %d, type '%s': parptr=%p, parval(%p)=%x\n", argPos, at, parptr, &parval, ((int*)parval)[0]);
	/* be careful - the NSLog below will crash on everything but objects
	   NSLog(@" parameter %d : %@", argPos, parval); */
		/* Dprintf("%s\n", [[NSString stringWithFormat: @" parameter %d : 0x%p (%@)", argPos, parptr, parval?parval:@"<null>"] UTF8String]); */
        [invocation setArgument: parptr atIndex: argPos];
		if (pCounter) UNPROTECT(pCounter);
        argPos++;
		if (tmp)
			free(tmp);
    }
	{
		BOOL isOk=YES;
		NS_DURING
			[invocation invoke];
		NS_HANDLER
			NSLog(@"Obj-C exception raised: %@", localException);
			isOk=NO;
		NS_ENDHANDLER
		if (!isOk)
			error("Obj-C exception was raised");
	}
	mrt=[ms methodReturnType];
	unsigned int buf_size = [ms methodReturnLength];
	Dprintf(" method return type='%s', length=%d\n", mrt, buf_size);
	if (buf_size > 128)
		retBuf = (char*) malloc(buf_size);
	[invocation getReturnValue: retBuf];
	{
		char *vrb = retBuf; /* we need a copy such that the original value can be freed */
		SEXP res = parseReturnValue(&vrb, &mrt);
		if (retBuf != staticRetBuf)
			free(retBuf);
		return res;
	}
}

SEXP printObjC(SEXP par)
{
	SEXP arg;
	id obj;
	NSString *s;
	par=CDR(par);
	arg=CAR(par);
	if (inherits(arg,"ObjCsel")) {
		SEL sec = SEXP2SEL(arg);
		if (!sec) return R_NilValue;
		s=NSStringFromSelector(sec);
	} else {
		obj=SEXP2ObjC(arg);
		if (!obj) return R_NilValue;
		s=[obj description];
	}
	if (!s) return R_NilValue;
	PROTECT(arg=allocVector(STRSXP,1));
	SET_STRING_ELT(arg, 0, mkChar([s UTF8String]));
	UNPROTECT(1);
	return arg;
}
