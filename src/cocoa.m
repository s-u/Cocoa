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
    sname = [[NSString alloc] initWithCString:c];
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
    sname = [[NSString alloc] initWithCString:c];
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

SEXP ObjCsendMsg(SEXP par)
{
    int argPos=2;
    id obj;
    NSInvocation *invocation;
    NSMethodSignature *ms;
    id retObject;
    SEL sel;
	const char *mrt;

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
    invocation = [NSInvocation invocationWithMethodSignature: ms];
    [invocation setSelector: sel];
    [invocation setTarget: obj];
    while (par && (TYPEOF(par)!=NILSXP)) {
		void *tmp=0;
        SEXP arg = CAR(par);
        id parval = nil;
		void *parptr = &parval;
		
        par = CDR(par);
        switch (TYPEOF(arg)) {
            case NILSXP: break;
			case INTSXP:
				parptr = INTEGER(arg);
				break;
			case REALSXP:
				parptr = REAL(arg);
				break;
			case LGLSXP:
			{
				int i=0, l=LENGTH(arg);
				tmp=malloc(l+1);
				while (i<l) {
					((BOOL*)tmp)[i]=(BOOL)(LOGICAL(arg)[i]);
					i++;
				}
				parptr = &tmp;
				break;
			}
            case STRSXP:
                if (LENGTH(arg)==1)
                    parval = [NSString stringWithCString: CHAR(STRING_ELT(arg, 0))];
                else
                    error("only single strings are supported in Obj-C calls.");
                break;
			case LISTSXP:
				if (TYPEOF(CAR(arg))!=EXTPTRSXP)
					error("dotted-pair lists are not supported arguments.");
				arg=CAR(arg);
			case EXTPTRSXP:
				parval = SEXP2ObjC(arg);
				break;
            default:
                error("unsupported parameter type in Obj-C call.");
        }
	/* be careful - the NSLog below will crash on everything but objects
	   NSLog(@" parameter %d : %@", argPos, parval); */
        [invocation setArgument: parptr atIndex: argPos];
        argPos++;
		if (tmp) {
			free(tmp);
			tmp=0;
		}
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
	/* modifiers: r = const, [nNoORV] - method-related */
	switch (*mrt) {
		case '#': /* class */
			[invocation getReturnValue: &retObject];
			return ObjCclass2SEXP(retObject);
		case '@': /* id */
			[invocation getReturnValue: &retObject];
			return ObjC2SEXP(retObject);
		case ':': /* SEL */
		{
			SEL s;
			[invocation getReturnValue: &s];
			return SEL2SEXP(s);
		}
		case '{': /* structure */
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
			Rprintf("Note: ObjC return type '%s' is not implemented yet.\n", mrt);
		case 'v': /* void */
			break; /* UNIMPLEMENTED */
		case 'i': /* signed int */
		case 'I': /* unsigned int */	
		{
			int i;
			[invocation getReturnValue: &i];
			return ScalarInteger(i);
		}
		case 's': /* short */
		{
			unsigned short i;
			[invocation getReturnValue: &i];
			return ScalarInteger((int)i);
		}
		case 'S': /* unsigned short */
		{
			signed short i;
			[invocation getReturnValue: &i];
			return ScalarInteger((int)i);
		}
		case 'f': /* float */
		{
			float f;
			[invocation getReturnValue: &f];
			return ScalarReal((double)f);
		}
		case 'd': /* double */
		{
			float f;
			[invocation getReturnValue: &f];
			return ScalarReal((double)f);
		}		
		case 'c': /* char */
		{
			char c;
			[invocation getReturnValue: &c];
			return ScalarInteger((int)c);
		}
		case 'C': /* char */
		{
			unsigned char c;
			[invocation getReturnValue: &c];
			return ScalarInteger((int)c);
		}
		case '[':
		{
			const char *c = mrt+1;
			while (*c>='0' && *c<='9') c++;
			if (*c && c[1]==']') { /* 1d array */
				char szb[32];
				long n = 0;
				memcpy(szb, mrt+1, c-mrt-1);
				szb[c-mrt-1]=0;
				n = atol(szb);
				switch (*c) {
					case 'i':
					case 'I':
					{
						SEXP r = allocVector(INTSXP, n);
						[invocation getReturnValue: INTEGER(r)];
						return r;
					}
					case 'f':
					{
						SEXP r = allocVector(REALSXP, n);
						if (n>0) {
							int i = 0;
							float *f = (float*) malloc(n * sizeof(float));
							[invocation getReturnValue: f];
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
						[invocation getReturnValue: REAL(r)];
						return r;
					}
				}
			}
			break;
		}
	}
    return R_NilValue;
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
	SET_STRING_ELT(arg, 0, mkChar([s cString]));
	UNPROTECT(1);
	return arg;
}
