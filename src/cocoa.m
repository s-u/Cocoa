#include <Cocoa/Cocoa.h>
#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>

NSAutoreleasePool * pool = NULL;

void InitializeCocoa()
{
    // need a auto release pool for allocating objects
	
	// BUT inside R.app it's very dangerous to create a new pool, because it could be
	// destroyed whenever R.app releases its pools. This is normally ok, but since the
	// use is likely to relay on the persistence of objects as ruled by R it's much safer
	// to let it be and let R.app manage the pools
	if (!objc_getClass("RController"))
		pool = [[NSAutoreleasePool alloc] init];

    /* Gaining access to the shared application.  This is a very important step because it initializes
     * the Cocoa envionment.  This is required to do things like put up Cocoa alert dialogs.  
     */
    [NSApplication sharedApplication];
}

void DeInitializeCocoa()
{
    //deallocate the autorelease pool if neccessary.
    if (pool)
        [pool release];
}

// send release msg to the corresponding Obj-C object as soon as it's no longer needed in R
void idObjCfinalizer(SEXP obj)
{
	if (TYPEOF(obj)==LISTSXP)
		obj=CAR(obj);
    if (TYPEOF(obj)==EXTPTRSXP) {
        id o = R_ExternalPtrAddr(obj);
        if (o) [o release];
    }
}

// convert Obj-C object into Obj-C reference in R - note that the object automatically receives a retain message
SEXP ObjC2SEXP(id obj)
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
    R_RegisterCFinalizer(sref, idObjCfinalizer); // should we use Ex and onExit=TRUE?
    return robj;
}

SEXP ObjCclass2SEXP(id obj)
{ // we make a reference, but we don't retain it and don't register a finalizer
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

// convert Obj-C reference in R into Obj-C object
id SEXP2ObjC(SEXP ref)
{
	if (TYPEOF(ref)==LISTSXP)
		ref=CAR(ref);
    if (TYPEOF(ref)!=EXTPTRSXP)
        error("Obj-C reference must be an external pointer.");
    return (id) R_ExternalPtrAddr(ref);
}

// selectors are kept in a global hash table, so they are never deallocated
// At any rate, it's not an object, so we cannot retain it
SEXP SEL2SEXP(SEL sel)
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

SEL SEXP2SEL(SEXP ref)
{
	if (!inherits(ref, "ObjCsel"))
		error("object is not a selector.");
	if (TYPEOF(ref)==LISTSXP)
		ref=CAR(ref);
    if (TYPEOF(ref)!=EXTPTRSXP)
        error("Obj-C selector must be an external pointer.");
    return (SEL) R_ExternalPtrAddr(ref);
}

// the string can be either STRSXP (then the first CHARSXP is used) or CHARSXP itself
SEL selectorFromRString(SEXP s)
{
    char *c;
    NSString *sname;
    SEL res;
    
    if (TYPEOF(s)!=STRSXP && TYPEOF(s)!=CHARSXP)
        error("Obj-C selector name must be a string.");
    if (TYPEOF(s)!=CHARSXP)
        s = STRING_ELT(s, 0);
    c = CHAR(s);
    // we use manual allocation to make sure we don't run into some allocation pool problems
    sname = [[NSString alloc] initWithCString:c];
    res = NSSelectorFromString(sname);
    [sname release];
    return res;
}

// this one is not really used ...
Class classFromRString(SEXP s)
{
    char *c;
    NSString *sname;
    Class res;

    if (TYPEOF(s)!=STRSXP && TYPEOF(s)!=CHARSXP)
        error("Obj-C class name must be a string.");
    if (TYPEOF(s)!=CHARSXP)
        s = STRING_ELT(s, 0);
    c = CHAR(s);
    // we use manual allocation to make sure we don't run into some allocation pool problems
    sname = [[NSString alloc] initWithCString:c];
    res = NSClassFromString(sname);
    [sname release];
    return res;
}

id classObjectFromRString(SEXP s)
{
    char *c;
    NSString *sname;
    id res;

    if (TYPEOF(s)!=STRSXP && TYPEOF(s)!=CHARSXP)
        error("Obj-C class name must be a string.");
    if (TYPEOF(s)!=CHARSXP)
        s = STRING_ELT(s, 0);
	
    return objc_getClass(CHAR(s));
}

//====== .External functions - those are called from the R code -

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
    
    //NSLog(@" object = %@", obj);

    x = CAR(par); par = CDR(par);
    if (TYPEOF(x)!=STRSXP || LENGTH(x)<1)
        error("selector must be a string.");

    sel = selectorFromRString(x);
    //NSLog(@" selector (%@) addr = %x", NSStringFromSelector(sel), sel);
    
	NSMethodSignature *ms = [obj methodSignatureForSelector: sel];
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
		// be careful - the NSLog below will crash on everything but objects
        //NSLog(@" parameter %d : %@", argPos, parval);
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
    NSLog(@" returned (type %s).", mrt);
	switch (*mrt) {
		case '#':
			[invocation getReturnValue: &retObject];
			return ObjCclass2SEXP(retObject);
		case '@':
			[invocation getReturnValue: &retObject];
			return ObjC2SEXP(retObject);
		case 'c':
			if (!mrt[1]) {
				BOOL ba;
				SEXP ro;
				[invocation getReturnValue: &ba];
				PROTECT(ro = allocVector(LGLSXP, 1));
				LOGICAL(ro)[0] = ba;
				UNPROTECT(1);
				return ro;
			}
			break;
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
