#include <Cocoa/Cocoa.h>

NSAutoreleasePool * pool = NULL;

void InitializeCocoa()
{
    //need a auto release pool for allocating objects.
    pool = [[NSAutoreleasePool alloc] init];

    /* Gaining access to the shared application.  This is a very important step because it initializes
     * the Cocoa envionment.  This is required to do things like put up Cocoa alert dialogs.  
     */
    [NSApplication sharedApplication];
}

void DeInitializeCocoa()
{
    //deallocate the autorelease pool if neccessary.
    if (pool != NULL)
        {[pool release];}
}
