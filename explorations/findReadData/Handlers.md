
 This illustrates an idiom I sometimes use.
 The issue is how to allow extensibility and customizability in a structured manner.
 This is a mix of 1) S3 classes and methods, and 2) an adaptible list of handler functions
  specified for a given call.
 The programmer can provide either a function in the call (2) or define a method for a function
 for a given class and have a generic function look for that.
 The generic function first looks for a matching handler in the call, and if it doesn't find it
 uses the S3 method dispatch to look for the corresponding method in the search path.

  This allows a programmer to provide a customized function for a specific call
  or to define/declare a default way to behave for all calls for a given class of object.
  The former can override the latter for a given call. 

 The situation for this example/case study is that we are using rstatic AST Call objects
 These are calls to functions that read data. We want to modify these so that they don't read
 an entire file/input source, but instead read a chunk of the data as we are changing the
 strategy from one single call to read data to multiple workers read (and processing) subsets

  So for example, we may modify the call
   read.table("file")
  to
   read.table("file", skip = numLinesToStart, n = numLinesInChunk)

 However, we might change
   d = read.fwf("file", widths = c(5, 10, 4, 8))
 to
   con = file("file", "w")
   seek(con, numBytes)
   d = read.fwf(con, widths = c(5, 10, 4, 8), n = numLinesInChunk)
   close(con); rm(con)
 

 We don't want to have to call a different function to handle each Call
 based on the name of the R read-data function in the Call, i.e. one for read.fwf, another for
 read.table, another for read_excel, etc.
 Instead, we want a single rewrite function, say named foo().

 We want the caller of foo() to be able to call that function and have foo() find the apprporiate method for
 rewriting the Call.
  We could do this with an S3 method for the class Call, i.e. define a function named foo.Call
 This is very "general" as it would apply to all Call objects.  We would have to have it look
 at the name of the function being called, e.g. read.fwf or read.table, and then conditionally
 determine what to do. This makes that foo.Call() function very large and complicated to deal
 with the many different functions it should know how to deal with in the call. More importantly,
 other users cannot add support to it for a new function, e.g. read_excel().

Instead, we might make foo.Call() take a named-list of functions that act as a per-call method
 dispatch. foo.Call() looks for an element in the handler list with the same name as
 the function name in the Call object, e.g. read.fwf.
  + If there is such an element in the handler list it calls that and returns the result.
  + If there is no corresponding element in the list, it performs the default operation.
  
However, we want users/programmers to also be able to provide a method for a particular function call.
We could have them define foo.read.fwf and dispatch to this.
This allows programmers to declare a "default" behaviour for foo() for Call's to this function.
Users do not have to specify the handler function in every call to foo(). Instead, it is found
in a "global" location. However, they can override this method for a particular call to foo() by
providing a handler. This avoids having to overwrite the, e.g., foo.read.fwf globally before a call
to foo(), and then restore it after the call completes. This would be the downside of using S3 methods
alone as they are essentially using global variables.

So we can define foo.default to be a regular generic, i.e. a call to UseMethod()
 ```
foo = 
function(x, ...) 
  UseMethod("foo")
 ```
 
Next, we can define foo.Call, a method for Call objects, to be 
```
foo.Call =
function(obj, handlers = getReadFunHandlers(...), ...)
{
   fname = getReadFunName(obj)
   m <- match(fname, names(handlers))
   if(!is.na(m))
       handlers[[m]](obj, handlers)
   else {
       class(obj) = c(fname, class(obj))
       foo(obj, handlers = list()) # UseMethod("foo") will pass the handlers again.
	                               # But either way, infinite recursion.
   }
}
```
This has problems, but let's take it as is for now.

We define getReadFunHandlers() to combine user-supplied named-handler functions
with the default handlers stored in a global variable.
This allows the caller to reuse the default values, but to override one or more
with their own handler functions and to provide handlers for other read-data functions
not already in the default handlers.
The function looks like
```
getReadFunHandlers =
function(..., .defaultHandlers = ReadFunHandlers, .tmp = list(...))
{
    if(length(.tmp))
        .defaultHandlers[names(.tmp)] = .tmp

    .defaultHandlers
}
```

This uses the global variable ReadFunHandlers and we define this to
be a "library" of helper functions.
```
ReadFunHandlers =
    list(read.csv = function(e, ...){ message("read.csv"); e},
         readLines = function(e, ...){ message("readLines"); e},
         read.fwf = function(e, ...){ message("read.fwf"); e}
         )    
```
We could also decide to leave this empty and implement these as methods, e.g. foo.read.csv,
foo.readLines. We'll return to this.


Now, consider a call to read.csv() and we pass that to foo()
```
foo(quote_ast(read.csv("file")))
```
The generic foo() dispatches this to foo.Call since the AST object is of class Call.
foo.Call() looks in the handlers for an element named read.csv. It finds it in the handlers
and so calls that function.

Consider a call to to read.table().
```
foo(quote_ast(read.table("file")))
```
This would actually end up in infinite recursion. We'll return to this.

Suppose we provide a handler for read.table in the call to foo()
```
foo(quote_ast(read.table("file")), read.table = function(x, ...){ message("In handler for read.table"); x})
```
Again, the dispatch will call foo.Call() and this will find the read.table handler in the list and
invoke that.

Now consider we define a method foo.read.table with the same function we specified as handler
function:
```
foo.read.table = 
function(x, ...) {
  message("In handler method for read.table")
  x
}
```
(We changed the message to indicate this was a method and not a regular handler function.)
Now
```
foo(quote_ast(read.table("file")))
```
works as expected and does not end up in an infinitely recursive call sequence.



The infinite recursion is caused in foo.Call() when
+ there is no handler function
+ we augment the class of the Call object to prepend the name of the function being called in the
  Call object 
+ and we call foo() or UseMethod("foo") to dispatch on this newly classed object.
If there is a method, e.g. foo.read.table, then all is well.
However, if there is no method defined for that class, then foo() or UseMethod() dispatches
again  to foo.Call(). Hence we get the same call over and over.

We could change the class to drop the "Call". So we would change this to, e.g.,
`c("read.table", "ASTNode", "R6")` via 
```
class(obj) = c(fname, class(obj)[-1])
```
However, then we lose the ability to work with the resulting object as a Call object
in the methods/handlers. So this is not ideal.


We could implement our own simple dispatch by looking for a function named
`paste(foo, fnmae, sep = ".")`, e.g. foo.read.table.
If this exists, we call it; if not, we perform the default operation on a Call.
We would implement this with
```
foo.Call =
function(obj, handlers = getReadFunHandlers(...), ...)
{
   fname = getReadFunName(obj)
   m <- match(fname, names(handlers))
   if(!is.na(m))
       handlers[[m]](obj, handlers)
   else {
       mname = sprintf("foo.%s", fname)
       w = find(mname, mode = "function")
	   if(length(w))
           (get(mname, w))(obj, ...)
 	   else
         obj # or call foo.default or NextMethod("foo")
   }
}
```

The call to find() may not be ideally what we want. This looks along the search path.
If our code is in a package, find() will not look in the package's namespace first.

In our implementation above, we just returned the unaltered Call object.
We could call `foo.default`, or better,  `NextMethod("foo")` to continue the dispatch.

As an alternative to our foo.Call() and infinite recursion, we could implement
the handlers in the generic function for foo itself, e.g.,
```
foo =
function(obj, handlers = getReadFunHandlers(...), ...)
{
    if(is(obj, "Call")) {
     fname = getReadFunName(obj)
     m <- match(fname, names(handlers))
     if(!is.na(m))
        return(handlers[[m]](obj, handlers))
    }

    class(obj) = c(fname, class(obj))
	UseMethod("foo")  #  foo(obj, handlers = list(), ...)
}
```
This avoids the infinite recursion as we only dispatch once and
will either match, e.g. foo.read.fwf or foo.Call.
This mixes an operation only meant for Call objects with the generic which is not ideal.
We also either don't define  foo.Call() or have it perform a default operation appropriate for any
Call object.


# An Example of a Method that Modifies the Call

To provide an actual working customized version for read.fwf, we can do so with a method
that adds a skip and n argument to the call to skip over some lines in the file
and read only a specified number of rows.
We might do this with
``` 
 foo.read.fwf =
 function(obj, handlers = getReadFunHandlers(...), ...)
 {
    obj$args$skip = quote(numLinesToSkip)	
    obj$args$n = quote(numLinesInChunk)
 }
```
where numLinesToSkip and numLinesInChunk are determined somewhere and provided in the context of the
call as possibly local variables on each parallel worker.

