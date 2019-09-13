#

getReadFunHandlers =
function(..., .defaultHandlers = ReadFunHandlers, .tmp = list(...))
{
    if(length(.tmp))
        .defaultHandlers[names(.tmp)] = .tmp

    .defaultHandlers
 
}

ReadFunHandlers =
    list(read.csv = function(e, ...){ cat("read.csv\n"); e},
         readLines = function(e, ...){cat("readLines\n"); e},
         read.fwf = function(e, ...){cat("read.fwf\n"); e}
         )    




foo =
function(obj, handlers = getReadFunHandlers(...), ...)
{
    fname = getReadFunName(obj)
    m <- match(fname, names(handlers))
    if(!is.na(m))
        handlers[[m]](obj, handlers)
    else {
        class(obj) = c(fname, class(obj))
        UseMethod("foo")
    }
}

if(FALSE) {
expr = to_ast(quote(read.fwf("file")))
foo(expr)

expr = to_ast(quote(read.table("file")))
foo(expr) # error - no method.

foo.default =
    function(x, ...) {
        cat("foo.default\n")
        x
    }

foo(expr) # uses foo.default


foo.read.table =
    function(x, ...) {
        cat("foo.read.table\n")
        x
    }

foo(expr)


foo(expr, read.table = function(e, ...){cat("anonymous/1-time read.table handler\n"); e})


getReadFunHandlers(read.fwf=function(e, ...){ # something different
})

h = getReadFunHandlers(read_excel = function(e, ...) {},
                   read.fwf=function(e, ...){ # something different
               })

foo(, h)
foo(, h)

foo.read_excel  = function(e, ...) { ###
   }
}


