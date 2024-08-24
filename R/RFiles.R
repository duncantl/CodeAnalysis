# Functions to list the files in a directory
# or a collection of files and directories
# and then to source those functions, including
# intercepting the S4 functions that could try to change
# a (locked) environment.

computeRFiles =
function(obj, recursive = TRUE, ...)
{
    isdir = file.info(obj)$isdir
    files = obj[!isdir]
    if(any(isdir))
        files = c(files, unlist( lapply(obj[isdir], getRFiles, ..., recursive = recursive)))

    files
}

getRFiles =
function(dir, pattern = '\\.[RrSsQq]$', recursive = TRUE)
   list.files(dir, pattern, full.names = TRUE, recursive = recursive)

sourceRFiles =
function(obj, files = computeRFiles(obj), e = mkS4Catcher())
{
    lapply(files, source, e)
    as.list.environment(e, TRUE)
}

mkS4Catcher =
    #
    # intercepts calls to setGeneric, setMethod, setAs and also setClass and setOldClass
    # assigns the actual function being defined into  the target environment
    # but with a different name.
    #
function(e = new.env(parent = p), p = new.env())
{
    addFun = function(id, fun) {
        if(exists(id, e, inherits = FALSE)) 
            warning("overwriting ", id)

        assign(id, fun, e)
    }
    
    p$setGeneric = function(name, def, ...)
                     addFun(name, def)
    p$setMethod = function(f, signature, definition, ...)
                     addFun(sprintf("%s.%s", f, paste(signature, collapse = ".")), definition)
    p$setAs = function(from, to, def)
                    addFun(paste("coerce", from, to, sep = "."),
                           structure(list(from = from, to = to, def = def), class = "SetAs"))

    p$setClass = function(Class, representation, ...) {
            # (Class, structure(list(Class = Class, representation = representation), class = "S4Class")
        }
    p$setOldClass = function(Classes, ...) {
                      # (Classes[1], structure(list(Classes = Classes), class = "S4OldClass"))
                 }    
    e
}
