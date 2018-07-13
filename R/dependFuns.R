getDepends =
function(file, info = as(readScript(file), "ScriptInfo"), fileFunctionNames = FileFunctionNames(),
          prev = info, loadPackages = TRUE, addExpressionNum = TRUE)
{
#  tmp = lapply(info, getDepend, fileFunctionNames = fileFunctionNames)
   tmp = vector("list", length(info))
   for(i in seq(along = info))
       tmp[[i]] = getDepend(info[[i]], fileFunctionNames = fileFunctionNames, prev = if(missing(prev)) info[seq_len(i-1L)] else prev, loadPackages = loadPackages)

  i = !sapply(tmp, is.null)
  if(!any(i)) {
      tmp = data.frame(filename = character(), operation = character(), stringsAsFactors = FALSE)
      if(addExpressionNum)
         tmp$expressionNum = integer()
      return(tmp)
  }

#if(any(sapply(tmp[i], function(x) is.list(x) && !is.data.frame(x)))) browser()
   
  ans = as.data.frame(do.call(rbind, tmp[i]), stringsAsFactors = FALSE)
    # We are losing the expression number within the sub-expressions
  if(addExpressionNum)
     ans$expressionNum = rep(which(i), sapply(tmp[i], function(x) if(is.data.frame(x)) nrow(x) else 1))
  ans
}

#DefaultFileFunctions = c("source", "load", "save", "png", "pdf", "read.csv", "read.table", "read.fwf", "file", "gzfile", "write.csv")
#dput(structure(DefaultFileFunctions, names = unname(sapply(DefaultFileFunctions, function(x) names(formals(get(x, mode = "function")))[1]))))
DefaultFileFunctions =
 structure(c("source", "load", "save", "png", "pdf", "read.csv", 
"read.table", "read.fwf", "file", "gzfile", "write.csv"), .Names = c("file", 
"file", "...", "filename", "file", "file", "file", "file", "description", 
"description", "..."))

# Override some of these.
names(DefaultFileFunctions)[match(c("save","write.csv"), DefaultFileFunctions)] = "file"


FileFunctionNames =
function(..., .funs = unlist(list(...)), .exclude = FALSE)
{
   if(.exclude)
      .funs
   else
      c(DefaultFileFunctions, .funs)
}


getDependsLanguage =
function(code, fileFunctionNames = FileFunctionNames(), prev = list())
{
    i = new("ScriptInfo", lapply(code, getInputs))
    tmp = getDepends(, i, fileFunctionNames, prev = prev, addExpressionNum = FALSE)
    tmp
}

getDepend =
function(node, fileFunctionNames = FileFunctionNames(), funs = names(node@functions), prev = list(), loadPackages = TRUE)
{
      # load any library
   if(loadPackages && length(node@libraries)) 
        sapply(node@libraries, function(x) try(library(x, character.only = TRUE)))

    
   if(any(fileFunctionNames %in% funs)) {   # length(node@strings) && 
      funs = intersect(fileFunctionNames, names(node@functions))
      k = node@code

        # Deal with top-level if statements
      if(class(k) == "if") {
         if(is.logical(k[[2]]) && !k[[2]]) {  #XXX Allow caller to specify this should be processed.
            if(length(k) == 4)
                return(getDependsLanguage(k[[4]], fileFunctionNames, prev = prev))
                       
            return(NULL)
         }

         return(getDependsLanguage(k[[3]], fileFunctionNames, prev = prev))
      }

        # Assignments, deal with the right hand side
      if(class(k) %in% c("<-", "="))
          k = k[[3]]

        # Nested calls.
      if(is.call(k) && !(as.character(k[[1]]) %in% funs)) {
             # Have to find which elements of the call contain the actual function of interest
          return(getDependsLanguage(k[-1], fileFunctionNames, prev = prev))
      }
      
      funName = as.character(k[[1]])

      fun = tryCatch(get(funName, mode = "function"), error = function(...) NULL)
      if(!is.null(fun) && typeof(fun) == "closure") {  # avoud "function" objects which are primitives.
          kall =  match.call(fun, k)
          i = match(funName, fileFunctionNames)
          arg = names(fileFunctionNames)[i]
          if(arg == "")  # no name, so assume first argument (2nd element of call)
                         # We could add other heuristics but probably do that to put a name on it before we start.
              arg = 2
          file = kall[[arg]]
      } else {
          file = k[[2]]
      }

# If the argument is not a string but a call or the name of a variable.      
      if(!is.character(file)) {
         if(is.name(file)) {
            v = getVariableDepends(as.character(file), lapply(prev, slot, "code"), prev, asIndex = TRUE)
            if(length(v)) {
                if(length(v) > 1)
                    warning("not handled properly yet")
                dep = prev[[ v[1] ]]
                if(length(dep@strings) == 1 && length(dep@outputs) == 1)
                    file = dep@strings
                else
                    file = as.character(NA)
            }
         } else if(is.call(file)) {
            # get literals at least.
            tmp = getInputs(file)
            file = tmp@strings
         } else
           file = as.character(NA)
         
     }
#XXX - we have a variable  but we haven't resolved it.
      # This occurs e.g. in 
if(!is.character(file)){
#     browser()
     file = as.character(NA)
 }
      
      c(filename = file, operation = funName)
   }
   else
     NULL
}

