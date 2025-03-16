ReadFileFuns = c("readLines", "scan", "readline",
    "dget", "readRDS", ".readRDS", "loadNamespace",
    "read.dcf", "readRenviron",
    "readBin", "readChar",
    "sys.source",
    "list.files", "dir", "file.info", "file.size", "file.mtime", "file.mode",
    "file.copy", "file.symlink", "file.exists", "file.remove", "file.append", "file.link",
      # utils package
    "read.csv", "read.table", "read.csv2", "read.delim", "read.delim2", "read.fwf", 
    "read.DIF", "unzip", "tar", "untar2",
    "file_test",
      # readxl
    "read_excel",
      # XML
    "htmlParse", "xmlParse"
    )

# Doesn't handle connections file(), etc. and open()

findPkgReadFuns =
function(pkg, recursive = TRUE, readFuns = ReadFileFuns)
{
    #XXX gets the non-exported versions also.
  if(is.character(pkg) && length(pkg) > 1)
      pkg = getNamespace(pkg)
  
  funs = as.list(pkg)
  funs = funs[sapply(funs, is.function)]
  globs = sapply(funs, function(f) getGlobals(f)$functions)

  # Could fold these into the loop, but written before the recursive version.
  i = lapply(globs, intersect, readFuns)
  ans = names(i)[sapply(i, length) > 0]
  
  if(recursive) {
     ans = c(ans, readFuns)
     while(TRUE) {
         o = setdiff(names(funs), ans)

         if(length(o) == 0)
             break
         i = lapply(globs[o], intersect, ans)
         w = sapply(i, length) > 0
         if(!any(w))
             break
         ans = c(names(i)[w], ans)
     }
  }
  setdiff(ans, readFuns)
}

getFilesRead =
function(fun, ..., readFileFuns = ReadFileFuns, recursive = FALSE)
    findCallsTo(fun, readFileFuns) 



