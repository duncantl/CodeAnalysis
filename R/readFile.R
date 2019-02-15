ReadFileFuns = c("read.table", "read.csv", "read_excel", "readLines", "list.files", "dir", "file.info")

getFilesRead =
function(fun, ..., recursive = FALSE)
{
    fun = to_ast(fun)
    idx = find_nodes(fun, isReadFileCall, ...)
    if(length(idx))
        sapply(idx, function(i) getFileRead(fun[[i]]))
}

isReadFileCall =
function(node, readFileFuns = ReadFileFuns)
{
    is(node, "Call") && is(node$fn, "Symbol") && node$fn$name %in% readFileFuns
}

getFileRead =
function(node)
{
    f = node$args[[1]]
    if(is(f, "Character"))
        f$value
    else
        character() # FIX
}
