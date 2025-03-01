getScriptGlobals =
function(file, ...)
{
    if(is.language(file))
        e = file
    else
        e = parse(file)
    
    f = function() {}
    body(f)[seq(along = e) + 1] = e
    getGlobals(f, ...)
}
