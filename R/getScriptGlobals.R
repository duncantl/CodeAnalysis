getScriptGlobals =
function(file, ...)
{
    e = parse(file)
    f = function() {}
    body(f)[seq(along = e) + 1] = e
    getGlobals(f, ...)
}
