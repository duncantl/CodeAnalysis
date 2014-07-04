
isDefaultValue =
function(x)
    !( is.name(x) && as.character(x) == "")


substituteDefaultValues =
function(f, sc = new("Script", as.list(body(f))[-1]), info = getInputs(sc))
{
    hasDef = sapply(formals(f), isDefaultValue)
    vnames = names(formals(f))[hasDef]

    w = lapply(vnames,
                function(var)
                    which(sapply(info, function(x) var %in% x@inputs)))
    names(w) = vnames
    vals = formals(f)[hasDef]
    e = mapply(function(id, e)
                  substitute( if(.missing(v)) v <- e, list(v = as.name(id), e = e)),
               vnames, vals)

  # now merge these new expressions into the list of expressions.
    w = unlist(w)
    w = w[order(w, decreasing = TRUE)]

    gr = split(data.frame(id = names(w), w = w), w)

    els = as.list(body(f))[-1]
        # now go backwards so the indices won't be affected.            
    for(i in rev(names(gr))) {
        pos = as.integer(i)
        els = c(els[1:(pos-1L)], e[as.character(gr[[i]]$id)], els[pos:length(els)])
    }


    body(f) = substitute({})
    body(f)[seq(along = els) + 1L] = els
#    body(f) = structure(c("{", els), class = "{")
    
     f
}

