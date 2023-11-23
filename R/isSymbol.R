isSymbol =
    # analogous to rstatic::is_symbol
    # Helper function
    # Version of this now also in indexWalkCode.
function(x, sym = character())
    is.name(x) && (length(sym) == 0 || as.character(x) %in% sym)

