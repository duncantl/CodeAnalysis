# Mon Sep 25 12:12:58 PDT 2017
#
# Implementing method described in transpile vignette:
#
# 1. Infer that a data frame `d` is created by a call to `read.csv()`
# 2. Identify all calls which subset `d` and transform them into a common
#    form.
# 4. Find `usedcolumns` the set of all columns which are used
# 5. Transform the `read.csv(...)` call into `data.table::fread(..., select =
#    usedcolumns)`
# 6. Transform the calls which subset `d` into new indices.


# Transform the calls which subset `d` into new indices.
update_indices = function(statement, index_locs, index_map)
{
    # for loops necessary for making incremental changes and avoiding the
    # need to merge.
    for(loc in index_locs){

        # If it's not a literal scalar then we assume here it's something like
        # x[, c(5, 20)] so that the inside can be evaluated.
        # It's preferable to check this assumption in an earlier step
        # rather than here because if the inside cannot be evaluated in a
        # simple way then we don't actually know which columns are being used
        # so this code should never run.

        # It's important to evaluate the original code rather than
        # just substituting, because the meaning could potentially change.
        # I'm thinking of something like seq(1, 20, by = 4)

        original = eval_literal(statement[[loc]])
        converted = sapply(original, function(x) which(x == index_map))
        statement[[loc]] = converted
    }
    statement
}


assigners = c("<-", "=", "assign")
readfuncs = c("read.csv", "read.table")


#' Transform To Faster Reads
#'
#' Reduce run time and memory use by transforming an expression to read only the
#' columns of a data frame that are necessary for the remainder of the
#' expression.
#'
#' @param expression, for example as returned from \code{base::parse}
#' @param varname character (optional) the name of the data frame of interest
#' @param colnames character (optional) column names for \code{varname}
#' @param readfunc character (optional) name of function originally used to
#'  read the data.
#' @return transformed code
#' @export
read_faster = function(expression, varname = NULL, colnames = NULL, readfunc = NULL)
{

    nulls = sapply(list(varname, colnames, readfunc), is.null)

    # Easy out if all are specified
    if(all(!nulls)){
        return(read_faster_work(expression, varname, colnames, readfunc))
    }

    if(any(!nulls)){
        stop("Must specify all or none of varname, colnames, readfunc")
    }

    out = expression
    for(readfunc in readfuncs){
        readlocs = find_var(expression, readfunc)
        for(loc in readlocs){
            depth = length(loc)
            possible_assign = expression[[loc[-c(depth - 1, depth)]]]
            if(as.character(possible_assign[[1]]) %in% assigners){
                varname = possible_assign[[2]]
                colnames = infer_colnames(possible_assign[[3]])
                out = read_faster_work(out, varname, colnames = colnames, readfunc = readfunc)
            }
        }
    }
    out
}


#' Infer Column Names
#'
#' Given an expression such as \code{read.table("data.txt", col.names =
#' c("a", "b", "c")} this returns \code{c("a", "b", "c")}. If \code{header
#' = TRUE} it attempts to look for the file to read the column names.
#'
#' @inheritParams read_faster
#' @return character vector of column names, or NULL if they can't be
#'  determined
infer_colnames = function(expression)
{

    # Easy case when they are passed explicitly to the read call.
    passed = expression[["col.names"]]
    if(!is.null(passed)){
        return(eval_literal(passed))
    }

    # file is the first arg. Haven't fully thought about more general
    # file like objects here.
    filename = eval_literal(expression[[2L]])

    if(file.exists(filename)){
        # Assuming that it accepts and uses the parameter `nrows`
        expression[["nrows"]] = 1L
        dframe = eval_literal(expression)
        return(colnames(dframe))
    }

    # Column names are unknown
    NULL
}


read_faster_work = function(expression, varname, colnames, readfunc)
{
    analyzed = lapply(expression, canon_form, varname = varname, colnames = colnames)

    column_indices = lapply(analyzed, `[[`, "column_indices")
    index_map = sort(unique(do.call(c, column_indices)))

    transformed = lapply(analyzed, `[[`, "transformed")

    index_locs = lapply(analyzed, `[[`, "index_locs")

    output = mapply(update_indices, transformed, index_locs
                 , MoreArgs = list(index_map = index_map))
    output = as.expression(output)

    # TODO:
    # - May want to move some of the following logic into a wrapper
    #   function since some is common across variables.
    # - Other read funcs

    readlocs = find_var(output, readfunc)

    subset_read_inserted = FALSE

    for(loc in readlocs){
        n = length(loc)
        parentloc = loc[-c(n-1, n)]
        parent = output[[parentloc]]
        if(as.character(parent[[1]]) %in% assigners){
            if(parent[[2]] == varname){
                # TODO: Assuming here the assignment statment looks like
                # x = read.csv(...)
                insertion_loc = loc[-n]
                output[[insertion_loc]] = to_fread(output[[insertion_loc]]
                                                   , select = index_map)
                subset_read_inserted = TRUE
                break
            }
        }
    }

    if(!subset_read_inserted) stop("Data reading call didn't change.")

    output
}


# 5. Transform the `read.csv(...)` call into `data.table::fread(..., select =
#    usedcolumns)`
to_fread = function(statement, select, remove_col.names = TRUE)
{
    transformed = statement
    transformed[[1]] = quote(data.table::fread)
    # Sometimes R just makes things too easy! So happy with this:
    transformed[["select"]] = as.integer(select)
    if(remove_col.names && !is.null(transformed[["col.names"]])){
        transformed[["col.names"]] = NULL
    }
    transformed
}
