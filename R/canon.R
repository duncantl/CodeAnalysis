# Fri Sep 22 11:26:16 PDT 2017
# Tools to transform R code into a "canonical form"

# Sun Sep 24 14:53:01 PDT 2017
# Not currently worrying about:
# - logical subsetting of columns
# - non unique column names



#' Identify all calls which subset \code{varname} and transform them into a common
#' form
#'
#' @param statement code which may or may not be subsetting the data frame
#' @param varname string containing name of data frame
#' @param colnames optional character vector mapping names to integers
#' @return list with all relevant information
canon_form = function(statement, varname, colnames = NULL)
{
    transformed = statement
    column_indices = integer()
    index_locs = list()

    varlocs = find_var(statement, varname)

    # Early outs
    if(length(varlocs) == 0){
        list(transformed = transformed
             , column_indices = column_indices
             , index_locs = index_locs
             )
    }

    for(varloc in varlocs){
        # If the parent statement is a call to one of the subset funcs then
        # transform it and record the indices.

        # TODO: Think more about:

        # Modifying the code as we go may affect the locations where the
        # variables where found. To ensure correctness may need a breadth
        # or depth first tree traversal.

        # What about code that implicitly uses all columns? Ie:
        # plot(varname)
        # If we see this we should just give up with the code rewriting.

        if(length(varloc) == 1){
            # Variable found in root of parse tree
            # After I've written all this code around it I don't think
            # it's even necessary, since this only happens with a line of
            # the form: x[, 1L]
            # which doesn't do anything in a script. It may be useful
            # later, so I'll leave it all here.
            root = TRUE
            parent = transformed
        } else {
            # Variable found deeper in tree
            root = FALSE
            insertion_point = varloc[-length(varloc)]
            parent = transformed[[insertion_point]]
        }

        funcname = as.character(parent[[1]])
        if(funcname %in% names(subset_funcs)){
            # Put this call into canonical form and update the transformed
            # statement with it.

            # Record the locations inside the parse tree where the
            # column indices are used. The locations are used later to
            # substitute updated column indices once we read in a subset of
            # the columns of the data.

            modified = subset_funcs[[funcname]](parent, colnames)
            if(root){
                transformed = modified$statement
                index_locs = c(index_locs, list(4L))
            } else {
                transformed[[insertion_point]] = modified$statement
                index_locs = c(index_locs, list(c(insertion_point, 4L)))
            }
            column_indices = c(column_indices, modified$column_indices)
        }
    }

    list(transformed = transformed
         , column_indices = sort(unique(column_indices))
         , index_locs = index_locs
         )
}


#' Check for and uniformly handle NULL column names
#'
#' @param colnames character or NULL
check_colnames = function(colnames)
{
    if(is.null(colnames)){
        stop("
The colnames are needed, but cannot be determined.
This may be because the data file is in a different relative directory.
")}
}


#' The following functions perform the work after we discover:
#'
#' 1. The location of the call ie. plot(x$y) should operate on x$y which is
#'   the 2nd element in the parse tree
#' 2. Which function to dispatch to, so we need to pick out `$` above
#'
#' These functions all do the same thing, for each of the special cases.
#' They do the following:
#'
#' 1. Transform the statement to a common form which uses only integer indices and
#'   single square brackets. The common form simplifies subsequent
#'   processing.
#' 2. Record all column indices which are used. This allows us to see which
#'   columns are needed at the end of a script.
#' @name to_ssb
NULL


#' Replace $ with [
#' @rdname to_ssb
dollar_to_ssb = function(statement, colnames)
{
    check_colnames(colnames)
    template = quote(dframe[, index])
    column_name = deparse(statement[[3]])
    column_index = which(colnames == column_name)[1]
    statement = sub_expr(template,
            list(dframe = statement[[2]], index = column_index))
    list(statement = statement, column_indices = column_index)
}


#' Replace [[ with [
#' @rdname to_ssb
double_to_ssb = function(statement, colnames)
{

    template = quote(dframe[, index])

    column = statement[[3]]

    column_index = if(is.numeric(column)){
        if(length(column) > 1) stop("Recursive indexing not currently supported")
        as.integer(column)
    } else if(is.character(column)){
        check_colnames(colnames)
        which(colnames == column)[1]
    } else {
        stop("Expected character or numeric for `[[` indexing")
    }

    statement = sub_expr(template,
            list(dframe = statement[[2]], index = column_index))
    list(statement = statement, column_indices = column_index)
}


#' Replace column subset [ possibly using names with [ using integers
#' @rdname to_ssb
single_to_ssb = function(statement, colnames)
{

    column = statement[[4]]

    if(class(column) == "call"){
        if(only_literals(column)){
            column = eval(column)
        } else {
            stop("Not a literal expression")
        }
    }

    column_index = if(is.numeric(column)){
        as.integer(column)
    } else if(is.character(column)){
        check_colnames(colnames)
        which(colnames %in% column)
    } else {
        stop("Expected character or numeric for `[` indexing")
    }

    statement[[4]] = column_index
    list(statement = statement, column_indices = column_index)
}


subset_funcs = list(`$` = dollar_to_ssb
                    , `[[` = double_to_ssb
                    , `[` = single_to_ssb
                    )
