#' Change the levels of a factor
#' @export
change.factor.levels <-
function (input.factor, old.list, new.list)
{
    for (index in seq_along(old.list)) {
        levels(input.factor)[levels(input.factor) == old.list[index]] <- new.list[index]
    }
    return(input.factor)
}
