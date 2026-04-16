`%$%` <- magrittr::`%$%`
`%<>%` <- magrittr::`%<>%`
`%>%` <- magrittr::`%>%`
set_rownames <- magrittr::set_rownames
`%within%` <- rutabaga::`%within%`
`%&%` <- rutabaga::`%&%`
`%OF%` <- dipsaus::`%OF%`
`%?<-%` <- dipsaus::`%?<-%`
str_collapse <- rutabaga::str_collapse
which.equal <- rutabaga::which.equal
fast_median <- ravetools::fast_median
rbind_list <- rutabaga::rbind_list
cbind_list <- rutabaga::cbind_list
m_se <- rutabaga::m_se
se <- rutabaga:::se
plus_minus <- rutabaga::plus_minus
rand_string <- raveio:::rand_string
stopifnot2 <- raveio:::stopifnot2

# require(data.table)
.datatable.aware = TRUE




closest_to_zero <- function(v) {
  v[which.min(abs(v))]
}


get_recursive_summary <- function(ll, nm, FUN=range) {
  if(is.null(ll[[nm]])) {
    return(FUN(unlist(lapply(ll, get_recursive_summary, nm=nm))))
  }

  FUN(ll[[nm]])
}
