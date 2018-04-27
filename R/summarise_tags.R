#' Summarise overlapping groups
#'
#' This extends dplyr::summarise to work with groups that overlap
#' @param .tbl data
#' @param .vars unquoted variable names, passed to \link[dplyr]{vars}
#' @param ... new variables to summarise, passed to \link[dplyr]{summarise}
#' @param .removeF defaults to `FALSE`. If `TRUE`, removes all values where output value is `FALSE`
#' @keywords dplyr




#' @export
summarise_tags <- function(.tbl, .vars, ..., .removeF=FALSE) {
  dots <- quos(...)

  summarise_tags_nogroup <- function(.tbl, .vars, ..., .removeF=FALSE) {
    dots <- quos(...)

    .tbl %>%
      transmute(!!!.vars) %>%
      map_dfr(
        ~ summarise(group_by(.tbl, "value" = ., add = TRUE), !!!dots) %>% # piping .tbl %>% group_by() %>% summarise() doesn't work for some reason
          filter_at(vars("value"), all_vars(!(. == FALSE & .removeF))) %>% # to remove rows where logical tag is FALSE. Done here so that factors with label 'FALSE' are not omitted
          mutate_at(vars("value"), as.character) # standardises 'value' column in case map_dfr tries to convert logical to factor
        ,
        .id = "variable"
      )
  }


  .tbl %>%
    nest() %>%
    mutate(
      tags = map(data, ~ summarise_tags_nogroup(., .vars, !!!dots, .removeF = .removeF))
    ) %>%
    unnest(tags) %>%
    purrrlyr::slice_rows(group_vars(.tbl))
}



#' @examples
#'
#'
#'mtcars %>%
#'  group_by(am) %>%
#'  summarise_tags(
#'    vars(cyl, hp_lt100 = hp<100),
#'    meanmpg = mean(mpg),
#'    meanwt = mean(wt)
#'  )

######################
