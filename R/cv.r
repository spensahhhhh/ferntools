#' @title Calculate CV
#'
#' @description Calculates the Coefficient of Variation ("CV") of a vector of numbers.
#' 
#' @param x Vector of numbers
#' @param na_rm Whether `NA`s should be removed. Defaults to `TRUE`
#'
#' @returns Coefficient of variation of the numbers provided to 'x'
#' @export
#' 
#' @examples
#' # Calculate CV
#' cv(x = c(4, 5, 6), na_rm = TRUE)
#' 
cv <- function(x = NULL, na_rm = TRUE){

  # Check input(s)
  if(is.null(x) || is.numeric(x) != TRUE)
    stop("'x' must be numeric")

  if(is.logical(na_rm) != TRUE){
    warning("'na_rm' must be a logical. Coercing to TRUE.")
    na_rm <- TRUE 
  }

  # Calculate SD and mean
  sd_x <- sd(x = x, na.rm = na_rm)
  avg_x <- mean(x = x, na.rm = na_rm)

  # Calculate coefficient of variation
  coef_var <- sd_x / avg_x

  # Return that 
  return(coef_var)
}

# End ----
