#' Function to produce equation of linear model
#' given the model object. Intended for use with lm
#' Author: Marina Varfolomeeva
#' Arguments:
#' fit - lm model object
#' strict - shoud the variable names be replaced with Xn
lm_equation <- function(fit, strict = TRUE, digits = 2, trim = TRUE){
  #   extracting call formula
  frml <- as.character(fit$call)[2]
  #   extract signs
  sign <- ifelse(grepl("-", coef(fit)[-1]), " - ", " + ")
  # extract coefficients
  coeffs <- format(abs(coef(fit)), digits = digits, trim = trim)
  if(strict == TRUE){
    i <- 1:(length(coeffs) - 1)
    vars <- c("Y", paste0(" X", i))

  } else {
    # extract vector of variable names
    vars <- unlist(strsplit(frml, "[~+]"))
    # combine everything
  }
  start <- ifelse(coef(fit)[1] > 0, paste(vars[1], coeffs[1], sep = " = "), paste(vars[1], coeffs[1], sep = " = - "))
  end <- paste(sign, coeffs[-1], vars[-1], sep = "", collapse = "")
  return(paste(start, end, sep = ""))
}
