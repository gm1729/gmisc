#' Trying Out New Ideas for the Wald Function.
#'
#' This function implements experimental additional functionality for
#' wald function.  If the function has a 'data' argument, the 'L'
#' matrix is so each row corresponds to a row of the data argument.
#' By default, the L matrix is the design matrix, thus producing fitted
#' values and standard errors.
#'
#' \emph{CAUTION:} Currently works only for \code{lme} objects. The
#' \code{model.matrix} method for \code{lm} does not handle
#' a data argument correctly.
#'
#' The function depends on a functional method for
#' \code{model.matrix} and thus, \code{model.frame} methods.
#' It is anticipated that some problems may arise due do problems
#' with these methods.
#'
#' @param fit A model with a method for \code{model.matrix}
#' @param data A data frame to generate values of the L matrix
#' @param predvar The name of the variable (appended with '.hat' and
#'   'se') for the estimated value.
#' @details The functionality duplicates, in part, that of
#' \code{predict(..., se.fit = TRUE)}, it can be used for fitting functions
#' that do not accept the \code{se.fit} argument, e.g. lme.
#' @return The input \code{data} along with the estimated variable and its
#'   standard error.
#' @examples
#' library(spida)
#' library(nlme)
#' fit <- lme(mathach ~ Sector * ses, hs, random = ~ 1 | school)
#' pred <- expand.grid( ses = seq(-2,2, 1), Sector = levels(hs$Sector))
#' head(Wald(fit, pred))
#'
Wald <-
  function(fit, data,
           predvar = as.character(formula(fit)[2])){
    # to be added in wald
    mm <- model.matrix(fit, data)
    ww <- as.data.frame(wald(fit, mm))
    if( is.null(predvar) || is.na(predvar) || (predvar == FALSE)) {
      names(ww) <- c("yhat", 'se')
    } else {
      names(ww) <- paste(predvar, c("hat","se"), sep = '.')
    }
    ret <- cbind(ww, data)
    ret
  }
