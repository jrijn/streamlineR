#' @import ggplot2
#' @import drc
#' @import ggpubr

#' @title Calculate R2
#' @export
# From: https://github.com/OnofriAndreaPG/aomisc
R2nls <- function(object){

  if (!inherits(object, "nls") & !inherits(object, "drc"))
    stop("use only with \"nls\" or \"drc\" objects")

  if (inherits(object, "nls")){
    formula <- as.formula(summary(object)$formula)
    varNames <- all.vars(formula)
    if(!is.environment(eval(object$data))){
      Y <- eval(object$data)[,varNames[1]]
    } else {
      Y <- get(varNames[1])
    }
    dfMod <- summary(object)$df[2]
    sigma <- summary(object)$sigma
  } else if (inherits(object, "drc")){
    object$origData
    varNames <- all.vars(object$call$formula)
    Y <- eval(object$data)[,varNames[1]]
    dfMod <- summary(object)$df
    sigma <- sqrt( summary(object)$resVar )
  }

  SSm <- sum( (fitted(object) - mean(Y))^2) # Regression deviance
  SSt <- deviance( lm(Y ~ 1) ) # Total deviance about the mean
  SSr <- deviance(object)  # Residual deviance
  PseudoR2 <- (SSt - SSr)/SSt # R2 as in Schebenberger Eq. 5.24 (pag. 211)
  R2 <- SSm/SSt # R2 traditional as in Scebenberger Eq. 5.23

  # MSt <- SSt/(length(Y) - 1)
  # MSr <- SSr/dfMod
  # R2adj <- 1 - MSr/MSt # Adjusted R2
  #
  # MSE <- SSr/dfMod
  # RMSE <- sigma
  # RRMSE <- RMSE/mean(Y)

  # R2 generalised: to be done for GLMs
  # ll1 <- as.numeric(logLik(object))
  # ll2 <- as.numeric(logLik( lm(Y ~ 1) ))
  # R2gen <- 1 - exp(-2/length(Y)*(ll1 - ll2)) # Schabenberger, 6.46 pag 343
  # R2genResc <- R2gen/(1 - exp(2/length(Y) * ll2)) # Schabenberger, 6.48 pag 344
  returnList <- list(PseudoR2 = PseudoR2, R2 = R2
                     # R2adj = R2adj,
                     #R2gen = R2gen, R2gen.rescaled = R2genResc,
                     # , MSE = MSE, RMSE = RMSE, RRMSE = RRMSE
  )
  returnList
}




#' @title Fit a 5 parameter logistical model to dose-response data.
#'
#' @param standards data.frame; With columns "dose" and "response".
#' @param samples data.frame; With column "response".
#' @param Rmin numeric; Optional, define the minimum.
#' @param Rmax numeric; Optional, define the maximum
#' @param summary boolean; Print fit summary, default = TRUE.
#'
#' @return Returns a data.frame with the estimated dose values corresponding to
#'         the samples response values.
#' @export
#'
#' @examples
#'
#' df <- dplyr::rename(standard,
#'                     response = absorbance,
#'                     dose = concentration)
#' df2 <- dplyr::rename(samples,
#'                      response = absorbance.av)
#' fit.5PL(df, df2, Amin = 0, summary = T)
#'
fit.5PL <- function(standards, samples,
                    Rmin = NA, Rmax = NA, n = NA, summary = TRUE) {

  if(!is.data.frame(standards)) {
    warning("The parameter 'standards' is not a data frame. Returning NULL.")
    return(NULL)
  }

  if(!is.data.frame(samples)) {
    warning("The parameter 'standards' is not a data frame. Returning NULL.")
    return(NULL)
  }

  fit <- drm(response ~ dose,
             data = standards,
             type = "continuous",
             fct = LL2.5(names = c("n", "Rmin", "Rmax", "EC50", "f"),
                         fixed = c(n, Rmin, Rmax, NA, NA)))

  if(summary) print(summary(fit))

  # predictions and confidence intervals.
  lowest = min(standards$dose)/10
  highest = max(standards$dose)*10
  demo.fits <- expand.grid(dose=exp(seq(log(0.001), log(highest), length=100)))

  # new data with predictions
  pm <- predict(fit, newdata=demo.fits, interval="confidence")
  demo.fits$p <- pm[,1]
  demo.fits$pmin <- pm[,2]
  demo.fits$pmax <- pm[,3]
  # print(head(demo.fits))

  samples.fit <- samples %>%
    bind_cols(ED(fit, samples$response, type = "absolute", display = F))
  # print(head(samples.fit))

  p1 <- ggplot(standards, aes(dose, response))+
    scale_x_continuous(trans = scales::log10_trans(),
                       minor_breaks = log10_minor_break(),
                       limits = c(1E1, max(standards$dose)*1.1))+
    scale_y_continuous(limits = c(0, max(standards$response)*1.1))+
    geom_point()+
    geom_point(data = samples.fit,
               aes(x = exp(Estimate),
                   y = response),
               color = "red")+
    geom_line(data = demo.fits, aes(x = dose, y = p))+
    geom_ribbon(data = demo.fits,
                aes(x = dose,
                    y = p,
                    ymin = pmin,
                    ymax = pmax),
                alpha = 0.15)+
    labs(caption = paste("pseudo-R2:",
                         round(R2nls(fit)$PseudoR2, digits = 4)))+
    publish(major_grid = T, minor_grid = T)

  fit.res <- as.data.frame(fit$predres)
  colnames(fit.res) <- make.names(colnames(fit.res))

  p2 <- ggplot(fit.res, aes(Predicted.values, Residuals))+
    geom_hline(yintercept = 0, linetype = "dashed")+
    geom_point()+
    labs(x = "Predicted values")+
    publish(major_grid = T, minor_grid = T)

  print(ggarrange(p1, p2),
        align = "hv")

  returnList <- list(
    estimates = samples.fit,
    fit = p1,
    estimates = p2
  )

  return(returnList)
}


