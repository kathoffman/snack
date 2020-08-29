#' @title GLM table with sandwich errors
#' @description tidy table to obtain estimates and 95% CIs calculated from robust standard errors for a linear or logistic regression model
#' @param glm_fit glm.fit object for a linear or logistic regression
#'
#' @return tidy tibble for plotting and reporting
#' @export
#'
#' @importFrom stats rnorm glm
#' @examples
#' x <- rnorm(100)
#' y <- rnorm(100)
#' fit <- glm(y~x)
#' sandwich_glm(fit)
sandwich_glm <- function(glm_fit){
  if (glm_fit$family$link == "identity"){
    return(sandwich_glm_identity(glm_fit))
  }
  if (glm_fit$family$link == "logit"){
    return(sandwich_glm_logit(glm_fit))
  }
  else{
    print("`sandwich_glm` does not yet support this `glm.fit` object.")
  }
}

#' @title identity link sandwich_glm
#' @description tidy sandwich glm for a linear model (identity link)
#'
#' @importFrom stats pnorm
#' @importFrom sandwich vcovHC
#' @param glm_fit glm.fit object for a linear regression
#'
#' @return tidy tibble for plotting and reporting
sandwich_glm_identity <- function(glm_fit){
  variable <- names(glm_fit$coefficients)
  sandwich_se <- diag(vcovHC(glm_fit, type = "HC"))^0.5
  df <- data.frame(variable, "estimate" = glm_fit$coefficients, sandwich_se)
  df$conf_low <- df$estimate - df$sandwich_se*1.96
  df$conf_high <- df$estimate + df$sandwich_se*1.96
  df$p_value = 2*(1-pnorm(abs(df$estimate)/df$sandwich_se))
  df <- df[-which(df$variable == "(Intercept)"),]
  out <- df[c("variable", "estimate", "conf_low", "conf_high", "p_value")]
  row.names(out) <- NULL
  return(out)
}

#' @title logit link sandwich_glm
#' @description tidy sandwich glm for a logistic model (logit link)
#'
#' @param glm_fit glm.fit object for a logistic regression
#' @importFrom stats pnorm
#' @importFrom sandwich vcovHC
#' @return tidy tibble for plotting and reporting
sandwich_glm_logit <- function(glm_fit){
  variable <- names(glm_fit$coefficients)
  sandwich_se <- diag(sandwich::vcovHC(glm_fit, type = "HC"))^0.5
  df <- data.frame(variable, "estimate" = glm_fit$coefficients, sandwich_se)
  df$conf_low <- exp(df$estimate - df$sandwich_se*1.96)
  df$conf_high <- exp(df$estimate + df$sandwich_se*1.96)
  df$p_value = 2*(1-pnorm(abs(df$estimate)/df$sandwich_se))
  df$estimate <- exp(df$estimate)
  df <- df[-which(df$variable == "(Intercept)"),]
  out <- df[c("variable", "estimate", "conf_low", "conf_high", "p_value")]
  row.names(out) <- NULL
  return(out)
}

#' @title Report ready GLM table with sandwich errors
#' @description tidy table to obtain ready-to-report estimates and 95% CIs calculated from robust standard errors for a linear or logistic regression model
#'
#' @param glm_fit glm.fit object for a linear or logistic regression
#' @importFrom stats rnorm glm
#' @return tidy data frame with variable, estimate (95% CI), and p-value, ready for reporting
#' @export
#'
#' @examples
#' x <- rnorm(100)
#' y <- rnorm(100)
#' fit <- glm(y~x)
#' report_sglm(fit)
#'
#' x <- rnorm(100)
#' y <- rbinom(100,1,.5)
#' fit <- glm(y~x, family="binomial")
#' report_sglm(fit)
report_sglm <- function(glm_fit){
  glm_tbl <- sandwich_glm(glm_fit)
  glm_tbl$estimate <- paste0(round(glm_tbl$estimate, 2), " (", round(glm_tbl$conf_low, 2), ", ", round(glm_tbl$conf_high,2),")")
  glm_tbl$p_value <- ifelse(glm_tbl$p_value < .001, "<.001", round(glm_tbl$p_value, 3))
  out <- glm_tbl[c("variable", "estimate","p_value")]
  return(out)
}
