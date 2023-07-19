#' @title The act_MLE_policy Function
#'
#' @description
#' `act_MLE_policy()` make an investment decision following the classical Mean-Variance policy.
#' Suppose the current wealth at time t is \eqn{x_t}, then the classical optimal policy is to
#' invest \eqn{u_t^* = -\frac{\rho}{\sigma}(x-\omega)} in the stock and invest the rest
#' \eqn{x_t - u_t} in the bond, where \eqn{\rho := \frac{\mu - r}{\sigma}} is the Sharpe ratio
#' and \eqn{r} is the riskfree interest rate.
#'
#' @param x_t (Required, numeric) current wealth
#' @param mu_t (Required, numeric) current MLE estimate of mu
#' @param sig_t (Required, numeric) current MLE estimate of sigma
#' @param rf (Optional, numeric) riskfree interest rate, default to 0.02
#' @param x0 (Optional, numeric) initial wealth, detault to 1
#' @param z (Optional, numeric) target terminal wealth, detault to 1.4
#' @param invest_hrzn (Optional, numeric) investment horizon, total length of the investment
#' period in years, default to 1.
#' @param exploratory (Optional, logical) default to False. If set to True, then apply the
#' optimal Exploratory Mean-Variance (EMV) policy by sampling \eqn{u_t \sim N(u_t^*, \frac{\lambda}{2\sigma^2}e^{-\rho^2(T-t)})},
#' with all of \eqn{(\rho, \mu, \sigma)} replaced with their MLE estimates.
#' @param lambda (Optional, numeric) the exploratory weight, default to NULL, only use it if
#' `exploratory == TRUE`
#' @param current_t (Optional, numeric) the current time, default to NULL, only use it if
#' `exploratory == TRUE`.
#'
#' @returns The amount of money invest in stock (a numerical value of length 1).
#'
#' @examples
#' # Take a non-exploratory action
#' act_MLE_policy(x_t = 0, mu_r=0.3, sig_t=0.2)
#'
#' # Take an exploratory action
#' act_MLE_policy(x_t = 0, mu_r=0.3, sig_t=0.2, exploratory=T, lambda=2, current_t=1/252)
#' @export

act_MLE_policy = function(x_t, mu_t, sig_t, rf = 0.02, x0=1, z=1.4, invest_hrzn = 1,
                          exploratory=F, lambda=NULL, current_t=NULL){
  testthat::expect_true(all(is.numeric(x_t), is.numeric(mu_t), is.numeric(sig_t), is.numeric(rf),
                            is.numeric(x0), is.numeric(z), is.numeric(invest_hrzn)),
                        info="All of (x_t, mu_t, sig_t, rf, z, invest_hrzn) have to take numerical values!")
  testthat::expect_true(all(length(x_t)==1, length(mu_t)==1, length(sig_t)==1, length(rf)==1,
                            length(x0)==1, length(z)==1, length(invest_hrzn)==1),
                        info="All of (x_t, mu_t, sig_t, rf, z, invest_hrzn) have to be singular numbers of length 1!")
  testthat::expect_true(all(sig_t > 0, rf > 0, x0>0, invest_hrzn > 0), info="All of (sig_t, rf, x0, invest_hrzn) have to be positive!")
  testthat::expect_true(is.logical(exploratory), info="exploratory has to take logical value!")

  rho_t = (mu_t - rf)/sig_t
  w = (z*exp((rho_t**2)*invest_hrzn) - x0)/(exp((rho_t**2)*invest_hrzn) - 1)
  if (exploratory){
    testthat::expect_true(all(is.numeric(lambda), is.numeric(current_t)),
                          info="When exploratory == TRUE, all of (lambda, current_t) have to take numerical values!")
    testthat::expect_true(current_t >= 0,
                          info="When exploratory == TRUE, current_t has to be non-negative!")

    u_mean = -rho_t*(x_t - w)/sig_t
    u_var = lambda*exp((rho_t**2)*(invest_hrzn-current_t))/(2*(sig_t**2))
    u = rnorm(1, mean=u_mean, sd=sqrt(u_var))
  }
  else{
    u = -rho_t*(x_t - w)/sig_t
  }
  return(u)
}
