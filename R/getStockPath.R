#' @title The `getStockPath` Function
#'
#' @description
#' `getStockPath()` simulates a path of stock price process, following a geometric Brownian Motion.
#'
#'
#' @details The stock price process \eqn{\{S_t\}_{0\leq t \leq T}} followng a geometric Brownian Motion with dynamic:
#' \eqn{d S_t = S_t (\mu dt + \sigma dW_t)}, where \eqn{\{W_t\}_{0\leq t \leq T}} is a 1-dim Brownian Motion.
#'
#'
#' @param mu (Required, numeric) the drift parameter in the stock price process dynamic.
#' @param sigma (Required, numeric) the diffusion parameter in the stock price process dynamic.
#' @param S0 (Optional, numeric) the initial stock price, default to 1.
#' @param dt (Optional, numeric) the discretization of continuous time, default to 1/252,
#' representing daily discretization. If set to 1/12, then this is monthly discretization.
#' @param invest_hrzn (Optional, numeric) investment horizon, total length of the investment
#' period in years, default to 1.
#' @param mwindow_size (Optional, numeric) moving window size, default to 100 and must be non-negative.
#'
#' @returns A simulated path of stock prices of length `invest_hrzn/dt + mwindow_size`.
#'
#' @examples
#' # Simulate 100 paths of stocks prices.
#' for (n in 1:100){
#'   St = simulate_stock_path(mu=mu, sigma=sig, dt=dt, invest_hrzn = 1, mwindow_size = 0)
#'   if (n == 1){
#'   plot(St, type = "l", ylim = c(0,2), col="gray",
#'       ylab="Stock Price ($)", xlab = "Time (days)")
#'   }
#'   else{
#'   lines(St, col="gray")
#'   }
#' }
#' @export


getStockPath = function(mu, sigma, S0 = 1, dt=1/252,
                        invest_hrzn=1, mwindow_size = 100){
  testthat::expect_true(all(is.numeric(mu), is.numeric(sigma), is.numeric(S0), is.numeric(dt),
                            is.numeric(invest_hrzn), is.numeric(mwindow_size)),
                        info="All of the inputs have to take numerical values!")
  testthat::expect_true(all(S0 > 0, dt > 0, invest_hrzn > 0),
                        info="All of (S0, dt, invest_hrzn) have to be positive!")
  testthat::expect_true(all(round(mwindow_size) == mwindow_size, mwindow_size >= 0),
                        info="mwindow_size has to be a non-negative integer!")

  dW = rnorm(ceiling(invest_hrzn/dt)+mwindow_size, mean = 0, sd=sqrt(dt))
  St = S0 * exp((mu-(sigma**2)/2)*dt + sigma*dW)
  St = c(S0, cumprod(St))
  return(St)
}







