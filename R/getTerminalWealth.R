#' @title The `getTerminalWealth` Function
#' @name getTerminalWealth
#' @aliases plot_terminalwealth
#'
#' @description
#' Given a path of stock price, make investment over time following either the Exploratory
#' Mean-Variance policy or the classical non-exploratory Mean-Variance (MV) policy with
#' Maximum Likelihood Estimation. The terminal wealth is the last portfolio value at the
#' end of the investment period.
#'
#' @param St a stock price path
#' @param policy the investment policy, default to 'MLE classic', can also take one of (MLE exploratory', 'EMV')
#' @param dt (Optional, numeric) the discretization of continuous time, default to 1/252,
#' @param rf (Optional, numeric) riskfree interest rate, default to 0.02
#' @param x0 (Optional, numeric) initial wealth, detault to 1
#' @param z (Optional, numeric) target terminal wealth, detault to 1.4
#' @param invest_hrzn (Optional, numeric) investment horizon, total length of the investment
#' period in years, default to 1.
#' @param lambda (Optional, numeric) the exploratory weight, default to 2
#' @param mwindow_size (Optional, numeric) moving window size, default to 100 and must be non-negative.
#' @param phi1 (Optional, numeric) a parameter in the optimal EMV policy distribution, default to 1
#' @param phi2 (Optional, numeric) a parameter in the optimal EMV policy distribution, default to 0.5
#' @param w (Optional, numeric) Lagrange multiplier, default to 1
#' @param invest_hrzn (Optional, numeric) investment horizon, total length of the investment
#' period in years, default to 1.
#' @param constrained_invest (Optional, logical) default to FALSE, if set to TRUE, then investment
#' is constrained so that abs(investment) <= `max_invest`
#' @param max_invest (Optional, numeric) if `constrained_invest` is set to TRUE, then investment
#' is constrained so that abs(investment) <= max_invest
#'
#'
#' @returns A named list containing:
#' \itemize{
#'  \item{"wealth": }{the path of wealth over the investment period, a vector of length `invest_hrzn`/`dt`}
#'  \item{"controls": }{the path of investmen decisions over the investment period, a vector of length `invest_hrzn`/`dt` - 1}
#'  \item{"terminal_wealth": }{the wealth at the end of the investment period, i.e., the last value of `wealth`}
#' }
#'
#'
#' @examples
#' # First get a stock path
#' St = simulate_stock_path(mu=-0.3, sigma=0.2, dt=1/252, invest_hrzn = 1, mwindow_size = 0)
#'
#' # Then, trade following the MLE classic policy and compute the terminal wealth
#' getTerminalWealth(St)
#'
#' @export

getTerminalWealth = function(St, policy = "MLE classic", dt=1/252, rf=0.02, x0=1, z=1.4,
                             invest_hrzn=1, lambda = 2, mwindow_size=100, phi1=1, phi2=0.5, w=1,
                             constrained_invest=FALSE, max_invest=5){

  testthat::expect_true(policy %in% c("MLE classic", "MLE exploratory", "EMV"),
                        info = "Argument policy has to be one of ('MLE classic', 'MLE exploratory', 'EMV')!")


  Ret_St = log(St[2:length(St)]/St[1:(length(St)-1)])

  if(grepl("MLE", policy)){
    # Compute MLE estimates of mu and sigma for the path
    mavg_Ret_St = rollmean(Ret_St, k=mwindow_size) # moving average of daily log-returns
    mvol_Ret_St = rollapply(Ret_St, mwindow_size, function(x) sqrt(var(x)), by=1) # moving volatility of daily log-returns
    sig_ts = mvol_Ret_St/sqrt(dt) # annualized moving volatility
    mu_ts = mavg_Ret_St/dt + (sig_ts**2)/2 # annualized moving drift
    rho_ts = (mu_ts - rf)/sig_ts # Sharpe ratio
    w_ts = (z*exp((rho_ts**2)*invest_hrzn) - x0)/(exp((rho_ts**2)*invest_hrzn) - 1) # Lagrange multiplier

    # Trade based on the MLE policy
    wealth = rep(1, length(Ret_St[mwindow_size:length(Ret_St)]))
    controls = rep(1, length(Ret_St[mwindow_size:length(Ret_St)])-1)
    for (i in 1:(length(wealth)-1)){
      xt_i = wealth[i]

      if (policy == "MLE classic"){
      # classical MV policy
      ut_i = act_MLE_policy(x_t=xt_i, mu_t=mu_ts[i], sig_t=sig_ts[i],
                            rf=rf, x0=x0, z=z, invest_hrzn=invest_hrzn, exploratory = F)
      }
      else if(policy == "MLE exploratory"){
        ut_i = act_MLE_policy(x_t=xt_i, mu_t=mu_ts[i], sig_t=sig_ts[i],
                              rf=rf, x0=x0, z=z, invest_hrzn=invest_hrzn,
                              exploratory = T, lambda = lambda, current_t = i*dt)
      }

      # Constraint
      if (constrained_invest & abs(ut_i) > max_invest){
        # print(paste("ut:", ut_i, "; xt: ", xt_i, "; mu_t: ", mu_ts[i],
        #             "; sig_t: ", sig_ts[i], "; rho_t: ", rho_ts[i],
        #             "; w_t: ", w_ts[i], sep = ""))
        ut_i = sign(ut_i)*max_invest
      }
      controls[i] = ut_i

      # Compute wealth in the next time step
      xt_ip1 = ut_i*exp(Ret_St[mwindow_size+i]) + (xt_i- ut_i)*(1+rf*dt)

      wealth[i+1] = xt_ip1
    }
  }

  else{
    wealth = rep(1, length(St))  # length(St) = invest_hrzn/dt = 252
    controls = rep(1, length(St)-1)

    for (i in 1:(length(wealth)-1)){
      xt = wealth[i]
      ut = act_EMV_policy(t=i*dt, x=xt, w=w, phi1=phi1, phi2=phi2,
                          lambda=lambda, invest_hrzn = invest_hrzn)
      controls[i] = ut
      xt_ = ut*exp(Ret_St[i]) + (xt- ut)*(1+rf*dt)
      wealth[i+1] = xt_
    }
  }

  return(list("wealth"=wealth,
              "controls"=controls,
              "terminal_wealth"=wealth[length(wealth)]))
}






