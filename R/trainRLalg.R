#' @title The trainRLalg Function
#'
#' @name trainRLalg
#' @aliases plot_trainResult
#'
#' @description
#' `trainRLalg` trains the RL model and updates parameters for the EMV policy and value function.
#'
#' @param N_train (Optional, numeric) number of training epochs, default to 10000
#' @param w_step (Optional, numeric) number of training epochs to update the Lagrange parameter w, default to 20
#' @param random.seed (Optional, numeric) default to 1234
#' @param alpha (Optional, numeric) learning rate to update the Lagrange parameter w, default to 0.05
#' @param lr_theta (Optional, numeric) learning rate to update \eqn{\theta = (\theta_1, \theta_2)}
#' @param lr_phi (Optional, numeric) learning rate to update \eqn{\phi = (\phi_1, \phi_2)}
#' @param lambda (Optional, numeric) the exploratory weight, default to 2
#' @param clip_gradient (Optional, numeric) clip the gradient within a prespecified range (-`clip_gradient`, +`clip_gradient`)
#' to help convergence and avoid gradient explosion, default to 10, and can be turned-off if set to NULL
#' @param theta0 (Optional, numeric) a parameter of the optimal value function, default to NULL and will be
#' automatically computed as \eqn{\theta_0 = -\theta_2 T^2 - \theta_1 T - (w-z)^2} in the function,
#' where \eqn{T} is the `invest_hrzn`
#' @param theta1 (Optional, numeric) a parameter of the optimal value function, default to 0
#' @param theta2 (Optional, numeric) a parameter of the optimal value function, default to -2
#' @param theta3 (Optional, numeric) a parameter of the optimal value function, default to NULL and will be
#' automatically computed as \eqn{\theta_3 = 2\phi_2} in the function
#' @param phi1 (Optional, numeric) a parameter of the optimal EMV policy, default to 0
#' @param phi2 (Optional, numeric) a parameter of the optimal EMV policy, default to 0.01
#' @param w (Optional, numeric) Lagrange multiplier, default to 1.4
#' @param z (Optional, numeric) target terminal wealth, default to 1.4
#' @param mu (Optional, numeric) the drift parameter in the stock price process dynamic, default to -0.3
#' @param sigma (Optional, numeric) the diffusion parameter in the stock price process dynamic, default to 0.2
#' @param S0 (Optional, numeric) the initial stock price, default to 1.
#' @param dt (Optional, numeric) the discretization of continuous time, default to 1/252,
#' representing daily discretization. If set to 1/12, then this is monthly discretization.
#' @param rf (Optional, numeric) riskfree interest rate, default to 0.02
#' @param invest_hrzn (Optional, numeric) investment horizon, total length of the investment
#' period in years, default to 1.
#'
#'
#' @returns A named list containing:
#' \itemize{
#'  \item{"params": }{a `N_train`-by-7 matrix containing the training paths of all the parameters
#'  (`theta_0`, `theta_1`, `theta_2`, `theta_3`, `phi1`, `phi2`, `w`)}
#'  \item{"wealth_paths": }{`N_train` paths of the portfolio wealth over the investment period}
#'  \item{"terminal_wealths": }{a vector of length `N_train` containing the terminal wealths of all the wealth paths}
#'  \item{"investment_paths": }{`N_train` paths of the investment decisions over the investment period}
#'  \item{"TD_errors": }{a vector of length `N_train` containing the TD errors over the training epochs}
#'  \item{"dC_dtheta1s": }{a vector of length `N_train` containing the gradient of TD error, computed for updating theta1}
#'  \item{"dC_dtheta2s": }{a vector of length `N_train` containing the gradient of TD error, computed for updating theta2}
#'  \item{"dC_dphi1s": }{a vector of length `N_train` containing the gradient of TD error, computed for updating phi1}
#'  \item{"dC_dphi2s": }{a vector of length `N_train` containing the gradient of TD error, computed for updating phi2}
#' }
#'
#' @export

trainRLalg = function(N_train=10000, w_step = 20, random.seed=1234, alpha=0.05,
                      lr_theta=0.0005, lr_phi=0.0005, lambda = 2, clip_gradient=10,
                      theta0=NULL, theta1=0, theta2=-2, theta3=NULL, phi1=0, phi2=0.01, w=1.4, z=1.4,
                      mu=-0.3, sigma=0.2, S0=1, dt=1/252, rf=0.02, invest_hrzn = 1
                      ){

  if (any(is.null(theta0), is.null(theta3))){
    theta0 = -theta2^invest_hrzn**2 - theta1*invest_hrzn - (w-z)**2
    theta3 = 2*phi2
  }

  wealth_paths = c()
  controls_paths = c()
  terminal_wealths = c()
  TD_errors = c()
  # TDs = c()
  params = c()
  dC_dtheta1s = c()
  dC_dtheta2s = c()
  dC_dphi1s = c()
  dC_dphi2s = c()
  set.seed(random.seed)

  for (k in 1:N_train){

    params = rbind(params, c(theta0, theta1, theta2, theta3, phi1, phi2, w))

    print(paste0("step: ", k, " in ", N_train, "; theta0: ", theta0, "; theta1: ",
                 theta1, "; theta2: ", theta2, "; theta3: ", theta3,
                 "; phi1: ", phi1, "; phi2: ", phi2, "; w: ", w))

    if (any(is.na(theta1), is.na(theta2), is.na(theta3), is.na(theta0), is.na(phi1), is.na(phi2))){
      print(paste0("k: ", k, "; theta0: ", theta0, "; theta1: ", theta1, "; theta2: ",
                   theta2, "; theta3: ", theta3, "; phi1: ",
                   phi1, "; phi2: ", phi2, "; w: ", w))
      stop(paste0("Got NaNs at step ", k, "!"))
    }

    St = getStockPath(mu=mu, sigma=sigma, dt=dt, invest_hrzn = invest_hrzn, mwindow_size=0)

    Out = getTerminalWealth(St, policy = "EMV", dt=dt, rf=rf, x0=x0, z=z,
                      invest_hrzn=invest_hrzn, lambda = lambda, mwindow_size=0,
                      phi1=phi1, phi2=phi2, w=w,
                      constrained_invest=FALSE, max_invest=5)

    # Record the wealths and controlsf
    wealth_paths = rbind(wealth_paths, Out$wealth)
    controls_paths = rbind(controls_paths, Out$controls)
    terminal_wealths = c(terminal_wealths, Out$terminal_wealth)

    x_is = Out$wealth  # 253
    t_is = seq(0, invest_hrzn, by=dt) # 253

    # Record the TD error
    TD_err = TD_error(t_is=t_is, x_is=x_is, dt=dt, w=w,
                      theta0=theta0, theta1=theta1, theta2=theta2, theta3=theta3,
                      phi1=phi1, phi2=phi2)
    TD_errors = c(TD_errors, TD_err)
    # rho = sqrt(2*phi2)

    td = TD(t_is=t_is, x_is=x_is, dt=dt, w=w,
            theta0=theta0, theta1=theta1, theta2=theta2, theta3=theta3,
            phi1=phi1, phi2=phi2, lambda=lambda, invest_hrzn=invest_hrzn)

    # update theta1 theta2
    dC_dtheta1 = sum(td * dt)
    dC_dtheta1 = sign(dC_dtheta1)*min(abs(dC_dtheta1), clip_gradient) # clip gradient

    theta1 = theta1 - lr_theta * dC_dtheta1
    dC_dtheta1s = c(dC_dtheta1s, dC_dtheta1)

    dC_dtheta2 = sum(td * (t_is[2:length(x_is)]**2 - t_is[1:length(x_is)-1]**2))
    dC_dtheta2 = sign(dC_dtheta2)*min(abs(dC_dtheta2), clip_gradient) # clip gradient

    theta2 = theta2 - lr_theta * dC_dtheta2
    dC_dtheta2s = c(dC_dtheta2s, dC_dtheta2)

    # update theta0 theta3
    theta0 = -theta2*invest_hrzn**2 - theta1*invest_hrzn - (w - z)**2
    theta3 = 2*phi2

    # update phi's
    # td = TD(t_is, x_is, w, theta0, theta1, theta2, theta3, phi1, phi2)
    dC_dphi1 = -lambda * sum(td* dt)
    dC_dphi1 = sign(dC_dphi1)*min(abs(dC_dphi1), clip_gradient) # clip gradient

    phi1 = phi1 - lr_phi*dC_dphi1
    dC_dphi1s = c(dC_dphi1s, dC_dphi1)

    # td = TD(t_is, x_is, w, theta0, theta1, theta2, theta3, phi1, phi2)
    dC_dphi2 = sum(td * dt
                   * (-(2*((x_is[2:length(x_is)] - w)**2)
                        * exp(-2*phi2*(invest_hrzn - t_is[2:length(x_is)]))
                        *(invest_hrzn-t_is[2:length(x_is)])
                        - 2*((x_is[1:(length(x_is)-1)] - w)**2) *
                          exp(-2*phi2*(invest_hrzn- t_is[1:(length(x_is)-1)]))
                        *(invest_hrzn-t_is[1:(length(x_is)-1)]))/dt
                      - lambda*(invest_hrzn - t_is[1:(length(x_is)-1)]))
    )
    dC_dphi2 = sign(dC_dphi2)*min(abs(dC_dphi2), clip_gradient) # clip gradient

    phi2 = max(phi2 - lr_phi*dC_dphi2, 0.001) # make sure phi2 never drops below 0.
    # phi2 = min(max(phi2 - lr_phi*dC_dphi2, 0.001), 5) # 5 >= phi2 > 0

    dC_dphi2s = c(dC_dphi2s, dC_dphi2)

    # Update lagrange multiplier w
    if (k %% w_step == 0){
      xT_z = (mean(terminal_wealths[(length(terminal_wealths)-w_step+1): length(terminal_wealths)]) - z)
      w = w - alpha * xT_z
    }

  }

  colnames(params) = c("theta0", "theta1", "theta2", "theta3", "phi1", "phi2", "w")

  return(list(
    "params"=params,
    "wealth_paths"=wealth_paths,
    "terminal_wealths" = terminal_wealths,
    "investment_paths"=controls_paths,
    "TD_errors"=TD_errors,
    "dC_dtheta1s" = dC_dtheta1s,
    "dC_dtheta2s" = dC_dtheta2s,
    "dC_dphi1s" = dC_dphi1s,
    "dC_dphi2s" = dC_dphi2s
  ))

}


#'
#' @rdname trainRLalg
#' @description
#' `plot_trainResult` plots the training loss (TD_error), the convergence of parameters.
#'
#' @param (Optional, numeric) trainRLalg_output an output object from the `trainRLalg` function, if NULL is given,
#' it will trigger the `trainRLalg` function and train the RL model.
#' @type (Required, string) specifies the training result to show, can take value from ("TD_errors", "params", "thetas", "phis")

plot_trainResult = function(trainRLalg_output, type, ...){
  if (is.null(trainRLalg_output)){
    trainRLalg_output = trainRLalg(...)
  }

  testthat::expect_true(type %in% c("TD_errors", "params", "thetas", "phis"),
                        info = "Argument `type` can only take one of (`TD_errors`, `params`, `thetas`, `phis)")

  if(type=="TD_errors"){
    TD_errors = trainRLalg_output$TD_errors
    plot(TD_errors, type="l", ylab="TD error", xlab="Training epochs", ...)
  }
  else if(type=="params"){
    params = trainRLalg_output$params
    for (p in 1:ncol(params)){
      if (p==1){
        plot(params[, p],type="l", lty=p, col=p,
             ylim=c(min(params), max(params)),
             xlim=c(0, nrow(params)*1.3),
             ylab="Parameters",
             xlab="Training epochs", ...)
        }
      else{
        lines(params[, p], lty=p, col=p, ...)
      }
      }
    legend("right", lty =1:ncol(params), col=1:ncol(params),
           legend = c("theta0", "theta1", "theta2", "theta3", "phi1", "phi2", "w"))

  }
  else if(type=="thetas"){
    params = trainRLalg_output$params
    for (p in 1:4){
      if (p==1){
        plot(params[, p],type="l", lty=p, col=p,
             ylim=c(min(params), max(params)),
             xlim=c(0, nrow(params)*1.3),
             ylab="theta's",
             xlab="Training epochs", ...)
      }
      else{
        lines(params[, p], lty=p, col=p, ...)
      }
    }
    legend("right", lty =1:4, col=1:4,
           legend = c("theta0", "theta1", "theta2", "theta3"))

  }
  else if(type=="phis"){
    params = trainRLalg_output$params
    for (p in 5:6){
      if (p==5){
        plot(params[, p],type="l", lty=p, col=p,
             ylim=c(min(params[, 5:6]), max(params[, 5:6])),
             xlim=c(0, nrow(params)*1.3),
             ylab="phi's",
             xlab="Training epochs", ...)
      }
      else{
        lines(params[, p], lty=p, col=p, ...)
      }
    }
    legend("right", lty =5:6, col=5:6,
           legend = c("phi1", "phi2"))
  }
}








