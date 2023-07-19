#' @title The `act_EMV_policy` Function
#'
#' @name act_EMV_policy
#' @aliases V_function
#' @aliases TD
#' @aliases TD_error
#'
#' @description
#' `act_EMV_policy()` make an investment decision following the Exploratory Mean-Variance (EMV) policy.
#' Suppose the current wealth at time t is \eqn{x_t}, then the optimal EMV policy is a Gaussian
#' distribution parameterized by \eqn{\phi = (\phi_1, \phi_2)}, i.e., \eqn{\pi^\phi(u; t,x,\omega)
#' = N \left(\sqrt{\frac{2\phi_2}{\lambda \pi}} e^{\phi_1 - \frac{1}{2}} (x-\omega),
#' \frac{1}{2\pi} e^{2\phi_2(T-t) + 2\phi_1 - 1} \right)}.
#' Then the optimal investment in the stock \eqn{u_t^*} is sampled from the Gaussian policy
#' distribution and the rest \eqn{x_t - u_t} is invested
#' in the bond.
#'
#' @param t (Required, numeric) current time
#' @param x (Required, numeric) current wealth
#' @param phi1 (Optional, numeric) a parameter in the optimal EMV policy distribution, default to 1
#' @param phi2 (Optional, numeric) a parameter in the optimal EMV policy distribution, default to 0.5
#' @param lambda (Optional, numeric) the exploratory weight, default to 2
#' @param invest_hrzn (Optional, numeric) investment horizon, total length of the investment
#' period in years, default to 1.
#'
#'
#' @returns The amount of money invest in stock (a numerical value of length 1).
#'
#' @examples
#' # Take a non-exploratory action
#' act_EMV_policy(t = 1/252, x=1)
#' @export
#'

act_EMV_policy = function(t, x, w=1, phi1=1, phi2=0.5, lambda=2, invest_hrzn=1){
  testthat::expect_true(all(is.numeric(t), is.numeric(x), is.numeric(w),
                            is.numeric(phi1), is.numeric(phi1), is.numeric(lambda), is.numeric(invest_hrzn)),
                        info="All of the inputs have to be numerical!")
  testthat::expect_true(all(length(t)==1, length(x)==1, length(w)==1,
                            length(phi1)==1, length(phi1)==1, length(lambda)==1, length(invest_hrzn)==1),
                        info="All of the inputs have to be a single number of length 1!")
  testthat::expect_true(phi2 > 0, info="Parameter phi2 has to be positive!")
  testthat::expect_true(all(t > 0, invest_hrzn > 0),
                        info="All of (t, invest_hrzn) have to be positive!")

  rho = sqrt(2*phi2)
  u_mean = sqrt(2*phi2/(lambda*pi)) * exp(phi1-0.5) *(x-w) # suppose we know rho < 0
  u_sd = sqrt(exp(2*phi2*(invest_hrzn-t) + 2*phi1 - 1)/(2*pi))
  u = rnorm(1, mean = u_mean, sd = u_sd)
  return(u)
}


#' @rdname act_EMV_policy
#' @description
#' `V_function` computes the optimal value function in the EMV problem, parameterized by
#' \eqn{\theta = (\theta_0, \theta_1, \theta_2, \theta_3)}. Mathematically, this is
#' \eqn{V^\theta(t,x) = (x-\omega)^2 e^{-\theta_3 (T-t)} + \theta_2 t^2 + \theta_1 t + \theta_0}.
#'
#' @param theta0 (Optional, numeric) a parameter of the optimal value function, default to 1,
#' same for theta1, theta2, theta3.
#'
#' @export

V_function = function(t, x, w=1, theta0=1, theta1=1, theta2=1, theta3=1, invest_hrzn=1){
  testthat::expect_true(all(is.numeric(t), is.numeric(x), is.numeric(w), is.numeric(theta0), is.numeric(theta1),
                            is.numeric(theta2), is.numeric(theta3), is.numeric(invest_hrzn)),
                        info="All of the inputs have to be numerical!")
  testthat::expect_true(all(length(w)==1, length(theta0)==1, length(theta1)==1,
                            length(theta2)==1, length(theta3)==1, length(invest_hrzn)==1),
                        info="All of the inputs (except for t,x) have to be a single number of length 1!")
  testthat::expect_true(theta3 > 0, info="Parameter theta3 has to be positive!")
  testthat::expect_true(all(t >= 0, invest_hrzn > 0),
                        info="All of (t, invest_hrzn) have to be positive!")

  V = ((x-w)**2) * exp(-theta3*(invest_hrzn - t)) + theta2*(t**2) + theta1*t + theta0
  return(V)
}



#' @rdname act_EMV_policy
#' @description
#' `TD` computes temporal difference as \eqn{TD(t,x) = \frac{V^\theta(t+1, X_{t+1}) - V^\theta(t, X_{t})}{\Delta t} - \lambda(\phi_1 + \phi_2(T-t))}.
#'
#' @param t_is a vector of time steps
#' @param x_is a vector of wealth over time
#'
#' @export
#'
TD = function(t_is, x_is, dt=1/252, w=1, theta0=1, theta1=1, theta2=1, theta3=1, phi1=1, phi2=0.5, lambda=2, invest_hrzn=1){
  testthat::expect_true(all(is.numeric(t_is), is.numeric(x_is), is.numeric(dt), is.numeric(w),
                            is.numeric(phi1), is.numeric(phi1),
                            is.numeric(theta0), is.numeric(theta1), is.numeric(theta2), is.numeric(theta3),
                            is.numeric(lambda), is.numeric(invest_hrzn)),
                        info="All of the inputs have to be numerical!")
  testthat::expect_true(all(length(w)==1, length(phi1)==1, length(phi1)==1,
                            length(theta0)==1, length(theta1)==1, length(theta2)==1, length(theta3)==1,
                            length(lambda)==1, length(invest_hrzn)==1),
                        info="All of the inputs (except for t_is, x_is) have to be a single number of length 1!")
  testthat::expect_true(phi2 > 0, info="Parameter phi2 has to be positive!")
  testthat::expect_true(theta3 > 0, info="Parameter theta3 has to be positive!")
  testthat::expect_true(all(dt >0, invest_hrzn > 0),
                        info="All of (dt, invest_hrzn) invest_hrzn has to be positive!")

  td = (V_function(t_is[2:length(t_is)],x_is[2:length(t_is)], w, theta0, theta1, theta2, theta3)
        - V_function(t_is[1:(length(t_is)-1)],x_is[1:(length(t_is)-1)], w, theta0, theta1, theta2, theta3))/dt
  - lambda *(phi1 + phi2*(invest_hrzn-t_is[1:(length(t_is)-1)]))
  return(td)
}


#' @rdname act_EMV_policy
#' @description
#' `TD_error` computes temporal difference error as \eqn{C(\theta, \phi) = \frac{1}{2} \sum_{t,x} TD(t,x)^2 \cdot dt}.
#'
#' @export
#'
TD_error = function(t_is, x_is, dt=1/252, w=1, theta0=1, theta1=1, theta2=1, theta3=1, phi1=1, phi2=0.5){
  testthat::expect_true(all(is.numeric(t_is), is.numeric(x_is), is.numeric(dt), is.numeric(w),
                            is.numeric(phi1), is.numeric(phi1),
                            is.numeric(theta0), is.numeric(theta1), is.numeric(theta2), is.numeric(theta3)),
                        info="All of the inputs have to be numerical!")
  testthat::expect_true(all(length(w)==1, length(phi1)==1, length(phi1)==1,
                            length(theta0)==1, length(theta1)==1, length(theta2)==1, length(theta3)==1),
                        info="All of the inputs (except for t_is, x_is) have to be a single number of length 1!")
  testthat::expect_true(all(phi2 > 0, theta3>0), info="Parameter (phi2, theta3) have to be positive!")
  testthat::expect_true(dt > 0, info="Argument dt has to be positive!")

  TD_err = sum(TD(t_is, x_is, dt, w, theta0, theta1, theta2, theta3, phi1, phi2)**2 *dt)/2
  return(TD_err)
}

