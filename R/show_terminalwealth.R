#' @title Illustration of Terminal Wealth
#' @name show_terminalwealth
#'
#' @rdname show_terminalwealth
#' @description
#' `plot_terminalwealth` plots the boxplot/density plot/ECDF plot/qqplot of the terminal wealth.
#'
#' @param terminal_wealth (Required, numeric) the terminal wealth from 1 or multiple polices, can take either a vector or a
#' matrix with multiple columns.
#' @param plot_type (Required, string) plot type, take value from ('boxplot', 'density', 'ecdf', 'qqplot')

plot_terminalwealth = function(terminal_wealth, plot_type, ...){
  testthat::expect_true(is.numeric(terminal_wealth), info="Argument `terminal_wealth` has to be numeric!")
  testthat::expect_true(plot_type %in% c('boxplot', 'density', 'ecdf', 'qqplot'),
                        info="Argument type has to take value from ('boxplot', 'density', 'ecdf', 'qqplot')!")

  if (plot_type=="boxplot"){
    boxplot(terminal_wealth, ...)
  }
  else if(plot_type == "density"){
    if (ncol(terminal_wealth) > 1){
      plot(density(terminal_wealth[,1]), main="", col=1,
           ylab="Density of terminal wealth",
           xlab = "Terminal wealth", ...)
      for (i in 2:ncol(terminal_wealth)){
        lines(density(terminal_wealth[,i]), col=i)
      }
      legend("topleft", col=1:ncol(terminal_wealth), lty=1,
             legend=colnames(terminal_wealth))
    }
    else{
      plot(density(terminal_wealth), main="",
         ylab="Density of terminal wealth",
         xlab = "Terminal wealth", ...)
      }
  }
  else if(plot_type=="ecdf"){
    if (ncol(terminal_wealth) > 1){
      plot(ecdf(terminal_wealth[,1]), main="", col=1,
           ylab="ECDF of terminal wealth",
           xlab = "Terminal wealth", ...)
      for (i in 2:ncol(terminal_wealth)){
        lines(ecdf(terminal_wealth[,i]), col=i)
      }
      legend("left", col=1:ncol(terminal_wealth),
             lty=1, legend=colnames(terminal_wealth))
    }
    else{
      plot(ecdf(terminal_wealth), main="",
           ylab="ECDF of terminal wealth",
           xlab = "Terminal wealth", ...)
    }
  }
  else if(plot_type=="qqplot"){
    testthat::expect_true(ncol(terminal_wealth)==2,
                          info="The input must have exactly 2 columns!")
    qqplot(terminal_wealth[,1], terminal_wealth[,2],
           xlab=colnames(terminal_wealth)[1], ylab=colnames(terminal_wealth)[2], ...)
    abline(a=0, b=1, col="red")
  }
}

#'
#' @rdname show_terminalwealth
#' @description
#' `table_terminalwealth` tabularizes the summary and quantiles of the terminal wealth.
#'
#' @param table_type the table type can be one of ('summary', 'quantile')
#' @param rf riskfree interest rate, default to 0.02
#'
#' @export

table_terminalwealth = function(terminal_wealth, table_type, rf=0.02, ...){
  if (table_type == "summary"){
    if (ncol(terminal_wealth) >1){
      smry = c()
      for (i in 1:ncol(terminal_wealth)){
        mu_hati = mean(terminal_wealth[,i])
        sig_hati = sqrt(var(terminal_wealth[,i]))
        smry = rbind(smry, c(mu_hati, sig_hati, (mu_hati-rf)/sig_hati))
      }
      colnames(smry) = c("Mean", "Volatility", "Sharpe ratio")

    # tab = c(mean(smry[,"Mean"]), sqrt(var(smry[,"Mean"])),
    # mean(smry[,"Volatility"]), sqrt(var(smry[,"Volatility"])),
    # mean(smry[,"Sharpe ratio"]), sqrt(var(smry[,"Sharpe ratio"])))
    tab = list("Mean" = mean(smry[,"Mean"]), "MCSE(Mean)" = sqrt(var(smry[,"Mean"])),
               "Volatility" = mean(smry[,"Volatility"]), "MCSE(Volatility)" = sqrt(var(smry[,"Volatility"])),
               "Sharpe ratio" = mean(smry[,"Sharpe ratio"]), "MCSE(Sharpe ratio)" = sqrt(var(smry[,"Sharpe ratio"])))
    # colnames(tab) = c("Mean", "MCSE(Mean)", "Volatility", "MCSE(Volatility)",
    #                   "Sharpe ratio", "MCSE(Sharpe ratio)")
    }
    else{
      mu_hat = mean(terminal_wealth)
      sig_hat = sqrt(var(terminal_wealth))
      # tab = c(mu_hat, sig_hat, (mu_hat-rf)/sig_hat)
      # colnames(tab) = c("Mean", "Volatility", "Sharpe ratio")
      tab = list("Mean"=mu_hat, "Volatility"=sig_hat, "Sharpe ratio"= (mu_hat-rf)/sig_hat)
    }
  }

  else if (table_type == "quantile"){
    tab = quantile(terminal_wealth, prob=c(0.05, 0.1, 0.25, 0.5, 0.75, 1),...)
  }
  return(tab)
}









