library(EMVportfolio)
act_MLE_policy(1, 0.3, 0.2)

St = getStockPath(-0.3, 0.2)
trade = getTerminalWealth(St)

is.numeric(trade$wealth)

Out = trainRLalg(N_train = 5000)

devtools::document()
devtools::load_all()


# =============== Plot Stock Price Paths ============= %
for (i in 1:100){
  St= getStockPath(-0.3, 0.2, mwindow_size = 0)
  if (i==1){
    par(mai=c(1.02,1,0.4,0.3))
    plot(St, col="gray", type="l", ylim = c(0.3,1.5),
         ylab="Stock Price ($)", xlab = "Time (days)",
         cex.lab=1.5)
  }
  else{
    lines(St, col="gray")
  }
}


# ================ Plot Train Results =============== %

PATH = "/Users/maxchen/Documents/Study/STA/STAT900/Writings/Final Project/logs/EMV1/"
params = read.csv(paste0(PATH, "params.csv"))
TD_errors = read.csv(paste0(PATH, "TD_errors.csv"))


trainRLalg_output = list("params"=params[,2:ncol(params)], "TD_errors"=TD_errors)
par(mai=c(1.02,1,0.4,0.3))
plot_trainResult(trainRLalg_output, type = "thetas", cex.lab=1.5)

par(mai=c(1.02,1,0.4,0.3))
plot_trainResult(trainRLalg_output, type = "phis", cex.lab=1.5)

par(mai=c(1.02,1,0.4,0.3))
plot_trainResult(trainRLalg_output, type = "TD_errors", cex.lab=1.5)

# ================ Simulated multiple runs for MCSE ============= %
N_runs = 50
N_sim = 100
set.seed(1111)
params = read.csv("/Users/maxchen/Documents/Study/STA/STAT900/Writings/Final Project/logs/EMV1/params.csv")
phi1 = params[nrow(params),"phi1"]
phi2 = params[nrow(params),"phi2"]
w = params[nrow(params), "w"]

terminal_wealths = list("MLE_classic_u"=c(), "MLE_classic"=c(),
                        "MLE_exploratory"=c(), "EMV"=c())
for(run in 1:N_runs){
  MLEcu = c()
  MLEc = c()
  MLEe = c()
  EMV = c()
  for(n in 1:N_sim){
    print(paste0("run: ", run, " in ", N_runs, "; sim: ", n, " in ", N_sim))
    St = getStockPath(mu=-0.3, sigma=0.2, mwindow_size = 100)
    Out_MLEcu = getTerminalWealth(St, "MLE classic", constrained_invest = F)
    Out_MLEc = getTerminalWealth(St, "MLE classic", constrained_invest = T, max_invest = 5)
    Out_MLEe = getTerminalWealth(St, "MLE exploratory", constrained_invest = T, max_invest = 5)
    MLEcu = c(MLEcu, Out_MLEcu$terminal_wealth)
    MLEc = c(MLEc, Out_MLEc$terminal_wealth)
    MLEe = c(MLEe, Out_MLEe$terminal_wealth)

    St_emv = St[101: length(St)]
    Out_EMV = getTerminalWealth(St_emv, "EMV", phi1=phi1, phi2=phi2, w=w)
    EMV = c(EMV, Out_EMV$terminal_wealth)
  }
  terminal_wealths$MLE_classic_u = cbind(terminal_wealths$MLE_classic_u, MLEcu)
  terminal_wealths$MLE_classic = cbind(terminal_wealths$MLE_classic, MLEc)
  terminal_wealths$MLE_exploratory = cbind(terminal_wealths$MLE_exploratory, MLEe)
  terminal_wealths$EMV = cbind(terminal_wealths$EMV, EMV)
}


# Tables
names(terminal_wealths)
terminal_wealths$MLE_classic_u

MLE = table_terminalwealth(terminal_wealths$MLE_classic_u, "summary")
MLE_constrained = table_terminalwealth(terminal_wealths$MLE_classic, "summary")
MLE_exploratory = table_terminalwealth(terminal_wealths$MLE_exploratory, "summary")
EMV = table_terminalwealth(terminal_wealths$EMV, "summary")
knitr::kable(data.frame(rbind(MLE, MLE_constrained, MLE_exploratory, EMV)), digits = 3)


MLE_q = table_terminalwealth(terminal_wealths$MLE_classic_u, "quantile", )
MLE_constrained_q = table_terminalwealth(terminal_wealths$MLE_classic, "quantile")
MLE_exploratory_q = table_terminalwealth(terminal_wealths$MLE_exploratory, "quantile")
EMV_q = table_terminalwealth(terminal_wealths$EMV, "quantile")
data.frame(round(rbind(MLE_q, MLE_constrained_q, MLE_exploratory_q, EMV_q), digits = 3))


# Plots
tws = cbind(c(terminal_wealths$MLE_classic_u),
           c(terminal_wealths$MLE_classic),
           c(terminal_wealths$MLE_exploratory),
           c(terminal_wealths$EMV))
colnames(tws) = c("MLE_classic_u", "MLE_constrained", "MLE_exploratory", "RL")

par(mai=c(1.02,1,0.4,0.3))
plot_terminalwealth(tws, plot_type = "boxplot",
                    ylim=c(-3,5), # c(min(terminal_wealths$MLE_classic ),
                           #max(terminal_wealths$MLE_exploratory)),
                    ylab="Terminal Wealth ($)", cex.lab=1.5)


par(mai=c(1.02,1,0.4,0.3))
plot_terminalwealth(tws, plot_type = "density",
                    xlim=c(min(terminal_wealths$MLE_classic ),
                           max(terminal_wealths$MLE_exploratory)),
                    ylim=c(0,6), cex.lab=1.5)


par(mai=c(1.02,1,0.4,0.3))
plot_terminalwealth(tws, plot_type = "ecdf",
                    xlim=c(-3,5), ylim=c(0,1), cex.lab=1.5)


par(mai=c(1.02,1,0.4,0.3))
for (i in 1:(ncol(tws)-1)){
  for (j in (i+1):ncol(tws)){
    if(i == 1){
      plot_terminalwealth(tws[,c(i,j)], plot_type = "qqplot",
                          xlim=c(-1000,1500), cex.lab=2)
    }
    else{
      plot_terminalwealth(tws[,c(i,j)], plot_type = "qqplot", cex.lab=2)
    }
  }
}


