rm(list=ls())

#####################################################################################
#distribution of end-of-period wealth assuming constant deposit levels each month 
#####################################################################################

##############################
# define parameters
##############################
mu <- 0.14 #drift parameter in annualized terms
sigma <- 0.2 #annual vol
dt <- 1/12 # monthly time periods
V0 <- 100 # initial value of wealth V(0)
nsims <- 15000 # number of value paths to generate
nsteps <- 60 # 60 months, 5 years
dep <- 5 # deposit level each month

##############################
# Generate sample paths for wealth 
# Assume returns follow a Geometric Brownian Motion process 
# log(V(t+dt)/V(t)) = (mu - 0.5*(sigma^2))*dt + e * sigma*sqrt(dt))
# e ~ N(0,1)
# Deposits are incorporated! See code below.
##############################

nu <- mu - 0.5*(sigma^2)
# simulate nsims return paths over the full 60 month horizon
# returns follow a log-normal (Geometric Brownian Motion) process
set.seed(2) #set seed for random number generation so results are replicable
mat <- matrix(rnorm(n = nsims*nsteps, mean = nu*dt, sd = sigma*sqrt(dt)), 
              nrow = nsims, ncol=nsteps) 

V.t.with.dep <- cbind(rep(V0, nsims), matrix(NA, nrow(mat), ncol(mat))) 
for (i in 2:ncol(V.t.with.dep)){
  if (i == 2){
    #period 1 return (does not incorporate deposit)
    V.t.with.dep[,i] <- V.t.with.dep[,(i-1)] * exp(mat[, (i-1)])
  } else {
    #first deposit made starting end of period 1 going forward
    #need to incorporate deposits starting with period 2 returns
    V.t.with.dep[,i] <- (V.t.with.dep[,(i-1)] + dep) * exp(mat[, (i-1)])
  }
} 

#sanity check
# value paths for wealth(excluding deposits)
# V.t <- V0*cbind(rep(1, nsims), t(apply(exp(mat), 1, cumprod)))
# all.equal(c(log(V0), log(V0) + cumsum(mat[105,])), log(V.t[105,]))
# matplot(x=0:nsteps, y=t(V.t), type="l",
#         col=rgb(0,0,100,50,maxColorValue=255),
#         lty=1, xlab="time", ylab="Wealth")


##############################
# plots and tables
# Risk is assessed based on the value of the portfolio at the end of each month 
# I plot the lines that connect the 95th percentile, 50th percentile, and 5th percentile
# of the wealth value distribution for each period
##############################

png(file = "WealthScenarios.png", width = 7, height = 5, units = "in", res = 300)
matplot(x=0:nsteps, y=t(V.t.with.dep), type="l",
        col=rgb(0,0,100,50,maxColorValue=255),
        lty=1, xlab="Months", ylab="Wealth($)",
        main="Simulated Growth Paths for Wealth Over 5 Years \n(Includes Regularly Scheduled Deposits)",
        cex.main = 0.95, yaxt="n")
axis(2, las=2)
lines(x=0:nsteps, y=apply(V.t.with.dep, 2, quantile, 0.95), col="green", lwd=2)
lines(x=0:nsteps, y=apply(V.t.with.dep, 2, quantile, 0.5), col="grey", lwd=2)
lines(x=0:nsteps, y=apply(V.t.with.dep, 2, quantile, 0.05), col="red", lwd=2)
legend("topleft", c("95th Percentile", "50th Percentile", "5th Percentile"), bty="n", 
       lty = 1, col=c("green", "grey", "red"), lwd=2, cex=0.95)
dev.off()



#strong market (95% quantile)
q95.i <- which.min(abs(V.t.with.dep[,61] - quantile(V.t.with.dep[,61], 0.95)))
# weak market (5% quantile)
q05.i <- which.min(abs(V.t.with.dep[,61] - quantile(V.t.with.dep[,61], 0.05)))
# typical market (Median, 50% quantile)
qMed.i <- which.min(abs(V.t.with.dep[,61] - median(V.t.with.dep[,61])))

png(file = "WealthScenarios.png", width = 7, height = 5, units = "in", res = 300)
plot(x=0:nsteps, y=t(V.t.with.dep[q95.i, ]), type="l", col="blue", lwd=2, 
     ylab = "Wealth ($)", xlab="Months", 
     main="Potential Growth of $100 with Regular Deposits Over 5 Years",
     cex.main = 0.95, yaxt="n")
axis(2, las=2)
lines(x=0:nsteps, y=t(V.t.with.dep[qMed.i, ]), col="black", lwd=2)
lines(x=0:nsteps, y=t(V.t.with.dep[q05.i, ]), col="red", lwd=2)
legend("topleft", c("95th Percentile", "50th Percentile", "5th Percentile"), bty="n", 
       lty = 1, col=c("blue", "black", "red"), lwd=2, cex=0.95)
dev.off()





