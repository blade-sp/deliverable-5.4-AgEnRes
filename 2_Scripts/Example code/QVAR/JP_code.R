library(quantreg)
library(vars)
library(readxl)

#setwd

#get data
data <- read_excel("CMO-Historical-Data-Monthly.xlsx", sheet = "Monthly Prices")
data1 <- read_excel("CMO-Historical-Data-Monthly.xlsx", sheet = "Monthly Indices")

gas <- as.numeric(unlist(data[378:777 , 9 ]))
dap <- as.numeric(unlist(data[378:777 , 59 ]))
food <- as.numeric(unlist(data1[381:780 , 7 ]))

dat <- cbind(gas, food, dap)

#choose lags and estimate VAR
a <- VARselect(dat, lag.max = 10, type = "const")

summary(nvar <- VAR(dat, p=3))

nn <- nrow(dat) #number of observations
nL <- 4 # trim for lags
n <- nn-3

#create variables
pg  <- dat[nL    :nn    ,1]
pg1 <- dat[(nL-1):(nn-1),1]
pg2 <- dat[(nL-2):(nn-2),1]
pg3 <- dat[(nL-3):(nn-3),1]
pd  <- dat[nL    :nn    ,3]
pd1 <- dat[(nL-1):(nn-1),3]
pd2 <- dat[(nL-2):(nn-2),3]
pd3 <- dat[(nL-3):(nn-3),3]
pf  <- dat[nL    :nn    ,2]
pf1 <- dat[(nL-1):(nn-1),2]
pf2 <- dat[(nL-2):(nn-2),2]
pf3 <- dat[(nL-3):(nn-3),2]

#define and estimate VAR
mg <- pg ~ pg1 + pd1 + pf1 + pg2 + pd2 + pf2 + pg3 + pd3 + pf3
md <- pd ~ pg1 + pd1 + pf1 + pg2 + pd2 + pf2 + pg3 + pd3 + pf3
mf <- pf ~ pg1 + pd1 + pf1 + pg2 + pd2 + pf2 + pg3 + pd3 + pf3

#select relevant quantiles
aa <- c(.1, .3, .5, .7, .9)
nqa <- length(aa)
#quantile regression

qga <- rq(mg, tau = aa)
qda <- rq(md, tau = aa)
qfa <- rq(mf, tau = aa)
#bootstrap standard errors
sum1 <- summary(qga, se = "boot", brmethod="xy" )
sum2 <- summary(qda, se = "boot", brmethod="xy" )
sum3 <- summary(qfa, se = "boot", brmethod="xy" )
sum1
sum2
sum3


# goodness of fit
fits1 <-rq(mg, tau = aa)
fits0 <-rq(pg~1, tau = aa)
rho <- function(u,tau=.5)u*(tau - (u < 0))
R1g <- 1 - fits1$rho/fits0$rho
fits1 <-rq(md, tau = aa)
fits0 <-rq(pd~1, tau = aa)
R1f <- 1 - fits1$rho/fits0$rho
fits1 <-rq(mf, tau = aa)
fits0 <-rq(pf~1, tau = aa)
R1o <- 1 - fits1$rho/fits0$rho

R1g
R1f
R1o