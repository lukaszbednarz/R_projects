# printTransactions.r
# 
# Function for HackerRank https://www.hackerrank.com/challenges/stockprediction challenge
# last updated: March 24 2016 Lukasz Bednarz
#
# Functions:
#	1. printTransactions  			takes a float m money left, an integer k number of stocks, 
#                             an integer d days left, a string array name of stock names, 
#                             an integer array owned of stocks owned, and an 2d integer 
#                             array prices of stock prices containing k arrays of length 5 
#                             and print your output.
#

printTransactions <-
function(m, k, d , name, owned, prices) {
  portfolio<-list("m" = m,
                  "k" = k,
                  "d" = d,
                  "name" = name,
                  "owned" = owned,
                  "prices" = prices)
  
  #call<- match.call()
  
  # read back from storage file
  if (file.exists('hist_prices.csv')){
    Data <- read.csv('hist_prices.csv')
    Data <- rbind(Data, portfolio$prices[,tail(1)])
    write.csv(Data, 'hist_prices.csv')
  }
  else {
    Data <- data.frame(t(prices))
    names(Data)<-name
    rownames(Data) <- c(1:nrow(Data))
    write.csv(Data, 'hist_prices.csv')
  }
  

  portfolio.new<-optimizePortfolio(Data, portfolio)
  
  result<-prepareOutput(portfolio, portfolio.new)
  
  writeLines(result)
  
  result
}

###################
# prepareOutput(portfolio.old, portfolio.new)
###################

prepareOutput <-
function(old_p, new_p) {
  
  delta<-new_p$owned - old_p$owned
  
  orders<-new_p$name[delta != 0]
  
  outp<-paste0(length(orders),"\n",collapse="")
  
  for (tick in orders) {
    cmd<-"BUY"
    if (delta[tick] < 0){
      cmd<-"SELL"
    }
    outp<-paste0(outp,paste(tick, cmd, delta[tick] ), "\n", collapse="")
  }
  
  outp
}

optimizePortfolio <-
function (data, portf) {
  
  ### Calculate covariance matrix

  stox.df.logd <- log(data[2:nrow(data),]) - log(data[1:nrow(data)-1,])
  
  stox.ret<-colMeans(stox.df.logd)
  target.ret = max(stox.ret)
  
  stox.cov<-cov(stox.df)
  
  price.current<-data[nrow(data),]
  
  # calculate current portfolio value of holdings
  w.current <- portfolio$owned*price.current
  
  # total portfolio value
  net.portf <- sum(w.current)
  
  w.current <- w.current/(net.portf + portf$m)

  # computing efficient portfolio
  w <- efficientPortfolio(stox.cov, stox.ret, price.current, net.portf + portf$m, portf$name )
  
  #  trades in terms of number of shares
  delta <- round((w - w.current)*(net.portf+portf$m)/price.current,0)
  
  # action
  
  portf.new <- portf
  
  portf.new$owned <- portf$owned + delta
  
  portf.new
}

efficientPortfolio <-
function(stox.cov, stox.ret, prices, totalValue,stox.names ){
  
  library(quadprog)
  # As there is no shorts
  # this is QP problem: min(-d' b + 1/2 b' D b) with the constraints A' b >= b_0.
  # in our case A consist of rules: weights'*returns = alpha_0 weights > = zeros(mx1)
  # weights'*ones(mx1) = 1
  # solve.QP(Dmat, dvec, Amat, bvec, meq=0, factorized=FALSE)
  N_stock<-nrow(stox.cov)
  
  risk.prem <- seq(0, 2, 0.1)
  
  meq <- 1
  Dmat <- 2*stox.cov
  
  # conditions for equality
  bvec.eq <- 1
  Amat.eq <- rep(1, N_stock)
  
  
  bvec.ineq <- rep(0, N_stock)
  Amat.ineq <- cbind(diag(N_stock),-diag(N_stock))
  
  
  ## conditioning matrix for stock due to granularity and price
  ## the weights have to be less than weight of max integer number of shares
  ## that can be used for given portfolio net value
  for (ii in 1:N_stock) {
    bvec.ineq <-c(bvec.ineq, -floor(totalValue/prices[ii])*prices[ii]/totalValue)
  }
  
  eff <- matrix(nrow=length(risk.prem), ncol=N_stock+3)
  colnames(eff)<-c(stox.names,"Std.Dev","Exp.Return","Sharpe")
  
  sharpe.max <- 0
  prem.max <- 1
  loop.max<-1
  loop <- 1
  
  
  # find approximate efficient portfolio with max sharpe
  for (ii in 1:length(risk.prem)) {
  
    dvec <- stox.ret*risk.prem[ii]
    Amat <- cbind(Amat.eq, Amat.ineq)
    bvec <- c(bvec.eq, bvec.ineq)
  
    result <- solve.QP(Dmat=Dmat,dvec=dvec,Amat=Amat,bvec=bvec,meq=meq)
    eff[loop,"Std.Dev"] <- sqrt(sum(result$solution *colSums((stox.cov * result$solution))))
    eff[loop,"Exp.Return"] <- as.numeric(result$solution %*% stox.ret)
    eff[loop,"Sharpe"] <- eff[loop,"Exp.Return"] / eff[loop,"Std.Dev"]
    eff[loop,1:N_stock] <- result$solution
    
    if (eff[loop,"Sharpe"] > sharpe.max){
      sharpe.max<-eff[loop,"Sharpe"]
      prem.max<-risk.prem[ii]
      loop.max<-ii
    }
    # increment loop counter
    loop <- loop+1
    
  }

  w.sharpe <- eff[loop.max,1:N_stock] 
  
  prices.sorted<-sort(as.numeric(prices), index.return=TRUE, decreasing = TRUE)
  prices.sorted.ix<-prices.sorted$ix[prices.sorted$x<=totalValue]
  # vector fixing stock values starting from most expensive
  # then continuing optimizing portfolio down to least expensive
  b.eq<-w.sharpe
  
  opt <- matrix(nrow=length(prices.sorted.ix), ncol=N_stock+3)
  colnames(opt)<-c(stox.names,"Std.Dev","Exp.Return","Sharpe")
  loop <-1
  
  while (loop < length(prices.sorted.ix)) {
    ii <- prices.sorted.ix[loop]
    meq <- meq +1
    Amat.vec<-rep(0,N_stock)
    Amat.vec[ii]<-1
    Amat.eq <-cbind(Amat.eq, Amat.vec)
    b.eq[ii]<-min(floor(totalValue/prices[ii])*prices[ii]/totalValue,
                  round(totalValue/prices[ii]*b.eq[ii]))
    bvec.eq<-c(bvec.eq,b.eq[ii])
    
    bvec<-c(bvec.eq,bvec.ineq)
    Amat<-cbind(Amat.eq, Amat.ineq)
    
    result <- solve.QP(Dmat=Dmat,dvec=dvec,Amat=Amat,bvec=bvec,meq=meq)
    opt[loop,"Std.Dev"] <- sqrt(sum(result$solution *colSums((stox.cov * result$solution))))
    opt[loop,"Exp.Return"] <- as.numeric(result$solution %*% stox.ret)
    opt[loop,"Sharpe"] <- eff[loop,"Exp.Return"] / eff[loop,"Std.Dev"]
    opt[loop,1:N_stock] <- result$solution
    
    # increment loop
    loop <- loop+1
  }
  
  result$solution
}
