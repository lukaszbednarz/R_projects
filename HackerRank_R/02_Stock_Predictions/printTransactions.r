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
    Data <- rbind(Data, portfolio$prices[,ncol(portfolio$prices)])
    write.csv(Data, 'hist_prices.csv', row.names = FALSE)
  }
  else {
    Data <- data.frame(t(prices))
    names(Data)<-name
    rownames(Data) <- c(1:nrow(Data))
    write.csv(Data, 'hist_prices.csv',row.names = FALSE)
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
    outp<-paste0(outp,paste(tick, cmd, abs(delta[tick]) ), "\n", collapse="")
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
  w <- efficientPortfolio(stox.cov, stox.ret, price.current, portf$owned, portf$m, portf$name )
  
  #  trades in terms of number of shares
  delta <- round((w - w.current)*(net.portf+portf$m)/price.current,0)
  
  # action
  
  portf.new <- portf
  
  portf.new$owned <- portf$owned + delta
  
  portf.new
}

efficientPortfolio <-
function(stox.cov, stox.ret, prices, owned, cash, stox.names ){
  
  library(quadprog)
  # As there is no shorts
  # this is QP problem: min(-d' b + 1/2 b' D b) with the constraints A' b >= b_0.
  # in our case A consist of rules: weights'*returns = alpha_0 weights > = zeros(mx1)
  # weights'*ones(mx1) = 1
  # solve.QP(Dmat, dvec, Amat, bvec, meq=0, factorized=FALSE)
  net.holdings <- owned*prices
  net.holdings.total <- sum(net.holdings)
  net.assets.total <- net.holdings.total + cash
  
  # maximum total of shares in portfolio
  stox.allocation <- 0.7
  
  N_stock <- nrow(stox.cov)
  
  # risk premium range for sharpe optimalization
  risk.prem <- seq(0, 2, 0.1)
  
  
  # maximum alocations for stocks
  alloc.max <- as.numeric(rep(0,N_stock))
  
  ## the weights have to be less than weight of max integer number of shares
  ## that can be used for given portfolio net value some stock cannot be bought because they
  ## are more expensive than whole net assets or available cash
  for (ii in 1:N_stock) {
    if (net.assets.total > 0) {
      alloc <- min(net.holdings[ii]/(net.assets.total ) + floor(cash/prices[ii])*prices[ii]/net.assets.total, stox.allocation)
      alloc <- floor(alloc*(net.assets.total )/prices[ii])*prices[ii]/(net.assets.total)
    }
    else {
      alloc <- floor(stox.allocation*cash/prices[ii])*prices[ii]/cash
    }
    
    alloc.max[ii] <- alloc
  }
  
  # sellect stocks for efficient portfolio 
  alloc.max.num.shares <- round(as.numeric(alloc.max)*net.assets.total/prices)
  stox.eff.ix <- c(1:N_stock)[alloc.max.num.shares > 0]
  stox.names.eff <- stox.names[stox.eff.ix]
  prices.eff <- prices[stox.eff.ix]
  stox.ret.eff <- stox.ret[stox.eff.ix]
  stox.cov.eff <- stox.cov[stox.eff.ix, stox.eff.ix]
  alloc.max.eff <- alloc.max[stox.eff.ix]
  
  # number of stock selected for optimization
  N_eff <- length(stox.eff.ix)
  
  w.opt <- alloc.max
  
  # optimize only if more than one stock can be bought/is hold
  if (N_eff > 1 & sum(as.numeric(alloc.max.eff)) > stox.allocation) {
    
   
    
    # reduced covariance matrix
    Dmat <- 2*stox.cov.eff
    
    # conditions for equality
    bvec.eq <- stox.allocation # we want to keep 30% of cash free for liquidity
    Amat.eq <- rep(1, length(N_eff))
    
    # number of equality conditions
    meq <- 1
    
    # conditions for inequality (range of assets weights)
    bvec.ineq.low <- as.numeric(rep(0, N_eff))
    bvec.ineq.max <- -as.numeric(alloc.max.eff)
    bvec.ineq <- c(bvec.ineq.low, bvec.ineq.max)
    
    Amat.ineq.low <- diag(N_eff)
    Amat.ineq.max <- -diag(N_eff)
    
    Amat.ineq <- cbind(Amat.ineq.low, Amat.ineq.max)
    
    
    eff <- matrix(nrow = length(risk.prem), ncol = N_eff + 3)
    colnames(eff) <- c(stox.names.eff,"Std.Dev","Exp.Return","Sharpe")
    
    sharpe.max <- 0
    prem.max <- 1
    loop.max <- 1
    loop <- 1
    
    
    # find approximate efficient portfolio with max sharpe
    for (ii in 1:length(risk.prem)) {
    
      dvec <- stox.ret.eff*risk.prem[ii]
      Amat <- cbind(Amat.eq, Amat.ineq)
      bvec <- c(bvec.eq, bvec.ineq)
    
      result <- solve.QP(Dmat = Dmat,dvec = dvec,Amat = Amat,bvec = bvec,meq = meq)
      eff[loop,"Std.Dev"] <- sqrt(sum(result$solution *colSums((stox.cov.eff * result$solution))))
      eff[loop,"Exp.Return"] <- as.numeric(result$solution %*% stox.ret.eff)
      eff[loop,"Sharpe"] <- eff[loop,"Exp.Return"] / eff[loop,"Std.Dev"]
      eff[loop,1:N_eff] <- result$solution
      
      if (eff[loop,"Sharpe"] > sharpe.max) {
        sharpe.max <- eff[loop,"Sharpe"]
        prem.max <- risk.prem[ii]
        loop.max <- ii
      }
      # increment loop counter
      loop <- loop + 1
      
    }
    
    # reduced eff portfolio weights
    w.sharpe.eff <- eff[loop.max,1:N_eff]
    
    # full portfolio weights
    w.sharpe <- rep(0,N_stock)
    w.sharpe[stox.eff.ix] <- w.sharpe.eff
    
    
    # sorting prices in decreasing order
    prices.sorted <- sort(as.numeric(prices[stox.eff.ix]), index.return = TRUE, decreasing = TRUE)
    prices.sorted.ix <- prices.sorted$ix
    # vector fixing stock values starting from most expensive
    # then continuing optimizing portfolio down to least expensive
    b.eq <- as.numeric(alloc.max)
    
    opt <- matrix(nrow = length(prices.sorted.ix) - 1, ncol = N_eff + 3)
    colnames(opt) <- c(stox.names.eff,"Std.Dev","Exp.Return","Sharpe")
    loop <- 1
    
    budget <- stox.allocation
    
    while (loop < length(prices.sorted.ix)) {
      ii <- prices.sorted.ix[loop]
      meq <- meq + 1
      # vector for stock beeing fixed in Amat
      Amat.vec <- rep(0,N_eff)
      Amat.vec[ii] <- 1
      Amat.eq <- cbind(Amat.eq, Amat.vec)
      
      # filling the available budget starting from most expensive asset
      b.eq[ii] <- min(floor(net.assets.total*budget/prices.eff[ii])*prices.eff[ii]/net.assets.total,
                    round(net.assets.total/prices.eff[ii]*w.sharpe.eff[ii]))
      bvec.eq <- c(bvec.eq, b.eq[ii])
      # adjusting remaining allocation budget
      budget <- budget - b.eq[ii]
      
      if (sum(as.numeric(b.eq) >=  stox.allocation)) {
      dvec <- stox.ret.eff*risk.prem[loop.max]
      
        # removing inequality condition for fixed assets
        bvec.ineq.opt.low <- bvec.ineq.low[-c(prices.sorted.ix[0:loop])]
        bvec.ineq.opt.max <- bvec.ineq.max[-c(prices.sorted.ix[0:loop])]
        bvec <- c(bvec.eq,bvec.ineq.opt.low, bvec.ineq.opt.max)
        
        Amat.ineq.opt.low <- Amat.ineq.low[,-c(prices.sorted.ix[0:loop])]
        Amat.ineq.opt.max <- Amat.ineq.max[,-c(prices.sorted.ix[0:loop])]
        Amat <- cbind(Amat.eq, Amat.ineq.opt.low, Amat.ineq.opt.max)
        
        # solving QR problem
        result <- solve.QP(Dmat = Dmat,dvec = dvec,Amat = Amat,bvec = bvec, meq = meq)
      }
      else {
        result$solution[-c(prices.sorted.ix[0:loop])] <- 0
         
      }
      
      opt[loop,"Std.Dev"] <- sqrt(sum(result$solution *colSums((stox.cov.eff * result$solution))))
      opt[loop,"Exp.Return"] <- as.numeric(result$solution %*% stox.ret.eff)
      opt[loop,"Sharpe"] <- eff[loop,"Exp.Return"] / eff[loop,"Std.Dev"]
      opt[loop,1:N_eff] <- result$solution
      
      # increment loop
      loop <- loop + 1
    }
  
    
    w.opt <- w.sharpe
    w.opt[stox.eff.ix] <- result$solution
  }
  
  # return optimized solution.
  w.opt
  
}
