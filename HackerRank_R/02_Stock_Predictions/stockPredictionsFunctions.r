cleanStart <-
function(fun = printTransactions) {
  source("printTransactions.r")
  #source("stockPredictionsFunctions.r")
  #debug(printTransactions)
  #debug(efficientPortfolio)
  debug(fun)
  if (file.exists("hist_prices.csv")){
    file.remove("hist_prices.csv")
  }
}

getHistData <-
function(fileName,k) {
  # read data from file
  #
  # inputs:
  # fileName		string with filename
  # k           number of stocks
  #
  con=file(fileName,open="r")
  
  inp=readLines(con)
  close(con)
  
  # read length of days
  N_days = length(strsplit(inp[2],"\\s+",perl = TRUE)[[1]])-1
  stox.price<-matrix(nrow = N_days,ncol = k)
  stox.tag<-vector(length = k)
  
  
  for (i in 1:k) {
    row<-strsplit(inp[i+1],"\\s+",perl = TRUE)[[1]]
    
    stox.price[,i]<-t(t(as.numeric(row[2:length(row)])))
    stox.tag[i]<-row[1]
    
  }
  
  stox.df<-data.frame(stox.price)
  names(stox.df)<-stox.tag
  rownames(stox.df) <- 1:nrow(stox.df)
  
  stox.df
}

executeTrans <-
function( portf, trans) {
  
  inp.lines<-strsplit(trans,"\\n",perl = TRUE)[[1]]
  N_trans<-as.numeric(inp.lines[1])
  # update days
  portf$d <- portf$d -1
  
  # read data from transaction and update m
  if (N_trans > 0) {
    for (ii in 2:(N_trans+1)) {
      elements<-strsplit(inp.lines[ii],"\\s",perl = TRUE)[[1]]
      tick<-elements[1]
      cmd<-elements[2]
      val<-as.numeric(elements[3])
      idx<-match(tick, portf$name)
      
      if (cmd == "BUY"){
        portf$owned[idx] <- portf$owned[idx] + val
        portf$m <- portf$m - val*portf$price[idx,ncol(portf$price)]
      }
      else {
        portf$owned[idx] <- portf$owned[idx] - val
        portf$m <- portf$m + val*portf$price[idx,ncol(portf$price)]
      }
    }
  } # end if (N_trans > 0)
  
  portf
}