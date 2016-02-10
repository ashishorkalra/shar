library(shiny)

shinyServer(function(input, output) {
  
  # builds a reactive expression that only invalidates 
  # when the value of input$goButton becomes out of date 
  # (i.e., when the button is pressed)
  ntext <- eventReactive(input$goButton, {
    input
  })
  
  
  output$table <- renderTable({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    inp = ntext()
    inFile <- inp$file1
    
    if (is.null(inFile))
      return(NULL)
    
    read.csv(inFile$datapath, header=input$header, sep=input$sep, 
             quote=input$quote)
  })
  # Generate a plot of the data. Also uses the inputs to build
  # the plot label. Note that the dependencies on both the inputs
  # and the data reactive expression are both tracked, and
  # all expressions are called in the sequence implied by the
  # dependency graph
  output$plot <- renderPlot({
    
    inp = ntext()
    inFile <- inp$file1
    
    if (is.null(inFile))
      return(NULL)
    
    symlist.m = read.csv(inFile$datapath, header=input$header, sep=input$sep, 
             quote=input$quote)
    etfUniv = as.vector(symlist.m[,"sym"])
    
    # get start and end date of simulation
    # ETF symbols plus explaination
    etfPort = c("SHY", "Core short-term US Bond", "Fixed Income", "2002-07-22",
                "AGG", "Core Total US Bond Market",  "Fixed Income", "2003-09-22",
                "HYG", "High Yield Corporate Bond ETF", "Fixed Income", "2007-04-04",
                "SPY", "S&P 500", "Equity", "2000-07-24",
                "XLU", "Utilities Select Sector", "Equity", "1998-12-16",
                "XPH", "S&P Pharmaceuticals", "Equity", "2006-06-19", 
                "XSD", "S&P Semiconductor", "Equity", "2006-01-31",
                "XRT", "S&P Retail", "Equity", "2006-06-19",
                "XHB", "S&P Homebuilders", "Equity", "2006-01-31",
                "XOP", "Oil and Gas", "Equity", "2006-06-19")
    
    etfPort.m = matrix(etfPort, ncol=4, byrow=T)
    colnames(etfPort.m) = c("sym", "desc", "type", "start")
    portfolioEndDate = inp$dateRange[2]
    
    startDates = as.Date(etfPort.m[,"start"], format="%Y-%m-%d")
    # the end of the period, going backward
    startDateIx = which.max(startDates)
    startDate = as.Date(startDates[startDateIx])
    endDate = findMarketDate(as.Date(portfolioEndDate))
    
    t = getCloseData(etfUniv, startDate, endDate)
    etfClose = data.frame(t)

    etfRet = apply(etfClose, 2, FUN=function(v) {calcRet(v)})
    
    # Maximum gain-loss portfolio
    port.l = backtestPortfolio( etfRet, portfolioWeights, gainLossRatio, emaMean, emaWin=12, adj=0.3 )
    portRet = calcBacktestRet(port.l, etfClose)
    
    # Maximum Sharpe ratio portfolio
    portSharpe.l = backtestPortfolio( etfRet, portfolioWeights, sharpeRatio, emaMean, emaWin=12, adj=0.3)
    portRetSharpe = calcBacktestRet(portSharpe.l, etfClose)
    
    sp500Sym = "^GSPC"
    benchQtrClose = getBenchmark( sp500Sym, port.l, etfClose)
    benchRet = calcRet(benchQtrClose)
    
    maxRetData = cbind(portRet, portRetSharpe, as.numeric(benchRet))
    colnames(maxRetData) = c("gain-loss", "Sharpe", "^GSPC")
    par(mfrow=c(1,1))
    chart.CumReturns(maxRetData, wealth.index=T, main="Maximum Gain-loss and Sharpe Ratio portfolios, with ^GSPC", 
                     col=c("blue", "magenta", "red"), legend.loc="topleft", 
                     lwd=4, lty = c(2, 4, 1), grid.color="slategrey")
    abline(h = 1, col="grey30")
    par(mfrow=c(1,1))

  })
  
  # Generate a summary of the data
  output$summary <- renderTable({

    inp = ntext()
    inFile <- inp$file1
    
    if (is.null(inFile))
      return(NULL)
    
    symlist.m = read.csv(inFile$datapath, header=input$header, sep=input$sep, 
                         quote=input$quote)
    etfUniv = as.vector(symlist.m[,"sym"])
    
    # get start and end date of simulation
    # ETF symbols plus explaination
    etfPort = c("SHY", "Core short-term US Bond", "Fixed Income", "2002-07-22",
                "AGG", "Core Total US Bond Market",  "Fixed Income", "2003-09-22",
                "HYG", "High Yield Corporate Bond ETF", "Fixed Income", "2007-04-04",
                "SPY", "S&P 500", "Equity", "2000-07-24",
                "XLU", "Utilities Select Sector", "Equity", "1998-12-16",
                "XPH", "S&P Pharmaceuticals", "Equity", "2006-06-19", 
                "XSD", "S&P Semiconductor", "Equity", "2006-01-31",
                "XRT", "S&P Retail", "Equity", "2006-06-19",
                "XHB", "S&P Homebuilders", "Equity", "2006-01-31",
                "XOP", "Oil and Gas", "Equity", "2006-06-19")
    
    etfPort.m = matrix(etfPort, ncol=4, byrow=T)
    colnames(etfPort.m) = c("sym", "desc", "type", "start")
    portfolioEndDate = inp$dateRange[2]
    
    startDates = as.Date(etfPort.m[,"start"], format="%Y-%m-%d")
    # the end of the period, going backward
    startDateIx = which.max(startDates)
    startDate = as.Date(startDates[startDateIx])
    endDate = findMarketDate(as.Date(portfolioEndDate))
    
    
    t = getCloseData(etfUniv, startDate, endDate)
    etfClose = data.frame(t)
    etfRet = apply(etfClose, 2, FUN=function(v) {calcRet(v)})
    
    
    
    
    # Maximum gain-loss portfolio
    port.l = backtestPortfolio( etfRet, portfolioWeights, gainLossRatio, emaMean, emaWin=12, adj=0.3 )
    portRet = calcBacktestRet(port.l, etfClose)
    
    # Maximum Sharpe ratio portfolio
    portSharpe.l = backtestPortfolio( etfRet, portfolioWeights, sharpeRatio, emaMean, emaWin=12, adj=0.3)
    portRetSharpe = calcBacktestRet(portSharpe.l, etfClose)
    
    sp500Sym = "^GSPC"
    benchQtrClose = getBenchmark( sp500Sym, port.l, etfClose)
    benchRet = calcRet(benchQtrClose)
    
    maxRetData = cbind(portRet, portRetSharpe, as.numeric(benchRet))
    colnames(maxRetData) = c("gain-loss", "Sharpe", "^GSPC")
    
    get.weights = function(x){
      x$indexes = paste(names(x$weights), collapse  = " ~ ")
      x$weights = paste(x$weights, collapse  = " ~ ")
      x
    }
    
    port.l.v1 = lapply(port.l, get.weights)
    portSharpe.l.v1 = lapply(portSharpe.l, get.weights)
    
    LS.df.port = as.data.frame(do.call(rbind, port.l.v1))
    LS.df.portsharpe.l = as.data.frame(do.call(rbind, portSharpe.l.v1))
    
    LS.df.port
  

    
  })
})




library(knitr)
library(tseries)
library(timeDate)
library(timeSeries)
library(zoo)
library(xts)
library(its)
library(data.table)
library(PerformanceAnalytics)
library(quadprog)
library(quantmod)
library(TTR)
library(corrplot)
library(xtable)
library(SiZer)
library(tawny)
library(tawny.types)
library(stringr)



#
# Calculate a set of portfolio returns.
#
# etfRet: a matrix of N x M returns 
#
# The function returns a list consisting of a data and the protfolio weights calculated for
# that date.
#
# func: the filter function for assets.
#
backtestPortfolio = function(etfRet, portfolioFunc, filterFunc, alphaFunc, emaWin = 12, adj = 0.10) {
  dataDates = rownames(etfRet)
  numWeeks = length(dataDates)
  endIx = rev( seq(from=numWeeks, to=53, by=-12))
  startIx = endIx - 51
  weight.l = list()
  for (i in 1:length(startIx)) {
    retBlock = etfRet[startIx[i]:endIx[i],]
    blockDates = rownames(retBlock)
    startDate = blockDates[1]
    endDate = blockDates[length(blockDates)]
    w = calcPortfolioWeights(retBlock, portfolioFunc, filterFunc, alphaFunc, emaWin, adj)
    weight.l[[i]] = list(date = endDate, weights = w)
  }
  return(weight.l)
}

#
# For a single period, calculate the portfolio weights for that period.
#
# func: the function to provide the values for filtering. For example, gainLossRatio or
# shareRatio
#
calcPortfolioWeights = function(retBlock, portfolioFunc, filterFunc, alphaFunc, emaWin = 12, adj = 0.10) {
  selectAssets = filterAssets(retBlock, filterFunc )
  if (length(selectAssets) > 1) {
    assetRet.z = zoo(retBlock[,selectAssets])
    t_port = TawnyPortfolio(assetRet.z, nrow(assetRet.z))
    S.hat = cov_shrink(t_port)
    alpha = apply(assetRet.z, 2, FUN=alphaFunc, win=emaWin)
    targetRet = max(alpha)
    targetRetAdj = targetRet - (targetRet * adj)
    wRaw = portfolioFunc(targetRetAdj, alpha, S.hat)
    # wVec = round(wRaw * 10000, -1)/10000
    wVec = roundWeights(wRaw)
    wVecTrunc = wVec[ wVec != 0]
  } else {
    wVecTrunc = 1
    names(wVecTrunc) = selectAssets
  }
  return(wVecTrunc)
}

#
# Filter assets on the basis of their gain-loss ratio and on the basis of correlation.
#
filterAssets = function( retBlock, func, n = 40 ) {
  lessCorNames = NULL
  if (n > 0) {
    assetVals = apply( coredata(retBlock), 2, FUN=func)
    names(assetVals) = colnames(retBlock)
    assetValsOrd = sort(assetVals, decreasing=TRUE) # larger is assumed to be better
    assetValsNames = names(assetValsOrd)
    lessCorNames = correlationFilter(assetValsNames, retBlock, n)
  }
  return( lessCorNames )
}

#
# Filter out ETFs that have a correlation of 0.9 or more with other ETFs.
# By filtering out assets with high correlation, assets in the same sector
# will tend to be filtered out.
#
correlationFilter = function(glNames, retBlock, n) {
  filtBlock = retBlock[,which(colnames(retBlock) %in% glNames)]
  corBlock = cor(filtBlock)
  filtNames = glNames
  ix = 1
  while (ix < length(filtNames)) {
    curName = filtNames[ix]
    end = length(filtNames)
    others = filtNames[(ix+1):end]
    corLine = corBlock[curName, others]
    remove = corLine[corLine >= 0.90]
    if (length(remove) > 0) {
      removeNames = names(remove)
      removeIxRaw = which(filtNames %in% removeNames)
      removeIx = removeIxRaw[ removeIxRaw > ix ]
      if (length(removeIx) > 0) {
        filtNames = filtNames[-removeIx]
      }
    }
    ix = ix + 1
  }
  end = min(n, length(filtNames))
  rsltNames = filtNames[1:end]
  return(rsltNames)
}


#
# the port.l has a set of portfolios (ETF symbols and weights). The associated date is the
# date that the portfolio will be bought and the date when the previous portfolio will be sold.
#
calcBacktestRet = function(port.l, etfClose) {
  buyDates = as.Date(sapply(port.l, FUN=function(e) {e$date}))
  closeDates = as.Date(as.vector(rownames(etfClose)))
  closeIx = which(closeDates %in% buyDates)
  port.lAdj = port.l
  if ((max(closeIx) + 12) <= length(closeDates) ) {
    closeIx = c(closeIx, (max(closeIx) + 12))
  } else {
    port.lAdj = port.l[-length(port.l)]
  }
  qtrClose = etfClose[closeIx,]
  qtrRet = apply(qtrClose, 2, FUN=function(v) {calcRet(v)})
  qtrDates = rownames(qtrRet)
  portRet = rep(0, length(port.lAdj))
  names(portRet) = qtrDates
  for (i in 1:length(port.lAdj)) {
    elem = port.lAdj[[i]]
    wts = elem$weights
    investDate = elem$date
    syms = names(wts)
    sellDate = qtrDates[i]
    r = qtrRet[i,syms]
    r_p = r %*% wts
    portRet[i] = r_p
  }
  return(portRet)
}

getBenchmark = function(benchSym, port.l, etfClose) {
  buyDates = as.Date(sapply(port.l, FUN=function(e) {e$date}))
  closeDates = as.Date(as.vector(rownames(etfClose)))
  closeIx = which(closeDates %in% buyDates)
  if ((max(closeIx) + 12) <= length(closeDates) ) {
    closeIx = c(closeIx, (max(closeIx) + 12))
  }
  qtrDates = closeDates[closeIx]
  startDate = qtrDates[1]
  endDate = qtrDates[ length(qtrDates) ]
  benchCloseData = getCloseData(benchSym, startDate, endDate)
  benchQtr = NULL
  if ((! is.null(benchCloseData)) && length(benchCloseData) > 0) {
    benchCloseDates = as.Date(index(benchCloseData))
    benchIx = which(benchCloseDates %in% qtrDates)
    benchQtr = benchCloseData[benchIx]
  }
  return(benchQtr)
}

#
# retTarget - the target return
# r_p - a vector of forecasted returns
# retMat.z - an n x m matrix (52 x 7, for seven assets and 52 weeks)
#
# The function returns the portfolio weights or null
#
portfolioWeights = function(retTarget, r_p, S.hat )
{
  numCols = ncol(S.hat)
  D = 2 * S.hat
  one = rep(1, numCols)
  diagOne = diag(numCols)
  A = cbind(one, r_p, diagOne)
  zero = rep(0, numCols)
  b0 = c(1, retTarget, zero)
  d = zero
  w = NULL
  try(
    {
      solveRslt = solve.QP(Dmat = D, dvec=d, Amat=A, bvec=b0, meq=2)
      w = round(solveRslt$solution, 6)
      names(w) = colnames(S.hat)
    }, silent = TRUE)
  return(w)
}


## filter functions : currently just gain and loss and sharpe ratio (which may be modified sharpe ratio)
gainLossRatio = function( retVec ) {
  n = length(retVec)
  bl_ratio = (sum(pmax(retVec, 0))/n) / (sum(pmax(0 - retVec, 0))/n)
  return(bl_ratio)
}

# 
# This is a modified version of the Sharpe Ratio, where the benchmark is not subtracted from 
# the asset/portfolio return. Subtracting the risk-free rate in an era of low interst rates would
# not make much of a different. Subtracting a benchmark like the S\&P 500 would give bond funds a
# negative Sharpe ratio which would cause problems in portfolio optimization.
# 
sharpeRatio = function( retVec) {
  sharpe = mean(retVec) / sd(retVec)
  return(sharpe)
}


## alpha funcs
emaMean = function(v, win=12) {
  ema = EMA(v, n = win)
  est = ema[ length(ema)]
  return(est)
}


# function that return close data for the vector of symbols
getCloseData = function(symbols, startDate, endDate, period="w")
{
  closeData.z = c()
  firstTime = TRUE
  minDate = c()
  maxDate = c()
  fetchedSyms = c()
  for (i in 1:length(symbols)) {
    sym = symbols[i]
    symClose.z = NULL
    timeOut = 1
    startDate.ch = as.character( findMarketDate(as.Date(startDate)))
    endDate.ch = as.character( findMarketDate(as.Date(endDate)))
    while ((timeOut < 7) && is.null(symClose.z)) {
      try(
        (symClose.z = get.hist.quote(instrument=sym, start=startDate.ch, end=endDate.ch, quote="AdjClose",
                                     provider="yahoo", compression=period, retclass="zoo", quiet=T)),
        silent = TRUE)
      endDate.ch = as.character( findMarketDate( (as.Date(endDate) - 1)))
      timeOut = timeOut + 1
    }
    if (! is.null(symClose.z)) {
      fetchedSyms = c(fetchedSyms, sym)
      dateIx = index(symClose.z)
      if (firstTime) {
        closeData.z = symClose.z
        firstTime = FALSE
        minDate = min(dateIx)
        maxDate = max(dateIx)
      } else {
        minDate = max(minDate, min(dateIx))
        maxDate = min(maxDate, max(dateIx))
        matIx = index(closeData.z)
        repeat {
          startIx = which(matIx == minDate)
          if (length(startIx) > 0 && startIx > 0) {
            break()
          } else {
            minDate = minDate + 1
          }
        }
        endIx = which(matIx == maxDate)
        matIxAdj = matIx[startIx:endIx]
        closeData.z = cbind(closeData.z[matIxAdj,], symClose.z[matIxAdj])
        
      }
    }
  }
  if (length(closeData.z) > 0) {
    colnames(closeData.z) = fetchedSyms
  }
  return( closeData.z )
}

#
# Find the nearest market date (moving backward in time)
#
findMarketDate = function( date )
{
  while(! isBizday(x = as.timeDate(date), holidays=holidayNYSE(as.numeric(format(date, "%Y"))))) {
    date = date - 1
  }
  return(date)
}


# calculate return of zoo object
roundWeights = function(wtsRaw) {
  wVec = round(round(wtsRaw * 1000, -1)/1000, 2)
  return(wVec)
}

calcRet = function( v ) {

  n = length(v)
  val = as.numeric(v)
  R = (val[2:n]/val[1:(n-1)]) - 1
  if (class(v) == "zoo") {
    R = zoo(R)
    index(R) = index(v)[2:n]
  } else {
    names(R) = names(v)[2:n]
  }
  return(R)
}