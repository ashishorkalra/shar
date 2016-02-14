library(tseries)
library(timeDate)
library(zoo)

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

strBegDte = '1950-01-03'
strEndDte = '2016-01-01'
frequency = 'd'

data.snp = getCloseData("^GSPC", strBegDte, strEndDte, frequency)


dates = index(data.snp)


# Separate Bull Bear Market Cycles and Calculate Cycle Statistics
numMinPeaktoTroughLossPercentage = 0.10
numMinTroughtoPeakGainPercentage = 0.15
numMinDuration = 30

TUH = data.snp[1]
TEL = data.snp[1]
DTUH = 1
DTEL = 1

PEL = 0
PUH = 0
DPEL = 1
DPUH = 1

srsReg = data.snp
srsReg[] = 0

for (intBar in 2 : length(data.snp))
{
  if(data.snp[intBar] > as.vector(TUH))
  {
    TUH = data.snp[intBar]
    DTUH = dates[intBar]
  }
  
  if (data.snp[intBar] < as.vector(TEL))
  {
    TEL = data.snp[intBar]
    DTEL = dates[intBar]
  }
  
  if((data.snp[intBar] == as.vector(TUH)) && (((as.vector(TUH)/as.vector(TEL)) - 1) > numMinTroughtoPeakGainPercentage) && (as.vector(DTUH - DTEL) > numMinDuration))
  {
    DPEL = dates[intBar]
    srsReg[intBar] = 1
  }

  if((data.snp[intBar] == as.vector(TEL)) && ((1 - (as.vector(TUH)/as.vector(TEL))) > numMinPeaktoTroughLossPercentage) && (as.vector(DTUH - DTEL) > numMinDuration)) 
  {
    DPUH = dates[intBar]
    srsReg[intBar] = -1
  }
    
}


zooToDf <- function(z) {
  df <- as.data.frame(z) 
  df$Date <- time(z) #create a Date column
  rownames(df) <- NULL #so row names not filled with dates
  df <- df[,c(ncol(df), 1:(ncol(df)-1))] #reorder columns so Date first
  return(df)
}

srsRegDF = zooToDf(srsReg)

plot(data.snp)
points(srsReg)

# coding convert is done..but i need to compare the data between matlab and yahoo
# i will compile the code in office and check if there is any -1
# first try to find it out for -1
# after solving this problem, i need to check the results some how