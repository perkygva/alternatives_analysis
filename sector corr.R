#Sector Correlation analysis with ETFs

source("load.R")
source("getstock_wrapper")
library(quantmod)

etf.tickers <- c("XLY", "XLP", "XLE", "XLF", "XLV", "XLI", "XLB", "XLK", "XLU", "SPY") 
sectors <- c("Consumer Discretionary", "Consumer Staples", 
            "Energy", "Financials", "Health Care", "Industrials", 
            "Materials", "Information Technology", "Utilities", "Index")


etfs.df = data.frame(etf.tickers, sectors = make.names(sectors))
housing.df = data.frame(housing.tickers, housing.names)

## Sectors
  symbols = getSymbols(etf.tickers, auto.assign = T, warnings = FALSE)
  px = do.call(merge, lapply(symbols,function(x) Ad((get(x)))))
  colnames(px) = etf.tickers
  px_tibble = as_tibble(px)%>%
    rownames_to_column(var = "Date")
    
  ret = do.call(merge, lapply(px, function(x) periodReturn(x, period = "weekly", type="log")))
  colnames(ret) = sectors
  ret_tibble = as_tibble(ret)%>%
    rownames_to_column(var = "Date")
 
 
###Correlation plots 
corrplot(cor(ret),order="AOE",type="upper",tl.pos="tp")
corrplot(cor(ret),add=TRUE, type="lower", method="number",order="AOE",
         diag=FALSE,tl.pos="n", cl.pos="n")

###Rolling correlation for specific sectors

  merged = etf_ret[, c(sector, "Index")]
  merged$corr = rollapplyr(merged, width = width, 
                          function(x) cor(x[,1], x[,2], use="pairwise.complete.obs"), by.column=F)
  merged$beta = rollapplyr(merged, width = width, 
                          function(x) {t = lm(x[,2]~x[,1], data = merged, na.rm=T); return(t$coef)}, by.column=F)

merged = sector_corr("Energy", 30)

data.frame(date = index(merged), merged)%>%
  group_by(yearmon = as.yearmon(date))
                

##### HOUSING
hs.tickers = c("IYR", "VRQ", "REET", "SCHH", "SPY", "DGS5" )
hs.names = c("iShares.US", "Vang_REIT", "iShares.Global", "Schwab.US", "SPY", "TR5y" )

hs.symbols = getSymbols(hs.tickers, auto.assign = T, warnings = FALSE)
hs.px = do.call(merge, lapply(hs.symbols,function(x) Cl((get(x)))))
colnames(hs.px) = hs.names
px_tibble = as_tibble(px)%>%
  rownames_to_column(var = "Date")

hs.ret = do.call(merge, lapply(hs.px, function(x) periodReturn(x, period = "weekly", type="log")))
colnames(hs.ret) = hs.names
ret_tibble = as_tibble(ret)%>%
  rownames_to_column(var = "Date")

###Correlation plots 
corrplot(cor(hs.ret),order="AOE",type="upper",tl.pos="tp")
corrplot(cor(hs.ret),add=TRUE, type="lower", method="number",order="AOE",
         diag=FALSE,tl.pos="n", cl.pos="n")
