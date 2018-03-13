##Portfolio construction and optimization
## Basic Equity sector example
library(pacman)
p_load(Quandl, quantmod, zoo ,PerformanceAnalytics, PortfolioAnalytics, quadprog, DEoptim, ggplot2 ) # see Quandl.com for more info

# Bloomberg data download to CSV
symbol_list = read.csv.zoo("data/sector_etf_data.csv", header=TRUE, sep = ",", format="%d/%m/%Y")
tickers = names(symbol_list)
colnames(symbol_list) = c("Financial", "Energy", "Tech", "Cons.Staples", 
                            "Cons.Discr", "Industrial", "HealthCare", "SPX")

sectors = getQuote(tickers, what = yahooQF(c("Name", "Previous Close", "Earnings/Share", "P/E Ratio", 
                                   "Dividend Yield", "Market Capitalization")))
sectors$Name = colnames(symbol_list)
sectors = sectors[2:4]
sectors$ticker = rownames(sectors)
sectors$PE = (sectors$`P. Close`/sectors$`Earnings/Share`)
gg_sector = ggplot(sectors, aes(x=Name, y = PE))+ geom_bar(stat = "identity", fill="steelblue")+
  labs(title = "US Sector valuations: Energy and Tech remain cheap")

#yahooQF()

#install.packages("ROI.plugin.glpk", dependencies, TRUE)
#install.packages("ROI.plugin.quadprog")
library(ROI.plugin.glpk)
library(ROI.plugin.quadprog)       

asset_ret = as.zoo(apply(symbol_list,2, function(x) round(diff(log(x)),2)), 
                   index(symbol_list[2:nrow(symbol_list)]))
asset_mat = asset_ret[,1:7][complete.cases(asset_ret)]
SPYReturn = asset_ret[,8]
ls()

chart.RiskReturnScatter(asset_ret, Rf = 0.03/12, add.names=TRUE, 
                       xlim = c(0.1, 0.26))
#table.CAPM(asset_ret[,1:7], asset_ret[,8], Rf=0.03, digits=2)
#chart.CumReturns(asset_mat)

##Minimum Variance portfolio - short selling not permitted
MinSD =portfolio.spec(assets=colnames(asset_mat))
MinSD = add.objective(portfolio=MinSD,type='risk', name='StdDev')
MinSD = add.constraint(portfolio = MinSD, type="full_investment")
MinSD = add.constraint(portfolio = MinSD, type="box", min=0.00, max=0.40)
Opt.MinSD=optimize.portfolio(R=asset_mat,portfolio=MinSD, trace=TRUE, optimize_method = "ROI")

##Mean Variance Portfolio - optimal return per risk
MaxSR = portfolio.spec(assets=colnames(asset_mat))
MaxSR = add.objective(portfolio = MaxSR, type="return", name="mean")
MaxSR = add.objective(portfolio = MaxSR, type="risk", name="StdDev")
MaxSR = add.constraint(portfolio = MaxSR, type="full_investment")
MaxSR = add.constraint(portfolio = MaxSR, type="box", min=0.00, max=0.40)

Opt.MaxSR = optimize.portfolio(R=asset_mat,portfolio=MaxSR, trace=TRUE, optimize_method = "ROI")

#  MaxER = portfolio.spec(assets=colnames(asset_mat))
MaxER = portfolio.spec(assets=colnames(asset_mat))
MaxER = add.objective(portfolio = MaxER, type="return", name="mean")
MaxER = add.constraint(portfolio = MaxER, type="full_investment")
MaxER = add.constraint(portfolio = MaxER, type="box", min=rep(0.00, 7), max=rep(0.40, 7))
#MaxER = add.constraint(portfolio = MaxER, type="long_only")
Opt.MaxER = optimize.portfolio(R=asset_mat,portfolio=MaxER, trace=TRUE, optimize_method = "ROI")


#Chart weights and risk-reward
minp = plot(Opt.MinSD, risk.col="StdDev", chart.assets=TRUE, main="Min SD Optimization")
maxp = plot(Opt.MaxSR, risk.col="StdDev", chart.assets=TRUE, main="Max sharpe")
maxRp = plot(Opt.MaxER, risk.col="StdDev", chart.assets=TRUE, main="Max return Optimization")


##BackTests
MinSDBT=optimize.portfolio.rebalancing(asset_mat,MinSD,rebalance_on = 'quarters',
                                       training_period = 24, optimize_method = "ROI")
MaxSRBT=optimize.portfolio.rebalancing(asset_mat, MaxSR,rebalance_on = 'quarters',
                                       training_period = 24, optimize_method = "ROI")
MaxERBT=optimize.portfolio.rebalancing(asset_mat, MaxER,rebalance_on = 'quarters',
                                      training_period = 24,optimize_method = "ROI")

MinSDPret=Return.rebalancing(R=asset_ret, weights=extractWeights(MinSDBT))
colnames(MinSDPret)=c('MinSD')
MaxSRPret=Return.rebalancing(R=asset_mat,weights=extractWeights(MaxSRBT))
colnames(MaxSRPret)=c('MeanSR')
MaxERPret=Return.rebalancing(R=asset_mat, weights=extractWeights(MaxSRBT))
colnames(MaxSRPret)=c('MeanER')
EWPret = Return.rebalancing(R=asset_mat)
colnames(EWPret)=c('EqualWeight')

PortfolioComparisonData=merge.xts(MinSDPret, MaxSRPret, MaxERPret,EWPret, SPYReturn)
chart.CumReturns(PortfolioComparisonData, main='Performance of Various Strategies', legend.loc = "right")

table.DownsideRisk(PortfolioComparisonData)
table.CAPM(PortfolioComparisonData[,1:3], PortfolioComparisonData[,4])#[c(2,6,9,10,11,12),]
table.AnnualizedReturns(PortfolioComparisonData, Rf = 0.03/12)


save(sectors, asset_ret, file = "sectors.RData")
save(Opt.MaxER,Opt.MinSD, Opt.MaxSR, PortfolioComparisonData, file = "pt.assets.RData")
