##Portfolio construction and optimization based on Sector Indices (ETF)
## Basic Equity sector example
library(pacman)
p_load(Quandl, quantmod, zoo ,PerformanceAnalytics, PortfolioAnalytics, quadprog, DEoptim, corrplot ) # Port Packages
p_load(readxl, plotly, dplyr)
# Bloomberg data download to CSV
symbol_list = read.csv.zoo("data/AssetClasses.csv", header=TRUE, sep = ",", format="%d/%m/%Y")
tickers = names(symbol_list)
colnames(symbol_list) = c("TotalMarket", "MidCap", "SmallCap", "TR_ST", "TotalBond", "TR_LT20", "TIPS", "Muni", 
                          "FTSE_AW", "FTSE_FSCap", "FTSE_EM", "REIT", "DB_COM", "Gold")


AR= read_excel("data/CMAssumptions.xlsx", sheet = "USD", range = "B5:g27",
                  col_types = c(rep("text", 2), rep("numeric",4)))%>%
  setNames(make.names(colnames(.)))%>%
  select(Asset.Class, Asset, ER, SD, pEq, pFI)%>%
  mutate_each(funs(round(.,2)), -Asset.Class, -Asset)
  

plot_ly(data = AR, type = "scatter", mode= "markers",
        x = ~ SD, y = ~ ER, color = ~ Asset.Class, 
        text   = ~ str_c("", Asset, "
                         ",
                         "Asset Class: ", Asset.Class, "
                         ",
                         "Corr Eq: ", pEq, "
                         ", 
                         "Corr FI: ", pFI))%>%
layout(title   = 'Asset Class Risk vs Reward',
      xaxis   = list(title = 'Risk (Annualized StDev)',
                     dtick = 0.01),
      yaxis   = list(title = 'Reward (Mean Log Returns)',
                     dtick = 0.01),
      margin = list(l = 100,
                    t = 100,
                    b = 100))
#install.packages("ROI.plugin.glpk", dependencies, TRUE)
#install.packages("ROI.plugin.quadprog")
library(ROI.plugin.glpk)
library(ROI.plugin.quadprog)       

assetRet = as.zoo(apply(symbol_list,2, function(x) round(diff(log(x)),2)), index(symbol_list[2:nrow(symbol_list)])) 

saveRDS(assetRet, file = "assetRet.RDS")
minRet = 0.06/100
meanRet = colMeans(assetRet, na.rm=T) #Return.annualized(assetRet) 
corr = corrplot(cor(assetRet), method = "square", type="lower", tl.pos="top", tl.cex=0.6, title = "Asset Class correlations")
corr = corrplot(cor(assetRet), add = TRUE, method = "number", type="upper", 
                diag=FALSE,tl.pos="n", cl.pos="n", number.digits = 1, number.cex=0.6)
ls()

table.AnnualizedReturns(assetRet, Rf = 0.03/12, digits = 2, geometric = TRUE)
# table.CAPM(assetRet[,2:14], assetRet[,1], Rf=0.03/12, digits=2)
 chart.RiskReturnScatter(assetRet)

#Standard Return and Risk optimization, random solver
port = portfolio.spec(assets=colnames(assetRet))
#port = add.constraint(port, type="group",
#                     groups = list(groupA=c(1,2), groupB= c(4,5,6,8), groupC=c(7),groupD = c(12,13,14),
#                     group_min = c(0, 0, 0.1, 0.4), group_max = c(0.20, 0.20, 0.25, 0.75))
#port = add.constraint(port, type = "long_only")
port = add.constraint(port, type = "weight_sum", min_sum = 0.90, max_sum = 1.20)


#rportfolios = random_portfolios(port, permutations = 10000, rep_method =sample, eliminate = TRUE)
#ROI approach chosen due to group constrains, which invalidates the random portfolio approach

MinVport = add.objective(port, type="risk", name = "StdDev")
MinVport = add.objective(MinVport, type="return", name = "mean")
MinVport = add.constraint(MinVport, type="box", min = 0, max = 0.25)
minVport.opt = optimize.portfolio(assetRet, MinVport, optimize_method = "ROI", trace=TRUE)

MaxRport = add.objective(port, type="return", name = "mean")
MaxRport = add.constraint(MaxRport, type="box", min = 0, max = 0.50)
maxRport.opt = optimize.portfolio(assetRet, MaxRport, optimize_method = "ROI", trace = TRUE)

plot(minVport.opt, risk.col="var", chart.assets=TRUE, main="Min Variance Optimization")
plot(maxRport.opt, risk.col="var", chart.assets=TRUE, main="Max Return Optimization")

###############
##Quadractic Utility optimization, using 0.5 risk aversion scale
Qut = portfolio.spec(assets=colnames(assetRet))
constraints = list(
  fi_constr = weight_sum_constraint(type="full_investment"),
  lo_constr = box_constraint(type="box", assets = Qut$assets, min = 0.00, max = 0.25))
objective = list(ret_obj = return_objective(name="mean"),
                 var_obj = portfolio_risk_objective(name="var", risk_aversion=0.25))
#Quadratic Utility = U(W) = W - bW^2, U'=1-2bW, U" = -2b
Opt.Qut=optimize.portfolio(R=assetRet, portfolio=Qut, constraints = constraints, objectives = objective, 
                           trace=TRUE, optimize_method = "ROI")

#Chart weights and risk-reward
plot(Opt.Qut, risk.col="var", chart.assets=TRUE, main="Max Utility Optimization")


# ##### THEMATIC EXPOSURE
port_weights = data.frame(Assets = c("TotalMarket", "MidCap", "SmallCap", "TR1_3", "TotalBond", "TR20", "TIPS", "Muni", 
                     "FTSE_AW", "FTSE_AWSmC_ex_US", "FTSE_EM", "REIT", "DB_COM", "Gold"),
                     Theme = as.numeric(c(-0.25, 0.10, 0.15, 0.00, 0.00, 0.00, 0.15, 0.00, 0, 0.15, 0.15, 0.15, 0.20, 0.15)),
                     MinVar = round(minVport.opt$weights,2), MaxRet = maxRport.opt$weights
                     )
port_weights$Target = round((3*port_weights$Theme+ port_weights$MinVar+ port_weights$MaxRet)/5,2)



##BackTest
MinVBT = optimize.portfolio.rebalancing(assetRet,MinVport,rebalance_on = 'years',
                                          training_period = 36, optimize_method = "ROI")
MinVPret=Return.rebalancing(R=assetRet, weights=extractWeights(MinVBT))
colnames(MinVPret)=c('MinSD')

MaxRBT = optimize.portfolio.rebalancing(assetRet,MaxRport,rebalance_on = 'years',
                                        training_period = 36, optimize_method = "ROI")
MaxRPret=Return.rebalancing(R=assetRet, weights=extractWeights(MaxRBT))
colnames(MaxRPret)=c('MaxR')


EWPret = Return.rebalancing(R=assetRet)
colnames(EWPret)=c('EqualWeight')

PTComparison=merge.xts(MinVPret, MaxRPret, EWPret, EQ.BM = assetRet[,1])
chart.CumReturns(PTComparison, main='Performance of Various Strategies', legend.loc = "left")

table.AnnualizedReturns(PTComparison)
table.CAPM(PTComparison, assetRet[,1], Rf = 0.01)
table.DownsideRisk(PTComparison, assetRet[,1], Rf = 0.01)
save(port_weights, PTComparison, file = "portWeights.RData")

