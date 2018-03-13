#Single Stock Monte Carlo

library(pacman)
p_load(quantmod, dplyr, zoo, ggplot)

ticker = "POT" ## Potash
getSymbols.google(ticker, env = globalenv(), from = "2007-01-01", to = "2017-05-01")
str(POT)

POT %>% chartSeries(TA = "addBBands(); addVo(); addRSI()",
                          subset = "2015/2016", theme = "white")
POT %>% chartSeries(TA = "addVo(); addRSI(); addSMA(14); addSMA(100)",
                    subset = "2015/2016", theme = "white")


log_returns = POT %>% Cl()%>% dailyReturn(type = "log")
m_log_returns = POT %>% Cl()%>% monthlyReturn(type = "log")
an_log_returns = POT %>% Cl() %>% annualReturn(type="log")


ggplot(log_returns, aes(x = daily.returns))+geom_histogram(bins = 100)+
  geom_density() + scale_x_continuous(limits = c(-.25, 0.25))+geom_rug()

##Distribution of log returns
probs = c(0.005, 0.025, 0.25, 0.5, 0.75, 0.975, 0.995)
dist_m_log_returns = m_log_returns%>%
  quantile(probs = probs, na.rm =T)%>%
  print()

dist_an_log_returns = an_log_returns%>%
  quantile(probs = probs, na.rm =T)%>%
  print()

save(POT, log_returns,m_log_returns, an_log_returns, dist_m_log_returns,
     dist_an_log_returns, probs, file = "pot_logret.RData")

#Point Esimate for mu, sd
mean_log_returns = mean(log_returns, na.rm = T); 
sd_log_returns = sd(log_returns, na.rm = T)

#Monte Carlo estimate for mu, sd
n = 252 #trading days per year
m = 1000 # simulations
mu = mean_log_returns
sigma = sd_log_returns
day = 1:n
px_init = POT$POT.Close[[nrow(POT)]]
set.seed(123)
mc_mat = matrix(nrow = n, ncol = m)

for (j in 1:m) { #columns
  mc_mat[[1,j]] = px_init
  for (i in 2:n) {
    mc_mat[[i,j]] = mc_mat[[i-1,j]]* exp(rnorm(1, mu, sigma))
  }}

#Format data
px_sim = cbind(day, mc_mat)%>%
  as_tibble()%>%
  setNames(c("Day",str_c("sim.", seq(1:m))))%>%
  gather(key = "simulation", value = "stock.px", -(Day))

ggMC = ggplot(px_sim, aes(x=Day, y = stock.px, Group = simulation))+
         geom_line(alpha=0.1)+ggtitle("Monte Carlo Simulation - 1000x 252 days")
  
probs = c(0.005, 0.025, 0.25, 0.5, 0.75, 0.975, 0.995)
start_px = filter(px_sim, Day == min(Day))
end_px = filter(px_sim, Day == max(Day))
dist_end_px = round(quantile(end_px$stock.px, probs=probs, na.rm=T),2)
dist_end_ret = quantile(log(end_px$stock.px/start_px$stock.px), probs=probs, na.rm=T)

save(dist_end_px, dist_end_ret,ggMC, file = "mc_dist.RData")

# CAGR calculations
y_hist = nrow(POT) / 252
start_hist = POT$POT.Close[[1]]
end_hist = POT$POT.Close[[nrow(POT)]]
y_sim = n/252
start_sim = end_hist
end_sim = dist_end_px[[4]]

CAGR_h = (end_hist/start_hist)^ (1/y_hist)-1
CAGR_s = (end_sim / start_sim)^(1/y_sim)-1

ret_sim = cbind(day, mc_mat)%>%
  as_tibble()%>%
  setNames(c("Day",str_c("sim.", seq(1:m))))%>%
  filter(row_number()==1 | row_number()==n())%>%
  mutate_each(funs(diff))%>%
  select(-Day)
  
quantile(as.vector(as.numeric(ret_sim[2,])))
hist(as.vector(as.numeric(ret_sim[2,])))

