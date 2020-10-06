
library(tseries)

N = 5000
reps = 50000

AR1 = arima.sim(model = list(1,1,0), N)
plot.ts(AR1)


adf = rep(NA, reps)

for (i in 1:reps){
  adf[i] = adf.test(AR1)
}

adf1 = as.numeric(adf)

plot(density(adf1))








