T = 50 #Observationer
N = 500 #Gentagelser


ts_all = c()


for(j in 1:N){
  y = numeric(60)
  y[1] = 0
  a = 1
  
  
  for(i in 2:length(y)){
    y[i] <- a * y[i - 1] + rnorm(1)
  }
  
  x = numeric(80)
  x[1] = mean(y)
  b = 1.02
  
  for(i in 2:length(x)){
    x[i] <- b * x[i - 1] + rnorm(1)
  }
  
  z = numeric(60)
  z[1] = mean(x)
  c = 1
  
  for(i in 2:length(z)){
    z[i] <- c * z[i - 1] + rnorm(1)
  }
  ts_all = ts(c(y,x,z))
}

plot(ts_all, type ="l")


### SADF

#sadf_dens = density(sadff$value)
#plot(sadf_dens)




vecc22 = c()
r0=20
ts_l = length(ts_all)
sadfstat11f = c()
rw_vecf =seq(from=200-r0, to= 0, by = -r0)
sadfstat11f = c()
for (rw in rw_vecf ){
  r1 = 1
  r2 = ts_l- rw
  ts_all_intf = ts_all[r1:r2]
  sadff1f= radf(ts_all_intf, lag=1)
  sadfstatf = sadff1f$adf
  sadfstat1f = print(sadfstatf)
  sadfstat11f = c(sadfstat11f ,sadfstat1f)
  vecc22 = c(vecc22, r2)
}


fsadf = cbind(sadfstat11f, vecc22)
sadfstat11f
max(sadfstat11f)
which.max(sadfstat11f)
fsadf[which.max(sadfstat11f),]



### BSADF
vecc11= c()
r0=20
ts_l = length(ts_all)
sadfstat11 = c()
rw_vec =seq(from=r0, to=200, by = r0)
sadfstat11 = c()
for (rw in rw_vec ){
  r1 =  ts_l - rw
  r2 = ts_l
  ts_all_int = ts_all[r1:r2]
  sadff1 = radf(ts_all_int, lag=1)
  sadfstat = sadff1$adf
  sadfstat1 = print(sadfstat)
  sadfstat11 = c(sadfstat11 ,sadfstat1)
  vecc11 = c(vecc11, r1)
}

bsadf_bind = cbind(sadfstat11, vecc11)

sadfstat11
max(sadfstat11)
which.max(sadfstat11)
bsadf_bind[which.max(sadfstat11),]


###GSADF
vecc1 = c()
vecc2 = c()
r0=20
r1_vec = seq(from =0, to =200-r0, by = r0)
r2_vec = seq(from=200, to= r0, by = -r0)
vec = c()
gsint_vec = c()
vec1 = c()
for (j in 1:length(r1_vec)){
  for (i in 1:length(r2_vec)){
    r1 = r1_vec[j]
    r2 = r2_vec[i]
    if (r1 < r2){ 
      gsint = ts_all[r1:r2]
      gsint1 = print(gsint[1])
      gsadf = radf(gsint,lag=1)
      statgsadf = gsadf$adf
      vec = c(vec,statgsadf)
      vecc1 = c(vecc1, r1)
      vecc2 = c(vecc2, r2)
    }
  }
}


bind  =cbind(vec, vecc1, vecc2)

length(vec)
vec
max(vec)
which.max(vec)

bind[which.max(vec),]

#gsadff = gsadf(500,150, adflag = 1, mflag = 1, swindow0 = 10)
#gsadf_dens = density(gsadff$value)
#plot(gsadf_dens)







