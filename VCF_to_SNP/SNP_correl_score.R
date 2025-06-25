


load("obj_all.ALTREF.out")



all.ALTREF[1:10,]

ok <- t(all.ALTREF[1:5000,11:ncol(all.ALTREF)])

okcor <- cor(ok)

plot(okcor[2,])

plot(apply(abs(okcor),1,quantile,0.95) )



dim(all.ALTREF)


q.snpcor <- matrix(NA,nrow = nrow(all.ALTREF),ncol=4)

stepsize <- 500

for( i in 1:100){
ok <- t(all.ALTREF[((i-1)*stepsize+1):((i*stepsize)+stepsize),11:ncol(all.ALTREF)])
okcor <- cor(ok)
q.snpcor[((i-1)*stepsize+1):(i*stepsize),] <- t(apply(abs(okcor),1,quantile,c(0.5,0.90,0.95,0.99)) )[1:stepsize,]
}
plot(q.snpcor[1:50000,3])




apply(abs(okcor),1,quantile,c(0.5,0.90,0.95,0.99))[,1:4]


q.snpcor <- matrix(NA,nrow = nrow(all.ALTREF),ncol=4)

stepsize <- 500

for( i in 1:4982){
  rstart <- ((i-1)*stepsize+1)
  rstop <- ((i*stepsize)+stepsize)
  
  ok <- t(all.ALTREF[rstart:rstop,11:ncol(all.ALTREF)])
  okcor <- cor(ok)
  
  q.snpcor[(rstart + 0.5*stepsize):(rstop - 0.5*stepsize),] <- t(apply(abs(okcor),1,quantile,c(0.5,0.90,0.95,0.99)) )[(stepsize*0.5+1):(stepsize*1.5),]
print(i)
  }
plot(q.snpcor[1:50000,4])

save(q.snpcor,file = "obj_q.snpcor.out")

dim(all.ALTREF)/500

all.ALTREF[q.snpcor[,4]<0.2 & !is.na(q.snpcor[,4]),"POS"]

all.ALTREF[all.ALTREF$POS == 68022507,]


#################### END #########################################################3