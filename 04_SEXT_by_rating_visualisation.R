# Figure 1

library(rethinking)

M4_sext <- readRDS("post_m4_sext_Long_11_06.RDS")

A_post_tog <- M4_sext

# M3_sext <- readRDS("post_m3_sext_2.RDS")

# A_post_tog <- M3_sext


# POSTERIOR - Iris L*a*b* across cultures: 

#                 1   2   3   4   5   6   7   8
# Cameroon_2013  99   0   0   0   0   0   0   0
# Cameroon_2016   0  98   0   0   0   0   0   0
# Colombia        0   0 138   0   0   0   0   0
# Czech_2016      0   0   0 100   0   0   0   0
# Czech_2019      0   0   0   0  95   0   0   0
# Iran            0   0   0   0   0  87   0   0
# Turkey          0   0   0   0   0   0 184   0


tiff("Posterior__SEXT_Logit_Long_M4.tif",width=32,height=26,units="cm",res=600,compression = "lzw")
layout(matrix(1:4,byrow=T,nrow=1), widths=c(2.9,2,2,2),heights=1)

# 1 out 4: Iris Colour - WOMEN

par(mar=c(3.1, 10.1, 1.1, 1.1),mgp=c(5,0.7,0))
plot(NULL,xlim=c(-2,2),ylim=c(1,25.5),yaxt="n",bty="n", ylab="", xlab="", cex.axis=0.9, 
     main="PIT (Sclera) Colour - Women", cex.main=1.5)

rect(-2, 0, 2, 26, col="#10101010", border=NA)

segments(-2, c(seq(from=1, to=8, by=1),seq(from=9.5, to=16.5, by=1),seq(from=18, to=25, by=1)), 2, 
         c(seq(from=1, to=8, by=1),seq(from=9.5, to=16.5, by=1),seq(from=18, to=25, by=1)), col="#40808080")

segments(-2, 8.75, 2, 8.75, col="#908080")
rect(-2, 7.5, 2, 8.5, col="#10101010", border=NA)

segments(-2, 17.25, 2, 17.25, col="#908080")
rect(-2, 16, 2, 17, col="#10101010", border=NA)

rect(-2, 24.5, 2, 25.5, col="#10101010", border=NA)


segments(0,0,0,26,col="#990909", lwd=2)



include <- list(
  # L_iris:
  A_post_tog$B_L_iris[,1], # GLOBAL - WOMEN
  (A_post_tog$B_L_iris[,1] + A_post_tog$betas[,1,7]), # L_iris women, Cameroon 2013
  (A_post_tog$B_L_iris[,1] + A_post_tog$betas[,2,7]), # L_iris women, Cameroon 2016
  (A_post_tog$B_L_iris[,1] + A_post_tog$betas[,3,7]), # L_iris women, Colombia
  (A_post_tog$B_L_iris[,1] + A_post_tog$betas[,4,7]), # L_iris women, CZ 2016
  (A_post_tog$B_L_iris[,1] + A_post_tog$betas[,5,7]), # L_iris women, CZ 2019
  (A_post_tog$B_L_iris[,1] + A_post_tog$betas[,6,7]), # L_iris women, Iran
  (A_post_tog$B_L_iris[,1] + A_post_tog$betas[,7,7]), # L_iris women, Turkey

  # A_iris
  A_post_tog$B_a_iris[,1], # GLOBAL - WOMEN
  (A_post_tog$B_a_iris[,1] + A_post_tog$betas[,1,8]), # a_iris women, Cameroon 2013
  (A_post_tog$B_a_iris[,1] + A_post_tog$betas[,2,8]), # a_iris women, Cameroon 2016
  (A_post_tog$B_a_iris[,1] + A_post_tog$betas[,3,8]), # a_iris women, Colombia
  (A_post_tog$B_a_iris[,1] + A_post_tog$betas[,4,8]), # a_iris women, CZ 2016
  (A_post_tog$B_a_iris[,1] + A_post_tog$betas[,5,8]), # a_iris women, CZ 2019
  (A_post_tog$B_a_iris[,1] + A_post_tog$betas[,6,8]), # a_iris women, Iran
  (A_post_tog$B_a_iris[,1] + A_post_tog$betas[,7,8]), # a_iris women, Turkey

  # B_iris
  A_post_tog$B_b_iris[,1], # GLOBAL - WOMEN
  (A_post_tog$B_b_iris[,1] + A_post_tog$betas[,1,9]), # b_iris women, Cameroon 2013
  (A_post_tog$B_b_iris[,1] + A_post_tog$betas[,2,9]), # b_iris women, Cameroon 2016
  (A_post_tog$B_b_iris[,1] + A_post_tog$betas[,3,9]), # b_iris women, Colombia
  (A_post_tog$B_b_iris[,1] + A_post_tog$betas[,4,9]), # b_iris women, CZ 2016
  (A_post_tog$B_b_iris[,1] + A_post_tog$betas[,5,9]), # b_iris women, CZ 2019
  (A_post_tog$B_b_iris[,1] + A_post_tog$betas[,6,9]), # b_iris women, Iran
  (A_post_tog$B_b_iris[,1] + A_post_tog$betas[,7,9]) # b_iris women, Turkey
)


cols<-c("#00FBFF", 
        "#8C00FF",
        "#00FF33",
        "#7a9c41",
        "#FFA600",
        
        "#00FBFF", 
        "#00FF33", 
        "#7a9c41"
)

cols <- rep(cols,3)
cols<-paste(cols,"80",sep="") 

ys<- c(seq(from=1, to=8, by=1),seq(from=9.5, to=16.5, by=1),seq(from=18, to=25, by=1))
ys<-rev(ys)

labels <- c(  "L* Global",
              "L* Cameroon 13",
              "L* Cameroon 16",
              "L* Colombia",
              "L* Czechia 16",
              "L* Czechia 19",
              "L* Iran",
              "L* Turkey",
              # A_iris
              "a* Global",
              "a* Cameroon 13",
              "a* Cameroon 16",
              "a* Colombia",
              "a* Czechia 16",
              "a* Czechia 19",
              "a* Iran",
              "a* Turkey",
              # B_iris
              "b* Global",
              "b* Cameroon 13",
              "b* Cameroon 16",
              "b* Colombia",
              "b* Czechia 16",
              "b* Czechia 19",
              "b* Iran",
              "b* Turkey"
)


Coefficients_check<-data.frame(Name=labels,Low_CI=rep(0,24),
                               Mean=rep(0,24),Up_CI=rep(0,24)) # Sorry, not in a mood to reinvent, how to .... do it aesthetically
for (i in 1:length(include)) {
  Coefficients_check$Mean[i]<-mean(include[[i]])
}
S1<-summary(as.factor(Coefficients_check$Mean))

for (i in 1:length(include)) {
  Coefficients_check$Low_CI[i]<-PI(include[[i]], prob = 0.95)[1]
}
S2<-summary(as.factor(Coefficients_check$Low_CI))

for (i in 1:length(include)) {
  Coefficients_check$Up_CI[i]<-PI(include[[i]], prob = 0.95)[2]
}
S3<-summary(as.factor(Coefficients_check$Up_CI))

for(i in 1:length(include)){ 
  toplot<-include[[i]]
  toplot_CI<-toplot
  mean_d<-mean(toplot)
  toplot<-toplot#[toplot>quantile(toplot,0.025)&toplot<quantile(toplot,0.975)]
  ycoord<-ys[i]
  colpol<-cols[i]
  textlab<-labels[i]
  dens<-density(toplot)
  ci <- quantile(toplot, c(0.025, 0.975))
  # lines(x=PI(toplot,prob=0.89),rep(ycoord-0.05,2),lwd=1.5, lty=5)
  points(x=mean(toplot_CI),ycoord,lwd=2,pch=21,col=1,bg=0,cex=1.2)
  axis(2,at=ycoord,labels=textlab,srt=0,las=1, pos = -2.1, cex.axis=1.5, tick=F, lwd.ticks = 0) 
#  text(format(round(Coefficients_check$Mean[i],2),nsmall=2), x=7.5, y=ys[i], cex=cex_t)
#  text(format(round(Coefficients_check$Low_CI[i],2),nsmall=2), x=5, y=ys[i], cex=cex_t)
#  text(format(round(Coefficients_check$Up_CI[i],2),nsmall=2), x=10, y=ys[i], cex=cex_t)
  for (i in 1:24) {
    c(polygon(c(dens$x,rev(dens$x)),ys[i]*(c(dens$y,rev(dens$y)*(-1))*0.03)+ycoord[i],border=F, lwd = 1.5, col=colpol))
  }
  lines(x=PI(toplot_CI,prob=0.95),rep(ycoord+0.05,2),lwd=2, lty=1, col='#2f4363')
  segments(x0 = ci[1], y0 = ycoord-0.15, x1 = ci[1], y1 = ycoord+0.25, lwd = 3, col = '#2f4363')
  segments(x0 = ci[2], y0 = ycoord-0.15, x1 = ci[2], y1 = ycoord+0.25, lwd = 3, col = '#2f4363')
  points(x=mean(toplot_CI),ycoord,lwd=2,pch=21,col=1,bg=0,cex=1.2)
}


# 2 out 4: Sclera Colour - WOMEN

# SCLERA: 
par(mar=c(3.1, 0.5, 1.1, 0.5),mgp=c(5,0.7,0))
plot(NULL,xlim=c(-2,2),ylim=c(1,25.5),yaxt="n",bty="n", ylab="", xlab="", cex.axis=0.9, 
     main="PIT (Sclera) Colour - Women", cex.main=1.5)

rect(-2, 0, 2, 26, col="#10101010", border=NA)

segments(-2, c(seq(from=1, to=8, by=1),seq(from=9.5, to=16.5, by=1),seq(from=18, to=25, by=1)), 2, 
         c(seq(from=1, to=8, by=1),seq(from=9.5, to=16.5, by=1),seq(from=18, to=25, by=1)), col="#40808080")

segments(-2, 8.75, 2, 8.75, col="#908080")
rect(-2, 7.5, 2, 8.5, col="#10101010", border=NA)

segments(-2, 17.25, 2, 17.25, col="#908080")
rect(-2, 16, 2, 17, col="#10101010", border=NA)

rect(-2, 24.5, 2, 25.5, col="#10101010", border=NA)


segments(0,0,0,26,col="#990909", lwd=2)

# Just hit the right posteriors - that's enough: 
str(A_post_tog) # BETAS: num[1:10000, NINE SAMPLES, 24 SLOPES - 12 Women, 12 Men)

include <- list(
  # SCLERA L:
  A_post_tog$B_L_sclera[,1], # GLOBAL - WOMEN
  (A_post_tog$B_L_sclera[,1] + A_post_tog$betas[,1,10]), # L_sclera women, Cameroon 2013
  (A_post_tog$B_L_sclera[,1] + A_post_tog$betas[,2,10]), # L_sclera women, Cameroon 2016
  (A_post_tog$B_L_sclera[,1] + A_post_tog$betas[,3,10]), # L_sclera women, Colombia
  (A_post_tog$B_L_sclera[,1] + A_post_tog$betas[,4,10]), # L_sclera women, CZ 2016
  (A_post_tog$B_L_sclera[,1] + A_post_tog$betas[,5,10]), # L_sclera women, CZ 2019
  (A_post_tog$B_L_sclera[,1] + A_post_tog$betas[,6,10]), # L_sclera women, Iran
  (A_post_tog$B_L_sclera[,1] + A_post_tog$betas[,7,10]), # L_sclera women, Turkey

  # SCLERA A
  A_post_tog$B_a_sclera[,1], # GLOBAL - WOMEN
  (A_post_tog$B_a_sclera[,1] + A_post_tog$betas[,1,11]), # a_sclera women, Cameroon 2013
  (A_post_tog$B_a_sclera[,1] + A_post_tog$betas[,2,11]), # a_sclera women, Cameroon 2016
  (A_post_tog$B_a_sclera[,1] + A_post_tog$betas[,3,11]), # a_sclera women, Colombia
  (A_post_tog$B_a_sclera[,1] + A_post_tog$betas[,4,11]), # a_sclera women, CZ 2016
  (A_post_tog$B_a_sclera[,1] + A_post_tog$betas[,5,11]), # a_sclera women, CZ 2019
  (A_post_tog$B_a_sclera[,1] + A_post_tog$betas[,6,11]), # a_sclera women, Iran
  (A_post_tog$B_a_sclera[,1] + A_post_tog$betas[,7,11]), # a_sclera women, Turkey

  # SCLERA B
  A_post_tog$B_b_sclera[,1], # GLOBAL - WOMEN
  (A_post_tog$B_b_sclera[,1] + A_post_tog$betas[,1,12]), # b_sclera women, Cameroon 2013
  (A_post_tog$B_b_sclera[,1] + A_post_tog$betas[,2,12]), # b_sclera women, Cameroon 2016
  (A_post_tog$B_b_sclera[,1] + A_post_tog$betas[,3,12]), # b_sclera women, Colombia
  (A_post_tog$B_b_sclera[,1] + A_post_tog$betas[,4,12]), # b_sclera women, CZ 2016
  (A_post_tog$B_b_sclera[,1] + A_post_tog$betas[,5,12]), # b_sclera women, CZ 2019
  (A_post_tog$B_b_sclera[,1] + A_post_tog$betas[,6,12]), # b_sclera women, Iran
  (A_post_tog$B_b_sclera[,1] + A_post_tog$betas[,7,12]) # b_sclera women, Turkey
)



for(i in 1:length(include)){ 
  toplot<-include[[i]]
  toplot_CI<-toplot
  mean_d<-mean(toplot)
  toplot<-toplot#[toplot>quantile(toplot,0.025)&toplot<quantile(toplot,0.975)]
  ycoord<-ys[i]
  colpol<-cols[i]
  textlab<-labels[i]
  dens<-density(toplot)
  ci <- quantile(toplot, c(0.025, 0.975))
  # lines(x=PI(toplot,prob=0.89),rep(ycoord-0.05,2),lwd=1.5, lty=5)
  points(x=mean(toplot_CI),ycoord,lwd=2,pch=21,col=1,bg=0,cex=1.2)
  #  axis(2,at=ycoord,labels=textlab,srt=0,las=1, cex.axis=1) 
#  text(format(round(Coefficients_check$Mean[i],2),nsmall=2), x=7.5, y=ys[i], cex=cex_t)
#  text(format(round(Coefficients_check$Low_CI[i],2),nsmall=2), x=5, y=ys[i], cex=cex_t)
#  text(format(round(Coefficients_check$Up_CI[i],2),nsmall=2), x=10, y=ys[i], cex=cex_t)
  for (i in 1:24) {
    c(polygon(c(dens$x,rev(dens$x)),ys[i]*(c(dens$y,rev(dens$y)*(-1))*0.03)+ycoord[i],border=F, lwd = 1.5, col=colpol))
  }
  lines(x=PI(toplot_CI,prob=0.95),rep(ycoord+0.05,2),lwd=2, lty=1, col='#2f4363')
  segments(x0 = ci[1], y0 = ycoord-0.15, x1 = ci[1], y1 = ycoord+0.25, lwd = 3, col = '#2f4363')
  segments(x0 = ci[2], y0 = ycoord-0.15, x1 = ci[2], y1 = ycoord+0.25, lwd = 3, col = '#2f4363')
  points(x=mean(toplot_CI),ycoord,lwd=2,pch=21,col=1,bg=0,cex=1.2)
}


# 3 out 4: Iris Colour - MEN

par(mar=c(3.1, 0.5, 1.1, 0.5),mgp=c(5,0.7,0))
plot(NULL,xlim=c(-2,2),ylim=c(1,25.5),yaxt="n",bty="n", ylab="", xlab="", cex.axis=0.9, 
     main="Iris Colour - Men", cex.main=1.5)

rect(-2, 0, 2, 26, col="#10101010", border=NA)

segments(-2, c(seq(from=1, to=8, by=1),seq(from=9.5, to=16.5, by=1),seq(from=18, to=25, by=1)), 2, 
         c(seq(from=1, to=8, by=1),seq(from=9.5, to=16.5, by=1),seq(from=18, to=25, by=1)), col="#40808080")

segments(-2, 8.75, 2, 8.75, col="#908080")
rect(-2, 7.5, 2, 8.5, col="#10101010", border=NA)

segments(-2, 17.25, 2, 17.25, col="#908080")
rect(-2, 16, 2, 17, col="#10101010", border=NA)

rect(-2, 24.5, 2, 25.5, col="#10101010", border=NA)


segments(0,0,0,26,col="#990909", lwd=2)

# Just hit the right posteriors - that's enough: 
str(A_post_tog) # BETAS: num[1:10000, NINE SAMPLES, 24 SLOPES - 12 Women, 12 Men)

include <- list(
  # L_iris:
  A_post_tog$B_L_iris[,2], # GLOBAL - MEN
  (A_post_tog$B_L_iris[,2] + A_post_tog$betas[,1,19]), # L_iris men, Cameroon 2013
  (A_post_tog$B_L_iris[,2] + A_post_tog$betas[,2,19]), # L_iris men, Cameroon 2016
  (A_post_tog$B_L_iris[,2] + A_post_tog$betas[,3,19]), # L_iris men, Colombia
  (A_post_tog$B_L_iris[,2] + A_post_tog$betas[,4,19]), # L_iris men, CZ 2016
  (A_post_tog$B_L_iris[,2] + A_post_tog$betas[,5,19]), # L_iris men, CZ 2019
  (A_post_tog$B_L_iris[,2] + A_post_tog$betas[,6,19]), # L_iris men, Iran
  (A_post_tog$B_L_iris[,2] + A_post_tog$betas[,7,19]), # L_iris men, Turkey

  # A_iris
  A_post_tog$B_a_iris[,2], # GLOBAL - WOMEN
  (A_post_tog$B_a_iris[,2] + A_post_tog$betas[,1,20]), # a_iris men, Cameroon 2013
  (A_post_tog$B_a_iris[,2] + A_post_tog$betas[,2,20]), # a_iris men, Cameroon 2016
  (A_post_tog$B_a_iris[,2] + A_post_tog$betas[,3,20]), # a_iris men, Colombia
  (A_post_tog$B_a_iris[,2] + A_post_tog$betas[,4,20]), # a_iris men, CZ 2016
  (A_post_tog$B_a_iris[,2] + A_post_tog$betas[,5,20]), # a_iris men, CZ 2019
  (A_post_tog$B_a_iris[,2] + A_post_tog$betas[,6,20]), # a_iris men, Iran
  (A_post_tog$B_a_iris[,2] + A_post_tog$betas[,7,20]), # a_iris men, Turkey

  # B_iris
  A_post_tog$B_b_iris[,2], # GLOBAL - WOMEN
  (A_post_tog$B_b_iris[,2] + A_post_tog$betas[,1,21]), # b_iris men, Cameroon 2013
  (A_post_tog$B_b_iris[,2] + A_post_tog$betas[,2,21]), # b_iris men, Cameroon 2016
  (A_post_tog$B_b_iris[,2] + A_post_tog$betas[,3,21]), # b_iris men, Colombia
  (A_post_tog$B_b_iris[,2] + A_post_tog$betas[,4,21]), # b_iris men, CZ 2016
  (A_post_tog$B_b_iris[,2] + A_post_tog$betas[,5,21]), # b_iris men, CZ 2019
  (A_post_tog$B_b_iris[,2] + A_post_tog$betas[,6,21]), # b_iris men, Iran
  (A_post_tog$B_b_iris[,2] + A_post_tog$betas[,7,21]) # b_iris men, Turkey
)



for(i in 1:length(include)){ 
  toplot<-include[[i]]
  toplot_CI<-toplot
  mean_d<-mean(toplot)
  toplot<-toplot#[toplot>quantile(toplot,0.025)&toplot<quantile(toplot,0.975)]
  ycoord<-ys[i]
  colpol<-cols[i]
  textlab<-labels[i]
  dens<-density(toplot)
  ci <- quantile(toplot, c(0.025, 0.975))
  # lines(x=PI(toplot,prob=0.89),rep(ycoord-0.05,2),lwd=1.5, lty=5)
  points(x=mean(toplot_CI),ycoord,lwd=2,pch=21,col=1,bg=0,cex=1.2)
  #  axis(2,at=ycoord,labels=textlab,srt=0,las=1, cex.axis=1) 
#  text(format(round(Coefficients_check$Mean[i],2),nsmall=2), x=7.5, y=ys[i], cex=cex_t)
#  text(format(round(Coefficients_check$Low_CI[i],2),nsmall=2), x=5, y=ys[i], cex=cex_t)
#  text(format(round(Coefficients_check$Up_CI[i],2),nsmall=2), x=10, y=ys[i], cex=cex_t)
  for (i in 1:24) {
    c(polygon(c(dens$x,rev(dens$x)),ys[i]*(c(dens$y,rev(dens$y)*(-1))*0.03)+ycoord[i],border=F, lwd = 1.5, col=colpol))
  }
  lines(x=PI(toplot_CI,prob=0.95),rep(ycoord+0.05,2),lwd=2, lty=1, col='#2f4363')
  segments(x0 = ci[1], y0 = ycoord-0.15, x1 = ci[1], y1 = ycoord+0.25, lwd = 3, col = '#2f4363')
  segments(x0 = ci[2], y0 = ycoord-0.15, x1 = ci[2], y1 = ycoord+0.25, lwd = 3, col = '#2f4363')
  points(x=mean(toplot_CI),ycoord,lwd=2,pch=21,col=1,bg=0,cex=1.2)
}


# 4 out 4: Sclera Colour - MEN

par(mar=c(3.1, 0.5, 1.1, 0.5),mgp=c(5,0.7,0))

plot(NULL,xlim=c(-2,2),ylim=c(1,25.5),yaxt="n",bty="n", ylab="", xlab="", cex.axis=0.9, 
     main="PIT (Sclera) Colour - Men", cex.main=1.5)

rect(-2, 0, 2, 26, col="#10101010", border=NA)

segments(-2, c(seq(from=1, to=8, by=1),seq(from=9.5, to=16.5, by=1),seq(from=18, to=25, by=1)), 2, 
         c(seq(from=1, to=8, by=1),seq(from=9.5, to=16.5, by=1),seq(from=18, to=25, by=1)), col="#40808080")

segments(-2, 8.75, 2, 8.75, col="#908080")
rect(-2, 7.5, 2, 8.5, col="#10101010", border=NA)

segments(-2, 17.25, 2, 17.25, col="#908080")
rect(-2, 16, 2, 17, col="#10101010", border=NA)

rect(-2, 24.5, 2, 25.5, col="#10101010", border=NA)


segments(0,0,0,26,col="#990909", lwd=2)

include <- list(
  # L_Sclera:
  A_post_tog$B_L_sclera[,2], # GLOBAL - MEN
  (A_post_tog$B_L_sclera[,2] + A_post_tog$betas[,1,22]), # L_sclera men, Cameroon 2013
  (A_post_tog$B_L_sclera[,2] + A_post_tog$betas[,2,22]), # L_sclera men, Cameroon 2016
  (A_post_tog$B_L_sclera[,2] + A_post_tog$betas[,3,22]), # L_sclera men, Colombia
  (A_post_tog$B_L_sclera[,2] + A_post_tog$betas[,4,22]), # L_Sclera men, CZ 2016
  (A_post_tog$B_L_sclera[,2] + A_post_tog$betas[,5,22]), # L_sclera men, CZ 2019
  (A_post_tog$B_L_sclera[,2] + A_post_tog$betas[,6,22]), # L_sclera men, Iran
  (A_post_tog$B_L_sclera[,2] + A_post_tog$betas[,7,22]), # L_sclera men, Turkey

  # A_Sclera
  A_post_tog$B_a_sclera[,2], # GLOBAL - MEN
  (A_post_tog$B_a_sclera[,2] + A_post_tog$betas[,1,23]), # a_sclera men, Cameroon 2013
  (A_post_tog$B_a_sclera[,2] + A_post_tog$betas[,2,23]), # a_sclera men, Cameroon 2016
  (A_post_tog$B_a_sclera[,2] + A_post_tog$betas[,3,23]), # a_sclera men, Colombia
  (A_post_tog$B_a_sclera[,2] + A_post_tog$betas[,4,23]), # a_sclera men, CZ 2016
  (A_post_tog$B_a_sclera[,2] + A_post_tog$betas[,5,23]), # a_sclera men, CZ 2019
  (A_post_tog$B_a_sclera[,2] + A_post_tog$betas[,6,23]), # a_sclera men, Iran
  (A_post_tog$B_a_sclera[,2] + A_post_tog$betas[,7,23]), # a_sclera men, Turkey

  # B_Sclera
  A_post_tog$B_b_sclera[,2], # GLOBAL - MEN
  (A_post_tog$B_b_sclera[,2] + A_post_tog$betas[,1,24]), # b_sclera men, Cameroon 2013
  (A_post_tog$B_b_sclera[,2] + A_post_tog$betas[,2,24]), # b_sclera men, Cameroon 2016
  (A_post_tog$B_b_sclera[,2] + A_post_tog$betas[,3,24]), # b_sclera men, Colombia
  (A_post_tog$B_b_sclera[,2] + A_post_tog$betas[,4,24]), # b_sclera men, CZ 2016
  (A_post_tog$B_b_sclera[,2] + A_post_tog$betas[,5,24]), # b_sclera men, CZ 2019
  (A_post_tog$B_b_sclera[,2] + A_post_tog$betas[,6,24]), # b_sclera men, Iran
  (A_post_tog$B_b_sclera[,2] + A_post_tog$betas[,7,24]) # b_sclera men, Turkey
)


#text(0,-1,"Slope estimate", cex=1, pos=1, offset=1, xpd=NA)
#  text("Mean", x=7.5, y=max(ys-pls), cex=cex_t2, offset=0.5, xpd=NA, srt=0)
#  text("2.5 %", x=5, y=max(ys-pls), cex=cex_t2, offset=0.5, xpd=NA, srt=0)
#  text("97.5 %", x=10, y=max(ys-pls), cex=cex_t2, offset=0.5, xpd=NA, srt=0)


for(i in 1:length(include)){ 
  toplot<-include[[i]]
  toplot_CI<-toplot
  mean_d<-mean(toplot)
  toplot<-toplot#[toplot>quantile(toplot,0.025)&toplot<quantile(toplot,0.975)]
  ycoord<-ys[i]
  colpol<-cols[i]
  textlab<-labels[i]
  dens<-density(toplot)
  ci <- quantile(toplot, c(0.025, 0.975))
  # lines(x=PI(toplot,prob=0.89),rep(ycoord-0.05,2),lwd=1.5, lty=5)
  points(x=mean(toplot_CI),ycoord,lwd=2,pch=21,col=1,bg=0,cex=1.2)
  #  axis(2,at=ycoord,labels=textlab,srt=0,las=1, cex.axis=1) 
# text(format(round(Coefficients_check$Mean[i],2),nsmall=2), x=7.5, y=ys[i], cex=cex_t)
# text(format(round(Coefficients_check$Low_CI[i],2),nsmall=2), x=5, y=ys[i], cex=cex_t)
# text(format(round(Coefficients_check$Up_CI[i],2),nsmall=2), x=10, y=ys[i], cex=cex_t)
  for (i in 1:24) {
    c(polygon(c(dens$x,rev(dens$x)),ys[i]*(c(dens$y,rev(dens$y)*(-1))*0.03)+ycoord[i],border=F, lwd = 1.5, col=colpol))
  }
  lines(x=PI(toplot_CI,prob=0.95),rep(ycoord+0.05,2),lwd=2, lty=1, col='#2f4363')
  segments(x0 = ci[1], y0 = ycoord-0.15, x1 = ci[1], y1 = ycoord+0.25, lwd = 3, col = '#2f4363')
  segments(x0 = ci[2], y0 = ycoord-0.15, x1 = ci[2], y1 = ycoord+0.25, lwd = 3, col = '#2f4363')
  points(x=mean(toplot_CI),ycoord,lwd=2,pch=21,col=1,bg=0,cex=1.2)
}

dev.off()




#---------------------------------------------------------------

#----------------------------------------------------------------

# SD distribution comparison 

# We need model 2 and 4, so upload model 2 in the system.
# Model 3 is just a version of model 4 
# Model 1 lacks terms for individual raters

M2_attr <- readRDS("post_m2_sext_Long_11_06.RDS")

A_post_tog_M2 <- M2_attr

# Raters: 
str(A_post_tog_M2$sigma_r) # One sigma - one parameter
str(A_post_tog$sigma_r) # Twelwe sigmas for twelwe parameters

# Targets: 
str(A_post_tog_M2$sigma_t) # One sigma - one parameter
str(A_post_tog$sigma_t) 

hist(A_post_tog_M2$sigma_r)
hist(A_post_tog_M2$sigma_t)
hist(A_post_tog$sigma_t) 
hist(A_post_tog$sigma_r[,1]) 



include <- list(
  A_post_tog_M2$sigma_t,
  A_post_tog_M2$sigma_r,
  A_post_tog$sigma_t,
  A_post_tog$sigma_r[,1], # Intercept 
  A_post_tog$sigma_r[,7], # L* iris
  A_post_tog$sigma_r[,8], # a* iris
  A_post_tog$sigma_r[,9], # b* iris
  A_post_tog$sigma_r[,10], # L* PIT
  A_post_tog$sigma_r[,11], # a* PIT
  A_post_tog$sigma_r[,12] # b* PIT
)


cols<-c("#00FBFF", 
        "#8C00FF",
        "#00FF33",
        "#7a9c41",
        "#FFA600",
        
        "#00FBFF", 
        "#00FF33", 
        "#7a9c41",
        "#8C00FF",
        "#FFA600"
)

cols<-paste(cols,"80",sep="") 

labs <- c("M2: sigma t",
          "M2: sigma_r",
          "M4: sigma_t",
          "M4: sigma_r (intercept)",
          "M4: sigma_r (L* iris)",
          "M4: sigma_r (a* iris)",
          "M4: sigma_r (b* iris)",
          "M4: sigma_r (L* PIT)",
          "M4: sigma_r (a* PIT)",
          "M4: sigma_r (b* PIT)"
)


tiff("Sigmas_SEXT_M2_M4.tif",width=20,height=20,units="cm",res=600,compression = "lzw")

# 1 out 4: Iris Colour - WOMEN

par(mar=c(3.1, 12.1, 1.1, 1.1),mgp=c(5,0.7,0))
plot(NULL,xlim=c(-0.1,3),ylim=c(1,10.5),yaxt="n",bty="n", ylab="", xlab="posterior", cex.axis=0.9, 
     main="Standard deviations as parameters", cex.main=1.5)

rect(-0.09, 0, 3, 10.5, col="#10101010", border=NA)

segments(-0.1, seq(from=1, to=10, by=1),3, 
         seq(from=1, to=10, by=1), col="#40808080")


segments(c(0.5,1, 1.5, 2, 2.5),0,c(0.5,1, 1.5, 2, 2.5),10,col="#80808080",lty=2)
segments(0,0,0,10.5,col="#990909", lwd=2)

ys<- c(seq(from=1, to=10, by=1))
ys<-rev(ys)


for(i in 1:length(include)){ 
  toplot<-include[[i]]
  toplot_CI<-toplot
  mean_d<-mean(toplot)
  toplot<-toplot#[toplot>quantile(toplot,0.025)&toplot<quantile(toplot,0.975)]
  ycoord<-ys[i]
  colpol<-cols[i]
  textlab<-labs[i]
  dens<-density(toplot)
  ci <- quantile(toplot, c(0.025, 0.975))
  # lines(x=PI(toplot,prob=0.89),rep(ycoord-0.05,2),lwd=1.5, lty=5)
  points(x=mean(toplot_CI),ycoord,lwd=2,pch=21,col=1,bg=0,cex=1.2)
  axis(2,at=ycoord,labels=textlab,srt=0,las=1, pos = 0, cex.axis=1.5, tick=F, lwd.ticks = 0) 
  #  text(format(round(Coefficients_check$Mean[i],2),nsmall=2), x=7.5, y=ys[i], cex=cex_t)
  #  text(format(round(Coefficients_check$Low_CI[i],2),nsmall=2), x=5, y=ys[i], cex=cex_t)
  #  text(format(round(Coefficients_check$Up_CI[i],2),nsmall=2), x=10, y=ys[i], cex=cex_t)
  for (i in 1:10) {
    c(polygon(c(dens$x,rev(dens$x)),ys[i]*(c(dens$y,rev(dens$y)*(-1))*0.005)+ycoord[i],border=F, lwd = 1.5, col=colpol))
  }
  lines(x=PI(toplot_CI,prob=0.95),rep(ycoord+0.05,2),lwd=2, lty=1, col='#2f4363')
  segments(x0 = ci[1], y0 = ycoord-0.15, x1 = ci[1], y1 = ycoord+0.25, lwd = 3, col = '#2f4363')
  segments(x0 = ci[2], y0 = ycoord-0.15, x1 = ci[2], y1 = ycoord+0.25, lwd = 3, col = '#2f4363')
  points(x=mean(toplot_CI),ycoord,lwd=2,pch=21,col=1,bg=0,cex=1.2)
}

dev.off()

