# Drawing distribution plots based on alternative analyses 

#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.
#
# Standardised together - Without Iranians 
#
#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.

setwd("D:/GACR_VF/Measured_Eye_Phenotype_And_Ascribed_Characteristics_In_Different_Countries/Final Figures Draft Data/Model Objects Alternatives/DONE_Standardised_separately_without_Iranian_samples")

load("Model_9_ATTRACT_BOTH_SEXES_WITHOUT_IRANIANS_Standardised_separately.Rdata")
load("Model_10_SEXTYP_both_sexes_standardised_together_without_Iranians.Rdata")


A_post_tog <- extract.samples(m9_attr)

S_post_tog <- extract.samples(m10_Sext)


tiff("Posterior__ATTR_Preds_TOGETHER_STANDARDISED_Without_Iranians.tif",width=32,height=26,units="cm",res=600,compression = "lzw")
layout(matrix(1:4,byrow=T,nrow=1), widths=c(2.9,2,2,2),heights=1)

# 1 out 4: Iris Colour - WOMEN

par(mar=c(3.1, 7.1, 1.1, 1.1), mgp=c(5, 0.7, 2))
plot(NULL, xlim=c(-0.75, 2.5), ylim=c(1, 28.5), yaxt="n", bty="n", ylab="", xlab="", cex.axis=0.9, 
     main="Iris Colour: WOMEN")

rect(-0.75, 0, 1, 29, col="#10101010", border=NA)

segments(-0.75, c(seq(from=1, to=9, by=1), seq(from=10.5, to=18.5, by=1), seq(from=20, to=28, by=1)), 1, 
         c(seq(from=1, to=9, by=1), seq(from=10.5, to=18.5, by=1), seq(from=20, to=28, by=1)), col="#40808080")

segments(-0.75, 9, 1, 9, col="#908080")
rect(-0.75, 8.5, 1, 9.5, col="#10101010", border=NA)

segments(-0.75, 18.5, 1, 18.5, col="#908080")
rect(-0.75, 18, 1, 19, col="#10101010", border=NA)

segments(-0.75, 28, 1, 28, col="#908080")
rect(-0.75, 27.5, 1, 28.5, col="#10101010", border=NA)

segments(c(-0.75, -0.25, 0.25, 0.75), 0, c(-0.75, -0.25, 0.25, 0.75), 29, col="#80808080", lty=2)
segments(0, 0, 0, 29, col="#990909", lwd=2)

# Just hit the right posteriors - that's enough: 
str(A_post_tog) # BETAS: num[1:10000, NINE SAMPLES, 24 SLOPES - 12 Women, 12 Men)

include <- list(
  # L_iris:
  A_post_tog$B_L_iris[,1], # GLOBAL - WOMEN
  (A_post_tog$B_L_iris[,1] + A_post_tog$betas[,1,7]), # L_iris women, Cameroon 2013
  (A_post_tog$B_L_iris[,1] + A_post_tog$betas[,2,7]), # L_iris women, Cameroon 2016
  (A_post_tog$B_L_iris[,1] + A_post_tog$betas[,3,7]), # L_iris women, Colombia
  (A_post_tog$B_L_iris[,1] + A_post_tog$betas[,4,7]), # L_iris women, CZ 2016
  (A_post_tog$B_L_iris[,1] + A_post_tog$betas[,5,7]), # L_iris women, CZ 2019
  (A_post_tog$B_L_iris[,1] + A_post_tog$betas[,6,7]), # L_iris women, India
  (A_post_tog$B_L_iris[,1] + A_post_tog$betas[,7,7]), # L_iris women, Turkey
  (A_post_tog$B_L_iris[,1] + A_post_tog$betas[,8,7]), # L_iris women, Vietnam

  # A_iris
  A_post_tog$B_a_iris[,1], # GLOBAL - WOMEN
  (A_post_tog$B_a_iris[,1] + A_post_tog$betas[,1,8]), # a_iris women, Cameroon 2013
  (A_post_tog$B_a_iris[,1] + A_post_tog$betas[,2,8]), # a_iris women, Cameroon 2016
  (A_post_tog$B_a_iris[,1] + A_post_tog$betas[,3,8]), # a_iris women, Colombia
  (A_post_tog$B_a_iris[,1] + A_post_tog$betas[,4,8]), # a_iris women, CZ 2016
  (A_post_tog$B_a_iris[,1] + A_post_tog$betas[,5,8]), # a_iris women, CZ 2019
  (A_post_tog$B_a_iris[,1] + A_post_tog$betas[,6,8]), # a_iris women, India
  (A_post_tog$B_a_iris[,1] + A_post_tog$betas[,7,8]), # a_iris women, Turkey
  (A_post_tog$B_a_iris[,1] + A_post_tog$betas[,8,8]), # a_iris women, Vietnam

  # B_iris
  A_post_tog$B_b_iris[,1], # GLOBAL - WOMEN
  (A_post_tog$B_b_iris[,1] + A_post_tog$betas[,1,9]), # b_iris women, Cameroon 2013
  (A_post_tog$B_b_iris[,1] + A_post_tog$betas[,2,9]), # b_iris women, Cameroon 2016
  (A_post_tog$B_b_iris[,1] + A_post_tog$betas[,3,9]), # b_iris women, Colombia
  (A_post_tog$B_b_iris[,1] + A_post_tog$betas[,4,9]), # b_iris women, CZ 2016
  (A_post_tog$B_b_iris[,1] + A_post_tog$betas[,5,9]), # b_iris women, CZ 2019
  (A_post_tog$B_b_iris[,1] + A_post_tog$betas[,6,9]), # b_iris women, India
  (A_post_tog$B_b_iris[,1] + A_post_tog$betas[,7,9]), # b_iris women, Turkey
  (A_post_tog$B_b_iris[,1] + A_post_tog$betas[,8,9]) # b_iris women, Vietnam
)


cols<-c("#00FBFF", 
        "#8C00FF",
        "#00FF33", 
        "#FFEA00",
        "#FFA600",
        
        "#00FBFF", 
        "#8C00FF",
        "#00FF33", 
        "#FFEA00",
        "#FFA600",
        
        "#00FBFF", 
        "#8C00FF",
        "#00FF33", 
        "#FFEA00",
        "#FFA600",
        
        "#00FBFF", 
        "#8C00FF",
        "#00FF33", 
        "#FFEA00",
        "#FFA600",
        
        "#00FBFF", 
        "#8C00FF",
        "#00FF33", 
        "#FFEA00",
        "#FFA600",
        
        "#00FBFF", 
        "#8C00FF"
)

cols<-paste(cols,"80",sep="") 

ys<- c(seq(from=1, to=9, by=1),seq(from=10.5, to=18.5, by=1),seq(from=20, to=28, by=1))
ys<-rev(ys)

labels <- c(  "L* Global",
              "L* Cameroon 13",
              "L* Cameroon 16",
              "L* Colombia",
              "L* Czechia 16",
              "L* Czechia 19",
              "L* India - FCD",
              "L* Turkey",
              "L* Vietnam",
              # A_iris
              "a* Global",
              "a* Cameroon 13",
              "a* Cameroon 16",
              "a* Colombia",
              "a* Czechia 16",
              "a* Czechia 19",
              "a* India - FCD",
              "a* Turkey",
              "a* Vietnam",
              # B_iris
              "b* Global",
              "b* Cameroon 13",
              "b* Cameroon 16",
              "b* Colombia",
              "b* Czechia 16",
              "b* Czechia 19",
              "b* India - FCD",
              "b* Turkey",
              "b* Vietnam"
)


Coefficients_check<-data.frame(Name=labels,Low_CI=rep(0,27),
                               Mean=rep(0,27),Up_CI=rep(0,27)) # Sorry, not in a mood to reinvent, how to .... do it aesthetically
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

cex_t2 <- 1.05
cex_t <- 1.15
pls <- -0.9
#text(0,-1,"Slope estimate", cex=1, pos=1, offset=1, xpd=NA)
text("Mean", x=1.75, y=max(ys-pls), cex=cex_t2, offset=0.5, xpd=NA, srt=0)
text("2.5 %", x=1.25, y=max(ys-pls), cex=cex_t2, offset=0.5, xpd=NA, srt=0)
text("97.5 %", x=2.25, y=max(ys-pls), cex=cex_t2, offset=0.5, xpd=NA, srt=0)


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
  axis(2,at=ycoord,labels=textlab,srt=0,las=1, cex.axis=1.2) 
  text(format(round(Coefficients_check$Mean[i],2),nsmall=2), x=1.75, y=ys[i], cex=cex_t)
  text(format(round(Coefficients_check$Low_CI[i],2),nsmall=2), x=1.25,y=ys[i], cex=cex_t)
  text(format(round(Coefficients_check$Up_CI[i],2),nsmall=2), x=2.25,y=ys[i], cex=cex_t)
  for (i in 1:27) {
    c(polygon(c(dens$x,rev(dens$x)),ys[i]*(c(dens$y,rev(dens$y)*(-1))*0.003)+ycoord[i],border=F, lwd = 1.5, col=colpol))
  }
  lines(x=PI(toplot_CI,prob=0.95),rep(ycoord+0.05,2),lwd=2, lty=1, col='#9090DDDF')
  segments(x0 = ci[1], y0 = ycoord-0.15, x1 = ci[1], y1 = ycoord+0.25, lwd = 3, col = '#9090DDDF')
  segments(x0 = ci[2], y0 = ycoord-0.15, x1 = ci[2], y1 = ycoord+0.25, lwd = 3, col = '#9090DDDF')
  points(x=mean(toplot_CI),ycoord,lwd=2,pch=21,col=1,bg=0,cex=1.2)
}


# 2 out 4: Sclera Colour - WOMEN


# SCLERA: 
par(mar=c(3.1, 0.5, 1.1, 0.5),mgp=c(5,0.7,2))

plot(NULL, xlim=c(-0.75, 2.5), ylim=c(1, 28.5), yaxt="n", bty="n", ylab="", xlab="", cex.axis=0.9, 
     main="Sclera Colour: WOMEN")

rect(-0.75, 0, 1, 29, col="#10101010", border=NA)

segments(-0.75, c(seq(from=1, to=9, by=1), seq(from=10.5, to=18.5, by=1), seq(from=20, to=28, by=1)), 1, 
         c(seq(from=1, to=9, by=1), seq(from=10.5, to=18.5, by=1), seq(from=20, to=28, by=1)), col="#40808080")

segments(-0.75, 9, 1, 9, col="#908080")
rect(-0.75, 8.5, 1, 9.5, col="#10101010", border=NA)

segments(-0.75, 18.5, 1, 18.5, col="#908080")
rect(-0.75, 18, 1, 19, col="#10101010", border=NA)

segments(-0.75, 28, 1, 28, col="#908080")
rect(-0.75, 27.5, 1, 28.5, col="#10101010", border=NA)

segments(c(-0.75, -0.25, 0.25, 0.75), 0, c(-0.75, -0.25, 0.25, 0.75), 29, col="#80808080", lty=2)
segments(0, 0, 0, 29, col="#990909", lwd=2)


include <- list(
  # SCLERA L:
  A_post_tog$B_L_sclera[,1], # GLOBAL - WOMEN
  (A_post_tog$B_L_sclera[,1] + A_post_tog$betas[,1,10]), # L_sclera women, Cameroon 2013
  (A_post_tog$B_L_sclera[,1] + A_post_tog$betas[,2,10]), # L_sclera women, Cameroon 2016
  (A_post_tog$B_L_sclera[,1] + A_post_tog$betas[,3,10]), # L_sclera women, Colombia
  (A_post_tog$B_L_sclera[,1] + A_post_tog$betas[,4,10]), # L_sclera women, CZ 2016
  (A_post_tog$B_L_sclera[,1] + A_post_tog$betas[,5,10]), # L_sclera women, CZ 2019
  (A_post_tog$B_L_sclera[,1] + A_post_tog$betas[,6,10]), # L_sclera women, India
  (A_post_tog$B_L_sclera[,1] + A_post_tog$betas[,7,10]), # L_sclera women, Turkey
  (A_post_tog$B_L_sclera[,1] + A_post_tog$betas[,8,10]), # L_sclera women, Vietnam

  # SCLERA A
  A_post_tog$B_a_sclera[,1], # GLOBAL - WOMEN
  (A_post_tog$B_a_sclera[,1] + A_post_tog$betas[,1,11]), # a_sclera women, Cameroon 2013
  (A_post_tog$B_a_sclera[,1] + A_post_tog$betas[,2,11]), # a_sclera women, Cameroon 2016
  (A_post_tog$B_a_sclera[,1] + A_post_tog$betas[,3,11]), # a_sclera women, Colombia
  (A_post_tog$B_a_sclera[,1] + A_post_tog$betas[,4,11]), # a_sclera women, CZ 2016
  (A_post_tog$B_a_sclera[,1] + A_post_tog$betas[,5,11]), # a_sclera women, CZ 2019
  (A_post_tog$B_a_sclera[,1] + A_post_tog$betas[,6,11]), # a_sclera women, India
  (A_post_tog$B_a_sclera[,1] + A_post_tog$betas[,7,11]), # a_sclera women, Turkey
  (A_post_tog$B_a_sclera[,1] + A_post_tog$betas[,8,11]), # a_sclera women, Vietnam

  # SCLERA B
  A_post_tog$B_b_sclera[,1], # GLOBAL - WOMEN
  (A_post_tog$B_b_sclera[,1] + A_post_tog$betas[,1,12]), # b_sclera women, Cameroon 2013
  (A_post_tog$B_b_sclera[,1] + A_post_tog$betas[,2,12]), # b_sclera women, Cameroon 2016
  (A_post_tog$B_b_sclera[,1] + A_post_tog$betas[,3,12]), # b_sclera women, Colombia
  (A_post_tog$B_b_sclera[,1] + A_post_tog$betas[,4,12]), # b_sclera women, CZ 2016
  (A_post_tog$B_b_sclera[,1] + A_post_tog$betas[,5,12]), # b_sclera women, CZ 2019
  (A_post_tog$B_b_sclera[,1] + A_post_tog$betas[,6,12]), # b_sclera women, India
  (A_post_tog$B_b_sclera[,1] + A_post_tog$betas[,7,12]), # b_sclera women, Turkey
  (A_post_tog$B_b_sclera[,1] + A_post_tog$betas[,8,12]) # b_sclera women, Vietnam
)


Coefficients_check<-data.frame(Name=labels,Low_CI=rep(0,27),
                               Mean=rep(0,27),Up_CI=rep(0,27)) # Sorry, not in a mood to reinvent, how to .... do it aesthetically
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


#text(0,-1,"Slope estimate", cex=1, pos=1, offset=1, xpd=NA)
text("Mean", x=1.75, y=max(ys-pls), cex=cex_t2, offset=0.5, xpd=NA, srt=0)
text("2.5 %", x=1.25, y=max(ys-pls), cex=cex_t2, offset=0.5, xpd=NA, srt=0)
text("97.5 %", x=2.25, y=max(ys-pls), cex=cex_t2, offset=0.5, xpd=NA, srt=0)


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
  text(format(round(Coefficients_check$Mean[i],2),nsmall=2), x=1.75, y=ys[i], cex=cex_t)
  text(format(round(Coefficients_check$Low_CI[i],2),nsmall=2), x=1.25,y=ys[i], cex=cex_t)
  text(format(round(Coefficients_check$Up_CI[i],2),nsmall=2), x=2.25,y=ys[i], cex=cex_t)
  for (i in 1:27) {
    c(polygon(c(dens$x,rev(dens$x)),ys[i]*(c(dens$y,rev(dens$y)*(-1))*0.003)+ycoord[i],border=F, lwd = 1.5, col=colpol))
  }
  lines(x=PI(toplot_CI,prob=0.95),rep(ycoord+0.05,2),lwd=2, lty=1, col='#9090DDDF')
  segments(x0 = ci[1], y0 = ycoord-0.15, x1 = ci[1], y1 = ycoord+0.25, lwd = 3, col = '#9090DDDF')
  segments(x0 = ci[2], y0 = ycoord-0.15, x1 = ci[2], y1 = ycoord+0.25, lwd = 3, col = '#9090DDDF')
  points(x=mean(toplot_CI),ycoord,lwd=2,pch=21,col=1,bg=0,cex=1.2)
}


# 3 out 4: Iris Colour - MEN


par(mar=c(3.1, 0.5, 1.1, 0.5),mgp=c(5,0.7,2))
plot(NULL, xlim=c(-0.75, 2.5), ylim=c(1, 28.5), yaxt="n", bty="n", ylab="", xlab="", cex.axis=0.9, 
     main="Iris Colour: MEN")

rect(-0.75, 0, 1, 29, col="#10101010", border=NA)

segments(-0.75, c(seq(from=1, to=9, by=1), seq(from=10.5, to=18.5, by=1), seq(from=20, to=28, by=1)), 1, 
         c(seq(from=1, to=9, by=1), seq(from=10.5, to=18.5, by=1), seq(from=20, to=28, by=1)), col="#40808080")

segments(-0.75, 9, 1, 9, col="#908080")
rect(-0.75, 8.5, 1, 9.5, col="#10101010", border=NA)

segments(-0.75, 18.5, 1, 18.5, col="#908080")
rect(-0.75, 18, 1, 19, col="#10101010", border=NA)

segments(-0.75, 28, 1, 28, col="#908080")
rect(-0.75, 27.5, 1, 28.5, col="#10101010", border=NA)

segments(c(-0.75, -0.25, 0.25, 0.75), 0, c(-0.75, -0.25, 0.25, 0.75), 29, col="#80808080", lty=2)
segments(0, 0, 0, 29, col="#990909", lwd=2)



include <- list(
  # L_iris:
  A_post_tog$B_L_iris[,2], # GLOBAL - MEN
  (A_post_tog$B_L_iris[,2] + A_post_tog$betas[,1,19]), # L_iris men, Cameroon 2013
  (A_post_tog$B_L_iris[,2] + A_post_tog$betas[,2,19]), # L_iris men, Cameroon 2016
  (A_post_tog$B_L_iris[,2] + A_post_tog$betas[,3,19]), # L_iris men, Colombia
  (A_post_tog$B_L_iris[,2] + A_post_tog$betas[,4,19]), # L_iris men, CZ 2016
  (A_post_tog$B_L_iris[,2] + A_post_tog$betas[,5,19]), # L_iris men, CZ 2019
  (A_post_tog$B_L_iris[,2] + A_post_tog$betas[,6,19]), # L_iris men, India
  (A_post_tog$B_L_iris[,2] + A_post_tog$betas[,7,19]), # L_iris men, Turkey
  (A_post_tog$B_L_iris[,2] + A_post_tog$betas[,8,19]), # L_iris men, Vietnam

  # A_iris
  A_post_tog$B_a_iris[,2], # GLOBAL - WOMEN
  (A_post_tog$B_a_iris[,2] + A_post_tog$betas[,1,20]), # a_iris men, Cameroon 2013
  (A_post_tog$B_a_iris[,2] + A_post_tog$betas[,2,20]), # a_iris men, Cameroon 2016
  (A_post_tog$B_a_iris[,2] + A_post_tog$betas[,3,20]), # a_iris men, Colombia
  (A_post_tog$B_a_iris[,2] + A_post_tog$betas[,4,20]), # a_iris men, CZ 2016
  (A_post_tog$B_a_iris[,2] + A_post_tog$betas[,5,20]), # a_iris men, CZ 2019
  (A_post_tog$B_a_iris[,2] + A_post_tog$betas[,6,20]), # a_iris men, India
  (A_post_tog$B_a_iris[,2] + A_post_tog$betas[,7,20]), # a_iris men, Turkey
  (A_post_tog$B_a_iris[,2] + A_post_tog$betas[,8,20]), # a_iris men, Vietnam

  # B_iris
  A_post_tog$B_b_iris[,2], # GLOBAL - WOMEN
  (A_post_tog$B_b_iris[,2] + A_post_tog$betas[,1,21]), # b_iris men, Cameroon 2013
  (A_post_tog$B_b_iris[,2] + A_post_tog$betas[,2,21]), # b_iris men, Cameroon 2016
  (A_post_tog$B_b_iris[,2] + A_post_tog$betas[,3,21]), # b_iris men, Colombia
  (A_post_tog$B_b_iris[,2] + A_post_tog$betas[,4,21]), # b_iris men, CZ 2016
  (A_post_tog$B_b_iris[,2] + A_post_tog$betas[,5,21]), # b_iris men, CZ 2019
  (A_post_tog$B_b_iris[,2] + A_post_tog$betas[,6,21]), # b_iris men, India
  (A_post_tog$B_b_iris[,2] + A_post_tog$betas[,7,21]), # b_iris men, Turkey
  (A_post_tog$B_b_iris[,2] + A_post_tog$betas[,8,21]) # b_iris men, Vietnam
)


Coefficients_check<-data.frame(Name=labels,Low_CI=rep(0,27),
                               Mean=rep(0,27),Up_CI=rep(0,27)) # Sorry, not in a mood to reinvent, how to .... do it aesthetically
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


#text(0,-1,"Slope estimate", cex=1, pos=1, offset=1, xpd=NA)
text("Mean", x=1.75, y=max(ys-pls), cex=cex_t2, offset=0.5, xpd=NA, srt=0)
text("2.5 %", x=1.25, y=max(ys-pls), cex=cex_t2, offset=0.5, xpd=NA, srt=0)
text("97.5 %", x=2.25, y=max(ys-pls), cex=cex_t2, offset=0.5, xpd=NA, srt=0)


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
  text(format(round(Coefficients_check$Mean[i],2),nsmall=2), x=1.75, y=ys[i], cex=cex_t)
  text(format(round(Coefficients_check$Low_CI[i],2),nsmall=2), x=1.25,y=ys[i], cex=cex_t)
  text(format(round(Coefficients_check$Up_CI[i],2),nsmall=2), x=2.25,y=ys[i], cex=cex_t)
  for (i in 1:27) {
    c(polygon(c(dens$x,rev(dens$x)),ys[i]*(c(dens$y,rev(dens$y)*(-1))*0.003)+ycoord[i],border=F, lwd = 1.5, col=colpol))
  }
  lines(x=PI(toplot_CI,prob=0.95),rep(ycoord+0.05,2),lwd=2, lty=1, col='#9090DDDF')
  segments(x0 = ci[1], y0 = ycoord-0.15, x1 = ci[1], y1 = ycoord+0.25, lwd = 3, col = '#9090DDDF')
  segments(x0 = ci[2], y0 = ycoord-0.15, x1 = ci[2], y1 = ycoord+0.25, lwd = 3, col = '#9090DDDF')
  points(x=mean(toplot_CI),ycoord,lwd=2,pch=21,col=1,bg=0,cex=1.2)
}


# 4 out 4: Sclera Colour - MEN


par(mar=c(3.1, 0.5, 1.1, 0.5),mgp=c(5,0.7,2))
plot(NULL, xlim=c(-0.75, 2.5), ylim=c(1, 28.5), yaxt="n", bty="n", ylab="", xlab="", cex.axis=0.9, 
     main="Sclera Colour: MEN")


rect(-0.75, 0, 1, 29, col="#10101010", border=NA)

segments(-0.75, c(seq(from=1, to=9, by=1), seq(from=10.5, to=18.5, by=1), seq(from=20, to=28, by=1)), 1, 
         c(seq(from=1, to=9, by=1), seq(from=10.5, to=18.5, by=1), seq(from=20, to=28, by=1)), col="#40808080")

segments(-0.75, 9, 1, 9, col="#908080")
rect(-0.75, 8.5, 1, 9.5, col="#10101010", border=NA)

segments(-0.75, 18.5, 1, 18.5, col="#908080")
rect(-0.75, 18, 1, 19, col="#10101010", border=NA)

segments(-0.75, 28, 1, 28, col="#908080")
rect(-0.75, 27.5, 1, 28.5, col="#10101010", border=NA)

segments(c(-0.75, -0.25, 0.25, 0.75), 0, c(-0.75, -0.25, 0.25, 0.75), 29, col="#80808080", lty=2)
segments(0, 0, 0, 29, col="#990909", lwd=2)


include <- list(
  # L_Sclera:
  A_post_tog$B_L_sclera[,2], # GLOBAL - MEN
  (A_post_tog$B_L_sclera[,2] + A_post_tog$betas[,1,22]), # L_sclera men, Cameroon 2013
  (A_post_tog$B_L_sclera[,2] + A_post_tog$betas[,2,22]), # L_sclera men, Cameroon 2016
  (A_post_tog$B_L_sclera[,2] + A_post_tog$betas[,3,22]), # L_sclera men, Colombia
  (A_post_tog$B_L_sclera[,2] + A_post_tog$betas[,4,22]), # L_Sclera men, CZ 2016
  (A_post_tog$B_L_sclera[,2] + A_post_tog$betas[,5,22]), # L_sclera men, CZ 2019
  (A_post_tog$B_L_sclera[,2] + A_post_tog$betas[,6,22]), # L_sclera men, India
  (A_post_tog$B_L_sclera[,2] + A_post_tog$betas[,7,22]), # L_sclera men, Turkey
  (A_post_tog$B_L_sclera[,2] + A_post_tog$betas[,8,22]), # L_sclera men, Vietnam

  # A_Sclera
  A_post_tog$B_a_sclera[,2], # GLOBAL - MEN
  (A_post_tog$B_a_sclera[,2] + A_post_tog$betas[,1,23]), # a_sclera men, Cameroon 2013
  (A_post_tog$B_a_sclera[,2] + A_post_tog$betas[,2,23]), # a_sclera men, Cameroon 2016
  (A_post_tog$B_a_sclera[,2] + A_post_tog$betas[,3,23]), # a_sclera men, Colombia
  (A_post_tog$B_a_sclera[,2] + A_post_tog$betas[,4,23]), # a_sclera men, CZ 2016
  (A_post_tog$B_a_sclera[,2] + A_post_tog$betas[,5,23]), # a_sclera men, CZ 2019
  (A_post_tog$B_a_sclera[,2] + A_post_tog$betas[,6,23]), # a_sclera men, India
  (A_post_tog$B_a_sclera[,2] + A_post_tog$betas[,7,23]), # a_sclera men, Turkey
  (A_post_tog$B_a_sclera[,2] + A_post_tog$betas[,8,23]), # a_sclera men, Vietnam

  # B_Sclera
  A_post_tog$B_b_sclera[,2], # GLOBAL - MEN
  (A_post_tog$B_b_sclera[,2] + A_post_tog$betas[,1,24]), # b_sclera men, Cameroon 2013
  (A_post_tog$B_b_sclera[,2] + A_post_tog$betas[,2,24]), # b_sclera men, Cameroon 2016
  (A_post_tog$B_b_sclera[,2] + A_post_tog$betas[,3,24]), # b_sclera men, Colombia
  (A_post_tog$B_b_sclera[,2] + A_post_tog$betas[,4,24]), # b_sclera men, CZ 2016
  (A_post_tog$B_b_sclera[,2] + A_post_tog$betas[,5,24]), # b_sclera men, CZ 2019
  (A_post_tog$B_b_sclera[,2] + A_post_tog$betas[,6,24]), # b_sclera men, India
  (A_post_tog$B_b_sclera[,2] + A_post_tog$betas[,7,24]), # b_sclera men, Turkey
  (A_post_tog$B_b_sclera[,2] + A_post_tog$betas[,8,24]) # b_sclera men, Vietnam
)


Coefficients_check<-data.frame(Name=labels,Low_CI=rep(0,27),
                               Mean=rep(0,27),Up_CI=rep(0,27)) 
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


#text(0,-1,"Slope estimate", cex=1, pos=1, offset=1, xpd=NA)
text("Mean", x=1.75, y=max(ys-pls), cex=cex_t2, offset=0.5, xpd=NA, srt=0)
text("2.5 %", x=1.25, y=max(ys-pls), cex=cex_t2, offset=0.5, xpd=NA, srt=0)
text("97.5 %", x=2.25, y=max(ys-pls), cex=cex_t2, offset=0.5, xpd=NA, srt=0)


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
  text(format(round(Coefficients_check$Mean[i],2),nsmall=2), x=1.75, y=ys[i], cex=cex_t)
  text(format(round(Coefficients_check$Low_CI[i],2),nsmall=2), x=1.25,y=ys[i], cex=cex_t)
  text(format(round(Coefficients_check$Up_CI[i],2),nsmall=2), x=2.25,y=ys[i], cex=cex_t)
  for (i in 1:27) {
    c(polygon(c(dens$x,rev(dens$x)),ys[i]*(c(dens$y,rev(dens$y)*(-1))*0.003)+ycoord[i],border=F, lwd = 1.5, col=colpol))
  }
  lines(x=PI(toplot_CI,prob=0.95),rep(ycoord+0.05,2),lwd=2, lty=1, col='#9090DDDF')
  segments(x0 = ci[1], y0 = ycoord-0.15, x1 = ci[1], y1 = ycoord+0.25, lwd = 3, col = '#9090DDDF')
  segments(x0 = ci[2], y0 = ycoord-0.15, x1 = ci[2], y1 = ycoord+0.25, lwd = 3, col = '#9090DDDF')
  points(x=mean(toplot_CI),ycoord,lwd=2,pch=21,col=1,bg=0,cex=1.2)
}

dev.off()




#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.
#
#  Perceived sex-typicality - data were standardised together
# 
#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.


# Predictors are the same, but there is then one culture less (Vietnam)

str(S_post_tog)

# betas [,1,1] -> intercept in 1 + (Sex - 1)*12 F = 1, M = 2, that is: 1 + (1-1)*12 = 1 for women; 1 + (2-1)*12 -> 13 for men 
# but there are only eight cultures (no Vietnam)
# therefore: 
# betas[,1,1] -> Intercept women, Cameroon 2013
# betas[,1,13] -> Intercept men, Cameroon 2013
# betas[,2,1] -> Intercept women, Cameroon 2016
# betas[,2,13] -> Intercept men, Cameroon 2016
# betas[,3,1] -> Intercept women, Colombia
# betas[,3,13] -> Intercept men, Colombia
# betas[,4,1] -> Intercept women, CZ 2016
# betas[,4,13] -> Intercept men, CZ 2016
# betas[,5,1] -> Intercept women, CZ 2019
# betas[,5,13] -> Intercept men, CZ 2019
# betas[,6,1] -> Intercept women, India
# betas[,6,13] -> Intercept men, India
# betas[,7,1] -> Intercept women, Iran
# betas[,7,13] -> Intercept men, Iran
# betas[,8,1] -> Intercept women, Turkey
# betas[,8,13] -> Intercept men, Turkey

# No Vietnam, but the rest is the same.

tiff("Posterior__SEXT_Preds_TOGETHER_STANDARDISED_without_Iranians.tif",width=32,height=26,units="cm",res=600,compression = "lzw")
layout(matrix(1:4,byrow=T,nrow=1), widths=c(2.9,2,2,2),heights=1)

par(mar=c(5.1, 10.1, 1.1, 1.1), mgp=c(5, 0.7, 2))
plot(NULL, xlim=c(-0.75, 2.5), ylim=c(1, 25.5), yaxt="n", bty="n", ylab="", cex.axis=0.7, 
     main="Iris Colour: WOMEN")

rect(-0.75, 0, 1, 26.5, col="#10101010", border=NA)

segments(-0.75, c(seq(from=1, to=8, by=1), seq(from=9.5, to=16.5, by=1), seq(from=18, to=25, by=1)), 1, 
         c(seq(from=1, to=8, by=1), seq(from=9.5, to=16.5, by=1), seq(from=18, to=25, by=1)), col="#40808080")

segments(-0.75, 8, 1, 8, col="#908080", lwd=2)
rect(-0.75, 7.5, 1, 8.5, col="#10101010", border=NA)

segments(-0.75, 16.5, 1, 16.5, col="#908080", lwd=2)
rect(-0.75, 16, 1, 17, col="#10101010", border=NA)

segments(-0.75, 25, 1, 25, col="#908080", lwd=2)
rect(-0.75, 24.5, 1, 25.5, col="#10101010", border=NA)

segments(c(-0.75, -0.25, 0.25, 0.75), 0, c(-0.75, -0.25, 0.25, 0.75), 26, col="#80808080", lty=2)
segments(0, 0, 0, 26, col="#990909", lwd=2)


include <- list(
  # L_iris:
  S_post_tog$B_L_iris[,1], # GLOBAL - WOMEN
  (S_post_tog$B_L_iris[,1] + S_post_tog$betas[,1,7]), # L_iris women, Cameroon 2013
  (S_post_tog$B_L_iris[,1] + S_post_tog$betas[,2,7]), # L_iris women, Cameroon 2016
  (S_post_tog$B_L_iris[,1] + S_post_tog$betas[,3,7]), # L_iris women, Colombia
  (S_post_tog$B_L_iris[,1] + S_post_tog$betas[,4,7]), # L_iris women, CZ 2016
  (S_post_tog$B_L_iris[,1] + S_post_tog$betas[,5,7]), # L_iris women, CZ 2019
  (S_post_tog$B_L_iris[,1] + S_post_tog$betas[,6,7]), # L_iris women, India
  (S_post_tog$B_L_iris[,1] + S_post_tog$betas[,7,7]), # L_iris women, Turkey

  # A_iris
  S_post_tog$B_a_iris[,1], # GLOBAL - WOMEN
  (S_post_tog$B_a_iris[,1] + S_post_tog$betas[,1,8]), # a_iris women, Cameroon 2013
  (S_post_tog$B_a_iris[,1] + S_post_tog$betas[,2,8]), # a_iris women, Cameroon 2016
  (S_post_tog$B_a_iris[,1] + S_post_tog$betas[,3,8]), # a_iris women, Colombia
  (S_post_tog$B_a_iris[,1] + S_post_tog$betas[,4,8]), # a_iris women, CZ 2016
  (S_post_tog$B_a_iris[,1] + S_post_tog$betas[,5,8]), # a_iris women, CZ 2019
  (S_post_tog$B_a_iris[,1] + S_post_tog$betas[,6,8]), # a_iris women, India
  (S_post_tog$B_a_iris[,1] + S_post_tog$betas[,7,8]), # a_iris women, Turkey

  # B_iris
  S_post_tog$B_b_iris[,1], # GLOBAL - WOMEN
  (S_post_tog$B_b_iris[,1] + S_post_tog$betas[,1,9]), # b_iris women, Cameroon 2013
  (S_post_tog$B_b_iris[,1] + S_post_tog$betas[,2,9]), # b_iris women, Cameroon 2016
  (S_post_tog$B_b_iris[,1] + S_post_tog$betas[,3,9]), # b_iris women, Colombia
  (S_post_tog$B_b_iris[,1] + S_post_tog$betas[,4,9]), # b_iris women, CZ 2016
  (S_post_tog$B_b_iris[,1] + S_post_tog$betas[,5,9]), # b_iris women, CZ 2019
  (S_post_tog$B_b_iris[,1] + S_post_tog$betas[,6,9]), # b_iris women, India
  (S_post_tog$B_b_iris[,1] + S_post_tog$betas[,7,9]) # b_iris women, Turkey
)



cols<-c("#00FBFF", 
        "#8C00FF",
        "#00FF33", 
        "#FFEA00",
        "#FFA600",
        
        "#00FBFF", 
        "#8C00FF",
        "#00FF33", 
        "#FFEA00",
        
        "#00FBFF", 
        "#8C00FF",
        "#00FF33", 
        "#FFEA00",
        "#FFA600",
        
        "#00FBFF", 
        "#8C00FF",
        "#00FF33", 
        "#FFEA00",
        
        "#00FBFF", 
        "#8C00FF",
        "#00FF33", 
        "#FFEA00",
        "#FFA600",
        
        "#00FBFF" 
)

cols<-paste(cols,"80",sep="") 

ys<- c(seq(from=1, to=8, by=1), seq(from=9.5, to=16.5, by=1), seq(from=18, to=25, by=1))
ys<-rev(ys)

labels <- c(  "L* Global",
              "L* Cameroon 13",
              "L* Cameroon 16",
              "L* Colombia",
              "L* Czechia 16",
              "L* Czechia 19",
              "L* India - FCD",
              "L* Turkey",
              # A_iris
              "a* Global",
              "a* Cameroon 13",
              "a* Cameroon 16",
              "a* Colombia",
              "a* Czechia 16",
              "a* Czechia 19",
              "a* India - FCD",
              "a* Turkey",
              # B_iris
              "b* Global",
              "b* Cameroon 13",
              "b* Cameroon 16",
              "b* Colombia",
              "b* Czechia 16",
              "b* Czechia 19",
              "b* India - FCD",
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

cex_t <- 1.15
cex_t2 <- 1.05
pls <- -0.9
#text(0,-1,"Slope estimate", cex=1, pos=1, offset=1, xpd=NA)
text("Mean", x=1.75, y=max(ys-pls), cex=cex_t2, offset=0.5, xpd=NA, srt=0)
text("2.5 %", x=1.25, y=max(ys-pls), cex=cex_t2, offset=0.5, xpd=NA, srt=0)
text("97.5 %", x=2.25, y=max(ys-pls), cex=cex_t2, offset=0.5, xpd=NA, srt=0)


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
  axis(2,at=ycoord,labels=textlab,srt=0,las=1, cex.axis=1.2) 
  text(format(round(Coefficients_check$Mean[i],2),nsmall=2), x=1.75, y=ys[i], cex=cex_t)
  text(format(round(Coefficients_check$Low_CI[i],2),nsmall=2), x=1.25,y=ys[i], cex=cex_t)
  text(format(round(Coefficients_check$Up_CI[i],2),nsmall=2), x=2.25,y=ys[i], cex=cex_t)
  for (i in 1:24) {
    c(polygon(c(dens$x,rev(dens$x)),ys[i]*(c(dens$y,rev(dens$y)*(-1))*0.003)+ycoord[i],border=F, lwd = 1.5, col=colpol))
  }
  lines(x=PI(toplot_CI,prob=0.95),rep(ycoord+0.05,2),lwd=2, lty=1, col='#9090DDDF')
  segments(x0 = ci[1], y0 = ycoord-0.15, x1 = ci[1], y1 = ycoord+0.25, lwd = 3, col = '#9090DDDF')
  segments(x0 = ci[2], y0 = ycoord-0.15, x1 = ci[2], y1 = ycoord+0.25, lwd = 3, col = '#9090DDDF')
  points(x=mean(toplot_CI),ycoord,lwd=2,pch=21,col=1,bg=0,cex=1.2)
}


# SCLERA: 
par(mar=c(5.1, 1.1, 1.1, 1.1), mgp=c(5, 0.7, 2))
plot(NULL,xlim=c(-0.7,2.5),ylim=c(1,25.5),yaxt="n",bty="n", ylab="", cex.axis=0.7, 
     main="Sclera Colour: WOMEN")

rect(-0.75, 0, 1, 26.5, col="#10101010", border=NA)

segments(-0.75, c(seq(from=1, to=8, by=1), seq(from=9.5, to=16.5, by=1), seq(from=18, to=25, by=1)), 1, 
         c(seq(from=1, to=8, by=1), seq(from=9.5, to=16.5, by=1), seq(from=18, to=25, by=1)), col="#40808080")

segments(-0.75, 8, 1, 8, col="#908080", lwd=2)
rect(-0.75, 7.5, 1, 8.5, col="#10101010", border=NA)

segments(-0.75, 16.5, 1, 16.5, col="#908080", lwd=2)
rect(-0.75, 16, 1, 17, col="#10101010", border=NA)

segments(-0.75, 25, 1, 25, col="#908080", lwd=2)
rect(-0.75, 24.5, 1, 25.5, col="#10101010", border=NA)

segments(c(-0.75, -0.25, 0.25, 0.75), 0, c(-0.75, -0.25, 0.25, 0.75), 26, col="#80808080", lty=2)
segments(0, 0, 0, 26, col="#990909", lwd=2)

include <- list(
  # SCLERA L:
  S_post_tog$B_L_sclera[,1], # GLOBAL - WOMEN
  (S_post_tog$B_L_sclera[,1] + S_post_tog$betas[,1,10]), # L_sclera women, Cameroon 2013
  (S_post_tog$B_L_sclera[,1] + S_post_tog$betas[,2,10]), # L_sclera women, Cameroon 2016
  (S_post_tog$B_L_sclera[,1] + S_post_tog$betas[,3,10]), # L_sclera women, Colombia
  (S_post_tog$B_L_sclera[,1] + S_post_tog$betas[,4,10]), # L_sclera women, CZ 2016
  (S_post_tog$B_L_sclera[,1] + S_post_tog$betas[,5,10]), # L_sclera women, CZ 2019
  (S_post_tog$B_L_sclera[,1] + S_post_tog$betas[,6,10]), # L_sclera women, India
  (S_post_tog$B_L_sclera[,1] + S_post_tog$betas[,7,10]), # L_sclera women, Turkey

  # SCLERA A
  S_post_tog$B_a_sclera[,1], # GLOBAL - WOMEN
  (S_post_tog$B_a_sclera[,1] + S_post_tog$betas[,1,11]), # a_sclera women, Cameroon 2013
  (S_post_tog$B_a_sclera[,1] + S_post_tog$betas[,2,11]), # a_sclera women, Cameroon 2016
  (S_post_tog$B_a_sclera[,1] + S_post_tog$betas[,3,11]), # a_sclera women, Colombia
  (S_post_tog$B_a_sclera[,1] + S_post_tog$betas[,4,11]), # a_sclera women, CZ 2016
  (S_post_tog$B_a_sclera[,1] + S_post_tog$betas[,5,11]), # a_sclera women, CZ 2019
  (S_post_tog$B_a_sclera[,1] + S_post_tog$betas[,6,11]), # a_sclera women, India
  (S_post_tog$B_a_sclera[,1] + S_post_tog$betas[,7,11]), # a_sclera women, Turkey

  # SCLERA B
  S_post_tog$B_b_sclera[,1], # GLOBAL - WOMEN
  (S_post_tog$B_b_sclera[,1] + S_post_tog$betas[,1,12]), # b_sclera women, Cameroon 2013
  (S_post_tog$B_b_sclera[,1] + S_post_tog$betas[,2,12]), # b_sclera women, Cameroon 2016
  (S_post_tog$B_b_sclera[,1] + S_post_tog$betas[,3,12]), # b_sclera women, Colombia
  (S_post_tog$B_b_sclera[,1] + S_post_tog$betas[,4,12]), # b_sclera women, CZ 2016
  (S_post_tog$B_b_sclera[,1] + S_post_tog$betas[,5,12]), # b_sclera women, CZ 2019
  (S_post_tog$B_b_sclera[,1] + S_post_tog$betas[,6,12]), # b_sclera women, India
  (S_post_tog$B_b_sclera[,1] + S_post_tog$betas[,7,12]) # b_sclera women, Turkey
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


#text(0,-1,"Slope estimate", cex=1, pos=1, offset=1, xpd=NA)
text("Mean", x=1.75, y=max(ys-pls), cex=cex_t2, offset=0.5, xpd=NA, srt=0)
text("2.5 %", x=1.25, y=max(ys-pls), cex=cex_t2, offset=0.5, xpd=NA, srt=0)
text("97.5 %", x=2.25, y=max(ys-pls), cex=cex_t2, offset=0.5, xpd=NA, srt=0)


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
  text(format(round(Coefficients_check$Mean[i],2),nsmall=2), x=1.75, y=ys[i], cex=cex_t)
  text(format(round(Coefficients_check$Low_CI[i],2),nsmall=2), x=1.25,y=ys[i], cex=cex_t)
  text(format(round(Coefficients_check$Up_CI[i],2),nsmall=2), x=2.25,y=ys[i], cex=cex_t)
  for (i in 1:24) {
    c(polygon(c(dens$x,rev(dens$x)),ys[i]*(c(dens$y,rev(dens$y)*(-1))*0.003)+ycoord[i],border=F, lwd = 1.5, col=colpol))
  }
  lines(x=PI(toplot_CI,prob=0.95),rep(ycoord+0.05,2),lwd=2, lty=1, col='#9090DDDF')
  segments(x0 = ci[1], y0 = ycoord-0.15, x1 = ci[1], y1 = ycoord+0.25, lwd = 3, col = '#9090DDDF')
  segments(x0 = ci[2], y0 = ycoord-0.15, x1 = ci[2], y1 = ycoord+0.25, lwd = 3, col = '#9090DDDF')
  points(x=mean(toplot_CI),ycoord,lwd=2,pch=21,col=1,bg=0,cex=1.2)
}


# MEN: 

par(mar=c(5.1, 1.1, 1.1, 1.1), mgp=c(5, 0.7, 2))
plot(NULL,xlim=c(-0.7,2.5),ylim=c(1,25.5),yaxt="n",bty="n", ylab="", cex.axis=0.7,  
     main="Iris Colour: MEN")
rect(-0.75, 0, 1, 26.5, col="#10101010", border=NA)

segments(-0.75, c(seq(from=1, to=8, by=1), seq(from=9.5, to=16.5, by=1), seq(from=18, to=25, by=1)), 1, 
         c(seq(from=1, to=8, by=1), seq(from=9.5, to=16.5, by=1), seq(from=18, to=25, by=1)), col="#40808080")

segments(-0.75, 8, 1, 8, col="#908080", lwd=2)
rect(-0.75, 7.5, 1, 8.5, col="#10101010", border=NA)

segments(-0.75, 16.5, 1, 16.5, col="#908080", lwd=2)
rect(-0.75, 16, 1, 17, col="#10101010", border=NA)

segments(-0.75, 25, 1, 25, col="#908080", lwd=2)
rect(-0.75, 24.5, 1, 25.5, col="#10101010", border=NA)

segments(c(-0.75, -0.25, 0.25, 0.75), 0, c(-0.75, -0.25, 0.25, 0.75), 26, col="#80808080", lty=2)
segments(0, 0, 0, 26, col="#990909", lwd=2)


include <- list(
  # L_iris:
  S_post_tog$B_L_iris[,2], # GLOBAL - MEN
  (S_post_tog$B_L_iris[,2] + S_post_tog$betas[,1,19]), # L_iris men, Cameroon 2013
  (S_post_tog$B_L_iris[,2] + S_post_tog$betas[,2,19]), # L_iris men, Cameroon 2016
  (S_post_tog$B_L_iris[,2] + S_post_tog$betas[,3,19]), # L_iris men, Colombia
  (S_post_tog$B_L_iris[,2] + S_post_tog$betas[,4,19]), # L_iris men, CZ 2016
  (S_post_tog$B_L_iris[,2] + S_post_tog$betas[,5,19]), # L_iris men, CZ 2019
  (S_post_tog$B_L_iris[,2] + S_post_tog$betas[,6,19]), # L_iris men, India
  (S_post_tog$B_L_iris[,2] + S_post_tog$betas[,7,19]), # L_iris men, Turkey

  # A_iris
  S_post_tog$B_a_iris[,2], # GLOBAL - WOMEN
  (S_post_tog$B_a_iris[,2] + S_post_tog$betas[,1,20]), # a_iris men, Cameroon 2013
  (S_post_tog$B_a_iris[,2] + S_post_tog$betas[,2,20]), # a_iris men, Cameroon 2016
  (S_post_tog$B_a_iris[,2] + S_post_tog$betas[,3,20]), # a_iris men, Colombia
  (S_post_tog$B_a_iris[,2] + S_post_tog$betas[,4,20]), # a_iris men, CZ 2016
  (S_post_tog$B_a_iris[,2] + S_post_tog$betas[,5,20]), # a_iris men, CZ 2019
  (S_post_tog$B_a_iris[,2] + S_post_tog$betas[,6,20]), # a_iris men, India
  (S_post_tog$B_a_iris[,2] + S_post_tog$betas[,7,20]), # a_iris men, Turkey

  # B_iris
  S_post_tog$B_b_iris[,2], # GLOBAL - WOMEN
  (S_post_tog$B_b_iris[,2] + S_post_tog$betas[,1,21]), # b_iris men, Cameroon 2013
  (S_post_tog$B_b_iris[,2] + S_post_tog$betas[,2,21]), # b_iris men, Cameroon 2016
  (S_post_tog$B_b_iris[,2] + S_post_tog$betas[,3,21]), # b_iris men, Colombia
  (S_post_tog$B_b_iris[,2] + S_post_tog$betas[,4,21]), # b_iris men, CZ 2016
  (S_post_tog$B_b_iris[,2] + S_post_tog$betas[,5,21]), # b_iris men, CZ 2019
  (S_post_tog$B_b_iris[,2] + S_post_tog$betas[,6,21]), # b_iris men, India
  (S_post_tog$B_b_iris[,2] + S_post_tog$betas[,7,21]) # b_iris men, Turkey
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


#text(0,-1,"Slope estimate", cex=1, pos=1, offset=1, xpd=NA)
text("Mean", x=1.75, y=max(ys-pls), cex=cex_t2, offset=0.5, xpd=NA, srt=0)
text("2.5 %", x=1.25, y=max(ys-pls), cex=cex_t2, offset=0.5, xpd=NA, srt=0)
text("97.5 %", x=2.25, y=max(ys-pls), cex=cex_t2, offset=0.5, xpd=NA, srt=0)


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
  text(format(round(Coefficients_check$Mean[i],2),nsmall=2), x=1.75, y=ys[i], cex=cex_t)
  text(format(round(Coefficients_check$Low_CI[i],2),nsmall=2), x=1.25,y=ys[i], cex=cex_t)
  text(format(round(Coefficients_check$Up_CI[i],2),nsmall=2), x=2.25,y=ys[i], cex=cex_t)
  for (i in 1:24) {
    c(polygon(c(dens$x,rev(dens$x)),ys[i]*(c(dens$y,rev(dens$y)*(-1))*0.003)+ycoord[i],border=F, lwd = 1.5, col=colpol))
  }
  lines(x=PI(toplot_CI,prob=0.95),rep(ycoord+0.05,2),lwd=2, lty=1, col='#9090DDDF')
  segments(x0 = ci[1], y0 = ycoord-0.15, x1 = ci[1], y1 = ycoord+0.25, lwd = 3, col = '#9090DDDF')
  segments(x0 = ci[2], y0 = ycoord-0.15, x1 = ci[2], y1 = ycoord+0.25, lwd = 3, col = '#9090DDDF')
  points(x=mean(toplot_CI),ycoord,lwd=2,pch=21,col=1,bg=0,cex=1.2)
}


# SCLERA_

par(mar=c(5.1, 1.1, 1.1, 1.1), mgp=c(5, 0.7, 2))
plot(NULL,xlim=c(-0.7,2.5),ylim=c(1,25.5),yaxt="n",bty="n", ylab="", cex.axis=0.7,  
     main="Sclera Colour: MEN")

rect(-0.75, 0, 1, 26.5, col="#10101010", border=NA)

segments(-0.75, c(seq(from=1, to=8, by=1), seq(from=9.5, to=16.5, by=1), seq(from=18, to=25, by=1)), 1, 
         c(seq(from=1, to=8, by=1), seq(from=9.5, to=16.5, by=1), seq(from=18, to=25, by=1)), col="#40808080")

segments(-0.75, 8, 1, 8, col="#908080", lwd=2)
rect(-0.75, 7.5, 1, 8.5, col="#10101010", border=NA)

segments(-0.75, 16.5, 1, 16.5, col="#908080", lwd=2)
rect(-0.75, 16, 1, 17, col="#10101010", border=NA)

segments(-0.75, 25, 1, 25, col="#908080", lwd=2)
rect(-0.75, 24.5, 1, 25.5, col="#10101010", border=NA)

segments(c(-0.75, -0.25, 0.25, 0.75), 0, c(-0.75, -0.25, 0.25, 0.75), 26, col="#80808080", lty=2)
segments(0, 0, 0, 26, col="#990909", lwd=2)

include <- list(
  # L_Sclera:
  S_post_tog$B_L_sclera[,2], # GLOBAL - MEN
  (S_post_tog$B_L_sclera[,2] + S_post_tog$betas[,1,22]), # L_sclera men, Cameroon 2013
  (S_post_tog$B_L_sclera[,2] + S_post_tog$betas[,2,22]), # L_sclera men, Cameroon 2016
  (S_post_tog$B_L_sclera[,2] + S_post_tog$betas[,3,22]), # L_sclera men, Colombia
  (S_post_tog$B_L_sclera[,2] + S_post_tog$betas[,4,22]), # L_Sclera men, CZ 2016
  (S_post_tog$B_L_sclera[,2] + S_post_tog$betas[,5,22]), # L_sclera men, CZ 2019
  (S_post_tog$B_L_sclera[,2] + S_post_tog$betas[,6,22]), # L_sclera men, India
  (S_post_tog$B_L_sclera[,2] + S_post_tog$betas[,7,22]), # L_sclera men, Turkey

  # A_Sclera
  S_post_tog$B_a_sclera[,2], # GLOBAL - MEN
  (S_post_tog$B_a_sclera[,2] + S_post_tog$betas[,1,23]), # a_sclera men, Cameroon 2013
  (S_post_tog$B_a_sclera[,2] + S_post_tog$betas[,2,23]), # a_sclera men, Cameroon 2016
  (S_post_tog$B_a_sclera[,2] + S_post_tog$betas[,3,23]), # a_sclera men, Colombia
  (S_post_tog$B_a_sclera[,2] + S_post_tog$betas[,4,23]), # a_sclera men, CZ 2016
  (S_post_tog$B_a_sclera[,2] + S_post_tog$betas[,5,23]), # a_sclera men, CZ 2019
  (S_post_tog$B_a_sclera[,2] + S_post_tog$betas[,6,23]), # a_sclera men, India
  (S_post_tog$B_a_sclera[,2] + S_post_tog$betas[,7,23]), # a_sclera men, Turkey

  # B_Sclera
  S_post_tog$B_b_sclera[,2], # GLOBAL - MEN
  (S_post_tog$B_b_sclera[,2] + S_post_tog$betas[,1,24]), # b_sclera men, Cameroon 2013
  (S_post_tog$B_b_sclera[,2] + S_post_tog$betas[,2,24]), # b_sclera men, Cameroon 2016
  (S_post_tog$B_b_sclera[,2] + S_post_tog$betas[,3,24]), # b_sclera men, Colombia
  (S_post_tog$B_b_sclera[,2] + S_post_tog$betas[,4,24]), # b_sclera men, CZ 2016
  (S_post_tog$B_b_sclera[,2] + S_post_tog$betas[,5,24]), # b_sclera men, CZ 2019
  (S_post_tog$B_b_sclera[,2] + S_post_tog$betas[,6,24]), # b_sclera men, India
  (S_post_tog$B_b_sclera[,2] + S_post_tog$betas[,7,24]) # b_sclera men, Turkey
)


Coefficients_check<-data.frame(Name=labels,Low_CI=rep(0,24),
                               Mean=rep(0,24),Up_CI=rep(0,24)) 
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


#text(0,-1,"Slope estimate", cex=1, pos=1, offset=1, xpd=NA)
text("Mean", x=1.75, y=max(ys-pls), cex=cex_t2, offset=0.5, xpd=NA, srt=0)
text("2.5 %", x=1.25, y=max(ys-pls), cex=cex_t2, offset=0.5, xpd=NA, srt=0)
text("97.5 %", x=2.25, y=max(ys-pls), cex=cex_t2, offset=0.5, xpd=NA, srt=0)


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
  text(format(round(Coefficients_check$Mean[i],2),nsmall=2), x=1.75, y=ys[i], cex=cex_t)
  text(format(round(Coefficients_check$Low_CI[i],2),nsmall=2), x=1.25,y=ys[i], cex=cex_t)
  text(format(round(Coefficients_check$Up_CI[i],2),nsmall=2), x=2.25,y=ys[i], cex=cex_t)
  for (i in 1:24) {
    c(polygon(c(dens$x,rev(dens$x)),ys[i]*(c(dens$y,rev(dens$y)*(-1))*0.003)+ycoord[i],border=F, lwd = 1.5, col=colpol))
  }
  lines(x=PI(toplot_CI,prob=0.95),rep(ycoord+0.05,2),lwd=2, lty=1, col='#9090DDDF')
  segments(x0 = ci[1], y0 = ycoord-0.15, x1 = ci[1], y1 = ycoord+0.25, lwd = 3, col = '#9090DDDF')
  segments(x0 = ci[2], y0 = ycoord-0.15, x1 = ci[2], y1 = ycoord+0.25, lwd = 3, col = '#9090DDDF')
  points(x=mean(toplot_CI),ycoord,lwd=2,pch=21,col=1,bg=0,cex=1.2)
}

dev.off()


#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.
#
# Standardised together - Without Skin L* 
#
#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.

setwd("D:/GACR_VF/Measured_Eye_Phenotype_And_Ascribed_Characteristics_In_Different_Countries/Final Figures Draft Data/Model Objects Alternatives/DONE_Standardised_together_skin_lightness_not_included")

load("Model_11_ATTRACT_both_sexes_standardised_together_Without_Skin_L.Rdata")
load("Model_12_SEXTYP_both_sexes_standardised_togetherWithout_Skin_L_.Rdata")


A_post_tog <- extract.samples(m11_attr)
S_post_tog <- extract.samples(m12_Sext)

#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.
#
# Attractiveness 
#
#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.


tiff("Posterior__ATTR_Preds_TOGETHER_STANDARDISED_Without_Skin_L.tif",width=32,height=26,units="cm",res=600,compression = "lzw")
layout(matrix(1:4,byrow=T,nrow=1), widths=c(2.9,2,2,2),heights=1)

# 1 out 4: Iris Colour - WOMEN

par(mar=c(3.1, 7.1, 1.1, 1.1),mgp=c(5,0.7,2))
plot(NULL,xlim=c(-0.75,2.5),ylim=c(1,31.5),yaxt="n",bty="n", ylab="", xlab="", cex.axis=0.9, 
     main="Iris Colour: WOMEN")

rect(-0.75, 0, 1, 32, col="#10101010", border=NA)

segments(-0.75, c(seq(from=1, to=10, by=1),seq(from=11.5, to=20.5, by=1),seq(from=22, to=31, by=1)), 1, 
         c(seq(from=1, to=10, by=1),seq(from=11.5, to=20.5, by=1),seq(from=22, to=31, by=1)), col="#40808080")

segments(-0.75, 10, 1, 10, col="#908080")
rect(-0.75, 9.5, 1, 10.5, col="#10101010", border=NA)

segments(-0.75, 20.5, 1, 20.5, col="#908080")
rect(-0.75, 20, 1, 21, col="#10101010", border=NA)

segments(-0.75, 31, 1, 31, col="#908080")
rect(-0.75, 30.5, 1, 31.5, col="#10101010", border=NA)


segments(c(-0.75,-0.25,0.25,0.75),0,c(-0.75,-0.25,0.25,0.75),32,col="#80808080",lty=2)
segments(0,0,0,32,col="#990909", lwd=2)

# Just hit the right posteriors - that's enough: 
str(A_post_tog) # BETAS: num[1:10000, NINE SAMPLES, 24 SLOPES - 12 Women, 12 Men)

include <- list(
  # L_iris:
  A_post_tog$B_L_iris[,1], # GLOBAL - WOMEN
  (A_post_tog$B_L_iris[,1] + A_post_tog$betas[,1,6]), # L_iris women, Cameroon 2013
  (A_post_tog$B_L_iris[,1] + A_post_tog$betas[,2,6]), # L_iris women, Cameroon 2016
  (A_post_tog$B_L_iris[,1] + A_post_tog$betas[,3,6]), # L_iris women, Colombia
  (A_post_tog$B_L_iris[,1] + A_post_tog$betas[,4,6]), # L_iris women, CZ 2016
  (A_post_tog$B_L_iris[,1] + A_post_tog$betas[,5,6]), # L_iris women, CZ 2019
  (A_post_tog$B_L_iris[,1] + A_post_tog$betas[,6,6]), # L_iris women, India
  (A_post_tog$B_L_iris[,1] + A_post_tog$betas[,7,6]), # L_iris women, Iran
  (A_post_tog$B_L_iris[,1] + A_post_tog$betas[,8,6]), # L_iris women, Turkey
  (A_post_tog$B_L_iris[,1] + A_post_tog$betas[,9,6]), # L_iris women, Vietnam
  
  # A_iris
  A_post_tog$B_a_iris[,1], # GLOBAL - WOMEN
  (A_post_tog$B_a_iris[,1] + A_post_tog$betas[,1,7]), # a_iris women, Cameroon 2013
  (A_post_tog$B_a_iris[,1] + A_post_tog$betas[,2,7]), # a_iris women, Cameroon 2016
  (A_post_tog$B_a_iris[,1] + A_post_tog$betas[,3,7]), # a_iris women, Colombia
  (A_post_tog$B_a_iris[,1] + A_post_tog$betas[,4,7]), # a_iris women, CZ 2016
  (A_post_tog$B_a_iris[,1] + A_post_tog$betas[,5,7]), # a_iris women, CZ 2019
  (A_post_tog$B_a_iris[,1] + A_post_tog$betas[,6,7]), # a_iris women, India
  (A_post_tog$B_a_iris[,1] + A_post_tog$betas[,7,7]), # a_iris women, Iran
  (A_post_tog$B_a_iris[,1] + A_post_tog$betas[,8,7]), # a_iris women, Turkey
  (A_post_tog$B_a_iris[,1] + A_post_tog$betas[,9,7]), # a_iris women, Vietnam
  
  # B_iris
  A_post_tog$B_b_iris[,1], # GLOBAL - WOMEN
  (A_post_tog$B_b_iris[,1] + A_post_tog$betas[,1,8]), # b_iris women, Cameroon 2013
  (A_post_tog$B_b_iris[,1] + A_post_tog$betas[,2,8]), # b_iris women, Cameroon 2016
  (A_post_tog$B_b_iris[,1] + A_post_tog$betas[,3,8]), # b_iris women, Colombia
  (A_post_tog$B_b_iris[,1] + A_post_tog$betas[,4,8]), # b_iris women, CZ 2016
  (A_post_tog$B_b_iris[,1] + A_post_tog$betas[,5,8]), # b_iris women, CZ 2019
  (A_post_tog$B_b_iris[,1] + A_post_tog$betas[,6,8]), # b_iris women, India
  (A_post_tog$B_b_iris[,1] + A_post_tog$betas[,7,8]), # b_iris women, Iran
  (A_post_tog$B_b_iris[,1] + A_post_tog$betas[,8,8]), # b_iris women, Turkey
  (A_post_tog$B_b_iris[,1] + A_post_tog$betas[,9,8]) # b_iris women, Vietnam
)


cols<-c("#00FBFF", 
        "#8C00FF",
        "#00FF33", 
        "#FFEA00",
        "#FFA600",
        
        "#00FBFF", 
        "#8C00FF",
        "#00FF33", 
        "#FFEA00",
        "#FFA600",
        
        "#00FBFF", 
        "#8C00FF",
        "#00FF33", 
        "#FFEA00",
        "#FFA600",
        
        "#00FBFF", 
        "#8C00FF",
        "#00FF33", 
        "#FFEA00",
        "#FFA600",
        
        "#00FBFF", 
        "#8C00FF",
        "#00FF33", 
        "#FFEA00",
        "#FFA600",
        
        "#00FBFF", 
        "#8C00FF",
        "#00FF33", 
        "#FFEA00",
        "#FFA600"
)

cols<-paste(cols,"80",sep="") 

ys<- c(seq(from=1, to=10, by=1),seq(from=11.5, to=20.5, by=1),seq(from=22, to=31, by=1))
ys<-rev(ys)

labels <- c(  "L* Global",
              "L* Cameroon 13",
              "L* Cameroon 16",
              "L* Colombia",
              "L* Czechia 16",
              "L* Czechia 19",
              "L* India - FCD",
              "L* Iran",
              "L* Turkey",
              "L* Vietnam",
              # A_iris
              "a* Global",
              "a* Cameroon 13",
              "a* Cameroon 16",
              "a* Colombia",
              "a* Czechia 16",
              "a* Czechia 19",
              "a* India - FCD",
              "a* Iran",
              "a* Turkey",
              "a* Vietnam",
              # B_iris
              "b* Global",
              "b* Cameroon 13",
              "b* Cameroon 16",
              "b* Colombia",
              "b* Czechia 16",
              "b* Czechia 19",
              "b* India - FCD",
              "b* Iran",
              "b* Turkey",
              "b* Vietnam"
)


Coefficients_check<-data.frame(Name=labels,Low_CI=rep(0,30),
                               Mean=rep(0,30),Up_CI=rep(0,30)) # Sorry, not in a mood to reinvent, how to .... do it aesthetically
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

cex_t2 <- 1.05
cex_t <- 1.15
pls <- -0.9
#text(0,-1,"Slope estimate", cex=1, pos=1, offset=1, xpd=NA)
text("Mean", x=1.75, y=max(ys-pls), cex=cex_t2, offset=0.5, xpd=NA, srt=0)
text("2.5 %", x=1.25, y=max(ys-pls), cex=cex_t2, offset=0.5, xpd=NA, srt=0)
text("97.5 %", x=2.25, y=max(ys-pls), cex=cex_t2, offset=0.5, xpd=NA, srt=0)


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
  axis(2,at=ycoord,labels=textlab,srt=0,las=1, cex.axis=1.2) 
  text(format(round(Coefficients_check$Mean[i],2),nsmall=2), x=1.75, y=ys[i], cex=cex_t)
  text(format(round(Coefficients_check$Low_CI[i],2),nsmall=2), x=1.25,y=ys[i], cex=cex_t)
  text(format(round(Coefficients_check$Up_CI[i],2),nsmall=2), x=2.25,y=ys[i], cex=cex_t)
  for (i in 1:30) {
    c(polygon(c(dens$x,rev(dens$x)),ys[i]*(c(dens$y,rev(dens$y)*(-1))*0.003)+ycoord[i],border=F, lwd = 1.5, col=colpol))
  }
  lines(x=PI(toplot_CI,prob=0.95),rep(ycoord+0.05,2),lwd=2, lty=1, col='#9090DDDF')
  segments(x0 = ci[1], y0 = ycoord-0.15, x1 = ci[1], y1 = ycoord+0.25, lwd = 3, col = '#9090DDDF')
  segments(x0 = ci[2], y0 = ycoord-0.15, x1 = ci[2], y1 = ycoord+0.25, lwd = 3, col = '#9090DDDF')
  points(x=mean(toplot_CI),ycoord,lwd=2,pch=21,col=1,bg=0,cex=1.2)
}


# 2 out 4: Sclera Colour - WOMEN


# SCLERA: 
par(mar=c(3.1, 0.5, 1.1, 0.5),mgp=c(5,0.7,2))

plot(NULL,xlim=c(-0.75,2.5),ylim=c(1,31.5),yaxt="n",bty="n", ylab="", xlab="", cex.axis=0.9, 
     main="Sclera Colour: WOMEN")

rect(-0.75, 0, 1, 32, col="#10101010", border=NA)

segments(-0.75, c(seq(from=1, to=10, by=1),seq(from=11.5, to=20.5, by=1),seq(from=22, to=31, by=1)), 1, 
         c(seq(from=1, to=10, by=1),seq(from=11.5, to=20.5, by=1),seq(from=22, to=31, by=1)), col="#40808080")

segments(-0.75, 10, 1, 10, col="#908080")
rect(-0.75, 9.5, 1, 10.5, col="#10101010", border=NA)

segments(-0.75, 20.5, 1, 20.5, col="#908080")
rect(-0.75, 20, 1, 21, col="#10101010", border=NA)

segments(-0.75, 31, 1, 31, col="#908080")
rect(-0.75, 30.5, 01, 31.5, col="#10101010", border=NA)


segments(c(-0.75,-0.25,0.25,0.75),0,c(-0.75,-0.25,0.25,0.75),32,col="#80808080",lty=2)
segments(0,0,0,32,col="#990909", lwd=2)

include <- list(
  # SCLERA L:
  A_post_tog$B_L_sclera[,1], # GLOBAL - WOMEN
  (A_post_tog$B_L_sclera[,1] + A_post_tog$betas[,1,9]), # L_sclera women, Cameroon 2013
  (A_post_tog$B_L_sclera[,1] + A_post_tog$betas[,2,9]), # L_sclera women, Cameroon 2016
  (A_post_tog$B_L_sclera[,1] + A_post_tog$betas[,3,9]), # L_sclera women, Colombia
  (A_post_tog$B_L_sclera[,1] + A_post_tog$betas[,4,9]), # L_sclera women, CZ 2016
  (A_post_tog$B_L_sclera[,1] + A_post_tog$betas[,5,9]), # L_sclera women, CZ 2019
  (A_post_tog$B_L_sclera[,1] + A_post_tog$betas[,6,9]), # L_sclera women, India
  (A_post_tog$B_L_sclera[,1] + A_post_tog$betas[,7,9]), # L_sclera women, Iran
  (A_post_tog$B_L_sclera[,1] + A_post_tog$betas[,8,9]), # L_sclera women, Turkey
  (A_post_tog$B_L_sclera[,1] + A_post_tog$betas[,9,9]), # L_sclera women, Vietnam
  
  # SCLERA A
  A_post_tog$B_a_sclera[,1], # GLOBAL - WOMEN
  (A_post_tog$B_a_sclera[,1] + A_post_tog$betas[,1,10]), # a_sclera women, Cameroon 2013
  (A_post_tog$B_a_sclera[,1] + A_post_tog$betas[,2,10]), # a_sclera women, Cameroon 2016
  (A_post_tog$B_a_sclera[,1] + A_post_tog$betas[,3,10]), # a_sclera women, Colombia
  (A_post_tog$B_a_sclera[,1] + A_post_tog$betas[,4,10]), # a_sclera women, CZ 2016
  (A_post_tog$B_a_sclera[,1] + A_post_tog$betas[,5,10]), # a_sclera women, CZ 2019
  (A_post_tog$B_a_sclera[,1] + A_post_tog$betas[,6,10]), # a_sclera women, India
  (A_post_tog$B_a_sclera[,1] + A_post_tog$betas[,7,10]), # a_sclera women, Iran
  (A_post_tog$B_a_sclera[,1] + A_post_tog$betas[,8,10]), # a_sclera women, Turkey
  (A_post_tog$B_a_sclera[,1] + A_post_tog$betas[,9,10]), # a_sclera women, Vietnam
  
  # SCLERA B
  A_post_tog$B_b_sclera[,1], # GLOBAL - WOMEN
  (A_post_tog$B_b_sclera[,1] + A_post_tog$betas[,1,11]), # b_sclera women, Cameroon 2013
  (A_post_tog$B_b_sclera[,1] + A_post_tog$betas[,2,11]), # b_sclera women, Cameroon 2016
  (A_post_tog$B_b_sclera[,1] + A_post_tog$betas[,3,11]), # b_sclera women, Colombia
  (A_post_tog$B_b_sclera[,1] + A_post_tog$betas[,4,11]), # b_sclera women, CZ 2016
  (A_post_tog$B_b_sclera[,1] + A_post_tog$betas[,5,11]), # b_sclera women, CZ 2019
  (A_post_tog$B_b_sclera[,1] + A_post_tog$betas[,6,11]), # b_sclera women, India
  (A_post_tog$B_b_sclera[,1] + A_post_tog$betas[,7,11]), # b_sclera women, Iran
  (A_post_tog$B_b_sclera[,1] + A_post_tog$betas[,8,11]), # b_sclera women, Turkey
  (A_post_tog$B_b_sclera[,1] + A_post_tog$betas[,9,11]) # b_sclera women, Vietnam
)


Coefficients_check<-data.frame(Name=labels,Low_CI=rep(0,30),
                               Mean=rep(0,30),Up_CI=rep(0,30)) # Sorry, not in a mood to reinvent, how to .... do it aesthetically
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


#text(0,-1,"Slope estimate", cex=1, pos=1, offset=1, xpd=NA)
text("Mean", x=1.75, y=max(ys-pls), cex=cex_t2, offset=0.5, xpd=NA, srt=0)
text("2.5 %", x=1.25, y=max(ys-pls), cex=cex_t2, offset=0.5, xpd=NA, srt=0)
text("97.5 %", x=2.25, y=max(ys-pls), cex=cex_t2, offset=0.5, xpd=NA, srt=0)


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
  text(format(round(Coefficients_check$Mean[i],2),nsmall=2), x=1.75, y=ys[i], cex=cex_t)
  text(format(round(Coefficients_check$Low_CI[i],2),nsmall=2), x=1.25,y=ys[i], cex=cex_t)
  text(format(round(Coefficients_check$Up_CI[i],2),nsmall=2), x=2.25,y=ys[i], cex=cex_t)
  for (i in 1:30) {
    c(polygon(c(dens$x,rev(dens$x)),ys[i]*(c(dens$y,rev(dens$y)*(-1))*0.003)+ycoord[i],border=F, lwd = 1.5, col=colpol))
  }
  lines(x=PI(toplot_CI,prob=0.95),rep(ycoord+0.05,2),lwd=2, lty=1, col='#9090DDDF')
  segments(x0 = ci[1], y0 = ycoord-0.15, x1 = ci[1], y1 = ycoord+0.25, lwd = 3, col = '#9090DDDF')
  segments(x0 = ci[2], y0 = ycoord-0.15, x1 = ci[2], y1 = ycoord+0.25, lwd = 3, col = '#9090DDDF')
  points(x=mean(toplot_CI),ycoord,lwd=2,pch=21,col=1,bg=0,cex=1.2)
}


# 3 out 4: Iris Colour - MEN


par(mar=c(3.1, 0.5, 1.1, 0.5),mgp=c(5,0.7,2))
plot(NULL,xlim=c(-0.75,2.5),ylim=c(1,31.5),yaxt="n",bty="n", ylab="", xlab="", cex.axis=0.9, 
     main="Iris Colour: MEN")

rect(-0.75, 0, 1, 32, col="#10101010", border=NA)

segments(-0.75, c(seq(from=1, to=10, by=1),seq(from=11.5, to=20.5, by=1),seq(from=22, to=31, by=1)), 1, 
         c(seq(from=1, to=10, by=1),seq(from=11.5, to=20.5, by=1),seq(from=22, to=31, by=1)), col="#40808080")

segments(-0.75, 10, 1, 10, col="#908080")
rect(-0.75, 9.5, 1, 10.5, col="#10101010", border=NA)

segments(-0.75, 20.5, 1, 20.5, col="#908080")
rect(-0.75, 20, 1, 21, col="#10101010", border=NA)

segments(-0.75, 31, 1, 31, col="#908080")
rect(-0.75, 30.5, 1, 31.5, col="#10101010", border=NA)


segments(c(-0.75,-0.25,0.25,0.75),0,c(-0.75,-0.25,0.25,0.75),32,col="#80808080",lty=2)
segments(0,0,0,32,col="#990909", lwd=2)

include <- list(
  # L_iris:
  A_post_tog$B_L_iris[,2], # GLOBAL - MEN
  (A_post_tog$B_L_iris[,2] + A_post_tog$betas[,1,17]), # L_iris men, Cameroon 2013
  (A_post_tog$B_L_iris[,2] + A_post_tog$betas[,2,17]), # L_iris men, Cameroon 2016
  (A_post_tog$B_L_iris[,2] + A_post_tog$betas[,3,17]), # L_iris men, Colombia
  (A_post_tog$B_L_iris[,2] + A_post_tog$betas[,4,17]), # L_iris men, CZ 2016
  (A_post_tog$B_L_iris[,2] + A_post_tog$betas[,5,17]), # L_iris men, CZ 2019
  (A_post_tog$B_L_iris[,2] + A_post_tog$betas[,6,17]), # L_iris men, India
  (A_post_tog$B_L_iris[,2] + A_post_tog$betas[,7,17]), # L_iris men, Iran
  (A_post_tog$B_L_iris[,2] + A_post_tog$betas[,8,17]), # L_iris men, Turkey
  (A_post_tog$B_L_iris[,2] + A_post_tog$betas[,9,17]), # L_iris men, Vietnam
  
  # A_iris
  A_post_tog$B_a_iris[,2], # GLOBAL - WOMEN
  (A_post_tog$B_a_iris[,2] + A_post_tog$betas[,1,18]), # a_iris men, Cameroon 2013
  (A_post_tog$B_a_iris[,2] + A_post_tog$betas[,2,18]), # a_iris men, Cameroon 2016
  (A_post_tog$B_a_iris[,2] + A_post_tog$betas[,3,18]), # a_iris men, Colombia
  (A_post_tog$B_a_iris[,2] + A_post_tog$betas[,4,18]), # a_iris men, CZ 2016
  (A_post_tog$B_a_iris[,2] + A_post_tog$betas[,5,18]), # a_iris men, CZ 2019
  (A_post_tog$B_a_iris[,2] + A_post_tog$betas[,6,18]), # a_iris men, India
  (A_post_tog$B_a_iris[,2] + A_post_tog$betas[,7,18]), # a_iris men, Iran
  (A_post_tog$B_a_iris[,2] + A_post_tog$betas[,8,18]), # a_iris men, Turkey
  (A_post_tog$B_a_iris[,2] + A_post_tog$betas[,9,18]), # a_iris men, Vietnam
  
  # B_iris
  A_post_tog$B_b_iris[,2], # GLOBAL - WOMEN
  (A_post_tog$B_b_iris[,2] + A_post_tog$betas[,1,19]), # b_iris men, Cameroon 2013
  (A_post_tog$B_b_iris[,2] + A_post_tog$betas[,2,19]), # b_iris men, Cameroon 2016
  (A_post_tog$B_b_iris[,2] + A_post_tog$betas[,3,19]), # b_iris men, Colombia
  (A_post_tog$B_b_iris[,2] + A_post_tog$betas[,4,19]), # b_iris men, CZ 2016
  (A_post_tog$B_b_iris[,2] + A_post_tog$betas[,5,19]), # b_iris men, CZ 2019
  (A_post_tog$B_b_iris[,2] + A_post_tog$betas[,6,19]), # b_iris men, India
  (A_post_tog$B_b_iris[,2] + A_post_tog$betas[,7,19]), # b_iris men, Iran
  (A_post_tog$B_b_iris[,2] + A_post_tog$betas[,8,19]), # b_iris men, Turkey
  (A_post_tog$B_b_iris[,2] + A_post_tog$betas[,9,19]) # b_iris men, Vietnam
)


Coefficients_check<-data.frame(Name=labels,Low_CI=rep(0,30),
                               Mean=rep(0,30),Up_CI=rep(0,30)) # Sorry, not in a mood to reinvent, how to .... do it aesthetically
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


#text(0,-1,"Slope estimate", cex=1, pos=1, offset=1, xpd=NA)
text("Mean", x=1.75, y=max(ys-pls), cex=cex_t2, offset=0.5, xpd=NA, srt=0)
text("2.5 %", x=1.25, y=max(ys-pls), cex=cex_t2, offset=0.5, xpd=NA, srt=0)
text("97.5 %", x=2.25, y=max(ys-pls), cex=cex_t2, offset=0.5, xpd=NA, srt=0)


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
  text(format(round(Coefficients_check$Mean[i],2),nsmall=2), x=1.75, y=ys[i], cex=cex_t)
  text(format(round(Coefficients_check$Low_CI[i],2),nsmall=2), x=1.25,y=ys[i], cex=cex_t)
  text(format(round(Coefficients_check$Up_CI[i],2),nsmall=2), x=2.25,y=ys[i], cex=cex_t)
  for (i in 1:30) {
    c(polygon(c(dens$x,rev(dens$x)),ys[i]*(c(dens$y,rev(dens$y)*(-1))*0.003)+ycoord[i],border=F, lwd = 1.5, col=colpol))
  }
  lines(x=PI(toplot_CI,prob=0.95),rep(ycoord+0.05,2),lwd=2, lty=1, col='#9090DDDF')
  segments(x0 = ci[1], y0 = ycoord-0.15, x1 = ci[1], y1 = ycoord+0.25, lwd = 3, col = '#9090DDDF')
  segments(x0 = ci[2], y0 = ycoord-0.15, x1 = ci[2], y1 = ycoord+0.25, lwd = 3, col = '#9090DDDF')
  points(x=mean(toplot_CI),ycoord,lwd=2,pch=21,col=1,bg=0,cex=1.2)
}


# 4 out 4: Sclera Colour - MEN


par(mar=c(3.1, 0.5, 1.1, 0.5),mgp=c(5,0.7,2))
plot(NULL,xlim=c(-0.75,2.5),ylim=c(1,31.5),yaxt="n",bty="n", ylab="", xlab="", cex.axis=0.9, 
     main="Sclera Colour: MEN")

rect(-0.75, 0, 1, 32, col="#10101010", border=NA)

segments(-0.75, c(seq(from=1, to=10, by=1),seq(from=11.5, to=20.5, by=1),seq(from=22, to=31, by=1)), 1, 
         c(seq(from=1, to=10, by=1),seq(from=11.5, to=20.5, by=1),seq(from=22, to=31, by=1)), col="#40808080")

segments(-0.75, 10, 1, 10, col="#908080")
rect(-0.75, 9.5, 1, 10.5, col="#10101010", border=NA)

segments(-0.75, 20.5, 1, 20.5, col="#908080")
rect(-0.75, 20, 1, 21, col="#10101010", border=NA)

segments(-0.75, 31, 1, 31, col="#908080")
rect(-0.75, 30.5, 1, 31.5, col="#10101010", border=NA)


segments(c(-0.75,-0.25,0.25,0.75),0,c(-0.75,-0.25,0.25,0.75),32,col="#80808080",lty=2)
segments(0,0,0,32,col="#990909", lwd=2)


include <- list(
  # L_Sclera:
  A_post_tog$B_L_sclera[,2], # GLOBAL - MEN
  (A_post_tog$B_L_sclera[,2] + A_post_tog$betas[,1,20]), # L_sclera men, Cameroon 2013
  (A_post_tog$B_L_sclera[,2] + A_post_tog$betas[,2,20]), # L_sclera men, Cameroon 2016
  (A_post_tog$B_L_sclera[,2] + A_post_tog$betas[,3,20]), # L_sclera men, Colombia
  (A_post_tog$B_L_sclera[,2] + A_post_tog$betas[,4,20]), # L_Sclera men, CZ 2016
  (A_post_tog$B_L_sclera[,2] + A_post_tog$betas[,5,20]), # L_sclera men, CZ 2019
  (A_post_tog$B_L_sclera[,2] + A_post_tog$betas[,6,20]), # L_sclera men, India
  (A_post_tog$B_L_sclera[,2] + A_post_tog$betas[,7,20]), # L_sclera men, Iran
  (A_post_tog$B_L_sclera[,2] + A_post_tog$betas[,8,20]), # L_sclera men, Turkey
  (A_post_tog$B_L_sclera[,2] + A_post_tog$betas[,9,20]), # L_sclera men, Vietnam
  
  # A_Sclera
  A_post_tog$B_a_sclera[,2], # GLOBAL - MEN
  (A_post_tog$B_a_sclera[,2] + A_post_tog$betas[,1,21]), # a_sclera men, Cameroon 2013
  (A_post_tog$B_a_sclera[,2] + A_post_tog$betas[,2,21]), # a_sclera men, Cameroon 2016
  (A_post_tog$B_a_sclera[,2] + A_post_tog$betas[,3,21]), # a_sclera men, Colombia
  (A_post_tog$B_a_sclera[,2] + A_post_tog$betas[,4,21]), # a_sclera men, CZ 2016
  (A_post_tog$B_a_sclera[,2] + A_post_tog$betas[,5,21]), # a_sclera men, CZ 2019
  (A_post_tog$B_a_sclera[,2] + A_post_tog$betas[,6,21]), # a_sclera men, India
  (A_post_tog$B_a_sclera[,2] + A_post_tog$betas[,7,21]), # a_sclera men, Iran
  (A_post_tog$B_a_sclera[,2] + A_post_tog$betas[,8,21]), # a_sclera men, Turkey
  (A_post_tog$B_a_sclera[,2] + A_post_tog$betas[,9,21]), # a_sclera men, Vietnam
  
  # B_Sclera
  A_post_tog$B_b_sclera[,2], # GLOBAL - MEN
  (A_post_tog$B_b_sclera[,2] + A_post_tog$betas[,1,22]), # b_sclera men, Cameroon 2013
  (A_post_tog$B_b_sclera[,2] + A_post_tog$betas[,2,22]), # b_sclera men, Cameroon 2016
  (A_post_tog$B_b_sclera[,2] + A_post_tog$betas[,3,22]), # b_sclera men, Colombia
  (A_post_tog$B_b_sclera[,2] + A_post_tog$betas[,4,22]), # b_sclera men, CZ 2016
  (A_post_tog$B_b_sclera[,2] + A_post_tog$betas[,5,22]), # b_sclera men, CZ 2019
  (A_post_tog$B_b_sclera[,2] + A_post_tog$betas[,6,22]), # b_sclera men, India
  (A_post_tog$B_b_sclera[,2] + A_post_tog$betas[,7,22]), # b_sclera men, Iran
  (A_post_tog$B_b_sclera[,2] + A_post_tog$betas[,8,22]), # b_sclera men, Turkey
  (A_post_tog$B_b_sclera[,2] + A_post_tog$betas[,9,22]) # b_sclera men, Vietnam
)


Coefficients_check<-data.frame(Name=labels,Low_CI=rep(0,30),
                               Mean=rep(0,30),Up_CI=rep(0,30)) 
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


#text(0,-1,"Slope estimate", cex=1, pos=1, offset=1, xpd=NA)
text("Mean", x=1.75, y=max(ys-pls), cex=cex_t2, offset=0.5, xpd=NA, srt=0)
text("2.5 %", x=1.25, y=max(ys-pls), cex=cex_t2, offset=0.5, xpd=NA, srt=0)
text("97.5 %", x=2.25, y=max(ys-pls), cex=cex_t2, offset=0.5, xpd=NA, srt=0)


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
  text(format(round(Coefficients_check$Mean[i],2),nsmall=2), x=1.75, y=ys[i], cex=cex_t)
  text(format(round(Coefficients_check$Low_CI[i],2),nsmall=2), x=1.25,y=ys[i], cex=cex_t)
  text(format(round(Coefficients_check$Up_CI[i],2),nsmall=2), x=2.25,y=ys[i], cex=cex_t)
  for (i in 1:30) {
    c(polygon(c(dens$x,rev(dens$x)),ys[i]*(c(dens$y,rev(dens$y)*(-1))*0.003)+ycoord[i],border=F, lwd = 1.5, col=colpol))
  }
  lines(x=PI(toplot_CI,prob=0.95),rep(ycoord+0.05,2),lwd=2, lty=1, col='#9090DDDF')
  segments(x0 = ci[1], y0 = ycoord-0.15, x1 = ci[1], y1 = ycoord+0.25, lwd = 3, col = '#9090DDDF')
  segments(x0 = ci[2], y0 = ycoord-0.15, x1 = ci[2], y1 = ycoord+0.25, lwd = 3, col = '#9090DDDF')
  points(x=mean(toplot_CI),ycoord,lwd=2,pch=21,col=1,bg=0,cex=1.2)
}

dev.off()

#POSEM
#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.
# 
# Without skin L* - Sextypicality
#
#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.

tiff("Posterior__SEXT_Preds_TOGETHER_STANDARDISED_Without_Skin_L.tif",width=32,height=26,units="cm",res=600,compression = "lzw")
layout(matrix(1:4,byrow=T,nrow=1), widths=c(2.9,2,2,2),heights=1)

par(mar=c(5.1, 10.1, 1.1, 1.1),mgp=c(5,0.7,2))
plot(NULL,xlim=c(-0.75,2.5),ylim=c(1,28.5),yaxt="n",bty="n", ylab="", cex.axis=0.7, 
     main="Iris Colour: WOMEN")

rect(-0.75, 0, 1, 29.5, col="#10101010", border=NA)

segments(-0.75, c(seq(from=1, to=9, by=1),seq(from=10.5, to=18.5, by=1),seq(from=20, to=28, by=1)), 1, 
         c(seq(from=1, to=9, by=1),seq(from=10.5, to=18.5, by=1),seq(from=20, to=28, by=1)), col="#40808080")

segments(-0.75, 9, 1, 9, col="#908080", lwd=2)
rect(-0.75, 8.5, 1, 9.5, col="#10101010", border=NA)

segments(-0.75, 18.5, 1, 18.5, col="#908080", lwd=2)
rect(-0.75, 18, 1, 19, col="#10101010", border=NA)

segments(-0.75, 28, 1, 28, col="#908080", lwd=2)
rect(-0.75, 27.5, 1, 28.5, col="#10101010", border=NA)


segments(c(-0.75,-0.25,0.25,0.75),0,c(-0.75,-0.25,0.25,0.75),29,col="#80808080",lty=2)
segments(0,0,0,29,col="#990909", lwd=2)


include <- list(
  # L_iris:
  S_post_tog$B_L_iris[,1], # GLOBAL - WOMEN
  (S_post_tog$B_L_iris[,1] + S_post_tog$betas[,1,6]), # L_iris women, Cameroon 2013
  (S_post_tog$B_L_iris[,1] + S_post_tog$betas[,2,6]), # L_iris women, Cameroon 2016
  (S_post_tog$B_L_iris[,1] + S_post_tog$betas[,3,6]), # L_iris women, Colombia
  (S_post_tog$B_L_iris[,1] + S_post_tog$betas[,4,6]), # L_iris women, CZ 2016
  (S_post_tog$B_L_iris[,1] + S_post_tog$betas[,5,6]), # L_iris women, CZ 2019
  (S_post_tog$B_L_iris[,1] + S_post_tog$betas[,6,6]), # L_iris women, India
  (S_post_tog$B_L_iris[,1] + S_post_tog$betas[,7,6]), # L_iris women, Iran
  (S_post_tog$B_L_iris[,1] + S_post_tog$betas[,8,6]), # L_iris women, Turkey
  
  # A_iris
  S_post_tog$B_a_iris[,1], # GLOBAL - WOMEN
  (S_post_tog$B_a_iris[,1] + S_post_tog$betas[,1,7]), # a_iris women, Cameroon 2013
  (S_post_tog$B_a_iris[,1] + S_post_tog$betas[,2,7]), # a_iris women, Cameroon 2016
  (S_post_tog$B_a_iris[,1] + S_post_tog$betas[,3,7]), # a_iris women, Colombia
  (S_post_tog$B_a_iris[,1] + S_post_tog$betas[,4,7]), # a_iris women, CZ 2016
  (S_post_tog$B_a_iris[,1] + S_post_tog$betas[,5,7]), # a_iris women, CZ 2019
  (S_post_tog$B_a_iris[,1] + S_post_tog$betas[,6,7]), # a_iris women, India
  (S_post_tog$B_a_iris[,1] + S_post_tog$betas[,7,7]), # a_iris women, Iran
  (S_post_tog$B_a_iris[,1] + S_post_tog$betas[,8,7]), # a_iris women, Turkey
  
  # B_iris
  S_post_tog$B_b_iris[,1], # GLOBAL - WOMEN
  (S_post_tog$B_b_iris[,1] + S_post_tog$betas[,1,8]), # b_iris women, Cameroon 2013
  (S_post_tog$B_b_iris[,1] + S_post_tog$betas[,2,8]), # b_iris women, Cameroon 2016
  (S_post_tog$B_b_iris[,1] + S_post_tog$betas[,3,8]), # b_iris women, Colombia
  (S_post_tog$B_b_iris[,1] + S_post_tog$betas[,4,8]), # b_iris women, CZ 2016
  (S_post_tog$B_b_iris[,1] + S_post_tog$betas[,5,8]), # b_iris women, CZ 2019
  (S_post_tog$B_b_iris[,1] + S_post_tog$betas[,6,8]), # b_iris women, India
  (S_post_tog$B_b_iris[,1] + S_post_tog$betas[,7,8]), # b_iris women, Iran
  (S_post_tog$B_b_iris[,1] + S_post_tog$betas[,8,8]) # b_iris women, Turkey
)



cols<-c("#00FBFF", 
        "#8C00FF",
        "#00FF33", 
        "#FFEA00",
        "#FFA600",
        
        "#00FBFF", 
        "#8C00FF",
        "#00FF33", 
        "#FFEA00",
        
        "#00FBFF", 
        "#8C00FF",
        "#00FF33", 
        "#FFEA00",
        "#FFA600",
        
        "#00FBFF", 
        "#8C00FF",
        "#00FF33", 
        "#FFEA00",
        
        "#00FBFF", 
        "#8C00FF",
        "#00FF33", 
        "#FFEA00",
        "#FFA600",
        
        "#00FBFF", 
        "#8C00FF",
        "#00FF33", 
        "#FFEA00"
)

cols<-paste(cols,"80",sep="") 

ys<- c(seq(from=1, to=9, by=1),seq(from=10.5, to=18.5, by=1),seq(from=20, to=28, by=1))
ys<-rev(ys)

labels <- c(  "L* Global",
              "L* Cameroon 13",
              "L* Cameroon 16",
              "L* Colombia",
              "L* Czechia 16",
              "L* Czechia 19",
              "L* India - FCD",
              "L* Iran",
              "L* Turkey",
              # A_iris
              "a* Global",
              "a* Cameroon 13",
              "a* Cameroon 16",
              "a* Colombia",
              "a* Czechia 16",
              "a* Czechia 19",
              "a* India - FCD",
              "a* Iran",
              "a* Turkey",
              # B_iris
              "b* Global",
              "b* Cameroon 13",
              "b* Cameroon 16",
              "b* Colombia",
              "b* Czechia 16",
              "b* Czechia 19",
              "b* India - FCD",
              "b* Iran",
              "b* Turkey"
)


Coefficients_check<-data.frame(Name=labels,Low_CI=rep(0,27),
                               Mean=rep(0,27),Up_CI=rep(0,27)) # Sorry, not in a mood to reinvent, how to .... do it aesthetically
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

cex_t <- 1.15
cex_t2 <- 1.05
pls <- -0.9
#text(0,-1,"Slope estimate", cex=1, pos=1, offset=1, xpd=NA)
text("Mean", x=1.75, y=max(ys-pls), cex=cex_t2, offset=0.5, xpd=NA, srt=0)
text("2.5 %", x=1.25, y=max(ys-pls), cex=cex_t2, offset=0.5, xpd=NA, srt=0)
text("97.5 %", x=2.25, y=max(ys-pls), cex=cex_t2, offset=0.5, xpd=NA, srt=0)


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
  axis(2,at=ycoord,labels=textlab,srt=0,las=1, cex.axis=1.2) 
  text(format(round(Coefficients_check$Mean[i],2),nsmall=2), x=1.75, y=ys[i], cex=cex_t)
  text(format(round(Coefficients_check$Low_CI[i],2),nsmall=2), x=1.25,y=ys[i], cex=cex_t)
  text(format(round(Coefficients_check$Up_CI[i],2),nsmall=2), x=2.25,y=ys[i], cex=cex_t)
  for (i in 1:27) {
    c(polygon(c(dens$x,rev(dens$x)),ys[i]*(c(dens$y,rev(dens$y)*(-1))*0.003)+ycoord[i],border=F, lwd = 1.5, col=colpol))
  }
  lines(x=PI(toplot_CI,prob=0.95),rep(ycoord+0.05,2),lwd=2, lty=1, col='#9090DDDF')
  segments(x0 = ci[1], y0 = ycoord-0.15, x1 = ci[1], y1 = ycoord+0.25, lwd = 3, col = '#9090DDDF')
  segments(x0 = ci[2], y0 = ycoord-0.15, x1 = ci[2], y1 = ycoord+0.25, lwd = 3, col = '#9090DDDF')
  points(x=mean(toplot_CI),ycoord,lwd=2,pch=21,col=1,bg=0,cex=1.2)
}


# SCLERA: 
par(mar=c(5.1, 1.1, 1.1, 1.1),mgp=c(5,0.7,2))
plot(NULL,xlim=c(-0.7,2.5),ylim=c(1,28.5),yaxt="n",bty="n", ylab="", cex.axis=0.7, 
     main="Sclera Colour: WOMEN")

rect(-0.75, 0, 1, 29.5, col="#10101010", border=NA)

segments(-0.75, c(seq(from=1, to=9, by=1),seq(from=10.5, to=18.5, by=1),seq(from=20, to=28, by=1)), 1, 
         c(seq(from=1, to=9, by=1),seq(from=10.5, to=18.5, by=1),seq(from=20, to=28, by=1)), col="#40808080")

segments(-0.75, 9, 1, 9, col="#908080", lwd=2)
rect(-0.75, 8.5, 1, 9.5, col="#10101010", border=NA)

segments(-0.75, 18.5, 1, 18.5, col="#908080", lwd=2)
rect(-0.75, 18, 1, 19, col="#10101010", border=NA)

segments(-0.75, 28, 1, 28, col="#908080", lwd=2)
rect(-0.75, 27.5, 1, 28.5, col="#10101010", border=NA)



segments(c(-0.75,-0.25,0.25,0.75),0,c(-0.75,-0.25,0.25,0.75),29,col="#80808080",lty=2)
segments(0,0,0,29,col="#990909", lwd=2)

include <- list(
  # SCLERA L:
  S_post_tog$B_L_sclera[,1], # GLOBAL - WOMEN
  (S_post_tog$B_L_sclera[,1] + S_post_tog$betas[,1,9]), # L_sclera women, Cameroon 2013
  (S_post_tog$B_L_sclera[,1] + S_post_tog$betas[,2,9]), # L_sclera women, Cameroon 2016
  (S_post_tog$B_L_sclera[,1] + S_post_tog$betas[,3,9]), # L_sclera women, Colombia
  (S_post_tog$B_L_sclera[,1] + S_post_tog$betas[,4,9]), # L_sclera women, CZ 2016
  (S_post_tog$B_L_sclera[,1] + S_post_tog$betas[,5,9]), # L_sclera women, CZ 2019
  (S_post_tog$B_L_sclera[,1] + S_post_tog$betas[,6,9]), # L_sclera women, India
  (S_post_tog$B_L_sclera[,1] + S_post_tog$betas[,7,9]), # L_sclera women, Iran
  (S_post_tog$B_L_sclera[,1] + S_post_tog$betas[,8,9]), # L_sclera women, Turkey
  
  # SCLERA A
  S_post_tog$B_a_sclera[,1], # GLOBAL - WOMEN
  (S_post_tog$B_a_sclera[,1] + S_post_tog$betas[,1,10]), # a_sclera women, Cameroon 2013
  (S_post_tog$B_a_sclera[,1] + S_post_tog$betas[,2,10]), # a_sclera women, Cameroon 2016
  (S_post_tog$B_a_sclera[,1] + S_post_tog$betas[,3,10]), # a_sclera women, Colombia
  (S_post_tog$B_a_sclera[,1] + S_post_tog$betas[,4,10]), # a_sclera women, CZ 2016
  (S_post_tog$B_a_sclera[,1] + S_post_tog$betas[,5,10]), # a_sclera women, CZ 2019
  (S_post_tog$B_a_sclera[,1] + S_post_tog$betas[,6,10]), # a_sclera women, India
  (S_post_tog$B_a_sclera[,1] + S_post_tog$betas[,7,10]), # a_sclera women, Iran
  (S_post_tog$B_a_sclera[,1] + S_post_tog$betas[,8,10]), # a_sclera women, Turkey
  
  # SCLERA B
  S_post_tog$B_b_sclera[,1], # GLOBAL - WOMEN
  (S_post_tog$B_b_sclera[,1] + S_post_tog$betas[,1,11]), # b_sclera women, Cameroon 2013
  (S_post_tog$B_b_sclera[,1] + S_post_tog$betas[,2,11]), # b_sclera women, Cameroon 2016
  (S_post_tog$B_b_sclera[,1] + S_post_tog$betas[,3,11]), # b_sclera women, Colombia
  (S_post_tog$B_b_sclera[,1] + S_post_tog$betas[,4,11]), # b_sclera women, CZ 2016
  (S_post_tog$B_b_sclera[,1] + S_post_tog$betas[,5,11]), # b_sclera women, CZ 2019
  (S_post_tog$B_b_sclera[,1] + S_post_tog$betas[,6,11]), # b_sclera women, India
  (S_post_tog$B_b_sclera[,1] + S_post_tog$betas[,7,11]), # b_sclera women, Iran
  (S_post_tog$B_b_sclera[,1] + S_post_tog$betas[,8,11]) # b_sclera women, Turkey
)



Coefficients_check<-data.frame(Name=labels,Low_CI=rep(0,27),
                               Mean=rep(0,27),Up_CI=rep(0,27)) # Sorry, not in a mood to reinvent, how to .... do it aesthetically
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


#text(0,-1,"Slope estimate", cex=1, pos=1, offset=1, xpd=NA)
text("Mean", x=1.75, y=max(ys-pls), cex=cex_t2, offset=0.5, xpd=NA, srt=0)
text("2.5 %", x=1.25, y=max(ys-pls), cex=cex_t2, offset=0.5, xpd=NA, srt=0)
text("97.5 %", x=2.25, y=max(ys-pls), cex=cex_t2, offset=0.5, xpd=NA, srt=0)


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
  text(format(round(Coefficients_check$Mean[i],2),nsmall=2), x=1.75, y=ys[i], cex=cex_t)
  text(format(round(Coefficients_check$Low_CI[i],2),nsmall=2), x=1.25,y=ys[i], cex=cex_t)
  text(format(round(Coefficients_check$Up_CI[i],2),nsmall=2), x=2.25,y=ys[i], cex=cex_t)
  for (i in 1:27) {
    c(polygon(c(dens$x,rev(dens$x)),ys[i]*(c(dens$y,rev(dens$y)*(-1))*0.003)+ycoord[i],border=F, lwd = 1.5, col=colpol))
  }
  lines(x=PI(toplot_CI,prob=0.95),rep(ycoord+0.05,2),lwd=2, lty=1, col='#9090DDDF')
  segments(x0 = ci[1], y0 = ycoord-0.15, x1 = ci[1], y1 = ycoord+0.25, lwd = 3, col = '#9090DDDF')
  segments(x0 = ci[2], y0 = ycoord-0.15, x1 = ci[2], y1 = ycoord+0.25, lwd = 3, col = '#9090DDDF')
  points(x=mean(toplot_CI),ycoord,lwd=2,pch=21,col=1,bg=0,cex=1.2)
}


# MEN: 

par(mar=c(5.1, 1.1, 1.1, 1.1),mgp=c(5,0.7,2))
plot(NULL,xlim=c(-0.7,2.5),ylim=c(1,28.5),yaxt="n",bty="n", ylab="", cex.axis=0.7, 
     main="Iris Colour: MEN")

rect(-0.75, 0, 1, 29.5, col="#10101010", border=NA)

segments(-0.75, c(seq(from=1, to=9, by=1),seq(from=10.5, to=18.5, by=1),seq(from=20, to=28, by=1)), 1, 
         c(seq(from=1, to=9, by=1),seq(from=10.5, to=18.5, by=1),seq(from=20, to=28, by=1)), col="#40808080")

segments(-0.75, 9, 1, 9, col="#908080", lwd=2)
rect(-0.75, 8.5, 1, 9.5, col="#10101010", border=NA)

segments(-0.75, 18.5, 1, 18.5, col="#908080", lwd=2)
rect(-0.75, 18, 1, 19, col="#10101010", border=NA)

segments(-0.75, 28, 1, 28, col="#908080", lwd=2)
rect(-0.75, 27.5, 1, 28.5, col="#10101010", border=NA)


segments(c(-0.75,-0.25,0.25,0.75),0,c(-0.75,-0.25,0.25,0.75),29,col="#80808080",lty=2)
segments(0,0,0,29,col="#990909", lwd=2)

include <- list(
  # L_iris:
  S_post_tog$B_L_iris[,2], # GLOBAL - MEN
  (S_post_tog$B_L_iris[,2] + S_post_tog$betas[,1,17]), # L_iris men, Cameroon 2013
  (S_post_tog$B_L_iris[,2] + S_post_tog$betas[,2,17]), # L_iris men, Cameroon 2016
  (S_post_tog$B_L_iris[,2] + S_post_tog$betas[,3,17]), # L_iris men, Colombia
  (S_post_tog$B_L_iris[,2] + S_post_tog$betas[,4,17]), # L_iris men, CZ 2016
  (S_post_tog$B_L_iris[,2] + S_post_tog$betas[,5,17]), # L_iris men, CZ 2019
  (S_post_tog$B_L_iris[,2] + S_post_tog$betas[,6,17]), # L_iris men, India
  (S_post_tog$B_L_iris[,2] + S_post_tog$betas[,7,17]), # L_iris men, Iran
  (S_post_tog$B_L_iris[,2] + S_post_tog$betas[,8,17]), # L_iris men, Turkey
  
  # A_iris
  S_post_tog$B_a_iris[,2], # GLOBAL - WOMEN
  (S_post_tog$B_a_iris[,2] + S_post_tog$betas[,1,18]), # a_iris men, Cameroon 2013
  (S_post_tog$B_a_iris[,2] + S_post_tog$betas[,2,18]), # a_iris men, Cameroon 2016
  (S_post_tog$B_a_iris[,2] + S_post_tog$betas[,3,18]), # a_iris men, Colombia
  (S_post_tog$B_a_iris[,2] + S_post_tog$betas[,4,18]), # a_iris men, CZ 2016
  (S_post_tog$B_a_iris[,2] + S_post_tog$betas[,5,18]), # a_iris men, CZ 2019
  (S_post_tog$B_a_iris[,2] + S_post_tog$betas[,6,18]), # a_iris men, India
  (S_post_tog$B_a_iris[,2] + S_post_tog$betas[,7,18]), # a_iris men, Iran
  (S_post_tog$B_a_iris[,2] + S_post_tog$betas[,8,18]), # a_iris men, Turkey
  
  # B_iris
  S_post_tog$B_b_iris[,2], # GLOBAL - WOMEN
  (S_post_tog$B_b_iris[,2] + S_post_tog$betas[,1,19]), # b_iris men, Cameroon 2013
  (S_post_tog$B_b_iris[,2] + S_post_tog$betas[,2,19]), # b_iris men, Cameroon 2016
  (S_post_tog$B_b_iris[,2] + S_post_tog$betas[,3,19]), # b_iris men, Colombia
  (S_post_tog$B_b_iris[,2] + S_post_tog$betas[,4,19]), # b_iris men, CZ 2016
  (S_post_tog$B_b_iris[,2] + S_post_tog$betas[,5,19]), # b_iris men, CZ 2019
  (S_post_tog$B_b_iris[,2] + S_post_tog$betas[,6,19]), # b_iris men, India
  (S_post_tog$B_b_iris[,2] + S_post_tog$betas[,7,19]), # b_iris men, Iran
  (S_post_tog$B_b_iris[,2] + S_post_tog$betas[,8,19]) # b_iris men, Turkey
)



Coefficients_check<-data.frame(Name=labels,Low_CI=rep(0,27),
                               Mean=rep(0,27),Up_CI=rep(0,27)) # Sorry, not in a mood to reinvent, how to .... do it aesthetically
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


#text(0,-1,"Slope estimate", cex=1, pos=1, offset=1, xpd=NA)
text("Mean", x=1.75, y=max(ys-pls), cex=cex_t2, offset=0.5, xpd=NA, srt=0)
text("2.5 %", x=1.25, y=max(ys-pls), cex=cex_t2, offset=0.5, xpd=NA, srt=0)
text("97.5 %", x=2.25, y=max(ys-pls), cex=cex_t2, offset=0.5, xpd=NA, srt=0)


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
  text(format(round(Coefficients_check$Mean[i],2),nsmall=2), x=1.75, y=ys[i], cex=cex_t)
  text(format(round(Coefficients_check$Low_CI[i],2),nsmall=2), x=1.25,y=ys[i], cex=cex_t)
  text(format(round(Coefficients_check$Up_CI[i],2),nsmall=2), x=2.25,y=ys[i], cex=cex_t)
  for (i in 1:27) {
    c(polygon(c(dens$x,rev(dens$x)),ys[i]*(c(dens$y,rev(dens$y)*(-1))*0.003)+ycoord[i],border=F, lwd = 1.5, col=colpol))
  }
  lines(x=PI(toplot_CI,prob=0.95),rep(ycoord+0.05,2),lwd=2, lty=1, col='#9090DDDF')
  segments(x0 = ci[1], y0 = ycoord-0.15, x1 = ci[1], y1 = ycoord+0.25, lwd = 3, col = '#9090DDDF')
  segments(x0 = ci[2], y0 = ycoord-0.15, x1 = ci[2], y1 = ycoord+0.25, lwd = 3, col = '#9090DDDF')
  points(x=mean(toplot_CI),ycoord,lwd=2,pch=21,col=1,bg=0,cex=1.2)
}


# SCLERA_

par(mar=c(5.1, 1.1, 1.1, 1.1),mgp=c(5,0.7,2))
plot(NULL,xlim=c(-0.7,2.5),ylim=c(1,28.5),yaxt="n",bty="n", ylab="", cex.axis=0.7, 
     main="Sclera Colour: MEN")

rect(-0.75, 0, 1, 29.5, col="#10101010", border=NA)

segments(-0.75, c(seq(from=1, to=9, by=1),seq(from=10.5, to=18.5, by=1),seq(from=20, to=28, by=1)), 1, 
         c(seq(from=1, to=9, by=1),seq(from=10.5, to=18.5, by=1),seq(from=20, to=28, by=1)), col="#40808080")

segments(-0.75, 9, 1, 9, col="#908080", lwd=2)
rect(-0.75, 8.5, 1, 9.5, col="#10101010", border=NA)

segments(-0.75, 18.5, 1, 18.5, col="#908080", lwd=2)
rect(-0.75, 18, 1, 19, col="#10101010", border=NA)

segments(-0.75, 28, 1, 28, col="#908080", lwd=2)
rect(-0.75, 27.5, 1, 28.5, col="#10101010", border=NA)


segments(c(-0.75,-0.25,0.25,0.75),0,c(-0.75,-0.25,0.25,0.75),29,col="#80808080",lty=2)
segments(0,0,0,29,col="#990909", lwd=2)

include <- list(
  # L_Sclera:
  S_post_tog$B_L_sclera[,2], # GLOBAL - MEN
  (S_post_tog$B_L_sclera[,2] + S_post_tog$betas[,1,20]), # L_sclera men, Cameroon 2013
  (S_post_tog$B_L_sclera[,2] + S_post_tog$betas[,2,20]), # L_sclera men, Cameroon 2016
  (S_post_tog$B_L_sclera[,2] + S_post_tog$betas[,3,20]), # L_sclera men, Colombia
  (S_post_tog$B_L_sclera[,2] + S_post_tog$betas[,4,20]), # L_Sclera men, CZ 2016
  (S_post_tog$B_L_sclera[,2] + S_post_tog$betas[,5,20]), # L_sclera men, CZ 2019
  (S_post_tog$B_L_sclera[,2] + S_post_tog$betas[,6,20]), # L_sclera men, India
  (S_post_tog$B_L_sclera[,2] + S_post_tog$betas[,7,20]), # L_sclera men, Iran
  (S_post_tog$B_L_sclera[,2] + S_post_tog$betas[,8,20]), # L_sclera men, Turkey
  
  # A_Sclera
  S_post_tog$B_a_sclera[,2], # GLOBAL - MEN
  (S_post_tog$B_a_sclera[,2] + S_post_tog$betas[,1,21]), # a_sclera men, Cameroon 2013
  (S_post_tog$B_a_sclera[,2] + S_post_tog$betas[,2,21]), # a_sclera men, Cameroon 2016
  (S_post_tog$B_a_sclera[,2] + S_post_tog$betas[,3,21]), # a_sclera men, Colombia
  (S_post_tog$B_a_sclera[,2] + S_post_tog$betas[,4,21]), # a_sclera men, CZ 2016
  (S_post_tog$B_a_sclera[,2] + S_post_tog$betas[,5,21]), # a_sclera men, CZ 2019
  (S_post_tog$B_a_sclera[,2] + S_post_tog$betas[,6,21]), # a_sclera men, India
  (S_post_tog$B_a_sclera[,2] + S_post_tog$betas[,7,21]), # a_sclera men, Iran
  (S_post_tog$B_a_sclera[,2] + S_post_tog$betas[,8,21]), # a_sclera men, Turkey
  
  # B_Sclera
  S_post_tog$B_b_sclera[,2], # GLOBAL - MEN
  (S_post_tog$B_b_sclera[,2] + S_post_tog$betas[,1,22]), # b_sclera men, Cameroon 2013
  (S_post_tog$B_b_sclera[,2] + S_post_tog$betas[,2,22]), # b_sclera men, Cameroon 2016
  (S_post_tog$B_b_sclera[,2] + S_post_tog$betas[,3,22]), # b_sclera men, Colombia
  (S_post_tog$B_b_sclera[,2] + S_post_tog$betas[,4,22]), # b_sclera men, CZ 2016
  (S_post_tog$B_b_sclera[,2] + S_post_tog$betas[,5,22]), # b_sclera men, CZ 2019
  (S_post_tog$B_b_sclera[,2] + S_post_tog$betas[,6,22]), # b_sclera men, India
  (S_post_tog$B_b_sclera[,2] + S_post_tog$betas[,7,22]), # b_sclera men, Iran
  (S_post_tog$B_b_sclera[,2] + S_post_tog$betas[,8,22]) # b_sclera men, Turkey
)


Coefficients_check<-data.frame(Name=labels,Low_CI=rep(0,27),
                               Mean=rep(0,27),Up_CI=rep(0,27)) 
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


#text(0,-1,"Slope estimate", cex=1, pos=1, offset=1, xpd=NA)
text("Mean", x=1.75, y=max(ys-pls), cex=cex_t2, offset=0.5, xpd=NA, srt=0)
text("2.5 %", x=1.25, y=max(ys-pls), cex=cex_t2, offset=0.5, xpd=NA, srt=0)
text("97.5 %", x=2.25, y=max(ys-pls), cex=cex_t2, offset=0.5, xpd=NA, srt=0)


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
  text(format(round(Coefficients_check$Mean[i],2),nsmall=2), x=1.75, y=ys[i], cex=cex_t)
  text(format(round(Coefficients_check$Low_CI[i],2),nsmall=2), x=1.25,y=ys[i], cex=cex_t)
  text(format(round(Coefficients_check$Up_CI[i],2),nsmall=2), x=2.25,y=ys[i], cex=cex_t)
  for (i in 1:27) {
    c(polygon(c(dens$x,rev(dens$x)),ys[i]*(c(dens$y,rev(dens$y)*(-1))*0.003)+ycoord[i],border=F, lwd = 1.5, col=colpol))
  }
  lines(x=PI(toplot_CI,prob=0.95),rep(ycoord+0.05,2),lwd=2, lty=1, col='#9090DDDF')
  segments(x0 = ci[1], y0 = ycoord-0.15, x1 = ci[1], y1 = ycoord+0.25, lwd = 3, col = '#9090DDDF')
  segments(x0 = ci[2], y0 = ycoord-0.15, x1 = ci[2], y1 = ycoord+0.25, lwd = 3, col = '#9090DDDF')
  points(x=mean(toplot_CI),ycoord,lwd=2,pch=21,col=1,bg=0,cex=1.2)
}

dev.off()
