# Visualisation when groups pooled:

# Set the WD properly, if the objects not present in the same folder! 

# 1 Fourteen/Twelve group solution

load("Model_14groups_ATTRACT_both_sexes_standardised_together.Rdata")
load("Model_12groups_SEXTYP_both_sexes_standardised_together.Rdata")

str(m12_Sext)

A_post_tog <- extract.samples(m14_attr)
S_post_tog <- extract.samples(m12_Sext)


tiff("Posterior__ATTR_14_groups_TOGETHER_STANDARDISED.tif",width=32,height=26,units="cm",res=600,compression = "lzw")
layout(matrix(1:4,byrow=T,nrow=1), widths=c(2.9,2,2,2),heights=1)

# 1 out 4: Iris Colour - WOMEN

par(mar=c(3.1, 10.1, 1.1, 1.1),mgp=c(5,0.7,0))
plot(NULL,xlim=c(-0.75,1),ylim=c(1,26.5),yaxt="n",bty="n", ylab="", xlab="", cex.axis=1.8, 
     main="Iris Colour - Women", cex.main=1.8)

rect(-0.75, 0, 1, 26, col="#10101010", border=NA)

segments(-0.75, c(seq(from=1, to=8, by=1),seq(from=9.5, to=16.5, by=1),seq(from=18, to=25, by=1)), 1, 
         c(seq(from=1, to=8, by=1),seq(from=9.5, to=16.5, by=1),seq(from=18, to=25, by=1)), col="#40808080")

segments(-0.75, 8, 1, 8, col="#908080")
rect(-0.75, 7.5, 1, 8.5, col="#10101010", border=NA)

segments(-0.75, 16.5, 1, 16.5, col="#908080")
rect(-0.75, 16, 1, 17, col="#10101010", border=NA)

segments(-0.75, 25, 1, 25, col="#908080")
rect(-0.75, 24.5, 1, 25.5, col="#10101010", border=NA)


segments(c(-0.75,-0.25,0.25,0.75),0,c(-0.75,-0.25,0.25,0.75),26,col="#80808080",lty=2)
segments(0,0,0,26,col="#990909", lwd=2)

# Just hit the right posteriors - that's enough: 
str(A_post_tog) # BETAS: num[1:10000, NINE SAMPLES, 24 SLOPES - 12 Women, 12 Men)

include <- list(
  # L_iris:
  A_post_tog$B_L_iris[,1], # GLOBAL - WOMEN
  (A_post_tog$B_L_iris[,1] + A_post_tog$betas[,1,7]), # L_iris women, Cameroon 
  (A_post_tog$B_L_iris[,1] + A_post_tog$betas[,2,7]), # L_iris women, Colombia
  (A_post_tog$B_L_iris[,1] + A_post_tog$betas[,3,7]), # L_iris women, CZ 
  (A_post_tog$B_L_iris[,1] + A_post_tog$betas[,4,7]), # L_iris women, India
  (A_post_tog$B_L_iris[,1] + A_post_tog$betas[,5,7]), # L_iris women, Iran
  (A_post_tog$B_L_iris[,1] + A_post_tog$betas[,6,7]), # L_iris women, Turkey
  (A_post_tog$B_L_iris[,1] + A_post_tog$betas[,7,7]), # L_iris women, Vietnam
  
  # A_iris
  A_post_tog$B_a_iris[,1], # GLOBAL - WOMEN
  (A_post_tog$B_a_iris[,1] + A_post_tog$betas[,1,8]), # a_iris women, Cameroon 
  (A_post_tog$B_a_iris[,1] + A_post_tog$betas[,2,8]), # a_iris women, Colombia
  (A_post_tog$B_a_iris[,1] + A_post_tog$betas[,3,8]), # a_iris women, CZ 
  (A_post_tog$B_a_iris[,1] + A_post_tog$betas[,4,8]), # a_iris women, India
  (A_post_tog$B_a_iris[,1] + A_post_tog$betas[,5,8]), # a_iris women, Iran
  (A_post_tog$B_a_iris[,1] + A_post_tog$betas[,6,8]), # a_iris women, Turkey
  (A_post_tog$B_a_iris[,1] + A_post_tog$betas[,7,8]), # a_iris women, Vietnam
  
  # B_iris
  A_post_tog$B_b_iris[,1], # GLOBAL - WOMEN
  (A_post_tog$B_b_iris[,1] + A_post_tog$betas[,1,9]), # b_iris women, Cameroon 
  (A_post_tog$B_b_iris[,1] + A_post_tog$betas[,2,9]), # b_iris women, Colombia
  (A_post_tog$B_b_iris[,1] + A_post_tog$betas[,3,9]), # b_iris women, CZ 
  (A_post_tog$B_b_iris[,1] + A_post_tog$betas[,4,9]), # b_iris women, India
  (A_post_tog$B_b_iris[,1] + A_post_tog$betas[,5,9]), # b_iris women, Iran
  (A_post_tog$B_b_iris[,1] + A_post_tog$betas[,6,9]), # b_iris women, Turkey
  (A_post_tog$B_b_iris[,1] + A_post_tog$betas[,7,9]) # b_iris women, Vietnam
)


cols<-c("#00FBFF", 
        "#8C00FF",
        "#00FF33", 
        "#FFA600",
        
        "#00FBFF", 
        "#8C00FF",
        "#00FF33", 
        "#FFA600",
        
        "#00FBFF", 
        "#8C00FF",
        "#00FF33", 
        "#FFA600",
        
        "#00FBFF", 
        "#8C00FF",
        "#00FF33", 
        "#FFA600",
        
        "#00FBFF", 
        "#8C00FF",
        "#00FF33", 
        "#FFA600",
        
        "#00FBFF", 
        "#8C00FF",
        "#00FF33", 
        "#FFA600"
)

cols<-paste(cols,"80",sep="") 

ys<- c(seq(from=1, to=8, by=1),seq(from=9.5, to=16.5, by=1),seq(from=18, to=25, by=1))
ys<-rev(ys)

labels <- c(  "L* Global",
              "L* Cameroon",
              "L* Colombia",
              "L* Czechia",
              "L* India - FCD",
              "L* Iran",
              "L* Turkey",
              "L* Vietnam",
              # A_iris
              "a* Global",
              "a* Cameroon",
              "a* Colombia",
              "a* Czechia",
              "a* India - FCD",
              "a* Iran",
              "a* Turkey",
              "a* Vietnam",
              # B_iris
              "b* Global",
              "b* Cameroon",
              "b* Colombia",
              "b* Czechia",
              "b* India - FCD",
              "b* Iran",
              "b* Turkey",
              "b* Vietnam"
)



cex_t2 <- 1.05
cex_t <- 1.15
pls <- -0.9


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
  points(x=mean(toplot_CI),ycoord,lwd=2,pch=21,col=1,bg=0,cex=1.2)
  axis(2,at=ycoord,labels=textlab,srt=0,las=1, pos=-0.72, cex.axis=1.7, tick=F, lwd.ticks=0) 
  for (i in 1:24) {
    c(polygon(c(dens$x,rev(dens$x)),ys[i]*(c(dens$y,rev(dens$y)*(-1))*0.003)+ycoord[i],border=F, lwd = 1.5, col=colpol))
  }
  lines(x=PI(toplot_CI,prob=0.95),rep(ycoord+0.05,2),lwd=2, lty=1, col='#2f4363')
  segments(x0 = ci[1], y0 = ycoord-0.15, x1 = ci[1], y1 = ycoord+0.25, lwd = 3, col = '#2f4363')
  segments(x0 = ci[2], y0 = ycoord-0.15, x1 = ci[2], y1 = ycoord+0.25, lwd = 3, col = '#2f4363')
  points(x=mean(toplot_CI),ycoord,lwd=2,pch=21,col=1,bg=0,cex=1.2)
}


# 2 out 4: Sclera Colour - WOMEN


# SCLERA: 
par(mar=c(3.1, 0.5, 1.1, 0.5),mgp=c(5,0.7,0))

plot(NULL,xlim=c(-0.75,1),ylim=c(1,26.5),yaxt="n",bty="n", ylab="", xlab="", cex.axis=1.8, 
     main="PIT (Sclera) colour - Women", cex.main=1.8)


rect(-0.75, 0, 1, 26, col="#10101010", border=NA)

segments(-0.75, c(seq(from=1, to=8, by=1),seq(from=9.5, to=16.5, by=1),seq(from=18, to=25, by=1)), 1, 
         c(seq(from=1, to=8, by=1),seq(from=9.5, to=16.5, by=1),seq(from=18, to=25, by=1)), col="#40808080")

segments(-0.75, 8, 1, 8, col="#908080")
rect(-0.75, 7.5, 1, 8.5, col="#10101010", border=NA)

segments(-0.75, 16.5, 1, 16.5, col="#908080")
rect(-0.75, 16, 1, 17, col="#10101010", border=NA)

segments(-0.75, 25, 1, 25, col="#908080")
rect(-0.75, 24.5, 1, 25.5, col="#10101010", border=NA)


segments(c(-0.75,-0.25,0.25,0.75),0,c(-0.75,-0.25,0.25,0.75),26,col="#80808080",lty=2)
segments(0,0,0,26,col="#990909", lwd=2)



include <- list(
  # SCLERA L:
  A_post_tog$B_L_sclera[,1], # GLOBAL - WOMEN
  (A_post_tog$B_L_sclera[,1] + A_post_tog$betas[,1,10]), # L_sclera women, Cameroon 
  (A_post_tog$B_L_sclera[,1] + A_post_tog$betas[,2,10]), # L_sclera women, Colombia
  (A_post_tog$B_L_sclera[,1] + A_post_tog$betas[,3,10]), # L_sclera women, CZ 
  (A_post_tog$B_L_sclera[,1] + A_post_tog$betas[,4,10]), # L_sclera women, India
  (A_post_tog$B_L_sclera[,1] + A_post_tog$betas[,5,10]), # L_sclera women, Iran
  (A_post_tog$B_L_sclera[,1] + A_post_tog$betas[,6,10]), # L_sclera women, Turkey
  (A_post_tog$B_L_sclera[,1] + A_post_tog$betas[,7,10]), # L_sclera women, Vietnam
  
  # SCLERA A
  A_post_tog$B_a_sclera[,1], # GLOBAL - WOMEN
  (A_post_tog$B_a_sclera[,1] + A_post_tog$betas[,1,11]), # a_sclera women, Cameroon 
  (A_post_tog$B_a_sclera[,1] + A_post_tog$betas[,2,11]), # a_sclera women, Colombia
  (A_post_tog$B_a_sclera[,1] + A_post_tog$betas[,3,11]), # a_sclera women, CZ 
  (A_post_tog$B_a_sclera[,1] + A_post_tog$betas[,4,11]), # a_sclera women, India
  (A_post_tog$B_a_sclera[,1] + A_post_tog$betas[,5,11]), # a_sclera women, Iran
  (A_post_tog$B_a_sclera[,1] + A_post_tog$betas[,6,11]), # a_sclera women, Turkey
  (A_post_tog$B_a_sclera[,1] + A_post_tog$betas[,7,11]), # a_sclera women, Vietnam
  
  # SCLERA B
  A_post_tog$B_b_sclera[,1], # GLOBAL - WOMEN
  (A_post_tog$B_b_sclera[,1] + A_post_tog$betas[,1,12]), # b_sclera women, Cameroon 
  (A_post_tog$B_b_sclera[,1] + A_post_tog$betas[,2,12]), # b_sclera women, Colombia
  (A_post_tog$B_b_sclera[,1] + A_post_tog$betas[,3,12]), # b_sclera women, CZ 
  (A_post_tog$B_b_sclera[,1] + A_post_tog$betas[,4,12]), # b_sclera women, India
  (A_post_tog$B_b_sclera[,1] + A_post_tog$betas[,5,12]), # b_sclera women, Iran
  (A_post_tog$B_b_sclera[,1] + A_post_tog$betas[,6,12]), # b_sclera women, Turkey
  (A_post_tog$B_b_sclera[,1] + A_post_tog$betas[,7,12]) # b_sclera women, Vietnam
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
  points(x=mean(toplot_CI),ycoord,lwd=2,pch=21,col=1,bg=0,cex=1.2)
  for (i in 1:24) {
    c(polygon(c(dens$x,rev(dens$x)),ys[i]*(c(dens$y,rev(dens$y)*(-1))*0.003)+ycoord[i],border=F, lwd = 1.5, col=colpol))
  }
  lines(x=PI(toplot_CI,prob=0.95),rep(ycoord+0.05,2),lwd=2, lty=1, col='#2f4363')
  segments(x0 = ci[1], y0 = ycoord-0.15, x1 = ci[1], y1 = ycoord+0.25, lwd = 3, col = '#2f4363')
  segments(x0 = ci[2], y0 = ycoord-0.15, x1 = ci[2], y1 = ycoord+0.25, lwd = 3, col = '#2f4363')
  points(x=mean(toplot_CI),ycoord,lwd=2,pch=21,col=1,bg=0,cex=1.2)
}


# 3 out 4: Iris Colour - MEN


par(mar=c(3.1, 0.5, 1.1, 0.5),mgp=c(6,0.7,0))
plot(NULL,xlim=c(-0.75,1),ylim=c(1,26.5),yaxt="n",bty="n", ylab="", xlab="", cex.axis=1.8, 
     main="Iris Colour - Men", cex.main=1.8)


rect(-0.75, 0, 1, 26, col="#10101010", border=NA)

segments(-0.75, c(seq(from=1, to=8, by=1),seq(from=9.5, to=16.5, by=1),seq(from=18, to=25, by=1)), 1, 
         c(seq(from=1, to=8, by=1),seq(from=9.5, to=16.5, by=1),seq(from=18, to=25, by=1)), col="#40808080")

segments(-0.75, 8, 1, 8, col="#908080")
rect(-0.75, 7.5, 1, 8.5, col="#10101010", border=NA)

segments(-0.75, 16.5, 1, 16.5, col="#908080")
rect(-0.75, 16, 1, 17, col="#10101010", border=NA)

segments(-0.75, 25, 1, 25, col="#908080")
rect(-0.75, 24.5, 1, 25.5, col="#10101010", border=NA)


segments(c(-0.75,-0.25,0.25,0.75),0,c(-0.75,-0.25,0.25,0.75),26,col="#80808080",lty=2)
segments(0,0,0,26,col="#990909", lwd=2)

include <- list(
  # L_iris:
  A_post_tog$B_L_iris[,2], # GLOBAL - MEN
  (A_post_tog$B_L_iris[,2] + A_post_tog$betas[,1,19]), # L_iris men, Cameroon 
  (A_post_tog$B_L_iris[,2] + A_post_tog$betas[,2,19]), # L_iris men, Colombia
  (A_post_tog$B_L_iris[,2] + A_post_tog$betas[,3,19]), # L_iris men, CZ 
  (A_post_tog$B_L_iris[,2] + A_post_tog$betas[,4,19]), # L_iris men, India
  (A_post_tog$B_L_iris[,2] + A_post_tog$betas[,5,19]), # L_iris men, Iran
  (A_post_tog$B_L_iris[,2] + A_post_tog$betas[,6,19]), # L_iris men, Turkey
  (A_post_tog$B_L_iris[,2] + A_post_tog$betas[,7,19]), # L_iris men, Vietnam
  
  # A_iris
  A_post_tog$B_a_iris[,2], # GLOBAL - WOMEN
  (A_post_tog$B_a_iris[,2] + A_post_tog$betas[,1,20]), # a_iris men, Cameroon 
  (A_post_tog$B_a_iris[,2] + A_post_tog$betas[,2,20]), # a_iris men, Colombia
  (A_post_tog$B_a_iris[,2] + A_post_tog$betas[,3,20]), # a_iris men, CZ 
  (A_post_tog$B_a_iris[,2] + A_post_tog$betas[,4,20]), # a_iris men, India
  (A_post_tog$B_a_iris[,2] + A_post_tog$betas[,5,20]), # a_iris men, Iran
  (A_post_tog$B_a_iris[,2] + A_post_tog$betas[,6,20]), # a_iris men, Turkey
  (A_post_tog$B_a_iris[,2] + A_post_tog$betas[,7,20]), # a_iris men, Vietnam
  
  # B_iris
  A_post_tog$B_b_iris[,2], # GLOBAL - WOMEN
  (A_post_tog$B_b_iris[,2] + A_post_tog$betas[,1,21]), # b_iris men, Cameroon 
  (A_post_tog$B_b_iris[,2] + A_post_tog$betas[,2,21]), # b_iris men, Colombia
  (A_post_tog$B_b_iris[,2] + A_post_tog$betas[,3,21]), # b_iris men, CZ 
  (A_post_tog$B_b_iris[,2] + A_post_tog$betas[,4,21]), # b_iris men, India
  (A_post_tog$B_b_iris[,2] + A_post_tog$betas[,5,21]), # b_iris men, Iran
  (A_post_tog$B_b_iris[,2] + A_post_tog$betas[,6,21]), # b_iris men, Turkey
  (A_post_tog$B_b_iris[,2] + A_post_tog$betas[,7,21]) # b_iris men, Vietnam
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
  points(x=mean(toplot_CI),ycoord,lwd=2,pch=21,col=1,bg=0,cex=1.2)
  for (i in 1:24) {
    c(polygon(c(dens$x,rev(dens$x)),ys[i]*(c(dens$y,rev(dens$y)*(-1))*0.003)+ycoord[i],border=F, lwd = 1.5, col=colpol))
  }
  lines(x=PI(toplot_CI,prob=0.95),rep(ycoord+0.05,2),lwd=2, lty=1, col='#2f4363')
  segments(x0 = ci[1], y0 = ycoord-0.15, x1 = ci[1], y1 = ycoord+0.25, lwd = 3, col = '#2f4363')
  segments(x0 = ci[2], y0 = ycoord-0.15, x1 = ci[2], y1 = ycoord+0.25, lwd = 3, col = '#2f4363')
  points(x=mean(toplot_CI),ycoord,lwd=2,pch=21,col=1,bg=0,cex=1.2)
}


# 4 out 4: Sclera Colour - MEN


par(mar=c(3.1, 0.5, 1.1, 0.5),mgp=c(5,0.7,0))
plot(NULL,xlim=c(-0.75,1),ylim=c(1,26.5),yaxt="n",bty="n", ylab="", xlab="", cex.axis=1.8, 
     main="PIT (Sclera) Colour -  Men", cex.main=1.8)

rect(-0.75, 0, 1, 26, col="#10101010", border=NA)

segments(-0.75, c(seq(from=1, to=8, by=1),seq(from=9.5, to=16.5, by=1),seq(from=18, to=25, by=1)), 1, 
         c(seq(from=1, to=8, by=1),seq(from=9.5, to=16.5, by=1),seq(from=18, to=25, by=1)), col="#40808080")

segments(-0.75, 8, 1, 8, col="#908080")
rect(-0.75, 7.5, 1, 8.5, col="#10101010", border=NA)

segments(-0.75, 16.5, 1, 16.5, col="#908080")
rect(-0.75, 16, 1, 17, col="#10101010", border=NA)

segments(-0.75, 25, 1, 25, col="#908080")
rect(-0.75, 24.5, 1, 25.5, col="#10101010", border=NA)


segments(c(-0.75,-0.25,0.25,0.75),0,c(-0.75,-0.25,0.25,0.75),26,col="#80808080",lty=2)
segments(0,0,0,26,col="#990909", lwd=2)




include <- list(
  # L_Sclera:
  A_post_tog$B_L_sclera[,2], # GLOBAL - MEN
  (A_post_tog$B_L_sclera[,2] + A_post_tog$betas[,1,22]), # L_sclera men, Cameroon 
  (A_post_tog$B_L_sclera[,2] + A_post_tog$betas[,2,22]), # L_sclera men, Colombia
  (A_post_tog$B_L_sclera[,2] + A_post_tog$betas[,3,22]), # L_sclera men, CZ 
  (A_post_tog$B_L_sclera[,2] + A_post_tog$betas[,4,22]), # L_sclera men, India
  (A_post_tog$B_L_sclera[,2] + A_post_tog$betas[,5,22]), # L_sclera men, Iran
  (A_post_tog$B_L_sclera[,2] + A_post_tog$betas[,6,22]), # L_sclera men, Turkey
  (A_post_tog$B_L_sclera[,2] + A_post_tog$betas[,7,22]), # L_sclera men, Vietnam
  
  # A_Sclera
  A_post_tog$B_a_sclera[,2], # GLOBAL - MEN
  (A_post_tog$B_a_sclera[,2] + A_post_tog$betas[,1,23]), # a_sclera men, Cameroon 
  (A_post_tog$B_a_sclera[,2] + A_post_tog$betas[,2,23]), # a_sclera men, Colombia
  (A_post_tog$B_a_sclera[,2] + A_post_tog$betas[,3,23]), # a_sclera men, CZ 
  (A_post_tog$B_a_sclera[,2] + A_post_tog$betas[,4,23]), # a_sclera men, India
  (A_post_tog$B_a_sclera[,2] + A_post_tog$betas[,5,23]), # a_sclera men, Iran
  (A_post_tog$B_a_sclera[,2] + A_post_tog$betas[,6,23]), # a_sclera men, Turkey
  (A_post_tog$B_a_sclera[,2] + A_post_tog$betas[,7,23]), # a_sclera men, Vietnam
  
  # B_Sclera
  A_post_tog$B_b_sclera[,2], # GLOBAL - MEN
  (A_post_tog$B_b_sclera[,2] + A_post_tog$betas[,1,24]), # b_sclera men, Cameroon 
  (A_post_tog$B_b_sclera[,2] + A_post_tog$betas[,2,24]), # b_sclera men, Colombia
  (A_post_tog$B_b_sclera[,2] + A_post_tog$betas[,3,24]), # b_sclera men, CZ
  (A_post_tog$B_b_sclera[,2] + A_post_tog$betas[,4,24]), # b_sclera men, India
  (A_post_tog$B_b_sclera[,2] + A_post_tog$betas[,5,24]), # b_sclera men, Iran
  (A_post_tog$B_b_sclera[,2] + A_post_tog$betas[,6,24]), # b_sclera men, Turkey
  (A_post_tog$B_b_sclera[,2] + A_post_tog$betas[,7,24]) # b_sclera men, Vietnam
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
  points(x=mean(toplot_CI),ycoord,lwd=2,pch=21,col=1,bg=0,cex=1.2)
 for (i in 1:24) {
    c(polygon(c(dens$x,rev(dens$x)),ys[i]*(c(dens$y,rev(dens$y)*(-1))*0.003)+ycoord[i],border=F, lwd = 1.5, col=colpol))
  }
  lines(x=PI(toplot_CI,prob=0.95),rep(ycoord+0.05,2),lwd=2, lty=1, col='#2f4363')
  segments(x0 = ci[1], y0 = ycoord-0.15, x1 = ci[1], y1 = ycoord+0.25, lwd = 3, col = '#2f4363')
  segments(x0 = ci[2], y0 = ycoord-0.15, x1 = ci[2], y1 = ycoord+0.25, lwd = 3, col = '#2f4363')
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


# betas[,8,13] -> Intercept men, Turkey

# No Vietnam, but the rest is the same.

tiff("Posterior__SEXT_12_groups_TOGETHER_STANDARDISED.tif",width=32,height=26,units="cm",res=600,compression = "lzw")
layout(matrix(1:4,byrow=T,nrow=1), widths=c(2.9,2,2,2),heights=1)

par(mar=c(5.1, 10.1, 1.1, 1.1),mgp=c(5,0.7,0))
plot(NULL,xlim=c(-0.75,1),ylim=c(1,22.5),yaxt="n",bty="n", ylab="", cex.axis=1.8, 
     main="Iris Colour - Women", cex.main=1.8)

rect(-0.75, 0, 1, 22.5, col="#10101010", border=NA)

segments(-0.75, c(seq(from=1, to=7, by=1),seq(from=8.5, to=14.5, by=1),seq(from=16, to=22, by=1)), 1, 
         c(seq(from=1, to=7, by=1),seq(from=8.5, to=14.5, by=1),seq(from=16, to=22, by=1)), col="#40808080")

segments(-0.75, 7, 1, 7, col="#908080", lwd=2)
rect(-0.75, 6.5, 1, 7.5, col="#10101010", border=NA)

segments(-0.75, 14.5, 1, 14.5, col="#908080", lwd=2)
rect(-0.75, 14, 1, 15, col="#10101010", border=NA)

segments(-0.75, 22, 1, 22, col="#908080", lwd=2)
rect(-0.75, 21.5, 1, 22.5, col="#10101010", border=NA)


segments(c(-0.75,-0.25,0.25,0.75),0,c(-0.75,-0.25,0.25,0.75),22.5,col="#80808080",lty=2)
segments(0,0,0,22.5,col="#990909", lwd=2)


include <- list(
  # L_iris:
  S_post_tog$B_L_iris[,1], # GLOBAL - WOMEN
  (S_post_tog$B_L_iris[,1] + S_post_tog$betas[,1,7]), # L_iris women, Cameroon 
  (S_post_tog$B_L_iris[,1] + S_post_tog$betas[,2,7]), # L_iris women, Colombia
  (S_post_tog$B_L_iris[,1] + S_post_tog$betas[,3,7]), # L_iris women, CZ 
  (S_post_tog$B_L_iris[,1] + S_post_tog$betas[,4,7]), # L_iris women, India
  (S_post_tog$B_L_iris[,1] + S_post_tog$betas[,5,7]), # L_iris women, Iran
  (S_post_tog$B_L_iris[,1] + S_post_tog$betas[,6,7]), # L_iris women, Turkey
  
  # A_iris
  S_post_tog$B_a_iris[,1], # GLOBAL - WOMEN
  (S_post_tog$B_a_iris[,1] + S_post_tog$betas[,1,8]), # a_iris women, Cameroon 
  (S_post_tog$B_a_iris[,1] + S_post_tog$betas[,2,8]), # a_iris women, Colombia
  (S_post_tog$B_a_iris[,1] + S_post_tog$betas[,3,8]), # a_iris women, CZ 
  (S_post_tog$B_a_iris[,1] + S_post_tog$betas[,4,8]), # a_iris women, India
  (S_post_tog$B_a_iris[,1] + S_post_tog$betas[,5,8]), # a_iris women, Iran
  (S_post_tog$B_a_iris[,1] + S_post_tog$betas[,6,8]), # a_iris women, Turkey
  
  # B_iris
  S_post_tog$B_b_iris[,1], # GLOBAL - WOMEN
  (S_post_tog$B_b_iris[,1] + S_post_tog$betas[,1,9]), # b_iris women, Cameroon 
  (S_post_tog$B_b_iris[,1] + S_post_tog$betas[,2,9]), # b_iris women, Colombia
  (S_post_tog$B_b_iris[,1] + S_post_tog$betas[,3,9]), # b_iris women, CZ 
  (S_post_tog$B_b_iris[,1] + S_post_tog$betas[,4,9]), # b_iris women, India
  (S_post_tog$B_b_iris[,1] + S_post_tog$betas[,5,9]), # b_iris women, Iran
  (S_post_tog$B_b_iris[,1] + S_post_tog$betas[,6,9]) # b_iris women, Turkey
)



cols<-c("#00FBFF", 
        "#8C00FF",
        "#00FF33", 
        "#FFA600",
        
        "#00FBFF", 
        "#8C00FF",
        "#00FF33", 

        "#00FBFF", 
        "#8C00FF",
        "#00FF33", 
        "#FFA600",
        
        "#00FBFF", 
        "#8C00FF",
        "#00FF33", 

        "#00FBFF", 
        "#8C00FF",
        "#00FF33", 
        "#FFA600",
        
        "#00FBFF", 
        "#8C00FF",
        "#00FF33"
)

cols<-paste(cols,"80",sep="") 

ys<- c(seq(from=1, to=7, by=1),seq(from=8.5, to=14.5, by=1),seq(from=16, to=22, by=1))
ys<-rev(ys)

labels <- c(  "L* Global",
              "L* Cameroon",
              "L* Colombia",
              "L* Czechia",
              "L* India - FCD",
              "L* Iran",
              "L* Turkey",
              # A_iris
              "a* Global",
              "a* Cameroon",
              "a* Colombia",
              "a* Czechia",
              "a* India - FCD",
              "a* Iran",
              "a* Turkey",
              # B_iris
              "b* Global",
              "b* Cameroon",
              "b* Colombia",
              "b* Czechia",
              "b* India - FCD",
              "b* Iran",
              "b* Turkey"
)


cex_t <- 1.15
cex_t2 <- 1.05
pls <- -0.9

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
  points(x=mean(toplot_CI),ycoord,lwd=2,pch=21,col=1,bg=0,cex=1.2)
  axis(2,at=ycoord,labels=textlab,srt=0,las=1, cex.axis=1.7, pos=-0.72, tick=F, lwd.ticks=0) 
   for (i in 1:21) {
    c(polygon(c(dens$x,rev(dens$x)),ys[i]*(c(dens$y,rev(dens$y)*(-1))*0.003)+ycoord[i],border=F, lwd = 1.5, col=colpol))
  }
  lines(x=PI(toplot_CI,prob=0.95),rep(ycoord+0.05,2),lwd=2, lty=1, col='#2f4363')
  segments(x0 = ci[1], y0 = ycoord-0.15, x1 = ci[1], y1 = ycoord+0.25, lwd = 3, col = '#2f4363')
  segments(x0 = ci[2], y0 = ycoord-0.15, x1 = ci[2], y1 = ycoord+0.25, lwd = 3, col = '#2f4363')
  points(x=mean(toplot_CI),ycoord,lwd=2,pch=21,col=1,bg=0,cex=1.2)
}


# SCLERA: 
par(mar=c(5.1, 1.1, 1.1, 1.1),mgp=c(5,0.7,0))
plot(NULL,xlim=c(-0.7,1),ylim=c(1,22.5),yaxt="n",bty="n", ylab="", cex.axis=1.8, 
     main="PIT (Sclera) Colour - Women", cex.main=1.8)

rect(-0.75, 0, 1, 22.5, col="#10101010", border=NA)

segments(-0.75, c(seq(from=1, to=7, by=1),seq(from=8.5, to=14.5, by=1),seq(from=16, to=22, by=1)), 1, 
         c(seq(from=1, to=7, by=1),seq(from=8.5, to=14.5, by=1),seq(from=16, to=22, by=1)), col="#40808080")

segments(-0.75, 7, 1, 7, col="#908080", lwd=2)
rect(-0.75, 6.5, 1, 7.5, col="#10101010", border=NA)

segments(-0.75, 14.5, 1, 14.5, col="#908080", lwd=2)
rect(-0.75, 14, 1, 15, col="#10101010", border=NA)

segments(-0.75, 22, 1, 22, col="#908080", lwd=2)
rect(-0.75, 21.5, 1, 22.5, col="#10101010", border=NA)


segments(c(-0.75,-0.25,0.25,0.75),0,c(-0.75,-0.25,0.25,0.75),22.5,col="#80808080",lty=2)
segments(0,0,0,22.5,col="#990909", lwd=2)



include <- list(
  # SCLERA L:
  S_post_tog$B_L_sclera[,1], # GLOBAL - WOMEN
  (S_post_tog$B_L_sclera[,1] + S_post_tog$betas[,1,10]), # L_sclera women, Cameroon 
  (S_post_tog$B_L_sclera[,1] + S_post_tog$betas[,2,10]), # L_sclera women, Colombia
  (S_post_tog$B_L_sclera[,1] + S_post_tog$betas[,3,10]), # L_sclera women, CZ 
  (S_post_tog$B_L_sclera[,1] + S_post_tog$betas[,4,10]), # L_sclera women, India
  (S_post_tog$B_L_sclera[,1] + S_post_tog$betas[,5,10]), # L_sclera women, Iran
  (S_post_tog$B_L_sclera[,1] + S_post_tog$betas[,6,10]), # L_sclera women, Turkey
  
  # SCLERA A
  S_post_tog$B_a_sclera[,1], # GLOBAL - WOMEN
  (S_post_tog$B_a_sclera[,1] + S_post_tog$betas[,1,11]), # a_sclera women, Cameroon
  (S_post_tog$B_a_sclera[,1] + S_post_tog$betas[,2,11]), # a_sclera women, Colombia
  (S_post_tog$B_a_sclera[,1] + S_post_tog$betas[,3,11]), # a_sclera women, CZ 
  (S_post_tog$B_a_sclera[,1] + S_post_tog$betas[,4,11]), # a_sclera women, India
  (S_post_tog$B_a_sclera[,1] + S_post_tog$betas[,5,11]), # a_sclera women, Iran
  (S_post_tog$B_a_sclera[,1] + S_post_tog$betas[,6,11]), # a_sclera women, Turkey
  
  # SCLERA B
  S_post_tog$B_b_sclera[,1], # GLOBAL - WOMEN
  (S_post_tog$B_b_sclera[,1] + S_post_tog$betas[,1,12]), # b_sclera women, Cameroon 
  (S_post_tog$B_b_sclera[,1] + S_post_tog$betas[,2,12]), # b_sclera women, Colombia
  (S_post_tog$B_b_sclera[,1] + S_post_tog$betas[,3,12]), # b_sclera women, CZ 
  (S_post_tog$B_b_sclera[,1] + S_post_tog$betas[,4,12]), # b_sclera women, India
  (S_post_tog$B_b_sclera[,1] + S_post_tog$betas[,5,12]), # b_sclera women, Iran
  (S_post_tog$B_b_sclera[,1] + S_post_tog$betas[,6,12]) # b_sclera women, Turkey
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
  for (i in 1:21) {
    c(polygon(c(dens$x,rev(dens$x)),ys[i]*(c(dens$y,rev(dens$y)*(-1))*0.003)+ycoord[i],border=F, lwd = 1.5, col=colpol))
  }
  lines(x=PI(toplot_CI,prob=0.95),rep(ycoord+0.05,2),lwd=2, lty=1, col='#2f4363')
  segments(x0 = ci[1], y0 = ycoord-0.15, x1 = ci[1], y1 = ycoord+0.25, lwd = 3, col = '#2f4363')
  segments(x0 = ci[2], y0 = ycoord-0.15, x1 = ci[2], y1 = ycoord+0.25, lwd = 3, col = '#2f4363')
  points(x=mean(toplot_CI),ycoord,lwd=2,pch=21,col=1,bg=0,cex=1.2)
}


# MEN: 

par(mar=c(5.1, 1.1, 1.1, 1.1),mgp=c(5,0.7,0))
plot(NULL,xlim=c(-0.7,1),ylim=c(1,22.5),yaxt="n",bty="n", ylab="", cex.axis=1.8, 
     main="Iris Colour - Men", cex.main=1.8)


rect(-0.75, 0, 1, 22.5, col="#10101010", border=NA)

segments(-0.75, c(seq(from=1, to=7, by=1),seq(from=8.5, to=14.5, by=1),seq(from=16, to=22, by=1)), 1, 
         c(seq(from=1, to=7, by=1),seq(from=8.5, to=14.5, by=1),seq(from=16, to=22, by=1)), col="#40808080")

segments(-0.75, 7, 1, 7, col="#908080", lwd=2)
rect(-0.75, 6.5, 1, 7.5, col="#10101010", border=NA)

segments(-0.75, 14.5, 1, 14.5, col="#908080", lwd=2)
rect(-0.75, 14, 1, 15, col="#10101010", border=NA)

segments(-0.75, 22, 1, 22, col="#908080", lwd=2)
rect(-0.75, 21.5, 1, 22.5, col="#10101010", border=NA)


segments(c(-0.75,-0.25,0.25,0.75),0,c(-0.75,-0.25,0.25,0.75),22.5,col="#80808080",lty=2)
segments(0,0,0,22.5,col="#990909", lwd=2)



include <- list(
  # L_iris:
  S_post_tog$B_L_iris[,2], # GLOBAL - MEN
  (S_post_tog$B_L_iris[,2] + S_post_tog$betas[,1,19]), # L_iris men, Cameroon 
  (S_post_tog$B_L_iris[,2] + S_post_tog$betas[,2,19]), # L_iris men, Colombia
  (S_post_tog$B_L_iris[,2] + S_post_tog$betas[,3,19]), # L_iris men, CZ 
  (S_post_tog$B_L_iris[,2] + S_post_tog$betas[,4,19]), # L_iris men, India
  (S_post_tog$B_L_iris[,2] + S_post_tog$betas[,5,19]), # L_iris men, Iran
  (S_post_tog$B_L_iris[,2] + S_post_tog$betas[,6,19]), # L_iris men, Turkey
  
  # A_iris
  S_post_tog$B_a_iris[,2], # GLOBAL - WOMEN
  (S_post_tog$B_a_iris[,2] + S_post_tog$betas[,1,20]), # a_iris men, Cameroon
  (S_post_tog$B_a_iris[,2] + S_post_tog$betas[,2,20]), # a_iris men, Colombia
  (S_post_tog$B_a_iris[,2] + S_post_tog$betas[,3,20]), # a_iris men, CZ
  (S_post_tog$B_a_iris[,2] + S_post_tog$betas[,4,20]), # a_iris men, India
  (S_post_tog$B_a_iris[,2] + S_post_tog$betas[,5,20]), # a_iris men, Iran
  (S_post_tog$B_a_iris[,2] + S_post_tog$betas[,6,20]), # a_iris men, Turkey
  
  # B_iris
  S_post_tog$B_b_iris[,2], # GLOBAL - WOMEN
  (S_post_tog$B_b_iris[,2] + S_post_tog$betas[,1,21]), # b_iris men, Cameroon 
  (S_post_tog$B_b_iris[,2] + S_post_tog$betas[,2,21]), # b_iris men, Colombia
  (S_post_tog$B_b_iris[,2] + S_post_tog$betas[,3,21]), # b_iris men, CZ 
  (S_post_tog$B_b_iris[,2] + S_post_tog$betas[,4,21]), # b_iris men, India
  (S_post_tog$B_b_iris[,2] + S_post_tog$betas[,5,21]), # b_iris men, Iran
  (S_post_tog$B_b_iris[,2] + S_post_tog$betas[,6,21]) # b_iris men, Turkey
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
  for (i in 1:21) {
    c(polygon(c(dens$x,rev(dens$x)),ys[i]*(c(dens$y,rev(dens$y)*(-1))*0.003)+ycoord[i],border=F, lwd = 1.5, col=colpol))
  }
  lines(x=PI(toplot_CI,prob=0.95),rep(ycoord+0.05,2),lwd=2, lty=1, col='#2f4363')
  segments(x0 = ci[1], y0 = ycoord-0.15, x1 = ci[1], y1 = ycoord+0.25, lwd = 3, col = '#2f4363')
  segments(x0 = ci[2], y0 = ycoord-0.15, x1 = ci[2], y1 = ycoord+0.25, lwd = 3, col = '#2f4363')
  points(x=mean(toplot_CI),ycoord,lwd=2,pch=21,col=1,bg=0,cex=1.2)
}


# SCLERA_

par(mar=c(5.1, 1.1, 1.1, 1.1),mgp=c(5,0.7,0))
plot(NULL,xlim=c(-0.7,1),ylim=c(1,22.5),yaxt="n",bty="n", ylab="", cex.axis=1.8, 
     main="Sclera Colour: MEN", cex.main=1.8)

rect(-0.75, 0, 1, 22.5, col="#10101010", border=NA)

segments(-0.75, c(seq(from=1, to=7, by=1),seq(from=8.5, to=14.5, by=1),seq(from=16, to=22, by=1)), 1, 
         c(seq(from=1, to=7, by=1),seq(from=8.5, to=14.5, by=1),seq(from=16, to=22, by=1)), col="#40808080")

segments(-0.75, 7, 1, 7, col="#908080", lwd=2)
rect(-0.75, 6.5, 1, 7.5, col="#10101010", border=NA)

segments(-0.75, 14.5, 1, 14.5, col="#908080", lwd=2)
rect(-0.75, 14, 1, 15, col="#10101010", border=NA)

segments(-0.75, 22, 1, 22, col="#908080", lwd=2)
rect(-0.75, 21.5, 1, 22.5, col="#10101010", border=NA)


segments(c(-0.75,-0.25,0.25,0.75),0,c(-0.75,-0.25,0.25,0.75),22.5,col="#80808080",lty=2)
segments(0,0,0,22.5,col="#990909", lwd=2)



include <- list(
  # L_Sclera:
  S_post_tog$B_L_sclera[,2], # GLOBAL - MEN
  (S_post_tog$B_L_sclera[,2] + S_post_tog$betas[,1,22]), # L_sclera men, Cameroon 
  (S_post_tog$B_L_sclera[,2] + S_post_tog$betas[,2,22]), # L_sclera men, Colombia
  (S_post_tog$B_L_sclera[,2] + S_post_tog$betas[,3,22]), # L_Sclera men, CZ 
  (S_post_tog$B_L_sclera[,2] + S_post_tog$betas[,4,22]), # L_sclera men, India
  (S_post_tog$B_L_sclera[,2] + S_post_tog$betas[,5,22]), # L_sclera men, Iran
  (S_post_tog$B_L_sclera[,2] + S_post_tog$betas[,6,22]), # L_sclera men, Turkey
  
  # A_Sclera
  S_post_tog$B_a_sclera[,2], # GLOBAL - MEN
  (S_post_tog$B_a_sclera[,2] + S_post_tog$betas[,1,23]), # a_sclera men, Cameroon 
  (S_post_tog$B_a_sclera[,2] + S_post_tog$betas[,2,23]), # a_sclera men, Colombia
  (S_post_tog$B_a_sclera[,2] + S_post_tog$betas[,3,23]), # a_sclera men, CZ 
  (S_post_tog$B_a_sclera[,2] + S_post_tog$betas[,4,23]), # a_sclera men, India
  (S_post_tog$B_a_sclera[,2] + S_post_tog$betas[,5,23]), # a_sclera men, Iran
  (S_post_tog$B_a_sclera[,2] + S_post_tog$betas[,6,23]), # a_sclera men, Turkey
  
  # B_Sclera
  S_post_tog$B_b_sclera[,2], # GLOBAL - MEN
  (S_post_tog$B_b_sclera[,2] + S_post_tog$betas[,1,24]), # b_sclera men, Cameroon 
  (S_post_tog$B_b_sclera[,2] + S_post_tog$betas[,2,24]), # b_sclera men, Colombia
  (S_post_tog$B_b_sclera[,2] + S_post_tog$betas[,3,24]), # b_sclera men, CZ 
  (S_post_tog$B_b_sclera[,2] + S_post_tog$betas[,4,24]), # b_sclera men, India
  (S_post_tog$B_b_sclera[,2] + S_post_tog$betas[,5,24]), # b_sclera men, Iran
  (S_post_tog$B_b_sclera[,2] + S_post_tog$betas[,6,24]) # b_sclera men, Turkey
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
  points(x=mean(toplot_CI),ycoord,lwd=2,pch=21,col=1,bg=0,cex=1.2)
  for (i in 1:21) {
    c(polygon(c(dens$x,rev(dens$x)),ys[i]*(c(dens$y,rev(dens$y)*(-1))*0.003)+ycoord[i],border=F, lwd = 1.5, col=colpol))
  }
  lines(x=PI(toplot_CI,prob=0.95),rep(ycoord+0.05,2),lwd=2, lty=1, col='#2f4363')
  segments(x0 = ci[1], y0 = ycoord-0.15, x1 = ci[1], y1 = ycoord+0.25, lwd = 3, col = '#2f4363')
  segments(x0 = ci[2], y0 = ycoord-0.15, x1 = ci[2], y1 = ycoord+0.25, lwd = 3, col = '#2f4363')
  points(x=mean(toplot_CI),ycoord,lwd=2,pch=21,col=1,bg=0,cex=1.2)
}

dev.off()



#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-
# Let's reduce even further...
# Four groups: 

# Attractiveness

load("Model_8groups_ATTRACT_both_sexes_standardised_together.Rdata")
load("Model_8groups_SEXTYP_both_sexes_standardised_together.Rdata")

A_post_tog <- extract.samples(m8g_attr)
S_post_tog <- extract.samples(m8g_Sext)


tiff("Posterior__ATTR_8_groups_TOGETHER_STANDARDISED.tif",width=32,height=16,units="cm",res=600,compression = "lzw")
layout(matrix(1:4,byrow=T,nrow=1), widths=c(2.9,2,2,2),heights=1)

# 1 out 4: Iris Colour - WOMEN

par(mar=c(3.1, 10.1, 1.1, 1.1),mgp=c(5,0.7,0))
plot(NULL,xlim=c(-0.75,1),ylim=c(1,17.5),yaxt="n",bty="n", ylab="", xlab="", cex.axis=1.8, 
     main="Iris Colour - Women", cex.main=1.8)

rect(-0.75, 0, 1, 17, col="#10101010", border=NA)

segments(-0.75, c(seq(from=1, to=5, by=1),seq(from=6.5, to=10.5, by=1),seq(from=12, to=16, by=1)), 1, 
         c(seq(from=1, to=5, by=1),seq(from=6.5, to=10.5, by=1),seq(from=12, to=16, by=1)), col="#40808080")

segments(-0.75, 5, 1, 5, col="#908080")
rect(-0.75, 4.5, 1, 5.5, col="#10101010", border=NA)

segments(-0.75, 10.5, 1, 10.5, col="#908080")
rect(-0.75, 10, 1, 11, col="#10101010", border=NA)

segments(-0.75, 16, 1, 16, col="#908080")
rect(-0.75, 15.5, 1, 16.5, col="#10101010", border=NA)


segments(c(-0.75,-0.5,-0.25,0.25,0.5,0.75),0,c(-0.75,-0.5,-0.25,0.25,0.5,0.75),16.5,col="#80808080",lty=2)
segments(0,0,0,16.5,col="#990909", lwd=2)

# Just hit the right posteriors - that's enough: 
str(A_post_tog) # BETAS: num[1:10000, NINE SAMPLES, 24 SLOPES - 12 Women, 12 Men)

include <- list(
  # L_iris:
  A_post_tog$B_L_iris[,1], # GLOBAL - WOMEN
  (A_post_tog$B_L_iris[,1] + A_post_tog$betas[,1,7]), # L_iris women, Cameroon 
  (A_post_tog$B_L_iris[,1] + A_post_tog$betas[,2,7]), # L_iris women, Colombia
  (A_post_tog$B_L_iris[,1] + A_post_tog$betas[,3,7]), # L_iris women, CZ 
  (A_post_tog$B_L_iris[,1] + A_post_tog$betas[,4,7]), # L_iris women, Asia

  
  # A_iris
  A_post_tog$B_a_iris[,1], # GLOBAL - WOMEN
  (A_post_tog$B_a_iris[,1] + A_post_tog$betas[,1,8]), # a_iris women, Cameroon 
  (A_post_tog$B_a_iris[,1] + A_post_tog$betas[,2,8]), # a_iris women, Colombia
  (A_post_tog$B_a_iris[,1] + A_post_tog$betas[,3,8]), # a_iris women, CZ 
  (A_post_tog$B_a_iris[,1] + A_post_tog$betas[,4,8]), # a_iris women, Asia
 
  
  # B_iris
  A_post_tog$B_b_iris[,1], # GLOBAL - WOMEN
  (A_post_tog$B_b_iris[,1] + A_post_tog$betas[,1,9]), # b_iris women, Cameroon 
  (A_post_tog$B_b_iris[,1] + A_post_tog$betas[,2,9]), # b_iris women, Colombia
  (A_post_tog$B_b_iris[,1] + A_post_tog$betas[,3,9]), # b_iris women, CZ 
  (A_post_tog$B_b_iris[,1] + A_post_tog$betas[,4,9]) # b_iris women, Asia
)


cols<-c("#00FBFF", 
        "#8C00FF",
        "#00FF33", 
        "#FFA600",
        "#00CCFF"
)

cols <- rep(cols,3)

cols<-paste(cols,"80",sep="") 

ys<- c(seq(from=1, to=5, by=1),seq(from=6.5, to=10.5, by=1),seq(from=12, to=16, by=1))
ys<-rev(ys)

labels <- c(  "L* Global",
              "L* Africa",
              "L* S.America",
              "L* Europe",
              "L* Asia",
              
              # A_iris
              "a* Global",
              "a* Africa",
              "a* S.America",
              "a* Europe",
              "a* Asia",

              # B_iris
              "b* Global",
              "b* Africa",
              "b* S.America",
              "b* Europe"
)



cex_t2 <- 1.05
cex_t <- 1.15
pls <- -0.9


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
  points(x=mean(toplot_CI),ycoord,lwd=2,pch=21,col=1,bg=0,cex=1.2)
  axis(2,at=ycoord,labels=textlab,srt=0,las=1, pos=-0.72, cex.axis=1.7, tick=F, lwd.ticks=0) 
  for (i in 1:15) {
    c(polygon(c(dens$x,rev(dens$x)),ys[i]*(c(dens$y,rev(dens$y)*(-1))*0.006)+ycoord[i],border=F, lwd = 1.5, col=colpol))
  }
  lines(x=PI(toplot_CI,prob=0.95),rep(ycoord+0.05,2),lwd=2, lty=1, col='#2f4363')
  segments(x0 = ci[1], y0 = ycoord-0.15, x1 = ci[1], y1 = ycoord+0.25, lwd = 3, col = '#2f4363')
  segments(x0 = ci[2], y0 = ycoord-0.15, x1 = ci[2], y1 = ycoord+0.25, lwd = 3, col = '#2f4363')
  points(x=mean(toplot_CI),ycoord,lwd=2,pch=21,col=1,bg=0,cex=1.2)
}


# 2 out 4: Sclera Colour - WOMEN


# SCLERA: 
par(mar=c(3.1, 0.5, 1.1, 0.5),mgp=c(5,0.7,0))

plot(NULL,xlim=c(-0.75,1),ylim=c(1,17.5),yaxt="n",bty="n", ylab="", xlab="", cex.axis=1.8, 
     main="PIT (Sclera) colour - Women", cex.main=1.8)


rect(-0.75, 0, 1, 17, col="#10101010", border=NA)

segments(-0.75, c(seq(from=1, to=5, by=1),seq(from=6.5, to=10.5, by=1),seq(from=12, to=16, by=1)), 1, 
         c(seq(from=1, to=5, by=1),seq(from=6.5, to=10.5, by=1),seq(from=12, to=16, by=1)), col="#40808080")

segments(-0.75, 5, 1, 5, col="#908080")
rect(-0.75, 4.5, 1, 5.5, col="#10101010", border=NA)

segments(-0.75, 10.5, 1, 10.5, col="#908080")
rect(-0.75, 10, 1, 11, col="#10101010", border=NA)

segments(-0.75, 16, 1, 16, col="#908080")
rect(-0.75, 15.5, 1, 16.5, col="#10101010", border=NA)


segments(c(-0.75,-0.5,-0.25,0.25,0.5,0.75),0,c(-0.75,-0.5,-0.25,0.25,0.5,0.75),16.5,col="#80808080",lty=2)
segments(0,0,0,16.5,col="#990909", lwd=2)


include <- list(
  # SCLERA L:
  A_post_tog$B_L_sclera[,1], # GLOBAL - WOMEN
  (A_post_tog$B_L_sclera[,1] + A_post_tog$betas[,1,10]), # L_sclera women, Cameroon 
  (A_post_tog$B_L_sclera[,1] + A_post_tog$betas[,2,10]), # L_sclera women, Colombia
  (A_post_tog$B_L_sclera[,1] + A_post_tog$betas[,3,10]), # L_sclera women, CZ 
  (A_post_tog$B_L_sclera[,1] + A_post_tog$betas[,4,10]), # L_sclera women, Asia

  
  # SCLERA A
  A_post_tog$B_a_sclera[,1], # GLOBAL - WOMEN
  (A_post_tog$B_a_sclera[,1] + A_post_tog$betas[,1,11]), # a_sclera women, Cameroon 
  (A_post_tog$B_a_sclera[,1] + A_post_tog$betas[,2,11]), # a_sclera women, Colombia
  (A_post_tog$B_a_sclera[,1] + A_post_tog$betas[,3,11]), # a_sclera women, CZ 
  (A_post_tog$B_a_sclera[,1] + A_post_tog$betas[,4,11]), # a_sclera women, Asia

  
  # SCLERA B
  A_post_tog$B_b_sclera[,1], # GLOBAL - WOMEN
  (A_post_tog$B_b_sclera[,1] + A_post_tog$betas[,1,12]), # b_sclera women, Cameroon 
  (A_post_tog$B_b_sclera[,1] + A_post_tog$betas[,2,12]), # b_sclera women, Colombia
  (A_post_tog$B_b_sclera[,1] + A_post_tog$betas[,3,12]), # b_sclera women, CZ 
  (A_post_tog$B_b_sclera[,1] + A_post_tog$betas[,4,12])  # b_sclera women, Asia
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
  points(x=mean(toplot_CI),ycoord,lwd=2,pch=21,col=1,bg=0,cex=1.2)
  for (i in 1:15) {
    c(polygon(c(dens$x,rev(dens$x)),ys[i]*(c(dens$y,rev(dens$y)*(-1))*0.006)+ycoord[i],border=F, lwd = 1.5, col=colpol))
  }
  lines(x=PI(toplot_CI,prob=0.95),rep(ycoord+0.05,2),lwd=2, lty=1, col='#2f4363')
  segments(x0 = ci[1], y0 = ycoord-0.15, x1 = ci[1], y1 = ycoord+0.25, lwd = 3, col = '#2f4363')
  segments(x0 = ci[2], y0 = ycoord-0.15, x1 = ci[2], y1 = ycoord+0.25, lwd = 3, col = '#2f4363')
  points(x=mean(toplot_CI),ycoord,lwd=2,pch=21,col=1,bg=0,cex=1.2)
}


# 3 out 4: Iris Colour - MEN


par(mar=c(3.1, 0.5, 1.1, 0.5),mgp=c(6,0.7,0))
plot(NULL,xlim=c(-0.75,1),ylim=c(1,17.5),yaxt="n",bty="n", ylab="", xlab="", cex.axis=1.8, 
     main="Iris Colour - Men", cex.main=1.8)


rect(-0.75, 0, 1, 17, col="#10101010", border=NA)

segments(-0.75, c(seq(from=1, to=5, by=1),seq(from=6.5, to=10.5, by=1),seq(from=12, to=16, by=1)), 1, 
         c(seq(from=1, to=5, by=1),seq(from=6.5, to=10.5, by=1),seq(from=12, to=16, by=1)), col="#40808080")

segments(-0.75, 5, 1, 5, col="#908080")
rect(-0.75, 4.5, 1, 5.5, col="#10101010", border=NA)

segments(-0.75, 10.5, 1, 10.5, col="#908080")
rect(-0.75, 10, 1, 11, col="#10101010", border=NA)

segments(-0.75, 16, 1, 16, col="#908080")
rect(-0.75, 15.5, 1, 16.5, col="#10101010", border=NA)


segments(c(-0.75,-0.5,-0.25,0.25,0.5,0.75),0,c(-0.75,-0.5,-0.25,0.25,0.5,0.75),16.5,col="#80808080",lty=2)
segments(0,0,0,16.5,col="#990909", lwd=2)


include <- list(
  # L_iris:
  A_post_tog$B_L_iris[,2], # GLOBAL - MEN
  (A_post_tog$B_L_iris[,2] + A_post_tog$betas[,1,19]), # L_iris men, Cameroon 
  (A_post_tog$B_L_iris[,2] + A_post_tog$betas[,2,19]), # L_iris men, Colombia
  (A_post_tog$B_L_iris[,2] + A_post_tog$betas[,3,19]), # L_iris men, CZ 
  (A_post_tog$B_L_iris[,2] + A_post_tog$betas[,4,19]), # L_iris men, Asia

  
  # A_iris
  A_post_tog$B_a_iris[,2], # GLOBAL - WOMEN
  (A_post_tog$B_a_iris[,2] + A_post_tog$betas[,1,20]), # a_iris men, Cameroon 
  (A_post_tog$B_a_iris[,2] + A_post_tog$betas[,2,20]), # a_iris men, Colombia
  (A_post_tog$B_a_iris[,2] + A_post_tog$betas[,3,20]), # a_iris men, CZ 
  (A_post_tog$B_a_iris[,2] + A_post_tog$betas[,4,20]), # a_iris men, Asia
  
  # B_iris
  A_post_tog$B_b_iris[,2], # GLOBAL - WOMEN
  (A_post_tog$B_b_iris[,2] + A_post_tog$betas[,1,21]), # b_iris men, Cameroon 
  (A_post_tog$B_b_iris[,2] + A_post_tog$betas[,2,21]), # b_iris men, Colombia
  (A_post_tog$B_b_iris[,2] + A_post_tog$betas[,3,21]), # b_iris men, CZ 
  (A_post_tog$B_b_iris[,2] + A_post_tog$betas[,4,21])  # b_iris men, Asia
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
  points(x=mean(toplot_CI),ycoord,lwd=2,pch=21,col=1,bg=0,cex=1.2)
  for (i in 1:15) {
    c(polygon(c(dens$x,rev(dens$x)),ys[i]*(c(dens$y,rev(dens$y)*(-1))*0.006)+ycoord[i],border=F, lwd = 1.5, col=colpol))
  }
  lines(x=PI(toplot_CI,prob=0.95),rep(ycoord+0.05,2),lwd=2, lty=1, col='#2f4363')
  segments(x0 = ci[1], y0 = ycoord-0.15, x1 = ci[1], y1 = ycoord+0.25, lwd = 3, col = '#2f4363')
  segments(x0 = ci[2], y0 = ycoord-0.15, x1 = ci[2], y1 = ycoord+0.25, lwd = 3, col = '#2f4363')
  points(x=mean(toplot_CI),ycoord,lwd=2,pch=21,col=1,bg=0,cex=1.2)
}


# 4 out 4: Sclera Colour - MEN


par(mar=c(3.1, 0.5, 1.1, 0.5),mgp=c(5,0.7,0))
plot(NULL,xlim=c(-0.75,1),ylim=c(1,17.5),yaxt="n",bty="n", ylab="", xlab="", cex.axis=1.8, 
     main="PIT (Sclera) Colour -  Men", cex.main=1.8)


rect(-0.75, 0, 1, 17, col="#10101010", border=NA)

segments(-0.75, c(seq(from=1, to=5, by=1),seq(from=6.5, to=10.5, by=1),seq(from=12, to=16, by=1)), 1, 
         c(seq(from=1, to=5, by=1),seq(from=6.5, to=10.5, by=1),seq(from=12, to=16, by=1)), col="#40808080")

segments(-0.75, 5, 1, 5, col="#908080")
rect(-0.75, 4.5, 1, 5.5, col="#10101010", border=NA)

segments(-0.75, 10.5, 1, 10.5, col="#908080")
rect(-0.75, 10, 1, 11, col="#10101010", border=NA)

segments(-0.75, 16, 1, 16, col="#908080")
rect(-0.75, 15.5, 1, 16.5, col="#10101010", border=NA)


segments(c(-0.75,-0.5,-0.25,0.25,0.5,0.75),0,c(-0.75,-0.5,-0.25,0.25,0.5,0.75),16.5,col="#80808080",lty=2)
segments(0,0,0,16.5,col="#990909", lwd=2)



include <- list(
  # L_Sclera:
  A_post_tog$B_L_sclera[,2], # GLOBAL - MEN
  (A_post_tog$B_L_sclera[,2] + A_post_tog$betas[,1,22]), # L_sclera men, Cameroon 
  (A_post_tog$B_L_sclera[,2] + A_post_tog$betas[,2,22]), # L_sclera men, Colombia
  (A_post_tog$B_L_sclera[,2] + A_post_tog$betas[,3,22]), # L_sclera men, CZ 
  (A_post_tog$B_L_sclera[,2] + A_post_tog$betas[,4,22]), # L_sclera men, Asia
  
  # A_Sclera
  A_post_tog$B_a_sclera[,2], # GLOBAL - MEN
  (A_post_tog$B_a_sclera[,2] + A_post_tog$betas[,1,23]), # a_sclera men, Cameroon 
  (A_post_tog$B_a_sclera[,2] + A_post_tog$betas[,2,23]), # a_sclera men, Colombia
  (A_post_tog$B_a_sclera[,2] + A_post_tog$betas[,3,23]), # a_sclera men, CZ 
  (A_post_tog$B_a_sclera[,2] + A_post_tog$betas[,4,23]), # a_sclera men, Asia
  
  # B_Sclera
  A_post_tog$B_b_sclera[,2], # GLOBAL - MEN
  (A_post_tog$B_b_sclera[,2] + A_post_tog$betas[,1,24]), # b_sclera men, Cameroon 
  (A_post_tog$B_b_sclera[,2] + A_post_tog$betas[,2,24]), # b_sclera men, Colombia
  (A_post_tog$B_b_sclera[,2] + A_post_tog$betas[,3,24]), # b_sclera men, CZ
  (A_post_tog$B_b_sclera[,2] + A_post_tog$betas[,4,24]) # b_sclera men, Asia
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
  points(x=mean(toplot_CI),ycoord,lwd=2,pch=21,col=1,bg=0,cex=1.2)
  for (i in 1:15) {
    c(polygon(c(dens$x,rev(dens$x)),ys[i]*(c(dens$y,rev(dens$y)*(-1))*0.006)+ycoord[i],border=F, lwd = 1.5, col=colpol))
  }
  lines(x=PI(toplot_CI,prob=0.95),rep(ycoord+0.05,2),lwd=2, lty=1, col='#2f4363')
  segments(x0 = ci[1], y0 = ycoord-0.15, x1 = ci[1], y1 = ycoord+0.25, lwd = 3, col = '#2f4363')
  segments(x0 = ci[2], y0 = ycoord-0.15, x1 = ci[2], y1 = ycoord+0.25, lwd = 3, col = '#2f4363')
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

# betas[,8,13] -> Intercept men, Turkey

# No Vietnam, but the rest is the same.

tiff("Posterior__SEXT_8_groups_TOGETHER_STANDARDISED.tif",width=32,height=16,units="cm",res=600,compression = "lzw")
layout(matrix(1:4,byrow=T,nrow=1), widths=c(2.9,2,2,2),heights=1)

par(mar=c(5.1, 10.1, 1.1, 1.1),mgp=c(5,0.7,0))
plot(NULL,xlim=c(-0.75,1),ylim=c(1,17.5),yaxt="n",bty="n", ylab="", cex.axis=1.8, 
     main="Iris Colour - Women", cex.main=1.8)

rect(-0.75, 0, 1, 17, col="#10101010", border=NA)

segments(-0.75, c(seq(from=1, to=5, by=1),seq(from=6.5, to=10.5, by=1),seq(from=12, to=16, by=1)), 1, 
         c(seq(from=1, to=5, by=1),seq(from=6.5, to=10.5, by=1),seq(from=12, to=16, by=1)), col="#40808080")

segments(-0.75, 5, 1, 5, col="#908080")
rect(-0.75, 4.5, 1, 5.5, col="#10101010", border=NA)

segments(-0.75, 10.5, 1, 10.5, col="#908080")
rect(-0.75, 10, 1, 11, col="#10101010", border=NA)

segments(-0.75, 16, 1, 16, col="#908080")
rect(-0.75, 15.5, 1, 16.5, col="#10101010", border=NA)


segments(c(-0.75,-0.5,-0.25,0.25,0.5,0.75),0,c(-0.75,-0.5,-0.25,0.25,0.5,0.75),16.5,col="#80808080",lty=2)
segments(0,0,0,16.5,col="#990909", lwd=2)


include <- list(
  # L_iris:
  S_post_tog$B_L_iris[,1], # GLOBAL - WOMEN
  (S_post_tog$B_L_iris[,1] + S_post_tog$betas[,1,7]), # L_iris women, Cameroon 
  (S_post_tog$B_L_iris[,1] + S_post_tog$betas[,2,7]), # L_iris women, Colombia
  (S_post_tog$B_L_iris[,1] + S_post_tog$betas[,3,7]), # L_iris women, CZ 
  (S_post_tog$B_L_iris[,1] + S_post_tog$betas[,4,7]), # L_iris women, Asia
  
  # A_iris
  S_post_tog$B_a_iris[,1], # GLOBAL - WOMEN
  (S_post_tog$B_a_iris[,1] + S_post_tog$betas[,1,8]), # a_iris women, Cameroon 
  (S_post_tog$B_a_iris[,1] + S_post_tog$betas[,2,8]), # a_iris women, Colombia
  (S_post_tog$B_a_iris[,1] + S_post_tog$betas[,3,8]), # a_iris women, CZ 
  (S_post_tog$B_a_iris[,1] + S_post_tog$betas[,4,8]), # a_iris women, Asia
  
  # B_iris
  S_post_tog$B_b_iris[,1], # GLOBAL - WOMEN
  (S_post_tog$B_b_iris[,1] + S_post_tog$betas[,1,9]), # b_iris women, Cameroon 
  (S_post_tog$B_b_iris[,1] + S_post_tog$betas[,2,9]), # b_iris women, Colombia
  (S_post_tog$B_b_iris[,1] + S_post_tog$betas[,3,9]), # b_iris women, CZ 
  (S_post_tog$B_b_iris[,1] + S_post_tog$betas[,4,9]) # b_iris women, Asia
)


cols # cols can stay

ys<- c(seq(from=1, to=5, by=1),seq(from=6.5, to=10.5, by=1),seq(from=12, to=16, by=1))
ys<-rev(ys)

labels # labels can stay


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
  points(x=mean(toplot_CI),ycoord,lwd=2,pch=21,col=1,bg=0,cex=1.2)
  axis(2,at=ycoord,labels=textlab,srt=0,las=1, cex.axis=1.7, pos=-0.72, tick=F, lwd.ticks=0) 
  for (i in 1:15) {
    c(polygon(c(dens$x,rev(dens$x)),ys[i]*(c(dens$y,rev(dens$y)*(-1))*0.006)+ycoord[i],border=F, lwd = 1.5, col=colpol))
  }
  lines(x=PI(toplot_CI,prob=0.95),rep(ycoord+0.05,2),lwd=2, lty=1, col='#2f4363')
  segments(x0 = ci[1], y0 = ycoord-0.15, x1 = ci[1], y1 = ycoord+0.25, lwd = 3, col = '#2f4363')
  segments(x0 = ci[2], y0 = ycoord-0.15, x1 = ci[2], y1 = ycoord+0.25, lwd = 3, col = '#2f4363')
  points(x=mean(toplot_CI),ycoord,lwd=2,pch=21,col=1,bg=0,cex=1.2)
}


# SCLERA: 
par(mar=c(5.1, 1.1, 1.1, 1.1),mgp=c(5,0.7,0))
plot(NULL,xlim=c(-0.7,1),ylim=c(1,17.5),yaxt="n",bty="n", ylab="", cex.axis=1.8, 
     main="PIT (Sclera) Colour - Women", cex.main=1.8)

rect(-0.75, 0, 1, 17, col="#10101010", border=NA)

segments(-0.75, c(seq(from=1, to=5, by=1),seq(from=6.5, to=10.5, by=1),seq(from=12, to=16, by=1)), 1, 
         c(seq(from=1, to=5, by=1),seq(from=6.5, to=10.5, by=1),seq(from=12, to=16, by=1)), col="#40808080")

segments(-0.75, 5, 1, 5, col="#908080")
rect(-0.75, 4.5, 1, 5.5, col="#10101010", border=NA)

segments(-0.75, 10.5, 1, 10.5, col="#908080")
rect(-0.75, 10, 1, 11, col="#10101010", border=NA)

segments(-0.75, 16, 1, 16, col="#908080")
rect(-0.75, 15.5, 1, 16.5, col="#10101010", border=NA)


segments(c(-0.75,-0.5,-0.25,0.25,0.5,0.75),0,c(-0.75,-0.5,-0.25,0.25,0.5,0.75),16.5,col="#80808080",lty=2)
segments(0,0,0,16.5,col="#990909", lwd=2)


include <- list(
  # SCLERA L:
  S_post_tog$B_L_sclera[,1], # GLOBAL - WOMEN
  (S_post_tog$B_L_sclera[,1] + S_post_tog$betas[,1,10]), # L_sclera women, Cameroon 
  (S_post_tog$B_L_sclera[,1] + S_post_tog$betas[,2,10]), # L_sclera women, Colombia
  (S_post_tog$B_L_sclera[,1] + S_post_tog$betas[,3,10]), # L_sclera women, CZ 
  (S_post_tog$B_L_sclera[,1] + S_post_tog$betas[,4,10]), # L_sclera women, Asia
  
  # SCLERA A
  S_post_tog$B_a_sclera[,1], # GLOBAL - WOMEN
  (S_post_tog$B_a_sclera[,1] + S_post_tog$betas[,1,11]), # a_sclera women, Cameroon
  (S_post_tog$B_a_sclera[,1] + S_post_tog$betas[,2,11]), # a_sclera women, Colombia
  (S_post_tog$B_a_sclera[,1] + S_post_tog$betas[,3,11]), # a_sclera women, CZ 
  (S_post_tog$B_a_sclera[,1] + S_post_tog$betas[,4,11]), # a_sclera women, Asia
  
  # SCLERA B
  S_post_tog$B_b_sclera[,1], # GLOBAL - WOMEN
  (S_post_tog$B_b_sclera[,1] + S_post_tog$betas[,1,12]), # b_sclera women, Cameroon 
  (S_post_tog$B_b_sclera[,1] + S_post_tog$betas[,2,12]), # b_sclera women, Colombia
  (S_post_tog$B_b_sclera[,1] + S_post_tog$betas[,3,12]), # b_sclera women, CZ 
  (S_post_tog$B_b_sclera[,1] + S_post_tog$betas[,4,12]) # b_sclera women, Asia
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
  for (i in 1:15) {
    c(polygon(c(dens$x,rev(dens$x)),ys[i]*(c(dens$y,rev(dens$y)*(-1))*0.006)+ycoord[i],border=F, lwd = 1.5, col=colpol))
  }
  lines(x=PI(toplot_CI,prob=0.95),rep(ycoord+0.05,2),lwd=2, lty=1, col='#2f4363')
  segments(x0 = ci[1], y0 = ycoord-0.15, x1 = ci[1], y1 = ycoord+0.25, lwd = 3, col = '#2f4363')
  segments(x0 = ci[2], y0 = ycoord-0.15, x1 = ci[2], y1 = ycoord+0.25, lwd = 3, col = '#2f4363')
  points(x=mean(toplot_CI),ycoord,lwd=2,pch=21,col=1,bg=0,cex=1.2)
}


# MEN: 

par(mar=c(5.1, 1.1, 1.1, 1.1),mgp=c(5,0.7,0))
plot(NULL,xlim=c(-0.7,1),ylim=c(1,17.5),yaxt="n",bty="n", ylab="", cex.axis=1.8, 
     main="Iris Colour - Men", cex.main=1.8)


rect(-0.75, 0, 1, 17, col="#10101010", border=NA)

segments(-0.75, c(seq(from=1, to=5, by=1),seq(from=6.5, to=10.5, by=1),seq(from=12, to=16, by=1)), 1, 
         c(seq(from=1, to=5, by=1),seq(from=6.5, to=10.5, by=1),seq(from=12, to=16, by=1)), col="#40808080")

segments(-0.75, 5, 1, 5, col="#908080")
rect(-0.75, 4.5, 1, 5.5, col="#10101010", border=NA)

segments(-0.75, 10.5, 1, 10.5, col="#908080")
rect(-0.75, 10, 1, 11, col="#10101010", border=NA)

segments(-0.75, 16, 1, 16, col="#908080")
rect(-0.75, 15.5, 1, 16.5, col="#10101010", border=NA)


segments(c(-0.75,-0.5,-0.25,0.25,0.5,0.75),0,c(-0.75,-0.5,-0.25,0.25,0.5,0.75),16.5,col="#80808080",lty=2)
segments(0,0,0,16.5,col="#990909", lwd=2)


include <- list(
  # L_iris:
  S_post_tog$B_L_iris[,2], # GLOBAL - MEN
  (S_post_tog$B_L_iris[,2] + S_post_tog$betas[,1,19]), # L_iris men, Cameroon 
  (S_post_tog$B_L_iris[,2] + S_post_tog$betas[,2,19]), # L_iris men, Colombia
  (S_post_tog$B_L_iris[,2] + S_post_tog$betas[,3,19]), # L_iris men, CZ 
  (S_post_tog$B_L_iris[,2] + S_post_tog$betas[,4,19]), # L_iris men, Asia
  
  # A_iris
  S_post_tog$B_a_iris[,2], # GLOBAL - WOMEN
  (S_post_tog$B_a_iris[,2] + S_post_tog$betas[,1,20]), # a_iris men, Cameroon
  (S_post_tog$B_a_iris[,2] + S_post_tog$betas[,2,20]), # a_iris men, Colombia
  (S_post_tog$B_a_iris[,2] + S_post_tog$betas[,3,20]), # a_iris men, CZ
  (S_post_tog$B_a_iris[,2] + S_post_tog$betas[,4,20]), # a_iris men, Asia
  
  # B_iris
  S_post_tog$B_b_iris[,2], # GLOBAL - WOMEN
  (S_post_tog$B_b_iris[,2] + S_post_tog$betas[,1,21]), # b_iris men, Cameroon 
  (S_post_tog$B_b_iris[,2] + S_post_tog$betas[,2,21]), # b_iris men, Colombia
  (S_post_tog$B_b_iris[,2] + S_post_tog$betas[,3,21]), # b_iris men, CZ 
  (S_post_tog$B_b_iris[,2] + S_post_tog$betas[,4,21]) # b_iris men, Asia
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
  for (i in 1:15) {
    c(polygon(c(dens$x,rev(dens$x)),ys[i]*(c(dens$y,rev(dens$y)*(-1))*0.006)+ycoord[i],border=F, lwd = 1.5, col=colpol))
  }
  lines(x=PI(toplot_CI,prob=0.95),rep(ycoord+0.05,2),lwd=2, lty=1, col='#2f4363')
  segments(x0 = ci[1], y0 = ycoord-0.15, x1 = ci[1], y1 = ycoord+0.25, lwd = 3, col = '#2f4363')
  segments(x0 = ci[2], y0 = ycoord-0.15, x1 = ci[2], y1 = ycoord+0.25, lwd = 3, col = '#2f4363')
  points(x=mean(toplot_CI),ycoord,lwd=2,pch=21,col=1,bg=0,cex=1.2)
}


# SCLERA_

par(mar=c(5.1, 1.1, 1.1, 1.1),mgp=c(5,0.7,0))
plot(NULL,xlim=c(-0.7,1),ylim=c(1,17.5),yaxt="n",bty="n", ylab="", cex.axis=1.8, 
     main="Sclera Colour: MEN", cex.main=1.8)


rect(-0.75, 0, 1, 17, col="#10101010", border=NA)

segments(-0.75, c(seq(from=1, to=5, by=1),seq(from=6.5, to=10.5, by=1),seq(from=12, to=16, by=1)), 1, 
         c(seq(from=1, to=5, by=1),seq(from=6.5, to=10.5, by=1),seq(from=12, to=16, by=1)), col="#40808080")

segments(-0.75, 5, 1, 5, col="#908080")
rect(-0.75, 4.5, 1, 5.5, col="#10101010", border=NA)

segments(-0.75, 10.5, 1, 10.5, col="#908080")
rect(-0.75, 10, 1, 11, col="#10101010", border=NA)

segments(-0.75, 16, 1, 16, col="#908080")
rect(-0.75, 15.5, 1, 16.5, col="#10101010", border=NA)


segments(c(-0.75,-0.5,-0.25,0.25,0.5,0.75),0,c(-0.75,-0.5,-0.25,0.25,0.5,0.75),16.5,col="#80808080",lty=2)
segments(0,0,0,16.5,col="#990909", lwd=2)


include <- list(
  # L_Sclera:
  S_post_tog$B_L_sclera[,2], # GLOBAL - MEN
  (S_post_tog$B_L_sclera[,2] + S_post_tog$betas[,1,22]), # L_sclera men, Cameroon 
  (S_post_tog$B_L_sclera[,2] + S_post_tog$betas[,2,22]), # L_sclera men, Colombia
  (S_post_tog$B_L_sclera[,2] + S_post_tog$betas[,3,22]), # L_Sclera men, CZ 
  (S_post_tog$B_L_sclera[,2] + S_post_tog$betas[,4,22]), # L_sclera men, Asia
  
  # A_Sclera
  S_post_tog$B_a_sclera[,2], # GLOBAL - MEN
  (S_post_tog$B_a_sclera[,2] + S_post_tog$betas[,1,23]), # a_sclera men, Cameroon 
  (S_post_tog$B_a_sclera[,2] + S_post_tog$betas[,2,23]), # a_sclera men, Colombia
  (S_post_tog$B_a_sclera[,2] + S_post_tog$betas[,3,23]), # a_sclera men, CZ 
  (S_post_tog$B_a_sclera[,2] + S_post_tog$betas[,4,23]), # a_sclera men, Asia
  
  # B_Sclera
  S_post_tog$B_b_sclera[,2], # GLOBAL - MEN
  (S_post_tog$B_b_sclera[,2] + S_post_tog$betas[,1,24]), # b_sclera men, Cameroon 
  (S_post_tog$B_b_sclera[,2] + S_post_tog$betas[,2,24]), # b_sclera men, Colombia
  (S_post_tog$B_b_sclera[,2] + S_post_tog$betas[,3,24]), # b_sclera men, CZ 
  (S_post_tog$B_b_sclera[,2] + S_post_tog$betas[,4,24]) # b_sclera men, Asia
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
  points(x=mean(toplot_CI),ycoord,lwd=2,pch=21,col=1,bg=0,cex=1.2)
  for (i in 1:15) {
    c(polygon(c(dens$x,rev(dens$x)),ys[i]*(c(dens$y,rev(dens$y)*(-1))*0.006)+ycoord[i],border=F, lwd = 1.5, col=colpol))
  }
  lines(x=PI(toplot_CI,prob=0.95),rep(ycoord+0.05,2),lwd=2, lty=1, col='#2f4363')
  segments(x0 = ci[1], y0 = ycoord-0.15, x1 = ci[1], y1 = ycoord+0.25, lwd = 3, col = '#2f4363')
  segments(x0 = ci[2], y0 = ycoord-0.15, x1 = ci[2], y1 = ycoord+0.25, lwd = 3, col = '#2f4363')
  points(x=mean(toplot_CI),ycoord,lwd=2,pch=21,col=1,bg=0,cex=1.2)
}

dev.off()



