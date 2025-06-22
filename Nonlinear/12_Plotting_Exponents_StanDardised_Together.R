
# Counterfactual plots...

#.-.-.-.-.-.-.-.-.-.
# Type One: Attractiveness & Standardised together
#.-.-.-.-.-.-.-.-.-.

#                                WAIC    SE dWAIC   dSE pWAIC weight
# m_1_4_only_varying_attr_sex2 2355.3 53.74   0.0    NA  77.7   0.53
# m_1_3_only_varying_attr      2356.6 53.78   1.4  2.84  76.7   0.27
# m_1_6_FREE_MF_attr           2359.0 53.46   3.7  7.07  95.9   0.08 
# m_1_attr                     2359.0 53.91   3.7  7.16  96.3   0.08
# m_1_5_FREE_and_LINN_attr     2360.3 53.43   5.1  6.95  95.7   0.04
# m_1_2_SQ7_attr               2379.6 53.61  24.3 12.76 123.3   0.00
# !! This will probably vary widely, but M_2 (m_1_2_SQ7_attr will stay as the last one)

# Load the posteriors: 

library(rethinking)

load("model_nonlinear_attr_21_06_25_1.Rdata")
post1 <- extract.samples(m_1_attr)
str(post1)

load("model_nonlinear_attr_21_06_25_2.Rdata")
post2 <- extract.samples(m_1_2_SQ7_attr)
str(post2)

load("model_nonlinear_attr_21_06_25_3.Rdata")
post3 <- extract.samples(m_1_3_only_varying_attr)
str(post3)

load("model_nonlinear_attr_21_06_25_4.Rdata")
post4 <- extract.samples(m_1_4_only_varying_attr_sex2)
str(post4)

load("model_nonlinear_attr_21_06_25_5.Rdata")
post5 <- extract.samples(m_1_5_FREE_and_LINN_attr)
str(post5)

load("model_nonlinear_attr_21_06_25_6.Rdata")
post6 <- extract.samples(m_1_6_FREE_MF_attr)
str(post6)

# Load the data: 
Eye <- read.csv2("Data_03_08_24.csv",T) 

# Set up the variable for scaling the data (only L* Sclera)...
stan_L_sclera <- function(x){(x-mean(Eye$L_sclera))/sd(Eye$L_sclera)}
unstan_L_sclera <- function(x){x*sd(Eye$L_sclera)+mean(Eye$L_sclera)}

stan_attr <- function(x){(x-mean(Eye$Attr))/sd(Eye$Attr)}
unstan_attr <- function(x){x*sd(Eye$Attr)+mean(Eye$Attr)}


# Process the importat part of the data: 
L_sclera <- data.frame(L_sclera_crude = Eye$L_sclera,
                       Attr = stan_attr(Eye$Attr),
                       L_sclera_stan = stan_L_sclera(Eye$L_sclera),
                       Culture = as.numeric(as.factor(Eye$Culture_Year)),
                       L_sclera_nz = stan_L_sclera(Eye$L_sclera)-min(stan_L_sclera(Eye$L_sclera))+0.001,
                       Sex = as.numeric(as.factor(Eye$Sex))
)

table(L_sclera$Culture,Eye$Culture_Year)

#     Cameroon_2013 Cameroon_2016 Colombia Czech_2016 Czech_2019 India_FCD Iran Turkey Vietnam
#   1            99             0        0          0          0         0    0      0       0
#   2             0            98        0          0          0         0    0      0       0
#   3             0             0      138          0          0         0    0      0       0
#   4             0             0        0        100          0         0    0      0       0
#   5             0             0        0          0         95         0    0      0       0
#   6             0             0        0          0          0       142    0      0       0
#   7             0             0        0          0          0         0   87      0       0
#   8             0             0        0          0          0         0    0    184       0
#   9             0             0        0          0          0         0    0      0      91

# Plotting free exponents estimates: 

# M3 (sexes not divided)
include <- list(
  post3$E_iris_L,
  post3$E_iris_a,
  post3$E_iris_b,
  post3$E_sclera_L,
  post3$E_sclera_a,
  post3$E_sclera_b,
  
  # 4 - ženy
  post4$E_iris_L[,1],
  post4$E_iris_a[,1],
  post4$E_iris_b[,1],
  post4$E_sclera_L[,1],
  post4$E_sclera_a[,1],
  post4$E_sclera_b[,1],
  # 4 - muži
  post4$E_iris_L[,2],
  post4$E_iris_a[,2],
  post4$E_iris_b[,2],
  post4$E_sclera_L[,2],
  post4$E_sclera_a[,2],
  post4$E_sclera_b[,2],
  
  post5$E_iris_L,
  post5$E_iris_a,
  post5$E_iris_b,
  post5$E_sclera_L,
  post5$E_sclera_a,
  post5$E_sclera_b,
  
  # 6 - ženy
  post6$E_iris_L[,1],
  post6$E_iris_a[,1],
  post6$E_iris_b[,1],
  post6$E_sclera_L[,1],
  post6$E_sclera_a[,1],
  post6$E_sclera_b[,1],
  
  # 6 - muži
  post6$E_iris_L[,2],
  post6$E_iris_a[,2],
  post6$E_iris_b[,2],
  post6$E_sclera_L[,2],
  post6$E_sclera_a[,2],
  post6$E_sclera_b[,2]
)


labels <- c("M3 exponent iris L* [MF]", 
            "M3 exponent iris a* [MF]",
            "M3 exponent iris b* [MF]",
            "M3 exponent PIT L* [MF]",
            "M3 exponent PIT a* [MF]",
            "M3 exponent PIT b* [MF]",
            
            # 4 - ženy
            "M4 exponent iris L* [F]",
            "M4 exponent iris a* [F]",
            "M4 exponent iris b* [F]",
            "M4 exponent PIT L* [F]",
            "M4 exponent PIT a* [F]",
            "M4 exponent PIT b* [F]",
            
            # 4 - muži
            "M4 exponent iris L* [M]",
            "M4 exponent iris a* [M]",
            "M4 exponent iris b* [M]",
            "M4 exponent PIT L* [M]",
            "M4 exponent PIT a* [M]",
            "M4 exponent PIT b* [M]",
            
            # M5
            "M5 exponent iris L* [MF]", 
            "M5 exponent iris a* [MF]",
            "M5 exponent iris b* [MF]",
            "M5 exponent PIT L* [MF]",
            "M5 exponent PIT a* [MF]",
            "M5 exponent PIT b* [MF]",
            
            # M6
            "M6 exponent iris L* [F]",
            "M6 exponent iris a* [F]",
            "M6 exponent iris b* [F]",
            "M6 exponent PIT L* [F]",
            "M6 exponent PIT a* [F]",
            "M6 exponent PIT b* [F]",
            
            # M6
            "M6 exponent iris L* [M]",
            "M6 exponent iris a* [M]",
            "M6 exponent iris b* [M]",
            "M6 exponent PIT L* [M]",
            "M6 exponent PIT a* [M]",
            "M6 exponent PIT b* [M]"
)


tiff("Predictions_attractiveness_nonlinear_slopes_ATTR.tif",width=30,height=38,units="cm",res=600,compression = "lzw")

par(mar=c(5.1, 11.1, 1.1, 1.1),mgp=c(1,0.7,0))
plot(NULL,xlim=c(-0.5,2),ylim=c(1,36.5),yaxt="n",bty="n", ylab="", xlab="Posterior dist.", cex.axis=0.7, 
     main="Attractiveness, Scaled Together")

segments(-0.25, c(seq(from=1, to=36, by=1)),2, col="#40808080")

segments(-0.25,33,2, col="#40404040", lwd=18)
segments(-0.25,27,2, col="#40404040", lwd=18)
segments(-0.25,21,2, col="#40404040", lwd=18)

ys<- c(seq(from=1, to=36, by=1))
ys<-rev(ys)

pls <- -1.1
cex_t2 <- 1

cols<-c("#00FBFF", 
        "#8C00FF",
        "#00FF33", 
        "#FFEEBB",
        "#FFA600",
        "#AAEEFF"
)

cols <- rep(cols,6)

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
  #lines(x=PI(toplot,prob=0.89),rep(ycoord-0.05,2),lwd=1.5, lty=5)
  points(x=mean(toplot_CI),ycoord,lwd=2,pch=21,col=1,bg=0,cex=1.2)
  axis(2,at=ycoord,labels=textlab,srt=0,las=1, cex.axis=1, pos=-0.4, tick=F) 
  
  for (i in 1:36) {
    c(polygon(c(dens$x,rev(dens$x)),ys[i]*(c(dens$y,rev(dens$y)*(-1))*0.003)+ycoord[i],border=F, lwd = 1.5, col=colpol))
  }
  lines(x=PI(toplot_CI,prob=0.95),rep(ycoord+0.05,2),lwd=2, lty=1, col='#2f4363')
  segments(x0 = ci[1], y0 = ycoord-0.15, x1 = ci[1], y1 = ycoord+0.25, lwd = 3, col = '#2f4363')
  segments(x0 = ci[2], y0 = ycoord-0.15, x1 = ci[2], y1 = ycoord+0.25, lwd = 3, col = '#2f4363')
  points(x=mean(toplot_CI),ycoord,lwd=2,pch=21,col=1,bg=0,cex=1.2)
}

abline(v=0, lwd=3)
abline(v=1, lwd=3)


dev.off()



#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-
#
# Type 2: Sextypicality - Standardised Together
#
#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-

setwd("D:/GACR_VF/Measured_Eye_Phenotype_And_Ascribed_Characteristics_In_Different_Countries/Final Figures Draft Data/Nelinear_Attempts/Standardised_Together_Sextypicality")

#                                 WAIC     SE  dWAIC    dSE  pWAIC weight
#  m_1_sext                     2037.1  53.94    0.0     NA   93.5    0.6
#  m_1_4_only_varying_sext_sex2 2037.9  52.40    0.8   9.38   80.3    0.4
#  m_1_3_only_varying_sext      2056.0  52.00   18.9   9.77   80.2    0.0
#  m_1_2_SQ7_sext               2061.1  53.86   24.0   7.18  114.2    0.0
#  m_1_6_FREE_QUAD_SEX_sext     3055.6  72.15 1018.5  57.78  505.9    0.0
#  m_1_5_FREE_QUAD_sext         5887.7 147.38 3850.6 150.75 1777.1    0.0


# Load the posterior: 
# Load the posteriors: 

load("model_nonlinear_sext_10_06_25_1.Rdata")
post1 <- extract.samples(m_1_sext)
str(post1)

load("model_nonlinear_sext_10_06_25_2.Rdata")
post2 <- extract.samples(m_1_2_SQ7_sext)
str(post2)

load("model_nonlinear_sext_10_06_25_3.Rdata")
post3 <- extract.samples(m_1_3_only_varying_sext)
str(post3)

load("model_nonlinear_sext_10_06_25_4.Rdata")
post4 <- extract.samples(m_1_4_only_varying_sext_sex2)
str(post4)

load("model_nonlinear_sext_10_06_25_5.Rdata")
post5 <- extract.samples(m_1_5_FREE_and_LINN_sext)
str(post5)

load("model_nonlinear_sext_10_06_25_6.Rdata")
post6 <- extract.samples(m_1_6_FREE_MF_sext)
str(post6)


# Load the data: 
Eye <- read.csv2("Data_03_08_24.csv",T) 
Eye <- Eye[!is.na(Eye$SexT),]

# Set up the variable for scaling the data (only L* Sclera)...
stan_L_sclera <- function(x){(x-mean(Eye$L_sclera))/sd(Eye$L_sclera)}
unstan_L_sclera <- function(x){x*sd(Eye$L_sclera)+mean(Eye$L_sclera)}

stan_sext <- function(x){(x-mean(Eye$SexT))/sd(Eye$SexT)}
unstan_sext <- function(x){x*sd(Eye$SexT)+mean(Eye$SexT)}


# Process the importat part of the data: 
L_sclera <- data.frame(L_sclera_crude = Eye$L_sclera,
                       SexT = stan_sext(Eye$SexT),
                       L_sclera_stan = stan_L_sclera(Eye$L_sclera),
                       L_sclera_nz = stan_L_sclera(Eye$L_sclera)-min(stan_L_sclera(Eye$L_sclera))+0.001,
                       Culture = as.numeric(as.factor(Eye$Culture_Year)),
                       Sex = as.numeric(as.factor(Eye$Sex))
)

table(L_sclera$Culture,Eye$Culture_Year) # Should be without Vietnamese sample - which has not been rated for SexTypicality...



# M3 (sexes not divided)
include <- list(
  post3$E_iris_L,
  post3$E_iris_a,
  post3$E_iris_b,
  post3$E_sclera_L,
  post3$E_sclera_a,
  post3$E_sclera_b,
  
  # 4 - ženy
  post4$E_iris_L[,1],
  post4$E_iris_a[,1],
  post4$E_iris_b[,1],
  post4$E_sclera_L[,1],
  post4$E_sclera_a[,1],
  post4$E_sclera_b[,1],
  # 4 - muži
  post4$E_iris_L[,2],
  post4$E_iris_a[,2],
  post4$E_iris_b[,2],
  post4$E_sclera_L[,2],
  post4$E_sclera_a[,2],
  post4$E_sclera_b[,2],
  
  post5$E_iris_L,
  post5$E_iris_a,
  post5$E_iris_b,
  post5$E_sclera_L,
  post5$E_sclera_a,
  post5$E_sclera_b,
  
  # 6 - ženy
  post6$E_iris_L[,1],
  post6$E_iris_a[,1],
  post6$E_iris_b[,1],
  post6$E_sclera_L[,1],
  post6$E_sclera_a[,1],
  post6$E_sclera_b[,1],
  
  # 6 - muži
  post6$E_iris_L[,2],
  post6$E_iris_a[,2],
  post6$E_iris_b[,2],
  post6$E_sclera_L[,2],
  post6$E_sclera_a[,2],
  post6$E_sclera_b[,2]
)


labels <- c("M3 exponent iris L* [MF]", 
            "M3 exponent iris a* [MF]",
            "M3 exponent iris b* [MF]",
            "M3 exponent PIT L* [MF]",
            "M3 exponent PIT a* [MF]",
            "M3 exponent PIT b* [MF]",
            
            # 4 - ženy
            "M4 exponent iris L* [F]",
            "M4 exponent iris a* [F]",
            "M4 exponent iris b* [F]",
            "M4 exponent PIT L* [F]",
            "M4 exponent PIT a* [F]",
            "M4 exponent PIT b* [F]",
            
            # 4 - muži
            "M4 exponent iris L* [M]",
            "M4 exponent iris a* [M]",
            "M4 exponent iris b* [M]",
            "M4 exponent PIT L* [M]",
            "M4 exponent PIT a* [M]",
            "M4 exponent PIT b* [M]",
            
            # M5
            "M5 exponent iris L* [MF]", 
            "M5 exponent iris a* [MF]",
            "M5 exponent iris b* [MF]",
            "M5 exponent PIT L* [MF]",
            "M5 exponent PIT a* [MF]",
            "M5 exponent PIT b* [MF]",
            
            # M6
            "M6 exponent iris L* [F]",
            "M6 exponent iris a* [F]",
            "M6 exponent iris b* [F]",
            "M6 exponent PIT L* [F]",
            "M6 exponent PIT a* [F]",
            "M6 exponent PIT b* [F]",
            
            # M6
            "M6 exponent iris L* [M]",
            "M6 exponent iris a* [M]",
            "M6 exponent iris b* [M]",
            "M6 exponent PIT L* [M]",
            "M6 exponent PIT a* [M]",
            "M6 exponent PIT b* [M]"
)


tiff("Predictions_sextypicality_nonlinear_slopes_SEXT.tif",width=30,height=38,units="cm",res=600,compression = "lzw")

par(mar=c(5.1, 11.1, 1.1, 1.1),mgp=c(1,0.7,0))
plot(NULL,xlim=c(-0.5,2),ylim=c(1,36.5),yaxt="n",bty="n", ylab="", xlab="Posterior dist.", cex.axis=0.7, 
     main="Sextypicality, Scaled Together")

segments(-0.25, c(seq(from=1, to=36, by=1)),2, col="#40808080")

segments(-0.25,33,2, col="#40404040", lwd=18)
segments(-0.25,27,2, col="#40404040", lwd=18)
segments(-0.25,21,2, col="#40404040", lwd=18)

ys<- c(seq(from=1, to=36, by=1))
ys<-rev(ys)

pls <- -1.1
cex_t2 <- 1

cols<-c("#00FBFF", 
        "#8C00FF",
        "#00FF33", 
        "#FFEEBB",
        "#FFA600",
        "#AAEEFF"
)

cols <- rep(cols,6)

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
  #lines(x=PI(toplot,prob=0.89),rep(ycoord-0.05,2),lwd=1.5, lty=5)
  points(x=mean(toplot_CI),ycoord,lwd=2,pch=21,col=1,bg=0,cex=1.2)
  axis(2,at=ycoord,labels=textlab,srt=0,las=1, cex.axis=1, pos=-0.4, tick=F) 
  
  for (i in 1:36) {
    c(polygon(c(dens$x,rev(dens$x)),ys[i]*(c(dens$y,rev(dens$y)*(-1))*0.003)+ycoord[i],border=F, lwd = 1.5, col=colpol))
  }
  lines(x=PI(toplot_CI,prob=0.95),rep(ycoord+0.05,2),lwd=2, lty=1, col='#2f4363')
  segments(x0 = ci[1], y0 = ycoord-0.15, x1 = ci[1], y1 = ycoord+0.25, lwd = 3, col = '#2f4363')
  segments(x0 = ci[2], y0 = ycoord-0.15, x1 = ci[2], y1 = ycoord+0.25, lwd = 3, col = '#2f4363')
  points(x=mean(toplot_CI),ycoord,lwd=2,pch=21,col=1,bg=0,cex=1.2)
}

abline(v=0, lwd=3)
abline(v=1, lwd=3)


dev.off()


