# Counterfactual plots...

setwd("D:/GACR_VF/Measured_Eye_Phenotype_And_Ascribed_Characteristics_In_Different_Countries/Final Figures Draft Data/Nelinear_Attempts/Standardised_Separately_Attractivenes")

#                                    WAIC     SE  dWAIC    dSE pWAIC weight
# SEP_m_1_attr                     2709.9  48.14    0.0     NA  99.6      1
# SEP_m_1_6_FREE_QUAD_SEX_attr     2733.0  47.26   23.2  14.69 107.2      0
# SEP_m_1_2_SQ7_attr               2733.1  47.70   23.2   9.87 129.8      0
# SEP_m_1_4_only_varying_attr_sex2 3438.0  60.86  728.2  46.09 361.0      0
# SEP_m_1_3_only_varying_attr      4013.7  82.47 1303.8  76.16 620.3      0
# SEP_m_1_5_FREE_QUAD_attr         4651.7 304.52 1941.8 303.80 941.9      0


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

library(rethinking)

# Load the posteriors: 

load("model_nonlinear_attr_SEP_12_06_25_1.Rdata")
post1 <- extract.samples(m_1_attrSEP)
str(post1)

load("model_nonlinear_attr_SEP_12_06_25_2.Rdata")
post2 <- extract.samples(m_1_2_SQ7_attrSEP)
str(post2)

load("model_nonlinear_attr_SEP_12_06_25_3.Rdata")
post3 <- extract.samples(m_1_3_only_varying_attrSEP)
str(post3)

load("model_nonlinear_attr_SEP_12_06_25_4.Rdata")
post4 <- extract.samples(m_1_4_only_varying_attr_sex2SEP)
str(post4)

load("model_nonlinear_attr_SEP_12_06_25_5.Rdata")
post5 <- extract.samples(m_1_5_FREE_and_LINN_attrSEP)
str(post5)

load("model_nonlinear_attr_SEP_12_06_25_6.Rdata")
post6 <- extract.samples(m_1_6_FREE_MF_attrSEP)
str(post6)

# Load the data: 
Eye <- read.csv2("Data_03_08_24.csv",T) 

Eye1 <- Eye

culture_split <- split(Eye1[2:ncol(Eye1)], Eye1$Culture_Year)

standardized_split <- lapply(culture_split, function(sub_df) {
  # Identify numeric columns
  numeric_cols <- sapply(sub_df, is.numeric)
  
  # Standardize numeric columns
  sub_df[, numeric_cols] <- scale(sub_df[, numeric_cols])
})

Eye_2 <- do.call(rbind, standardized_split)
Eye_2 <- as.data.frame(Eye_2)

Eye_2$Culture_Year <- Eye$Culture_Year
Eye_2$sex <- Eye$Sex

# Process the importat part of the data: 
L_sclera <- data.frame(L_sclera_crude = Eye$L_sclera,
                       Attr = Eye_2$Attr,
                       L_sclera_stan = Eye_2$L_sclera,
                       Culture = as.numeric(as.factor(Eye_2$Culture_Year)),
                       L_sclera_nz = Eye_2$L_sclera-min(Eye_2$L_sclera)+0.001,
                       Sex = as.numeric(as.factor(Eye_2$sex))
)

table(L_sclera$Culture,Eye_2$Culture_Year)

#                 1   2   3   4   5   6   7   8   9
# Cameroon_2013  99   0   0   0   0   0   0   0   0
# Cameroon_2016   0  98   0   0   0   0   0   0   0
# Colombia        0   0 138   0   0   0   0   0   0
# Czech_2016      0   0   0 100   0   0   0   0   0
# Czech_2019      0   0   0   0  95   0   0   0   0
# India_FCD       0   0   0   0   0 142   0   0   0
# Iran            0   0   0   0   0   0  87   0   0
# Turkey          0   0   0   0   0   0   0 184   0
# Vietnam         0   0   0   0   0   0   0   0  91


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

summary.data.frame(include)
str(include)

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


tiff("Predictions_attractiveness_nonlinear_slopes_ATTR_STAN_SEP.tif",width=30,height=38,units="cm",res=600,compression = "lzw")

par(mar=c(5.1, 11.1, 1.1, 1.1),mgp=c(1,0.7,0))
plot(NULL,xlim=c(-0.5,2),ylim=c(1,36.5),yaxt="n",bty="n", ylab="", xlab="Posterior dist.", cex.axis=0.7, 
     main="Attractiveness, Scaled Separately")

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
# Plot #2A Sextypicality... 
# ! Standardised separately
#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-


# Load the posteriors: 

load("model_nonlinear_sext_SEP_12_06_25_1.Rdata")
post1 <- extract.samples(m_1_sextSEP)
str(post1)

load("model_nonlinear_sext_SEP_12_06_25_2.Rdata")
post2 <- extract.samples(m_1_2_SQ7_sextSEP)
str(post2)

load("model_nonlinear_sext_SEP_12_06_25_3.Rdata")
post3 <- extract.samples(m_1_3_only_varying_sextSEP)
str(post3)

load("model_nonlinear_sext_SEP_12_06_25_4.Rdata")
post4 <- extract.samples(m_1_4_only_varying_sext_sex2SEP)
str(post4)

load("model_nonlinear_sext_SEP_12_06_25_5.Rdata")
post5 <- extract.samples(m_1_5_FREE_QUAD_sextSEP)
str(post5)

load("model_nonlinear_sext_SEP_12_06_25_6.Rdata")
post6 <- extract.samples(m_1_6_FREE_QUAD_SEX_sextSEP)
str(post6)

# Load the data: 
Eye <- read.csv2("Data_03_08_24.csv",T) 

Eye1 <- Eye

culture_split <- split(Eye1[2:ncol(Eye1)], Eye1$Culture_Year)

standardized_split <- lapply(culture_split, function(sub_df) {
  # Identify numeric columns
  numeric_cols <- sapply(sub_df, is.numeric)
  
  # Standardize numeric columns
  sub_df[, numeric_cols] <- scale(sub_df[, numeric_cols])
})

Eye_2 <- do.call(rbind, standardized_split)
Eye_2 <- as.data.frame(Eye_2)

Eye_2$Culture_Year <- Eye$Culture_Year
Eye_2$sex <- Eye$Sex

# Process the importat part of the data: 
L_sclera <- data.frame(L_sclera_crude = Eye$L_sclera,
                       SexT = Eye_2$SexT,
                       L_sclera_stan = Eye_2$L_sclera,
                       Culture = as.numeric(as.factor(Eye_2$Culture_Year)),
                       L_sclera_nz = Eye_2$L_sclera-min(Eye_2$L_sclera)+0.001,
                       Sex = as.numeric(as.factor(Eye_2$sex))
)

table(L_sclera$Culture,Eye_2$Culture_Year)

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


tiff("Predictions_sextypicality_nonlinear_slopes_sext.tif",width=30,height=38,units="cm",res=600,compression = "lzw")

par(mar=c(5.1, 11.1, 1.1, 1.1),mgp=c(1,0.7,0))
plot(NULL,xlim=c(-0.5,2),ylim=c(1,36.5),yaxt="n",bty="n", ylab="", xlab="Posterior dist.", cex.axis=0.7, 
     main="Sextypicality, Scaled Separately")

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

