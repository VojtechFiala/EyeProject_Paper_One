# Upload and check the data
Eye1 <- read.csv2("Data_03_08_24.csv",T)

tapply(Eye1$Attr,Eye1$Culture_Year, mean)

summary(Eye1)
Eye1 <- Eye1[!is.na(Eye1$Age),]

Eye1_F <- Eye1[Eye1$Sex=="F",]
summary(Eye1_F)

tapply(Eye1_F$Attr,Eye1_F$Culture_Year,mean)

Eye1_M <- Eye1[Eye1$Sex=="M",]
summary(Eye1_M)

summary(as.factor(Eye1$Sex))
summary(as.factor(Eye1$Culture_Year))

table(Eye1$Culture_Year,as.integer(as.factor(Eye1$Culture_Year)))

library(vioplot)

# Table of descriptive stats for the article (based on final data after edits)
DescStat <- data.frame(
  Sample = levels(as.factor(Eye1$Culture_Year)),
  Count = summary(as.factor(Eye1$Culture_Year)),
  Count_men = summary(as.factor(Eye1$Culture_Year[Eye1$Sex=="M"])),
  Count_women = summary(as.factor(Eye1$Culture_Year[Eye1$Sex=="F"])),
  
  # Age
  Age = paste(paste(round(tapply(Eye1$Age,Eye1$Culture_Year,mean),2),"+/-"),round(tapply(Eye1$Age,Eye1$Culture_Year,sd),2)),
  Age_men = paste(paste(round(tapply(Eye1$Age[Eye1$Sex=="M"],Eye1$Culture_Year[Eye1$Sex=="M"],mean),2),"+/-"),
                  round(tapply(Eye1$Age[Eye1$Sex=="M"],Eye1$Culture_Year[Eye1$Sex=="M"],sd),2)),
  Age_women = paste(paste(round(tapply(Eye1$Age[Eye1$Sex=="F"],Eye1$Culture_Year[Eye1$Sex=="F"],mean),2),"+/-"),
                    round(tapply(Eye1$Age[Eye1$Sex=="F"],Eye1$Culture_Year[Eye1$Sex=="F"],sd),2)),
  
  # L skin
  L_skin = paste(paste(round(tapply(Eye1$L_skin,Eye1$Culture_Year,mean),2),"+/-"),round(tapply(Eye1$L_skin,Eye1$Culture_Year,sd),2)),
  L_skin_men = paste(paste(round(tapply(Eye1$L_skin[Eye1$Sex=="M"],Eye1$Culture_Year[Eye1$Sex=="M"],mean),2),"+/-"),
                     round(tapply(Eye1$L_skin[Eye1$Sex=="M"],Eye1$Culture_Year[Eye1$Sex=="M"],sd),2)),
  L_skin_women = paste(paste(round(tapply(Eye1$L_skin[Eye1$Sex=="F"],Eye1$Culture_Year[Eye1$Sex=="F"],mean),2),"+/-"),
                       round(tapply(Eye1$L_skin[Eye1$Sex=="F"],Eye1$Culture_Year[Eye1$Sex=="F"],sd),2)),
  # L iris 
  L_iris = paste(paste(round(tapply(Eye1$L_iris,Eye1$Culture_Year,mean),2),"+/-"),round(tapply(Eye1$L_iris,Eye1$Culture_Year,sd),2)),
  L_iris_men = paste(paste(round(tapply(Eye1$L_iris[Eye1$Sex=="M"],Eye1$Culture_Year[Eye1$Sex=="M"],mean),2),"+/-"),
                     round(tapply(Eye1$L_iris[Eye1$Sex=="M"],Eye1$Culture_Year[Eye1$Sex=="M"],sd),2)),
  L_iris_women = paste(paste(round(tapply(Eye1$L_iris[Eye1$Sex=="F"],Eye1$Culture_Year[Eye1$Sex=="F"],mean),2),"+/-"),
                       round(tapply(Eye1$L_iris[Eye1$Sex=="F"],Eye1$Culture_Year[Eye1$Sex=="F"],sd),2)),
  # a iris 
  a_iris = paste(paste(round(tapply(Eye1$a_iris,Eye1$Culture_Year,mean),2),"+/-"),round(tapply(Eye1$a_iris,Eye1$Culture_Year,sd),2)),
  a_iris_men = paste(paste(round(tapply(Eye1$a_iris[Eye1$Sex=="M"],Eye1$Culture_Year[Eye1$Sex=="M"],mean),2),"+/-"),
                     round(tapply(Eye1$a_iris[Eye1$Sex=="M"],Eye1$Culture_Year[Eye1$Sex=="M"],sd),2)),
  a_iris_women = paste(paste(round(tapply(Eye1$a_iris[Eye1$Sex=="F"],Eye1$Culture_Year[Eye1$Sex=="F"],mean),2),"+/-"),
                       round(tapply(Eye1$a_iris[Eye1$Sex=="F"],Eye1$Culture_Year[Eye1$Sex=="F"],sd),2)),
  # b iris 
  b_iris = paste(paste(round(tapply(Eye1$b_iris,Eye1$Culture_Year,mean),2),"+/-"),round(tapply(Eye1$b_iris,Eye1$Culture_Year,sd),2)),
  b_iris_men = paste(paste(round(tapply(Eye1$b_iris[Eye1$Sex=="M"],Eye1$Culture_Year[Eye1$Sex=="M"],mean),2),"+/-"),
                     round(tapply(Eye1$b_iris[Eye1$Sex=="M"],Eye1$Culture_Year[Eye1$Sex=="M"],sd),2)),
  b_iris_women = paste(paste(round(tapply(Eye1$b_iris[Eye1$Sex=="F"],Eye1$Culture_Year[Eye1$Sex=="F"],mean),2),"+/-"),
                       round(tapply(Eye1$b_iris[Eye1$Sex=="F"],Eye1$Culture_Year[Eye1$Sex=="F"],sd),2)),
  
  # L sclera 
  L_sclera = paste(paste(round(tapply(Eye1$L_sclera,Eye1$Culture_Year,mean),2),"+/-"),round(tapply(Eye1$L_sclera,Eye1$Culture_Year,sd),2)),
  L_sclera_men = paste(paste(round(tapply(Eye1$L_sclera[Eye1$Sex=="M"],Eye1$Culture_Year[Eye1$Sex=="M"],mean),2),"+/-"),
                       round(tapply(Eye1$L_sclera[Eye1$Sex=="M"],Eye1$Culture_Year[Eye1$Sex=="M"],sd),2)),
  L_sclera_women = paste(paste(round(tapply(Eye1$L_sclera[Eye1$Sex=="F"],Eye1$Culture_Year[Eye1$Sex=="F"],mean),2),"+/-"),
                         round(tapply(Eye1$L_sclera[Eye1$Sex=="F"],Eye1$Culture_Year[Eye1$Sex=="F"],sd),2)),
  # a sclera 
  a_sclera = paste(paste(round(tapply(Eye1$a_sclera,Eye1$Culture_Year,mean),2),"+/-"),round(tapply(Eye1$a_sclera,Eye1$Culture_Year,sd),2)),
  a_sclera_men = paste(paste(round(tapply(Eye1$a_sclera[Eye1$Sex=="M"],Eye1$Culture_Year[Eye1$Sex=="M"],mean),2),"+/-"),
                       round(tapply(Eye1$a_sclera[Eye1$Sex=="M"],Eye1$Culture_Year[Eye1$Sex=="M"],sd),2)),
  a_sclera_women = paste(paste(round(tapply(Eye1$a_sclera[Eye1$Sex=="F"],Eye1$Culture_Year[Eye1$Sex=="F"],mean),2),"+/-"),
                         round(tapply(Eye1$a_sclera[Eye1$Sex=="F"],Eye1$Culture_Year[Eye1$Sex=="F"],sd),2)),
  # b sclera 
  b_sclera = paste(paste(round(tapply(Eye1$b_sclera,Eye1$Culture_Year,mean),2),"+/-"),round(tapply(Eye1$b_sclera,Eye1$Culture_Year,sd),2)),
  b_sclera_men = paste(paste(round(tapply(Eye1$b_sclera[Eye1$Sex=="M"],Eye1$Culture_Year[Eye1$Sex=="M"],mean),2),"+/-"),
                       round(tapply(Eye1$b_sclera[Eye1$Sex=="M"],Eye1$Culture_Year[Eye1$Sex=="M"],sd),2)),
  b_sclera_women = paste(paste(round(tapply(Eye1$b_sclera[Eye1$Sex=="F"],Eye1$Culture_Year[Eye1$Sex=="F"],mean),2),"+/-"),
                         round(tapply(Eye1$b_sclera[Eye1$Sex=="F"],Eye1$Culture_Year[Eye1$Sex=="F"],sd),2)),
  # MeasSext
  MeasSext = paste(paste(round(tapply(Eye1$SexTypMeas,Eye1$Culture_Year,mean),2),"+/-"),round(tapply(Eye1$SexTypMeas,Eye1$Culture_Year,sd),2)),
  MeasSext_men = paste(paste(round(tapply(Eye1$SexTypMeas[Eye1$Sex=="M"],Eye1$Culture_Year[Eye1$Sex=="M"],mean),2),"+/-"),
                       round(tapply(Eye1$SexTypMeas[Eye1$Sex=="M"],Eye1$Culture_Year[Eye1$Sex=="M"],sd),2)),
  MeasSext_women = paste(paste(round(tapply(Eye1$SexTypMeas[Eye1$Sex=="F"],Eye1$Culture_Year[Eye1$Sex=="F"],mean),2),"+/-"),
                         round(tapply(Eye1$SexTypMeas[Eye1$Sex=="F"],Eye1$Culture_Year[Eye1$Sex=="F"],sd),2)),
  # Distinctiveness
  Distinctiveness = paste(paste(round(tapply(Eye1$LOCAL_DIST,Eye1$Culture_Year,mean),2),"+/-"),round(tapply(Eye1$LOCAL_DIST,Eye1$Culture_Year,sd),2)),
  Distinct_men = paste(paste(round(tapply(Eye1$LOCAL_DIST[Eye1$Sex=="M"],Eye1$Culture_Year[Eye1$Sex=="M"],mean),2),"+/-"),
                       round(tapply(Eye1$LOCAL_DIST[Eye1$Sex=="M"],Eye1$Culture_Year[Eye1$Sex=="M"],sd),2)),
  Distinct_women = paste(paste(round(tapply(Eye1$LOCAL_DIST[Eye1$Sex=="F"],Eye1$Culture_Year[Eye1$Sex=="F"],mean),2),"+/-"),
                         round(tapply(Eye1$LOCAL_DIST[Eye1$Sex=="F"],Eye1$Culture_Year[Eye1$Sex=="F"],sd),2)),
  # Asym
  Asymmetry = paste(paste(round(tapply(Eye1$Asym,Eye1$Culture_Year,mean),2),"+/-"),round(tapply(Eye1$Asym,Eye1$Culture_Year,sd),2)),
  Asymmetry_men = paste(paste(round(tapply(Eye1$Asym[Eye1$Sex=="M"],Eye1$Culture_Year[Eye1$Sex=="M"],mean),2),"+/-"),
                        round(tapply(Eye1$Asym[Eye1$Sex=="M"],Eye1$Culture_Year[Eye1$Sex=="M"],sd),2)),
  Asym_women = paste(paste(round(tapply(Eye1$Asym[Eye1$Sex=="F"],Eye1$Culture_Year[Eye1$Sex=="F"],mean),2),"+/-"),
                     round(tapply(Eye1$Asym[Eye1$Sex=="F"],Eye1$Culture_Year[Eye1$Sex=="F"],sd),2)),
  # Attractiveness (perc.)
  Attractiveness = paste(paste(round(tapply(Eye1$Attr,Eye1$Culture_Year,mean),2),"+/-"),round(tapply(Eye1$Attr,Eye1$Culture_Year,sd),2)),
  Attract_men = paste(paste(round(tapply(Eye1$Attr[Eye1$Sex=="M"],Eye1$Culture_Year[Eye1$Sex=="M"],mean),2),"+/-"),
                      round(tapply(Eye1$Attr[Eye1$Sex=="M"],Eye1$Culture_Year[Eye1$Sex=="M"],sd),2)),
  Attract_women = paste(paste(round(tapply(Eye1$Attr[Eye1$Sex=="F"],Eye1$Culture_Year[Eye1$Sex=="F"],mean),2),"+/-"),
                        round(tapply(Eye1$Attr[Eye1$Sex=="F"],Eye1$Culture_Year[Eye1$Sex=="F"],sd),2)),
  # Sextypicality (perc.)
  Perc_Sextypicality = paste(paste(round(tapply(Eye1$SexT,Eye1$Culture_Year,mean),2),"+/-"),round(tapply(Eye1$SexT,Eye1$Culture_Year,sd),2)),
  Perc_Sext_men = paste(paste(round(tapply(Eye1$SexT[Eye1$Sex=="M"],Eye1$Culture_Year[Eye1$Sex=="M"],mean),2),"+/-"),
                        round(tapply(Eye1$SexT[Eye1$Sex=="M"],Eye1$Culture_Year[Eye1$Sex=="M"],sd),2)),
  Perc_Sext_women = paste(paste(round(tapply(Eye1$SexT[Eye1$Sex=="F"],Eye1$Culture_Year[Eye1$Sex=="F"],mean),2),"+/-"),
                          round(tapply(Eye1$SexT[Eye1$Sex=="F"],Eye1$Culture_Year[Eye1$Sex=="F"],sd),2))
)

write.csv2(DescStat, file="Descriptive_Stats_stimuli.csv")
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

# Data could be scaled within or across the cultures:
# Let's see what would be better: 
# The most substantial cross-culture (or cross-photo sample) difference is expected for L*a*b:

# WOMEN:

culture_split <- split(Eye1_F[2:ncol(Eye1_F)], Eye1_F$Group)

standardized_split <- lapply(culture_split, function(sub_df) {
  # Identify numeric columns
  numeric_cols <- sapply(sub_df, is.numeric)
  
  # Standardize numeric columns
  sub_df[, numeric_cols] <- scale(sub_df[, numeric_cols])
})

Eye1_F2 <- do.call(rbind, standardized_split)
Eye1_F2 <- as.data.frame(Eye1_F2)

Eye1_F2$Culture_Year <- Eye1_F$Group


# MEN:

culture_split <- split(Eye1_M[2:ncol(Eye1_M)], Eye1_M$Group)

standardized_split <- lapply(culture_split, function(sub_df) {
  # Identify numeric columns
  numeric_cols <- sapply(sub_df, is.numeric)
  
  # Standardize numeric columns
  sub_df[, numeric_cols] <- scale(sub_df[, numeric_cols])
})

Eye1_M2 <- do.call(rbind, standardized_split)
Eye1_M2 <- as.data.frame(Eye1_M2)

Eye1_M2$Culture_Year <- Eye1_M$Group

library(vioplot)

cols <- c("#445EFF","#705EFF","#505EFF","#905EFF","#600DDD","#400DDD","#100DDD","#444DDD","#411EFF")

# MEN 

tiff("Distributions_CIELAB_L_Mals_14_09_24.tif",width=30,height=30,units="cm",res=600,compression = "lzw")
layout(matrix(1:9,byrow=T,nrow=3), widths=c(2,2,2),heights=c(1,1.1))
# L* skin: Standardised vs. Not....
vioplot(Eye1_M$L_skin~Eye1_M$Culture_Year, range = 1.5, horizontal = F, col = cols, plotCentre = "point", 
        main="Skin L*: Original Scale", ylab="Distribution", xlab="")
vioplot(scale(Eye1_M$L_skin)~Eye1_M$Culture_Year, range = 1.5, horizontal = F, col = cols, plotCentre = "point",
        main="Skin L*: Scaled together", ylab="Stand. distribution", xlab="")
vioplot(scale(Eye1_M2$L_skin)~Eye1_M2$Culture_Year, range = 1.5, horizontal = F, col = cols, plotCentre = "point",
        main="Skin L*: Scaled separately", ylab="Stand. distribution", xlab="")

# L* iris: Standardised vs. Not...
vioplot(Eye1_M$L_iris~Eye1_M$Culture_Year, range = 1.5, horizontal = F, col = cols, plotCentre = "point", 
        main="Iris L*: Original Scale", ylab="Distribution", xlab="")
vioplot(scale(Eye1_M$L_iris)~Eye1_M$Culture_Year, range = 1.5, horizontal = F, col = cols, plotCentre = "point",
        main="Iris L*: Scaled together", ylab="Stand. distribution", xlab="")
vioplot(scale(Eye1_M2$L_iris)~Eye1_M2$Culture_Year, range = 1.5, horizontal = F, col = cols, plotCentre = "point",
        main="Iris L*: Scaled separately", ylab="Stand. distribution", xlab="")

# L* sclera: Standardised vs. Not...
vioplot(Eye1_M$L_sclera~Eye1_M$Culture_Year, range = 1.5, horizontal = F, col = cols, plotCentre = "point", 
        main="Sclera L*: Original Scale", ylab="Distribution", xlab="")
vioplot(scale(Eye1_M$L_sclera)~Eye1_M$Culture_Year, range = 1.5, horizontal = F, col = cols, plotCentre = "point",
        main="Sclera L*: Scaled together", ylab="Stand. distribution", xlab="")
vioplot(scale(Eye1_M2$L_sclera)~Eye1_M2$Culture_Year, range = 1.5, horizontal = F, col = cols, plotCentre = "point",
        main="Sclera L*: Scaled separately", ylab="Stand. distribution", xlab="")
dev.off()


# a*
tiff("Distributions_CIELAB_a_Mals_14_09_24.tif",width=30,height=30,units="cm",res=600,compression = "lzw")
layout(matrix(1:9,byrow=T,nrow=3), widths=c(2,2,2),heights=c(1,1.1))
# L* skin: Standardised vs. Not....
vioplot(Eye1_M$a_skin~Eye1_M$Culture_Year, range = 1.5, horizontal = F, col = cols, plotCentre = "point", 
        main="Skin a*: Original Scale", ylab="Distribution", xlab="")
vioplot(scale(Eye1_M$a_skin)~Eye1_M$Culture_Year, range = 1.5, horizontal = F, col = cols, plotCentre = "point",
        main="Skin a*: Scaled together", ylab="Stand. distribution", xlab="")
vioplot(scale(Eye1_M2$a_skin)~Eye1_M2$Culture_Year, range = 1.5, horizontal = F, col = cols, plotCentre = "point",
        main="Skin a*: Scaled separately", ylab="Stand. distribution", xlab="")

# a* iris: Standardised vs. Not...
vioplot(Eye1_M$a_iris~Eye1_M$Culture_Year, range = 1.5, horizontal = F, col = cols, plotCentre = "point", 
        main="Iris a*: Original Scale", ylab="Distribution", xlab="")
vioplot(scale(Eye1_M$a_iris)~Eye1_M$Culture_Year, range = 1.5, horizontal = F, col = cols, plotCentre = "point",
        main="Iris a*: Scaled together", ylab="Stand. distribution", xlab="")
vioplot(scale(Eye1_M2$a_iris)~Eye1_M2$Culture_Year, range = 1.5, horizontal = F, col = cols, plotCentre = "point",
        main="Iris a*: Scaled separately", ylab="Stand. distribution", xlab="")

# a* sclera: Standardised vs. Not...
vioplot(Eye1_M$a_sclera~Eye1_M$Culture_Year, range = 1.5, horizontal = F, col = cols, plotCentre = "point", 
        main="Sclera a*: Original Scale", ylab="Distribution", xlab="")
vioplot(scale(Eye1_M$a_sclera)~Eye1_M$Culture_Year, range = 1.5, horizontal = F, col = cols, plotCentre = "point",
        main="Sclera a*: Scaled together", ylab="Stand. distribution", xlab="")
vioplot(scale(Eye1_M2$a_sclera)~Eye1_M2$Culture_Year, range = 1.5, horizontal = F, col = cols, plotCentre = "point",
        main="Sclera a*: Scaled separately", ylab="Stand. distribution", xlab="")
dev.off()

# b* 
tiff("Distributions_CIELAB_b_Mals_14_09_24.tif",width=30,height=30,units="cm",res=600,compression = "lzw")
layout(matrix(1:9,byrow=T,nrow=3), widths=c(2,2,2),heights=c(1,1.1))
# L* skin: Standardised vs. Not....
vioplot(Eye1_M$b_skin~Eye1_M$Culture_Year, range = 1.5, horizontal = F, col = cols, plotCentre = "point", 
        main="Skin b*: Original Scale", ylab="Distribution", xlab="")
vioplot(scale(Eye1_M$b_skin)~Eye1_M$Culture_Year, range = 1.5, horizontal = F, col = cols, plotCentre = "point",
        main="Skin b*: Scaled together", ylab="Stand. distribution", xlab="")
vioplot(scale(Eye1_M2$b_skin)~Eye1_M2$Culture_Year, range = 1.5, horizontal = F, col = cols, plotCentre = "point",
        main="Skin b*: Scaled separately", ylab="Stand. distribution", xlab="")

# b* iris: Standardised vs. Not...
vioplot(Eye1_M$b_iris~Eye1_M$Culture_Year, range = 1.5, horizontal = F, col = cols, plotCentre = "point", 
        main="Iris b*: Original Scale", ylab="Distribution", xlab="")
vioplot(scale(Eye1_M$b_iris)~Eye1_M$Culture_Year, range = 1.5, horizontal = F, col = cols, plotCentre = "point",
        main="Iris b*: Scaled together", ylab="Stand. distribution", xlab="")
vioplot(scale(Eye1_M2$b_iris)~Eye1_M2$Culture_Year, range = 1.5, horizontal = F, col = cols, plotCentre = "point",
        main="Iris b*: Scaled separately", ylab="Stand. distribution", xlab="")

# b* sclera: Standardised vs. Not...
vioplot(Eye1_M$b_sclera~Eye1_M$Culture_Year, range = 1.5, horizontal = F, col = cols, plotCentre = "point", 
        main="Sclera b*: Original Scale", ylab="Distribution", xlab="")
vioplot(scale(Eye1_M$b_sclera)~Eye1_M$Culture_Year, range = 1.5, horizontal = F, col = cols, plotCentre = "point",
        main="Sclera b*: Scaled together", ylab="Stand. distribution", xlab="")
vioplot(scale(Eye1_M2$b_sclera)~Eye1_M2$Culture_Year, range = 1.5, horizontal = F, col = cols, plotCentre = "point",
        main="Sclera b*: Scaled separately", ylab="Stand. distribution", xlab="")
dev.off()




# WOMEN

tiff("Distributions_CIELAB_L_Fems_14_09_24.tif",width=30,height=30,units="cm",res=600,compression = "lzw")
layout(matrix(1:9,byrow=T,nrow=3), widths=c(2,2,2),heights=c(1,1.1))
# L* skin: Standardised vs. Not....
vioplot(Eye1_F$L_skin~Eye1_F$Culture_Year, range = 1.5, horizontal = F, col = cols, plotCentre = "point", 
        main="Skin L*: Original Scale", ylab="Distribution", xlab="")
vioplot(scale(Eye1_F$L_skin)~Eye1_F$Culture_Year, range = 1.5, horizontal = F, col = cols, plotCentre = "point",
        main="Skin L*: Scaled together", ylab="Stand. distribution", xlab="")
vioplot(scale(Eye1_F2$L_skin)~Eye1_F2$Culture_Year, range = 1.5, horizontal = F, col = cols, plotCentre = "point",
        main="Skin L*: Scaled separately", ylab="Stand. distribution", xlab="")

# L* iris: Standardised vs. Not...
vioplot(Eye1_F$L_iris~Eye1_F$Culture_Year, range = 1.5, horizontal = F, col = cols, plotCentre = "point", 
        main="Iris L*: Original Scale", ylab="Distribution", xlab="")
vioplot(scale(Eye1_F$L_iris)~Eye1_F$Culture_Year, range = 1.5, horizontal = F, col = cols, plotCentre = "point",
        main="Iris L*: Scaled together", ylab="Stand. distribution", xlab="")
vioplot(scale(Eye1_F2$L_iris)~Eye1_F2$Culture_Year, range = 1.5, horizontal = F, col = cols, plotCentre = "point",
        main="Iris L*: Scaled separately", ylab="Stand. distribution", xlab="")

# L* sclera: Standardised vs. Not...
vioplot(Eye1_F$L_sclera~Eye1_F$Culture_Year, range = 1.5, horizontal = F, col = cols, plotCentre = "point", 
        main="Sclera L*: Original Scale", ylab="Distribution", xlab="")
vioplot(scale(Eye1_F$L_sclera)~Eye1_F$Culture_Year, range = 1.5, horizontal = F, col = cols, plotCentre = "point",
        main="Sclera L*: Scaled together", ylab="Stand. distribution", xlab="")
vioplot(scale(Eye1_F2$L_sclera)~Eye1_F2$Culture_Year, range = 1.5, horizontal = F, col = cols, plotCentre = "point",
        main="Sclera L*: Scaled separately", ylab="Stand. distribution", xlab="")
dev.off()


# a*
tiff("Distributions_CIELAB_a_Fems_14_09_24.tif",width=30,height=30,units="cm",res=600,compression = "lzw")
layout(matrix(1:9,byrow=T,nrow=3), widths=c(2,2,2),heights=c(1,1.1))
# L* skin: Standardised vs. Not....
vioplot(Eye1_F$a_skin~Eye1_F$Culture_Year, range = 1.5, horizontal = F, col = cols, plotCentre = "point", 
        main="Skin a*: Original Scale", ylab="Distribution", xlab="")
vioplot(scale(Eye1_F$a_skin)~Eye1_F$Culture_Year, range = 1.5, horizontal = F, col = cols, plotCentre = "point",
        main="Skin a*: Scaled together", ylab="Stand. distribution", xlab="")
vioplot(scale(Eye1_F2$a_skin)~Eye1_F2$Culture_Year, range = 1.5, horizontal = F, col = cols, plotCentre = "point",
        main="Skin a*: Scaled separately", ylab="Stand. distribution", xlab="")

# a* iris: Standardised vs. Not...
vioplot(Eye1_F$a_iris~Eye1_F$Culture_Year, range = 1.5, horizontal = F, col = cols, plotCentre = "point", 
        main="Iris a*: Original Scale", ylab="Distribution", xlab="")
vioplot(scale(Eye1_F$a_iris)~Eye1_F$Culture_Year, range = 1.5, horizontal = F, col = cols, plotCentre = "point",
        main="Iris a*: Scaled together", ylab="Stand. distribution", xlab="")
vioplot(scale(Eye1_F2$a_iris)~Eye1_F2$Culture_Year, range = 1.5, horizontal = F, col = cols, plotCentre = "point",
        main="Iris a*: Scaled separately", ylab="Stand. distribution", xlab="")

# a* sclera: Standardised vs. Not...
vioplot(Eye1_F$a_sclera~Eye1_F$Culture_Year, range = 1.5, horizontal = F, col = cols, plotCentre = "point", 
        main="Sclera a*: Original Scale", ylab="Distribution", xlab="")
vioplot(scale(Eye1_F$a_sclera)~Eye1_F$Culture_Year, range = 1.5, horizontal = F, col = cols, plotCentre = "point",
        main="Sclera a*: Scaled together", ylab="Stand. distribution", xlab="")
vioplot(scale(Eye1_F2$a_sclera)~Eye1_F2$Culture_Year, range = 1.5, horizontal = F, col = cols, plotCentre = "point",
        main="Sclera a*: Scaled separately", ylab="Stand. distribution", xlab="")
dev.off()

# b* 
tiff("Distributions_CIELAB_b_Fems_14_09_24.tif",width=30,height=30,units="cm",res=600,compression = "lzw")
layout(matrix(1:9,byrow=T,nrow=3), widths=c(2,2,2),heights=c(1,1.1))
# L* skin: Standardised vs. Not....
vioplot(Eye1_F$b_skin~Eye1_F$Culture_Year, range = 1.5, horizontal = F, col = cols, plotCentre = "point", 
        main="Skin b*: Original Scale", ylab="Distribution", xlab="")
vioplot(scale(Eye1_F$b_skin)~Eye1_F$Culture_Year, range = 1.5, horizontal = F, col = cols, plotCentre = "point",
        main="Skin b*: Scaled together", ylab="Stand. distribution", xlab="")
vioplot(scale(Eye1_F2$b_skin)~Eye1_F2$Culture_Year, range = 1.5, horizontal = F, col = cols, plotCentre = "point",
        main="Skin b*: Scaled separately", ylab="Stand. distribution", xlab="")

# b* iris: Standardised vs. Not...
vioplot(Eye1_F$b_iris~Eye1_F$Culture_Year, range = 1.5, horizontal = F, col = cols, plotCentre = "point", 
        main="Iris b*: Original Scale", ylab="Distribution", xlab="")
vioplot(scale(Eye1_F$b_iris)~Eye1_F$Culture_Year, range = 1.5, horizontal = F, col = cols, plotCentre = "point",
        main="Iris b*: Scaled together", ylab="Stand. distribution", xlab="")
vioplot(scale(Eye1_F2$b_iris)~Eye1_F2$Culture_Year, range = 1.5, horizontal = F, col = cols, plotCentre = "point",
        main="Iris b*: Scaled separately", ylab="Stand. distribution", xlab="")

# b* sclera: Standardised vs. Not...
vioplot(Eye1_F$b_sclera~Eye1_F$Culture_Year, range = 1.5, horizontal = F, col = cols, plotCentre = "point", 
        main="Sclera b*: Original Scale", ylab="Distribution", xlab="")
vioplot(scale(Eye1_F$b_sclera)~Eye1_F$Culture_Year, range = 1.5, horizontal = F, col = cols, plotCentre = "point",
        main="Sclera b*: Scaled together", ylab="Stand. distribution", xlab="")
vioplot(scale(Eye1_F2$b_sclera)~Eye1_F2$Culture_Year, range = 1.5, horizontal = F, col = cols, plotCentre = "point",
        main="Sclera b*: Scaled separately", ylab="Stand. distribution", xlab="")
dev.off()


# Plotting togeter - Figure 1 in the manuscript [Together = men and female in the same plot]: 

# Upload and check the data
Eye1 <- read.csv2("Data_03_08_24.csv",T)

tapply(Eye1$Attr,Eye1$Culture_Year, mean)

summary(Eye1)
Eye1 <- Eye1[!is.na(Eye1$Age),]

summary(as.factor(Eye1$Sex))
summary(as.factor(Eye1$Culture_Year))

table(Eye1$Culture_Year,as.integer(as.factor(Eye1$Culture_Year)))

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

# Data could be scaled within or across the cultures:
# Let's see what would be better: 
# The most substantial cross-culture (or cross-photo sample) difference is expected for L*a*b:

# Standardisation:

culture_split <- split(Eye1[2:ncol(Eye1)], Eye1$Culture_Year)

standardized_split <- lapply(culture_split, function(sub_df) {
  # Identify numeric columns
  numeric_cols <- sapply(sub_df, is.numeric)
  
  # Standardize numeric columns
  sub_df[, numeric_cols] <- scale(sub_df[, numeric_cols])
})

Eye1_2 <- do.call(rbind, standardized_split)
Eye1_2 <- as.data.frame(Eye1_2)

Eye1_2$Culture_Year <- Eye1$Culture_Year
Eye1_2$sex <- Eye1$Sex


tiff("Distributions_CIELAB_L_TOGETHER.tif",width=30,height=30,units="cm",res=600,compression = "lzw")
layout(matrix(1:9,byrow=T,nrow=3), widths=c(2,2,2),heights=c(1,1.1))
# L* skin: Standardised vs. Not....
vioplot(Eye1$L_skin~Eye1$Culture_Year, range = 1.5, horizontal = F, col = cols, plotCentre = "point", 
        main="Skin L*: Original Scale", ylab="Distribution", xlab="")
vioplot(scale(Eye1$L_skin)~Eye1$Culture_Year, range = 1.5, horizontal = F, col = cols, plotCentre = "point",
        main="Skin L*: Scaled together", ylab="Stand. distribution", xlab="")
vioplot(scale(Eye1_2$L_skin)~Eye1_2$Culture_Year, range = 1.5, horizontal = F, col = cols, plotCentre = "point",
        main="Skin L*: Scaled separately", ylab="Stand. distribution", xlab="")

# L* iris: Standardised vs. Not...
vioplot(Eye1$L_iris~Eye1$Culture_Year, range = 1.5, horizontal = F, col = cols, plotCentre = "point", 
        main="Iris L*: Original Scale", ylab="Distribution", xlab="")
vioplot(scale(Eye1$L_iris)~Eye1$Culture_Year, range = 1.5, horizontal = F, col = cols, plotCentre = "point",
        main="Iris L*: Scaled together", ylab="Stand. distribution", xlab="")
vioplot(scale(Eye1_2$L_iris)~Eye1_2$Culture_Year, range = 1.5, horizontal = F, col = cols, plotCentre = "point",
        main="Iris L*: Scaled separately", ylab="Stand. distribution", xlab="")

# L* sclera: Standardised vs. Not...
vioplot(Eye1$L_sclera~Eye1$Culture_Year, range = 1.5, horizontal = F, col = cols, plotCentre = "point", 
        main="Sclera L*: Original Scale", ylab="Distribution", xlab="")
vioplot(scale(Eye1$L_sclera)~Eye1$Culture_Year, range = 1.5, horizontal = F, col = cols, plotCentre = "point",
        main="Sclera L*: Scaled together", ylab="Stand. distribution", xlab="")
vioplot(scale(Eye1_2$L_sclera)~Eye1_2$Culture_Year, range = 1.5, horizontal = F, col = cols, plotCentre = "point",
        main="Sclera L*: Scaled separately", ylab="Stand. distribution", xlab="")
dev.off()


# a*
tiff("Distributions_CIELAB_a_TOGETHER.tif",width=30,height=30,units="cm",res=600,compression = "lzw")
layout(matrix(1:9,byrow=T,nrow=3), widths=c(2,2,2),heights=c(1,1.1))
# L* skin: Standardised vs. Not....
vioplot(Eye1$a_skin~Eye1$Culture_Year, range = 1.5, horizontal = F, col = cols, plotCentre = "point", 
        main="Skin a*: Original Scale", ylab="Distribution", xlab="")
vioplot(scale(Eye1$a_skin)~Eye1$Culture_Year, range = 1.5, horizontal = F, col = cols, plotCentre = "point",
        main="Skin a*: Scaled together", ylab="Stand. distribution", xlab="")
vioplot(scale(Eye1_2$a_skin)~Eye1_2$Culture_Year, range = 1.5, horizontal = F, col = cols, plotCentre = "point",
        main="Skin a*: Scaled separately", ylab="Stand. distribution", xlab="")

# a* iris: Standardised vs. Not...
vioplot(Eye1$a_iris~Eye1$Culture_Year, range = 1.5, horizontal = F, col = cols, plotCentre = "point", 
        main="Iris a*: Original Scale", ylab="Distribution", xlab="")
vioplot(scale(Eye1$a_iris)~Eye1$Culture_Year, range = 1.5, horizontal = F, col = cols, plotCentre = "point",
        main="Iris a*: Scaled together", ylab="Stand. distribution", xlab="")
vioplot(scale(Eye1_2$a_iris)~Eye1_2$Culture_Year, range = 1.5, horizontal = F, col = cols, plotCentre = "point",
        main="Iris a*: Scaled separately", ylab="Stand. distribution", xlab="")

# a* sclera: Standardised vs. Not...
vioplot(Eye1$a_sclera~Eye1$Culture_Year, range = 1.5, horizontal = F, col = cols, plotCentre = "point", 
        main="Sclera a*: Original Scale", ylab="Distribution", xlab="")
vioplot(scale(Eye1$a_sclera)~Eye1$Culture_Year, range = 1.5, horizontal = F, col = cols, plotCentre = "point",
        main="Sclera a*: Scaled together", ylab="Stand. distribution", xlab="")
vioplot(scale(Eye1_2$a_sclera)~Eye1_2$Culture_Year, range = 1.5, horizontal = F, col = cols, plotCentre = "point",
        main="Sclera a*: Scaled separately", ylab="Stand. distribution", xlab="")
dev.off()

# b* 
tiff("Distributions_CIELAB_b_TOGETHER.tif",width=30,height=30,units="cm",res=600,compression = "lzw")
layout(matrix(1:9,byrow=T,nrow=3), widths=c(2,2,2),heights=c(1,1.1))
# L* skin: Standardised vs. Not....
vioplot(Eye1$b_skin~Eye1$Culture_Year, range = 1.5, horizontal = F, col = cols, plotCentre = "point", 
        main="Skin b*: Original Scale", ylab="Distribution", xlab="")
vioplot(scale(Eye1$b_skin)~Eye1$Culture_Year, range = 1.5, horizontal = F, col = cols, plotCentre = "point",
        main="Skin b*: Scaled together", ylab="Stand. distribution", xlab="")
vioplot(scale(Eye1_2$b_skin)~Eye1_2$Culture_Year, range = 1.5, horizontal = F, col = cols, plotCentre = "point",
        main="Skin b*: Scaled separately", ylab="Stand. distribution", xlab="")

# b* iris: Standardised vs. Not...
vioplot(Eye1$b_iris~Eye1$Culture_Year, range = 1.5, horizontal = F, col = cols, plotCentre = "point", 
        main="Iris b*: Original Scale", ylab="Distribution", xlab="")
vioplot(scale(Eye1$b_iris)~Eye1$Culture_Year, range = 1.5, horizontal = F, col = cols, plotCentre = "point",
        main="Iris b*: Scaled together", ylab="Stand. distribution", xlab="")
vioplot(scale(Eye1_2$b_iris)~Eye1_2$Culture_Year, range = 1.5, horizontal = F, col = cols, plotCentre = "point",
        main="Iris b*: Scaled separately", ylab="Stand. distribution", xlab="")

# b* sclera: Standardised vs. Not...
vioplot(Eye1$b_sclera~Eye1$Culture_Year, range = 1.5, horizontal = F, col = cols, plotCentre = "point", 
        main="Sclera b*: Original Scale", ylab="Distribution", xlab="")
vioplot(scale(Eye1$b_sclera)~Eye1$Culture_Year, range = 1.5, horizontal = F, col = cols, plotCentre = "point",
        main="Sclera b*: Scaled together", ylab="Stand. distribution", xlab="")
vioplot(scale(Eye1_2$b_sclera)~Eye1_2$Culture_Year, range = 1.5, horizontal = F, col = cols, plotCentre = "point",
        main="Sclera b*: Scaled separately", ylab="Stand. distribution", xlab="")
dev.off()


#  FIGURE 1 in the manuscript: 
# Iris L*a*b*
# Sclera L*a*b*
# Men and women in a single vioplot M/F
cols_M <- c("#445EFF","#705EFF","#505EFF","#3744d4","#3b74f7","#4a55cf","#100DDD","#444DDD","#411EFF")
cols_F <- c("#7ff581","#78d67a","#4cba4f","#29a62d","#58a15b","#49a34d","#65c756","#42bd2f","#2cba16")
cult_labs <- c("CMR 13", "CMR 16", "Colombia", "CZ 16", "CZ 19", "India", "Iran", "Turkey", "Vietnam")

tiff("Distributions_CIELAB_L_a_b_iris_sclera.tif",width=35,height=25,units="cm",res=600,compression = "lzw")

layout(matrix(1:6,byrow=T,nrow=2), widths=c(2,2,2),heights=c(1,1))

# L* skin: Standardised vs. Not....
par(cex.main = 1.65)

vioplot(Eye1_M$L_sclera~Eye1_M$Culture_Year, col = cols_M, plotCentre = "line", side = "left", range = 1.5, horizontal = F,  
        cex.main=2.5, main="Peri-iridal tissues [PIT/PIVA] L* (Lightness axis)", xaxt='n', ylab="", xlab="")
axis(1, at = 1:9, labels = rep("",9), cex=2.5, las=2)
text(x = 1:length(cult_labs),
     y = par("usr")[3]-3,
     labels = cult_labs,
     xpd = NA,
     ## Rotate the labels by 35 degrees.
     srt = 35,
     cex = 1.6,
     adj=0.7)
vioplot(Eye1_F$L_sclera~Eye1_F$Culture_Year, col = cols_F, plotCentre = "line", side = "right", add = T, range = 1.5, horizontal = F)
legend("topleft", fill = c("blue", "#4cba4f"), legend = c("Men", "Women"), title = "Colour palette")

vioplot(Eye1_M$a_sclera~Eye1_M$Culture_Year, col = cols_M, plotCentre = "line", side = "left", range = 1.5, horizontal = F,  
        cex.main=2.5, main="Peri-iridal tissues [PIT/PIVA] a* (Red-Green axis)", xaxt='n', ylab="", xlab="")
axis(1, at = 1:9, labels = rep("",9), cex=2.5, las=2)
text(x = 1:length(cult_labs),
     y = par("usr")[3]-1,
     labels = cult_labs,
     xpd = NA,
     ## Rotate the labels by 35 degrees.
     srt = 35,
     cex = 1.6,
     adj=0.7)
vioplot(Eye1_F$a_sclera~Eye1_F$Culture_Year, col = cols_F, plotCentre = "line", side = "right", add = T, range = 1.5, horizontal = F)
#legend("topright", fill = c("blue", "#4cba4f"), legend = c("Men", "Women"), title = "Colour palette")

vioplot(Eye1_M$b_sclera~Eye1_M$Culture_Year, col = cols_M, plotCentre = "line", side = "left", range = 1.5, horizontal = F,  
        cex.main=2.5, main="Peri-iridal tissues [PIT/PIVA] b* (Blue-Yellow axis)", xaxt='n', ylab="", xlab="")
axis(1, at = 1:9, labels = rep("",9), cex=2.5, las=2)
text(x = 1:length(cult_labs),
     y = par("usr")[3]-2.0,
     labels = cult_labs,
     xpd = NA,
     ## Rotate the labels by 35 degrees.
     srt = 35,
     cex = 1.6,
     adj=0.7)
vioplot(Eye1_F$b_sclera~Eye1_F$Culture_Year, col = cols_F, plotCentre = "line", side = "right", add = T, range = 1.5, horizontal = F)
#legend("topright", fill = c("blue", "#4cba4f"), legend = c("Men", "Women"), title = "Colour palette")



vioplot(Eye1_M$L_iris~Eye1_M$Culture_Year, col = cols_M, plotCentre = "line", side = "left", range = 1.5, horizontal = F,  
        cex.main=2.5, main="Iris L* (Lightness axis)", xaxt='n', ylab="", xlab="")
axis(1, at = 1:9, labels = rep("",9), cex=2.5, las=2)
text(x = 1:length(cult_labs),
     y = par("usr")[3]-3,
     labels = cult_labs,
     xpd = NA,
     ## Rotate the labels by 35 degrees.
     srt = 35,
     cex = 1.6,
     adj=0.7)
vioplot(Eye1_F$L_iris~Eye1_F$Culture_Year, col = cols_F, plotCentre = "line", side = "right", add = T, range = 1.5, horizontal = F)
legend("topleft", fill = c("blue", "#4cba4f"), legend = c("Men", "Women"), title = "Colour palette")

vioplot(Eye1_M$a_iris~Eye1_M$Culture_Year, col = cols_M, plotCentre = "line", side = "left", range = 1.5, horizontal = F,  
        cex.main=2.5, main="Iris a* (Red-Green axis)", xaxt='n', ylab="", xlab="")
axis(1, at = 1:9, labels = rep("",9), cex=2.5, las=2)
text(x = 1:length(cult_labs),
     y = par("usr")[3]-2.0,
     labels = cult_labs,
     xpd = NA,
     ## Rotate the labels by 35 degrees.
     srt = 35,
     cex = 1.6,
     adj=0.7)
vioplot(Eye1_F$a_iris~Eye1_F$Culture_Year, col = cols_F, plotCentre = "line", side = "right", add = T, range = 1.5, horizontal = F)
#legend("topleft", fill = c("blue", "#4cba4f"), legend = c("Men", "Women"), title = "Colour palette")

vioplot(Eye1_M$b_iris~Eye1_M$Culture_Year, col = cols_M, plotCentre = "line", side = "left", range = 1.5, horizontal = F,  
        cex.main=2.5, main="Iris b* (Blue-Yellow axis)", xaxt='n', ylab="", xlab="", ylim=c(-10,30))
axis(1, at = 1:9, labels = rep("",9), cex=2.5, las=2)
text(x = 1:length(cult_labs),
     y = par("usr")[3]-2.25,
     labels = cult_labs,
     xpd = NA,
     ## Rotate the labels by 35 degrees.
     srt = 35,
     cex = 1.6,
     adj=0.7)
vioplot(Eye1_F$b_iris~Eye1_F$Culture_Year, col = cols_F, plotCentre = "line", side = "right", add = T, range = 1.5, horizontal = F)
#legend("topleft", fill = c("blue", "#4cba4f"), legend = c("Men", "Women"), title = "Colour palette")

dev.off()

