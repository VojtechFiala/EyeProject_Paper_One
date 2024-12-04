########################################################################
# Dear readers, the facial landmarks may lead to identification of individual 
# participants, and we were not given clearance to make them publicly available. 
# The script below, therefore, only serve for illustratory purposes. 
# If you spot a mistake please let us know on fialavoj@natur.cuni.cz
# If you are thinking of a project in which you may take an advantage of our 
# facial stimuli or the landmark configurations, please, contact karel.kleisner@natur.cuni.cz

# If you want to rerun the analyses, reported in the paper, other than the 
# caclucation of GMM variables, please, continue to the script "2_Drawing_distribution_plots_and_deriving_tables.R"
# All else should work correctly! 

########################################################################


# Packages for GM calculation: 

library(Morpho)
library(geomorph)
library(abind)


# The data (ratings of attractiveness and sextypicality, age, colour variables of face and eyes): 
Lab_eye <- read.csv2("Eye_Proj_FINDATA.csv",T)

Lab_eye$Group <- paste(Lab_eye$Culture_Year,Lab_eye$Sex)

levels(as.factor(Lab_eye$Group)) # Group combines country, year, and sex... 

# Subset the data for each culture: First women, then men: 

# These GMM datasets has not been made publicly available! 

Lab_CMR_13 <- rbind.data.frame(Lab_eye[Lab_eye$Group=="Cameroon_2013 F",],
                               Lab_eye[Lab_eye$Group=="Cameroon_2013 M",])

Lab_CMR_16 <- rbind.data.frame(Lab_eye[Lab_eye$Group=="Cameroon_2016 F",],
                               Lab_eye[Lab_eye$Group=="Cameroon_2016 M",])

Lab_Colombia <- rbind.data.frame(Lab_eye[Lab_eye$Group=="Colombia F",],
                               Lab_eye[Lab_eye$Group=="Colombia M",])

Lab_Czech_16 <- rbind.data.frame(Lab_eye[Lab_eye$Group=="Czech_2016 F",],
                                 Lab_eye[Lab_eye$Group=="Czech_2016 M",])

Lab_Czech_19 <- rbind.data.frame(Lab_eye[Lab_eye$Group=="Czech_2019 F",],
                                 Lab_eye[Lab_eye$Group=="Czech_2019 M",])

Lab_India <- rbind.data.frame(Lab_eye[Lab_eye$Group=="India_FCD F",],
                                 Lab_eye[Lab_eye$Group=="India_FCD M",])

Lab_Iran <- rbind.data.frame(Lab_eye[Lab_eye$Group=="Iran F",],
                                 Lab_eye[Lab_eye$Group=="Iran M",])

Lab_Turkey <- rbind.data.frame(Lab_eye[Lab_eye$Group=="Turkey F",],
                        Lab_eye[Lab_eye$Group=="Turkey M",])
#Lab_Turkey$ID <- paste(Lab_Turkey$ID, ".bmp")
#Lab_Turkey$ID <- gsub(" ","",Lab_Turkey$ID)

Lab_Vietnam <- rbind.data.frame(Lab_eye[Lab_eye$Group=="Vietnam F",],
                                 Lab_eye[Lab_eye$Group=="Vietnam M",])


# Load tools for GM - links, sliders, tps files 
slids<-read.table("slidersR.txt")
links<-read.table("linksR.txt")

# All the subsequent calculations were conducted using NON-symmetrised sets of coordinates: 

# Individual TPS files: 
CAM13F <- readland.tps("F_cmr13.tps", specID = "imageID")
CAM13M <- readland.tps("M_cmr13.tps", specID = "imageID")
CAM13 <- abind(CAM13F,CAM13M) # -> WOMEN -> MEN -> 
data.frame(Dim=dimnames(CAM13)[[3]],Lab=Lab_CMR_13$ID) 

CAM16F <- readland.tps("F_cmr16.tps", specID = "ID")
CAM16M <- readland.tps("M_cmr16.tps", specID = "ID")
CAM16 <- abind(CAM16F,CAM16M)   # -> WOMEN -> MEN -> 
data.frame(dim=dimnames(CAM16)[[3]],Lab=Lab_CMR_16$ID)

COLF <- readland.tps("F_col.tps", specID = "imageID")
COLM <- readland.tps("M_col.tps", specID = "imageID")
COL <- abind(COLF,COLM) # -> WOMEN -> MEN -> 
data.frame(dim=dimnames(COL)[[3]],Lab=Lab_Colombia$ID)

CZ16F <- readland.tps("F_cz16.tps", specID = "imageID")
CZ16M <- readland.tps("M_cz16.tps", specID = "imageID") 
CZ16 <- abind(CZ16F,CZ16M) # -> WOMEN -> MEN -> 
data.frame(dim=dimnames(CZ16)[[3]],Lab=Lab_Czech_16$ID)
cor(as.numeric(dimnames(CZ16)[[3]]),as.numeric(substr(Lab_Czech_16$ID,1,4)))

CZ19F <- readland.tps("CZ_Women_2019_Checked.tps", specID = "imageID")
CZ19M <- readland.tps("CZ_Men_2019_CHECKED.tps", specID = "imageID")
CZ19 <- abind(CZ19F,CZ19M)  # -> WOMEN -> MEN -> 
data.frame(dim=dimnames(CZ19)[[3]],Lab=Lab_Czech_19$ID)

INDF <- readland.tps("India_F_Checked.tps", specID = "imageID")
INDM <- readland.tps("India_M_Checked.tps", specID = "imageID")
IND <- abind(INDF,INDM)
data.frame(dim=dimnames(IND)[[3]],Lab=Lab_India$ID) 

IRF <- readland.tps("Iran_females_72_Checked.tps", specID = "imageID")
IRM <- readland.tps("Iran_males_72_Checked.tps", specID = "imageID")
IR <- abind(IRF,IRM)
data.frame(dim=dimnames(IR)[[3]],Lab=Lab_Iran$ID) 


TURF <- readland.tps("F_tur.tps", specID = "imageID")
TURF <- TURF[,,match(Lab_Turkey$ID[Lab_Turkey$Sex=="F"],dimnames(TURF)[[3]])]
TURM <- readland.tps("M_tur.tps", specID = "imageID")
TURM <- TURM[,,match(Lab_Turkey$ID[Lab_Turkey$Sex=="M"],dimnames(TURM)[[3]])]
TUR <- abind(TURF,TURM)
cor(as.numeric(dimnames(TUR)[[3]]),as.numeric(Lab_Turkey$ID))


VNF <- readland.tps("VNM_females.tps", specID = "imageID")
VNF<-VNF[,,-match("2080",dimnames(VNF)[[3]])]
str(VNF)
VNM <- readland.tps ("VNM_males.tps",specID = "imageID")
VNM<-VNM[,,-match(c("1031","2031"),dimnames(VNM)[[3]])]
str(VNM)
VN <- abind (VNF, VNM)
cor(as.numeric(dimnames(VN)[[3]]),as.numeric(Lab_Vietnam$ID)) # Correct order... 

# Procrustes analyses:

# CMR_13
gp_cmr13 <- gpagen(CAM13,  ProcD = F, curves = slids, PrinAxes = F, Proj = TRUE); plot(gp_cmr13)

# Create geomorph data frames for nonsymetrized and symetrized data with varibales corresponding to several variables from the Lab table
gp_cmr13 <- geomorph.data.frame(gp_cmr13, sex = Lab_CMR_13$Sex, 
                              set = Lab_CMR_13$Culture_Year, nat = Lab_CMR_13$Nat)


# Subset landmark coordinates according to sex 
shapesex_CMR_13 <- coords.subset(gp_cmr13$coords, gp_cmr13$sex)


#
##
###
#####
###
##
#

# CMR_16
gp_cmr16 <- gpagen(CAM16,  ProcD = F, curves = slids, PrinAxes = F, Proj = TRUE); plot(gp_cmr16)

# Create geomorph data frame
gp_cmr16 <- geomorph.data.frame(gp_cmr16, sex = Lab_CMR_16$Sex, 
                                set = Lab_CMR_16$Culture_Year, nat = Lab_CMR_16$Nat)


# Subset landmark coordinates according to sex 
shapesex_CMR_16 <- coords.subset(gp_cmr16$coords, gp_cmr16$sex)


#
##
###
#####
###
##
#

# Colombia
gp_col <- gpagen(COL,  ProcD = F, curves = slids, PrinAxes = F, Proj = TRUE); plot(gp_col)

# Create geomorph data frame 
gp_col <- geomorph.data.frame(gp_col, sex = Lab_Colombia$Sex, 
                                set = Lab_Colombia$Culture_Year, nat = Lab_Colombia$Nat)


# Subset landmark coordinates according to sex 
shapesex_COL <- coords.subset(gp_col$coords, gp_col$sex)


#
##
###
#####
###
##
#

# Czech 16
gp_cz_16 <- gpagen(CZ16,  ProcD = F, curves = slids, PrinAxes = F, Proj = TRUE); plot(gp_cz_16)

# Create geomorph data frame
gp_cz_16 <- geomorph.data.frame(gp_cz_16, sex = Lab_Czech_16$Sex, 
                              set = Lab_Czech_16$Culture_Year, nat = Lab_Czech_16$Nat)


# Subset landmark coordinates according to sex 
shapesex_CZ16 <- coords.subset(gp_cz_16$coords, gp_cz_16$sex)



#
##
###
#####
###
##
#

# Czech 19
gp_cz_19 <- gpagen(CZ19,  ProcD = F, curves = slids, PrinAxes = F, Proj = TRUE); plot(gp_cz_19)

# Create geomorph data frame
gp_cz_19 <- geomorph.data.frame(gp_cz_19, sex = Lab_Czech_19$Sex, 
                                set = Lab_Czech_19$Culture_Year, nat = Lab_Czech_19$Nat)


# Subset landmark coordinates according to sex 
shapesex_CZ19 <- coords.subset(gp_cz_19$coords, gp_cz_19$sex)


##
###
#####
###
##
#

# India - CFD
gp_india <- gpagen(IND,  ProcD = F, curves = slids, PrinAxes = F, Proj = TRUE); plot(gp_india)

# Create geomorph data frame
gp_india <- geomorph.data.frame(gp_india, sex = Lab_India$Sex, 
                                set = Lab_India$Culture_Year, nat = Lab_India$Nat)


# Subset landmark coordinates according to sex 
shapesex_INDIA <- coords.subset(gp_india$coords, gp_india$sex)



#
##
###
#####
###
##
#

# Iran
gp_iran <- gpagen(IR,  ProcD = F, curves = slids, PrinAxes = F, Proj = TRUE); plot(gp_iran)

# Create geomorph data frame
gp_iran <- geomorph.data.frame(gp_iran, sex = Lab_Iran$Sex, 
                                set = Lab_Iran$Culture_Year, nat = Lab_Iran$Nat)


# Subset landmark coordinates according to sex 
shapesex_IRAN <- coords.subset(gp_iran$coords, gp_iran$sex)


#
##
###
#####
###
##
#

# Turkey
gp_turkey <- gpagen(TUR,  ProcD = F, curves = slids, PrinAxes = F, Proj = TRUE); plot(gp_turkey)

# Create geomorph data frame
gp_turkey <- geomorph.data.frame(gp_turkey, sex = Lab_Turkey$Sex, 
                               set = Lab_Turkey$Culture_Year, nat = Lab_Turkey$Nat)


# Subset landmark coordinates according to sex 
shapesex_TURKEY <- coords.subset(gp_turkey$coords, gp_turkey$sex)



#
##
###
#####
###
##
#

# Vietnam
gp_viet <- gpagen(VN,  ProcD = F, curves = slids, PrinAxes = F, Proj = TRUE); plot(gp_viet)

# Create geomorph data frame
gp_viet <- geomorph.data.frame(gp_viet, sex = Lab_Vietnam$Sex, 
                                 set = Lab_Vietnam$Culture_Year, nat = Lab_Vietnam$Nat)


# Subset landmark coordinates according to sex 
shapesex_VIETNAM <- coords.subset(gp_viet$coords, gp_viet$sex)



#
##
###
#####
###
##
#


#######################
# CALCULATE METRICS   #
#######################

#
##
###
#####
###
##
#

########################
#  1 Distinctiveness   #
########################


## Function for calculating distance from average

dist.f.mean <- function (x) {
  
  lokanf<-rep(0,dim (x) [3])
  for (i in 1:dim (x) [3])
    
  {
    puvfanf<-(x)[,,i]
    
    lokanf[i]<-(sum(rowSums((puvfanf-mshape(x))^2)))^0.5
  }
  
  lokanf}


### CMR 13
CMR13_F_AV_IR <- dist.f.mean (shapesex_CMR_13$F);CMR13_F_AV_IR
CMR13_M_AV_IR <- dist.f.mean (shapesex_CMR_13$M);CMR13_M_AV_IR

avrgCMR13 <- c(CMR13_F_AV_IR,  CMR13_M_AV_IR) # Local distinctiveness: FEMALES - CZ 13, CZ 16 COl, CMR 16, CMR 19, Ind, IR, Tr, VN | MALES - CZ 13, CZ 16 COl, CMR 16, CMR 19, Ind, IR, Tr, VN

Lab_CMR_13$LOCAL_DIST <- avrgCMR13


### CMR 16
CMR16_F_AV_IR <- dist.f.mean (shapesex_CMR_16$F);CMR16_F_AV_IR
CMR16_M_AV_IR <- dist.f.mean (shapesex_CMR_16$M);CMR16_M_AV_IR

avrgCMR16 <- c(CMR16_F_AV_IR,  CMR16_M_AV_IR) # Local distinctiveness:

Lab_CMR_16$LOCAL_DIST <- avrgCMR16



### Colombia
COL_F_AV_IR <- dist.f.mean (shapesex_COL$F);COL_F_AV_IR
COL_M_AV_IR <- dist.f.mean (shapesex_COL$M);COL_M_AV_IR

avrgCOL <- c(COL_F_AV_IR, COL_M_AV_IR) # Local distinctiveness

Lab_Colombia$LOCAL_DIST <- avrgCOL



### CZ 16
CZ16_F_AV_IR <- dist.f.mean (shapesex_CZ16$F);CZ16_F_AV_IR
CZ16_M_AV_IR <- dist.f.mean (shapesex_CZ16$M);CZ16_M_AV_IR

avrgCZ16 <- c(CZ16_F_AV_IR,  CZ16_M_AV_IR) # Local distinctiveness: 

Lab_Czech_16$LOCAL_DIST <- avrgCZ16



### CZ 19
CZ19_F_AV_IR <- dist.f.mean (shapesex_CZ19$F);CZ19_F_AV_IR
CZ19_M_AV_IR <- dist.f.mean (shapesex_CZ19$M);CZ19_M_AV_IR

avrgCZ19 <- c(CZ19_F_AV_IR,  CZ19_M_AV_IR) # Local distinctiveness: 

Lab_Czech_19$LOCAL_DIST <- avrgCZ19



### India - CFD
INDIA_F_AV_IR <- dist.f.mean (shapesex_INDIA$F);INDIA_F_AV_IR
INDIA_M_AV_IR <- dist.f.mean (shapesex_INDIA$M);INDIA_M_AV_IR

avrgINDIA <- c(INDIA_F_AV_IR,  INDIA_M_AV_IR) # Local distinctiveness: 

Lab_India$LOCAL_DIST <- avrgINDIA



### Iran 
IRAN_F_AV_IR <- dist.f.mean (shapesex_IRAN$F);IRAN_F_AV_IR
IRAN_M_AV_IR <- dist.f.mean (shapesex_IRAN$M);IRAN_M_AV_IR

avrgIRAN <- c(IRAN_F_AV_IR,  IRAN_M_AV_IR) # Local distinctiveness: 

Lab_Iran$LOCAL_DIST <- avrgIRAN




### Turkey
TURK_F_AV_IR <- dist.f.mean (shapesex_TURKEY$F);TURK_F_AV_IR
TURK_M_AV_IR <- dist.f.mean (shapesex_TURKEY$M);TURK_M_AV_IR

avrgTURK <- c(TURK_F_AV_IR,  TURK_M_AV_IR) # Local distinctiveness: 

Lab_Turkey$LOCAL_DIST <- avrgTURK




### Vietnam
VIET_F_AV_IR <- dist.f.mean (shapesex_VIETNAM$F);VIET_F_AV_IR
VIET_M_AV_IR <- dist.f.mean (shapesex_VIETNAM$M);VIET_M_AV_IR

avrgVIET <- c(VIET_F_AV_IR,  VIET_M_AV_IR) # Local distinctiveness: 

Lab_Vietnam$LOCAL_DIST <- avrgVIET


##############################
# 2 SEXUAL SHAPE DIMORPHISM  #
##############################

# CMR 13
CMR13gdf <- geomorph.data.frame(coords = gp_cmr13$coords, sex = Lab_CMR_13$Sex)
reg1<-procD.lm(coords ~ sex, iter = 9999, data = CMR13gdf); summary(reg1)

coefficients<-coef(reg1, test = F)
sex.vecs<-coefficients[2,]

scores <- two.d.array (gp_cmr13$coords) %*% t(coefficients)
sc <- scores [,2]

fsc <- sc [which(Lab_CMR_13$Sex=="F")];fsc
msc <- sc [which(Lab_CMR_13$Sex=="M")];msc

fsc_cmr13 <- sc [which(Lab_CMR_13$Sex=="F")]
msc_cmr13 <- sc [which(Lab_CMR_13$Sex=="M")]

## The nation specific scores concatenated into vectors of all sex scores 
fsexscores1 <- c(fsc)
msexscores1 <- c(msc)
sexscores1 <- c(fsexscores1,msexscores1)
Sextypicality <- scale(c(-1*fsexscores1, msexscores1)) # Females are minus one: This is to make sex-typicality from SShD (sexual shape dimorphism)
Sextypicality[,1]

Lab_CMR_13$SexTypMeas <- Sextypicality[,1]




# CMR 16

onecoords<-coords.subset(gp_cmr16$coords, gp_cmr16$set) # Right order

### 
CMR16gdf <- geomorph.data.frame(coords = onecoords$Cameroon_2016, sex = Lab_CMR_16$Sex)
reg1<-procD.lm(coords ~ sex, iter = 9999, data = CMR16gdf); summary(reg1)

coefficients<-coef(reg1, test = F)
sex.vecs<-coefficients[2,]

scores <- two.d.array (onecoords$Cameroon_2016) %*% t(coefficients)
sc <- scores [,2]

fsc <- sc [which(Lab_CMR_16$Sex=="F")];fsc
msc <- sc [which(Lab_CMR_16$Sex=="M")];msc

fsc_cmr16 <- sc [which(Lab_CMR_16$Sex=="F")]
msc_cmr16 <- sc [which(Lab_CMR_16$Sex=="M")]

## The nation specific scores concatenated into vectors of all sex scores 
Sextypicality <- scale(c(-1*fsc, msc)) # Females are minus one.
Sextypicality[,1]

Lab_CMR_16$SexTypMeas <- Sextypicality[,1]




# Colombia

onecoords<-coords.subset(gp_col$coords, gp_col$set) # Right order

### 
COLgdf <- geomorph.data.frame(coords = onecoords$Colombia, sex = Lab_Colombia$Sex)
reg1<-procD.lm(coords ~ sex, iter = 9999, data = COLgdf); summary(reg1)

coefficients<-coef(reg1, test = F)
sex.vecs<-coefficients[2,]

scores <- two.d.array (onecoords$Colombia) %*% t(coefficients)
sc <- scores [,2]

fsc <- sc [which(Lab_Colombia$Sex=="F")];fsc
msc <- sc [which(Lab_Colombia$Sex=="M")];msc

fsc_col <- sc [which(Lab_Colombia$Sex=="F")]
msc_col <- sc [which(Lab_Colombia$Sex=="M")]

## The nation specific scores concatenated into vectors of all sex scores 
Sextypicality <- scale(c(-1*fsc, msc)) # Females are minus one...
Sextypicality[,1]

Lab_Colombia$SexTypMeas <- Sextypicality[,1]





# Czech 16

onecoords<-coords.subset(gp_cz_16$coords, gp_cz_16$set) # Right order

### 
CZ16gdf <- geomorph.data.frame(coords = onecoords$Czech_2016, sex = Lab_Czech_16$Sex)
reg1<-procD.lm(coords ~ sex, iter = 9999, data = CZ16gdf); summary(reg1)

coefficients<-coef(reg1, test = F)
sex.vecs<-coefficients[2,]

scores <- two.d.array (onecoords$Czech_2016) %*% t(coefficients)
sc <- scores [,2]

fsc <- sc [which(Lab_Czech_16$Sex=="F")];fsc
msc <- sc [which(Lab_Czech_16$Sex=="M")];msc

fsc_cz16 <- sc [which(Lab_Czech_16$Sex=="F")]
msc_cz16 <- sc [which(Lab_Czech_16$Sex=="M")]


## The nation specific scores concatenated into vectors of all sex scores 
Sextypicality <- scale(c(-1*fsc, msc)) # Females are minus one...
Sextypicality[,1]

Lab_Czech_16$SexTypMeas <- Sextypicality[,1]




# Czech 19

onecoords<-coords.subset(gp_cz_19$coords, gp_cz_19$set) # Right order

### 
CZ19gdf <- geomorph.data.frame(coords = onecoords$Czech_2019, sex = Lab_Czech_19$Sex)
reg1<-procD.lm(coords ~ sex, iter = 9999, data = CZ19gdf); summary(reg1)

coefficients<-coef(reg1, test = F)
sex.vecs<-coefficients[2,]

scores <- two.d.array (onecoords$Czech_2019) %*% t(coefficients)
sc <- scores [,2]

fsc <- sc [which(Lab_Czech_19$Sex=="F")];fsc
msc <- sc [which(Lab_Czech_19$Sex=="M")];msc

fsc_cz19 <- sc [which(Lab_Czech_19$Sex=="F")]
msc_cz19 <- sc [which(Lab_Czech_19$Sex=="M")]

## The nation specific scores concatenated into vectors of all sex scores 
Sextypicality <- scale(c(-1*fsc, msc)) # Females are minus one...
Sextypicality[,1]

Lab_Czech_19$SexTypMeas <- Sextypicality[,1]





# India CFD

onecoords<-coords.subset(gp_india$coords, gp_india$set) # Right order

### 
INDIAgdf <- geomorph.data.frame(coords = onecoords$India, sex = Lab_India$Sex)
reg1<-procD.lm(coords ~ sex, iter = 9999, data = INDIAgdf); summary(reg1)

coefficients<-coef(reg1, test = F)
sex.vecs<-coefficients[2,]

scores <- two.d.array (onecoords$India) %*% t(coefficients)
sc <- scores [,2]

fsc <- sc [which(Lab_India$Sex=="F")];fsc
msc <- sc [which(Lab_India$Sex=="M")];msc

fsc_india <- sc [which(Lab_India$Sex=="F")]
msc_india <- sc [which(Lab_India$Sex=="M")]

## The nation specific scores concatenated into vectors of all sex scores 
Sextypicality <- scale(c(-1*fsc, msc)) # Females are minus one...
Sextypicality[,1]

Lab_India$SexTypMeas <- Sextypicality[,1]




# Iran

onecoords<-coords.subset(gp_iran$coords, gp_iran$set) # Right order

### 
IRANgdf <- geomorph.data.frame(coords = onecoords$Iran, sex = Lab_Iran$Sex)
reg1<-procD.lm(coords ~ sex, iter = 9999, data = IRANgdf); summary(reg1)

coefficients<-coef(reg1, test = F)
sex.vecs<-coefficients[2,]

scores <- two.d.array (onecoords$Iran) %*% t(coefficients)
sc <- scores [,2]

fsc <- sc [which(Lab_Iran$Sex=="F")];fsc
msc <- sc [which(Lab_Iran$Sex=="M")];msc

fsc_iran <- sc [which(Lab_Iran$Sex=="F")]
msc_iran <- sc [which(Lab_Iran$Sex=="M")]

## The nation specific scores concatenated into vectors of all sex scores 
Sextypicality <- scale(c(-1*fsc, msc)) # Females are minus one...
Sextypicality[,1]

Lab_Iran$SexTypMeas <- Sextypicality[,1]




# Turkey

onecoords<-coords.subset(gp_turkey$coords, gp_turkey$set) # Right order

### 
TURgdf <- geomorph.data.frame(coords = onecoords$Turkey, sex = Lab_Turkey$Sex)
reg1<-procD.lm(coords ~ sex, iter = 9999, data = TURgdf); summary(reg1)

coefficients<-coef(reg1, test = F)
sex.vecs<-coefficients[2,]

scores <- two.d.array (onecoords$Turkey) %*% t(coefficients)
sc <- scores [,2]

fsc <- sc [which(Lab_Turkey$Sex=="F")];fsc
msc <- sc [which(Lab_Turkey$Sex=="M")];msc

fsc_turkey <- sc [which(Lab_Turkey$Sex=="F")]
msc_turkey <- sc [which(Lab_Turkey$Sex=="M")]

## The nation specific scores concatenated into vectors of all sex scores 
Sextypicality <- scale(c(-1*fsc, msc)) # Females are minus one...
Sextypicality[,1]

Lab_Turkey$SexTypMeas <- Sextypicality[,1]




# Vietnam

onecoords<-coords.subset(gp_viet$coords, gp_viet$set) # Right order

### 
VIETgdf <- geomorph.data.frame(coords = onecoords$Vietnam, sex = Lab_Vietnam$Sex)
reg1<-procD.lm(coords ~ sex, iter = 9999, data = VIETgdf); summary(reg1)

coefficients<-coef(reg1, test = F)
sex.vecs<-coefficients[2,]

scores <- two.d.array (onecoords$Vietnam) %*% t(coefficients)
sc <- scores [,2]

fsc <- sc [which(Lab_Vietnam$Sex=="F")];fsc
msc <- sc [which(Lab_Vietnam$Sex=="M")];msc

fsc_vn <- sc [which(Lab_Vietnam$Sex=="F")]
msc_vn <- sc [which(Lab_Vietnam$Sex=="M")]

## The nation specific scores concatenated into vectors of all sex scores 
Sextypicality <- scale(c(-1*fsc, msc)) # Females are minus one...
Sextypicality[,1]

Lab_Vietnam$SexTypMeas <- Sextypicality[,1]


fsc <- c(fsc_cmr13,fsc_cmr16,fsc_col,fsc_cz16,fsc_cz19,fsc_india,fsc_iran,fsc_turkey,fsc_vn) 
msc <- c(msc_cmr13,msc_cmr16,msc_col,msc_cz16,msc_cz19,msc_india,msc_iran,msc_turkey,msc_vn)

sexscores1 <- c(fsc,msc)

#Check this later, each set of data is arranged differently
library(vioplot)
vioplot(sexscores1~Lab_eye$Sex, range = 1.5, horizontal = F, col = "gold", plotCentre = "point")
vioplot(sexscores1~Lab_eye$Culture_Year, range = 1.5, horizontal = F, col = "gold", plotCentre = "point")

## Visualization of both sexes malenes-femaleness distributions
vioplot(fsc~Lab_eye$Culture_Year[Lab_eye$Sex=="F"], range = 1.5, horizontal = F, col = "#FF770080", plotCentre = "point", ylim=c(-0.004, 0.004), xlab="Country",ylab="SShD")
vioplot(msc~Lab_eye$Culture_Year[Lab_eye$Sex=="M"], range = 1.5, horizontal = F, col = "#3366FF80", plotCentre = "point",add=T,xlab="",ylab="")
vioplot(sexscores1~Lab_eye$Culture_Year, range = 1.5, horizontal = F, col = "#FFFFFF00", lineCol = NA,add=T,xlab="",ylab="",border="#FF0066",lwd=1.8,rectCol=NA,pchMed=NA)
title (main = "Sexual Shape Dimorphism")

# Testing correspondence with previous calculations (where available). 

Karl<-read.table("data.clean.txt",T)
levels(as.factor((Karl$set)))
# "AVN" = Asian Vietnamese, YES  
# "BRAZ" = Brazilians = NO  
# "CMR12" = Cameroonians, 2012 = NO
# "CMR13" = Cameroonians, 2013 = YES, but some will be missing
# "CMR16" = Cameroonians, 2016 = YES, but some will be missing
# "COL" = Colombians = YES, but some may be missing
# "CZ120" = NO 
# "CZ16" = YES
# "NAM" = NO
# "RO" = NO
# "TUR" = YES, but some may be missing   
# "UK" = N0

Karl$set <- as.character(Karl$set)

Karl$set[is.na(Karl$set)]<- "India"

levels(as.factor((Karl$set)))

Karl_VN <- Karl[Karl$set=="AVN",]
View(data.frame(Karl_VN$name,Lab_Vietnam$ID))

Karl_CMR13 <- Karl[Karl$set=="CMR13",]
View(data.frame(Karl_CMR13$name,Lab_CMR_13$ID))

Karl_CMR16 <- Karl[Karl$set=="CMR16",]
Karl_CMR16 <- Karl_CMR16[Karl_CMR16$name %in% Lab_CMR_16$ID,]
View(data.frame(Karl_CMR16$name,Lab_CMR_16$ID))

Karl_COL <- Karl[Karl$set=="COL",]
View(data.frame(Karl_COL$name,Lab_Colombia$ID))

Karl_CZ16 <- Karl[Karl$set=="CZ16",]
View(data.frame(Karl_CZ16$name,Lab_Czech_16$ID))

Karl_INDIA <- Karl[Karl$set=="India",]
View(data.frame(Karl_INDIA$name,Lab_India$ID))

Karl_TUR <- Karl[Karl$set=="TUR",]
Karl_TUR <- Karl_TUR[Karl_TUR$name %in% Lab_Turkey$ID,]
View(data.frame(Karl_TUR$name,Lab_Turkey$ID))

# Distinctiveness: 

cor.test(Karl_VN$Distinctiveness, Lab_Vietnam$LOCAL_DIST) # 0.999

cor.test(Karl_CMR13$Distinctiveness, Lab_CMR_13$LOCAL_DIST) # 0.94

cor.test(Karl_CMR16$Distinctiveness, Lab_CMR_16$LOCAL_DIST) # 0.93

cor.test(Karl_COL$Distinctiveness, Lab_Colombia$LOCAL_DIST) # 0.999

cor.test(Karl_CZ16$Distinctiveness, Lab_Czech_16$LOCAL_DIST) # 0.87

cor.test(Karl_TUR$Distinctiveness, Lab_Turkey$LOCAL_DIST) # 0.98

# Sextypicality: 

cor(Karl_VN$Sextypicality, Lab_Vietnam$SexTypMeas) # 0.993

cor(Karl_CMR13$Sextypicality, Lab_CMR_13$SexTypMeas) # 0.36 (different orientation of the vectors, otherwise OK, according to a co-author)

cor(Karl_CMR16$Sextypicality, Lab_CMR_16$SexTypMeas) # 0.63

cor(Karl_COL$Sextypicality, Lab_Colombia$SexTypMeas) # 0.68

cor(Karl_CZ16$Sextypicality, Lab_Czech_16$SexTypMeas) # 0.86

cor(Karl_TUR$Sextypicality, Lab_Turkey$SexTypMeas) # 0.92



############################
#  Asymmetry #
############################

# All landmarks into a single file: 

# Combine all the data
one <- abind (CAM13, CAM16, COL, CZ16, CZ19, IND, IR, TUR, VN)

# Divide the data to two sets according to the sex
fdat <- subset(Lab_eye, Sex=="F")
mdat <- subset(Lab_eye, Sex=="M")

# Connect these data frames to the final data 
dat<- rbind(fdat, mdat)

# Procrustes analysis of the full dataset
gpone<-gpagen(one,  ProcD = F, curves = slids, PrinAxes = F, Proj = TRUE); plot(gpone)


# Create geomorph data frames for non- and - symetrized data with varibales corresponding to several variables from symdat
onegdf <- geomorph.data.frame(gpone, sex = dat$Sex, set = dat$Culture_Year)

# Subset landmark coordinates according to sex 
shapesex<- coords.subset(onegdf$coords, onegdf$sex)

# Create array ordered by sex from the shapesex data frame
shape <- abind (shapesex$F, shapesex$M)

# Generalized procrustes analysis of the resulting array
gpsh<-gpagen(shape,  ProcD = F, curves = NULL, PrinAxes = F, Proj = TRUE)
plot(gpsh)


plot(gpagen(gpsh$coords, PrinAxes = F))
X<-as.data.frame(two.d.array(gpsh$coords))
Xr <- X*matrix (rep(c(-1,1), each=nrow(X),times=ncol(X)/2), nrow = nrow(X), ncol = ncol (X))

Xr <- as.data.frame (Xr)
names(Xr)
coor<-Xr

# extract numbers from column names
cisla <- as.numeric(unlist(regmatches(names(coor), gregexpr("[[:digit:]]+", names(coor)))))
cisla2 <- cisla # copy

# re-label
left<-c(50,51,52,53,54,55,56,57,58,5,7,12,10,70,15,17,18,20,26,23,21,22,21,25,24,71,35,46,45,44,34,49,48,47)
right<-c(59,60,61,62,63,64,65,66,67,4,6,11,9,69,14,16,19,27,33,29,28,30,28,32,31,72,37,38,39,40,36,41,42,43)

# extract numbers from column names
cisla <- as.numeric(unlist(regmatches(names(coor), gregexpr("[[:digit:]]+", names(coor)))))
cisla2 <- cisla # copy

# exchange
pozice <- match(cisla,left)[-which(is.na(match(cisla,left)))]
pozice2 <- match(cisla,right)[-which(is.na(match(cisla,right)))]

cisla[-which(is.na(match(cisla,left)))] <- right[pozice]
cisla[-which(is.na(match(cisla2,right)))] <- left[pozice2]
cisla

# ordering
poradi <- order(cisla)

sloupec <- paste(cisla,c(".X",".Y"),sep="")
names(coor) <- sloupec
coor<-coor[,poradi]

# Plot original and reflected configurations

plot(gpagen(arrayspecs(coor, 72, 2), PrinAxes = F))
plot(gpagen(arrayspecs(X, 72, 2), PrinAxes = F))

# join and average original and reflected confidurations and plot the result
Xsym <- abind (arrayspecs(X, 72, 2), arrayspecs(coor, 72, 2))

plot(gpagen(Xsym, PrinAxes = F))

XSYM <-(Xsym [1:72, 1:2, 1:length(dat$Culture_Year)] + Xsym [1:72, 1:2, (1+length(dat$Culture_Year)):(length(dat$Culture_Year)*2)])/2
# Here sex seems irrelevant! 

plot(gpagen(XSYM, PrinAxes = F))



# calculating the scores of asymmetry (= the Euclidean distance between original and mirrored configurations)

symscore <-rep (0,length(dat$Culture_Year))
for (i in 1:length(dat$Culture_Year))
{
  origos <- (X) [i,]
  reflect <- (coor) [i,]
  
  symscore [i]<-sqrt(sum((origos-reflect)^2))
}

hist(symscore)

# male & female symmetry scores altogether and for each population

fsymscore <- symscore [dat$Sex == "F"]
msymscore <- symscore [dat$Sex == "M"]

cmrFS13 <- fsymscore [fdat$Culture_Year == "Cameroon_2013"]
cmrMS13 <- msymscore [mdat$Culture_Year == "Cameroon_2013"]

cmrFS16 <- fsymscore [fdat$Culture_Year == "Cameroon_2016"]
cmrMS16 <- msymscore [mdat$Culture_Year == "Cameroon_2016"]

colFS <- fsymscore [fdat$Culture_Year == "Colombia"]
colMS <- msymscore [mdat$Culture_Year == "Colombia"]

czFS16 <- fsymscore [fdat$Culture_Year == "Czech_2016"]
czMS16 <- msymscore [mdat$Culture_Year == "Czech_2016"]

czFS19 <- fsymscore [fdat$Culture_Year == "Czech_2019"]
czMS19 <- msymscore [mdat$Culture_Year == "Czech_2019"]

inFS <- fsymscore [fdat$Culture_Year == "India_FCD"]
inMS <- msymscore [mdat$Culture_Year == "India_FCD"]

irFS <- fsymscore [fdat$Culture_Year == "Iran"]
irMS <- msymscore [mdat$Culture_Year == "Iran"]

trFS <- fsymscore [fdat$Culture_Year == "Turkey"]
trMS <- msymscore [mdat$Culture_Year == "Turkey"]

viFS <- fsymscore [fdat$Culture_Year == "Vietnam"]
viMS <- msymscore [mdat$Culture_Year == "Vietnam"]

Lab_CMR_13$Asym <- c(cmrFS13,cmrMS13)
Lab_CMR_16$Asym <- c(cmrFS16,cmrMS16)

Lab_Colombia$Asym <- c(colFS,colMS)

Lab_Czech_16$Asym <- c(czFS16,czMS16)
Lab_Czech_19$Asym <- c(czFS19,czMS19)

Lab_India$Asym <- c(inFS,inMS)
Lab_Iran$Asym <- c(irFS,irMS)

Lab_Turkey$Asym <- c(trFS,trMS)

Lab_Vietnam$Asym <- c(viFS,viMS)

# Create the data frame: 
Lab_eye_fin <- rbind.data.frame(Lab_CMR_13,Lab_CMR_16, Lab_Colombia, Lab_Czech_16,
                                Lab_Czech_19, Lab_India, Lab_Iran, Lab_Turkey, Lab_Vietnam)

# save: 
write.csv2(Lab_eye_fin, file="Data_03_08_24.csv", row.names=F)

# NOTES: 
# 1) Morphometric measures were calculated separately for each population. 
# That enables us to run the analyses without Iranians simply by dropping them from the database. 

# 2) Caculating GMM de novo was not necessary, but we used it as a double-check that our previously 
# obtained results are robust.

# 3) For the same reason, GMM variables were included in the analysis, altough it mainly concerns 
# skin tone and iris and peri-iridal tissues' colouration. 