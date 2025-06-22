folder0<-getwd()

d$Nat
d.s$Culture_Year

source("01_SEXT_data_preparation.R")

# Nefakčí 
# setwd(file.path(folder0,"ind_ratings"))
list.files()

# Funkce
trlong<-function(set,Culture_Year="",tsex=""){
  rating=unlist(set[,2:ncol(set)]) # rating - škála je tomu buřt
  out<-data.frame(
    Culture_Year=rep(Culture_Year,length(rating)),
    sex=rep(tsex,length(rating)),
    target=rep(names(set)[2:ncol(set)],each=nrow(set)),
    rater=rep(c(set[,1]),times=ncol(set)-1),
    rating=rating)
  rownames(out)<-NULL
  return(out)
}


#--------------------------------------
# 
# Part 1: Attractiveness (mind that you need different long data things for attractiveness and sextypicality)
#
#--------------------------------------

# Original levels c("Cameroon_2013","Cameroon_2016","Colombia","Czech_2016","Czech_2019","India_FCD","Iran","Turkey")

# A já si to s dovolením natáhnu všechno najednou: 

# Cameroon
cam13_F <- read.csv2("CMR13F_fem.csv")
cam13_M <- read.csv2("CMR13M_masc.csv")

cam16_F <- read.csv2("CMR16F_fem.csv")
cam16_M <- read.csv2("CMR16M_masc.csv")

# Colombia
col_F <- read.csv2("Col_F_fem.csv")
col_M <- read.csv2("Col_M_masc.csv")

# Czechia
cz16_F <- read.csv2("CZ_16_Fem_women.csv")
cz16_M <- read.csv2("CZ_16_Masc_men.csv")

cz19_F <- read.csv2("CZ_19_Fem_women.csv")
cz19_M <- read.csv2("CZ_19_Masc_men.csv")

# India - NA! 

# Iran
ir_F <- read.csv2("Iran_F_fem.csv")
ir_M <- read.csv2("Iran_M_Masc.csv")

# Turkey
tur_F <- read.csv2("Tur_F_Fem.csv")
tur_M <- read.csv2("Tur_M_Masc.csv")

# Vietnam
# NA vn_F <- read.delim("Vietnam_females_attr_rated_by_VT_males_ED.txt")
# NA vn_M <- read.delim("Vietnam_males_attr_rated_by_VT_females_ED.txt")

dl<-rbind(
  trlong(cam13_F,Culture_Year="Cameroon_2013",tsex="F"), 
  trlong(cam13_M,Culture_Year="Cameroon_2013",tsex="M"),
  
  trlong(cam16_F,Culture_Year="Cameroon_2016",tsex="F"),
  trlong(cam16_M,Culture_Year="Cameroon_2016",tsex="M"),
  
  trlong(col_F,Culture_Year="Colombia",tsex="F"),
  trlong(col_M,Culture_Year="Colombia",tsex="M"),
  
  trlong(cz16_F,Culture_Year="Czech_2016",tsex="F"),
  trlong(cz16_M,Culture_Year="Czech_2016",tsex="M"),
  
  trlong(cz19_F,Culture_Year="Czech_2019",tsex="F"),
  trlong(cz19_M,Culture_Year="Czech_2019",tsex="M"),
  
  trlong(ir_F,Culture_Year="Iran",tsex="F"),
  trlong(ir_M,Culture_Year="Iran",tsex="M"),
  
  trlong(tur_F,Culture_Year="Turkey",tsex="F"),
  trlong(tur_M,Culture_Year="Turkey",tsex="M")
)

# All the Stimuli IDs that start with X must be corrected...

dl$target <- sub("^X", "", dl$target)

# ID21_2_M_CMR13_DSC_0019.2 is just "ID21_2_M_CMR13_DSC_0019"
dl[dl$target=="ID21_2_M_CMR13_DSC_0019",]

dl[is.na(dl$Asymmetry),]


# Morpho
dl$SexTypMeas<-d.s$SexTypMeas[match(paste(dl$Culture_Year,dl$sex,dl$target),
                                          paste(d$Culture_Year,d$Sex,d$ID))]

dl$Asymmetry<-d.s$Asymmetry[match(paste(dl$Culture_Year,dl$sex,dl$target),
                                    paste(d$Culture_Year,d$Sex,d$ID))]

dl$Distinctiveness<-d.s$Distinctiveness[match(paste(dl$Culture_Year,dl$sex,dl$target),
                                  paste(d$Culture_Year,d$Sex,d$ID))]

# Age 
dl$Age<-d.s$Age[match(paste(dl$Culture_Year,dl$sex,dl$target),
                                              paste(d$Culture_Year,d$Sex,d$ID))]

# Skin L*
dl$L_skin <- d.s$L_skin[match(paste(dl$Culture_Year,dl$sex,dl$target),
                      paste(d$Culture_Year,d$Sex,d$ID))]

# Iris L*a*b*
dl$L_iris <- d.s$L_iris[match(paste(dl$Culture_Year,dl$sex,dl$target),
                              paste(d$Culture_Year,d$Sex,d$ID))]

dl$a_iris <- d.s$a_iris[match(paste(dl$Culture_Year,dl$sex,dl$target),
                              paste(d$Culture_Year,d$Sex,d$ID))]

dl$b_iris <- d.s$b_iris[match(paste(dl$Culture_Year,dl$sex,dl$target),
                              paste(d$Culture_Year,d$Sex,d$ID))]

# Sclera L*a*b*
dl$L_sclera <- d.s$L_sclera[match(paste(dl$Culture_Year,dl$sex,dl$target),
                              paste(d$Culture_Year,d$Sex,d$ID))]

dl$a_sclera <- d.s$a_sclera[match(paste(dl$Culture_Year,dl$sex,dl$target),
                              paste(d$Culture_Year,d$Sex,d$ID))]

dl$b_sclera <- d.s$b_sclera[match(paste(dl$Culture_Year,dl$sex,dl$target),
                              paste(d$Culture_Year,d$Sex,d$ID))]

summary(as.factor(dl$Culture_Year[!is.na(dl$rating)]))
summary(as.factor(dl$Culture_Year[is.na(dl$rating)])) # NAs - exclude...

setwd(folder0)
