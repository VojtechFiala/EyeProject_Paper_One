
source("01_ATTR_data_preparation.R")

list.files()

# Function
trlong<-function(set,Culture_Year="",tsex=""){
  rating=unlist(set[,2:ncol(set)])
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

# Original levels c("Cameroon_2013","Cameroon_2016","Colombia","Czech_2016","Czech_2019","India_FCD","Iran","Turkey","Vietnam")

# Cameroon
cam13_F <- read.delim("Cameroon_2013_females_attr_rated_by_CMR_males_ED.txt")
cam13_M <- read.delim("Cameroon_2013_males_attr_rated_by_CMR_females_ED.txt")

cam16_F <- read.delim("Cameroon_2016_females_attr_rated_by_CMR_males_ED.txt")
cam16_M <- read.delim("Cameroon_2016_males_attr_rated_by_CMR_females_ED.txt")

# Colombia
col_F <- read.csv2("Col_F_attr.csv")
col_M <- read.csv2("Col_M_attr.csv")

# Czechia
cz16_F <- read.delim("Czech_2016_females_attr_rated_by_CZ_males_ED.txt")
cz16_M <- read.delim("Czech_2016_males_attr_rated_by_CZ_females_ED.txt")

cz19_F <- read.csv2("CZ_19_Attr_women.csv")
cz19_M <- read.csv2("CZ_19_Attr_men.csv")

# India - NA! 

# Iran
ir_F <- read.csv2("Iran_F_Attr.csv")
ir_M <- read.csv2("Iran_M_Attr.csv")

# Turkey
tur_F <- read.delim("Turkey_BFD_females_attr_rated_by_TUR_bothsex_ED.txt")
tur_M <- read.delim("Turkey_BFD_males_attr_rated_by_TUR_bothsex_ED.txt")

# Vietnam
vn_F <- read.delim("Vietnam_females_attr_rated_by_VT_males_ED.txt")
vn_M <- read.delim("Vietnam_males_attr_rated_by_VT_females_ED.txt")

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
  trlong(tur_M,Culture_Year="Turkey",tsex="M"),
  
  trlong(vn_F,Culture_Year="Vietnam",tsex="F"),
  trlong(vn_M,Culture_Year="Vietnam",tsex="M")
)

# All the Stimuli IDs that start with X must be corrected...

dl$target <- sub("^X", "", dl$target)

# ID21_2_M_CMR13_DSC_0019.2 is just "ID21_2_M_CMR13_DSC_0019"
dl[dl$target=="ID21_2_M_CMR13_DSC_0019.2",]
dl$target[dl$target=="ID21_2_M_CMR13_DSC_0019.2"] <- "ID21_2_M_CMR13_DSC_0019"
dl[dl$target=="ID21_2_M_CMR13_DSC_0019.2",]

dl[is.na(dl$Asymmetry),]


# Morphometrics
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


summary(as.factor(dl$nat[!is.na(dl$Sextypicality)]))
summary(as.factor(dl$nat[is.na(dl$Sextypicality)]))
