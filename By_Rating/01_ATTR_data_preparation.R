#Load the data
d<-read.csv2("Data_03_08_24.csv",header=T,stringsAsFactors = F)

#Row labels for summarization tabs
labtab<-c("Attractiveness","Sextypicality","Asymmetry","Distinctiveness","SShD","Age",
          "L_skin","L_iris","a_iris","b_iris","L_sclera","a_sclera","b_sclera")

# Means by groups...
meantab<-rbind(
  tapply(d$Attr,d$Group,mean,na.rm=T),
  tapply(d$SexT,d$Group,mean,na.rm=T),
  tapply(d$Asym,d$Group,mean,na.rm=T),
  tapply(d$LOCAL_DIST,d$Group,mean,na.rm=T),
  tapply(d$SexTypMeas,d$Group,mean,na.rm=T),
  tapply(d$Age,d$Group,mean,na.rm=T),
  tapply(d$L_skin,d$Group,mean,na.rm=T),
  tapply(d$L_iris,d$Group,mean,na.rm=T),
  tapply(d$a_iris,d$Group,mean,na.rm=T),
  tapply(d$b_iris,d$Group,mean,na.rm=T),
  tapply(d$L_sclera,d$Group,mean,na.rm=T),
  tapply(d$a_sclera,d$Group,mean,na.rm=T),
  tapply(d$b_sclera,d$Group,mean,na.rm=T))

# SDs acc. to groups
sdtab<-rbind(
  tapply(d$Attr,d$Group,sd,na.rm=T),
  tapply(d$SexT,d$Group,sd,na.rm=T),
  tapply(d$Asym,d$Group,sd,na.rm=T),
  tapply(d$LOCAL_DIST,d$Group,sd,na.rm=T),
  tapply(d$SexTypMeas,d$Group,sd,na.rm=T),
  tapply(d$Age,d$Group,sd,na.rm=T),
  tapply(d$L_skin,d$Group,sd,na.rm=T),
  tapply(d$L_iris,d$Group,sd,na.rm=T),
  tapply(d$a_iris,d$Group,sd,na.rm=T),
  tapply(d$b_iris,d$Group,sd,na.rm=T),
  tapply(d$L_sclera,d$Group,sd,na.rm=T),
  tapply(d$a_sclera,d$Group,sd,na.rm=T),
  tapply(d$b_sclera,d$Group,sd,na.rm=T))


rownames(meantab)<-labtab
rownames(sdtab)<-labtab

meantab
sdtab

#Indian sample lacks information about body height and weight
write.table(meantab,"meantab.txt",col.names=NA,sep="\t")
write.table(sdtab,"sdtab.txt",col.names=NA,sep="\t")

restab<-as.data.frame(t(matrix(paste(format(round(meantab,2),nsmall=2)," (",format(round(sdtab,2),nsmall=2),")",sep=""),ncol=ncol(meantab))))
restab<-cbind(nat=sapply(strsplit(colnames(meantab)," "),function(x)x[1]),
              sex=sapply(strsplit(colnames(meantab)," "),function(x)x[2]),
              restab)

names(restab)[3:15]<-rownames(meantab)
write.table(restab,"restab.txt",row.names=F,sep="\t")

# labtab <- c("Attractiveness","Sextypicality","Asymmetry","Distinctiveness","SShD","Age",
# "L_skin","L_iris","a_iris","b_iris","L_sclera","a_sclera","b_sclera")
d.s<-data.frame(
  d[,c(3,2,1,14,15)],
  Attractiveness=scale(d$Attr),
  Sextypicality=scale(d$SexT),
  Asymmetry=scale(d$Asym),
  Distinctiveness=scale(d$LOCAL_DIST),
  SexTypMeas=scale(d$SexTypMeas),
  Age=scale(d$Age),
  
  L_skin = scale(d$L_skin),
  L_iris = scale(d$L_iris),
  a_iris = scale(d$a_iris),
  b_iris = scale(d$b_iris),
  L_sclera = scale(d$L_sclera),
  a_sclera = scale(d$a_sclera),
  b_sclera = scale(d$b_sclera)
  )

chars<-c("Attractiveness","Sextypicality","Asymmetry","Distinctiveness","SShD","Age",
          "L_skin","L_iris","a_iris","b_iris","L_sclera","a_sclera","b_sclera")

ALLmeans <- c(mean(d$Attr,na.rm=T),
              mean(d$SexT,na.rm=T),
              mean(d$Asym,na.rm=T),
              mean(d$LOCAL_DIST,na.rm=T),
              mean(d$SexTypMeas,na.rm=T),
              mean(d$Age,na.rm=T),
              mean(d$L_skin,na.rm=T),
              mean(d$L_iris,na.rm=T),
              mean(d$a_iris,na.rm=T),
              mean(d$b_iris,na.rm=T),
              mean(d$L_sclera,na.rm=T),
              mean(d$a_sclera,na.rm=T),
              mean(d$b_sclera,na.rm=T)
)

ALLsds <- c(sd(d$Attr,na.rm=T),
            sd(d$SexT,na.rm=T),
            sd(d$Asym,na.rm=T),
            sd(d$LOCAL_DIST,na.rm=T),
            sd(d$SexTypMeas,na.rm=T),
            sd(d$Age,na.rm=T),
            sd(d$L_skin,na.rm=T),
            sd(d$L_iris,na.rm=T),
            sd(d$a_iris,na.rm=T),
            sd(d$b_iris,na.rm=T),
            sd(d$L_sclera,na.rm=T),
            sd(d$a_sclera,na.rm=T),
            sd(d$b_sclera,na.rm=T)
)


names(ALLmeans)<-chars
names(ALLsds)<-chars

#Personalized scale function
scaleP<-function(x,char,m=ALLmeans,s=ALLsds){(x-m[which(names(m)==char)])/s[which(names(s)==char)]}
cor(scale(d$Attr)[1:1034],scaleP(d$Attr,"Attractiveness")[1:1034]) # Je to to samý, akorát Petrovo verze: 
# scale(d$Attractiveness)==scaleP(d$Attractiveness,"Attractiveness")
# Z nějakýho důvodu nefakčí...

#Create a function that allows to easily reconstruct the original values 
# (useful for predictions on the original scale)
descaleP<-function(x,char,m=ALLmeans,s=ALLsds){x*s[which(names(s)==char)]+m[which(names(m)==char)]}

descaleP(scale(d$Attr),"Attractiveness")==d$Attr # Tohle může bejt to samý: 
# Ukaž: 
descaleP(scale(d$Attr), "Attractiveness")[1:100]
d$Attr[1:100]

cor(descaleP(scale(d$Attr), "Attractiveness")[1:1034], d$Attr[1:1034])
hist((descaleP(scale(d$Attr), "Attractiveness")[1:1034]-d$Attr[1:1034]))

#Check how means and SDs look by nation
tapply(d.s$Attractiveness,d$Nat,mean,na.rm=T)
tapply(d.s$Sextypicality,d$Nat,mean,na.rm=T)
tapply(d.s$Asymmetry,d$Nat,mean,na.rm=T)
tapply(d.s$Distinctiveness,d$Nat,mean,na.rm=T)

tapply(d.s$Attractiveness,d$Nat,sd,na.rm=T)
tapply(d.s$Sextypicality,d$Nat,sd,na.rm=T)
tapply(d.s$Asymmetry,d$Nat,sd,na.rm=T)
tapply(d.s$Distinctiveness,d$Nat,sd,na.rm=T)


#See by sample counts and level orderings
table(as.integer(as.factor(d.s$Nat)),d$Nat)
table(as.integer(as.factor(d.s$Sex)),d$Sex)
table(as.integer(as.factor(d.s$Group)),d$Group)
table(as.integer(as.factor(d.s$Culture_Year)),d$Culture_Year)


