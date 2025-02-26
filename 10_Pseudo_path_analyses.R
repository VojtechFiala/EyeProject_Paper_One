# Simple "path" analyses - separately culture by culture: 

Eye1 <- read.csv2("Data_03_08_24.csv",T)

tapply(Eye1$Attr,Eye1$Culture_Year, mean)

summary(Eye1)
Eye1 <- Eye1[!is.na(Eye1$Age),]

summary(as.factor(Eye1$Sex))
summary(as.factor(Eye1$Culture_Year))


table(Eye1$Culture_Year,as.integer(as.factor(Eye1$Culture_Year)))

library(rethinking)

#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.--.-..--.-.-.-.-.

# When standardised across cultures "Together" - below: These are only path analyses! 

#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.--.-..--.-.-.-.-.

# 1 SKIn
stan_L_skin <- function(x){(x-mean(Eye$L_skin))/sd(Eye$L_skin)}
unstan_L_skin <- function(x){x*sd(Eye$L_skin)+mean(Eye$L_skin)}

# 2 IRIS
stan_L_iris <- function(x){(x-mean(Eye$L_iris))/sd(Eye$L_iris)}
unstan_L_iris <- function(x){x*sd(Eye$L_iris)+mean(Eye$L_iris)}

stan_a_iris <- function(x){(x-mean(Eye$a_iris))/sd(Eye$a_iris)}
unstan_a_iris <- function(x){x*sd(Eye$a_iris)+mean(Eye$a_iris)}

stan_b_iris <- function(x){(x-mean(Eye$b_iris))/sd(Eye$b_iris)}
unstan_b_iris <- function(x){x*sd(Eye$b_iris)+mean(Eye$b_iris)}

# 5 Sclera
stan_L_sclera <- function(x){(x-mean(Eye$L_sclera))/sd(Eye$L_sclera)}
unstan_L_sclera <- function(x){x*sd(Eye$L_sclera)+mean(Eye$L_sclera)}

stan_a_sclera <- function(x){(x-mean(Eye$a_sclera))/sd(Eye$a_sclera)}
unstan_a_sclera <- function(x){x*sd(Eye$a_sclera)+mean(Eye$a_sclera)}

stan_b_sclera <- function(x){(x-mean(Eye$b_sclera))/sd(Eye$b_sclera)}
unstan_b_sclera <- function(x){x*sd(Eye$b_sclera)+mean(Eye$b_sclera)}

# 8 Morphometrics 
stan_MeasSext <- function(x){(x-mean(Eye$SexTypMeas))/sd(Eye$SexTypMeas)}
unstan_MeasSext <- function(x){x*sd(Eye$SexTypMeas)+mean(Eye$SexTypMeas)}

stan_DIST <- function(x){(x-mean(Eye$LOCAL_DIST))/sd(Eye$LOCAL_DIST)}
unstan_DIST <- function(x){x*sd(Eye$LOCAL_DIST)+mean(Eye$LOCAL_DIST)}

stan_Asym <- function(x){(x-mean(Eye$Asym))/sd(Eye$Asym)}
unstan_Asym <- function(x){x*sd(Eye$Asym)+mean(Eye$Asym)}

# 11 Age  
stan_Age <- function(x){(x-mean(Eye$Age))/sd(Eye$Age)}
unstan_Age <- function(x){x*sd(Eye$Age)+mean(Eye$Age)}

# 12 Attractiveness
stan_Attractiveness <- function(x){(x-mean(Eye$Attr))/sd(Eye$Attr)}
unstan_Attractiveness <- function(x){x*sd(Eye$Attr)+mean(Eye$Attr)}

# 13 Sextypicality (perceived)
stan_SextP <- function(x){(x-mean(Eye$SexT))/sd(Eye$SexT)}
unstan_SextP <- function(x){x*sd(Eye$SexT)+mean(Eye$SexT)}

Eye <- Eye1

# Exclude Vietnamese - there is no Sexpitylity ratings...
Eye <- Eye[Eye$Culture_Year!="Vietnam",]

data_AtrSext <- list(
  Sex = as.integer(ifelse(Eye$Sex=="F",1,2)),
  Grp = as.integer(as.factor(Eye$Culture_Year)),
  L_skin = stan_L_skin(Eye$L_skin),
  L_iris = stan_L_iris(Eye$L_iris),
  a_iris = stan_a_iris(Eye$a_iris),
  b_iris = stan_b_iris(Eye$b_iris),
  L_sclera = stan_L_sclera(Eye$L_sclera),
  a_sclera = stan_a_sclera(Eye$a_sclera),
  b_sclera = stan_b_sclera(Eye$b_sclera),
  MeasSext = stan_MeasSext(Eye$SexTypMeas),
  DIST =stan_DIST(Eye$LOCAL_DIST),
  Asym = stan_Asym(Eye$Asym),
  Age = stan_Age(Eye$Age), 
  Attractiveness = stan_Attractiveness(Eye$Attr),
  Sextypicality = stan_SextP(Eye$SexT) # SextP = Sextypicality Perceived...
)

summary.data.frame(data_AtrSext)

data_AtrSext <- as.data.frame(data_AtrSext)

#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.
#
# Cameroon - 2013
#
#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.

data_Attr_Cam_13 <- data_AtrSext[data_AtrSext$Grp==1,]

head(data_Attr_Cam_13)

Fdat <- list(
  Attractiveness = data_Attr_Cam_13$Attractiveness[data_Attr_Cam_13$Sex == 1],
  L_sclera = data_Attr_Cam_13$L_sclera[data_Attr_Cam_13$Sex == 1],
  Sextypicality = data_Attr_Cam_13$Sextypicality[data_Attr_Cam_13$Sex == 1]
)

summary.data.frame(Fdat)

# Defining the model
CMR_13_F_1 <- ulam(
  alist(
    # Model for Perceived Attractiveness
    Attractiveness ~ dnorm(mu_A, sigma_A),
    mu_A <- a_Attr + B_Light_A * L_sclera + B_sext_A * Sextypicality,
    
    # Model for Perceived Sex-typicality
    Sextypicality ~ dnorm(mu_Sp, sigma_Sp),
    mu_Sp <- a_Sext + B_Light_Sext * L_sclera,
    
    # Priors
    a_Attr ~ dnorm(0, 1),
    B_Light_A ~ dnorm(0, 1),
    B_sext_A ~ dnorm(0, 1),
    sigma_A ~ dexp(1),
    
    a_Sext ~ dnorm(0, 1),
    B_Light_Sext ~ dnorm(0, 1),
    sigma_Sp ~ dexp(1)
    
), data = Fdat, chains = 1, cores = 1, iter=1e4)

# Defining the model
CMR_13_F_2 <- ulam(
  alist(
    # Model for Perceived Attractiveness
    Attractiveness ~ dnorm(mu_A, sigma_A),
    mu_A <- a_Attr + B_Light_A * L_sclera + B_sext_A * Sextypicality,
    
    # Priors
    a_Attr ~ dnorm(0, 1),
    B_Light_A ~ dnorm(0, 1),
    B_sext_A ~ dnorm(0, 1),
    sigma_A ~ dexp(1)

  ), data = Fdat, chains = 1, cores = 1, iter=1e4)

# Defining the model
CMR_13_F_3 <- ulam(
  alist(
    # Model for Perceived Attractiveness
    Attractiveness ~ dnorm(mu_A, sigma_A),
    mu_A <- a_Attr + B_Light_A * L_sclera,
    
    # Model for Perceived Sex-typicality
    Sextypicality ~ dnorm(mu_Sp, sigma_Sp),
    mu_Sp <- a_Sext + B_Light_Sext * L_sclera,
    
    # Priors
    a_Attr ~ dnorm(0, 1),
    B_Light_A ~ dnorm(0, 1),
    sigma_A ~ dexp(1),
    
    a_Sext ~ dnorm(0, 1),
    B_Light_Sext ~ dnorm(0, 1),
    sigma_Sp ~ dexp(1)
    
  ), data = Fdat, chains = 1, cores = 1, iter=1e4)

# Summarizing the model
precis(CMR_13_F_1, depth=3, prob=0.95)
precis(CMR_13_F_2, depth=3, prob=0.95)
precis(CMR_13_F_3, depth=3, prob=0.95) # Draw...


#------ 

data_Attr_Cam_13 <- data_AtrSext[data_AtrSext$Grp==1,]

head(data_Attr_Cam_13)

Mdat <- list(
  Attractiveness = data_Attr_Cam_13$Attractiveness[data_Attr_Cam_13$Sex == 2],
  L_sclera = data_Attr_Cam_13$L_sclera[data_Attr_Cam_13$Sex == 2],
  Sextypicality = data_Attr_Cam_13$Sextypicality[data_Attr_Cam_13$Sex == 2]
)

summary.data.frame(Mdat)

# Defining the model
CMR_13_M <- ulam(
  alist(
    # Model for Perceived Attractiveness
    Attractiveness ~ dnorm(mu_A, sigma_A),
    mu_A <- a_Attr + B_Light_A * L_sclera + B_sext_A * Sextypicality,
    
    # Model for Perceived Sex-typicality
    Sextypicality ~ dnorm(mu_Sp, sigma_Sp),
    mu_Sp <- a_Sext + B_Light_Sext * L_sclera,
    
    # Priors
    a_Attr ~ dnorm(0, 0.5),
    B_Light_A ~ dnorm(0, 0.5),
    B_sext_A ~ dnorm(0, 0.5),
    sigma_A ~ dexp(1),
    
    a_Sext ~ dnorm(0, 1),
    B_Light_Sext ~ dnorm(0, 0.5),
    sigma_Sp ~ dexp(1)
    
  ), data = Mdat, chains = 1, cores = 1, iter=1e4)


# Defining the model
CMR_13_M_2 <- ulam(
  alist(
    # Model for Perceived Attractiveness
    Attractiveness ~ dnorm(mu_A, sigma_A),
    mu_A <- a_Attr + B_Light_A * L_sclera + B_sext_A * Sextypicality,
    
    # Priors
    a_Attr ~ dnorm(0, 1),
    B_Light_A ~ dnorm(0, 1),
    B_sext_A ~ dnorm(0, 1),
    sigma_A ~ dexp(1)
    
  ), data = Mdat, chains = 1, cores = 1, iter=1e4)

# Defining the model
CMR_13_M_3 <- ulam(
  alist(
    # Model for Perceived Attractiveness
    Attractiveness ~ dnorm(mu_A, sigma_A),
    mu_A <- a_Attr + B_Light_A * L_sclera,
    
    # Model for Perceived Sex-typicality
    Sextypicality ~ dnorm(mu_Sp, sigma_Sp),
    mu_Sp <- a_Sext + B_Light_Sext * L_sclera,
    
    # Priors
    a_Attr ~ dnorm(0, 1),
    B_Light_A ~ dnorm(0, 1),
    sigma_A ~ dexp(1),
    
    a_Sext ~ dnorm(0, 1),
    B_Light_Sext ~ dnorm(0, 1),
    sigma_Sp ~ dexp(1)
    
  ), data = Mdat, chains = 1, cores = 1, iter=1e4)

# Summarizing the model
precis(CMR_13_M, depth=3, prob=0.95)
precis(CMR_13_M_2, depth=3, prob=0.95)
precis(CMR_13_M_3, depth=3, prob=0.95) # Draw...

#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.
#
# Cameroon - 2016
#
#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.

data_Attr_Cam_16 <- data_AtrSext[data_AtrSext$Grp==2,]


head(data_Attr_Cam_16)

Fdat <- list(
  Attractiveness = data_Attr_Cam_16$Attractiveness[data_Attr_Cam_16$Sex == 1],
  L_sclera = data_Attr_Cam_16$L_sclera[data_Attr_Cam_16$Sex == 1],
  Sextypicality = data_Attr_Cam_16$Sextypicality[data_Attr_Cam_16$Sex == 1]
)

summary.data.frame(Fdat)

# Defining the model
CMR_16_F_1 <- ulam(
  alist(
    # Model for Perceived Attractiveness
    Attractiveness ~ dnorm(mu_A, sigma_A),
    mu_A <- a_Attr + B_Light_A * L_sclera + B_sext_A * Sextypicality,
    
    # Model for Perceived Sex-typicality
    Sextypicality ~ dnorm(mu_Sp, sigma_Sp),
    mu_Sp <- a_Sext + B_Light_Sext * L_sclera,
    
    # Priors
    a_Attr ~ dnorm(0, 1),
    B_Light_A ~ dnorm(0, 1),
    B_sext_A ~ dnorm(0, 1),
    sigma_A ~ dexp(1),
    
    a_Sext ~ dnorm(0, 1),
    B_Light_Sext ~ dnorm(0, 1),
    sigma_Sp ~ dexp(1)
    
  ), data = Fdat, chains = 1, cores = 1, iter=1e4)

# Defining the model
CMR_16_F_2 <- ulam(
  alist(
    # Model for Perceived Attractiveness
    Attractiveness ~ dnorm(mu_A, sigma_A),
    mu_A <- a_Attr + B_Light_A * L_sclera + B_sext_A * Sextypicality,
    
    # Priors
    a_Attr ~ dnorm(0, 1),
    B_Light_A ~ dnorm(0, 1),
    B_sext_A ~ dnorm(0, 1),
    sigma_A ~ dexp(1)
    
  ), data = Fdat, chains = 1, cores = 1, iter=1e4)

# Defining the model
CMR_16_F_3 <- ulam(
  alist(
    # Model for Perceived Attractiveness
    Attractiveness ~ dnorm(mu_A, sigma_A),
    mu_A <- a_Attr + B_Light_A * L_sclera,
    
    # Model for Perceived Sex-typicality
    Sextypicality ~ dnorm(mu_Sp, sigma_Sp),
    mu_Sp <- a_Sext + B_Light_Sext * L_sclera,
    
    # Priors
    a_Attr ~ dnorm(0, 1),
    B_Light_A ~ dnorm(0, 1),
    sigma_A ~ dexp(1),
    
    a_Sext ~ dnorm(0, 1),
    B_Light_Sext ~ dnorm(0, 1),
    sigma_Sp ~ dexp(1)
    
  ), data = Fdat, chains = 1, cores = 1, iter=1e4)

# Summarizing the model
precis(CMR_16_F_1, depth=3, prob=0.95)
precis(CMR_16_F_2, depth=3, prob=0.95)
precis(CMR_16_F_3, depth=3, prob=0.95) # Draw...


#------ 

data_Attr_Cam_16 <- data_AtrSext[data_AtrSext$Grp==2,]

head(data_Attr_Cam_16)

Mdat <- list(
  Attractiveness = data_Attr_Cam_16$Attractiveness[data_Attr_Cam_16$Sex == 2],
  L_sclera = data_Attr_Cam_16$L_sclera[data_Attr_Cam_16$Sex == 2],
  Sextypicality = data_Attr_Cam_16$Sextypicality[data_Attr_Cam_16$Sex == 2]
)

summary.data.frame(Mdat)

# Defining the model
CMR_16_M <- ulam(
  alist(
    # Model for Perceived Attractiveness
    Attractiveness ~ dnorm(mu_A, sigma_A),
    mu_A <- a_Attr + B_Light_A * L_sclera + B_sext_A * Sextypicality,
    
    # Model for Perceived Sex-typicality
    Sextypicality ~ dnorm(mu_Sp, sigma_Sp),
    mu_Sp <- a_Sext + B_Light_Sext * L_sclera,
    
    # Priors
    a_Attr ~ dnorm(0, 0.5),
    B_Light_A ~ dnorm(0, 0.5),
    B_sext_A ~ dnorm(0, 0.5),
    sigma_A ~ dexp(1),
    
    a_Sext ~ dnorm(0, 1),
    B_Light_Sext ~ dnorm(0, 0.5),
    sigma_Sp ~ dexp(1)
    
  ), data = Mdat, chains = 1, cores = 1, iter=1e4)


# Defining the model
CMR_16_M_2 <- ulam(
  alist(
    # Model for Perceived Attractiveness
    Attractiveness ~ dnorm(mu_A, sigma_A),
    mu_A <- a_Attr + B_Light_A * L_sclera + B_sext_A * Sextypicality,
    
    # Priors
    a_Attr ~ dnorm(0, 1),
    B_Light_A ~ dnorm(0, 1),
    B_sext_A ~ dnorm(0, 1),
    sigma_A ~ dexp(1)
    
  ), data = Mdat, chains = 1, cores = 1, iter=1e4)

# Defining the model
CMR_16_M_3 <- ulam(
  alist(
    # Model for Perceived Attractiveness
    Attractiveness ~ dnorm(mu_A, sigma_A),
    mu_A <- a_Attr + B_Light_A * L_sclera,
    
    # Model for Perceived Sex-typicality
    Sextypicality ~ dnorm(mu_Sp, sigma_Sp),
    mu_Sp <- a_Sext + B_Light_Sext * L_sclera,
    
    # Priors
    a_Attr ~ dnorm(0, 1),
    B_Light_A ~ dnorm(0, 1),
    sigma_A ~ dexp(1),
    
    a_Sext ~ dnorm(0, 1),
    B_Light_Sext ~ dnorm(0, 1),
    sigma_Sp ~ dexp(1)
    
  ), data = Mdat, chains = 1, cores = 1, iter=1e4)

# Summarizing the model
precis(CMR_16_M, depth=3, prob=0.95)
precis(CMR_16_M_2, depth=3, prob=0.95)
precis(CMR_16_M_3, depth=3, prob=0.95) # Draw...



#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.
#
# Colombia
#
#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.

data_Attr_Col <- data_AtrSext[data_AtrSext$Grp==3,]

head(data_Attr_Col)

Fdat <- list(
  Attractiveness = data_Attr_Col$Attractiveness[data_Attr_Col$Sex == 1],
  L_sclera = data_Attr_Col$L_sclera[data_Attr_Col$Sex == 1],
  Sextypicality = data_Attr_Col$Sextypicality[data_Attr_Col$Sex == 1]
)

summary.data.frame(Fdat)

# Defining the model
COL_F_1 <- ulam(
  alist(
    # Model for Perceived Attractiveness
    Attractiveness ~ dnorm(mu_A, sigma_A),
    mu_A <- a_Attr + B_Light_A * L_sclera + B_sext_A * Sextypicality,
    
    # Model for Perceived Sex-typicality
    Sextypicality ~ dnorm(mu_Sp, sigma_Sp),
    mu_Sp <- a_Sext + B_Light_Sext * L_sclera,
    
    # Priors
    a_Attr ~ dnorm(0, 1),
    B_Light_A ~ dnorm(0, 1),
    B_sext_A ~ dnorm(0, 1),
    sigma_A ~ dexp(1),
    
    a_Sext ~ dnorm(0, 1),
    B_Light_Sext ~ dnorm(0, 1),
    sigma_Sp ~ dexp(1)
    
  ), data = Fdat, chains = 1, cores = 1, iter=1e4)

# Defining the model
COL_F_2 <- ulam(
  alist(
    # Model for Perceived Attractiveness
    Attractiveness ~ dnorm(mu_A, sigma_A),
    mu_A <- a_Attr + B_Light_A * L_sclera + B_sext_A * Sextypicality,
    
    # Priors
    a_Attr ~ dnorm(0, 1),
    B_Light_A ~ dnorm(0, 1),
    B_sext_A ~ dnorm(0, 1),
    sigma_A ~ dexp(1)
    
  ), data = Fdat, chains = 1, cores = 1, iter=1e4)

# Defining the model
COL_F_3 <- ulam(
  alist(
    # Model for Perceived Attractiveness
    Attractiveness ~ dnorm(mu_A, sigma_A),
    mu_A <- a_Attr + B_Light_A * L_sclera,
    
    # Model for Perceived Sex-typicality
    Sextypicality ~ dnorm(mu_Sp, sigma_Sp),
    mu_Sp <- a_Sext + B_Light_Sext * L_sclera,
    
    # Priors
    a_Attr ~ dnorm(0, 1),
    B_Light_A ~ dnorm(0, 1),
    sigma_A ~ dexp(1),
    
    a_Sext ~ dnorm(0, 1),
    B_Light_Sext ~ dnorm(0, 1),
    sigma_Sp ~ dexp(1)
    
  ), data = Fdat, chains = 1, cores = 1, iter=1e4)

# Summarizing the model
precis(COL_F_1, depth=3, prob=0.95)
precis(COL_F_2, depth=3, prob=0.95)
precis(COL_F_3, depth=3, prob=0.95) # Draw...

#----

head(data_Attr_Col)

Mdat <- list(
  Attractiveness = data_Attr_Col$Attractiveness[data_Attr_Col$Sex == 2],
  L_sclera = data_Attr_Col$L_sclera[data_Attr_Col$Sex == 2],
  Sextypicality = data_Attr_Col$Sextypicality[data_Attr_Col$Sex == 2]
)

summary.data.frame(Mdat)


summary.data.frame(Fdat)

# Defining the model
COL_M_1 <- ulam(
  alist(
    # Model for Perceived Attractiveness
    Attractiveness ~ dnorm(mu_A, sigma_A),
    mu_A <- a_Attr + B_Light_A * L_sclera + B_sext_A * Sextypicality,
    
    # Model for Perceived Sex-typicality
    Sextypicality ~ dnorm(mu_Sp, sigma_Sp),
    mu_Sp <- a_Sext + B_Light_Sext * L_sclera,
    
    # Priors
    a_Attr ~ dnorm(0, 1),
    B_Light_A ~ dnorm(0, 1),
    B_sext_A ~ dnorm(0, 1),
    sigma_A ~ dexp(1),
    
    a_Sext ~ dnorm(0, 1),
    B_Light_Sext ~ dnorm(0, 1),
    sigma_Sp ~ dexp(1)
    
  ), data = Mdat, chains = 1, cores = 1, iter=1e4)

# Defining the model
COL_M_2 <- ulam(
  alist(
    # Model for Perceived Attractiveness
    Attractiveness ~ dnorm(mu_A, sigma_A),
    mu_A <- a_Attr + B_Light_A * L_sclera + B_sext_A * Sextypicality,
    
    # Priors
    a_Attr ~ dnorm(0, 1),
    B_Light_A ~ dnorm(0, 1),
    B_sext_A ~ dnorm(0, 1),
    sigma_A ~ dexp(1)
    
  ), data = Mdat, chains = 1, cores = 1, iter=1e4)

# Defining the model
COL_M_3 <- ulam(
  alist(
    # Model for Perceived Attractiveness
    Attractiveness ~ dnorm(mu_A, sigma_A),
    mu_A <- a_Attr + B_Light_A * L_sclera,
    
    # Model for Perceived Sex-typicality
    Sextypicality ~ dnorm(mu_Sp, sigma_Sp),
    mu_Sp <- a_Sext + B_Light_Sext * L_sclera,
    
    # Priors
    a_Attr ~ dnorm(0, 1),
    B_Light_A ~ dnorm(0, 1),
    sigma_A ~ dexp(1),
    
    a_Sext ~ dnorm(0, 1),
    B_Light_Sext ~ dnorm(0, 1),
    sigma_Sp ~ dexp(1)
    
  ), data = Mdat, chains = 1, cores = 1, iter=1e4)

# Summarizing the model
precis(COL_M_1, depth=3, prob=0.95)
precis(COL_M_2, depth=3, prob=0.95)
precis(COL_M_3, depth=3, prob=0.95) # Draw...



#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.
#
# Czech Rep. - 2016
#
#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.

data_Attr_CZ_16 <- data_AtrSext[data_AtrSext$Grp==4,]

head(data_Attr_CZ_16)

Fdat <- list(
  Attractiveness = data_Attr_CZ_16$Attractiveness[data_Attr_CZ_16$Sex == 1],
  L_sclera = data_Attr_CZ_16$L_sclera[data_Attr_CZ_16$Sex == 1],
  Sextypicality = data_Attr_CZ_16$Sextypicality[data_Attr_CZ_16$Sex == 1]
)

summary.data.frame(Fdat)

# Defining the model
CZ16_F_1 <- ulam(
  alist(
    # Model for Perceived Attractiveness
    Attractiveness ~ dnorm(mu_A, sigma_A),
    mu_A <- a_Attr + B_Light_A * L_sclera + B_sext_A * Sextypicality,
    
    # Model for Perceived Sex-typicality
    Sextypicality ~ dnorm(mu_Sp, sigma_Sp),
    mu_Sp <- a_Sext + B_Light_Sext * L_sclera,
    
    # Priors
    a_Attr ~ dnorm(0, 1),
    B_Light_A ~ dnorm(0, 1),
    B_sext_A ~ dnorm(0, 1),
    sigma_A ~ dexp(1),
    
    a_Sext ~ dnorm(0, 1),
    B_Light_Sext ~ dnorm(0, 1),
    sigma_Sp ~ dexp(1)
    
  ), data = Fdat, chains = 1, cores = 1, iter=1e4)

# Defining the model
CZ16_F_2 <- ulam(
  alist(
    # Model for Perceived Attractiveness
    Attractiveness ~ dnorm(mu_A, sigma_A),
    mu_A <- a_Attr + B_Light_A * L_sclera + B_sext_A * Sextypicality,
    
    # Priors
    a_Attr ~ dnorm(0, 1),
    B_Light_A ~ dnorm(0, 1),
    B_sext_A ~ dnorm(0, 1),
    sigma_A ~ dexp(1)
    
  ), data = Fdat, chains = 1, cores = 1, iter=1e4)

# Defining the model
CZ16_F_3 <- ulam(
  alist(
    # Model for Perceived Attractiveness
    Attractiveness ~ dnorm(mu_A, sigma_A),
    mu_A <- a_Attr + B_Light_A * L_sclera,
    
    # Model for Perceived Sex-typicality
    Sextypicality ~ dnorm(mu_Sp, sigma_Sp),
    mu_Sp <- a_Sext + B_Light_Sext * L_sclera,
    
    # Priors
    a_Attr ~ dnorm(0, 1),
    B_Light_A ~ dnorm(0, 1),
    sigma_A ~ dexp(1),
    
    a_Sext ~ dnorm(0, 1),
    B_Light_Sext ~ dnorm(0, 1),
    sigma_Sp ~ dexp(1)
    
  ), data = Fdat, chains = 1, cores = 1, iter=1e4)

# Summarizing the model
precis(CZ16_F_1, depth=3, prob=0.95)
precis(CZ16_F_2, depth=3, prob=0.95)
precis(CZ16_F_3, depth=3, prob=0.95) # Draw...

#----

head(data_Attr_CZ_16)

Mdat <- list(
  Attractiveness = data_Attr_CZ_16$Attractiveness[data_Attr_CZ_16$Sex == 2],
  L_sclera = data_Attr_CZ_16$L_sclera[data_Attr_CZ_16$Sex == 2],
  Sextypicality = data_Attr_CZ_16$Sextypicality[data_Attr_CZ_16$Sex == 2]
)

summary.data.frame(Mdat)


summary.data.frame(Fdat)

# Defining the model
CZ16_M_1 <- ulam(
  alist(
    # Model for Perceived Attractiveness
    Attractiveness ~ dnorm(mu_A, sigma_A),
    mu_A <- a_Attr + B_Light_A * L_sclera + B_sext_A * Sextypicality,
    
    # Model for Perceived Sex-typicality
    Sextypicality ~ dnorm(mu_Sp, sigma_Sp),
    mu_Sp <- a_Sext + B_Light_Sext * L_sclera,
    
    # Priors
    a_Attr ~ dnorm(0, 1),
    B_Light_A ~ dnorm(0, 1),
    B_sext_A ~ dnorm(0, 1),
    sigma_A ~ dexp(1),
    
    a_Sext ~ dnorm(0, 1),
    B_Light_Sext ~ dnorm(0, 1),
    sigma_Sp ~ dexp(1)
    
  ), data = Mdat, chains = 1, cores = 1, iter=1e4)

# Defining the model
CZ16_M_2 <- ulam(
  alist(
    # Model for Perceived Attractiveness
    Attractiveness ~ dnorm(mu_A, sigma_A),
    mu_A <- a_Attr + B_Light_A * L_sclera + B_sext_A * Sextypicality,
    
    # Priors
    a_Attr ~ dnorm(0, 1),
    B_Light_A ~ dnorm(0, 1),
    B_sext_A ~ dnorm(0, 1),
    sigma_A ~ dexp(1)
    
  ), data = Mdat, chains = 1, cores = 1, iter=1e4)

# Defining the model
CZ16_M_3 <- ulam(
  alist(
    # Model for Perceived Attractiveness
    Attractiveness ~ dnorm(mu_A, sigma_A),
    mu_A <- a_Attr + B_Light_A * L_sclera,
    
    # Model for Perceived Sex-typicality
    Sextypicality ~ dnorm(mu_Sp, sigma_Sp),
    mu_Sp <- a_Sext + B_Light_Sext * L_sclera,
    
    # Priors
    a_Attr ~ dnorm(0, 1),
    B_Light_A ~ dnorm(0, 1),
    sigma_A ~ dexp(1),
    
    a_Sext ~ dnorm(0, 1),
    B_Light_Sext ~ dnorm(0, 1),
    sigma_Sp ~ dexp(1)
    
  ), data = Mdat, chains = 1, cores = 1, iter=1e4)

# Summarizing the model
precis(CZ16_M_1, depth=3, prob=0.95)
precis(CZ16_M_2, depth=3, prob=0.95)
precis(CZ16_M_3, depth=3, prob=0.95) # Draw...


#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.
#
# Czech Rep. - 2019
#
#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.

data_Attr_CZ_19 <- data_AtrSext[data_AtrSext$Grp==5,]

head(data_Attr_CZ_19)

Fdat <- list(
  Attractiveness = data_Attr_CZ_19$Attractiveness[data_Attr_CZ_19$Sex == 1],
  L_sclera = data_Attr_CZ_19$L_sclera[data_Attr_CZ_19$Sex == 1],
  Sextypicality = data_Attr_CZ_19$Sextypicality[data_Attr_CZ_19$Sex == 1]
)

summary.data.frame(Fdat)

# Defining the model
CZ19_F_1 <- ulam(
  alist(
    # Model for Perceived Attractiveness
    Attractiveness ~ dnorm(mu_A, sigma_A),
    mu_A <- a_Attr + B_Light_A * L_sclera + B_sext_A * Sextypicality,
    
    # Model for Perceived Sex-typicality
    Sextypicality ~ dnorm(mu_Sp, sigma_Sp),
    mu_Sp <- a_Sext + B_Light_Sext * L_sclera,
    
    # Priors
    a_Attr ~ dnorm(0, 1),
    B_Light_A ~ dnorm(0, 1),
    B_sext_A ~ dnorm(0, 1),
    sigma_A ~ dexp(1),
    
    a_Sext ~ dnorm(0, 1),
    B_Light_Sext ~ dnorm(0, 1),
    sigma_Sp ~ dexp(1)
    
  ), data = Fdat, chains = 1, cores = 1, iter=1e4)

# Defining the model
CZ19_F_2 <- ulam(
  alist(
    # Model for Perceived Attractiveness
    Attractiveness ~ dnorm(mu_A, sigma_A),
    mu_A <- a_Attr + B_Light_A * L_sclera + B_sext_A * Sextypicality,
    
    # Priors
    a_Attr ~ dnorm(0, 1),
    B_Light_A ~ dnorm(0, 1),
    B_sext_A ~ dnorm(0, 1),
    sigma_A ~ dexp(1)
    
  ), data = Fdat, chains = 1, cores = 1, iter=1e4)

# Defining the model
CZ19_F_3 <- ulam(
  alist(
    # Model for Perceived Attractiveness
    Attractiveness ~ dnorm(mu_A, sigma_A),
    mu_A <- a_Attr + B_Light_A * L_sclera,
    
    # Model for Perceived Sex-typicality
    Sextypicality ~ dnorm(mu_Sp, sigma_Sp),
    mu_Sp <- a_Sext + B_Light_Sext * L_sclera,
    
    # Priors
    a_Attr ~ dnorm(0, 1),
    B_Light_A ~ dnorm(0, 1),
    sigma_A ~ dexp(1),
    
    a_Sext ~ dnorm(0, 1),
    B_Light_Sext ~ dnorm(0, 1),
    sigma_Sp ~ dexp(1)
    
  ), data = Fdat, chains = 1, cores = 1, iter=1e4)

# Summarizing the model
precis(CZ19_F_1, depth=3, prob=0.95)
precis(CZ19_F_2, depth=3, prob=0.95)
precis(CZ19_F_3, depth=3, prob=0.95) # Draw...

#----

head(data_Attr_CZ_19)

Mdat <- list(
  Attractiveness = data_Attr_CZ_19$Attractiveness[data_Attr_CZ_19$Sex == 2],
  L_sclera = data_Attr_CZ_19$L_sclera[data_Attr_CZ_19$Sex == 2],
  Sextypicality = data_Attr_CZ_19$Sextypicality[data_Attr_CZ_19$Sex == 2]
)

summary.data.frame(Mdat)


summary.data.frame(Fdat)

# Defining the model
CZ19_M_1 <- ulam(
  alist(
    # Model for Perceived Attractiveness
    Attractiveness ~ dnorm(mu_A, sigma_A),
    mu_A <- a_Attr + B_Light_A * L_sclera + B_sext_A * Sextypicality,
    
    # Model for Perceived Sex-typicality
    Sextypicality ~ dnorm(mu_Sp, sigma_Sp),
    mu_Sp <- a_Sext + B_Light_Sext * L_sclera,
    
    # Priors
    a_Attr ~ dnorm(0, 1),
    B_Light_A ~ dnorm(0, 1),
    B_sext_A ~ dnorm(0, 1),
    sigma_A ~ dexp(1),
    
    a_Sext ~ dnorm(0, 1),
    B_Light_Sext ~ dnorm(0, 1),
    sigma_Sp ~ dexp(1)
    
  ), data = Mdat, chains = 1, cores = 1, iter=1e4)

# Defining the model
CZ19_M_2 <- ulam(
  alist(
    # Model for Perceived Attractiveness
    Attractiveness ~ dnorm(mu_A, sigma_A),
    mu_A <- a_Attr + B_Light_A * L_sclera + B_sext_A * Sextypicality,
    
    # Priors
    a_Attr ~ dnorm(0, 1),
    B_Light_A ~ dnorm(0, 1),
    B_sext_A ~ dnorm(0, 1),
    sigma_A ~ dexp(1)
    
  ), data = Mdat, chains = 1, cores = 1, iter=1e4)

# Defining the model
CZ19_M_3 <- ulam(
  alist(
    # Model for Perceived Attractiveness
    Attractiveness ~ dnorm(mu_A, sigma_A),
    mu_A <- a_Attr + B_Light_A * L_sclera,
    
    # Model for Perceived Sex-typicality
    Sextypicality ~ dnorm(mu_Sp, sigma_Sp),
    mu_Sp <- a_Sext + B_Light_Sext * L_sclera,
    
    # Priors
    a_Attr ~ dnorm(0, 1),
    B_Light_A ~ dnorm(0, 1),
    sigma_A ~ dexp(1),
    
    a_Sext ~ dnorm(0, 1),
    B_Light_Sext ~ dnorm(0, 1),
    sigma_Sp ~ dexp(1)
    
  ), data = Mdat, chains = 1, cores = 1, iter=1e4)

# Summarizing the model
precis(CZ19_M_1, depth=3, prob=0.95)
precis(CZ19_M_2, depth=3, prob=0.95)
precis(CZ19_M_3, depth=3, prob=0.95) # Draw...



#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.
#
# India - CFD
#
#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.

data_Attr_IND <- data_AtrSext[data_AtrSext$Grp==6,]

head(data_Attr_IND)

Fdat <- list(
  Attractiveness = data_Attr_IND$Attractiveness[data_Attr_IND$Sex == 1],
  L_sclera = data_Attr_IND$L_sclera[data_Attr_IND$Sex == 1],
  Sextypicality = data_Attr_IND$Sextypicality[data_Attr_IND$Sex == 1]
)

summary.data.frame(Fdat)

# Defining the model
IND_F_1 <- ulam(
  alist(
    # Model for Perceived Attractiveness
    Attractiveness ~ dnorm(mu_A, sigma_A),
    mu_A <- a_Attr + B_Light_A * L_sclera + B_sext_A * Sextypicality,
    
    # Model for Perceived Sex-typicality
    Sextypicality ~ dnorm(mu_Sp, sigma_Sp),
    mu_Sp <- a_Sext + B_Light_Sext * L_sclera,
    
    # Priors
    a_Attr ~ dnorm(0, 1),
    B_Light_A ~ dnorm(0, 1),
    B_sext_A ~ dnorm(0, 1),
    sigma_A ~ dexp(1),
    
    a_Sext ~ dnorm(0, 1),
    B_Light_Sext ~ dnorm(0, 1),
    sigma_Sp ~ dexp(1)
    
  ), data = Fdat, chains = 1, cores = 1, iter=1e4)

# Defining the model
IND_F_2 <- ulam(
  alist(
    # Model for Perceived Attractiveness
    Attractiveness ~ dnorm(mu_A, sigma_A),
    mu_A <- a_Attr + B_Light_A * L_sclera + B_sext_A * Sextypicality,
    
    # Priors
    a_Attr ~ dnorm(0, 1),
    B_Light_A ~ dnorm(0, 1),
    B_sext_A ~ dnorm(0, 1),
    sigma_A ~ dexp(1)
    
  ), data = Fdat, chains = 1, cores = 1, iter=1e4)

# Defining the model
IND_F_3 <- ulam(
  alist(
    # Model for Perceived Attractiveness
    Attractiveness ~ dnorm(mu_A, sigma_A),
    mu_A <- a_Attr + B_Light_A * L_sclera,
    
    # Model for Perceived Sex-typicality
    Sextypicality ~ dnorm(mu_Sp, sigma_Sp),
    mu_Sp <- a_Sext + B_Light_Sext * L_sclera,
    
    # Priors
    a_Attr ~ dnorm(0, 1),
    B_Light_A ~ dnorm(0, 1),
    sigma_A ~ dexp(1),
    
    a_Sext ~ dnorm(0, 1),
    B_Light_Sext ~ dnorm(0, 1),
    sigma_Sp ~ dexp(1)
    
  ), data = Fdat, chains = 1, cores = 1, iter=1e4)

# Summarizing the model
precis(IND_F_1, depth=3, prob=0.95)
precis(IND_F_2, depth=3, prob=0.95)
precis(IND_F_3, depth=3, prob=0.95) # Draw...

#----

head(data_Attr_IND)

Mdat <- list(
  Attractiveness = data_Attr_IND$Attractiveness[data_Attr_IND$Sex == 2],
  L_sclera = data_Attr_IND$L_sclera[data_Attr_IND$Sex == 2],
  Sextypicality = data_Attr_IND$Sextypicality[data_Attr_IND$Sex == 2]
)

summary.data.frame(Mdat)

# Defining the model
IND_M_1 <- ulam(
  alist(
    # Model for Perceived Attractiveness
    Attractiveness ~ dnorm(mu_A, sigma_A),
    mu_A <- a_Attr + B_Light_A * L_sclera + B_sext_A * Sextypicality,
    
    # Model for Perceived Sex-typicality
    Sextypicality ~ dnorm(mu_Sp, sigma_Sp),
    mu_Sp <- a_Sext + B_Light_Sext * L_sclera,
    
    # Priors
    a_Attr ~ dnorm(0, 1),
    B_Light_A ~ dnorm(0, 1),
    B_sext_A ~ dnorm(0, 1),
    sigma_A ~ dexp(1),
    
    a_Sext ~ dnorm(0, 1),
    B_Light_Sext ~ dnorm(0, 1),
    sigma_Sp ~ dexp(1)
    
  ), data = Mdat, chains = 1, cores = 1, iter=1e4)

# Defining the model
IND_M_2 <- ulam(
  alist(
    # Model for Perceived Attractiveness
    Attractiveness ~ dnorm(mu_A, sigma_A),
    mu_A <- a_Attr + B_Light_A * L_sclera + B_sext_A * Sextypicality,
    
    # Priors
    a_Attr ~ dnorm(0, 1),
    B_Light_A ~ dnorm(0, 1),
    B_sext_A ~ dnorm(0, 1),
    sigma_A ~ dexp(1)
    
  ), data = Mdat, chains = 1, cores = 1, iter=1e4)

# Defining the model
IND_M_3 <- ulam(
  alist(
    # Model for Perceived Attractiveness
    Attractiveness ~ dnorm(mu_A, sigma_A),
    mu_A <- a_Attr + B_Light_A * L_sclera,
    
    # Model for Perceived Sex-typicality
    Sextypicality ~ dnorm(mu_Sp, sigma_Sp),
    mu_Sp <- a_Sext + B_Light_Sext * L_sclera,
    
    # Priors
    a_Attr ~ dnorm(0, 1),
    B_Light_A ~ dnorm(0, 1),
    sigma_A ~ dexp(1),
    
    a_Sext ~ dnorm(0, 1),
    B_Light_Sext ~ dnorm(0, 1),
    sigma_Sp ~ dexp(1)
    
  ), data = Mdat, chains = 1, cores = 1, iter=1e4)

# Summarizing the model
precis(IND_M_1, depth=3, prob=0.95)
precis(IND_M_2, depth=3, prob=0.95)
precis(IND_M_3, depth=3, prob=0.95) # Draw...



#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.
#
# Iran
#
#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.

data_Attr_IR <- data_AtrSext[data_AtrSext$Grp==7,]

head(data_Attr_IR)

Fdat <- list(
  Attractiveness = data_Attr_IR$Attractiveness[data_Attr_IR$Sex == 1],
  L_sclera = data_Attr_IR$L_sclera[data_Attr_IR$Sex == 1],
  Sextypicality = data_Attr_IR$Sextypicality[data_Attr_IR$Sex == 1]
)

summary.data.frame(Fdat)

# Defining the model
IR_F_1 <- ulam(
  alist(
    # Model for Perceived Attractiveness
    Attractiveness ~ dnorm(mu_A, sigma_A),
    mu_A <- a_Attr + B_Light_A * L_sclera + B_sext_A * Sextypicality,
    
    # Model for Perceived Sex-typicality
    Sextypicality ~ dnorm(mu_Sp, sigma_Sp),
    mu_Sp <- a_Sext + B_Light_Sext * L_sclera,
    
    # Priors
    a_Attr ~ dnorm(0, 1),
    B_Light_A ~ dnorm(0, 1),
    B_sext_A ~ dnorm(0, 1),
    sigma_A ~ dexp(1),
    
    a_Sext ~ dnorm(0, 1),
    B_Light_Sext ~ dnorm(0, 1),
    sigma_Sp ~ dexp(1)
    
  ), data = Fdat, chains = 1, cores = 1, iter=1e4)

# Defining the model
IR_F_2 <- ulam(
  alist(
    # Model for Perceived Attractiveness
    Attractiveness ~ dnorm(mu_A, sigma_A),
    mu_A <- a_Attr + B_Light_A * L_sclera + B_sext_A * Sextypicality,
    
    # Priors
    a_Attr ~ dnorm(0, 1),
    B_Light_A ~ dnorm(0, 1),
    B_sext_A ~ dnorm(0, 1),
    sigma_A ~ dexp(1)
    
  ), data = Fdat, chains = 1, cores = 1, iter=1e4)

# Defining the model
IR_F_3 <- ulam(
  alist(
    # Model for Perceived Attractiveness
    Attractiveness ~ dnorm(mu_A, sigma_A),
    mu_A <- a_Attr + B_Light_A * L_sclera,
    
    # Model for Perceived Sex-typicality
    Sextypicality ~ dnorm(mu_Sp, sigma_Sp),
    mu_Sp <- a_Sext + B_Light_Sext * L_sclera,
    
    # Priors
    a_Attr ~ dnorm(0, 1),
    B_Light_A ~ dnorm(0, 1),
    sigma_A ~ dexp(1),
    
    a_Sext ~ dnorm(0, 1),
    B_Light_Sext ~ dnorm(0, 1),
    sigma_Sp ~ dexp(1)
    
  ), data = Fdat, chains = 1, cores = 1, iter=1e4)

# Summarizing the model
precis(IR_F_1, depth=3, prob=0.95)
precis(IR_F_2, depth=3, prob=0.95)
precis(IR_F_3, depth=3, prob=0.95) # Draw...

#----

head(data_Attr_IR)

Mdat <- list(
  Attractiveness = data_Attr_IR$Attractiveness[data_Attr_IR$Sex == 2],
  L_sclera = data_Attr_IR$L_sclera[data_Attr_IR$Sex == 2],
  Sextypicality = data_Attr_IR$Sextypicality[data_Attr_IR$Sex == 2]
)

summary.data.frame(Mdat)

# Defining the model
IR_M_1 <- ulam(
  alist(
    # Model for Perceived Attractiveness
    Attractiveness ~ dnorm(mu_A, sigma_A),
    mu_A <- a_Attr + B_Light_A * L_sclera + B_sext_A * Sextypicality,
    
    # Model for Perceived Sex-typicality
    Sextypicality ~ dnorm(mu_Sp, sigma_Sp),
    mu_Sp <- a_Sext + B_Light_Sext * L_sclera,
    
    # Priors
    a_Attr ~ dnorm(0, 1),
    B_Light_A ~ dnorm(0, 1),
    B_sext_A ~ dnorm(0, 1),
    sigma_A ~ dexp(1),
    
    a_Sext ~ dnorm(0, 1),
    B_Light_Sext ~ dnorm(0, 1),
    sigma_Sp ~ dexp(1)
    
  ), data = Mdat, chains = 1, cores = 1, iter=1e4)

# Defining the model
IR_M_2 <- ulam(
  alist(
    # Model for Perceived Attractiveness
    Attractiveness ~ dnorm(mu_A, sigma_A),
    mu_A <- a_Attr + B_Light_A * L_sclera + B_sext_A * Sextypicality,
    
    # Priors
    a_Attr ~ dnorm(0, 1),
    B_Light_A ~ dnorm(0, 1),
    B_sext_A ~ dnorm(0, 1),
    sigma_A ~ dexp(1)
    
  ), data = Mdat, chains = 1, cores = 1, iter=1e4)

# Defining the model
IR_M_3 <- ulam(
  alist(
    # Model for Perceived Attractiveness
    Attractiveness ~ dnorm(mu_A, sigma_A),
    mu_A <- a_Attr + B_Light_A * L_sclera,
    
    # Model for Perceived Sex-typicality
    Sextypicality ~ dnorm(mu_Sp, sigma_Sp),
    mu_Sp <- a_Sext + B_Light_Sext * L_sclera,
    
    # Priors
    a_Attr ~ dnorm(0, 1),
    B_Light_A ~ dnorm(0, 1),
    sigma_A ~ dexp(1),
    
    a_Sext ~ dnorm(0, 1),
    B_Light_Sext ~ dnorm(0, 1),
    sigma_Sp ~ dexp(1)
    
  ), data = Mdat, chains = 1, cores = 1, iter=1e4)

# Summarizing the model
precis(IR_M_1, depth=3, prob=0.95)
precis(IR_M_2, depth=3, prob=0.95)
precis(IR_M_3, depth=3, prob=0.95) # Draw...


#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.
#
# Turkey
#
#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.

data_Attr_TR <- data_AtrSext[data_AtrSext$Grp==8,]

head(data_Attr_TR)

Fdat <- list(
  Attractiveness = data_Attr_TR$Attractiveness[data_Attr_TR$Sex == 1],
  L_sclera = data_Attr_TR$L_sclera[data_Attr_TR$Sex == 1],
  Sextypicality = data_Attr_TR$Sextypicality[data_Attr_TR$Sex == 1]
)

summary.data.frame(Fdat)

# Defining the model
TUR_F_1 <- ulam(
  alist(
    # Model for Perceived Attractiveness
    Attractiveness ~ dnorm(mu_A, sigma_A),
    mu_A <- a_Attr + B_Light_A * L_sclera + B_sext_A * Sextypicality,
    
    # Model for Perceived Sex-typicality
    Sextypicality ~ dnorm(mu_Sp, sigma_Sp),
    mu_Sp <- a_Sext + B_Light_Sext * L_sclera,
    
    # Priors
    a_Attr ~ dnorm(0, 1),
    B_Light_A ~ dnorm(0, 1),
    B_sext_A ~ dnorm(0, 1),
    sigma_A ~ dexp(1),
    
    a_Sext ~ dnorm(0, 1),
    B_Light_Sext ~ dnorm(0, 1),
    sigma_Sp ~ dexp(1)
    
  ), data = Fdat, chains = 1, cores = 1, iter=1e4)

# Defining the model
TUR_F_2 <- ulam(
  alist(
    # Model for Perceived Attractiveness
    Attractiveness ~ dnorm(mu_A, sigma_A),
    mu_A <- a_Attr + B_Light_A * L_sclera + B_sext_A * Sextypicality,
    
    # Priors
    a_Attr ~ dnorm(0, 1),
    B_Light_A ~ dnorm(0, 1),
    B_sext_A ~ dnorm(0, 1),
    sigma_A ~ dexp(1)
    
  ), data = Fdat, chains = 1, cores = 1, iter=1e4)

# Defining the model
TUR_F_3 <- ulam(
  alist(
    # Model for Perceived Attractiveness
    Attractiveness ~ dnorm(mu_A, sigma_A),
    mu_A <- a_Attr + B_Light_A * L_sclera,
    
    # Model for Perceived Sex-typicality
    Sextypicality ~ dnorm(mu_Sp, sigma_Sp),
    mu_Sp <- a_Sext + B_Light_Sext * L_sclera,
    
    # Priors
    a_Attr ~ dnorm(0, 1),
    B_Light_A ~ dnorm(0, 1),
    sigma_A ~ dexp(1),
    
    a_Sext ~ dnorm(0, 1),
    B_Light_Sext ~ dnorm(0, 1),
    sigma_Sp ~ dexp(1)
    
  ), data = Fdat, chains = 1, cores = 1, iter=1e4)

# Summarizing the model
precis(TUR_F_1, depth=3, prob=0.95)
precis(TUR_F_2, depth=3, prob=0.95)
precis(TUR_F_3, depth=3, prob=0.95) # Draw...

#----

head(data_Attr_TR)

Mdat <- list(
  Attractiveness = data_Attr_TR$Attractiveness[data_Attr_TR$Sex == 2],
  L_sclera = data_Attr_TR$L_sclera[data_Attr_TR$Sex == 2],
  Sextypicality = data_Attr_TR$Sextypicality[data_Attr_TR$Sex == 2]
)

summary.data.frame(Mdat)

# Defining the model
TUR_M_1 <- ulam(
  alist(
    # Model for Perceived Attractiveness
    Attractiveness ~ dnorm(mu_A, sigma_A),
    mu_A <- a_Attr + B_Light_A * L_sclera + B_sext_A * Sextypicality,
    
    # Model for Perceived Sex-typicality
    Sextypicality ~ dnorm(mu_Sp, sigma_Sp),
    mu_Sp <- a_Sext + B_Light_Sext * L_sclera,
    
    # Priors
    a_Attr ~ dnorm(0, 1),
    B_Light_A ~ dnorm(0, 1),
    B_sext_A ~ dnorm(0, 1),
    sigma_A ~ dexp(1),
    
    a_Sext ~ dnorm(0, 1),
    B_Light_Sext ~ dnorm(0, 1),
    sigma_Sp ~ dexp(1)
    
  ), data = Mdat, chains = 1, cores = 1, iter=1e4)

# Defining the model
TUR_M_2 <- ulam(
  alist(
    # Model for Perceived Attractiveness
    Attractiveness ~ dnorm(mu_A, sigma_A),
    mu_A <- a_Attr + B_Light_A * L_sclera + B_sext_A * Sextypicality,
    
    # Priors
    a_Attr ~ dnorm(0, 1),
    B_Light_A ~ dnorm(0, 1),
    B_sext_A ~ dnorm(0, 1),
    sigma_A ~ dexp(1)
    
  ), data = Mdat, chains = 1, cores = 1, iter=1e4)

# Defining the model
TUR_M_3 <- ulam(
  alist(
    # Model for Perceived Attractiveness
    Attractiveness ~ dnorm(mu_A, sigma_A),
    mu_A <- a_Attr + B_Light_A * L_sclera,
    
    # Model for Perceived Sex-typicality
    Sextypicality ~ dnorm(mu_Sp, sigma_Sp),
    mu_Sp <- a_Sext + B_Light_Sext * L_sclera,
    
    # Priors
    a_Attr ~ dnorm(0, 1),
    B_Light_A ~ dnorm(0, 1),
    sigma_A ~ dexp(1),
    
    a_Sext ~ dnorm(0, 1),
    B_Light_Sext ~ dnorm(0, 1),
    sigma_Sp ~ dexp(1)
    
  ), data = Mdat, chains = 1, cores = 1, iter=1e4)

# Summarizing the model
precis(TUR_M_1, depth=3, prob=0.95)
precis(TUR_M_2, depth=3, prob=0.95)
precis(TUR_M_3, depth=3, prob=0.95) # Draw...

#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.
#
# Vietnam
#
#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.

# NA - Do not have SexTypicality Ratings: data_Attr_VN <- data_Attr[data_Attr$Grp==9,]
