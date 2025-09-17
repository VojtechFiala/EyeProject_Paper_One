# Analysis that includes quadratic effects for selected coefficients (eye colour)...

# Version "GEO_ADDED": In August, 2025, We added analyses with geographically-pooled sample

# Version 1: 
# CZ Samples pooled
# CMR Samples pooled
# All other samples kept separately

# Version 2: 
# CZ Samples pooled
# CMR Samples pooled
# Tur+Iran+India+VN pooled
# Colombia left separately as there is no other representant of South America

# We also show the predictions when the above is done post-hoc, by combining posterior from already-sampled main model.
# This is intended to illustrate the robusticity of our results.

# # Upload and check the data
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

#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.--.-..--.-.-.-.-.

# When standardised across cultures "Together" - below. For analyzes based on data standardised within each culture, please, see script 5_...

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

# Data: 
data_AttrCHECK <- list(
  Sex = as.integer(ifelse(Eye$Sex=="F",1,2)),
  Grp = as.integer(as.factor(Eye$Culture_Year)),
  L_skin = scale(Eye$L_skin),
  L_iris = scale(Eye$L_iris),
  a_iris = scale(Eye$a_iris),
  b_iris = scale(Eye$b_iris),
  L_sclera = scale(Eye$L_sclera),
  a_sclera = scale(Eye$a_sclera),
  b_sclera = scale(Eye$b_sclera),
  MeasSext = scale(Eye$SexTypMeas),
  DIST = scale(Eye$LOCAL_DIST),
  Asym = scale(Eye$Asym),
  Age = scale(Eye$Age), 
  Attractiveness = scale(Eye$Attr)
)

data_Attr2 <- list(
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
  Attractiveness = stan_Attractiveness(Eye$Attr)
)

summary.data.frame(data_AttrCHECK)  
summary.data.frame(data_Attr2)
# It does the same thing...


# Analyses: 
#-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.
#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.

# We use an older version of rethinking, before cmdstanr was introduced: https://github.com/rmcelreath/rethinking?tab=readme-ov-file#within-chain-multithreading
# Should work well with the new version, but mind that the model's objects are not compatible between the new and old version. 

library(rethinking)

# 1 Attractiveness: 

m1_attr <- ulam( 
  alist(
    Attractiveness ~ normal(mu, sigma),
    
    mu <- mu1 + mu2,
    
    mu1 <- A[Sex] + 
      betas[Grp, 1 + (Sex - 1)*12] + # Intercept (fixed, then per-culture, per-Sex)
      BSexTyp[Sex]*MeasSext + betas[Grp, 2 + (Sex - 1)*12]*MeasSext + # Sex typicality
      BAsym[Sex]*Asym + betas[Grp, 3 + (Sex - 1)*12]*Asym + # Asymmetry
      BDIST [Sex]*DIST + betas[Grp, 4 + (Sex - 1)*12]*DIST + # Distinctiveness
      BAge[Sex]*Age + betas[Grp, 5 + (Sex - 1)*12]*Age + # Age
      B_L_Skin[Sex]*L_skin + betas[Grp, 6 + (Sex - 1)*12]*L_skin, # Skin lightness
    
    mu2 <-  B_L_iris[Sex]*L_iris + betas[Grp, 7 + (Sex - 1)*12]*L_iris + # Iris lightness
      B_a_iris[Sex]*a_iris + betas[Grp, 8 + (Sex - 1)*12]*a_iris + # Iris redness
      B_b_iris[Sex]*b_iris + betas[Grp, 9 + (Sex - 1)*12]*b_iris + # Iris yellowness
      B_L_sclera[Sex]*L_sclera + betas[Grp, 10 + (Sex - 1)*12]*L_sclera + # Sclera lightness
      B_a_sclera[Sex]*a_sclera + betas[Grp, 11 + (Sex - 1)*12]*a_sclera + # Sclera redness
      B_b_sclera[Sex]*b_sclera + betas[Grp, 12 + (Sex - 1)*12]*b_sclera,  # Sclera yellowness
    
    # Non-centered parameterization for betas
    transpars> matrix[Grp, 24]:betas <- compose_noncentered(sigma_group, L_Rho_group, z_group),
    
    matrix[24, Grp]:z_group ~ normal(0, 1),
    
    # Hyperpriors for the group-level effects
    vector[24]:sigma_group ~ dexp(1),
    cholesky_factor_corr[24]:L_Rho_group ~ lkj_corr_cholesky(2),
    
    A[Sex]  ~ normal(0, 0.2),
    vector[2]:BSexTyp ~ normal(0, 0.5),
    vector[2]:BDIST ~ normal(0, 0.5),
    vector[2]:BAsym ~ normal(0,0.5),
    vector[2]:BAge ~ normal(0, 0.5),
    vector[2]:B_L_Skin ~ normal(0, 0.5),
    vector[2]:B_L_iris ~ normal(0, 0.5),
    vector[2]:B_a_iris ~ normal(0, 0.5),
    vector[2]:B_b_iris ~ normal(0, 0.5),
    vector[2]:B_L_sclera ~ normal(0, 0.5),
    vector[2]:B_a_sclera ~ normal(0, 0.5),
    vector[2]:B_b_sclera ~ normal(0, 0.5),
    
    sigma ~ dexp(1),
    
    gq> matrix[24, 24]:Rho_group <<- Chol_to_Corr(L_Rho_group)
    
  ), data = data_Attr2, chains = 4, cores = 4, log_lik = T, iter = 2500, control = list(max_treedepth = 18, adapt_delta = 0.99)
)

M1_coefs <- precis(m1_attr, depth=3, prob=0.95)
write.csv2(M1_coefs, file="Model_1_ATTRACT_both_sexes_standardised_together_Coeftab.csv")
save(m1_attr, file="Model_1_ATTRACT_both_sexes_standardised_together.Rdata")

# Note, on the first run, it threw a warning "warning message: In doTryCatch(return(expr), name, parentenv, handler) : restarting 
# interrupted promise evaluation". Therefore, by accidnet, the model was run three times: First time using data scale using "scale" 
# function, then with custom-made function and finally to check whether the warning is, as the Internet says "just a thing that 
# sometimes pops-up as a consequence of background processess in the OS". That is why we know that for for L* of sclera, in women, 
# the effect is steadily close to 0.30, but the lower end of the 95 % CI tends to just over/under-run the zero. It has no statistical 
# importance, but mind this during re-analysis. You may get a result that is just / is just not credibly non-zero with regard to the 
# selected fraction of CI - 95 %. 


#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.

# 2 Sextypicality

Eye2 <- Eye1[!is.na(Eye1$SexT),] # I need Eye2 as a different object, since sextypicality has not been rated in Vietnam, 
# Then, Vietnam needs to be exluced from the data...
Eye  <- Eye2 

# SCALED AFTER EXCLUSION! 

# Data: 
data_sextCHECK <- list(
  Sex = as.integer(ifelse(Eye2$Sex=="F",1,2)),
  Grp = as.integer(as.factor(Eye2$Culture_Year)),
  L_skin = scale(Eye2$L_skin),
  L_iris = scale(Eye2$L_iris),
  a_iris = scale(Eye2$a_iris),
  b_iris = scale(Eye2$b_iris),
  L_sclera = scale(Eye2$L_sclera),
  a_sclera = scale(Eye2$a_sclera),
  b_sclera = scale(Eye2$b_sclera),
  MeasSext = scale(Eye2$SexTypMeas),
  DIST = scale(Eye2$LOCAL_DIST),
  Asym = scale(Eye2$Asym),
  Age = scale(Eye2$Age), 
  Sextypicality = scale(Eye2$SexT)
)


data_sext2 <- list(
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
  Sextypicality = stan_SextP(as.numeric(Eye$SexT))
)

summary.data.frame(data_sextCHECK)  
summary.data.frame(data_sext2)  



m2_Sext <- ulam(
  alist(
    Sextypicality ~ normal(mu, sigma),
    
    mu <- mu1 + mu2,
    
    mu1 <- A[Sex] + 
      betas[Grp, 1 + (Sex - 1)*12] + # Intercept (fixed, then per-culture, per-Sex)
      BSexTyp[Sex]*MeasSext + betas[Grp, 2 + (Sex - 1)*12]*MeasSext + # Sex typicality
      BAsym[Sex]*Asym + betas[Grp, 3 + (Sex - 1)*12]*Asym + # Asymmetry
      BDIST [Sex]*DIST + betas[Grp, 4 + (Sex - 1)*12]*DIST + # Distinctiveness
      BAge[Sex]*Age + betas[Grp, 5 + (Sex - 1)*12]*Age + # Age
      B_L_Skin[Sex]*L_skin + betas[Grp, 6 + (Sex - 1)*12]*L_skin, # Skin lightness
    
    mu2 <-  B_L_iris[Sex]*L_iris + betas[Grp, 7 + (Sex - 1)*12]*L_iris + # Iris lightness
      B_a_iris[Sex]*a_iris + betas[Grp, 8 + (Sex - 1)*12]*a_iris + # Iris redness
      B_b_iris[Sex]*b_iris + betas[Grp, 9 + (Sex - 1)*12]*b_iris + # Iris yellowness
      B_L_sclera[Sex]*L_sclera + betas[Grp, 10 + (Sex - 1)*12]*L_sclera + # Sclera lightness
      B_a_sclera[Sex]*a_sclera + betas[Grp, 11 + (Sex - 1)*12]*a_sclera + # Sclera redness
      B_b_sclera[Sex]*b_sclera + betas[Grp, 12 + (Sex - 1)*12]*b_sclera,  # Sclera yellowness
    
    # Non-centered parameterization for betas
    transpars> matrix[Grp, 24]:betas <- compose_noncentered(sigma_group, L_Rho_group, z_group),
    
    matrix[24, Grp]:z_group ~ normal(0, 1),
    
    # Hyperpriors for the group-level effects
    vector[24]:sigma_group ~ dexp(1),
    cholesky_factor_corr[24]:L_Rho_group ~ lkj_corr_cholesky(2),
    
    A[Sex]  ~ normal(0, 0.2),
    vector[2]:BSexTyp ~ normal(0, 0.5),
    vector[2]:BDIST ~ normal(0, 0.5),
    vector[2]:BAsym ~ normal(0,0.5),
    vector[2]:BAge ~ normal(0, 0.5),
    vector[2]:B_L_Skin ~ normal(0, 0.5),
    vector[2]:B_L_iris ~ normal(0, 0.5),
    vector[2]:B_a_iris ~ normal(0, 0.5),
    vector[2]:B_b_iris ~ normal(0, 0.5),
    vector[2]:B_L_sclera ~ normal(0, 0.5),
    vector[2]:B_a_sclera ~ normal(0, 0.5),
    vector[2]:B_b_sclera ~ normal(0, 0.5),
    
    sigma ~ dexp(1),
    
    gq> matrix[24, 24]:Rho_group <<- Chol_to_Corr(L_Rho_group)
    
  ), data = data_sext2, chains = 4, cores = 4, log_lik = T, iter = 5000, control = list(max_treedepth = 18, adapt_delta = 0.99)
)

M2_Scoefs <- precis(m2_Sext, depth=3, prob=0.95)
write.csv2(M2_Scoefs, file="Model_2_SEXTYP_both_sexes_standardised_together_Coeftab.csv")
save(m2_Sext, file="Model_2_SEXTYP_both_sexes_standardised_together.Rdata") 


#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.

# Path analyses to inspect association between iris ligtness, facial shape sextypicality,
# perceived sex-typicality and perceived attractiveness

# Sext -> Attr: Simple mediation analysis - run on the "Standardised together thing"

#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-..-.-.





# Women: 





# Data: 
data_AT_med <- data.frame(
  Sex = as.integer(ifelse(Eye2$Sex=="F",1,2)),
  Grp = as.integer(as.factor(Eye2$Culture_Year)),
  L_skin = scale(Eye2$L_skin),
  L_iris = scale(Eye2$L_iris),
  a_iris = scale(Eye2$a_iris),
  b_iris = scale(Eye2$b_iris),
  L_sclera = scale(Eye2$L_sclera),
  a_sclera = scale(Eye2$a_sclera),
  b_sclera = scale(Eye2$b_sclera),
  MeasSext = scale(Eye2$SexTypMeas),
  DIST = scale(Eye2$LOCAL_DIST),
  Asym = scale(Eye2$Asym),
  Age = scale(Eye2$Age), 
  Sextypicality = scale(Eye2$SexT),
  Attractiveness= scale(Eye2$Attr)
)

data_AT_med_F <- as.list(data_AT_med[data_AT_med$Sex==1,]) 
data_AT_med_M <- as.list(data_AT_med[data_AT_med$Sex==2,]) 


########
#      #
#  1F  #
#      #
########

# Female Attractiveness modelled as depending on Scleral L*, Perceived and Measured SexTypicality and Age.
# Perceived Female sextypicality modelled as depeneding on Scleral L*, measured SexTypicality and Age.

# Defining the model
fATR_mediation <- ulam(
  alist(
    # Model for Perceived Attractiveness
    Attractiveness ~ dnorm(mu_A, sigma_A),
    mu_A <- a_Attr + B_Light_A * L_sclera + B_sext_A * Sextypicality + b_SShD_A * MeasSext + b_Age_A * Age,
    
    # Model for Perceived Sex-typicality
    Sextypicality ~ dnorm(mu_Sp, sigma_Sp),
    mu_Sp <- a_Sext + B_Light_Sext * L_sclera + b_SShD_S * MeasSext + b_Age_S * Age,
    
    # Model for Measured Sex-typicality
    MeasSext ~ dnorm(mu_Ms, sigma_Ms),
    mu_Ms <- a_MeSx + b_Light_MeSx * L_sclera + b_Age_MeSx * Age,
    
    # Model for Scleral Lightness
    L_sclera ~ dnorm(mu_L, sigma_L),
    mu_L <- a_L + b_Age_L * Age,
    
    # Priors
    a_Attr ~ dnorm(0, 1),
    B_Light_A ~ dnorm(0, 1),
    B_sext_A ~ dnorm(0, 1),
    b_SShD_A ~ dnorm(0, 1),
    b_Age_A ~ dnorm(0, 1),
    sigma_A ~ dexp(1),
    
    a_Sext ~ dnorm(0, 1),
    B_Light_Sext ~ dnorm(0, 1),
    b_SShD_S ~ dnorm(0, 1),
    b_Age_S ~ dnorm(0, 1),
    sigma_Sp ~ dexp(1),
    
    a_MeSx ~ dnorm(0, 1),
    b_Light_MeSx ~ dnorm(0, 1),
    b_Age_MeSx ~ dnorm(0, 1),
    sigma_Ms ~ dexp(1),
    
    a_L ~ dnorm(0, 1),
    b_Age_L ~ dnorm(0, 1),
    sigma_L ~ dexp(1)
  ), data = data_AT_med_F, chains = 1, cores = 1, iter=1e4)


# Summarizing the model
precis(fATR_mediation, depth=3, prob=0.95)

########
#      #
#  2F  #
#      #
########

# Female Attractiveness modelled as depending on Scleral L*, Perceived and Measured SexTypicality and Age.
# Perceived Female sextypicality modelled as depending on measured SexTypicality and Age, but NOT Scleral L*. 

# Defining the model
fATR_mediation2 <- ulam(
  alist(
    # Model for Perceived Attractiveness
    Attractiveness ~ dnorm(mu_A, sigma_A),
    mu_A <- a_Attr + B_Light_A * L_sclera + B_sext_A * Sextypicality + b_SShD_A * MeasSext + b_Age_A * Age,
    
    # Model for Perceived Sex-typicality
    Sextypicality ~ dnorm(mu_Sp, sigma_Sp),
    mu_Sp <- a_Sext + b_SShD_S * MeasSext + b_Age_S * Age,
    
    # Model for Measured Sex-typicality
    MeasSext ~ dnorm(mu_Ms, sigma_Ms),
    mu_Ms <- a_MeSx + b_Light_MeSx * L_sclera + b_Age_MeSx * Age,
    
    # Model for Scleral Lightness
    L_sclera ~ dnorm(mu_L, sigma_L),
    mu_L <- a_L + b_Age_L * Age,
    
    # Priors
    a_Attr ~ dnorm(0, 1),
    B_Light_A ~ dnorm(0, 1),
    B_sext_A ~ dnorm(0, 1),
    b_SShD_A ~ dnorm(0, 1),
    b_Age_A ~ dnorm(0, 1),
    sigma_A ~ dexp(1),
    
    a_Sext ~ dnorm(0, 1),
    b_SShD_S ~ dnorm(0, 1),
    b_Age_S ~ dnorm(0, 1),
    sigma_Sp ~ dexp(1),
    
    a_MeSx ~ dnorm(0, 1),
    b_Light_MeSx ~ dnorm(0, 1),
    b_Age_MeSx ~ dnorm(0, 1),
    sigma_Ms ~ dexp(1),
    
    a_L ~ dnorm(0, 1),
    b_Age_L ~ dnorm(0, 1),
    sigma_L ~ dexp(1)
  ), data = data_AT_med_F, chains = 1, cores = 1, iter=1e4)

precis(fATR_mediation2, depth=3, prob=0.95)

########
#      #
#  3F  #
#      #
########

# Female Attractiveness modelled as depending on Perceived and Measured SexTypicality and Age, but not Scleral L*.
# Perceived Female sextypicality modelled as depending on measured SexTypicality and Age, but NOT Scleral L*. 

# Defining the model
fATR_mediation3 <- ulam(
  alist(
    # Model for Perceived Attractiveness
    Attractiveness ~ dnorm(mu_A, sigma_A),
    mu_A <- a_Attr + B_sext_A * Sextypicality + b_SShD_A * MeasSext + b_Age_A * Age,
    
    # Model for Perceived Sex-typicality
    Sextypicality ~ dnorm(mu_Sp, sigma_Sp),
    mu_Sp <- a_Sext + b_SShD_S * MeasSext + b_Age_S * Age,
    
    # Model for Measured Sex-typicality
    MeasSext ~ dnorm(mu_Ms, sigma_Ms),
    mu_Ms <- a_MeSx + b_Light_MeSx * L_sclera + b_Age_MeSx * Age,
    
    # Model for Scleral Lightness
    L_sclera ~ dnorm(mu_L, sigma_L),
    mu_L <- a_L + b_Age_L * Age,
    
    # Priors
    a_Attr ~ dnorm(0, 1),
    B_sext_A ~ dnorm(0, 1),
    b_SShD_A ~ dnorm(0, 1),
    b_Age_A ~ dnorm(0, 1),
    sigma_A ~ dexp(1),
    
    a_Sext ~ dnorm(0, 1),
    b_SShD_S ~ dnorm(0, 1),
    b_Age_S ~ dnorm(0, 1),
    sigma_Sp ~ dexp(1),
    
    a_MeSx ~ dnorm(0, 1),
    b_Light_MeSx ~ dnorm(0, 1),
    b_Age_MeSx ~ dnorm(0, 1),
    sigma_Ms ~ dexp(1),
    
    a_L ~ dnorm(0, 1),
    b_Age_L ~ dnorm(0, 1),
    sigma_L ~ dexp(1)
  ), data = data_AT_med_F, chains = 1, cores = 1, iter=1e4)

precis(fATR_mediation3, depth=2, prob=0.95)

########
#      #
#  4F  #
#      #
########

# Female Attractiveness modelled as depending on Scleral L*, and Measured SexTypicality and Age.
# Perceived Female sextypicality is not considered.

# Defining the model
fATR_mediation4 <- ulam(
  alist(
    # Model for Perceived Attractiveness
    Attractiveness ~ dnorm(mu_A, sigma_A),
    mu_A <- a_Attr + B_Light_A * L_sclera + b_SShD_A * MeasSext + b_Age_A * Age,
    
    # Model for Measured Sex-typicality
    MeasSext ~ dnorm(mu_Ms, sigma_Ms),
    mu_Ms <- a_MeSx + b_Light_MeSx * L_sclera + b_Age_MeSx * Age,
    
    # Model for Scleral Lightness
    L_sclera ~ dnorm(mu_L, sigma_L),
    mu_L <- a_L + b_Age_L * Age,
    
    # Priors
    a_Attr ~ dnorm(0, 1),
    B_Light_A ~ dnorm(0, 1),
    b_SShD_A ~ dnorm(0, 1),
    b_Age_A ~ dnorm(0, 1),
    sigma_A ~ dexp(1),
    
    a_MeSx ~ dnorm(0, 1),
    b_Light_MeSx ~ dnorm(0, 1),
    b_Age_MeSx ~ dnorm(0, 1),
    sigma_Ms ~ dexp(1),
    
    a_L ~ dnorm(0, 1),
    b_Age_L ~ dnorm(0, 1),
    sigma_L ~ dexp(1)
  ), data = data_AT_med_F, chains = 1, cores = 1, iter=1e4)


# Summarizing the model
precis(fATR_mediation4, depth=2, prob=0.95)

########
#      #
#  5F  #
#      #
########

# Perceived Sextypicality modelled as depending on measured Scleral L*, measured Sextypicality and age; attractiveness not considered.

# Defining the model
fATR_mediation5 <- ulam(
  alist(
    # Model for Perceived Sex-typicality
    Sextypicality ~ dnorm(mu_Sp, sigma_Sp),
    mu_Sp <- a_Sext + B_Light_Sext * L_sclera + b_SShD_S * MeasSext + b_Age_S * Age,
    
    # Model for Measured Sex-typicality
    MeasSext ~ dnorm(mu_Ms, sigma_Ms),
    mu_Ms <- a_MeSx + b_Light_MeSx * L_sclera + b_Age_MeSx * Age,
    
    # Model for Scleral Lightness
    L_sclera ~ dnorm(mu_L, sigma_L),
    mu_L <- a_L + b_Age_L * Age,
    
    # Priors
    a_Sext ~ dnorm(0, 1),
    B_Light_Sext ~ dnorm(0, 1),
    b_SShD_S ~ dnorm(0, 1),
    b_Age_S ~ dnorm(0, 1),
    sigma_Sp ~ dexp(1),
    
    a_MeSx ~ dnorm(0, 1),
    b_Light_MeSx ~ dnorm(0, 1),
    b_Age_MeSx ~ dnorm(0, 1),
    sigma_Ms ~ dexp(1),
    
    a_L ~ dnorm(0, 1),
    b_Age_L ~ dnorm(0, 1),
    sigma_L ~ dexp(1)
  ), data = data_AT_med_F, chains = 1, cores = 1, iter=1e4)


# Summarizing the model
precis(fATR_mediation5, depth=2, prob=0.95)


#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-..-.-.
# 
# "TRIANGLES":  L* ---> SexTyp ---> Attract 
#
#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-..-.-.


summary(as.data.frame(data_AT_med_F))

# Full triangle model
f_ATR_TRIANGLE <- ulam(
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
  ), data = data_AT_med_F, chains = 1, cores = 1, iter=1e4)

# Summarizing the model
precis(f_ATR_TRIANGLE, depth=2, prob=0.95)


# Delete the Sext ---> Attract branch: 
f_ATR_TRIANGLE_2 <- ulam(
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
    
  ), data = data_AT_med_F, chains = 1, cores = 1, iter=1e4)

# Summarizing the model
precis(f_ATR_TRIANGLE_2, depth=2, prob=0.95)



# Delete the Lightness ---> SexT branch: 
f_ATR_TRIANGLE_3 <- ulam(
  alist(
    # Model for Perceived Attractiveness
    Attractiveness ~ dnorm(mu_A, sigma_A),
    mu_A <- a_Attr + B_Light_A * L_sclera + B_sext_A * Sextypicality,
    
    # Priors
    a_Attr ~ dnorm(0, 1),
    B_Light_A ~ dnorm(0, 1),
    B_sext_A ~ dnorm(0, 1),
    sigma_A ~ dexp(1)
    
  ), data = data_AT_med_F, chains = 1, cores = 1, iter=1e4)

# Summarizing the model
precis(f_ATR_TRIANGLE_3, depth=2, prob=0.95)


# Defining the model
f_ATR_TRIANGLE_4 <- ulam(
  alist(
    # Model for Perceived Attractiveness
    Attractiveness ~ dnorm(mu_A, sigma_A),
    mu_A <- a_Attr + B_sext_A * Sextypicality,
    
    # Model for Perceived Sex-typicality
    Sextypicality ~ dnorm(mu_Sp, sigma_Sp),
    mu_Sp <- a_Sext + B_Light_Sext * L_sclera,
    
    # Priors
    a_Attr ~ dnorm(0, 1),
    B_sext_A ~ dnorm(0, 1),
    sigma_A ~ dexp(1),
    
    a_Sext ~ dnorm(0, 1),
    B_Light_Sext ~ dnorm(0, 1),
    sigma_Sp ~ dexp(1)
  ), data = data_AT_med_F, chains = 1, cores = 1, iter=1e4)

# Summarizing the model
precis(f_ATR_TRIANGLE_4, depth=2, prob=0.95)


# Defining the model
f_ATR_TRIANGLE_5 <- ulam(
  alist(
    # Model for Perceived Attractiveness
    Attractiveness ~ dnorm(mu_A, sigma_A),
    mu_A <- a_Attr + B_Light_Attr * L_sclera,
    
    a_Attr ~ dnorm(0, 1),
    B_Light_Attr ~ dnorm(0, 1),
    sigma_A ~ dexp(1)
    
  ), data = data_AT_med_F, chains = 1, cores = 1, iter=1e4)

# Summarizing the model
precis(f_ATR_TRIANGLE_5, depth=2, prob=0.95)


#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.



# MEN




#######
#     #
# 1M  #
#     #
#######

# Male Attractiveness modelled as depending on Scleral L*, Perceived and Measured SexTypicality and Age.
# Perceived male sextypicality modelled as depeneding on Scleral L*, measured SexTypicality and Age.

# Defining the model
mATR_mediation <- ulam(
  alist(
    # Model for Perceived Attractiveness
    Attractiveness ~ dnorm(mu_A, sigma_A),
    mu_A <- a_Attr + B_Light_A * L_sclera + B_sext_A * Sextypicality + b_SShD_A * MeasSext + b_Age_A * Age,
    
    # Model for Perceived Sex-typicality
    Sextypicality ~ dnorm(mu_Sp, sigma_Sp),
    mu_Sp <- a_Sext + B_Light_Sext * L_sclera + b_SShD_S * MeasSext + b_Age_S * Age,
    
    # Model for Measured Sex-typicality
    MeasSext ~ dnorm(mu_Ms, sigma_Ms),
    mu_Ms <- a_MeSx + b_Light_MeSx * L_sclera + b_Age_MeSx * Age,
    
    # Model for Scleral Lightness
    L_sclera ~ dnorm(mu_L, sigma_L),
    mu_L <- a_L + b_Age_L * Age,
    
    # Priors
    a_Attr ~ dnorm(0, 1),
    B_Light_A ~ dnorm(0, 1),
    B_sext_A ~ dnorm(0, 1),
    b_SShD_A ~ dnorm(0, 1),
    b_Age_A ~ dnorm(0, 1),
    sigma_A ~ dexp(1),
    
    a_Sext ~ dnorm(0, 1),
    B_Light_Sext ~ dnorm(0, 1),
    b_SShD_S ~ dnorm(0, 1),
    b_Age_S ~ dnorm(0, 1),
    sigma_Sp ~ dexp(1),
    
    a_MeSx ~ dnorm(0, 1),
    b_Light_MeSx ~ dnorm(0, 1),
    b_Age_MeSx ~ dnorm(0, 1),
    sigma_Ms ~ dexp(1),
    
    a_L ~ dnorm(0, 1),
    b_Age_L ~ dnorm(0, 1),
    sigma_L ~ dexp(1)
  ), data = data_AT_med_M, chains = 1, cores = 1, iter=1e4)


# Summarizing the model
precis(mATR_mediation, depth=3, prob=0.95)

########
#      #
#  2M  #
#      #
########

# Male Attractiveness modelled as depending on Scleral L*, Perceived and Measured SexTypicality and Age.
# Perceived Female sextypicality modelled as depending on measured SexTypicality and Age, but NOT Scleral L*. 

# Defining the model
mATR_mediation2 <- ulam(
  alist(
    # Model for Perceived Attractiveness
    Attractiveness ~ dnorm(mu_A, sigma_A),
    mu_A <- a_Attr + B_Light_A * L_sclera + B_sext_A * Sextypicality + b_SShD_A * MeasSext + b_Age_A * Age,
    
    # Model for Perceived Sex-typicality
    Sextypicality ~ dnorm(mu_Sp, sigma_Sp),
    mu_Sp <- a_Sext + b_SShD_S * MeasSext + b_Age_S * Age,
    
    # Model for Measured Sex-typicality
    MeasSext ~ dnorm(mu_Ms, sigma_Ms),
    mu_Ms <- a_MeSx + b_Light_MeSx * L_sclera + b_Age_MeSx * Age,
    
    # Model for Scleral Lightness
    L_sclera ~ dnorm(mu_L, sigma_L),
    mu_L <- a_L + b_Age_L * Age,
    
    # Priors
    a_Attr ~ dnorm(0, 1),
    B_Light_A ~ dnorm(0, 1),
    B_sext_A ~ dnorm(0, 1),
    b_SShD_A ~ dnorm(0, 1),
    b_Age_A ~ dnorm(0, 1),
    sigma_A ~ dexp(1),
    
    a_Sext ~ dnorm(0, 1),
    b_SShD_S ~ dnorm(0, 1),
    b_Age_S ~ dnorm(0, 1),
    sigma_Sp ~ dexp(1),
    
    a_MeSx ~ dnorm(0, 1),
    b_Light_MeSx ~ dnorm(0, 1),
    b_Age_MeSx ~ dnorm(0, 1),
    sigma_Ms ~ dexp(1),
    
    a_L ~ dnorm(0, 1),
    b_Age_L ~ dnorm(0, 1),
    sigma_L ~ dexp(1)
  ), data = data_AT_med_M, chains = 1, cores = 1, iter=1e4)

precis(mATR_mediation2, depth=3, prob=0.95)

########
#      #
#  3M  #
#      #
########

# Male Attractiveness modelled as depending on Perceived and Measured SexTypicality and Age, but not Scleral L*.
# Perceived male sextypicality modelled as depending on measured SexTypicality and Age, but NOT Scleral L*. 

# Defining the model
mATR_mediation3 <- ulam(
  alist(
    # Model for Perceived Attractiveness
    Attractiveness ~ dnorm(mu_A, sigma_A),
    mu_A <- a_Attr + B_sext_A * Sextypicality + b_SShD_A * MeasSext + b_Age_A * Age,
    
    # Model for Perceived Sex-typicality
    Sextypicality ~ dnorm(mu_Sp, sigma_Sp),
    mu_Sp <- a_Sext + b_SShD_S * MeasSext + b_Age_S * Age,
    
    # Model for Measured Sex-typicality
    MeasSext ~ dnorm(mu_Ms, sigma_Ms),
    mu_Ms <- a_MeSx + b_Light_MeSx * L_sclera + b_Age_MeSx * Age,
    
    # Model for Scleral Lightness
    L_sclera ~ dnorm(mu_L, sigma_L),
    mu_L <- a_L + b_Age_L * Age,
    
    # Priors
    a_Attr ~ dnorm(0, 1),
    B_sext_A ~ dnorm(0, 1),
    b_SShD_A ~ dnorm(0, 1),
    b_Age_A ~ dnorm(0, 1),
    sigma_A ~ dexp(1),
    
    a_Sext ~ dnorm(0, 1),
    b_SShD_S ~ dnorm(0, 1),
    b_Age_S ~ dnorm(0, 1),
    sigma_Sp ~ dexp(1),
    
    a_MeSx ~ dnorm(0, 1),
    b_Light_MeSx ~ dnorm(0, 1),
    b_Age_MeSx ~ dnorm(0, 1),
    sigma_Ms ~ dexp(1),
    
    a_L ~ dnorm(0, 1),
    b_Age_L ~ dnorm(0, 1),
    sigma_L ~ dexp(1)
  ), data = data_AT_med_M, chains = 1, cores = 1, iter=1e4)

precis(mATR_mediation3, depth=2, prob=0.95)

########
#      #
#  4M  #
#      #
########

# Male Attractiveness modelled as depending on Scleral L*, and Measured SexTypicality and Age.
# Perceived male sextypicality is not considered.

# Defining the model
mATR_mediation4 <- ulam(
  alist(
    # Model for Perceived Attractiveness
    Attractiveness ~ dnorm(mu_A, sigma_A),
    mu_A <- a_Attr + B_Light_A * L_sclera + b_SShD_A * MeasSext + b_Age_A * Age,
    
    # Model for Measured Sex-typicality
    MeasSext ~ dnorm(mu_Ms, sigma_Ms),
    mu_Ms <- a_MeSx + b_Light_MeSx * L_sclera + b_Age_MeSx * Age,
    
    # Model for Scleral Lightness
    L_sclera ~ dnorm(mu_L, sigma_L),
    mu_L <- a_L + b_Age_L * Age,
    
    # Priors
    a_Attr ~ dnorm(0, 1),
    B_Light_A ~ dnorm(0, 1),
    b_SShD_A ~ dnorm(0, 1),
    b_Age_A ~ dnorm(0, 1),
    sigma_A ~ dexp(1),
    
    a_MeSx ~ dnorm(0, 1),
    b_Light_MeSx ~ dnorm(0, 1),
    b_Age_MeSx ~ dnorm(0, 1),
    sigma_Ms ~ dexp(1),
    
    a_L ~ dnorm(0, 1),
    b_Age_L ~ dnorm(0, 1),
    sigma_L ~ dexp(1)
  ), data = data_AT_med_M, chains = 1, cores = 1, iter=1e4)


# Summarizing the model
precis(mATR_mediation4, depth=2, prob=0.95)

########
#      #
#  5M  #
#      #
########

# Perceived Sextypicality modelled as depending on measured Scleral L*, measured Sextypicality and age; attractiveness not considered.

# Defining the model
mATR_mediation5 <- ulam(
  alist(
    # Model for Perceived Sex-typicality
    Sextypicality ~ dnorm(mu_Sp, sigma_Sp),
    mu_Sp <- a_Sext + B_Light_Sext * L_sclera + b_SShD_S * MeasSext + b_Age_S * Age,
    
    # Model for Measured Sex-typicality
    MeasSext ~ dnorm(mu_Ms, sigma_Ms),
    mu_Ms <- a_MeSx + b_Light_MeSx * L_sclera + b_Age_MeSx * Age,
    
    # Model for Scleral Lightness
    L_sclera ~ dnorm(mu_L, sigma_L),
    mu_L <- a_L + b_Age_L * Age,
    
    # Priors
    a_Sext ~ dnorm(0, 1),
    B_Light_Sext ~ dnorm(0, 1),
    b_SShD_S ~ dnorm(0, 1),
    b_Age_S ~ dnorm(0, 1),
    sigma_Sp ~ dexp(1),
    
    a_MeSx ~ dnorm(0, 1),
    b_Light_MeSx ~ dnorm(0, 1),
    b_Age_MeSx ~ dnorm(0, 1),
    sigma_Ms ~ dexp(1),
    
    a_L ~ dnorm(0, 1),
    b_Age_L ~ dnorm(0, 1),
    sigma_L ~ dexp(1)
  ), data = data_AT_med_M, chains = 1, cores = 1, iter=1e4)


# Summarizing the model
precis(mATR_mediation5, depth=2, prob=0.95)


#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-..-.-.
# 
# "TRIANGLES":  L* ---> SexTyp ---> Attract 
#
#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-..-.-.

summary(as.data.frame(data_AT_med_M))

# Full triangle model

# Defining the model
m_ATR_TRIANGLE <- ulam(
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
  ), data = data_AT_med_M, chains = 1, cores = 1, iter=1e4)

# Summarizing the model
precis(m_ATR_TRIANGLE, depth=2, prob=0.95)

# Delete the Sext ---> Attract branch: 
m_ATR_TRIANGLE_2 <- ulam(
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
    
  ), data = data_AT_med_M, chains = 1, cores = 1, iter=1e4)

# Summarizing the model
precis(m_ATR_TRIANGLE_2, depth=2, prob=0.95)


# Delete the Lightness ---> SexT branch: 
m_ATR_TRIANGLE_3 <- ulam(
  alist(
    # Model for Perceived Attractiveness
    Attractiveness ~ dnorm(mu_A, sigma_A),
    mu_A <- a_Attr + B_Light_A * L_sclera + B_sext_A * Sextypicality,
    
    # Priors
    a_Attr ~ dnorm(0, 1),
    B_Light_A ~ dnorm(0, 1),
    B_sext_A ~ dnorm(0, 1),
    sigma_A ~ dexp(1)
    
  ), data = data_AT_med_M, chains = 1, cores = 1, iter=1e4)

# Summarizing the model
precis(m_ATR_TRIANGLE_3, depth=2, prob=0.95)


# Defining the model
m_ATR_TRIANGLE_4 <- ulam(
  alist(
    # Model for Perceived Attractiveness
    Attractiveness ~ dnorm(mu_A, sigma_A),
    mu_A <- a_Attr + B_sext_A * Sextypicality,
    
    # Model for Perceived Sex-typicality
    Sextypicality ~ dnorm(mu_Sp, sigma_Sp),
    mu_Sp <- a_Sext + B_Light_Sext * L_sclera,
    
    # Priors
    a_Attr ~ dnorm(0, 1),
    B_sext_A ~ dnorm(0, 1),
    sigma_A ~ dexp(1),
    
    a_Sext ~ dnorm(0, 1),
    B_Light_Sext ~ dnorm(0, 1),
    sigma_Sp ~ dexp(1)
  ), data = data_AT_med_M, chains = 1, cores = 1, iter=1e4)

# Summarizing the model
precis(m_ATR_TRIANGLE_4, depth=2, prob=0.95)


# Defining the model
m_ATR_TRIANGLE_5 <- ulam(
  alist(
    # Model for Perceived Attractiveness
    Attractiveness ~ dnorm(mu_A, sigma_A),
    mu_A <- a_Attr + B_Light_Attr * L_sclera,
    
    a_Attr ~ dnorm(0, 1),
    B_Light_Attr ~ dnorm(0, 1),
    sigma_A ~ dexp(1)
    
  ), data = data_AT_med_M, chains = 1, cores = 1, iter=1e4)

# Summarizing the model
precis(m_ATR_TRIANGLE_5, depth=2, prob=0.95)






#-------------------------------------------------------------------------------
# Alternative analysis for reviews on the 25th of August, 2025

# CZ Samples pooled
# CMR Samples pooled
# All other samples kept separately

# SCALED TOGETHER: Attractiveness

levels(as.factor(Eye$Culture_Year))

# Let's collapse... it: 
Eye$Group2 <- Eye$Culture_Year

# Collapse Czech_2016 and Czech_2019 → Czech
Eye$Group2 <- sub("^Czech_201[69]", "Czech", Eye$Group2)

# Collapse Cameroon_2013 and Cameroon_2016 → Cameroon
Eye$Group2 <- sub("^Cameroon_201[36]", "Cameroon", Eye$Group2)

levels(as.factor(Eye$Group2))

# Data: 
data_Attr2 <- list(
  Sex = as.integer(ifelse(Eye$Sex=="F",1,2)),
  Grp = as.integer(as.factor(Eye$Group2)),
  L_skin = scale(Eye$L_skin),
  L_iris = scale(Eye$L_iris),
  a_iris = scale(Eye$a_iris),
  b_iris = scale(Eye$b_iris),
  L_sclera = scale(Eye$L_sclera),
  a_sclera = scale(Eye$a_sclera),
  b_sclera = scale(Eye$b_sclera),
  MeasSext = scale(Eye$SexTypMeas),
  DIST = scale(Eye$LOCAL_DIST),
  Asym = scale(Eye$Asym),
  Age = scale(Eye$Age), 
  Attractiveness = scale(Eye$Attr)
)

summary.data.frame(data_Attr2)



library(rethinking)

# 1 Attractiveness: 14 in the title stands for 14 groups we have now...

m14_attr <- ulam( 
  alist(
    Attractiveness ~ normal(mu, sigma),
    
    mu <- mu1 + mu2,
    
    mu1 <- A[Sex] + 
      betas[Grp, 1 + (Sex - 1)*12] + # Intercept (fixed, then per-culture, per-Sex)
      BSexTyp[Sex]*MeasSext + betas[Grp, 2 + (Sex - 1)*12]*MeasSext + # Sex typicality
      BAsym[Sex]*Asym + betas[Grp, 3 + (Sex - 1)*12]*Asym + # Asymmetry
      BDIST [Sex]*DIST + betas[Grp, 4 + (Sex - 1)*12]*DIST + # Distinctiveness
      BAge[Sex]*Age + betas[Grp, 5 + (Sex - 1)*12]*Age + # Age
      B_L_Skin[Sex]*L_skin + betas[Grp, 6 + (Sex - 1)*12]*L_skin, # Skin lightness
    
    mu2 <-  B_L_iris[Sex]*L_iris + betas[Grp, 7 + (Sex - 1)*12]*L_iris + # Iris lightness
      B_a_iris[Sex]*a_iris + betas[Grp, 8 + (Sex - 1)*12]*a_iris + # Iris redness
      B_b_iris[Sex]*b_iris + betas[Grp, 9 + (Sex - 1)*12]*b_iris + # Iris yellowness
      B_L_sclera[Sex]*L_sclera + betas[Grp, 10 + (Sex - 1)*12]*L_sclera + # Sclera lightness
      B_a_sclera[Sex]*a_sclera + betas[Grp, 11 + (Sex - 1)*12]*a_sclera + # Sclera redness
      B_b_sclera[Sex]*b_sclera + betas[Grp, 12 + (Sex - 1)*12]*b_sclera,  # Sclera yellowness
    
    # Non-centered parameterization for betas
    transpars> matrix[Grp, 24]:betas <- compose_noncentered(sigma_group, L_Rho_group, z_group),
    
    matrix[24, Grp]:z_group ~ normal(0, 1),
    
    # Hyperpriors for the group-level effects
    vector[24]:sigma_group ~ dexp(1),
    cholesky_factor_corr[24]:L_Rho_group ~ lkj_corr_cholesky(2),
    
    A[Sex]  ~ normal(0, 0.2),
    vector[2]:BSexTyp ~ normal(0, 0.5),
    vector[2]:BDIST ~ normal(0, 0.5),
    vector[2]:BAsym ~ normal(0,0.5),
    vector[2]:BAge ~ normal(0, 0.5),
    vector[2]:B_L_Skin ~ normal(0, 0.5),
    vector[2]:B_L_iris ~ normal(0, 0.5),
    vector[2]:B_a_iris ~ normal(0, 0.5),
    vector[2]:B_b_iris ~ normal(0, 0.5),
    vector[2]:B_L_sclera ~ normal(0, 0.5),
    vector[2]:B_a_sclera ~ normal(0, 0.5),
    vector[2]:B_b_sclera ~ normal(0, 0.5),
    
    sigma ~ dexp(1),
    
    gq> matrix[24, 24]:Rho_group <<- Chol_to_Corr(L_Rho_group)
    
  ), data = data_Attr2, chains = 4, cores = 4, log_lik = T, iter = 2500, control = list(max_treedepth = 18, adapt_delta = 0.99)
)

M14_coefs <- precis(m14_attr, depth=3, prob=0.95)
write.csv2(M14_coefs, file="Model_14groups_ATTRACT_both_sexes_standardised_together_Coeftab.csv")
M14_post <- extract.samples(m14_attr)
saveRDS(M14_post, file="post_Model_14groups_ATTRACT_both_sexes_standardised_togetherg.RDS")
save(m14_attr, file="Model_14groups_ATTRACT_both_sexes_standardised_together.Rdata")



#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.
# Sextypicality: 

Eye2 <- Eye1[!is.na(Eye1$SexT),] # No Vietnam data...

levels(as.factor(Eye2$Culture_Year))

# Let's collapse... it: 
Eye2$Group2 <- Eye2$Culture_Year

# Collapse Czech_2016 and Czech_2019 → Czech
Eye2$Group2 <- sub("^Czech_201[69]", "Czech", Eye2$Group2)

# Collapse Cameroon_2013 and Cameroon_2016 → Cameroon
Eye2$Group2 <- sub("^Cameroon_201[36]", "Cameroon", Eye2$Group2)

levels(as.factor(Eye2$Group2))



# Data: 
data_sextCHECK <- list(
  Sex = as.integer(ifelse(Eye2$Sex=="F",1,2)),
  Grp = as.integer(as.factor(Eye2$Group2)),
  L_skin = scale(Eye2$L_skin),
  L_iris = scale(Eye2$L_iris),
  a_iris = scale(Eye2$a_iris),
  b_iris = scale(Eye2$b_iris),
  L_sclera = scale(Eye2$L_sclera),
  a_sclera = scale(Eye2$a_sclera),
  b_sclera = scale(Eye2$b_sclera),
  MeasSext = scale(Eye2$SexTypMeas),
  DIST = scale(Eye2$LOCAL_DIST),
  Asym = scale(Eye2$Asym),
  Age = scale(Eye2$Age), 
  Sextypicality = scale(Eye2$SexT)
)

summary.data.frame(data_sextCHECK)


m12_Sext <- ulam(
  alist(
    Sextypicality ~ normal(mu, sigma),
    
    mu <- mu1 + mu2,
    
    mu1 <- A[Sex] + 
      betas[Grp, 1 + (Sex - 1)*12] + # Intercept (fixed, then per-culture, per-Sex)
      BSexTyp[Sex]*MeasSext + betas[Grp, 2 + (Sex - 1)*12]*MeasSext + # Sex typicality
      BAsym[Sex]*Asym + betas[Grp, 3 + (Sex - 1)*12]*Asym + # Asymmetry
      BDIST [Sex]*DIST + betas[Grp, 4 + (Sex - 1)*12]*DIST + # Distinctiveness
      BAge[Sex]*Age + betas[Grp, 5 + (Sex - 1)*12]*Age + # Age
      B_L_Skin[Sex]*L_skin + betas[Grp, 6 + (Sex - 1)*12]*L_skin, # Skin lightness
    
    mu2 <-  B_L_iris[Sex]*L_iris + betas[Grp, 7 + (Sex - 1)*12]*L_iris + # Iris lightness
      B_a_iris[Sex]*a_iris + betas[Grp, 8 + (Sex - 1)*12]*a_iris + # Iris redness
      B_b_iris[Sex]*b_iris + betas[Grp, 9 + (Sex - 1)*12]*b_iris + # Iris yellowness
      B_L_sclera[Sex]*L_sclera + betas[Grp, 10 + (Sex - 1)*12]*L_sclera + # Sclera lightness
      B_a_sclera[Sex]*a_sclera + betas[Grp, 11 + (Sex - 1)*12]*a_sclera + # Sclera redness
      B_b_sclera[Sex]*b_sclera + betas[Grp, 12 + (Sex - 1)*12]*b_sclera,  # Sclera yellowness
    
    # Non-centered parameterization for betas
    transpars> matrix[Grp, 24]:betas <- compose_noncentered(sigma_group, L_Rho_group, z_group),
    
    matrix[24, Grp]:z_group ~ normal(0, 1),
    
    # Hyperpriors for the group-level effects
    vector[24]:sigma_group ~ dexp(1),
    cholesky_factor_corr[24]:L_Rho_group ~ lkj_corr_cholesky(2),
    
    A[Sex]  ~ normal(0, 0.2),
    vector[2]:BSexTyp ~ normal(0, 0.5),
    vector[2]:BDIST ~ normal(0, 0.5),
    vector[2]:BAsym ~ normal(0,0.5),
    vector[2]:BAge ~ normal(0, 0.5),
    vector[2]:B_L_Skin ~ normal(0, 0.5),
    vector[2]:B_L_iris ~ normal(0, 0.5),
    vector[2]:B_a_iris ~ normal(0, 0.5),
    vector[2]:B_b_iris ~ normal(0, 0.5),
    vector[2]:B_L_sclera ~ normal(0, 0.5),
    vector[2]:B_a_sclera ~ normal(0, 0.5),
    vector[2]:B_b_sclera ~ normal(0, 0.5),
    
    sigma ~ dexp(1),
    
    gq> matrix[24, 24]:Rho_group <<- Chol_to_Corr(L_Rho_group)
    
  ), data = data_sextCHECK, chains = 4, cores = 4, log_lik = T, iter = 2500, control = list(max_treedepth = 18, adapt_delta = 0.99)
)

M12_Scoefs <- precis(m12_Sext, depth=3, prob=0.95)
write.csv2(M12_Scoefs, file="Model_12groups_SEXTYP_both_sexes_standardised_together_Coeftab.csv")
M12_post <- extract.samples(m12_Sext)
saveRDS(M12_post, file="post_Model_12groups_SEXTYP_both_sexes_standardised_togetherg.RDS")
save(m12_Sext, file="Model_12groups_SEXTYP_both_sexes_standardised_together.Rdata") 




# Attractiveness - standardised together - even lower number of groups: 
# CZ Samples pooled
# CMR Samples pooled
# India + Turkey + Iran + Vietnam pooled
# Colombia left as is...

levels(as.factor(Eye$Culture_Year))

# Let's collapse... it: 
Eye$Group2 <- Eye$Culture_Year

# Collapse Czech_2016 and Czech_2019 → Czech
Eye$Group2 <- sub("^Czech_201[69]", "Czech", Eye$Group2)

# Collapse Cameroon_2013 and Cameroon_2016 → Cameroon
Eye$Group2 <- sub("^Cameroon_201[36]", "Cameroon", Eye$Group2)

levels(as.factor(Eye$Group2))

Eye$Group3 <- ifelse(Eye$Group2=="Cameroon","Cameroon",
                     ifelse(Eye$Group2=="Colombia","Colombia",
                            ifelse(Eye$Group2=="Czech","Czech","Asia")))

levels(as.factor(Eye$Group3))


# Data: 
data_Attr2 <- list(
  Sex = as.integer(ifelse(Eye$Sex=="F",1,2)),
  Grp = as.integer(as.factor(Eye$Group3)),
  L_skin = scale(Eye$L_skin),
  L_iris = scale(Eye$L_iris),
  a_iris = scale(Eye$a_iris),
  b_iris = scale(Eye$b_iris),
  L_sclera = scale(Eye$L_sclera),
  a_sclera = scale(Eye$a_sclera),
  b_sclera = scale(Eye$b_sclera),
  MeasSext = scale(Eye$SexTypMeas),
  DIST = scale(Eye$LOCAL_DIST),
  Asym = scale(Eye$Asym),
  Age = scale(Eye$Age), 
  Attractiveness = scale(Eye$Attr)
)

summary.data.frame(data_Attr2)



library(rethinking)

# 1 Attractiveness: 8 in the title stands for 8 groups we have now...

m8g_attr <- ulam( 
  alist(
    Attractiveness ~ normal(mu, sigma),
    
    mu <- mu1 + mu2,
    
    mu1 <- A[Sex] + 
      betas[Grp, 1 + (Sex - 1)*12] + # Intercept (fixed, then per-culture, per-Sex)
      BSexTyp[Sex]*MeasSext + betas[Grp, 2 + (Sex - 1)*12]*MeasSext + # Sex typicality
      BAsym[Sex]*Asym + betas[Grp, 3 + (Sex - 1)*12]*Asym + # Asymmetry
      BDIST [Sex]*DIST + betas[Grp, 4 + (Sex - 1)*12]*DIST + # Distinctiveness
      BAge[Sex]*Age + betas[Grp, 5 + (Sex - 1)*12]*Age + # Age
      B_L_Skin[Sex]*L_skin + betas[Grp, 6 + (Sex - 1)*12]*L_skin, # Skin lightness
    
    mu2 <-  B_L_iris[Sex]*L_iris + betas[Grp, 7 + (Sex - 1)*12]*L_iris + # Iris lightness
      B_a_iris[Sex]*a_iris + betas[Grp, 8 + (Sex - 1)*12]*a_iris + # Iris redness
      B_b_iris[Sex]*b_iris + betas[Grp, 9 + (Sex - 1)*12]*b_iris + # Iris yellowness
      B_L_sclera[Sex]*L_sclera + betas[Grp, 10 + (Sex - 1)*12]*L_sclera + # Sclera lightness
      B_a_sclera[Sex]*a_sclera + betas[Grp, 11 + (Sex - 1)*12]*a_sclera + # Sclera redness
      B_b_sclera[Sex]*b_sclera + betas[Grp, 12 + (Sex - 1)*12]*b_sclera,  # Sclera yellowness
    
    # Non-centered parameterization for betas
    transpars> matrix[Grp, 24]:betas <- compose_noncentered(sigma_group, L_Rho_group, z_group),
    
    matrix[24, Grp]:z_group ~ normal(0, 1),
    
    # Hyperpriors for the group-level effects
    vector[24]:sigma_group ~ dexp(1),
    cholesky_factor_corr[24]:L_Rho_group ~ lkj_corr_cholesky(2),
    
    A[Sex]  ~ normal(0, 0.2),
    vector[2]:BSexTyp ~ normal(0, 0.5),
    vector[2]:BDIST ~ normal(0, 0.5),
    vector[2]:BAsym ~ normal(0,0.5),
    vector[2]:BAge ~ normal(0, 0.5),
    vector[2]:B_L_Skin ~ normal(0, 0.5),
    vector[2]:B_L_iris ~ normal(0, 0.5),
    vector[2]:B_a_iris ~ normal(0, 0.5),
    vector[2]:B_b_iris ~ normal(0, 0.5),
    vector[2]:B_L_sclera ~ normal(0, 0.5),
    vector[2]:B_a_sclera ~ normal(0, 0.5),
    vector[2]:B_b_sclera ~ normal(0, 0.5),
    
    sigma ~ dexp(1),
    
    gq> matrix[24, 24]:Rho_group <<- Chol_to_Corr(L_Rho_group)
    
  ), data = data_Attr2, chains = 4, cores = 4, log_lik = T, iter = 2500, control = list(max_treedepth = 18, adapt_delta = 0.99)
)

M8g_coefs <- precis(m8g_attr, depth=3, prob=0.95)
write.csv2(M8g_coefs, file="Model_8groups_ATTRACT_both_sexes_standardised_together_Coeftab.csv")
M8ga_post <- extract.samples(m8g_attr)
saveRDS(M8ga_post, file="post_Model_8groups_ATTRACT_both_sexes_standardised_togetherg.RDS")
save(m8g_attr, file="Model_8groups_ATTRACT_both_sexes_standardised_together.Rdata") 


#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.
# Sextypicality: 

Eye2 <- Eye1[!is.na(Eye1$SexT),] # No Vietnam data...

levels(as.factor(Eye2$Culture_Year))

# Let's collapse... it: 
Eye2$Group2 <- Eye2$Culture_Year

# Collapse Czech_2016 and Czech_2019 → Czech
Eye2$Group2 <- sub("^Czech_201[69]", "Czech", Eye2$Group2)

# Collapse Cameroon_2013 and Cameroon_2016 → Cameroon
Eye2$Group2 <- sub("^Cameroon_201[36]", "Cameroon", Eye2$Group2)

levels(as.factor(Eye2$Group2))


Eye2$Group3 <- ifelse(Eye2$Group2=="Cameroon","Cameroon",
                     ifelse(Eye2$Group2=="Colombia","Colombia",
                            ifelse(Eye2$Group2=="Czech","Czech","Asia")))

levels(as.factor(Eye2$Group3))


# Data: 
data_sextCHECK <- list(
  Sex = as.integer(ifelse(Eye2$Sex=="F",1,2)),
  Grp = as.integer(as.factor(Eye2$Group3)),
  L_skin = scale(Eye2$L_skin),
  L_iris = scale(Eye2$L_iris),
  a_iris = scale(Eye2$a_iris),
  b_iris = scale(Eye2$b_iris),
  L_sclera = scale(Eye2$L_sclera),
  a_sclera = scale(Eye2$a_sclera),
  b_sclera = scale(Eye2$b_sclera),
  MeasSext = scale(Eye2$SexTypMeas),
  DIST = scale(Eye2$LOCAL_DIST),
  Asym = scale(Eye2$Asym),
  Age = scale(Eye2$Age), 
  Sextypicality = scale(Eye2$SexT)
)

summary.data.frame(data_sextCHECK)


m8g_Sext <- ulam(
  alist(
    Sextypicality ~ normal(mu, sigma),
    
    mu <- mu1 + mu2,
    
    mu1 <- A[Sex] + 
      betas[Grp, 1 + (Sex - 1)*12] + # Intercept (fixed, then per-culture, per-Sex)
      BSexTyp[Sex]*MeasSext + betas[Grp, 2 + (Sex - 1)*12]*MeasSext + # Sex typicality
      BAsym[Sex]*Asym + betas[Grp, 3 + (Sex - 1)*12]*Asym + # Asymmetry
      BDIST [Sex]*DIST + betas[Grp, 4 + (Sex - 1)*12]*DIST + # Distinctiveness
      BAge[Sex]*Age + betas[Grp, 5 + (Sex - 1)*12]*Age + # Age
      B_L_Skin[Sex]*L_skin + betas[Grp, 6 + (Sex - 1)*12]*L_skin, # Skin lightness
    
    mu2 <-  B_L_iris[Sex]*L_iris + betas[Grp, 7 + (Sex - 1)*12]*L_iris + # Iris lightness
      B_a_iris[Sex]*a_iris + betas[Grp, 8 + (Sex - 1)*12]*a_iris + # Iris redness
      B_b_iris[Sex]*b_iris + betas[Grp, 9 + (Sex - 1)*12]*b_iris + # Iris yellowness
      B_L_sclera[Sex]*L_sclera + betas[Grp, 10 + (Sex - 1)*12]*L_sclera + # Sclera lightness
      B_a_sclera[Sex]*a_sclera + betas[Grp, 11 + (Sex - 1)*12]*a_sclera + # Sclera redness
      B_b_sclera[Sex]*b_sclera + betas[Grp, 12 + (Sex - 1)*12]*b_sclera,  # Sclera yellowness
    
    # Non-centered parameterization for betas
    transpars> matrix[Grp, 24]:betas <- compose_noncentered(sigma_group, L_Rho_group, z_group),
    
    matrix[24, Grp]:z_group ~ normal(0, 1),
    
    # Hyperpriors for the group-level effects
    vector[24]:sigma_group ~ dexp(1),
    cholesky_factor_corr[24]:L_Rho_group ~ lkj_corr_cholesky(2),
    
    A[Sex]  ~ normal(0, 0.2),
    vector[2]:BSexTyp ~ normal(0, 0.5),
    vector[2]:BDIST ~ normal(0, 0.5),
    vector[2]:BAsym ~ normal(0,0.5),
    vector[2]:BAge ~ normal(0, 0.5),
    vector[2]:B_L_Skin ~ normal(0, 0.5),
    vector[2]:B_L_iris ~ normal(0, 0.5),
    vector[2]:B_a_iris ~ normal(0, 0.5),
    vector[2]:B_b_iris ~ normal(0, 0.5),
    vector[2]:B_L_sclera ~ normal(0, 0.5),
    vector[2]:B_a_sclera ~ normal(0, 0.5),
    vector[2]:B_b_sclera ~ normal(0, 0.5),
    
    sigma ~ dexp(1),
    
    gq> matrix[24, 24]:Rho_group <<- Chol_to_Corr(L_Rho_group)
    
  ), data = data_sextCHECK, chains = 4, cores = 4, log_lik = T, iter = 2500, control = list(max_treedepth = 18, adapt_delta = 0.99)
)

M8g_Scoefs <- precis(m8g_Sext, depth=3, prob=0.95)
write.csv2(M8g_Scoefs, file="Model_8groups_SEXTYP_both_sexes_standardised_together_Coeftab.csv")
M8gs_post <- extract.samples(m8g_attr)
saveRDS(M8gs_post, file="post_Model_8groups_SEXTYP_both_sexes_standardised_togetherg.RDS")
save(m8g_Sext, file="Model_8groups_SEXTYP_both_sexes_standardised_together.Rdata") 


