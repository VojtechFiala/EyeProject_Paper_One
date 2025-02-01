#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.

# When standardised across cultures "Together" - This time, we (i) Exclude Iranians due to concerns on 
# the quality of morphometric data; (ii) Exclude skin L* from the analysis to see if the results are robust to this change.

#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.


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

#-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.--.-..--.-.-.-.-.

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

# Exclude Iranians - but not yet the Vietnamese. We've got their attractiveness ratings! 

Eye_IR <- Eye[Eye$Culture_Year!="Iran",]
Eye <- Eye_IR
levels(as.factor(Eye$Culture_Year))

# Data: 
data_Attr <- list(
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

summary.data.frame(data_Attr)

library(rethinking)

# Analyses: 
#-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.

#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.
# Models without Iranians 
#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.

# Attractiveness

m9_attr <- ulam( 
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
    
  ), data = data_Attr, chains = 8, cores = 8, log_lik = T, iter = 2500, control = list(max_treedepth = 18, adapt_delta = 0.99)
)

M9_coefs <- precis(m9_attr, depth=3, prob=0.95)
write.csv2(M9_coefs, file="Model_9_ATTRACT_both_sexes_standardised_together_without_Iranians_Coeftab.csv")
#save(m9_attr, file="Model_9_ATTRACT_both_sexes_standardised_together_without_Iranians.Rdata")



#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.
#
# Sextypicality
#
#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.


Eye <- Eye[!is.na(Eye$SexT),] # I need Eye2 as a different object, since sextypicality has not been rated in Vietnam, 
# Then, Vietnam needs to be exluced from the data...

# SCALED AFTER EXCLUSION! 

data_sext <- list(
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

summary.data.frame(data_sext)  # seven groups = without Iran (on purpose) and Vietnam (no data since the beginning) 


m10_Sext <- ulam(
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
    
  ), data = data_sext, chains = 8, cores = 8, log_lik = T, iter = 2500, control = list(max_treedepth = 18, adapt_delta = 0.99)
)

M10_Scoefs <- precis(m10_Sext, depth=3, prob=0.95)
write.csv2(M10_Scoefs, file="Model_10_SEXTYP_both_sexes_standardised_together_without_IraniansCoeftab.csv")
save(m10_Sext, file="Model_10_SEXTYP_both_sexes_standardised_together_without_Iranians.Rdata") 



#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.
#
# Models with skin L* excluded - But Iranians are back
#
#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.

Eye <- Eye1
levels(as.factor(Eye$Culture_Year)) # Iranians' back


# Data: 
data_Attr <- list(
  Sex = as.integer(ifelse(Eye$Sex=="F",1,2)),
  Grp = as.integer(as.factor(Eye$Culture_Year)),
 # L_skin = stan_L_skin(Eye$L_skin),
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

summary.data.frame(data_Attr)

#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.
# Attractiveness

m11_attr <- ulam(
  alist(
    Attractiveness ~ normal(mu, sigma),
    
    mu <- mu1 + mu2,
    
    mu1 <- A[Sex] + 
      betas[Grp, 1 + (Sex - 1)*11] + # Intercept (fixed, then per-culture, per-Sex)
      BSexTyp[Sex]*MeasSext + betas[Grp, 2 + (Sex - 1)*11]*MeasSext + # Sex typicality
      BAsym[Sex]*Asym + betas[Grp, 3 + (Sex - 1)*11]*Asym + # Asymmetry
      BDIST [Sex]*DIST + betas[Grp, 4 + (Sex - 1)*11]*DIST + # Distinctiveness
      BAge[Sex]*Age + betas[Grp, 5 + (Sex - 1)*11]*Age, # Age
    
    mu2 <-  B_L_iris[Sex]*L_iris + betas[Grp, 6 + (Sex - 1)*11]*L_iris + # Iris lightness
      B_a_iris[Sex]*a_iris + betas[Grp, 7 + (Sex - 1)*11]*a_iris + # Iris redness
      B_b_iris[Sex]*b_iris + betas[Grp, 8 + (Sex - 1)*11]*b_iris + # Iris yellowness
      B_L_sclera[Sex]*L_sclera + betas[Grp, 9 + (Sex - 1)*11]*L_sclera + # Sclera lightness
      B_a_sclera[Sex]*a_sclera + betas[Grp, 10 + (Sex - 1)*11]*a_sclera + # Sclera redness
      B_b_sclera[Sex]*b_sclera + betas[Grp, 11 + (Sex - 1)*11]*b_sclera,  # Sclera yellowness
    
    # Non-centered parameterization for betas
    transpars> matrix[Grp, 22]:betas <- compose_noncentered(sigma_group, L_Rho_group, z_group),
    
    matrix[22, Grp]:z_group ~ normal(0, 1),
    
    # Hyperpriors for the group-level effects
    vector[22]:sigma_group ~ dexp(1),
    cholesky_factor_corr[22]:L_Rho_group ~ lkj_corr_cholesky(2),
    
    A[Sex]  ~ normal(0, 0.2),
    vector[2]:BSexTyp ~ normal(0, 0.5),
    vector[2]:BDIST ~ normal(0, 0.5),
    vector[2]:BAsym ~ normal(0,0.5),
    vector[2]:BAge ~ normal(0, 0.5),
    vector[2]:B_L_iris ~ normal(0, 0.5),
    vector[2]:B_a_iris ~ normal(0, 0.5),
    vector[2]:B_b_iris ~ normal(0, 0.5),
    vector[2]:B_L_sclera ~ normal(0, 0.5),
    vector[2]:B_a_sclera ~ normal(0, 0.5),
    vector[2]:B_b_sclera ~ normal(0, 0.5),
    
    sigma ~ dexp(1),
    
    gq> matrix[22, 22]:Rho_group <<- Chol_to_Corr(L_Rho_group)
    
  ), data = data_Attr, chains = 4, cores = 4, log_lik = T, iter = 5000, control = list(max_treedepth = 18, adapt_delta = 0.99)
)

M11_coefs <- precis(m11_attr, depth=3, prob=0.95)
write.csv2(M11_coefs, file="Model_11_ATTRACT_both_sexes_standardised_together_Without_Skin_L_Coeftab.csv")
#save(m11_attr, file="Model_11_ATTRACT_both_sexes_standardised_together_Without_Skin_L.Rdata")


#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.
# Sextypicality


Eye2 <- Eye1[!is.na(Eye1$SexT),] # I need Eye2 as a different object, since sextypicality has not been rated in Vietnam, 
# Then, Vietnam needs to be exluced from the data...
Eye  <- Eye2 

# SCALED AFTER EXCLUSION! 

data_sext <- list(
  Sex = as.integer(ifelse(Eye$Sex=="F",1,2)),
  Grp = as.integer(as.factor(Eye$Culture_Year)),
 # L_skin = stan_L_skin(Eye$L_skin),
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

summary.data.frame(data_sext)  


m12_Sext <- ulam(
  alist(
    Sextypicality ~ normal(mu, sigma),
    
    mu <- mu1 + mu2,
    
    mu1 <- A[Sex] + 
      betas[Grp, 1 + (Sex - 1)*11] + # Intercept (fixed, then per-culture, per-Sex)
      BSexTyp[Sex]*MeasSext + betas[Grp, 2 + (Sex - 1)*11]*MeasSext + # Sex typicality
      BAsym[Sex]*Asym + betas[Grp, 3 + (Sex - 1)*11]*Asym + # Asymmetry
      BDIST [Sex]*DIST + betas[Grp, 4 + (Sex - 1)*11]*DIST + # Distinctiveness
      BAge[Sex]*Age + betas[Grp, 5 + (Sex - 1)*11]*Age, # Age

    mu2 <-  B_L_iris[Sex]*L_iris + betas[Grp, 6 + (Sex - 1)*11]*L_iris + # Iris lightness
      B_a_iris[Sex]*a_iris + betas[Grp, 7 + (Sex - 1)*11]*a_iris + # Iris redness
      B_b_iris[Sex]*b_iris + betas[Grp, 8 + (Sex - 1)*11]*b_iris + # Iris yellowness
      B_L_sclera[Sex]*L_sclera + betas[Grp, 9 + (Sex - 1)*11]*L_sclera + # Sclera lightness
      B_a_sclera[Sex]*a_sclera + betas[Grp, 10 + (Sex - 1)*11]*a_sclera + # Sclera redness
      B_b_sclera[Sex]*b_sclera + betas[Grp, 11 + (Sex - 1)*11]*b_sclera,  # Sclera yellowness
    
    # Non-centered parameterization for betas
    transpars> matrix[Grp, 22]:betas <- compose_noncentered(sigma_group, L_Rho_group, z_group),
    
    matrix[22, Grp]:z_group ~ normal(0, 1),
    
    # Hyperpriors for the group-level effects
    vector[22]:sigma_group ~ dexp(1),
    cholesky_factor_corr[22]:L_Rho_group ~ lkj_corr_cholesky(2),
    
    A[Sex]  ~ normal(0, 0.2),
    vector[2]:BSexTyp ~ normal(0, 0.5),
    vector[2]:BDIST ~ normal(0, 0.5),
    vector[2]:BAsym ~ normal(0,0.5),
    vector[2]:BAge ~ normal(0, 0.5),
    vector[2]:B_L_iris ~ normal(0, 0.5),
    vector[2]:B_a_iris ~ normal(0, 0.5),
    vector[2]:B_b_iris ~ normal(0, 0.5),
    vector[2]:B_L_sclera ~ normal(0, 0.5),
    vector[2]:B_a_sclera ~ normal(0, 0.5),
    vector[2]:B_b_sclera ~ normal(0, 0.5),
    
    sigma ~ dexp(1),
    
    gq> matrix[22, 22]:Rho_group <<- Chol_to_Corr(L_Rho_group)
    
  ), data = data_sext, chains = 4, cores = 4, log_lik = T, iter = 5000, control = list(max_treedepth = 18, adapt_delta = 0.99)
)

M12_Scoefs <- precis(m12_Sext, depth=3, prob=0.95)
write.csv2(M12_Scoefs, file="Model_12_SEXTYP_both_sexes_standardised_together_Without_Skin_L_Coeftab.csv")
#save(m12_Sext, file="Model_12_SEXTYP_both_sexes_standardised_togetherWithout_Skin_L_.Rdata") 


