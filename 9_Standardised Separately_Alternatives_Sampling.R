#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.--.-..--.-.-.-.-.

# Standardised Separately - alternative analyses

#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.--.-..--.-.-.-.-.

# Analyses: 
library(rethinking)

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

#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.--.-..--.-.-.-.-.

# Standardised Separately, Analysis Without Iranian Sample: ATTRACTIVENESS

#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.--.-..--.-.-.-.-.

# Standardisation:

Eye1_3 <- Eye1[Eye1$Culture_Year!="Iran",]
Eye1_3a <- Eye1_3

culture_split <- split(Eye1_3[], Eye1_3$Culture_Year)

standardized_split <- lapply(culture_split, function(sub_df) {
  # Identify numeric columns
  numeric_cols <- sapply(sub_df, is.numeric)
  
  # Standardize numeric columns
  sub_df[, numeric_cols] <- scale(sub_df[, numeric_cols])
})

Eye1_3 <- do.call(rbind, standardized_split)
Eye1_3 <- as.data.frame(Eye1_3)

Eye1_3$Culture_Year <- Eye1_3a$Culture_Year
Eye1_3$Sex <- Eye1_3a$Sex

# Data: 
data_Attr <- list(
  Sex = as.integer(ifelse(Eye1_3$Sex=="F",1,2)),
  Grp = as.integer(as.factor(Eye1_3$Culture_Year)),
  L_skin = (Eye1_3$L_skin),
  L_iris = (Eye1_3$L_iris),
  a_iris = (Eye1_3$a_iris),
  b_iris = (Eye1_3$b_iris),
  L_sclera = (Eye1_3$L_sclera),
  a_sclera = (Eye1_3$a_sclera),
  b_sclera = (Eye1_3$b_sclera),
  MeasSext = (Eye1_3$SexTypMeas),
  DIST = (Eye1_3$LOCAL_DIST),
  Asym = (Eye1_3$Asym),
  Age = (Eye1_3$Age), 
  Attractiveness = (Eye1_3$Attr)
)

summary.data.frame(data_Attr)  


#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.


# 1 attractiveness when standardised separately, no of groups does not matter for the indexes in the model... 
#         1:8, 1:12/13:24, 1:N_Samples
#         where, however, grp is not specified as an integer. 


m5_attr <- ulam(
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
    
  ), data = data_Attr, chains = 4, cores = 4, log_lik = T, iter = 5000, control = list(max_treedepth = 18, adapt_delta = 0.99)
)

M5_coefs <- precis(m5_attr, depth=3, prob=0.95)
write.csv2(M5_coefs, file="Model_5_ATTRACT_BOTH_SEXES_WITHOUT_IRANIANS_Standardised_separately_COEFTAB.csv")
save(m5_attr, file="Model_5_ATTRACT_BOTH_SEXES_WITHOUT_IRANIANS_Standardised_separately.Rdata")


#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.

# Sextypikalita (without Vietnamese and Iranians): 

Eye1_3b <- Eye1_3[!is.na(Eye1_3$SexT),]

# Data: 
data_Sext <- list(
  Sex = as.integer(ifelse(Eye1_3b$Sex=="F",1,2)),
  Grp = as.integer(as.factor(Eye1_3b$Culture_Year)),
  L_skin = (Eye1_3b$L_skin),
  L_iris = (Eye1_3b$L_iris),
  a_iris = (Eye1_3b$a_iris),
  b_iris = (Eye1_3b$b_iris),
  L_sclera = (Eye1_3b$L_sclera),
  a_sclera = (Eye1_3b$a_sclera),
  b_sclera = (Eye1_3b$b_sclera),
  MeasSext = (Eye1_3b$SexTypMeas),
  DIST = (Eye1_3b$LOCAL_DIST),
  Asym = (Eye1_3b$Asym),
  Age = (Eye1_3b$Age), 
  Sextypicality = (Eye1_3b$SexT)
)

summary.data.frame(data_Sext)

# It has been standardised within each sample - separately. So it does not matter I subset only after the standardisation. 
# 7 samples ~ grp = 7.

m6_Sext <- ulam(
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
    
  ), data = data_Sext, chains = 4, cores = 4, log_lik = T, iter = 5000, control = list(max_treedepth = 18, adapt_delta = 0.99)
)

M6_Scoefs <- precis(m6_Sext, depth=3, prob=0.95)
write.csv2(M6_Scoefs, file="Model_6_SEXTYP_BOTH_SEXES_WITHOUT_IRANIANS_Standardised_separately_COEFTAB.csv")
save(m6_Sext, file="Model_6_SEXTYP_BOTH_SEXES_WITHOUT_IRANIANS_Standardised_separately.Rdata")

#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.--.-..--.-.-.-.-.

# Standardised Separately, Analysis SKIN_L NOT CONSIDERED: ATTRACTIVENESS

#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.--.-..--.-.-.-.-.




#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.--.-..--.-.-.-.-.

# Standardised Separately, Analysis SKIN_L NOT CONSIDERED: SEXTYPICALITY

#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.--.-..--.-.-.-.-.










# POSEM





















# One good reason not to use the new Peter's way of standardising. A reviewer 
# won't see that I spend months measuring eyes, months developing the model,
# two infernal months trying to fix rusty GMM analysis and then end up with 
# a very similar model like was the Peter's one. 
# She would see the model that is similar. Therefore, I at least use the old way 
# of standardizing so that I cannot be blaimed it's 1:1 copy 

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



# Analyses: 
#-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.
#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.

# 1 Attractiveness: 

m2_attr <- ulam(
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
    
  ), data = data_Attr2, chains = 4, cores = 4, log_lik = T, iter = 5000, control = list(max_treedepth = 18, adapt_delta = 0.99)
)

M2_coefs <- precis(m2_attr, depth=3, prob=0.95)
write.csv2(M2_coefs, file="Model_2_ATRAKT_OBA_SEXY_Standardizovano_dohromady.csv")
save(m2_attr, file="Model_2_ATRAKT_OBA_SEXY_Standardizovano_dohromady.Rdata")

# Note, on the first run, it thrown a warning "warning message: In doTryCatch(return(expr), name, parentenv, handler) : restarting interrupted promise evaluation". 
# Therefore, by accidnet, the model was run three times: First time using data scale using "scale" function, then with custom-made function and finally 
# to check whether the warning is, as the Internet says "just a thing that sometimes pops-up as a consequence of background processess in the OS". 
# For L*sclera, the effect is close to 0.30, but the upper end of the 95 % CI tends to just over/under run the zero. It has no statistical importance,
# but mind this during re-analysis. 

# Perculture: 
hist(M2_coefs$sd[1418:1633]) # Highest "SD" is around 0.17, which is IMO still OK with regard to collinearity. 
M2_coefs$sd[1418:1633]

# Without skin lightness (attractiveness):

m2_2_attr <- ulam(
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
    
  ), data = data_Attr2, chains = 4, cores = 4, log_lik = T, iter = 5000, control = list(max_treedepth = 18, adapt_delta = 0.99)
)

M2_2_coefs <- precis(m2_2_attr, depth=3, prob=0.95)
write.csv2(M2_2_coefs, file="Model_2_2_ATRAKT_OBA_SEXY_Standardizovano_dohromady.csv")
save(m2_2_attr, file="Model_2_2_ATRAKT_OBA_SEXY_Standardizovano_dohromady.Rdata")

# Sextypicality

Eye2 <- Eye1[!is.na(Eye1$SexT),] # Pozn: Eye1 is OK, nonstandardised. The sole reason I need Eye2 is that sextypicality has not been rated in Vietnam 
Eye  <- Eye2 # and thus, some data shall be excluded...

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
write.csv2(M2_Scoefs, file="Model_1_SEXTYP_OBA_SEXY_Standardizovano_dohromady.csv")
save(m2_Sext, file="Model_1_SEXTYP_OBA_SEXY_Standardizovano_dohromady.Rdata") 

# Without skin lightness (Sextpyicality)

m2_2_Sext <- ulam(
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
    
  ), data = data_sext2, chains = 4, cores = 4, log_lik = T, iter = 5000, control = list(max_treedepth = 18, adapt_delta = 0.99)
)

M2_2_Scoefs <- precis(m2_2_Sext, depth=3, prob=0.95)
write.csv2(M2_2_Scoefs, file="Model_1_2_SEXTYP_OBA_SEXY_Standardizovano_dohromady.csv")
save(m2_2_Sext, file="Model_1_2_SEXTYP_OBA_SEXY_Standardizovano_dohromady.Rdata")

#-.-.-.--.-.-.-.-.-.-.-.-.-.--.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.--.-.-.
# Sext -> Attr: Simple mediation analysis - let's decide to run it on the "Standardised together thing"
#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.--.-.-.-.-.-.-.-.-.-.-.-.-.-.-..--..-.-.

# Dej tam (jetsli to máš) jen měřenou, vnímanou, atraktivitu a L 
# (dva prokorelovaný prediktory L a měřená), pak z obou na vnímanou a všechny tř na atraktivitu. To je zajímavej systém.
# A nebo na to prď.

# Protože by se tam nabízelo dát taky velikost vlivu na attractiveness v modelu, kde je taky vliv 
# perceived sextypicality na atraktivitu, jako prostě to brát jako mediační analýzu a ukazovat všechny ty efekty.
# st ~ L
# att ~ st + L
# Ale je to jedno. Prostě je to použitelný.

# Only for women: 

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

library(rethinking)

#######
#     #
#  1  #
#     #
#######

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

#######
#     #
#  2  #
#     #
#######

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

#######
#     #
#  3  #
#     #
#######

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

#######
#     #
#  4  #
#     #
#######

# SECOND LAST ONE - let's kill sextypicality 

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

#######
#     #
#  5  #
#     #
#######

# SECOND LAST ONE - let's kill attractiveness 

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


###############################################################################
# TRIANGLE - mediation analysis 
###############################################################################
# L* ---> SexTyp ---> Attract 
###############################################################################

summary(as.data.frame(data_AT_med_F))

# Defining the model
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


# Kill the Sext ---> Attract: 
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



# Kill the Lightness ---> SexT: 
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




#######
#     #
#    #
#   #
####### 
# MEN!
#######
#   #
#    #
#     #
#######



#######
#     #
# 1M  #
#     #
#######

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

#######
#     #
#  2  #
#     #
#######

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

#######
#     #
#  3  #
#     #
#######

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

#######
#     #
#  4  #
#     #
#######

# SECOND LAST ONE - let's kill sextypicality 

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

#######
#     #
#  5  #
#     #
#######

# SECOND LAST ONE - let's kill attractiveness 

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


###############################################################################
# TRIANGLE - mediation analysis 
###############################################################################
# L* ---> SexTyp ---> Attract 
###############################################################################

summary(as.data.frame(data_AT_med_M))

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


# Kill the Sext ---> Attract: 
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



# Kill the Lightness ---> SexT: 
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


#  PAK SMAŽ
setwd("D:/GACR_VF/Measured_Eye_Phenotype_And_Ascribed_Characteristics_In_Different_Countries/Fotky/Data a modely/TADY/Modely_sampling_FINAL/Modely_hlavni_neprepsat")

load("Model_1_ATRAKT_OBA_SEXY_Standardizovano_zvlast.Rdata")
M1 <- precis(m1_attr, depth=3, prob=0.95)
write.csv2(M1, file="Model_1_ATRAKT_OBA_SEXY_Standardizovano_zvlast_COEFTAB.csv")
rm("M1", "m1_attr")


load("Model_1_SEXTYP_OBA_SEXY_Standardizovano_dohromady.Rdata")
M2 <- precis(m2_Sext, depth=3, prob=0.95)
write.csv2(M2, file="Model_1_SEXTYP_OBA_SEXY_Standardizovano_dohromady_COEFTAB.csv")
rm("M2","m2_Sext")

load("Model_1_SEXTYP_OBA_SEXY_Standardizovano_zvlast.Rdata")
M3 <- precis(m1_Sext, depth=3, prob=0.95)
write.csv2(M3, file="Model_1_SEXTYP_OBA_SEXY_Standardizovano_zvlast_COEFTAB.csv")
rm("M3", "m1_Sext")

load("Model_2_ATRAKT_OBA_SEXY_Standardizovano_dohromady.Rdata")
M4 <- precis(m2_attr, depth=3, prob=0.95)
write.csv2(M4, file="Model_2_ATRAKT_OBA_SEXY_Standardizovano_dohromady_COEFTAB.csv")
rm("M4","m2_attr")
