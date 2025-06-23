#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-
#
# Attractiveness - when standardised within each culture 
#
#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-

# Analysis that includes quadratic effects for selected coefficients (eye colour)...
# On the contrary, there is no reason to suspect the effect of age, Dist, FA, and Skin L* to be nonlinear...
# The same de facto applies for Iris L*a*b* and Sclera a*b*, however...

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

# When standardised SEPARATELY!!

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

# Standardise within each culture: 

# Standardisation:

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


# Checking if the shift "all-is-above-zero", which is needed for "varying exponent" is OK here...
# that is - if the range is kept
hist((Eye_2$L_skin)) 
hist((Eye_2$L_skin)-min((Eye_2$L_skin))+0.001)

diff(range((Eye_2$L_skin))) 
diff(range((Eye_2$L_skin)-min((Eye_2$L_skin))+0.001))
# ól good! 

data_Attr2 <- list(
  Sex = as.integer(ifelse(Eye_2$sex=="F",1,2)),
  Grp = as.integer(as.factor(Eye_2$Culture_Year)),
  L_skin = (Eye_2$L_skin)-min((Eye_2$L_skin))+0.001, # NZ = nonzero
  L_iris = (Eye_2$L_iris)-min((Eye_2$L_iris))+0.001, # NZ = nonzero
  a_iris = (Eye_2$a_iris)-min((Eye_2$a_iris))+0.001, # NZ = nonzero
  b_iris = (Eye_2$b_iris)-min((Eye_2$b_iris))+0.001, # NZ = nonzero
  L_sclera = (Eye_2$L_sclera)-min((Eye_2$L_sclera))+0.001, # NZ = nonzero
  a_sclera = (Eye_2$a_sclera)-min((Eye_2$a_sclera))+0.001, # NZ = nonzero
  b_sclera = (Eye_2$b_sclera)-min((Eye_2$b_sclera))+0.001, # NZ = nonzero
  MeasSext = (Eye_2$SexTypMeas)-min(Eye_2$SexTypMeas)+0.001,
  DIST =(Eye_2$LOCAL_DIST)-min(Eye_2$LOCAL_DIST)+0.001,
  Asym = (Eye_2$Asym)-min(Eye_2$Asym)+0.001,
  Age = (Eye_2$Age)-min(Eye_2$Age)+0.001,
 Attractiveness = (Eye_2$Attr)-min(Eye_2$Attr)+0.001
)

summary.data.frame(data_Attr2)



# 1 Attractiveness: 

# 1_1) Without any non-linear terms: 

m_1_attrSEP <- ulam( 
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

post_1 <- extract.samples(m_1_attrSEP)
save(m_1_attrSEP, file="model_nonlinear_attr_SEP_12_06_25_1.Rdata")
saveRDS(post_1, file="model_nonlinear_attr_SEP_12_06_25_1_POST.RDS")


# x-------------------------------------------------------
# 1_2) All the considered quadratic terms + LINEAR: 

m_1_2_SQ7_attrSEP <- ulam( 
  alist(
    Attractiveness ~ normal(mu, sigma),
    
    mu <- mu1 + mu2 + mu3 + mu4,
    
    mu1 <- A[Sex] + betas[Grp, 1 + (Sex - 1)*19] + # Intercept (fixed, then per-culture, per-Sex)
      BSexTyp[Sex]*MeasSext + betas[Grp, 2 + (Sex - 1)*19]*MeasSext + # Sex typicality
      BAsym[Sex]*Asym + betas[Grp, 3 + (Sex - 1)*19]*Asym + # Asymmetry
      BDIST [Sex]*DIST + betas[Grp, 4 + (Sex - 1)*19]*DIST + # Distinctiveness
      BAge[Sex]*Age + betas[Grp, 5 + (Sex - 1)*19]*Age + # Age
      B_L_Skin[Sex]*L_skin + betas[Grp, 6 + (Sex - 1)*19]*L_skin, # Skin lightness
    
    mu2 <-  B_L_iris[Sex]*L_iris + betas[Grp, 7 + (Sex - 1)*19]*L_iris + # Iris lightness
      B_a_iris[Sex]*a_iris + betas[Grp, 8 + (Sex - 1)*19]*a_iris + # Iris redness
      B_b_iris[Sex]*b_iris + betas[Grp, 9 + (Sex - 1)*19]*b_iris + # Iris yellowness
      B_L_sclera[Sex]*L_sclera + betas[Grp, 10 + (Sex - 1)*19]*L_sclera + # Sclera lightness
      B_a_sclera[Sex]*a_sclera + betas[Grp, 11 + (Sex - 1)*19]*a_sclera + # Sclera redness
      B_b_sclera[Sex]*b_sclera + betas[Grp, 12 + (Sex - 1)*19]*b_sclera,  # Sclera yellowness
    
    # Adding the selected quadratic terms (fixed + varying) 
    mu3 <- 
      B_L_Skin_sq[Sex]*L_skin^2 + betas[Grp, 13 + (Sex - 1)*19]*L_skin^2 + # Skin lightness
      B_L_iris_sq[Sex]*L_iris^2 + betas[Grp, 14 + (Sex - 1)*19]*L_iris^2 + # Iris lightness
      B_a_iris_sq[Sex]*a_iris^2 + betas[Grp, 15 + (Sex - 1)*19]*a_iris^2 + # Iris redness
      B_b_iris_sq[Sex]*b_iris^2 + betas[Grp, 16 + (Sex - 1)*19]*b_iris^2,  # Iris yellowness
    
    mu4 <- 
      B_L_sclera_sq[Sex]*L_sclera^2 + betas[Grp, 17 + (Sex - 1)*19]*L_sclera^2 + # Sclera lightness
      B_a_sclera_sq[Sex]*a_sclera^2 + betas[Grp, 18 + (Sex - 1)*19]*a_sclera^2 + # Sclera redness
      B_b_sclera_sq[Sex]*b_sclera^2 + betas[Grp, 19 + (Sex - 1)*19]*b_sclera^2, # Sclera yellowness
    
    # Non-centered parameterization for betas
    transpars> matrix[Grp, 38]:betas <- compose_noncentered(sigma_group, L_Rho_group, z_group),
    
    matrix[38, Grp]:z_group ~ normal(0, 1),
    
    # Hyperpriors for the group-level effects
    vector[38]:sigma_group ~ dexp(1),
    cholesky_factor_corr[38]:L_Rho_group ~ lkj_corr_cholesky(2),
    
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
    
    # Quadratic fixed: 
    vector[2]:B_L_Skin_sq ~ normal(0, 0.5),
    vector[2]:B_L_iris_sq ~ normal(0, 0.5),
    vector[2]:B_a_iris_sq ~ normal(0, 0.5),
    vector[2]:B_b_iris_sq ~ normal(0, 0.5),
    vector[2]:B_L_sclera_sq ~ normal(0, 0.5),
    vector[2]:B_a_sclera_sq ~ normal(0, 0.5),
    vector[2]:B_b_sclera_sq ~ normal(0, 0.5),
    
    sigma ~ dexp(1),
    
    gq> matrix[38, 38]:Rho_group <<- Chol_to_Corr(L_Rho_group)
    
  ), data = data_Attr2, chains = 4, cores = 4, log_lik = T, iter = 2500, control = list(max_treedepth = 18, adapt_delta = 0.99)
)

post_2 <- extract.samples(m_1_2_SQ7_attrSEP)
save(m_1_2_SQ7_attrSEP, file="model_nonlinear_attr_SEP_12_06_25_2.Rdata")
saveRDS(post_2, file="model_nonlinear_attr_SEP_12_06_25_2_POST.RDS")


# x-------------------------------------------------------
# 1_3) Linear terms where needed, terms with free exponent everyhwere else (mind to use the edited 
# (always non-zero) variables where needed): 


m_1_3_only_varying_attrSEP <- ulam( 
  alist(
    Attractiveness ~ normal(mu, sigma),
    
    mu <- mu1 + mu2 + mu3,
    
    mu1 <- A[Sex] + betas[Grp, 1 + (Sex - 1)*12] + # Intercept (fixed, then per-culture, per-Sex)
      BSexTyp[Sex]*MeasSext + betas[Grp, 2 + (Sex - 1)*12]*MeasSext + # Sex typicality
      BAsym[Sex]*Asym + betas[Grp, 3 + (Sex - 1)*12]*Asym + # Asymmetry
      BDIST [Sex]*DIST + betas[Grp, 4 + (Sex - 1)*12]*DIST + # Distinctiveness
      BAge[Sex]*Age + betas[Grp, 5 + (Sex - 1)*12]*Age, # Age
    
    mu2 <- 
      B_L_Skin_var[Sex]*L_skin^E_skin_L + betas[Grp, 6 + (Sex - 1)*12]*L_skin^E_skin_L + # Skin lightness
      B_L_iris_var[Sex]*L_iris^E_iris_L + betas[Grp, 7 + (Sex - 1)*12]*L_iris^E_iris_L + # Iris lightness
      B_a_iris_var[Sex]*a_iris^E_iris_a + betas[Grp, 8 + (Sex - 1)*12]*a_iris^E_iris_a + # Iris redness
      B_b_iris_var[Sex]*b_iris^E_iris_b + betas[Grp, 9 + (Sex - 1)*12]*b_iris^E_iris_b,  # Iris yellowness
    
    mu3 <- 
      B_L_sclera_var[Sex]*L_sclera^E_sclera_L + betas[Grp, 10 + (Sex - 1)*12]*L_sclera^E_sclera_L + # Sclera lightness
      B_a_sclera_var[Sex]*a_sclera^E_sclera_a + betas[Grp, 11 + (Sex - 1)*12]*a_sclera^E_sclera_a + # Sclera redness
      B_b_sclera_var[Sex]*b_sclera^E_sclera_b + betas[Grp, 12 + (Sex - 1)*12]*b_sclera^E_sclera_b, # Sclera yellowness
    
    # Za totohle stavu exponent nevaríruje, čili všechno, co odlišuje slopy pro jednotlivé skupiny (a bohužel taky pohlaví),
    # musí téct skrze bety, což je nešťastný. 
    
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
    vector[2]:B_L_Skin_var ~ normal(0, 0.5),
    vector[2]:B_L_iris_var ~ normal(0, 0.5),
    vector[2]:B_a_iris_var ~ normal(0, 0.5),
    vector[2]:B_b_iris_var ~ normal(0, 0.5),
    vector[2]:B_L_sclera_var ~ normal(0, 0.5),
    vector[2]:B_a_sclera_var ~ normal(0, 0.5),
    vector[2]:B_b_sclera_var ~ normal(0, 0.5),
    
    # Varying exponent (exponent může bejt jakejkoli, problém byl s tím záporem u samotné proměnné, doufám): 
    E_skin_L ~ dexp(1),
    E_iris_L ~ dexp(1),
    E_iris_a ~ dexp(1),
    E_iris_b ~ dexp(1),
    E_sclera_L ~ dexp(1),
    E_sclera_a ~ dexp(1),
    E_sclera_b ~ dexp(1),
    
    sigma ~ dexp(1),
    
    gq> matrix[24, 24]:Rho_group <<- Chol_to_Corr(L_Rho_group)
    
  ), data = data_Attr2, chains = 4, cores = 4, log_lik = T, iter = 2500, control = list(max_treedepth = 18, adapt_delta = 0.99)
)

post_3 <- extract.samples(m_1_3_only_varying_attrSEP)
save(m_1_3_only_varying_attrSEP, file="model_nonlinear_attr_SEP_12_06_25_3.Rdata")
saveRDS(post_3, file="model_nonlinear_attr_SEP_12_06_25_3_POST.RDS")

# x-------------------------------------------------------
# 1_4) Linear terms where needed, terms with free exponent everyhwere else (mind to use the edited 
# (always non-zero) variables where needed): 
# And with [sex] to account for potentially different exponents between men and women...

m_1_4_only_varying_attr_sex2SEP <- ulam( 
  alist(
    Attractiveness ~ normal(mu, sigma),
    
    mu <- mu1 + mu2 + mu3,
    
    mu1 <- A[Sex] + betas[Grp, 1 + (Sex - 1)*12] + # Intercept (fixed, then per-culture, per-Sex)
      BSexTyp[Sex]*MeasSext + betas[Grp, 2 + (Sex - 1)*12]*MeasSext + # Sex typicality
      BAsym[Sex]*Asym + betas[Grp, 3 + (Sex - 1)*12]*Asym + # Asymmetry
      BDIST [Sex]*DIST + betas[Grp, 4 + (Sex - 1)*12]*DIST + # Distinctiveness
      BAge[Sex]*Age + betas[Grp, 5 + (Sex - 1)*12]*Age, # Age
    
    mu2 <- 
      B_L_Skin_var[Sex]*L_skin^E_skin_L[Sex] + betas[Grp, 6 + (Sex - 1)*12]*L_skin^E_skin_L[Sex] + # Skin lightness
      B_L_iris_var[Sex]*L_iris^E_iris_L[Sex] + betas[Grp, 7 + (Sex - 1)*12]*L_iris^E_iris_L[Sex] + # Iris lightness
      B_a_iris_var[Sex]*a_iris^E_iris_a[Sex] + betas[Grp, 8 + (Sex - 1)*12]*a_iris^E_iris_a[Sex]+ # Iris redness
      B_b_iris_var[Sex]*b_iris^E_iris_b[Sex] + betas[Grp, 9 + (Sex - 1)*12]*b_iris^E_iris_b[Sex],  # Iris yellowness
    
    mu3 <- 
      B_L_sclera_var[Sex]*L_sclera^E_sclera_L[Sex] + betas[Grp, 10 + (Sex - 1)*12]*L_sclera^E_sclera_L[Sex] + # Sclera lightness
      B_a_sclera_var[Sex]*a_sclera^E_sclera_a[Sex] + betas[Grp, 11 + (Sex - 1)*12]*a_sclera^E_sclera_a[Sex] + # Sclera redness
      B_b_sclera_var[Sex]*b_sclera^E_sclera_b[Sex] + betas[Grp, 12 + (Sex - 1)*12]*b_sclera^E_sclera_b[Sex], # Sclera yellowness
    
    # Za totohle stavu exponent nevaríruje, čili všechno, co odlišuje slopy pro jednotlivé skupiny (a bohužel taky pohlaví),
    # musí téct skrze bety, což je nešťastný. 
    
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
    vector[2]:B_L_Skin_var ~ normal(0, 0.5),
    vector[2]:B_L_iris_var ~ normal(0, 0.5),
    vector[2]:B_a_iris_var ~ normal(0, 0.5),
    vector[2]:B_b_iris_var ~ normal(0, 0.5),
    vector[2]:B_L_sclera_var ~ normal(0, 0.5),
    vector[2]:B_a_sclera_var ~ normal(0, 0.5),
    vector[2]:B_b_sclera_var ~ normal(0, 0.5),
    
    # Varying exponent (exponent může bejt jakejkoli, problém byl s tím záporem u samotné proměnné, doufám): 
    vector[2]:E_skin_L ~ dexp(1),
    vector[2]:E_iris_L ~ dexp(1),
    vector[2]:E_iris_a ~ dexp(1),
    vector[2]:E_iris_b ~ dexp(1),
    vector[2]:E_sclera_L ~ dexp(1),
    vector[2]:E_sclera_a ~ dexp(1),
    vector[2]:E_sclera_b ~ dexp(1),
    
    sigma ~ dexp(1),
    
    gq> matrix[24, 24]:Rho_group <<- Chol_to_Corr(L_Rho_group)
    
  ), data = data_Attr2, chains = 4, cores = 4, log_lik = T, iter = 2500, control = list(max_treedepth = 18, adapt_delta = 0.99)
)

post_4 <- extract.samples(m_1_4_only_varying_attr_sex2SEP)
save(m_1_4_only_varying_attr_sex2SEP, file="model_nonlinear_attr_SEP_12_06_25_4.Rdata")
saveRDS(post_4, file="model_nonlinear_attr_SEP_12_06_25_4_POST.RDS")



# 1-5) With "Free exponent" and linear terms 

m_1_5_FREE_and_LINN_attrSEP <- ulam( 
  alist(
    Attractiveness ~ normal(mu, sigma),
    
    mu <- mu1 + mu2 + mu3 + mu4,
    
    mu1 <- A[Sex] + betas[Grp, 1 + (Sex - 1)*19] + # Intercept (fixed, then per-culture, per-Sex)
      BSexTyp[Sex]*MeasSext + betas[Grp, 2 + (Sex - 1)*19]*MeasSext + # Sex typicality
      BAsym[Sex]*Asym + betas[Grp, 3 + (Sex - 1)*19]*Asym + # Asymmetry
      BDIST [Sex]*DIST + betas[Grp, 4 + (Sex - 1)*19]*DIST + # Distinctiveness
      BAge[Sex]*Age + betas[Grp, 5 + (Sex - 1)*19]*Age + # Age
      B_L_Skin[Sex]*L_skin + betas[Grp, 6 + (Sex - 1)*19]*L_skin, # Skin lightness
    
    mu2 <-  B_L_iris[Sex]*L_iris + betas[Grp, 7 + (Sex - 1)*19]*L_iris + # Iris lightness
      B_a_iris[Sex]*a_iris + betas[Grp, 8 + (Sex - 1)*19]*a_iris + # Iris redness
      B_b_iris[Sex]*b_iris + betas[Grp, 9 + (Sex - 1)*19]*b_iris + # Iris yellowness
      B_L_sclera[Sex]*L_sclera + betas[Grp, 10 + (Sex - 1)*19]*L_sclera + # Sclera lightness
      B_a_sclera[Sex]*a_sclera + betas[Grp, 11 + (Sex - 1)*19]*a_sclera + # Sclera redness
      B_b_sclera[Sex]*b_sclera + betas[Grp, 12 + (Sex - 1)*19]*b_sclera,  # Sclera yellowness
    
    mu3 <- 
      B_L_Skin_var[Sex]*L_skin^E_skin_L + betas[Grp, 6 + (Sex - 1)*19]*L_skin^E_skin_L + # Skin lightness
      B_L_iris_var[Sex]*L_iris^E_iris_L + betas[Grp, 7 + (Sex - 1)*19]*L_iris^E_iris_L + # Iris lightness
      B_a_iris_var[Sex]*a_iris^E_iris_a + betas[Grp, 8 + (Sex - 1)*19]*a_iris^E_iris_a + # Iris redness
      B_b_iris_var[Sex]*b_iris^E_iris_b + betas[Grp, 9 + (Sex - 1)*19]*b_iris^E_iris_b,  # Iris yellowness
    
    mu4 <- 
      B_L_sclera_var[Sex]*L_sclera^E_sclera_L + betas[Grp, 10 + (Sex - 1)*19]*L_sclera^E_sclera_L + # Sclera lightness
      B_a_sclera_var[Sex]*a_sclera^E_sclera_a + betas[Grp, 11 + (Sex - 1)*19]*a_sclera^E_sclera_a + # Sclera redness
      B_b_sclera_var[Sex]*b_sclera^E_sclera_b + betas[Grp, 12 + (Sex - 1)*19]*b_sclera^E_sclera_b, # Sclera yellowness
    
    # Non-centered parameterization for betas
    transpars> matrix[Grp, 38]:betas <- compose_noncentered(sigma_group, L_Rho_group, z_group),
    
    matrix[38, Grp]:z_group ~ normal(0, 1),
    
    # Hyperpriors for the group-level effects
    vector[38]:sigma_group ~ dexp(1),
    cholesky_factor_corr[38]:L_Rho_group ~ lkj_corr_cholesky(2),
    
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
    
    # Free exponents - priors: 
    E_skin_L ~ dexp(1),
    
    E_iris_L ~ dexp(1),
    E_iris_a ~ dexp(1),
    E_iris_b ~ dexp(1),
    
    E_sclera_L ~ dexp(1),
    E_sclera_a ~ dexp(1),
    E_sclera_b ~ dexp(1),
    
    # Free exponent's betas - priors: 
    vector[2]:B_L_Skin_var ~ normal(0, 0.5),
    vector[2]:B_L_iris_var ~ normal(0, 0.5),
    vector[2]:B_a_iris_var ~ normal(0, 0.5),
    vector[2]:B_b_iris_var ~ normal(0, 0.5),
    vector[2]:B_L_sclera_var ~ normal(0, 0.5),
    vector[2]:B_a_sclera_var ~ normal(0, 0.5),
    vector[2]:B_b_sclera_var ~ normal(0, 0.5),
    
    sigma ~ dexp(1),
    
    gq> matrix[38, 38]:Rho_group <<- Chol_to_Corr(L_Rho_group)
    
  ), data = data_Attr2, chains = 4, cores = 4, log_lik = T, iter = 2500, control = list(max_treedepth = 18, adapt_delta = 0.99)
)

post_5 <- extract.samples(m_1_5_FREE_and_LINN_attrSEP)
save(m_1_5_FREE_and_LINN_attrSEP, file="model_nonlinear_attr_SEP_12_06_25_5.Rdata")
saveRDS(post_5, file="model_nonlinear_attr_SEP_12_06_25_5_POST.RDS")




# 1-6) With "Free exponent", "linear terms" and sex[]: 

m_1_6_FREE_MF_attrSEP <- ulam( 
  alist(
    Attractiveness ~ normal(mu, sigma),
    
    mu <- mu1 + mu2 + mu3 + mu4,
    
    mu1 <- A[Sex] + betas[Grp, 1 + (Sex - 1)*19] + # Intercept (fixed, then per-culture, per-Sex)
      BSexTyp[Sex]*MeasSext + betas[Grp, 2 + (Sex - 1)*19]*MeasSext + # Sex typicality
      BAsym[Sex]*Asym + betas[Grp, 3 + (Sex - 1)*19]*Asym + # Asymmetry
      BDIST [Sex]*DIST + betas[Grp, 4 + (Sex - 1)*19]*DIST + # Distinctiveness
      BAge[Sex]*Age + betas[Grp, 5 + (Sex - 1)*19]*Age + # Age
      B_L_Skin[Sex]*L_skin + betas[Grp, 6 + (Sex - 1)*19]*L_skin, # Skin lightness
    
    mu2 <-  B_L_iris[Sex]*L_iris + betas[Grp, 7 + (Sex - 1)*19]*L_iris + # Iris lightness
      B_a_iris[Sex]*a_iris + betas[Grp, 8 + (Sex - 1)*19]*a_iris + # Iris redness
      B_b_iris[Sex]*b_iris + betas[Grp, 9 + (Sex - 1)*19]*b_iris + # Iris yellowness
      B_L_sclera[Sex]*L_sclera + betas[Grp, 10 + (Sex - 1)*19]*L_sclera + # Sclera lightness
      B_a_sclera[Sex]*a_sclera + betas[Grp, 11 + (Sex - 1)*19]*a_sclera + # Sclera redness
      B_b_sclera[Sex]*b_sclera + betas[Grp, 12 + (Sex - 1)*19]*b_sclera,  # Sclera yellowness
    
    mu3 <- 
      B_L_Skin_var[Sex]*L_skin^E_skin_L[Sex] + betas[Grp, 6 + (Sex - 1)*19]*L_skin^E_skin_L[Sex] + # Skin lightness
      B_L_iris_var[Sex]*L_iris^E_iris_L[Sex] + betas[Grp, 7 + (Sex - 1)*19]*L_iris^E_iris_L[Sex] + # Iris lightness
      B_a_iris_var[Sex]*a_iris^E_iris_a[Sex] + betas[Grp, 8 + (Sex - 1)*19]*a_iris^E_iris_a[Sex] + # Iris redness
      B_b_iris_var[Sex]*b_iris^E_iris_b[Sex] + betas[Grp, 9 + (Sex - 1)*19]*b_iris^E_iris_b[Sex],  # Iris yellowness
    
    mu4 <- 
      B_L_sclera_var[Sex]*L_sclera^E_sclera_L[Sex] + betas[Grp, 10 + (Sex - 1)*19]*L_sclera^E_sclera_L[Sex] + # Sclera lightness
      B_a_sclera_var[Sex]*a_sclera^E_sclera_a[Sex] + betas[Grp, 11 + (Sex - 1)*19]*a_sclera^E_sclera_a[Sex] + # Sclera redness
      B_b_sclera_var[Sex]*b_sclera^E_sclera_b[Sex] + betas[Grp, 12 + (Sex - 1)*19]*b_sclera^E_sclera_b[Sex], # Sclera yellowness
    
    # Non-centered parameterization for betas
    transpars> matrix[Grp, 38]:betas <- compose_noncentered(sigma_group, L_Rho_group, z_group),
    
    matrix[38, Grp]:z_group ~ normal(0, 1),
    
    # Hyperpriors for the group-level effects
    vector[38]:sigma_group ~ dexp(1),
    cholesky_factor_corr[38]:L_Rho_group ~ lkj_corr_cholesky(2),
    
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
    
    # Free exponents - priors: 
    vector[2]:E_skin_L ~ dexp(1),
    
    vector[2]:E_iris_L ~ dexp(1),
    vector[2]:E_iris_a ~ dexp(1),
    vector[2]:E_iris_b ~ dexp(1),
    
    vector[2]:E_sclera_L ~ dexp(1),
    vector[2]:E_sclera_a ~ dexp(1),
    vector[2]:E_sclera_b ~ dexp(1),
    
    # Free exponent's betas - priors: 
    vector[2]:B_L_Skin_var ~ normal(0, 0.5),
    vector[2]:B_L_iris_var ~ normal(0, 0.5),
    vector[2]:B_a_iris_var ~ normal(0, 0.5),
    vector[2]:B_b_iris_var ~ normal(0, 0.5),
    vector[2]:B_L_sclera_var ~ normal(0, 0.5),
    vector[2]:B_a_sclera_var ~ normal(0, 0.5),
    vector[2]:B_b_sclera_var ~ normal(0, 0.5),
    
    sigma ~ dexp(1),
    
    gq> matrix[38, 38]:Rho_group <<- Chol_to_Corr(L_Rho_group)
    
  ), data = data_Attr2, chains = 4, cores = 4, log_lik = T, iter = 2500, control = list(max_treedepth = 18, adapt_delta = 0.99)
)


post_6 <- extract.samples(m_1_6_FREE_MF_attrSEP)
save(m_1_6_FREE_MF_attrSEP, file="model_nonlinear_attr_SEP_12_06_25_6.Rdata")
saveRDS(post_6, file="model_nonlinear_attr_SEP_12_06_25_6_POST.RDS")


compare(m_1_attrSEP,
        m_1_2_SQ7_attrSEP,
        m_1_3_only_varying_attrSEP,
        m_1_4_only_varying_attr_sex2SEP,
        m_1_5_FREE_and_LINN_attrSEP,
        m_1_6_FREE_MF_attrSEP
)

# m_1_3_only_varying_attrSEP      2707.8 48.03   0.0    NA  81.0   0.47
# m_1_4_only_varying_attr_sex2SEP 2708.0 48.15   0.2  2.27  82.5   0.43
# m_1_attrSEP                     2711.2 48.15   3.4  8.43  99.9   0.09
# m_1_5_FREE_and_LINN_attrSEP     2717.4 48.24   9.6  7.91  99.3   0.00
# m_1_6_FREE_MF_attrSEP           2717.6 48.21   9.8  7.90  99.4   0.00
# m_1_2_SQ7_attrSEP               2732.8 47.66  25.0 13.15 129.7   0.00




# SET IT BACK

M1_coefs <- precis(SEP_m_1_attr, depth=3, prob=0.95)
M2_coefs <- precis(SEP_m_1_2_SQ7_attr, depth=3, prob=0.95)
M3_coefs <- precis(SEP_m_1_3_only_varying_attr, depth=3, prob=0.95)
M4_coefs <- precis(SEP_m_1_4_only_varying_attr_sex2, depth=3, prob=0.95)
M5_coefs <- precis(SEP_m_1_5_FREE_QUAD_attr, depth=3, prob=0.95)
M6_coefs <- precis(SEP_m_1_6_FREE_QUAD_SEX_attr, depth=3, prob=0.95)

write.csv2(M1_coefs, file="SEP_Model_1_attr.csv")
write.csv2(M2_coefs, file="SEP_Model_2_attr.csv")
write.csv2(M3_coefs, file="SEP_Model_3_attr.csv")
write.csv2(M4_coefs, file="SEP_Model_4_attr.csv")
write.csv2(M5_coefs, file="SEP_Model_5_attr.csv")
write.csv2(M6_coefs, file="SEP_Model_6_attr.csv")



#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-
#
# Sextypicality - when standardised within each culture 
#
#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-

# Analysis that includes quadratic effects for selected coefficients (eye colour)...
# On the contrary, there is no reason to suspect the effect of age, Dist, FA, and Skin L* to be nonlinear...
# The same de facto applies for Iris L*a*b* and Sclera a*b*, however...

# # Upload and check the data
Eye1 <- read.csv2("Data_03_08_24.csv",T)

tapply(Eye1$Attr,Eye1$Culture_Year, mean)

summary(Eye1)
Eye1 <- Eye1[!is.na(Eye1$Age),]
Eye1 <- Eye1[!is.na(Eye1$SexT),]


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

# When standardised SEPARATELY!!

#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.--.-..--.-.-.-.-.

Eye <- Eye1

# Standardise within each culture: 

# Standardisation:

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


# Checking if the shift "all-is-above-zero", which is needed for "varying exponent" is OK here...
# that is - if the range is kept
hist((Eye_2$L_skin)) 
hist((Eye_2$L_skin)-min((Eye_2$L_skin))+0.001)

diff(range((Eye_2$L_skin))) 
diff(range((Eye_2$L_skin)-min((Eye_2$L_skin))+0.001))
# ól good! 

data_sext2 <- list(
  Sex = as.integer(ifelse(Eye_2$sex=="F",1,2)),
  Grp = as.integer(as.factor(Eye_2$Culture_Year)),
  L_skin = (Eye_2$L_skin)-min((Eye_2$L_skin))+0.001, # NZ = nonzero
  L_iris = (Eye_2$L_iris)-min((Eye_2$L_iris))+0.001, # NZ = nonzero
  a_iris = (Eye_2$a_iris)-min((Eye_2$a_iris))+0.001, # NZ = nonzero
  b_iris = (Eye_2$b_iris)-min((Eye_2$b_iris))+0.001, # NZ = nonzero
  L_sclera = (Eye_2$L_sclera)-min((Eye_2$L_sclera))+0.001, # NZ = nonzero
  a_sclera = (Eye_2$a_sclera)-min((Eye_2$a_sclera))+0.001, # NZ = nonzero
  b_sclera = (Eye_2$b_sclera)-min((Eye_2$b_sclera))+0.001, # NZ = nonzero
  MeasSext = (Eye_2$SexTypMeas)-min(Eye_2$SexTypMeas)+0.001,
  DIST =(Eye_2$LOCAL_DIST)-min(Eye_2$LOCAL_DIST)+0.001,
  Asym = (Eye_2$Asym)-min(Eye_2$Asym)+0.001,
  Age = (Eye_2$Age)-min(Eye_2$Age)+0.001,
  Sextypicality = (Eye_2$SexT)-min(Eye_2$SexT)+0.001
)

summary.data.frame(data_sext2)


# Analyses: 
#-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.
#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.

# We use an older version of rethinking, before cmdstanr was introduced: https://github.com/rmcelreath/rethinking?tab=readme-ov-file#within-chain-multithreading
# Should work well with the new version, but mind that the model's objects are not compatible between the new and old version. 

library(rethinking)

# 1 Sextypicality: 


# 1_1) Without any non-linear terms: 

# (Večerní volací znak [VVZ]: "Klasik")

m_1_sextSEP <- ulam( 
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
  
  ), data = data_sext2, chains = 4, cores = 4, log_lik = T, iter = 2500, control = list(max_treedepth = 18, adapt_delta = 0.99)
)

post_1 <- extract.samples(m_1_sextSEP)
save(m_1_sextSEP, file="model_nonlinear_sext_SEP_12_06_25_1.Rdata")
saveRDS(post_1, file="model_nonlinear_sext_SEP_12_06_25_1_POST.RDS")



# 1_2) All the considered quadratic terms: 

# (Večerní volací znak [VVZ]: "Klasik-na-kvadrát")

m_1_2_SQ7_sextSEP <- ulam( 
  alist(
    Sextypicality ~ normal(mu, sigma),
    
    mu <- mu1 + mu2 + mu3 + mu4,
    
    mu1 <- A[Sex] + betas[Grp, 1 + (Sex - 1)*19] + # Intercept (fixed, then per-culture, per-Sex)
      BSexTyp[Sex]*MeasSext + betas[Grp, 2 + (Sex - 1)*19]*MeasSext + # Sex typicality
      BAsym[Sex]*Asym + betas[Grp, 3 + (Sex - 1)*19]*Asym + # Asymmetry
      BDIST [Sex]*DIST + betas[Grp, 4 + (Sex - 1)*19]*DIST + # Distinctiveness
      BAge[Sex]*Age + betas[Grp, 5 + (Sex - 1)*19]*Age + # Age
      B_L_Skin[Sex]*L_skin + betas[Grp, 6 + (Sex - 1)*19]*L_skin, # Skin lightness
    
    mu2 <-  B_L_iris[Sex]*L_iris + betas[Grp, 7 + (Sex - 1)*19]*L_iris + # Iris lightness
      B_a_iris[Sex]*a_iris + betas[Grp, 8 + (Sex - 1)*19]*a_iris + # Iris redness
      B_b_iris[Sex]*b_iris + betas[Grp, 9 + (Sex - 1)*19]*b_iris + # Iris yellowness
      B_L_sclera[Sex]*L_sclera + betas[Grp, 10 + (Sex - 1)*19]*L_sclera + # Sclera lightness
      B_a_sclera[Sex]*a_sclera + betas[Grp, 11 + (Sex - 1)*19]*a_sclera + # Sclera redness
      B_b_sclera[Sex]*b_sclera + betas[Grp, 12 + (Sex - 1)*19]*b_sclera,  # Sclera yellowness
    
    # Adding the selected quadratic terms (fixed + varying) 
    mu3 <- 
      B_L_Skin_sq[Sex]*L_skin^2 + betas[Grp, 13 + (Sex - 1)*19]*L_skin^2 + # Skin lightness
      B_L_iris_sq[Sex]*L_iris^2 + betas[Grp, 14 + (Sex - 1)*19]*L_iris^2 + # Iris lightness
      B_a_iris_sq[Sex]*a_iris^2 + betas[Grp, 15 + (Sex - 1)*19]*a_iris^2 + # Iris redness
      B_b_iris_sq[Sex]*b_iris^2 + betas[Grp, 16 + (Sex - 1)*19]*b_iris^2,  # Iris yellowness
    
    mu4 <- 
      B_L_sclera_sq[Sex]*L_sclera^2 + betas[Grp, 17 + (Sex - 1)*19]*L_sclera^2 + # Sclera lightness
      B_a_sclera_sq[Sex]*a_sclera^2 + betas[Grp, 18 + (Sex - 1)*19]*a_sclera^2 + # Sclera redness
      B_b_sclera_sq[Sex]*b_sclera^2 + betas[Grp, 19 + (Sex - 1)*19]*b_sclera^2, # Sclera yellowness
    
    # Non-centered parameterization for betas
    transpars> matrix[Grp, 38]:betas <- compose_noncentered(sigma_group, L_Rho_group, z_group),
    
    matrix[38, Grp]:z_group ~ normal(0, 1),
    
    # Hyperpriors for the group-level effects
    vector[38]:sigma_group ~ dexp(1),
    cholesky_factor_corr[38]:L_Rho_group ~ lkj_corr_cholesky(2),
    
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
    
    # Quadratic fixed: 
    vector[2]:B_L_Skin_sq ~ normal(0, 0.5),
    vector[2]:B_L_iris_sq ~ normal(0, 0.5),
    vector[2]:B_a_iris_sq ~ normal(0, 0.5),
    vector[2]:B_b_iris_sq ~ normal(0, 0.5),
    vector[2]:B_L_sclera_sq ~ normal(0, 0.5),
    vector[2]:B_a_sclera_sq ~ normal(0, 0.5),
    vector[2]:B_b_sclera_sq ~ normal(0, 0.5),
    
    sigma ~ dexp(1),
    
    gq> matrix[38, 38]:Rho_group <<- Chol_to_Corr(L_Rho_group)
    
  ), data = data_sext2, chains = 4, cores = 4, log_lik = T, iter = 2500, control = list(max_treedepth = 18, adapt_delta = 0.99)
)

post_2 <- extract.samples(m_1_2_SQ7_sextSEP)
save(m_1_2_SQ7_sextSEP, file="model_nonlinear_sext_SEP_12_06_25_2.Rdata")
saveRDS(post_2, file="model_nonlinear_sext_SEP_12_06_25_2_POST.RDS")



# 1_3) Linear terms where needed, terms with free exponent everyhwere else (mind to use the edited 
# (always non-zero) variables where needed): 

# (Večerní volací znak [VVZ]: "Klasik-varying_exponent-šroubky-a-matky-neřešim")

m_1_3_only_varying_sextSEP <- ulam( 
  alist(
    Sextypicality ~ normal(mu, sigma),
    
    mu <- mu1 + mu2 + mu3,
    
    mu1 <- A[Sex] + betas[Grp, 1 + (Sex - 1)*12] + # Intercept (fixed, then per-culture, per-Sex)
      BSexTyp[Sex]*MeasSext + betas[Grp, 2 + (Sex - 1)*12]*MeasSext + # Sex typicality
      BAsym[Sex]*Asym + betas[Grp, 3 + (Sex - 1)*12]*Asym + # Asymmetry
      BDIST [Sex]*DIST + betas[Grp, 4 + (Sex - 1)*12]*DIST + # Distinctiveness
      BAge[Sex]*Age + betas[Grp, 5 + (Sex - 1)*12]*Age, # Age
    
    mu2 <- 
      B_L_Skin_var[Sex]*L_skin^E_skin_L + betas[Grp, 6 + (Sex - 1)*12]*L_skin^E_skin_L + # Skin lightness
      B_L_iris_var[Sex]*L_iris^E_iris_L + betas[Grp, 7 + (Sex - 1)*12]*L_iris^E_iris_L + # Iris lightness
      B_a_iris_var[Sex]*a_iris^E_iris_a + betas[Grp, 8 + (Sex - 1)*12]*a_iris^E_iris_a + # Iris redness
      B_b_iris_var[Sex]*b_iris^E_iris_b + betas[Grp, 9 + (Sex - 1)*12]*b_iris^E_iris_b,  # Iris yellowness
    
    mu3 <- 
      B_L_sclera_var[Sex]*L_sclera^E_sclera_L + betas[Grp, 10 + (Sex - 1)*12]*L_sclera^E_sclera_L + # Sclera lightness
      B_a_sclera_var[Sex]*a_sclera^E_sclera_a + betas[Grp, 11 + (Sex - 1)*12]*a_sclera^E_sclera_a + # Sclera redness
      B_b_sclera_var[Sex]*b_sclera^E_sclera_b + betas[Grp, 12 + (Sex - 1)*12]*b_sclera^E_sclera_b, # Sclera yellowness
    
    # Za totohle stavu exponent nevaríruje, čili všechno, co odlišuje slopy pro jednotlivé skupiny (a bohužel taky pohlaví),
    # musí téct skrze bety, což je nešťastný. 
    
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
    vector[2]:B_L_Skin_var ~ normal(0, 0.5),
    vector[2]:B_L_iris_var ~ normal(0, 0.5),
    vector[2]:B_a_iris_var ~ normal(0, 0.5),
    vector[2]:B_b_iris_var ~ normal(0, 0.5),
    vector[2]:B_L_sclera_var ~ normal(0, 0.5),
    vector[2]:B_a_sclera_var ~ normal(0, 0.5),
    vector[2]:B_b_sclera_var ~ normal(0, 0.5),
    
    # Varying exponent (exponent může bejt jakejkoli, problém byl s tím záporem u samotné proměnné, doufám): 
    E_skin_L ~ dexp(1),
    E_iris_L ~ dexp(1),
    E_iris_a ~ dexp(1),
    E_iris_b ~ dexp(1),
    E_sclera_L ~ dexp(1),
    E_sclera_a ~ dexp(1),
    E_sclera_b ~ dexp(1),
    
    sigma ~ dexp(1),
    
    gq> matrix[24, 24]:Rho_group <<- Chol_to_Corr(L_Rho_group)
    
  ), data = data_sext2, chains = 4, cores = 4, log_lik = T, iter = 2500, control = list(max_treedepth = 18, adapt_delta = 0.99)
)

post_3 <- extract.samples(m_1_3_only_varying_sextSEP)
save(m_1_3_only_varying_sextSEP, file="model_nonlinear_sext_SEP_12_06_25_3.Rdata")
saveRDS(post_3, file="model_nonlinear_sext_SEP_12_06_25_3_POST.RDS")


# 1_4) Linear terms where needed, terms with free exponent everyhwere else (mind to use the edited 
# (always non-zero) variables where needed): 
# And with [sex] to account for potentially different exponents between men and women...

# (Večerní volací znak [VVZ]: "Klasik-varying_exponent-šroubky-a-matky-řEšim")

m_1_4_only_varying_sext_sex2SEP <- ulam( 
  alist(
    Sextypicality ~ normal(mu, sigma),
    
    mu <- mu1 + mu2 + mu3,
    
    mu1 <- A[Sex] + betas[Grp, 1 + (Sex - 1)*12] + # Intercept (fixed, then per-culture, per-Sex)
      BSexTyp[Sex]*MeasSext + betas[Grp, 2 + (Sex - 1)*12]*MeasSext + # Sex typicality
      BAsym[Sex]*Asym + betas[Grp, 3 + (Sex - 1)*12]*Asym + # Asymmetry
      BDIST [Sex]*DIST + betas[Grp, 4 + (Sex - 1)*12]*DIST + # Distinctiveness
      BAge[Sex]*Age + betas[Grp, 5 + (Sex - 1)*12]*Age, # Age
    
    mu2 <- 
      B_L_Skin_var[Sex]*L_skin^E_skin_L[Sex] + betas[Grp, 6 + (Sex - 1)*12]*L_skin^E_skin_L[Sex] + # Skin lightness
      B_L_iris_var[Sex]*L_iris^E_iris_L[Sex] + betas[Grp, 7 + (Sex - 1)*12]*L_iris^E_iris_L[Sex] + # Iris lightness
      B_a_iris_var[Sex]*a_iris^E_iris_a[Sex] + betas[Grp, 8 + (Sex - 1)*12]*a_iris^E_iris_a[Sex]+ # Iris redness
      B_b_iris_var[Sex]*b_iris^E_iris_b[Sex] + betas[Grp, 9 + (Sex - 1)*12]*b_iris^E_iris_b[Sex],  # Iris yellowness
    
    mu3 <- 
      B_L_sclera_var[Sex]*L_sclera^E_sclera_L[Sex] + betas[Grp, 10 + (Sex - 1)*12]*L_sclera^E_sclera_L[Sex] + # Sclera lightness
      B_a_sclera_var[Sex]*a_sclera^E_sclera_a[Sex] + betas[Grp, 11 + (Sex - 1)*12]*a_sclera^E_sclera_a[Sex] + # Sclera redness
      B_b_sclera_var[Sex]*b_sclera^E_sclera_b[Sex] + betas[Grp, 12 + (Sex - 1)*12]*b_sclera^E_sclera_b[Sex], # Sclera yellowness
    
    # Za totohle stavu exponent nevaríruje, čili všechno, co odlišuje slopy pro jednotlivé skupiny (a bohužel taky pohlaví),
    # musí téct skrze bety, což je nešťastný. 
    
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
    vector[2]:B_L_Skin_var ~ normal(0, 0.5),
    vector[2]:B_L_iris_var ~ normal(0, 0.5),
    vector[2]:B_a_iris_var ~ normal(0, 0.5),
    vector[2]:B_b_iris_var ~ normal(0, 0.5),
    vector[2]:B_L_sclera_var ~ normal(0, 0.5),
    vector[2]:B_a_sclera_var ~ normal(0, 0.5),
    vector[2]:B_b_sclera_var ~ normal(0, 0.5),
    
    # Varying exponent (exponent může bejt jakejkoli, problém byl s tím záporem u samotné proměnné, doufám): 
    vector[2]:E_skin_L ~ dexp(1),
    vector[2]:E_iris_L ~ dexp(1),
    vector[2]:E_iris_a ~ dexp(1),
    vector[2]:E_iris_b ~ dexp(1),
    vector[2]:E_sclera_L ~ dexp(1),
    vector[2]:E_sclera_a ~ dexp(1),
    vector[2]:E_sclera_b ~ dexp(1),
    
    sigma ~ dexp(1),
    
    gq> matrix[24, 24]:Rho_group <<- Chol_to_Corr(L_Rho_group)
    
  ), data = data_sext2, chains = 4, cores = 4, log_lik = T, iter = 2500, control = list(max_treedepth = 18, adapt_delta = 0.99)
)

post_4 <- extract.samples(m_1_4_only_varying_sext_sex2SEP)
save(m_1_4_only_varying_sext_sex2SEP, file="model_nonlinear_sext_SEP_12_06_25_4.Rdata")
saveRDS(post_4, file="model_nonlinear_sext_SEP_12_06_25_4_POST.RDS")


# 1-5) With "Free exponent" and quadratic terms; without [sex] in case of free exponent: 

# (Večerní volací znak [VVZ]: "Klasik-varying_exponent-exponent-šroubky-a-matky-neřešim")

m_1_5_FREE_QUAD_sextSEP <- ulam( 
  alist(
    Sextypicality ~ normal(mu, sigma),
    
    mu <- mu1 + mu2 + mu3 + mu4,
    
    mu1 <- A[Sex] + betas[Grp, 1 + (Sex - 1)*19] + # Intercept (fixed, then per-culture, per-Sex)
      BSexTyp[Sex]*MeasSext + betas[Grp, 2 + (Sex - 1)*19]*MeasSext + # Sex typicality
      BAsym[Sex]*Asym + betas[Grp, 3 + (Sex - 1)*19]*Asym + # Asymmetry
      BDIST [Sex]*DIST + betas[Grp, 4 + (Sex - 1)*19]*DIST + # Distinctiveness
      BAge[Sex]*Age + betas[Grp, 5 + (Sex - 1)*19]*Age + # Age
      B_L_Skin[Sex]*L_skin + betas[Grp, 6 + (Sex - 1)*19]*L_skin, # Skin lightness
    
    mu2 <-  B_L_iris[Sex]*L_iris + betas[Grp, 7 + (Sex - 1)*19]*L_iris + # Iris lightness
      B_a_iris[Sex]*a_iris + betas[Grp, 8 + (Sex - 1)*19]*a_iris + # Iris redness
      B_b_iris[Sex]*b_iris + betas[Grp, 9 + (Sex - 1)*19]*b_iris + # Iris yellowness
      B_L_sclera[Sex]*L_sclera + betas[Grp, 10 + (Sex - 1)*19]*L_sclera + # Sclera lightness
      B_a_sclera[Sex]*a_sclera + betas[Grp, 11 + (Sex - 1)*19]*a_sclera + # Sclera redness
      B_b_sclera[Sex]*b_sclera + betas[Grp, 12 + (Sex - 1)*19]*b_sclera,  # Sclera yellowness
    
    mu3 <- 
      B_L_Skin_var[Sex]*L_skin^E_skin_L + betas[Grp, 6 + (Sex - 1)*19]*L_skin^E_skin_L + # Skin lightness
      B_L_iris_var[Sex]*L_iris^E_iris_L + betas[Grp, 7 + (Sex - 1)*19]*L_iris^E_iris_L + # Iris lightness
      B_a_iris_var[Sex]*a_iris^E_iris_a + betas[Grp, 8 + (Sex - 1)*19]*a_iris^E_iris_a + # Iris redness
      B_b_iris_var[Sex]*b_iris^E_iris_b + betas[Grp, 9 + (Sex - 1)*19]*b_iris^E_iris_b,  # Iris yellowness
    
    mu4 <- 
      B_L_sclera_var[Sex]*L_sclera^E_sclera_L + betas[Grp, 10 + (Sex - 1)*19]*L_sclera^E_sclera_L + # Sclera lightness
      B_a_sclera_var[Sex]*a_sclera^E_sclera_a + betas[Grp, 11 + (Sex - 1)*19]*a_sclera^E_sclera_a + # Sclera redness
      B_b_sclera_var[Sex]*b_sclera^E_sclera_b + betas[Grp, 12 + (Sex - 1)*19]*b_sclera^E_sclera_b, # Sclera yellowness
    
    # Non-centered parameterization for betas
    transpars> matrix[Grp, 38]:betas <- compose_noncentered(sigma_group, L_Rho_group, z_group),
    
    matrix[38, Grp]:z_group ~ normal(0, 1),
    
    # Hyperpriors for the group-level effects
    vector[38]:sigma_group ~ dexp(1),
    cholesky_factor_corr[38]:L_Rho_group ~ lkj_corr_cholesky(2),
    
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
    
    # Free exponents - priors: 
    E_skin_L ~ dexp(1),
    
    E_iris_L ~ dexp(1),
    E_iris_a ~ dexp(1),
    E_iris_b ~ dexp(1),
    
    E_sclera_L ~ dexp(1),
    E_sclera_a ~ dexp(1),
    E_sclera_b ~ dexp(1),
    
    # Free exponent's betas - priors: 
    vector[2]:B_L_Skin_var ~ normal(0, 0.5),
    vector[2]:B_L_iris_var ~ normal(0, 0.5),
    vector[2]:B_a_iris_var ~ normal(0, 0.5),
    vector[2]:B_b_iris_var ~ normal(0, 0.5),
    vector[2]:B_L_sclera_var ~ normal(0, 0.5),
    vector[2]:B_a_sclera_var ~ normal(0, 0.5),
    vector[2]:B_b_sclera_var ~ normal(0, 0.5),
    
    sigma ~ dexp(1),
    
    gq> matrix[38, 38]:Rho_group <<- Chol_to_Corr(L_Rho_group)
    
  ), data = data_sext2, chains = 4, cores = 4, log_lik = T, iter = 2500, control = list(max_treedepth = 18, adapt_delta = 0.99)
)

post_5 <- extract.samples(m_1_5_FREE_QUAD_sextSEP)
save(m_1_5_FREE_QUAD_sextSEP, file="model_nonlinear_sext_SEP_12_06_25_5.Rdata")
saveRDS(post_5, file="model_nonlinear_sext_SEP_12_06_25_5_POST.RDS")



# 1-6) With "Free exponent" and quadratic terms, and sex[] in free exponents...: 

# (Večerní volací znak [VVZ]: "Klasik-varying_exponent-exponent-šroubky-a-matky-řešim")

# Vona to možná není blbost, ono tady by v případě obligátně kvadratického vztahu mělo vyjít 
# volnej exponent poblíže jedný
# a nějakej slope pro ten kvadrát - a ten model by měl bejt teoreticky nejlepší podle WAIC. 

m_1_6_FREE_QUAD_SEX_sextSEP <- ulam( 
  alist(
    Sextypicality ~ normal(mu, sigma),
    
    mu <- mu1 + mu2 + mu3 + mu4,
    
    mu1 <- A[Sex] + betas[Grp, 1 + (Sex - 1)*19] + # Intercept (fixed, then per-culture, per-Sex)
      BSexTyp[Sex]*MeasSext + betas[Grp, 2 + (Sex - 1)*19]*MeasSext + # Sex typicality
      BAsym[Sex]*Asym + betas[Grp, 3 + (Sex - 1)*19]*Asym + # Asymmetry
      BDIST [Sex]*DIST + betas[Grp, 4 + (Sex - 1)*19]*DIST + # Distinctiveness
      BAge[Sex]*Age + betas[Grp, 5 + (Sex - 1)*19]*Age + # Age
      B_L_Skin[Sex]*L_skin + betas[Grp, 6 + (Sex - 1)*19]*L_skin, # Skin lightness
    
    mu2 <-  B_L_iris[Sex]*L_iris + betas[Grp, 7 + (Sex - 1)*19]*L_iris + # Iris lightness
      B_a_iris[Sex]*a_iris + betas[Grp, 8 + (Sex - 1)*19]*a_iris + # Iris redness
      B_b_iris[Sex]*b_iris + betas[Grp, 9 + (Sex - 1)*19]*b_iris + # Iris yellowness
      B_L_sclera[Sex]*L_sclera + betas[Grp, 10 + (Sex - 1)*19]*L_sclera + # Sclera lightness
      B_a_sclera[Sex]*a_sclera + betas[Grp, 11 + (Sex - 1)*19]*a_sclera + # Sclera redness
      B_b_sclera[Sex]*b_sclera + betas[Grp, 12 + (Sex - 1)*19]*b_sclera,  # Sclera yellowness
    
    mu3 <- 
      B_L_Skin_var[Sex]*L_skin^E_skin_L[Sex] + betas[Grp, 6 + (Sex - 1)*19]*L_skin^E_skin_L[Sex] + # Skin lightness
      B_L_iris_var[Sex]*L_iris^E_iris_L[Sex] + betas[Grp, 7 + (Sex - 1)*19]*L_iris^E_iris_L[Sex] + # Iris lightness
      B_a_iris_var[Sex]*a_iris^E_iris_a[Sex] + betas[Grp, 8 + (Sex - 1)*19]*a_iris^E_iris_a[Sex] + # Iris redness
      B_b_iris_var[Sex]*b_iris^E_iris_b[Sex] + betas[Grp, 9 + (Sex - 1)*19]*b_iris^E_iris_b[Sex],  # Iris yellowness
    
    mu4 <- 
      B_L_sclera_var[Sex]*L_sclera^E_sclera_L[Sex] + betas[Grp, 10 + (Sex - 1)*19]*L_sclera^E_sclera_L[Sex] + # Sclera lightness
      B_a_sclera_var[Sex]*a_sclera^E_sclera_a[Sex] + betas[Grp, 11 + (Sex - 1)*19]*a_sclera^E_sclera_a[Sex] + # Sclera redness
      B_b_sclera_var[Sex]*b_sclera^E_sclera_b[Sex] + betas[Grp, 12 + (Sex - 1)*19]*b_sclera^E_sclera_b[Sex], # Sclera yellowness
    
    # Non-centered parameterization for betas
    transpars> matrix[Grp, 38]:betas <- compose_noncentered(sigma_group, L_Rho_group, z_group),
    
    matrix[38, Grp]:z_group ~ normal(0, 1),
    
    # Hyperpriors for the group-level effects
    vector[38]:sigma_group ~ dexp(1),
    cholesky_factor_corr[38]:L_Rho_group ~ lkj_corr_cholesky(2),
    
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
    
    # Free exponents - priors: 
    vector[2]:E_skin_L ~ dexp(1),
    
    vector[2]:E_iris_L ~ dexp(1),
    vector[2]:E_iris_a ~ dexp(1),
    vector[2]:E_iris_b ~ dexp(1),
    
    vector[2]:E_sclera_L ~ dexp(1),
    vector[2]:E_sclera_a ~ dexp(1),
    vector[2]:E_sclera_b ~ dexp(1),
    
    # Free exponent's betas - priors: 
    vector[2]:B_L_Skin_var ~ normal(0, 0.5),
    vector[2]:B_L_iris_var ~ normal(0, 0.5),
    vector[2]:B_a_iris_var ~ normal(0, 0.5),
    vector[2]:B_b_iris_var ~ normal(0, 0.5),
    vector[2]:B_L_sclera_var ~ normal(0, 0.5),
    vector[2]:B_a_sclera_var ~ normal(0, 0.5),
    vector[2]:B_b_sclera_var ~ normal(0, 0.5),
    
    sigma ~ dexp(1),
    
    gq> matrix[38, 38]:Rho_group <<- Chol_to_Corr(L_Rho_group)
    
  ), data = data_sext2, chains = 4, cores = 4, log_lik = T, iter = 2500, control = list(max_treedepth = 18, adapt_delta = 0.99)
)


post_6 <- extract.samples(m_1_6_FREE_QUAD_SEX_sextSEP)
save(m_1_6_FREE_QUAD_SEX_sextSEP, file="model_nonlinear_sext_SEP_12_06_25_6.Rdata")
saveRDS(post_6, file="model_nonlinear_sext_SEP_12_06_25_6_POST.RDS")


m_1_sextSEP
                      

compare(m_1_sextSEP,
        m_1_2_SQ7_sextSEP, 
        m_1_3_only_varying_sextSEP,
        m_1_4_only_varying_sext_sex2SEP,
        m_1_5_FREE_QUAD_sextSEP, 
        m_1_6_FREE_QUAD_SEX_sextSEP)


write.csv(WAIC_SPSX, file="WAIC_Sextypicality_separated.csv")


#                                   WAIC    SE dWAIC   dSE pWAIC weight
# m_1_4_only_varying_sext_sex2SEP 2383.4 47.86   0.0    NA  79.2   0.81
# m_1_3_only_varying_sextSEP      2387.3 47.91   3.9  2.47  78.4   0.11
# m_1_sextSEP                     2389.0 48.51   5.6  7.54  92.7   0.05
# m_1_6_FREE_QUAD_SEX_sextSEP     2390.8 49.04   7.4  7.19  92.1   0.02
# m_1_5_FREE_QUAD_sextSEP         2392.1 49.01   8.7  7.11  92.5   0.01
# m_1_2_SQ7_sextSEP               2412.9 48.72  29.5 11.93 119.0   0.00


M1_coefs <- precis(m_1_attrSEP, depth=3, prob=0.95)
M2_coefs <- precis(m_1_2_SQ7_attrSEP, depth=3, prob=0.95)
M3_coefs <- precis(m_1_3_only_varying_attrSEP, depth=3, prob=0.95)
M4_coefs <- precis(m_1_4_only_varying_attr_sex2SEP, depth=3, prob=0.95)
M5_coefs <- precis(m_1_5_FREE_QUAD_attrSEP, depth=3, prob=0.95)
M6_coefs <- precis(m_1_6_FREE_QUAD_SEX_attrSEP, depth=3, prob=0.95)


write.csv2(M1_coefs, file="SEP_SEXT_Model_1.csv")
write.csv2(M2_coefs, file="SEP_SEXT_Model_2.csv")
write.csv2(M3_coefs, file="SEP_SEXT_Model_3.csv")
write.csv2(M4_coefs, file="SEP_SEXT_Model_4.csv")
write.csv2(M5_coefs, file="SEP_SEXT_Model_5.csv")
write.csv2(M6_coefs, file="SEP_SEXT_Model_6.csv")


# setwd! 
save.image("SEP_NonLinearModels_Image_Sextypicality_Standardised_Separately_13_06_25.Rdata")

