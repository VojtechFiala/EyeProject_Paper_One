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


# Checking if the shift "all-is-above-zero", which is needed for "varying exponent" is OK here...
# that is - if the range is kept
hist(stan_L_skin(Eye$L_skin)) 
hist(stan_L_skin(Eye$L_skin)-min(stan_L_skin(Eye$L_skin))+0.001)

diff(range(stan_L_skin(Eye$L_skin))) 
diff(range(stan_L_skin(Eye$L_skin)-min(stan_L_skin(Eye$L_skin))+0.001))
# ól good! 

data_Attr2 <- list(
  Sex = as.integer(ifelse(Eye$Sex=="F",1,2)),
  Grp = as.integer(as.factor(Eye$Culture_Year)),
  L_skin = stan_L_skin(Eye$L_skin), # Not to be used
  L_iris = stan_L_iris(Eye$L_iris), # Not to be used
  a_iris = stan_a_iris(Eye$a_iris), # Not to be used
  b_iris = stan_b_iris(Eye$b_iris), # Not to be used
  L_sclera = stan_L_sclera(Eye$L_sclera), # Not to be used
  a_sclera = stan_a_sclera(Eye$a_sclera), # Not to be used
  b_sclera = stan_b_sclera(Eye$b_sclera), # Not to be used
  L_skin_nz = stan_L_skin(Eye$L_skin)-min(stan_L_skin(Eye$L_skin))+0.001, # NZ = nonzero
  L_iris_nz = stan_L_iris(Eye$L_iris)-min(stan_L_iris(Eye$L_iris))+0.001, # NZ = nonzero
  a_iris_nz = stan_a_iris(Eye$a_iris)-min(stan_a_iris(Eye$a_iris))+0.001, # NZ = nonzero
  b_iris_nz = stan_b_iris(Eye$b_iris)-min(stan_b_iris(Eye$b_iris))+0.001, # NZ = nonzero
  L_sclera_nz = stan_L_sclera(Eye$L_sclera)-min(stan_L_sclera(Eye$L_sclera))+0.001, # NZ = nonzero
  a_sclera_nz = stan_a_sclera(Eye$a_sclera)-min(stan_a_sclera(Eye$a_sclera))+0.001, # NZ = nonzero
  b_sclera_nz = stan_b_sclera(Eye$b_sclera)-min(stan_b_sclera(Eye$b_sclera))+0.001, # NZ = nonzero
  MeasSext = stan_MeasSext(Eye$SexTypMeas)-min(stan_MeasSext(Eye$SexTypMeas))+0.001,
  DIST = stan_DIST(Eye$LOCAL_DIST)-min(stan_DIST(Eye$LOCAL_DIST))+0.001,
  Asym = stan_Asym(Eye$Asym)-min(stan_Asym(Eye$Asym))+0.001,
  Age = stan_Age(Eye$Age)-min(stan_Age(Eye$Age))+0.001,
  Attractiveness = stan_Attractiveness(Eye$Attr)-min(stan_Attractiveness(Eye$Attr))+0.001
)

summary.data.frame(data_Attr2)


# Analyses: 
#-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.
#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.

library(rethinking)

# 1 Attractiveness: 

# 1_1) Without any non-linear terms: 

m_1_attr <- ulam( 
  alist(
    Attractiveness ~ normal(mu, sigma),
    
    mu <- mu1 + mu2 + mu3,
    
    mu1 <- A[Sex] + 
      betas[Grp, 1 + (Sex - 1)*12] + # Intercept (fixed, then per-culture, per-Sex)
      BSexTyp[Sex]*MeasSext + betas[Grp, 2 + (Sex - 1)*12]*MeasSext + # Sex typicality
      BAsym[Sex]*Asym + betas[Grp, 3 + (Sex - 1)*12]*Asym + # Asymmetry
      BDIST [Sex]*DIST + betas[Grp, 4 + (Sex - 1)*12]*DIST + # Distinctiveness
      BAge[Sex]*Age + betas[Grp, 5 + (Sex - 1)*12]*Age, # Age
    
    mu2 <- B_L_Skin[Sex]*L_skin_nz + betas[Grp, 6 + (Sex - 1)*12]*L_skin_nz +  # Skin lightness
      B_L_iris[Sex]*L_iris_nz + betas[Grp, 7 + (Sex - 1)*12]*L_iris_nz, # Iris lightness
      
    mu3 <-   B_a_iris[Sex]*a_iris_nz + betas[Grp, 8 + (Sex - 1)*12]*a_iris_nz + # Iris redness
      B_b_iris[Sex]*b_iris_nz + betas[Grp, 9 + (Sex - 1)*12]*b_iris_nz + # Iris yellowness
      B_L_sclera[Sex]*L_sclera_nz + betas[Grp, 10 + (Sex - 1)*12]*L_sclera_nz + # Sclera lightness
      B_a_sclera[Sex]*a_sclera_nz + betas[Grp, 11 + (Sex - 1)*12]*a_sclera_nz + # Sclera redness
      B_b_sclera[Sex]*b_sclera_nz + betas[Grp, 12 + (Sex - 1)*12]*b_sclera_nz,  # Sclera yellowness
    
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

post_1 <- extract.samples(m_1_attr)
save(m_1_attr, file="model_nonlinear_attr_21_06_25_1.Rdata")
saveRDS(post_1, file="model_nonlinear_attr_21_06_25_1_POST.RDS")


# x-------------------------------------------------------
# 1_2) All the considered quadratic terms + LINEAR: 

m_1_2_SQ7_attr <- ulam( 
  alist(
    Attractiveness ~ normal(mu, sigma),
    
    mu <- mu1 + mu2 + mu3 + mu4,
    
    mu1 <- A[Sex] + betas[Grp, 1 + (Sex - 1)*19] + # Intercept (fixed, then per-culture, per-Sex)
      BSexTyp[Sex]*MeasSext + betas[Grp, 2 + (Sex - 1)*19]*MeasSext + # Sex typicality
      BAsym[Sex]*Asym + betas[Grp, 3 + (Sex - 1)*19]*Asym + # Asymmetry
      BDIST [Sex]*DIST + betas[Grp, 4 + (Sex - 1)*19]*DIST + # Distinctiveness
      BAge[Sex]*Age + betas[Grp, 5 + (Sex - 1)*19]*Age + # Age
      B_L_Skin[Sex]*L_skin_nz + betas[Grp, 6 + (Sex - 1)*19]*L_skin_nz, # Skin lightness
    
    mu2 <-  B_L_iris[Sex]*L_iris_nz + betas[Grp, 7 + (Sex - 1)*19]*L_iris_nz + # Iris lightness
      B_a_iris[Sex]*a_iris_nz + betas[Grp, 8 + (Sex - 1)*19]*a_iris_nz + # Iris redness
      B_b_iris[Sex]*b_iris_nz + betas[Grp, 9 + (Sex - 1)*19]*b_iris_nz + # Iris yellowness
      B_L_sclera[Sex]*L_sclera_nz + betas[Grp, 10 + (Sex - 1)*19]*L_sclera_nz + # Sclera lightness
      B_a_sclera[Sex]*a_sclera_nz + betas[Grp, 11 + (Sex - 1)*19]*a_sclera_nz, # Sclera redness

    # Adding the selected quadratic terms (fixed + varying) 
    mu3 <- 
      B_b_sclera[Sex]*b_sclera_nz + betas[Grp, 12 + (Sex - 1)*19]*b_sclera_nz +  # Sclera yellowness
      B_L_Skin_sq[Sex]*L_skin_nz^2 + betas[Grp, 13 + (Sex - 1)*19]*L_skin_nz^2 + # Skin lightness
      B_L_iris_sq[Sex]*L_iris_nz^2 + betas[Grp, 14 + (Sex - 1)*19]*L_iris_nz^2 + # Iris lightness
      B_a_iris_sq[Sex]*a_iris_nz^2 + betas[Grp, 15 + (Sex - 1)*19]*a_iris_nz^2 + # Iris redness
      B_b_iris_sq[Sex]*b_iris_nz^2 + betas[Grp, 16 + (Sex - 1)*19]*b_iris_nz^2,  # Iris yellowness

    mu4 <- 
      B_L_sclera_sq[Sex]*L_sclera_nz^2 + betas[Grp, 17 + (Sex - 1)*19]*L_sclera_nz^2 + # Sclera lightness
      B_a_sclera_sq[Sex]*a_sclera_nz^2 + betas[Grp, 18 + (Sex - 1)*19]*a_sclera_nz^2 + # Sclera redness
      B_b_sclera_sq[Sex]*b_sclera_nz^2 + betas[Grp, 19 + (Sex - 1)*19]*b_sclera_nz^2, # Sclera yellowness
    
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

post_2 <- extract.samples(m_1_2_SQ7_attr)
save(m_1_2_SQ7_attr, file="model_nonlinear_attr_21_06_25_2.Rdata")
saveRDS(post_2, file="model_nonlinear_attr_21_06_25_2_POST.RDS")


# x-------------------------------------------------------
# 1_3) Linear terms where needed, terms with free exponent everyhwere else (mind to use the edited 
# (always non-zero) variables where needed): 


m_1_3_only_varying_attr <- ulam( 
  alist(
    Attractiveness ~ normal(mu, sigma),
    
    mu <- mu1 + mu2 + mu3,
    
    mu1 <- A[Sex] + betas[Grp, 1 + (Sex - 1)*12] + # Intercept (fixed, then per-culture, per-Sex)
      BSexTyp[Sex]*MeasSext + betas[Grp, 2 + (Sex - 1)*12]*MeasSext + # Sex typicality
      BAsym[Sex]*Asym + betas[Grp, 3 + (Sex - 1)*12]*Asym + # Asymmetry
      BDIST [Sex]*DIST + betas[Grp, 4 + (Sex - 1)*12]*DIST + # Distinctiveness
      BAge[Sex]*Age + betas[Grp, 5 + (Sex - 1)*12]*Age, # Age

    mu2 <- 
      B_L_Skin_var[Sex]*L_skin_nz^E_skin_L + betas[Grp, 6 + (Sex - 1)*12]*L_skin_nz^E_skin_L + # Skin lightness
      B_L_iris_var[Sex]*L_iris_nz^E_iris_L + betas[Grp, 7 + (Sex - 1)*12]*L_iris_nz^E_iris_L + # Iris lightness
      B_a_iris_var[Sex]*a_iris_nz^E_iris_a + betas[Grp, 8 + (Sex - 1)*12]*a_iris_nz^E_iris_a + # Iris redness
      B_b_iris_var[Sex]*b_iris_nz^E_iris_b + betas[Grp, 9 + (Sex - 1)*12]*b_iris_nz^E_iris_b,  # Iris yellowness
    
    mu3 <- 
      B_L_sclera_var[Sex]*L_sclera_nz^E_sclera_L + betas[Grp, 10 + (Sex - 1)*12]*L_sclera_nz^E_sclera_L + # Sclera lightness
      B_a_sclera_var[Sex]*a_sclera_nz^E_sclera_a + betas[Grp, 11 + (Sex - 1)*12]*a_sclera_nz^E_sclera_a + # Sclera redness
      B_b_sclera_var[Sex]*b_sclera_nz^E_sclera_b + betas[Grp, 12 + (Sex - 1)*12]*b_sclera_nz^E_sclera_b, # Sclera yellowness
    
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

post_3 <- extract.samples(m_1_3_only_varying_attr)
save(m_1_3_only_varying_attr, file="model_nonlinear_attr_21_06_25_3.Rdata")
saveRDS(post_3, file="model_nonlinear_attr_21_06_25_3_POST.RDS")

# x-------------------------------------------------------
# 1_4) Linear terms where needed, terms with free exponent everyhwere else (mind to use the edited 
# (always non-zero) variables where needed): 
# And with [sex] to account for potentially different exponents between men and women...

m_1_4_only_varying_attr_sex2 <- ulam( 
  alist(
    Attractiveness ~ normal(mu, sigma),
    
    mu <- mu1 + mu2 + mu3,
    
    mu1 <- A[Sex] + betas[Grp, 1 + (Sex - 1)*12] + # Intercept (fixed, then per-culture, per-Sex)
      BSexTyp[Sex]*MeasSext + betas[Grp, 2 + (Sex - 1)*12]*MeasSext + # Sex typicality
      BAsym[Sex]*Asym + betas[Grp, 3 + (Sex - 1)*12]*Asym + # Asymmetry
      BDIST [Sex]*DIST + betas[Grp, 4 + (Sex - 1)*12]*DIST + # Distinctiveness
      BAge[Sex]*Age + betas[Grp, 5 + (Sex - 1)*12]*Age, # Age
    
    mu2 <- 
      B_L_Skin_var[Sex]*L_skin_nz^E_skin_L[Sex] + betas[Grp, 6 + (Sex - 1)*12]*L_skin_nz^E_skin_L[Sex] + # Skin lightness
      B_L_iris_var[Sex]*L_iris_nz^E_iris_L[Sex] + betas[Grp, 7 + (Sex - 1)*12]*L_iris_nz^E_iris_L[Sex] + # Iris lightness
      B_a_iris_var[Sex]*a_iris_nz^E_iris_a[Sex] + betas[Grp, 8 + (Sex - 1)*12]*a_iris_nz^E_iris_a[Sex] + # Iris redness
      B_b_iris_var[Sex]*b_iris_nz^E_iris_b[Sex] + betas[Grp, 9 + (Sex - 1)*12]*b_iris_nz^E_iris_b[Sex],  # Iris yellowness
    
    mu3 <- 
      B_L_sclera_var[Sex]*L_sclera_nz^E_sclera_L[Sex] + betas[Grp, 10 + (Sex - 1)*12]*L_sclera_nz^E_sclera_L[Sex] + # Sclera lightness
      B_a_sclera_var[Sex]*a_sclera_nz^E_sclera_a[Sex] + betas[Grp, 11 + (Sex - 1)*12]*a_sclera_nz^E_sclera_a[Sex] + # Sclera redness
      B_b_sclera_var[Sex]*b_sclera_nz^E_sclera_b[Sex] + betas[Grp, 12 + (Sex - 1)*12]*b_sclera_nz^E_sclera_b[Sex], # Sclera yellowness
    
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

post_4 <- extract.samples(m_1_4_only_varying_attr_sex2)
save(m_1_4_only_varying_attr_sex2, file="model_nonlinear_attr_21_06_25_4.Rdata")
saveRDS(post_4, file="model_nonlinear_attr_21_06_25_4_POST.RDS")



# 1-5) With "Free exponent" and linear terms 

m_1_5_FREE_and_LINN_attr <- ulam( 
  alist(
    Attractiveness ~ normal(mu, sigma),
    
    mu <- mu1 + mu2 + mu3 + mu4,
    
    mu1 <- A[Sex] + betas[Grp, 1 + (Sex - 1)*19] + # Intercept (fixed, then per-culture, per-Sex)
      BSexTyp[Sex]*MeasSext + betas[Grp, 2 + (Sex - 1)*19]*MeasSext + # Sex typicality
      BAsym[Sex]*Asym + betas[Grp, 3 + (Sex - 1)*19]*Asym + # Asymmetry
      BDIST [Sex]*DIST + betas[Grp, 4 + (Sex - 1)*19]*DIST + # Distinctiveness
      BAge[Sex]*Age + betas[Grp, 5 + (Sex - 1)*19]*Age + # Age
      B_L_Skin[Sex]*L_skin_nz + betas[Grp, 6 + (Sex - 1)*19]*L_skin_nz, # Skin lightness
    
    mu2 <-  B_L_iris[Sex]*L_iris_nz + betas[Grp, 7 + (Sex - 1)*19]*L_iris_nz + # Iris lightness
      B_a_iris[Sex]*a_iris_nz + betas[Grp, 8 + (Sex - 1)*19]*a_iris_nz + # Iris redness
      B_b_iris[Sex]*b_iris_nz + betas[Grp, 9 + (Sex - 1)*19]*b_iris_nz + # Iris yellowness
      B_L_sclera[Sex]*L_sclera_nz + betas[Grp, 10 + (Sex - 1)*19]*L_sclera_nz + # Sclera lightness
      B_a_sclera[Sex]*a_sclera_nz + betas[Grp, 11 + (Sex - 1)*19]*a_sclera_nz, # Sclera redness
    
    mu3 <-       
      B_b_sclera[Sex]*b_sclera_nz + betas[Grp, 12 + (Sex - 1)*19]*b_sclera_nz +  # Sclera yellowness
      B_L_Skin_var[Sex]*L_skin_nz^E_skin_L + betas[Grp, 6 + (Sex - 1)*19]*L_skin_nz^E_skin_L + # Skin lightness
      B_L_iris_var[Sex]*L_iris_nz^E_iris_L + betas[Grp, 7 + (Sex - 1)*19]*L_iris_nz^E_iris_L + # Iris lightness
      B_a_iris_var[Sex]*a_iris_nz^E_iris_a + betas[Grp, 8 + (Sex - 1)*19]*a_iris_nz^E_iris_a, # Iris redness
    
    mu4 <-       
      B_b_iris_var[Sex]*b_iris_nz^E_iris_b + betas[Grp, 9 + (Sex - 1)*19]*b_iris_nz^E_iris_b +  # Iris yellowness
      B_L_sclera_var[Sex]*L_sclera_nz^E_sclera_L + betas[Grp, 10 + (Sex - 1)*19]*L_sclera_nz^E_sclera_L + # Sclera lightness
      B_a_sclera_var[Sex]*a_sclera_nz^E_sclera_a + betas[Grp, 11 + (Sex - 1)*19]*a_sclera_nz^E_sclera_a + # Sclera redness
      B_b_sclera_var[Sex]*b_sclera_nz^E_sclera_b + betas[Grp, 12 + (Sex - 1)*19]*b_sclera_nz^E_sclera_b, # Sclera yellowness
    
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

post_5 <- extract.samples(m_1_5_FREE_and_LINN_attr)
save(m_1_5_FREE_and_LINN_attr, file="model_nonlinear_attr_21_06_25_5.Rdata")
saveRDS(post_5, file="model_nonlinear_attr_21_06_25_5_POST.RDS")




# 1-6) With "Free exponent", "linear terms" and sex[]: 

m_1_6_FREE_MF_attr <- ulam( 
  alist(
    Attractiveness ~ normal(mu, sigma),
    
    mu <- mu1 + mu2 + mu3 + mu4 + mu5 + mu6 + mu7,
    
    mu1 <- A[Sex] + betas[Grp, 1 + (Sex - 1)*19] + # Intercept (fixed, then per-culture, per-Sex)
      BSexTyp[Sex]*MeasSext + betas[Grp, 2 + (Sex - 1)*19]*MeasSext + # Sex typicality
      BAsym[Sex]*Asym + betas[Grp, 3 + (Sex - 1)*19]*Asym + # Asymmetry
      BDIST [Sex]*DIST + betas[Grp, 4 + (Sex - 1)*19]*DIST + # Distinctiveness
      BAge[Sex]*Age + betas[Grp, 5 + (Sex - 1)*19]*Age, # Age

    mu2 <-        
      B_L_Skin[Sex]*L_skin_nz + betas[Grp, 6 + (Sex - 1)*19]*L_skin_nz + # Skin lightness
      B_L_iris[Sex]*L_iris_nz + betas[Grp, 7 + (Sex - 1)*19]*L_iris_nz + # Iris lightness
      B_a_iris[Sex]*a_iris_nz + betas[Grp, 8 + (Sex - 1)*19]*a_iris_nz, # Iris redness
    
    mu3 <-  B_b_iris[Sex]*b_iris_nz + betas[Grp, 9 + (Sex - 1)*19]*b_iris_nz + # Iris yellowness
      B_L_sclera[Sex]*L_sclera_nz + betas[Grp, 10 + (Sex - 1)*19]*L_sclera_nz, # Sclera lightness
    
    mu4 <-   
      B_a_sclera[Sex]*a_sclera_nz + betas[Grp, 11 + (Sex - 1)*19]*a_sclera_nz + # Sclera redness
      B_b_sclera[Sex]*b_sclera_nz + betas[Grp, 12 + (Sex - 1)*19]*b_sclera_nz +  # Sclera yellowness
      B_L_Skin_var[Sex]*L_skin_nz^E_skin_L[Sex] + betas[Grp, 6 + (Sex - 1)*19]*L_skin_nz^E_skin_L[Sex], # Skin lightness
    
    mu5 <-    
      B_L_iris_var[Sex]*L_iris_nz^E_iris_L[Sex] + betas[Grp, 7 + (Sex - 1)*19]*L_iris_nz^E_iris_L[Sex] + # Iris lightness
      B_a_iris_var[Sex]*a_iris_nz^E_iris_a[Sex] + betas[Grp, 8 + (Sex - 1)*19]*a_iris_nz^E_iris_a[Sex], # Iris redness
    
    mu6 <-       
      B_b_iris_var[Sex]*b_iris_nz^E_iris_b[Sex] + betas[Grp, 9 + (Sex - 1)*19]*b_iris_nz^E_iris_b[Sex] +  # Iris yellowness
      B_L_sclera_var[Sex]*L_sclera_nz^E_sclera_L[Sex] + betas[Grp, 10 + (Sex - 1)*19]*L_sclera_nz^E_sclera_L[Sex], # Sclera lightness
  
    mu7 <-  B_a_sclera_var[Sex]*a_sclera_nz^E_sclera_a[Sex] + betas[Grp, 11 + (Sex - 1)*19]*a_sclera_nz^E_sclera_a[Sex] + # Sclera redness
      B_b_sclera_var[Sex]*b_sclera_nz^E_sclera_b[Sex] + betas[Grp, 12 + (Sex - 1)*19]*b_sclera_nz^E_sclera_b[Sex], # Sclera yellowness
    
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


post_6 <- extract.samples(m_1_6_FREE_MF_attr)
save(m_1_6_FREE_MF_attr, file="model_nonlinear_attr_21_06_25_6.Rdata")
saveRDS(post_6, file="model_nonlinear_attr_21_06_25_6_POST.RDS")


compare(m_1_attr,
        m_1_2_SQ7_attr,
        m_1_3_only_varying_attr,
        m_1_4_only_varying_attr_sex2,
        m_1_5_FREE_and_LINN_attr,
        m_1_6_FREE_MF_attr
)

#                                WAIC    SE dWAIC   dSE pWAIC weight
# m_1_4_only_varying_attr_sex2 2355.3 53.74   0.0    NA  77.7   0.53
# m_1_3_only_varying_attr      2356.6 53.78   1.4  2.84  76.7   0.27
# m_1_6_FREE_MF_attr           2359.0 53.46   3.7  7.07  95.9   0.08 (tenhle je taky volnej a lineární)
# m_1_attr                     2359.0 53.91   3.7  7.16  96.3   0.08
# m_1_5_FREE_and_LINN_attr     2360.3 53.43   5.1  6.95  95.7   0.04
# m_1_2_SQ7_attr               2379.6 53.61  24.3 12.76 123.3   0.00

save.image("NonLinearModels__attr_10_06_25_Image.Rdata")

M1_coefs <- precis(m_1_attr, depth=3, prob=0.95)
M2_coefs <- precis(m_1_2_SQ7_attr, depth=3, prob=0.95)
M3_coefs <- precis(m_1_3_only_varying_attr, depth=3, prob=0.95)
M4_coefs <- precis(m_1_4_only_varying_attr_sex2, depth=3, prob=0.95)
M5_coefs <- precis(m_1_5_FREE_QUAD_attr, depth=3, prob=0.95)
M6_coefs <- precis(m_1_6_FREE_QUAD_SEX_attr, depth=3, prob=0.95)

#write.csv2(M1_coefs, file="Model_1.csv")
#write.csv2(M2_coefs, file="Model_2.csv")
#write.csv2(M3_coefs, file="Model_3.csv")
#write.csv2(M4_coefs, file="Model_4.csv")
#write.csv2(M5_coefs, file="Model_5.csv")
#write.csv2(M6_coefs, file="Model_6.csv")





