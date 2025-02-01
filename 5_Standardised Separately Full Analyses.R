
#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.--.-..--.-.-.-.-.

# Standardised Separately - analogue of the main analyses, described in the manuscript...

#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.--.-..--.-.-.-.-.

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

# Data could be scaled within or across the cultures:
# Let's see what would be better: 
# The most substantial cross-culture (or cross-photo sample) difference is expected for L*a*b:

# Standardisation:

culture_split <- split(Eye1[2:ncol(Eye1)], Eye1$Culture_Year)

standardized_split <- lapply(culture_split, function(sub_df) {
  # Identify numeric columns
  numeric_cols <- sapply(sub_df, is.numeric)
  
  # Standardize numeric columns
  sub_df[, numeric_cols] <- scale(sub_df[, numeric_cols])
})

Eye1_2 <- do.call(rbind, standardized_split)
Eye1_2 <- as.data.frame(Eye1_2)

Eye1_2$Culture_Year <- Eye1$Culture_Year
Eye1_2$sex <- Eye1$Sex


# Data: 
data_Attr <- list(
  Sex = as.integer(ifelse(Eye1_2$sex=="F",1,2)),
  Grp = as.integer(as.factor(Eye1_2$Culture_Year)),
  L_skin = (Eye1_2$L_skin),
  L_iris = (Eye1_2$L_iris),
  a_iris = (Eye1_2$a_iris),
  b_iris = (Eye1_2$b_iris),
  L_sclera = (Eye1_2$L_sclera),
  a_sclera = (Eye1_2$a_sclera),
  b_sclera = (Eye1_2$b_sclera),
  MeasSext = (Eye1_2$SexTypMeas),
  DIST = (Eye1_2$LOCAL_DIST),
  Asym = (Eye1_2$Asym),
  Age = (Eye1_2$Age), 
  Attractiveness = (Eye1_2$Attr)
)


summary.data.frame(data_Attr)  


# Analyses: 
library(rethinking)

#-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.

#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.

# MF

# 1 attractiveness when standardised separately

m3_attr <- ulam(
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

M3_coefs <- precis(m3_attr, depth=3, prob=0.95)
write.csv2(M3_coefs, file="Model_3_ATTRACT_BOTH_SEXES_Standardised_separately_COEFTAB.csv")
save(m3_attr, file="Model_3_ATTRACT_BOTH_SEXES_Standardised_separately.Rdata")

# Perculture: 
hist(M1_coefs$sd[1418:1633]) # Highest "SD" is around 0.17, which is IMO still OK.
M1_coefs$sd[1418:1633]


#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.

# Sextypicality (without Vietnamese), when standardised separately: 

Eye1_2 <- Eye1_2[!is.na(Eye1_2$SexT),]

# Data: 
data_Sext <- list(
  Sex = as.integer(ifelse(Eye1_2$sex=="F",1,2)),
  Grp = as.integer(as.factor(Eye1_2$Culture_Year)),
  L_skin = (Eye1_2$L_skin),
  L_iris = (Eye1_2$L_iris),
  a_iris = (Eye1_2$a_iris),
  b_iris = (Eye1_2$b_iris),
  L_sclera = (Eye1_2$L_sclera),
  a_sclera = (Eye1_2$a_sclera),
  b_sclera = (Eye1_2$b_sclera),
  MeasSext = (Eye1_2$SexTypMeas),
  DIST = (Eye1_2$LOCAL_DIST),
  Asym = (Eye1_2$Asym),
  Age = (Eye1_2$Age), 
  Sextypicality = (Eye1_2$SexT)
)

summary.data.frame(data_Sext)

m4_Sext <- ulam(
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

M4_Scoefs <- precis(m1_Sext, depth=3, prob=0.95)
write.csv2(M4_Scoefs, file="Model_4_SEXTYP_BOTH_SEXES_Standardised_separately_COEFTAB.csv")
save(m4_Sext, file="Model_4_SEXTYP_BOTH_SEXES_Standardised_separately.Rdata")


# Perculture: 
hist(M1_Scoefs$sd[1394:1585]) # Highest "SD" is around 0.17, which is IMO still OK.
M1_Scoefs$sd[1394:1585]




