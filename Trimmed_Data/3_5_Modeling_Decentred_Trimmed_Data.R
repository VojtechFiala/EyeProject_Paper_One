# EXCLUDING potential OUTLIERS

# TYPE 1 - Exclude those who can be consider outliers "globally": 

# # Upload and check the data
Eye1 <- read.csv2("Data_03_08_24.csv",T)

tapply(Eye1$Attr,Eye1$Culture_Year, mean)

summary(Eye1)
Eye1 <- Eye1[!is.na(Eye1$Age),]

summary(as.factor(Eye1$Sex))
summary(as.factor(Eye1$Culture_Year))


table(Eye1$Culture_Year,as.integer(as.factor(Eye1$Culture_Year)))

# Excluding those who are more than 1.5-times the IQr away from median...
# Skin L*
Q1 <- quantile(Eye1$L_skin, 0.25)
Q3 <- quantile(Eye1$L_skin, 0.75)
IQR_val <- Q3 - Q1
lower_bound_L_Skin <- Q1 - 1.5 * IQR_val
upper_bound_L_Skin <- Q3 + 1.5 * IQR_val

# Iris L*
Q1 <- quantile(Eye1$L_iris, 0.25)
Q3 <- quantile(Eye1$L_iris, 0.75)
IQR_val <- Q3 - Q1
lower_bound_L_iris <- Q1 - 1.5 * IQR_val
upper_bound_L_iris <- Q3 + 1.5 * IQR_val

# Sclera L*
Q1 <- quantile(Eye1$L_sclera, 0.25)
Q3 <- quantile(Eye1$L_sclera, 0.75)
IQR_val <- Q3 - Q1
lower_bound_L_sclera <- Q1 - 1.5 * IQR_val
upper_bound_L_sclera <- Q3 + 1.5 * IQR_val

# Iris a*
Q1 <- quantile(Eye1$a_iris, 0.25)
Q3 <- quantile(Eye1$a_iris, 0.75)
IQR_val <- Q3 - Q1
lower_bound_a_iris <- Q1 - 1.5 * IQR_val
upper_bound_a_iris <- Q3 + 1.5 * IQR_val

# Sclera a*
Q1 <- quantile(Eye1$a_sclera, 0.25)
Q3 <- quantile(Eye1$a_sclera, 0.75)
IQR_val <- Q3 - Q1
lower_bound_a_sclera <- Q1 - 1.5 * IQR_val
upper_bound_a_sclera <- Q3 + 1.5 * IQR_val

# Iris b*
Q1 <- quantile(Eye1$b_iris, 0.25)
Q3 <- quantile(Eye1$b_iris, 0.75)
IQR_val <- Q3 - Q1
lower_bound_b_iris <- Q1 - 1.5 * IQR_val
upper_bound_b_iris <- Q3 + 1.5 * IQR_val

# Sclera b*
Q1 <- quantile(Eye1$b_sclera, 0.25)
Q3 <- quantile(Eye1$b_sclera, 0.75)
IQR_val <- Q3 - Q1
lower_bound_b_sclera <- Q1 - 1.5 * IQR_val
upper_bound_b_sclera <- Q3 + 1.5 * IQR_val

# AVRG/DIST 
Q1 <- quantile(Eye1$LOCAL_DIST, 0.25)
Q3 <- quantile(Eye1$LOCAL_DIST, 0.75)
IQR_val <- Q3 - Q1
lower_bound_dist <- Q1 - 1.5 * IQR_val
upper_bound_dist <- Q3 + 1.5 * IQR_val

# SexTypMeas
Q1 <- quantile(Eye1$SexTypMeas, 0.25)
Q3 <- quantile(Eye1$SexTypMeas, 0.75)
IQR_val <- Q3 - Q1
lower_bound_SexTypMeas <- Q1 - 1.5 * IQR_val
upper_bound_SexTypMeas <- Q3 + 1.5 * IQR_val

# FA
Q1 <- quantile(Eye1$Asym, 0.25)
Q3 <- quantile(Eye1$Asym, 0.75)
IQR_val <- Q3 - Q1
lower_bound_Asym <- Q1 - 1.5 * IQR_val
upper_bound_Asym <- Q3 + 1.5 * IQR_val

# Age
Q1 <- quantile(Eye1$Age, 0.25)
Q3 <- quantile(Eye1$Age, 0.75)
IQR_val <- Q3 - Q1
lower_bound_Age <- Q1 - 1.5 * IQR_val
upper_bound_Age <- Q3 + 1.5 * IQR_val

# Perc SexTyp
Q1 <- quantile(Eye1$SexT, 0.25, na.rm = T)
Q3 <- quantile(Eye1$SexT, 0.75, na.rm = T)
IQR_val <- Q3 - Q1
lower_bound_PercSexTyp <- Q1 - 1.5 * IQR_val
upper_bound_PercSexTyp <- Q3 + 1.5 * IQR_val

# Attr
Q1 <- quantile(Eye1$Attr, 0.25)
Q3 <- quantile(Eye1$Attr, 0.75)
IQR_val <- Q3 - Q1
lower_bound_Attr <- Q1 - 1.5 * IQR_val
upper_bound_Attr <- Q3 + 1.5 * IQR_val

Eye2 <- Eye1[Eye1$L_skin < upper_bound_L_Skin & Eye1$L_skin > lower_bound_L_Skin,]
Eye2 <- Eye2[Eye2$L_iris < upper_bound_L_iris & Eye2$L_iris > lower_bound_L_iris,]
Eye2 <- Eye2[Eye2$L_sclera < upper_bound_L_sclera & Eye2$L_sclera > lower_bound_L_sclera,]

Eye2 <- Eye2[Eye2$a_iris < upper_bound_a_iris & Eye2$a_iris > lower_bound_a_iris,]
Eye2 <- Eye2[Eye2$a_sclera < upper_bound_a_sclera & Eye2$a_sclera > lower_bound_a_sclera,]
Eye2 <- Eye2[Eye2$a_sclera < upper_bound_a_sclera & Eye2$a_sclera > lower_bound_a_sclera,]

Eye2 <- Eye2[Eye2$b_sclera < upper_bound_b_sclera & Eye2$b_sclera > lower_bound_b_sclera,]
Eye2 <- Eye2[Eye2$b_iris < upper_bound_b_iris & Eye2$b_iris > lower_bound_b_iris,]

Eye2 <- Eye2[Eye2$LOCAL_DIST < upper_bound_dist & Eye2$LOCAL_DIST > lower_bound_dist,]
Eye2 <- Eye2[Eye2$SexTypMeas < upper_bound_SexTypMeas & Eye2$SexTypMeas > lower_bound_SexTypMeas,]
Eye2 <- Eye2[Eye2$Asym < upper_bound_Asym & Eye2$Asym > lower_bound_Asym,]

Eye2 <- Eye2[Eye2$Age < upper_bound_Age & Eye2$Age > lower_bound_Age,]
Eye2 <- Eye2[is.na(Eye2$SexT) | (Eye2$SexT < upper_bound_PercSexTyp & Eye2$SexT > lower_bound_PercSexTyp), ]
Eye2 <- Eye2[Eye2$Attr < upper_bound_Attr & Eye2$Attr > lower_bound_Attr,]

summary(as.factor(Eye2$Culture_Year))



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

Eye <- Eye2

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

m1_attr_1_5_IQR <- ulam( 
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
    
  ), data = data_Attr2, chains = 4, cores = 4, log_lik = F, iter = 2500, control = list(max_treedepth = 18, adapt_delta = 0.99)
)

M1__1_5_IQRcoefs <- precis(m1_attr_1_5_IQR, depth=3, prob=0.95)
write.csv2(M1__1_5_IQRcoefs, file="Model_1_1_5Q_ATTRACT_both_sexes_standardised_together_Coeftab_outliers_excluded_together.csv")
post_m1_1_5_IQR <- extract.samples(m1_attr_1_5_IQR)
saveRDS(post_m1_1_5_IQR, file="post_m1_1_5_IQR__both_sexes_standardised_together_Coeftab_outliers_excluded_together_posterior.RDS")
save(m1_attr_1_5_IQR, file="Model_1_1_5Q_ATTRACT__both_sexes_standardised_together_Coeftab_outliers_excluded_together.Rdata")


# Note, on the first run, it threw a warning "warning message: In doTryCatch(return(expr), name, parentenv, handler) : restarting 
# interrupted promise evaluation". Therefore, by accidnet, the model was run three times: First time using data scale using "scale" 
# function, then with custom-made function and finally to check whether the warning is, as the Internet says "just a thing that 
# sometimes pops-up as a consequence of background processess in the OS". That is why we know that for for L* of sclera, in women, 
# the effect is steadily close to 0.30, but the lower end of the 95 % CI tends to just over/under-run the zero. It has no statistical 
# importance, but mind this during re-analysis. You may get a result that is just / is just not credibly non-zero with regard to the 
# selected fraction of CI - 95 %. 


#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.

# 2 Sextypicality

Eye3 <- Eye2[!is.na(Eye2$SexT),] # I need Eye2 as a different object, since sextypicality has not been rated in Vietnam, 
# Then, Vietnam needs to be exluced from the data...
Eye  <- Eye3 

# SCALED AFTER EXCLUSION! 


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

summary.data.frame(data_sext2)  



m2__1_5_IQR_Sext <- ulam(
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
    
  ), data = data_sext2, chains = 4, cores = 4, log_lik = F, iter = 2500, control = list(max_treedepth = 18, adapt_delta = 0.99)
)

M2_1_5_IQR_Scoefs <- precis(m2__1_5_IQR_Sext, depth=3, prob=0.95)
write.csv2(M2_1_5_IQR_Scoefs, file="Model_2_1_5_IQR_SEXTYP_both_sexes_standardised_together_Coeftab_outliers_excluded_together_Coeftab.csv")
post_m2_1_5_IQR_Sext <- extract.samples(m2__1_5_IQR_Sext)
saveRDS(post_m2_1_5_IQR_Sext, file="post_m1_1_5_IQR__both_sexes_standardised_together_Coeftab_outliers_excluded_together_posterior.RDS")
save(m2__1_5_IQR_Sext, file="Model_2_1_5_IQR_SEXTYP__both_sexes_standardised_together_Coeftab_outliers_excluded_together.Rdata") 




#-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.
# 1.5*IQR within each sample, not across them...
#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.

# Analysis that includes quadratic effects for selected coefficients (eye colour)...


# # Upload and check the data
Eye1 <- read.csv2("Data_03_08_24.csv",T)

tapply(Eye1$Attr,Eye1$Culture_Year, mean)

summary(Eye1)
Eye1 <- Eye1[!is.na(Eye1$Age),]

summary(as.factor(Eye1$Sex))
summary((Eye1))


# Note - this variant is without SexTypicality, since there is a vector of NAs for culture = Vietnam
# The same process will be then repeated for SexTypicality, when Vietnam is already dropped.

# Copy the original data
Eye1_clean <- Eye1

# Get list of cultures and numeric variables
cultures <- unique(Eye1_clean$Culture_Year)
vars <- colnames(Eye1_clean)[c(4:12,16:18)]

# Initialise a logical vector: TRUE = keep, FALSE = drop
keep_row <- rep(TRUE, nrow(Eye1_clean))

# Loop through each culture and variable
for (cult in cultures) {
  for (var in vars) {
    # Get indices for rows in this culture
    idx <- which(Eye1_clean$Culture_Year == cult)
    
    # Extract values for this variable and culture
    vals <- Eye1_clean[idx, var]
    q1 <- quantile(vals, 0.25)
    q3 <- quantile(vals, 0.75)
    iqr <- q3 - q1
    lower <- q1 - 1.5 * iqr
    upper <- q3 + 1.5 * iqr
    
    # Mark rows as FALSE if any value is an outlier
    outlier_idx <- idx[vals < lower | vals > upper]
    keep_row[outlier_idx] <- FALSE
  }
}

# Subset the data: only rows where all variables are within bounds
Eye1_filtered <- Eye1_clean[keep_row, ]

summary(as.factor(Eye1_filtered$Culture_Year))
summary.data.frame(Eye1_filtered)


# Keep going with Eye1_filtered...

data_Attr2 <- list(
  Sex = as.integer(ifelse(Eye1_filtered$Sex=="F",1,2)),
  Grp = as.integer(as.factor(Eye1_filtered$Culture_Year)),
  L_skin = stan_L_skin(Eye1_filtered$L_skin),
  L_iris = stan_L_iris(Eye1_filtered$L_iris),
  a_iris = stan_a_iris(Eye1_filtered$a_iris),
  b_iris = stan_b_iris(Eye1_filtered$b_iris),
  L_sclera = stan_L_sclera(Eye1_filtered$L_sclera),
  a_sclera = stan_a_sclera(Eye1_filtered$a_sclera),
  b_sclera = stan_b_sclera(Eye1_filtered$b_sclera),
  MeasSext = stan_MeasSext(Eye1_filtered$SexTypMeas),
  DIST =stan_DIST(Eye1_filtered$LOCAL_DIST),
  Asym = stan_Asym(Eye1_filtered$Asym),
  Age = stan_Age(Eye1_filtered$Age), 
  Attractiveness = stan_Attractiveness(Eye1_filtered$Attr)
)

summary.data.frame(data_Attr2)



library(rethinking)

# 1 Attractiveness: 

m1_attr_1_5_IQR_separately <- ulam( 
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
    
  ), data = data_Attr2, chains = 4, cores = 4, log_lik = F, iter = 2500, control = list(max_treedepth = 18, adapt_delta = 0.99)
)

M1__1_5_IQR_separately_coefs <- precis(m1_attr_1_5_IQR_separately, depth=3, prob=0.95)
write.csv2(M1__1_5_IQR_separately_coefs, file="Model_1_1_5Q_ATTRACT_both_sexes_standardised_together_Coeftab_OUTLIERS_EXCLUDED_SEPARATELY.csv")
post_m1_1_5_IQR_separately <- extract.samples(m1_attr_1_5_IQR_separately)
saveRDS(post_m1_1_5_IQR_separately, file="post_m1_1_5_IQR_posterior_OUTLIERS_EXCLUDED_SEPARATELY.RDS")
save(m1_attr_1_5_IQR_separately, file="Model_1_1_5Q_ATTRACT_both_sexes_standardised_together_OUTLIERS_EXCLUDED_SEPARATELY.Rdata")



#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.

# 2 Sextypicality

Eye <- Eye1[!is.na(Eye1$SexT),] # I need Eye2 as a different object, since sextypicality has not been rated in Vietnam, 
# Then, Vietnam needs to be exluced from the data...

Eye <- Eye[!is.na(Eye$Age),]

summary(as.factor(Eye$Sex))
summary((Eye))


# Note - this variant is without SexTypicality, since there is a vector of NAs for culture = Vietnam
# The same process will be then repeated for SexTypicality, when Vietnam is already dropped.

# Copy the original data
Eye1_clean <- Eye

# Get list of cultures and numeric variables
cultures <- unique(Eye1_clean$Culture_Year)
vars <- colnames(Eye1_clean)[c(4:11,13,16:18)]

# Initialise a logical vector: TRUE = keep, FALSE = drop
keep_row <- rep(TRUE, nrow(Eye1_clean))

# Loop through each culture and variable
for (cult in cultures) {
  for (var in vars) {
    # Get indices for rows in this culture
    idx <- which(Eye1_clean$Culture_Year == cult)
    
    # Extract values for this variable and culture
    vals <- Eye1_clean[idx, var]
    q1 <- quantile(vals, 0.25)
    q3 <- quantile(vals, 0.75)
    iqr <- q3 - q1
    lower <- q1 - 1.5 * iqr
    upper <- q3 + 1.5 * iqr
    
    # Mark rows as FALSE if any value is an outlier
    outlier_idx <- idx[vals < lower | vals > upper]
    keep_row[outlier_idx] <- FALSE
  }
}

# Subset the data: only rows where all variables are within bounds
Eye1_filtered <- Eye1_clean[keep_row, ]

summary(as.factor(Eye1_filtered$Culture_Year))
summary.data.frame(Eye1_filtered)


# Keep going with Eye1_filtered...

data_sext2 <- list(
  Sex = as.integer(ifelse(Eye1_filtered$Sex=="F",1,2)),
  Grp = as.integer(as.factor(Eye1_filtered$Culture_Year)),
  L_skin = stan_L_skin(Eye1_filtered$L_skin),
  L_iris = stan_L_iris(Eye1_filtered$L_iris),
  a_iris = stan_a_iris(Eye1_filtered$a_iris),
  b_iris = stan_b_iris(Eye1_filtered$b_iris),
  L_sclera = stan_L_sclera(Eye1_filtered$L_sclera),
  a_sclera = stan_a_sclera(Eye1_filtered$a_sclera),
  b_sclera = stan_b_sclera(Eye1_filtered$b_sclera),
  MeasSext = stan_MeasSext(Eye1_filtered$SexTypMeas),
  DIST =stan_DIST(Eye1_filtered$LOCAL_DIST),
  Asym = stan_Asym(Eye1_filtered$Asym),
  Age = stan_Age(Eye1_filtered$Age), 
  Sextypicality = stan_SextP(as.numeric(Eye1_filtered$SexT))
)

summary.data.frame(data_sext2)  



m2__1_5_IQR_Sext_separately <- ulam(
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
    
  ), data = data_sext2, chains = 4, cores = 4, log_lik = F, iter = 2500, control = list(max_treedepth = 18, adapt_delta = 0.99)
)

M2_1_5_IQR_separately_Scoefs <- precis(m2__1_5_IQR_Sext_separately, depth=3, prob=0.95)
write.csv2(M2_1_5_IQR_separately_Scoefs, file="Model_2_1_5_IQR_SEXTYP_both_sexes_standardised_together_Coeftab_OUTLIERS_EXCLUDED_SEPARATELY.csv")
post_m2_1_5_IQR_separately_Sext <- extract.samples(m2__1_5_IQR_Sext_separately)
saveRDS(post_m2_1_5_IQR_separately_Sext, file="post_m1_1_5_IQR_separately_posterior_OUTLIERS_EXCLUDED_SEPARATELY.RDS")
save(m2__1_5_IQR_Sext_separately, file="Model_2_1_5_IQR_SEXTYP_both_sexes_standardised_together_OUTLIERS_EXCLUDED_SEPARATELY.Rdata") 












#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-

#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-

#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-

#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-

#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-

#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-

# Standardised separately

#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-

#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-

#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-

# PART ONE: 
# OUTLIERS EXCLUDED FROM THE POOLED DATA


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

# Exclude outliers: 

# Excluding those who are more than 1.5-times the IQr away from median...
# Skin L*
Q1 <- quantile(Eye1$L_skin, 0.25)
Q3 <- quantile(Eye1$L_skin, 0.75)
IQR_val <- Q3 - Q1
lower_bound_L_Skin <- Q1 - 1.5 * IQR_val
upper_bound_L_Skin <- Q3 + 1.5 * IQR_val

# Iris L*
Q1 <- quantile(Eye1$L_iris, 0.25)
Q3 <- quantile(Eye1$L_iris, 0.75)
IQR_val <- Q3 - Q1
lower_bound_L_iris <- Q1 - 1.5 * IQR_val
upper_bound_L_iris <- Q3 + 1.5 * IQR_val

# Sclera L*
Q1 <- quantile(Eye1$L_sclera, 0.25)
Q3 <- quantile(Eye1$L_sclera, 0.75)
IQR_val <- Q3 - Q1
lower_bound_L_sclera <- Q1 - 1.5 * IQR_val
upper_bound_L_sclera <- Q3 + 1.5 * IQR_val

# Iris a*
Q1 <- quantile(Eye1$a_iris, 0.25)
Q3 <- quantile(Eye1$a_iris, 0.75)
IQR_val <- Q3 - Q1
lower_bound_a_iris <- Q1 - 1.5 * IQR_val
upper_bound_a_iris <- Q3 + 1.5 * IQR_val

# Sclera a*
Q1 <- quantile(Eye1$a_sclera, 0.25)
Q3 <- quantile(Eye1$a_sclera, 0.75)
IQR_val <- Q3 - Q1
lower_bound_a_sclera <- Q1 - 1.5 * IQR_val
upper_bound_a_sclera <- Q3 + 1.5 * IQR_val

# Iris b*
Q1 <- quantile(Eye1$b_iris, 0.25)
Q3 <- quantile(Eye1$b_iris, 0.75)
IQR_val <- Q3 - Q1
lower_bound_b_iris <- Q1 - 1.5 * IQR_val
upper_bound_b_iris <- Q3 + 1.5 * IQR_val

# Sclera b*
Q1 <- quantile(Eye1$b_sclera, 0.25)
Q3 <- quantile(Eye1$b_sclera, 0.75)
IQR_val <- Q3 - Q1
lower_bound_b_sclera <- Q1 - 1.5 * IQR_val
upper_bound_b_sclera <- Q3 + 1.5 * IQR_val

# AVRG/DIST 
Q1 <- quantile(Eye1$LOCAL_DIST, 0.25)
Q3 <- quantile(Eye1$LOCAL_DIST, 0.75)
IQR_val <- Q3 - Q1
lower_bound_dist <- Q1 - 1.5 * IQR_val
upper_bound_dist <- Q3 + 1.5 * IQR_val

# SexTypMeas
Q1 <- quantile(Eye1$SexTypMeas, 0.25)
Q3 <- quantile(Eye1$SexTypMeas, 0.75)
IQR_val <- Q3 - Q1
lower_bound_SexTypMeas <- Q1 - 1.5 * IQR_val
upper_bound_SexTypMeas <- Q3 + 1.5 * IQR_val

# FA
Q1 <- quantile(Eye1$Asym, 0.25)
Q3 <- quantile(Eye1$Asym, 0.75)
IQR_val <- Q3 - Q1
lower_bound_Asym <- Q1 - 1.5 * IQR_val
upper_bound_Asym <- Q3 + 1.5 * IQR_val

# Age
Q1 <- quantile(Eye1$Age, 0.25)
Q3 <- quantile(Eye1$Age, 0.75)
IQR_val <- Q3 - Q1
lower_bound_Age <- Q1 - 1.5 * IQR_val
upper_bound_Age <- Q3 + 1.5 * IQR_val

# Perc SexTyp
Q1 <- quantile(Eye1$SexT, 0.25, na.rm = T)
Q3 <- quantile(Eye1$SexT, 0.75, na.rm = T)
IQR_val <- Q3 - Q1
lower_bound_PercSexTyp <- Q1 - 1.5 * IQR_val
upper_bound_PercSexTyp <- Q3 + 1.5 * IQR_val

# Attr
Q1 <- quantile(Eye1$Attr, 0.25)
Q3 <- quantile(Eye1$Attr, 0.75)
IQR_val <- Q3 - Q1
lower_bound_Attr <- Q1 - 1.5 * IQR_val
upper_bound_Attr <- Q3 + 1.5 * IQR_val

Eye2 <- Eye1[Eye1$L_skin < upper_bound_L_Skin & Eye1$L_skin > lower_bound_L_Skin,]
Eye2 <- Eye2[Eye2$L_iris < upper_bound_L_iris & Eye2$L_iris > lower_bound_L_iris,]
Eye2 <- Eye2[Eye2$L_sclera < upper_bound_L_sclera & Eye2$L_sclera > lower_bound_L_sclera,]

Eye2 <- Eye2[Eye2$a_iris < upper_bound_a_iris & Eye2$a_iris > lower_bound_a_iris,]
Eye2 <- Eye2[Eye2$a_sclera < upper_bound_a_sclera & Eye2$a_sclera > lower_bound_a_sclera,]
Eye2 <- Eye2[Eye2$a_sclera < upper_bound_a_sclera & Eye2$a_sclera > lower_bound_a_sclera,]

Eye2 <- Eye2[Eye2$b_sclera < upper_bound_b_sclera & Eye2$b_sclera > lower_bound_b_sclera,]
Eye2 <- Eye2[Eye2$b_iris < upper_bound_b_iris & Eye2$b_iris > lower_bound_b_iris,]

Eye2 <- Eye2[Eye2$LOCAL_DIST < upper_bound_dist & Eye2$LOCAL_DIST > lower_bound_dist,]
Eye2 <- Eye2[Eye2$SexTypMeas < upper_bound_SexTypMeas & Eye2$SexTypMeas > lower_bound_SexTypMeas,]
Eye2 <- Eye2[Eye2$Asym < upper_bound_Asym & Eye2$Asym > lower_bound_Asym,]

Eye2 <- Eye2[Eye2$Age < upper_bound_Age & Eye2$Age > lower_bound_Age,]
Eye2 <- Eye2[is.na(Eye2$SexT) | (Eye2$SexT < upper_bound_PercSexTyp & Eye2$SexT > lower_bound_PercSexTyp), ]
Eye2 <- Eye2[Eye2$Attr < upper_bound_Attr & Eye2$Attr > lower_bound_Attr,]

summary(as.factor(Eye2$Culture_Year))


# Standardisation: 

culture_split <- split(Eye2[2:ncol(Eye2)], Eye2$Culture_Year)

standardized_split <- lapply(culture_split, function(sub_df) {
  # Identify numeric columns
  numeric_cols <- sapply(sub_df, is.numeric)
  
  # Standardize numeric columns
  sub_df[, numeric_cols] <- scale(sub_df[, numeric_cols])
})

Eye1_2 <- do.call(rbind, standardized_split)
Eye1_2 <- as.data.frame(Eye1_2)

Eye1_2$Culture_Year <- Eye2$Culture_Year
Eye1_2$sex <- Eye2$Sex


# Data: 
data_Attr2 <- list(
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


summary.data.frame(data_Attr2)  


# Analyses: 
library(rethinking)

# ATTRACTIVENESS

m1_attr_1_5_IQR_ssep <- ulam( 
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
    
  ), data = data_Attr2, chains = 4, cores = 4, log_lik = F, iter = 2500, control = list(max_treedepth = 18, adapt_delta = 0.99)
)

M1__1_5_IQRcoefs_ssep <- precis(m1_attr_1_5_IQR_ssep, depth=3, prob=0.95)
write.csv2(M1__1_5_IQRcoefs_ssep, file="Model_1_1_5Q_ATTRACT_both_sexes_standardised_SEPARATELY_Coeftab_outliers_excluded_together.csv")
post_m1_1_5_IQR_ssep <- extract.samples(m1_attr_1_5_IQR_ssep)
saveRDS(post_m1_1_5_IQR_ssep, file="post_m1_1_5_IQR__ATTRboth_sexes_standardised_SEPARATELY_Coeftab_outliers_excluded_together_posterior.RDS")
save(m1_attr_1_5_IQR_ssep, file="Model_1_1_5Q_ATTRACT__both_sexes_standardised_SEPARATELY_Coeftab_outliers_excluded_together.Rdata")


# SEXTYPICALITY


# Upload and check the data
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

# Exclude outliers: 

# Excluding those who are more than 1.5-times the IQr away from median...
# Skin L*
Q1 <- quantile(Eye1$L_skin, 0.25)
Q3 <- quantile(Eye1$L_skin, 0.75)
IQR_val <- Q3 - Q1
lower_bound_L_Skin <- Q1 - 1.5 * IQR_val
upper_bound_L_Skin <- Q3 + 1.5 * IQR_val

# Iris L*
Q1 <- quantile(Eye1$L_iris, 0.25)
Q3 <- quantile(Eye1$L_iris, 0.75)
IQR_val <- Q3 - Q1
lower_bound_L_iris <- Q1 - 1.5 * IQR_val
upper_bound_L_iris <- Q3 + 1.5 * IQR_val

# Sclera L*
Q1 <- quantile(Eye1$L_sclera, 0.25)
Q3 <- quantile(Eye1$L_sclera, 0.75)
IQR_val <- Q3 - Q1
lower_bound_L_sclera <- Q1 - 1.5 * IQR_val
upper_bound_L_sclera <- Q3 + 1.5 * IQR_val

# Iris a*
Q1 <- quantile(Eye1$a_iris, 0.25)
Q3 <- quantile(Eye1$a_iris, 0.75)
IQR_val <- Q3 - Q1
lower_bound_a_iris <- Q1 - 1.5 * IQR_val
upper_bound_a_iris <- Q3 + 1.5 * IQR_val

# Sclera a*
Q1 <- quantile(Eye1$a_sclera, 0.25)
Q3 <- quantile(Eye1$a_sclera, 0.75)
IQR_val <- Q3 - Q1
lower_bound_a_sclera <- Q1 - 1.5 * IQR_val
upper_bound_a_sclera <- Q3 + 1.5 * IQR_val

# Iris b*
Q1 <- quantile(Eye1$b_iris, 0.25)
Q3 <- quantile(Eye1$b_iris, 0.75)
IQR_val <- Q3 - Q1
lower_bound_b_iris <- Q1 - 1.5 * IQR_val
upper_bound_b_iris <- Q3 + 1.5 * IQR_val

# Sclera b*
Q1 <- quantile(Eye1$b_sclera, 0.25)
Q3 <- quantile(Eye1$b_sclera, 0.75)
IQR_val <- Q3 - Q1
lower_bound_b_sclera <- Q1 - 1.5 * IQR_val
upper_bound_b_sclera <- Q3 + 1.5 * IQR_val

# AVRG/DIST 
Q1 <- quantile(Eye1$LOCAL_DIST, 0.25)
Q3 <- quantile(Eye1$LOCAL_DIST, 0.75)
IQR_val <- Q3 - Q1
lower_bound_dist <- Q1 - 1.5 * IQR_val
upper_bound_dist <- Q3 + 1.5 * IQR_val

# SexTypMeas
Q1 <- quantile(Eye1$SexTypMeas, 0.25)
Q3 <- quantile(Eye1$SexTypMeas, 0.75)
IQR_val <- Q3 - Q1
lower_bound_SexTypMeas <- Q1 - 1.5 * IQR_val
upper_bound_SexTypMeas <- Q3 + 1.5 * IQR_val

# FA
Q1 <- quantile(Eye1$Asym, 0.25)
Q3 <- quantile(Eye1$Asym, 0.75)
IQR_val <- Q3 - Q1
lower_bound_Asym <- Q1 - 1.5 * IQR_val
upper_bound_Asym <- Q3 + 1.5 * IQR_val

# Age
Q1 <- quantile(Eye1$Age, 0.25)
Q3 <- quantile(Eye1$Age, 0.75)
IQR_val <- Q3 - Q1
lower_bound_Age <- Q1 - 1.5 * IQR_val
upper_bound_Age <- Q3 + 1.5 * IQR_val

# Perc SexTyp
Q1 <- quantile(Eye1$SexT, 0.25, na.rm = T)
Q3 <- quantile(Eye1$SexT, 0.75, na.rm = T)
IQR_val <- Q3 - Q1
lower_bound_PercSexTyp <- Q1 - 1.5 * IQR_val
upper_bound_PercSexTyp <- Q3 + 1.5 * IQR_val

# SexT
Q1 <- quantile(Eye1$SexT, 0.25)
Q3 <- quantile(Eye1$SexT, 0.75)
IQR_val <- Q3 - Q1
lower_bound_SexT <- Q1 - 1.5 * IQR_val
upper_bound_SexT <- Q3 + 1.5 * IQR_val

Eye2 <- Eye1[Eye1$L_skin < upper_bound_L_Skin & Eye1$L_skin > lower_bound_L_Skin,]
Eye2 <- Eye2[Eye2$L_iris < upper_bound_L_iris & Eye2$L_iris > lower_bound_L_iris,]
Eye2 <- Eye2[Eye2$L_sclera < upper_bound_L_sclera & Eye2$L_sclera > lower_bound_L_sclera,]

Eye2 <- Eye2[Eye2$a_iris < upper_bound_a_iris & Eye2$a_iris > lower_bound_a_iris,]
Eye2 <- Eye2[Eye2$a_sclera < upper_bound_a_sclera & Eye2$a_sclera > lower_bound_a_sclera,]
Eye2 <- Eye2[Eye2$a_sclera < upper_bound_a_sclera & Eye2$a_sclera > lower_bound_a_sclera,]

Eye2 <- Eye2[Eye2$b_sclera < upper_bound_b_sclera & Eye2$b_sclera > lower_bound_b_sclera,]
Eye2 <- Eye2[Eye2$b_iris < upper_bound_b_iris & Eye2$b_iris > lower_bound_b_iris,]

Eye2 <- Eye2[Eye2$LOCAL_DIST < upper_bound_dist & Eye2$LOCAL_DIST > lower_bound_dist,]
Eye2 <- Eye2[Eye2$SexTypMeas < upper_bound_SexTypMeas & Eye2$SexTypMeas > lower_bound_SexTypMeas,]
Eye2 <- Eye2[Eye2$Asym < upper_bound_Asym & Eye2$Asym > lower_bound_Asym,]

Eye2 <- Eye2[Eye2$Age < upper_bound_Age & Eye2$Age > lower_bound_Age,]
Eye2 <- Eye2[is.na(Eye2$SexT) | (Eye2$SexT < upper_bound_PercSexTyp & Eye2$SexT > lower_bound_PercSexTyp), ]
Eye2 <- Eye2[Eye2$SexT < upper_bound_SexT & Eye2$SexT > lower_bound_SexT,]

summary(as.factor(Eye2$Culture_Year))


# Standardisation: 

culture_split <- split(Eye2[2:ncol(Eye2)], Eye2$Culture_Year)

standardized_split <- lapply(culture_split, function(sub_df) {
  # Identify numeric columns
  numeric_cols <- sapply(sub_df, is.numeric)
  
  # Standardize numeric columns
  sub_df[, numeric_cols] <- scale(sub_df[, numeric_cols])
})

Eye1_2 <- do.call(rbind, standardized_split)
Eye1_2 <- as.data.frame(Eye1_2)

Eye1_2$Culture_Year <- Eye2$Culture_Year
Eye1_2$sex <- Eye2$Sex


# Data: 
data_sext <- list(
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


summary.data.frame(data_sext)  


m1_sext_1_5_IQR_ssep <- ulam(
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
    
  ), data = data_sext, chains = 4, cores = 4, log_lik = T, iter = 5000, control = list(max_treedepth = 18, adapt_delta = 0.99)
)


M1__1_5_IQRcoefs_ssep <- precis(m1_sext_1_5_IQR_ssep, depth=3, prob=0.95)
write.csv2(M1__1_5_IQRcoefs_ssep, file="Model_1_1_5Q_SEXTYP_both_sexes_standardised_SEPARATELY_Coeftab_outliers_excluded_together.csv")
post_m1_1_5_IQR_ssep <- extract.samples(m1_sext_1_5_IQR_ssep)
saveRDS(post_m1_1_5_IQR_ssep, file="post_m1_1_5_IQR__SEXTboth_sexes_standardised_SEPARATELY_Coeftab_outliers_excluded_together_posterior.RDS")
save(m1_sext_1_5_IQR_ssep, file="Model_1_1_5Q_SEXTYP__both_sexes_standardised_SEPARATELY_Coeftab_outliers_excluded_together.Rdata")


#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-

#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-

#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-


# PART TWO: 
# OUTLIERS EXCLUDED FROM THE INDIVIDUAL DATASETS

# # Upload and check the data
Eye1 <- read.csv2("Data_03_08_24.csv",T)

tapply(Eye1$Attr,Eye1$Culture_Year, mean)

summary(Eye1)
Eye1 <- Eye1[!is.na(Eye1$Age),]

summary(as.factor(Eye1$Sex))
summary((Eye1))


# Note - this variant is without SexTypicality, since there is a vector of NAs for culture = Vietnam
# The same process will be then repeated for SexTypicality, when Vietnam is already dropped.

# Copy the original data
Eye1_clean <- Eye1

# Get list of cultures and numeric variables
cultures <- unique(Eye1_clean$Culture_Year)
vars <- colnames(Eye1_clean)[c(4:12,16:18)]

# Initialise a logical vector: TRUE = keep, FALSE = drop
keep_row <- rep(TRUE, nrow(Eye1_clean))

# Loop through each culture and variable
for (cult in cultures) {
  for (var in vars) {
    # Get indices for rows in this culture
    idx <- which(Eye1_clean$Culture_Year == cult)
    
    # Extract values for this variable and culture
    vals <- Eye1_clean[idx, var]
    q1 <- quantile(vals, 0.25)
    q3 <- quantile(vals, 0.75)
    iqr <- q3 - q1
    lower <- q1 - 1.5 * iqr
    upper <- q3 + 1.5 * iqr
    
    # Mark rows as FALSE if any value is an outlier
    outlier_idx <- idx[vals < lower | vals > upper]
    keep_row[outlier_idx] <- FALSE
  }
}

# Subset the data: only rows where all variables are within bounds
Eye1_filtered <- Eye1_clean[keep_row, ]

summary(as.factor(Eye1_filtered$Culture_Year))
summary.data.frame(Eye1_filtered)

# Standardisation: 
Eye2 <- Eye1_filtered

culture_split <- split(Eye2[2:ncol(Eye2)], Eye2$Culture_Year)

standardized_split <- lapply(culture_split, function(sub_df) {
  # Identify numeric columns
  numeric_cols <- sapply(sub_df, is.numeric)
  
  # Standardize numeric columns
  sub_df[, numeric_cols] <- scale(sub_df[, numeric_cols])
})

Eye1_2 <- do.call(rbind, standardized_split)
Eye1_2 <- as.data.frame(Eye1_2)

Eye1_2$Culture_Year <- Eye2$Culture_Year
Eye1_2$sex <- Eye2$Sex


# Data: 
data_Attr2 <- list(
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


summary.data.frame(data_Attr2)  


# Analyses: 
library(rethinking)

# ATTRACTIVENESS

m1_attr_1_5_IQR_ssep <- ulam( 
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
    
  ), data = data_Attr2, chains = 4, cores = 4, log_lik = F, iter = 2500, control = list(max_treedepth = 18, adapt_delta = 0.99)
)

M1__1_5_IQRcoefs_ssep <- precis(m1_attr_1_5_IQR_ssep, depth=3, prob=0.95)
write.csv2(M1__1_5_IQRcoefs_ssep, file="Model_1_1_5Q_ATTRACT_both_sexes_standardised_SEPARATELY_Coeftab_outliers_excluded_SEPARATELY.csv")
post_m1_1_5_IQR_ssep <- extract.samples(m1_attr_1_5_IQR_ssep)
saveRDS(post_m1_1_5_IQR_ssep, file="post_m1_1_5_IQR__ATTRboth_sexes_standardised_SEPARATELY_Coeftab_outliers_excluded_SEPARATELY_posterior.RDS")
save(m1_attr_1_5_IQR_ssep, file="Model_1_1_5Q_ATTRACT__both_sexes_standardised_SEPARATELY_Coeftab_outliers_excluded_SEPARATELY.Rdata")




# SEXTYPICALITY

# PART TWO: 
# OUTLIERS EXCLUDED FROM THE INDIVIDUAL DATASETS

# # Upload and check the data
Eye1 <- read.csv2("Data_03_08_24.csv",T)

tapply(Eye1$Attr,Eye1$Culture_Year, mean)

summary(Eye1)
Eye1 <- Eye1[!is.na(Eye1$Age),]
Eye1 <- Eye1[!is.na(Eye1$SexT),]

summary(as.factor(Eye1$Sex))
summary((Eye1))


# Note - this variant is without SexTypicality, since there is a vector of NAs for culture = Vietnam
# The same process will be then repeated for SexTypicality, when Vietnam is already dropped.

# Copy the original data
Eye1_clean <- Eye1

# Get list of cultures and numeric variables
cultures <- unique(Eye1_clean$Culture_Year)
vars <- colnames(Eye1_clean)[c(4:12,16:18)]

# Initialise a logical vector: TRUE = keep, FALSE = drop
keep_row <- rep(TRUE, nrow(Eye1_clean))

# Loop through each culture and variable
for (cult in cultures) {
  for (var in vars) {
    # Get indices for rows in this culture
    idx <- which(Eye1_clean$Culture_Year == cult)
    
    # Extract values for this variable and culture
    vals <- Eye1_clean[idx, var]
    q1 <- quantile(vals, 0.25)
    q3 <- quantile(vals, 0.75)
    iqr <- q3 - q1
    lower <- q1 - 1.5 * iqr
    upper <- q3 + 1.5 * iqr
    
    # Mark rows as FALSE if any value is an outlier
    outlier_idx <- idx[vals < lower | vals > upper]
    keep_row[outlier_idx] <- FALSE
  }
}

# Subset the data: only rows where all variables are within bounds
Eye1_filtered <- Eye1_clean[keep_row, ]

summary(as.factor(Eye1_filtered$Culture_Year))
summary.data.frame(Eye1_filtered)

# Standardisation: 
Eye2 <- Eye1_filtered

culture_split <- split(Eye2[2:ncol(Eye2)], Eye2$Culture_Year)

standardized_split <- lapply(culture_split, function(sub_df) {
  # Identify numeric columns
  numeric_cols <- sapply(sub_df, is.numeric)
  
  # Standardize numeric columns
  sub_df[, numeric_cols] <- scale(sub_df[, numeric_cols])
})

Eye1_2 <- do.call(rbind, standardized_split)
Eye1_2 <- as.data.frame(Eye1_2)

Eye1_2$Culture_Year <- Eye2$Culture_Year
Eye1_2$sex <- Eye2$Sex


# Data: 
data_SexT <- list(
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


m1_sext_1_5_IQR_ssep <- ulam(
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
    
  ), data = data_SexT, chains = 4, cores = 4, log_lik = T, iter = 2500, control = list(max_treedepth = 18, adapt_delta = 0.99)
)


M1__1_5_IQRcoefs_ssep <- precis(m1_sext_1_5_IQR_ssep, depth=3, prob=0.95)
write.csv2(M1__1_5_IQRcoefs_ssep, file="Model_1_1_5Q_SEXTYP_both_sexes_standardised_SEPARATELY_Coeftab_outliers_excluded_SEPARATELY.csv")
post_m1_1_5_IQR_ssep <- extract.samples(m1_sext_1_5_IQR_ssep)
saveRDS(post_m1_1_5_IQR_ssep, file="post_m1_1_5_IQR__SEXTboth_sexes_standardised_SEPARATELY_Coeftab_outliers_excluded_SEPARATELY_posterior.RDS")
save(m1_sext_1_5_IQR_ssep, file="Model_1_1_5Q_SEXTYP__both_sexes_standardised_SEPARATELY_Coeftab_outliers_excluded_SEPARATELY.Rdata")
