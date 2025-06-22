library(rethinking)

source("02_ATTR_data_by_rating_prepare.R")

names(dl)

summary(as.factor(dl$rating))

nrow(dl)
dl<-dl[!is.na(match(dl$rating,1:7)),]
nrow(dl) # Turks now went away...

summary(as.factor(dl$rating)) # People do not like themselves! 


#Some extra rubbish summary to add to sample size table in supplement, go to line 39 where the interesting analysis starts
summary(as.factor(dl$Culture_Year))
sum(summary(as.factor(dl$Culture_Year)))
mean(summary(as.factor(dl$Culture_Year)))
sd(summary(as.factor(dl$Culture_Year)))

# Let leave this part for future...
# unique(dl$nat)
# sapply(unique(dl$nat), function(x){length(unique(paste(dl$nat,dl$sex,dl$rater)[dl$nat==x]))})
# sapply(unique(dl$nat), function(x){length(unique(paste(dl$nat,dl$sex,dl$rater)[dl$nat==x & dl$sex=="F"]))})
# sapply(unique(dl$nat), function(x){length(unique(paste(dl$nat,dl$sex,dl$rater)[dl$nat==x & dl$sex=="M"]))})

# sum(sapply(unique(dl$nat), function(x){length(unique(paste(dl$nat,dl$sex,dl$rater)[dl$nat==x]))}))
# mean(sapply(unique(dl$nat), function(x){length(unique(paste(dl$nat,dl$sex,dl$rater)[dl$nat==x]))}))
# sd(sapply(unique(dl$nat), function(x){length(unique(paste(dl$nat,dl$sex,dl$rater)[dl$nat==x]))}))

# sum(sapply(unique(dl$nat), function(x){length(unique(paste(dl$nat,dl$sex,dl$rater)[dl$nat==x& dl$sex=="F"]))}))
# mean(sapply(unique(dl$nat), function(x){length(unique(paste(dl$nat,dl$sex,dl$rater)[dl$nat==x& dl$sex=="F"]))}))
# sd(sapply(unique(dl$nat), function(x){length(unique(paste(dl$nat,dl$sex,dl$rater)[dl$nat==x& dl$sex=="F"]))}))

# sum(sapply(unique(dl$nat), function(x){length(unique(paste(dl$nat,dl$sex,dl$rater)[dl$nat==x& dl$sex=="M"]))}))
# mean(sapply(unique(dl$nat), function(x){length(unique(paste(dl$nat,dl$sex,dl$rater)[dl$nat==x& dl$sex=="M"]))}))
# sd(sapply(unique(dl$nat), function(x){length(unique(paste(dl$nat,dl$sex,dl$rater)[dl$nat==x& dl$sex=="M"]))}))

# sapply(unique(dl$nat), function(x){length(unique(dl$rater[dl$nat==x & dl$sex=="F"]))})

d.list<-list(rating=as.integer(as.numeric(dl$rating)),
                 tar=as.integer(as.factor(paste(dl$Culture_Year,dl$target))),
                 rat=as.integer(as.factor(paste(dl$Culture_Year,dl$rater))),
                 MeasSext=dl$SexTypMeas,
                 Asym=dl$Asymmetry,
                 DIST=dl$Distinctiveness,
                 Age = scale(dl$Age), 
                 Sex=as.integer(as.factor(dl$sex)),
                 sex_c = ifelse(dl$sex=="F",-0.5,0.5),
                 Grp=as.integer(as.factor(dl$Culture_Year)),
                 # KOLORY
                 L_skin=dl$L_skin,
                 L_sclera=dl$L_sclera,
                 a_sclera=dl$a_sclera,
                 b_sclera=dl$b_sclera,
                 L_iris=dl$L_iris,
                 a_iris=dl$a_iris,
                 b_iris=dl$b_iris
                 )
    
summary.data.frame(d.list) # N = 892,

# Manually-edited model Without varying effects per raters...

m1_attr <- ulam( 
  alist(
    rating ~ ordered_logistic(phi, alpha),
    
    phi <- mu1 + mu2 + mu3, # mu3 bude level hodnotitele a ksichta...
    
    mu1 <- B_sex*sex_c + 
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
    
    mu3 <- z_t[tar]*sigma_t,
    
    # Non-centered parameterization for betas
    transpars> matrix[Grp, 24]:betas <- compose_noncentered(sigma_group, L_Rho_group, z_group),
    
    matrix[24, Grp]:z_group ~ normal(0, 1),
    
    # Hyperpriors for the group-level effects
    vector[24]:sigma_group ~ dexp(1),
    cholesky_factor_corr[24]:L_Rho_group ~ lkj_corr_cholesky(2),
    
    B_sex  ~ normal(0, 0.5),
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
    
    # Level of individual raters: 
    z_t[tar] ~ normal(0,1),
    sigma_t ~ exponential(1),
    
    gq> matrix[24, 24]:Rho_group <<- Chol_to_Corr(L_Rho_group),
    
    alpha ~ normal(0,1.5)
    
  ), data=d.list , chains=4 , cores=4, log_lik=TRUE ,iter = 750, warmup=250, sample=T)


post<-extract.samples(m1_attr)
WAIC1 <- WAIC(m1_attr)

save(m1_attr,file="(m1_attr_11_06.Rdata")
saveRDS(post, file="post_m1_attr_Long_11_06.RDS")
write.csv(WAIC1, file="WAIC1_attr_11_06.csv")


# x-----------------------------------------------------------------------------

# Now adding the "level of raters" - varying intercept only...

m2_attr <- ulam( 
  alist(
    rating ~ ordered_logistic(phi, alpha),
    
    phi <- mu1 + mu2 + mu3, # mu3 bude level hodnotitele a ksichta...
    
    mu1 <- B_sex*sex_c + 
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
    
    mu3 <- z_pr[rat]*sigma_r + z_t[tar]*sigma_t,
    
    # Non-centered parameterization for betas
    transpars> matrix[Grp, 24]:betas <- compose_noncentered(sigma_group, L_Rho_group, z_group),
    
    matrix[24, Grp]:z_group ~ normal(0, 1),
    
    # Hyperpriors for the group-level effects
    vector[24]:sigma_group ~ dexp(1),
    cholesky_factor_corr[24]:L_Rho_group ~ lkj_corr_cholesky(2),
    
    B_sex  ~ normal(0, 0.5),
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
    
    # Level of individual faces: 
    z_t[tar] ~ normal(0,1),
    sigma_t ~ dexp(1),
    
    # level of individual raters (intercept only):
    z_pr[rat] ~ normal(0,1),
    sigma_r ~ dexp(1),
    
    gq> matrix[24, 24]:Rho_group <<- Chol_to_Corr(L_Rho_group),
    
    alpha ~ normal(0,1.5)
    
  ), data=d.list , chains=4 , cores=4, log_lik=TRUE ,iter = 750, warmup=250, sample=T)

post<-extract.samples(m2_attr)
WAIC2 <- WAIC(m2_attr)

save(m2_attr,file="(m2_attr_11_06.Rdata")
saveRDS(post, file="post_m2_attr_Long_11_06.RDS")
write.csv(WAIC2, file="WAIC2_attr_11_06.csv")


# x-----------------------------------------------------------------------------

# Now adding full-fledged "effects of raters", "simpler way" 

m3_attr <- ulam( 
  alist(
    rating ~ ordered_logistic(phi, alpha),
    
    phi <- mu1 + mu2 + mu3 + mu4, # mu3-4 bude level hodnotitele a ksichta...
    
    mu1 <- B_sex*sex_c + 
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
    
    mu3 <- pr[rat,1] + pr[rat,2]*MeasSext + pr[rat,3]*Asym + pr[rat,4]*DIST + 
      pr[rat,5]*Age + pr[rat,6]*L_skin + pr[rat,7]*L_iris + 
      pr[rat,8]*a_iris + pr[rat,9]*b_iris + pr[rat,10]*L_sclera +
      pr[rat,11]*a_sclera + pr[rat,12]*b_sclera,
      
    mu4 <- z_t[tar]*sigma_t,
    
    # Non-centered parameterization for betas
    transpars> matrix[Grp, 24]:betas <- compose_noncentered(sigma_group, L_Rho_group, z_group),
    
    matrix[24, Grp]:z_group ~ normal(0, 1),
    
    # Hyperpriors for the group-level effects
    vector[24]:sigma_group ~ dexp(1),
    cholesky_factor_corr[24]:L_Rho_group ~ lkj_corr_cholesky(2),
    
    B_sex  ~ normal(0, 0.5),
    vector[2]:BSexTyp ~ normal(0, 2.5),
    vector[2]:BDIST ~ normal(0, 2.5),
    vector[2]:BAsym ~ normal(0,2.5),
    vector[2]:BAge ~ normal(0, 2.5),
    vector[2]:B_L_Skin ~ normal(0, 2.5),
    vector[2]:B_L_iris ~ normal(0, 2.5),
    vector[2]:B_a_iris ~ normal(0, 2.5),
    vector[2]:B_b_iris ~ normal(0, 2.5),
    vector[2]:B_L_sclera ~ normal(0, 2.5),
    vector[2]:B_a_sclera ~ normal(0, 2.5),
    vector[2]:B_b_sclera ~ normal(0, 2.5),
    
    # Level of individual faces: 
    z_t[tar] ~ normal(0,1),
    sigma_t ~ dexp(1),
    
    # level of individual raters (full-scope):
    vector[12]:pr[rat] ~ multi_normal(c(0,0,0,0,0,0,0,0,0,0,0,0),Rho_r,sigma_r), # sorry, rep(0,12) does not work! 
    sigma_r ~ dexp(1),
    Rho_r ~ lkj_corr(2),
    
    gq> matrix[24, 24]:Rho_group <<- Chol_to_Corr(L_Rho_group),
    
    alpha ~ normal(0,1.5)
    
  ), data=d.list , chains=4 , cores=4, log_lik=TRUE ,iter = 750, warmup=250, sample=T)

post<-extract.samples(m3_attr)
WAIC3 <- WAIC(m3_attr)

save(m3_attr,file="(m3_attr_11_06.Rdata")
saveRDS(post, file="post_m3_attr_Long_11_06.RDS")
write.csv(WAIC3, file="WAIC3_attr_11_06.csv")


# x----------------------------------------------------------------------------

# Now adding full-fledged "effects of raters", "decentred way" 
m4_attr <- ulam( 
  alist(
    rating ~ ordered_logistic(phi, alpha),
    
    phi <- mu1 + mu2 + mu3 + mu4, # mu3-4 bude level hodnotitele a ksichta...
    
    mu1 <- B_sex*sex_c + 
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
    
    mu3 <- pr[rat,1] + pr[rat,2]*MeasSext + pr[rat,3]*Asym + pr[rat,4]*DIST + 
      pr[rat,5]*Age + pr[rat,6]*L_skin + pr[rat,7]*L_iris + 
      pr[rat,8]*a_iris + pr[rat,9]*b_iris + pr[rat,10]*L_sclera +
      pr[rat,11]*a_sclera + pr[rat,12]*b_sclera,
    
    mu4 <- z_t[tar]*sigma_t,
    
    # Non-centered parameterization for betas
    transpars> matrix[Grp, 24]:betas <- compose_noncentered(sigma_group, L_Rho_group, z_group),
    
    matrix[24, Grp]:z_group ~ normal(0, 1),
    
    # Hyperpriors for the group-level effects
    vector[24]:sigma_group ~ dexp(1),
    cholesky_factor_corr[24]:L_Rho_group ~ lkj_corr_cholesky(2),
    
    B_sex  ~ normal(0, 0.5),
    vector[2]:BSexTyp ~ normal(0, 2.5),
    vector[2]:BDIST ~ normal(0, 2.5),
    vector[2]:BAsym ~ normal(0,2.5),
    vector[2]:BAge ~ normal(0, 2.5),
    vector[2]:B_L_Skin ~ normal(0, 2.5),
    vector[2]:B_L_iris ~ normal(0, 2.5),
    vector[2]:B_a_iris ~ normal(0, 2.5),
    vector[2]:B_b_iris ~ normal(0, 2.5),
    vector[2]:B_L_sclera ~ normal(0, 2.5),
    vector[2]:B_a_sclera ~ normal(0, 2.5),
    vector[2]:B_b_sclera ~ normal(0, 2.5),
    
    # Level of individual faces: 
    z_t[tar] ~ normal(0,1),
    sigma_t ~ dexp(1),
    
    # level of individual raters (full-scope):
    transpars> matrix[rat,12]:pr <- compose_noncentered(sigma_r, L_Rho_r, z_r), # sorry, rep(0,12) does not work! 
    
    matrix[12,rat]:z_r ~ normal(0,1),
    
    vector[12]:sigma_r ~ dexp(1),
    cholesky_factor_corr[12]:L_Rho_r ~ lkj_corr_cholesky(2),
    
    gq> matrix[12,12]:Rho_r <<- Chol_to_Corr(L_Rho_r),
    
    gq> matrix[24, 24]:Rho_group <<- Chol_to_Corr(L_Rho_group),
    
    alpha ~ normal(0,1.5)
    
  ), data=d.list , chains=4 , cores=4, log_lik=TRUE ,iter = 750, warmup=250, sample=T)

post<-extract.samples(m4_attr)
WAIC4 <- WAIC(m4_attr)

save(m4_attr,file="(m4_attr_11_06.Rdata")
saveRDS(post, file="post_m4_attr_Long_11_06.RDS")
write.csv(WAIC4, file="WAIC4_attr_11_06.csv")


# x-----------------------------------------------------------------------------

WAIC <- compare(m1_attr,m2_attr,m3_attr,m4_attr)
write.csv(WAIC, file="WAIC_Attr_11_06_25.csv")

plot(compare(m1_attr, m2_attr, m3_attr, m4_attr))


