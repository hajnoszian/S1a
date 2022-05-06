library(dplyr)
library(tidyverse)

####Loading Data####
library(readr)
df_Gen <- read_csv("Ian+Study+1a_General_May+26,+2021_09.36.csv")
df_SONA <- read_csv("Ian+Study+1a_SONA_May+26,+2021_09.35.csv")
df_VP<- read_csv("Ian+Study+1a_VP_May+26,+2021_09.35.csv")

# ====Merging Datasets ====
df_SONAVP <- full_join(df_SONA, df_VP)
df <- full_join(df_SONAVP, df_Gen)


# ----Removing unnecessary columns, Converting to appropriate data type----
df <- select(df,
       -(StartDate:UserLanguage))
df <- select(df,
             -c(BFI_DO, EC_DO, Attach_DO, RNSF1_DO, KM_P_DO, KM_CSR_DO, KM_M_DO, KM_L_DO, RNSF2_DO, Q12.1_DO, SC0, FL_3_DO, id, ID))

df <- df[-c(1,2), ] #remove the first two rows (meta-data)
df[, c(1:76, 78:79, 81:82, 84:88)] <-  sapply(df[, c(1:76, 78:79, 81:82, 84:88)], as.numeric)

##Renaming some columns
df <- rename(df, 
       Consent = Q1.2,
       Vid_SelfRep = Q12.1,
       Vid_Audio = Q12.4,
       Vid_Issue = Q12.2,
       Vid_Issue_TEXT = Q12.2_2_TEXT,
       Vid_PrevExp = Q12.3,
       Cond = FL_5_DO)

df <- mutate(df, Cond = 
                    recode(Cond,
                           'VideoCondition1"ThaiMedicine"Zickfeldetal.2019' = "1",
                           'VideoNeutral1"CleanHardwoodFloors"(Riveraetal.,2019)' = "0")
            )
df$Vid_PrevExp <- factor(df$Vid_PrevExp)
df$Cond <- as.numeric(df$Cond)
df$Cond_01 <-df$Cond #making a numeric based one, just in case Rjags doesn't use the factor contrasts that usual R regressions use (i.e. when a factor, R will turn levels 1,2 into 0,1 for calculations. Idk if Jags will do that)
##Create participant ID
df$id <- 1:nrow(df)

###Pre-Processing###

##Recoding amiss values
#Race
df$Race[df$Race_10_TEXT == "European Celt (not Anglo)"] <- 0
df$Race[df$Race_10_TEXT == "White"] <- 0

##Tidying Factor Variables (E.g.Demographics)
factor(df$Gender)#checking for levels used
df$Gender <- factor(df$Gender, labels = c(
  "Female", "Male", "Nonbinary, Genderqueer",
  "Bigender, Genderfluid", "Agender, Genderless"
))

df$Race#checking for levels used
df$Race <- factor(df$Race, labels = c(
  "White, Caucasian, Anglo", "Black, African, Caribbean", "Hispanic, Latino/a, Chicano/a",
  "East Asian", "South Asian", "Southeast Asian", "Middle Easter, Arab", "Mixed or Multiple Ethnic Groups",
  "Other"
))

factor(df$Education)#checking for levels used
df$Education <- factor(df$Education, labels = c(
  "GCSE, O-Levels, or Standard Grades",
  "A-Levels or Highers/Advanced Highers",
  "Vocational degree (e.g., SVQ, HNC, HND",
  "Undergraduate degree (e.g., BSc, BA)",
  "Master's degree (e.g., MSc, MPhil)",
  "PhD, PsyD",
  "Other advanced or professional degree (e.g., MD, JD)"
))

df$Student <- factor(df$Student, labels = c(
  "No", "Yes"
))

df$Employed <- factor(df$Employed, labels = c(
  "No",
  "Yes, part-time",
  "Yes, full-time"
))

factor(df$Income)#checking levels used
df$Income <- factor(df$Income, labels = c(
  "£0-£12,500",
  "£12,501-£14,549",
  "£14,550-£24,944",
  "£24,945-£43,430",
  "£43,431-£150,000",
  "£150,001+"
))

factor(df$SexualOr)
df$SexualOr <- factor(df$SexualOr, labels = c(
  "Straight", "Gay", "Lesbian",
  "Queer", "Bisexual, Pansexual", "Asexual",
  "Other"
))

##reverse-scored items and creating collated Scores

##BFI Personality
#reverse coding equation: (total available responses +1) - score
#Extraversion: items 1R, 6, 11
df$BFI_1R <- (7+1) - df$BFI_1
df$BFI_E <- rowMeans(df[,c('BFI_1R', 'BFI_6', 'BFI_11')], na.rm = T)

#Agreeableness 2,7R,12
df$BFI_7R <- (7+1) - df$BFI_7
df$BFI_A <- rowMeans(df[,c('BFI_2', 'BFI_7R', 'BFI_12')], na.rm = T)

#Conscientiousness: 3R, 8R, 13
df$BFI_3R <- (7+1) - df$BFI_3
df$BFI_8R <- (7+1) - df$BFI_8
df$BFI_C <- rowMeans(df[,c('BFI_3R', 'BFI_8R', 'BFI_13')], na.rm = T)

#Neuroticism: 4,9,14R
df$BFI_14R <- (7+1) - df$BFI_14
df$BFI_N <- rowMeans(df[,c('BFI_4', 'BFI_9', 'BFI_14R')], na.rm = T)

#Openmindedness: 5, 10R, 15
df$BFI_10R <- (7+1) - df$BFI_10
df$BFI_O <- rowMeans(df[,c('BFI_5', 'BFI_10R', 'BFI_15')], na.rm = T)


##EC Empathic Concern IRI
#should be on 0-4 scale, not 1-5
df[, c("EC_1","EC_2", "EC_3", "EC_4","EC_5","EC_6", "EC_7")] <- df[, c("EC_1","EC_2", "EC_3", "EC_4","EC_5","EC_6", "EC_7")]-1
#reverse coding equation, incl a zero point: (total available responses-1) - score
#items: 1, 2R, 3, 4R, 5R, 6, 7
df$EC_2R <- 4 - df$EC_2
df$EC_4R <- 4 - df$EC_4
df$EC_5R <- 4 - df$EC_5
df$EC <- rowMeans(df[, c("EC_1", "EC_2R", "EC_3", "EC_4R", "EC_5R", "EC_6", "EC_7")], na.rm = T)

##Attach General/Global Attachment
#reverse coding equation: (total available responses +1) - score
#Attachment Avoidance items: 1R, 2R, 3R, 4R, 5 , 6
df$Attach_1R <- 8 - df$Attach_1
df$Attach_2R <- 8 - df$Attach_2
df$Attach_3R <- 8 - df$Attach_3
df$Attach_4R <- 8 - df$Attach_4
df$Attach_avd <- rowMeans(df[, c("Attach_1R", "Attach_2R", "Attach_3R", "Attach_4R", "Attach_5", "Attach_6")], na.rm = T)
#Attachment Anxiety items: 7,8,9
df$Attach_anx <- rowMeans(df[, c("Attach_7", "Attach_8", "Attach_9")], na.rm = T)

##RNSF1 Relatedness Needs Satisfaction/Frustration Time 1
#RNS items: 1, 2, 3, 4
df$RNS1 <- rowMeans(df[, c("RNSF1_1","RNSF1_2","RNSF1_3","RNSF1_4")])
#RNF items: 5,6,7,8
df$RNF1 <- rowMeans(df[, c("RNSF1_5","RNSF1_6","RNSF1_7","RNSF1_8")])

##KM_ Kama Muta (Physical reaction, Communal Sharing Rel, Motivation, Positive Affect, Label)
#all 5 aspects should be on 0-6 scale, not 1-7

df[, c("KM_P_1","KM_P_2", "KM_P_3", "KM_P_4" ,"KM_P_5", "KM_P_6", "KM_P_7", "KM_P_8", "KM_P_9", "KM_P_10", "KM_P_11","KM_P_12")] <- 
  df[, c("KM_P_1","KM_P_2", "KM_P_3", "KM_P_4" ,"KM_P_5", "KM_P_6", "KM_P_7", "KM_P_8", "KM_P_9", "KM_P_10", "KM_P_11","KM_P_12")]-1

df[, c("KM_CSR_1","KM_CSR_2","KM_CSR_3", "KM_CSR_4", "KM_CSR_5")] <- 
  df[, c("KM_CSR_1","KM_CSR_2","KM_CSR_3", "KM_CSR_4", "KM_CSR_5")]-1

df[, c("KM_M_1","KM_M_2", "KM_M_3", "KM_M_4")] <- 
  df[, c("KM_M_1","KM_M_2", "KM_M_3", "KM_M_4")]-1

df[, ("KM_PA_1")] <- 
  df[, ("KM_PA_1")]-1

df[, c("KM_L_1", "KM_L_2", "KM_L_3")] <- 
  df[, c("KM_L_1", "KM_L_2", "KM_L_3")]-1

df$KM_P <- rowMeans(df[, c("KM_P_1","KM_P_2", "KM_P_3", "KM_P_4" ,"KM_P_5", "KM_P_6", "KM_P_7", "KM_P_8", "KM_P_9", "KM_P_10", "KM_P_11","KM_P_12")], na.rm = T)
df$KM_CSR <- rowMeans(df[, c("KM_CSR_1","KM_CSR_2","KM_CSR_3", "KM_CSR_4")], na.rm = T)  #note, KM_CSR_5 not included here b/c it was a attention check item, not part of the scale
df$KM_M <- rowMeans(df[, c("KM_M_1","KM_M_2", "KM_M_3", "KM_M_4")], na.rm = T)
df$KM_PA <- df$KM_PA_1
df$KM_L <- rowMeans(df[, c("KM_L_1", "KM_L_2", "KM_L_3")], na.rm = T)

#Attention check was placed on KM_CSR_5
df$AttCheck <- df$KM_CSR_5 #correct answer "Not at all" is coded as 0
df <- select(df, -KM_CSR_5)


##RNSF2 Relatedness Needs Satisfaction/Frustration Time 2
#RNS items: 1, 2, 3, 4
df$RNS2 <- rowMeans(df[, c("RNSF2_1","RNSF2_2","RNSF2_3","RNSF2_4")])
#RNF items: 5,6,7,8
df$RNF2 <- rowMeans(df[, c("RNSF2_5","RNSF2_6","RNSF2_7","RNSF2_8")])


##Checking attention check, correct answer "Not at all" is coded as 0
sum(is.na(df$AttCheck)) #17 NAs, i.e. people who didn't make it far enough to complete the attention check
table((df$AttCheck[df$AttCheck != 0]), exclude = NULL) #17 failed attention checks, this number only includes those who go this far
#expect to remove at least 17+17 = 34 participants from the dataset
df$AttCheck <- ifelse(df$AttCheck == 0,
                       "Pass",
                       "Fail")

##Looking at video issues
unique(df$Vid_Issue_TEXT)
length(unique(df$Vid_Issue_TEXT))-1 #12 total issues;NA is counted as "unique" so removing by 1 that to get true count
nrow(df) - sum(is.na(df$Vid_Issue_TEXT))#12, alternative calculation of NAs and non-NAs in the video issue text var
df$Vid_Issue <- factor(df$Vid_Issue)

##Segmenting Out for Final df, by priority of importance
#consent, must be confirmed
df <- subset(df, Consent == 1)

#attention check, must be successful. 
df <- subset(df, AttCheck == "Pass")

nrow(df)#after those "hard" cuts, we're down to 154 (1 from NA consent, 33 from fail/NA attention checks)

#video condition/self-report confirmation
#3 == KM video, 7 == floor clean video: Conditions 1 KM, 0 Neutral
df$Vid_Check <- paste(df$Vid_SelfRep, df$Cond) #only want "3 1" or " 7 0", all other combinations are incorrect
table(df$Vid_Check, exclude = NULL) #looks like 2 failed checks, 6 NA checks (people didn't fulfill one or both, and 146 completes)
df$Vid_Check <- ifelse(df$Vid_Check == "3 1" | df$Vid_Check == "7 0",
       "Pass",
       "Fail")
table(df$Vid_Check, exclude = NULL) #Confirmed, 8 fails 146 completes
df$Cond <- factor(df$Cond, levels = c(0, 1), labels = c("Control", "KM")) 

#Vid condition check,successful
df <- subset(df, Vid_Check == "Pass")

nrow(df)# current 146, confirmed 8 cut

##Pre-Analysis variable creation
##I need to make averages of each part first
df$KM_Avg <- (df$KM_P + df$KM_CSR + df$KM_M + df$KM_PA + df$KM_L)/5
#b/c sigmoid(0) = 0.5, you need the input values to be both above and below 0 to access the full 0.0-1.0 function range.
#a simple zscore would make the mean of the data the zero point, which is not what we want. We want the middlest point of the scale to reflec the zero point.
df$KM_Scl <- (df$KM_Avg - 3)/1 #scaling such that the "mean" is == 0. I.e. if a participant had an avg resp of 3 (the middle of the 0-6 range) across all 5 KM sections, they would get a 0, therefore a 50% probability KM
df$pKM <- 1/(1+(exp(1)^-(df$KM_Scl)))
 

##Initial Data Checks
#standardize data

df$pKM_Z <- scale(df$pKM)
df$RNF1_Z <- scale(df$RNF1)
df$RNF2_Z <- scale(df$RNF2)
df$RNS1_Z <- scale(df$RNS1)
df$RNS2_Z <- scale(df$RNS2)
df$Attach_anx_Z <- scale(df$Attach_anx)
df$Attach_avd_Z <- scale(df$Attach_avd)

#demographic distributions, histograms(age, race/ethnicity, gender)
library(ggplot2)
library(gridExtra)
library(viridis)

hist(df$Age_1)
plot(df$Race)
plot(df$Gender)
plot(df$Income)

df %>% select(
  Gender, Age_1, Race, Education, Employed, Income, SexualOr) %>% 
  summary()
nrow(df)

sd(df$Age_1)

table(df$Race)

table(df$Education)

table(df$SexualOr)

table(df$Student)


#RNS/F distributions pre/post, histograms
p1 <- ggplot(df, aes(RNF1_Z)) +
  geom_histogram(aes(color = Cond, fill = Cond), 
                 position = 'identity',
                 bins = 10,
                 alpha = 0.3)

p2 <- ggplot(df, aes(RNF2_Z)) +
  geom_histogram(aes(color = Cond, fill = Cond), 
                 position = 'identity',
                 bins = 10,
                 alpha = 0.3)

p3 <- ggplot(df, aes(RNS1_Z)) +
  geom_histogram(aes(color = Cond, fill = Cond), 
                 position = 'identity',
                 bins = 10,
                 alpha = 0.3)

p4 <- ggplot(df, aes(RNS2_Z)) +
  geom_histogram(aes(color = Cond, fill = Cond), 
                 position = 'identity',
                 bins = 10,
                 alpha = 0.3)
grid.arrange(p1,p2,p3,p4, nrow = 2)

p1 <- ggplot(df, aes(RNF1_Z, group = Cond, fill = Cond)) +
  geom_density(alpha = 0.6) +
  scale_fill_viridis(discrete=TRUE) +
  scale_color_viridis(discrete=TRUE)

p2 <- ggplot(df, aes(RNF2_Z, group = Cond, fill = Cond)) +
  geom_density(alpha = 0.6) +
  scale_fill_viridis(discrete=TRUE) +
  scale_color_viridis(discrete=TRUE)

p3 <- ggplot(df, aes(RNS1_Z, group = Cond, fill = Cond)) +
  geom_density(alpha = 0.6) +
  scale_fill_viridis(discrete=TRUE) +
  scale_color_viridis(discrete=TRUE)

p4 <- ggplot(df, aes(RNS2_Z, group = Cond, fill = Cond)) +
  geom_density(alpha = 0.6) +
  scale_fill_viridis(discrete=TRUE) +
  scale_color_viridis(discrete=TRUE)

grid.arrange(p1,p2,p3,p4, nrow = 2)

df_p <- df %>% 
  select("RNS1_Z", "RNS2_Z") %>% 
  pivot_longer(cols = c("RNS1_Z", "RNS2_Z"),
               names_to = "Time",
               values_to = "Score")
p1 <- ggplot(df_p, aes(Score, group = Time, fill = Time)) +
  geom_density(alpha = 0.6) +
  scale_fill_viridis(discrete = T) +
  scale_color_viridis(discrete = T)

ggplot(df_p, aes(Time, Score)) +
  geom_violin() + 
  geom_boxplot(width = 0.1, color = "black", alpha = 0.4)

df_p <- df %>% 
  select("RNF1_Z", "RNF2_Z") %>% 
  pivot_longer(cols = c("RNF1_Z", "RNF2_Z"),
               names_to = "Time",
               values_to = "Score")

p2 <- ggplot(df_p, aes(Score, group = Time, fill = Time)) +
  geom_density(alpha = 0.6) +
  scale_fill_viridis(discrete = T) +
  scale_color_viridis(discrete = T)

ggplot(df_p, aes(Time, Score)) +
  geom_violin() + 
  geom_boxplot(width = 0.1, color = "black", alpha = 0.4)

grid.arrange(p1,p2, nrow = 1)

#KM components, pKM distributions
ggplot(df, aes(Cond, pKM, fill = Cond)) +
  geom_violin() +
  geom_boxplot(width = 0.1, color = "black", alpha = 0.4)

ggplot(df, aes(Cond, pKM_Z, fill = Cond)) +
  geom_violin() #cool, just checking they are the same.

p1 <- ggplot(df, aes(KM_P, group = Cond, fill = Cond)) +
  geom_density(alpha = 0.6) +
  scale_fill_viridis(discrete=TRUE) +
  scale_color_viridis(discrete=TRUE)

p2 <- ggplot(df, aes(KM_CSR, group = Cond, fill = Cond)) +
  geom_density(alpha = 0.6) +
  scale_fill_viridis(discrete=TRUE) +
  scale_color_viridis(discrete=TRUE)

p3 <- ggplot(df, aes(KM_M, group = Cond, fill = Cond)) +
  geom_density(alpha = 0.6) +
  scale_fill_viridis(discrete=TRUE) +
  scale_color_viridis(discrete=TRUE)

p4 <- ggplot(df, aes(KM_PA, group = Cond, fill = Cond)) +
  geom_density(alpha = 0.6) +
  scale_fill_viridis(discrete=TRUE) +
  scale_color_viridis(discrete=TRUE)

p5 <- ggplot(df, aes(KM_L, group = Cond, fill = Cond)) +
  geom_density(alpha = 0.6) +
  scale_fill_viridis(discrete=TRUE) +
  scale_color_viridis(discrete=TRUE)

grid.arrange(p1,p2,p3,p4,p5, nrow = 2)

#Tech difficulties mapping

table(df$Vid_Issue, df$Cond) #no huge difference between conditions on tech difficulties. 
ggplot(df, aes(Vid_Issue, pKM, fill = Vid_Issue)) +
  geom_violin() +
  geom_boxplot(width = 0.1, color = "black", alpha = 0.4) #means look the same, no big differences here

ggplot(df, aes(Vid_Issue, group = Cond, fill = Cond)) +
  geom_density(alpha = 0.6) +
  scale_fill_viridis(discrete = T) +
  scale_color_viridis((discrete = T))

#Previous experience mapping

table(df$Vid_PrevExp, df$Cond) #1 is no prevExp, 2 is yes prevExp
ggplot(df, aes(Cond, pKM, fill = Vid_PrevExp)) +
  geom_violin() +
  geom_boxplot(width = 0.1, color = "black", alpha = 0.4)

df[df$Cond == "KM",] %>% 
  group_by(Vid_PrevExp) %>% 
  summarise_at(vars(pKM), list(pKM = mean)) #so a slight difference, I think prevExp actually ADDS to KM (may be indicator of self-selection though)
##quick correlation shows no relation to EC at least



#correlation table: 5KMs, pKM, Personality, RNS/F T1-T2
library(Hmisc)
df_cor <- df %>% 
  select(KM_P, KM_CSR, KM_M, KM_PA, KM_L, Attach_anx, Attach_avd, EC, Vid_PrevExp)
rcorr(as.matrix(df_cor))

df_cor <- df %>% 
  select(KM_P, KM_CSR, KM_M, KM_PA, KM_L, BFI_O, BFI_C, BFI_E, BFI_A, BFI_N)
rcorr(as.matrix(df_cor))

df_cor <- df %>% 
  select(KM_P, KM_CSR, KM_M, KM_PA, KM_L, pKM, RNS1, RNS2, RNF1, RNF2)
rcorr(as.matrix(df_cor))

####Bayesian Estimation...attempt####


##XMY Mediation: X->M 

require(rjags)
M_Model <- "model {

#likelihood
for(i in 1:n){
M[i]~dnorm(mu[i], tau)
mu[i] = beta0 + beta1*X[i]
}

#Prior for beta
beta0 ~ dnorm(mu0, tau0)
beta1 ~ dnorm(mu0, tau0)


tau0=1/sigma02

#Prior for precision
tau ~ dt(a,1/b^2,1)T(0,)

#compute variance
sigma2 = 1/tau
}"

M_data <- list(
  n = nrow(df),
  M = df$pKM_Z[,1],
  X = df$Cond,
  mu0 = 0,
  sigma02 = 2^2,
  a = 0,
  b = 25
)

M_inits <- list(
  list(beta0 = 0, beta1 = 2),
  list(beta0 = 10, beta1 = 0.01),
  list(beta0 = 5, beta1 = 0.5)
)

M_Model <- jags.model(file = textConnection(M_Model),
                            data = M_data,
                            inits = M_inits,
                            n.chains = 3,
                            quiet = TRUE)
update(M_Model, n.iter = 5000)
M_Model_res <- coda.samples(M_Model,
                                variable.names = c("beta0", "beta1", "sigma2"),
                                n.iter = 10000)
plot(M_Model_res) #Trace plots and parameter density

gelman.plot(M_Model_res) #BGR statistic, want closer to 1

effectiveSize(M_Model_res[[1]][,"beta0"])
effectiveSize(M_Model_res[[1]][,"beta1"])

autocorr.plot(M_Model_res[[1]][,"beta0"], main = "Intercept") #Suggests to me maybe go larger with sampling, correct trend at least
autocorr.plot(M_Model_res[[1]][,"beta1"], main = "Slope") #

summary(M_Model_res) #want MC error (Time-series SE) to be lower than 1/20th size of parameter SDs

##X-> Y_RNS
require(rjags)
RNS_Model <- "model {

#likelihood
for(i in 1:n){
Y[i]~dnorm(mu[i], tau)
mu[i] = beta0 + beta1*X[i]
}

#Prior for beta
beta0 ~ dnorm(mu0, tau0)
beta1 ~ dnorm(mu0, tau0)


tau0=1/sigma02

#Prior for precision
tau ~ dt(a,1/b^2,1)T(0,)

#compute variance
sigma2 = 1/tau
}"

RNS_data <- list(
  n = nrow(df),
  Y = df$RNS2_Z[,1],
  X = df$Cond,
  mu0 = 0,
  sigma02 = 2^2,
  a = 0,
  b = 25
)

RNS_inits <- list(
  list(beta0 = 0, beta1 = 2),
  list(beta0 = 10, beta1 = 0.01),
  list(beta0 = 5, beta1 = 0.5)
)

RNS_Model <- jags.model(file = textConnection(RNS_Model),
                      data = RNS_data,
                      inits = RNS_inits,
                      n.chains = 3,
                      quiet = TRUE)
update(RNS_Model, n.iter = 5000)
RNS_Model_res <- coda.samples(RNS_Model,
                            variable.names = c("beta0", "beta1", "sigma2"),
                            n.iter = 10000)
plot(RNS_Model_res) #Trace plots and parameter density

gelman.plot(RNS_Model_res) #BGR statistic, want closer to 1

effectiveSize(RNS_Model_res[[1]][,"beta0"])
effectiveSize(RNS_Model_res[[1]][,"beta1"])

autocorr.plot(RNS_Model_res[[1]][,"beta0"], main = "Intercept") #Suggests to me maybe go larger with sampling, correct trend at least
autocorr.plot(RNS_Model_res[[1]][,"beta1"], main = "Slope") #

summary(RNS_Model_res)

##X-> Y_RNF
require(rjags)
RNF_Model <- "model {

#likelihood
for(i in 1:n){
Y[i]~dnorm(mu[i], tau)
mu[i] = beta0 + beta1*X[i]
}

#Prior for beta
beta0 ~ dnorm(mu0, tau0)
beta1 ~ dnorm(mu0, tau0)


tau0=1/sigma02

#Prior for precision
tau ~ dt(a,1/b^2,1)T(0,)

#compute variance
sigma2 = 1/tau
}"

RNF_data <- list(
  n = nrow(df),
  Y = df$RNF2_Z[,1],
  X = df$Cond,
  mu0 = 0,
  sigma02 = 2^2,
  a = 0,
  b = 25
)

RNF_inits <- list(
  list(beta0 = 0, beta1 = 2),
  list(beta0 = 10, beta1 = 0.01),
  list(beta0 = 5, beta1 = 0.5)
)

RNF_Model <- jags.model(file = textConnection(RNF_Model),
                        data = RNF_data,
                        inits = RNF_inits,
                        n.chains = 3,
                        quiet = TRUE)
update(RNF_Model, n.iter = 5000)
RNF_Model_res <- coda.samples(RNF_Model,
                              variable.names = c("beta0", "beta1", "sigma2"),
                              n.iter = 10000)
plot(RNF_Model_res) #Trace plots and parameter density

gelman.plot(RNF_Model_res) #BGR statistic, want closer to 1

effectiveSize(RNF_Model_res[[1]][,"beta0"])
effectiveSize(RNF_Model_res[[1]][,"beta1"])

autocorr.plot(RNF_Model_res[[1]][,"beta0"], main = "Intercept") #Suggests to me maybe go larger with sampling, correct trend at least
autocorr.plot(RNF_Model_res[[1]][,"beta1"], main = "Slope") #

summary(RNF_Model_res)

##X + Cov -> Y_RNS
require(rjags)
RNS_C_Model <- "model {

#likelihood
for(i in 1:n){
Y[i]~dnorm(mu[i], tau)
mu[i] = beta0 + beta1*X[i] + beta2*C[i]
}

#Prior for beta
beta0 ~ dnorm(mu0, tau0)
beta1 ~ dnorm(mu0, tau0)
beta2 ~ dnorm(mu0, tau0)

tau0=1/sigma02

#Prior for precision
tau ~ dt(a,1/b^2,1)T(0,)

#compute variance
sigma2 = 1/tau
}"

RNS_data <- list(
  n = nrow(df),
  Y = df$RNS2_Z[,1],
  X = df$Cond,
  C = df$RNS1_Z[,1],
  mu0 = 0,
  sigma02 = 2^2,
  a = 0,
  b = 25
)

RNS_inits <- list(
  list(beta0 = 0, beta1 = 2, beta2 = 5),
  list(beta0 = 10, beta1 = 0.01, beta2 = 0),
  list(beta0 = 5, beta1 = 0.5, beta2 = 0.01)
)

RNS_C_Model <- jags.model(file = textConnection(RNS_C_Model),
                        data = RNS_data,
                        inits = RNS_inits,
                        n.chains = 3,
                        quiet = TRUE)
update(RNS_C_Model, n.iter = 5000)
RNS_C_Model_res <- coda.samples(RNS_C_Model,
                              variable.names = c("beta0", "beta1", "beta2", "sigma2"),
                              n.iter = 10000)
plot(RNS_C_Model_res) #Trace plots and parameter density

gelman.plot(RNS_C_Model_res) #BGR statistic, want closer to 1

effectiveSize(RNS_C_Model_res[[1]][,"beta0"])
effectiveSize(RNS_C_Model_res[[1]][,"beta1"])

autocorr.plot(RNS_C_Model_res[[1]][,"beta0"], main = "Intercept") #Suggests to me maybe go larger with sampling, correct trend at least
autocorr.plot(RNS_C_Model_res[[1]][,"beta1"], main = "Slope") #

summary(RNS_C_Model_res)

##X + Cov -> Y_RNF
RNF_C_Model <- "model {

#likelihood
for(i in 1:n){
Y[i]~dnorm(mu[i], tau)
mu[i] = beta0 + beta1*X[i] + beta2*C[i]
}

#Prior for beta
beta0 ~ dnorm(mu0, tau0)
beta1 ~ dnorm(mu0, tau0)
beta2 ~ dnorm(mu0, tau0)

tau0=1/sigma02

#Prior for precision
tau ~ dt(a,1/b^2,1)T(0,)

#compute variance
sigma2 = 1/tau
}"

RNF_data <- list(
  n = nrow(df),
  Y = df$RNF2_Z[,1],
  X = df$Cond,
  C = df$RNF1_Z[,1],
  mu0 = 0,
  sigma02 = 2^2,
  a = 0,
  b = 25
)

RNF_inits <- list(
  list(beta0 = 0, beta1 = 2, beta2 = 5),
  list(beta0 = 10, beta1 = 0.01, beta2 = 0),
  list(beta0 = 5, beta1 = 0.5, beta2 = 0.01)
)

RNF_C_Model <- jags.model(file = textConnection(RNF_C_Model),
                          data = RNF_data,
                          inits = RNF_inits,
                          n.chains = 3,
                          quiet = TRUE)
update(RNF_C_Model, n.iter = 5000)
RNF_C_Model_res <- coda.samples(RNF_C_Model,
                                variable.names = c("beta0", "beta1", "beta2", "sigma2"),
                                n.iter = 10000)
plot(RNF_C_Model_res) #Trace plots and parameter density

gelman.plot(RNF_C_Model_res) #BGR statistic, want closer to 1

effectiveSize(RNF_C_Model_res[[1]][,"beta0"])
effectiveSize(RNF_C_Model_res[[1]][,"beta1"])

autocorr.plot(RNF_C_Model_res[[1]][,"beta0"], main = "Intercept") #Suggests to me maybe go larger with sampling, correct trend at least
autocorr.plot(RNF_C_Model_res[[1]][,"beta1"], main = "Slope") #

summary(RNF_C_Model_res)


#====RNS
##XMY Mediation: XM + Cov -> Y

require(rjags)
RNS_Y_Model <- "model {

#likelihood
for(i in 1:n){
Y[i]~dnorm(mu[i], tau)
mu[i] = beta0 + beta1*M[i] + beta2*X[i] + beta3*C[i]
}

#Prior for beta
beta0 ~ dnorm(mu0, tau0)
beta1 ~ dnorm(mu0, tau0)
beta2 ~ dnorm(mu0, tau0)
beta3 ~ dnorm(mu0, tau0)

tau0=1/sigma02

#Prior for precision
tau ~ dt(a,1/b^2,1)T(0,)

#compute variance
sigma2 = 1/tau
}"

RNS_Y_data <- list(
  n = nrow(df),
  Y = df$RNS2_Z[,1],
  M = df$pKM_Z[,1],
  X = df$Cond,
  C = df$RNS1_Z[,1],
  mu0 = 0,
  sigma02 = 2^2,
  a = 0,
  b = 25
)

RNS_Y_inits <- list(
  list(beta0 = 0, beta1 = 2, beta2 = 5, beta3 = 10),
  list(beta0 = 10, beta1 = 0.01, beta2 = 0, beta3 = 0.0001),
  list(beta0 = 5, beta1 = 0.5, beta2 = 0.01, beta3 = 0)
)

RNS_Y_Model <- jags.model(file = textConnection(RNS_Y_Model),
                          data = RNS_Y_data,
                          inits = RNS_Y_inits,
                          n.chains = 3,
                          quiet = TRUE)
update(RNS_Y_Model, n.iter = 5000)
RNS_Y_Model_res <- coda.samples(RNS_Y_Model,
                                variable.names = c("beta0", "beta1", "beta2", "beta3", "sigma2"),
                                n.iter = 10000) 
plot(RNS_Y_Model_res) #Trace plots and parameter densities

gelman.plot(RNS_Y_Model_res) #BGR statistic, quant measure of chain mixing, very close to 1

effectiveSize(RNS_Y_Model_res[[1]][,"beta0"]) #measure of how many 'effective' independent samples we got
effectiveSize(RNS_Y_Model_res[[1]][,"beta1"]) #with iter=10k, Suggests to me maybe go larger with sampling, correct trend at least
effectiveSize(RNS_Y_Model_res[[1]][,"beta2"])
effectiveSize(RNS_Y_Model_res[[1]][,"beta3"])

autocorr.plot(RNS_Y_Model_res[[1]][,"beta0"], main = "Intercept") #measure of whether we have independent sampling yet, may need thinning with larger iteration
autocorr.plot(RNS_Y_Model_res[[1]][,"beta1"], main = "pKM Slope") #
autocorr.plot(RNS_Y_Model_res[[1]][,"beta2"], main = "Cond Slope") #
autocorr.plot(RNS_Y_Model_res[[1]][,"beta3"], main = "T1 Slope") #



summary(RNS_Y_Model_res) #remember; beta0 = intercept (reflects neutral cond base), beta1 = pKM effect, beta2 = Cond effect, beta3 = T1 Covariate effect


#===RNF
##XMY Mediation: XM + Cov -> Y

require(rjags)
RNF_Y_Model <- "model {

#likelihood
for(i in 1:n){
Y[i]~dnorm(mu[i], tau)
mu[i] = beta0 + beta1*M[i] + beta2*X[i] + beta3*C[i]
}

#Prior for beta
beta0 ~ dnorm(mu0, tau0)
beta1 ~ dnorm(mu0, tau0)
beta2 ~ dnorm(mu0, tau0)
beta3 ~ dnorm(mu0, tau0)

tau0=1/sigma02

#Prior for precision
tau ~ dt(a,1/b^2,1)T(0,)

#compute variance
sigma2 = 1/tau
}"

RNF_Y_data <- list(
  n = nrow(df),
  Y = df$RNF2_Z[,1],
  M = df$pKM_Z[,1],
  X = df$Cond,
  C = df$RNF1_Z[,1],
  mu0 = 0,
  sigma02 = 2^2,
  a = 0,
  b = 25
)

RNF_Y_inits <- list(
  list(beta0 = 0, beta1 = 2, beta2 = 5, beta3 = 10),
  list(beta0 = 10, beta1 = 0.01, beta2 = 0, beta3 = 0.0001),
  list(beta0 = 5, beta1 = 0.5, beta2 = 0.01, beta3 = 0)
)

RNF_Y_Model <- jags.model(file = textConnection(RNF_Y_Model),
                          data = RNF_Y_data,
                          inits = RNF_Y_inits,
                          n.chains = 3,
                          quiet = TRUE)
update(RNF_Y_Model, n.iter = 5000)
RNF_Y_Model_res <- coda.samples(RNF_Y_Model,
                                variable.names = c("beta0", "beta1", "beta2", "beta3", "sigma2"),
                                n.iter = 10000) 
plot(RNF_Y_Model_res) #Trace plots and parameter densities

gelman.plot(RNF_Y_Model_res) #BGR statistic, want closer to 1

effectiveSize(RNF_Y_Model_res[[1]][,"beta0"]) #with iter=10k, Suggests to me maybe go larger with sampling, correct trend at least
effectiveSize(RNF_Y_Model_res[[1]][,"beta1"])
effectiveSize(RNF_Y_Model_res[[1]][,"beta2"])
effectiveSize(RNF_Y_Model_res[[1]][,"beta3"])

autocorr.plot(RNF_Y_Model_res[[1]][,"beta0"], main = "Intercept") 
autocorr.plot(RNF_Y_Model_res[[1]][,"beta1"], main = "Slope") #

summary(RNF_Y_Model_res) #remember; beta0 = intercept (reflects neutral cond base), beta1 = pKM effect, beta2 = Cond' effect, beta3 = T1 Covariate effect

plot(RNS_Y_Model_res)



###########Just to see without T2 covariate--how important was it that we used this design?
##XMY Mediation: XM -> Y without T2 covariate


##XMY Mediation: XM -> Y
#RNS
require(rjags)
RNS_Yt_Model <- "model {

#likelihood
for(i in 1:n){
Y[i]~dnorm(mu[i], tau)
mu[i] = beta0 + beta1*M[i] + beta2*X[i]
}

#Prior for beta
beta0 ~ dnorm(mu0, tau0)
beta1 ~ dnorm(mu0, tau0)
beta2 ~ dnorm(mu0, tau0)

tau0=1/sigma02

#Prior for precision
tau ~ dt(a,1/b^2,1)T(0,)

#compute variance
sigma2 = 1/tau
}"

RNS_Y_data <- list(
  n = nrow(df),
  Y = df$RNS2_Z[,1],
  M = df$pKM_Z[,1],
  X = df$Cond,
  mu0 = 0,
  sigma02 = 2^2,
  a = 0,
  b = 25
)

RNS_Y_inits <- list(
  list(beta0 = 0, beta1 = 2, beta2 = 5),
  list(beta0 = 10, beta1 = 0.01, beta2 = 0),
  list(beta0 = 5, beta1 = 0.5, beta2 = 0.01)
)

RNS_Yt_Model <- jags.model(file = textConnection(RNS_Yt_Model),
                          data = RNS_Y_data,
                          inits = RNS_Y_inits,
                          n.chains = 3,
                          quiet = TRUE)
update(RNS_Yt_Model, n.iter = 5000)
RNS_Yt_Model_res <- coda.samples(RNS_Yt_Model,
                                variable.names = c("beta0", "beta1", "beta2", "sigma2"),
                                n.iter = 10000) #
plot(RNS_Yt_Model_res) #Trace plots and parameter densities

gelman.plot(RNS_Yt_Model_res) #BGR statistic, quant measure of chain mixing, very close to 1

effectiveSize(RNS_Yt_Model_res[[1]][,"beta0"]) #measure of how many 'effective' independent samples we got
effectiveSize(RNS_Yt_Model_res[[1]][,"beta1"]) #with iter=10k, Suggests to me maybe go larger with sampling, correct trend at least
effectiveSize(RNS_Yt_Model_res[[1]][,"beta2"])
effectiveSize(RNS_Yt_Model_res[[1]][,"beta3"])

autocorr.plot(RNS_Yt_Model_res[[1]][,"beta0"], main = "Intercept") #measure of whether we have independent sampling yet, may need thinning with larger iteration
autocorr.plot(RNS_Yt_Model_res[[1]][,"beta1"], main = "pKM Slope") #
autocorr.plot(RNS_Yt_Model_res[[1]][,"beta2"], main = "Cond Slope") #

summary(RNS_Yt_Model_res)



#RNF
require(rjags)
RNF_Yt_Model <- "model {

#likelihood
for(i in 1:n){
Y[i]~dnorm(mu[i], tau)
mu[i] = beta0 + beta1*M[i] + beta2*X[i]
}

#Prior for beta
beta0 ~ dnorm(mu0, tau0)
beta1 ~ dnorm(mu0, tau0)
beta2 ~ dnorm(mu0, tau0)
beta3 ~ dnorm(mu0, tau0)

tau0=1/sigma02

#Prior for precision
tau ~ dt(a,1/b^2,1)T(0,)

#compute variance
sigma2 = 1/tau
}"

RNF_Y_data <- list(
  n = nrow(df),
  Y = df$RNF2_Z[,1],
  M = df$pKM_Z[,1],
  X = df$Cond,
  mu0 = 0,
  sigma02 = 2^2,
  a = 0,
  b = 25
)

RNF_Y_inits <- list(
  list(beta0 = 0, beta1 = 2, beta2 = 5),
  list(beta0 = 10, beta1 = 0.01, beta2 = 0),
  list(beta0 = 5, beta1 = 0.5, beta2 = 0.01)
)

RNF_Yt_Model <- jags.model(file = textConnection(RNF_Yt_Model),
                          data = RNF_Y_data,
                          inits = RNF_Y_inits,
                          n.chains = 3,
                          quiet = TRUE)
update(RNF_Yt_Model, n.iter = 5000)
RNF_Yt_Model_res <- coda.samples(RNF_Yt_Model,
                                variable.names = c("beta0", "beta1", "beta2", "sigma2"),
                                n.iter = 10000) #
plot(RNF_Yt_Model_res) #Trace plots and parameter densities

gelman.plot(RNF_Yt_Model_res) #BGR statistic, want closer to 1

effectiveSize(RNF_Yt_Model_res[[1]][,"beta0"]) 
effectiveSize(RNF_Yt_Model_res[[1]][,"beta1"])

autocorr.plot(RNF_Yt_Model_res[[1]][,"beta0"], main = "Intercept") 
autocorr.plot(RNF_Yt_Model_res[[1]][,"beta1"], main = "Slope") #

summary(RNF_Yt_Model_res) #remember; beta0 = intercept (reflects neutral cond base), beta1 = mediator pKM effect, beta2 = Cond' effect


#Step 1
summary(M_Model_res)

#Step 2
summary(RNS_Model_res)
summary(RNF_Model_res)

#Step 2.5
summary(RNS_C_Model_res)
summary(RNF_C_Model_res)

#Step 3
summary(RNS_Y_Model_res)
summary(RNF_Y_Model_res)

#Step 3 (more like step 2.9)
summary(RNS_Yt_Model_res)
summary(RNF_Yt_Model_res)
