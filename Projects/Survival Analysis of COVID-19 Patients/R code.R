############### Survival Analysis of COVID-19 Patients ############################
## Load Packages
library(readr)
library(dplyr)
library(ggplot2)
library(survival)
library(KMsurv)
library(ggfortify)
library(ggforce)
library(ggpubr)
library(survminer)

#### Import data --------------------------------------
Data <- read_csv("D:/Uni Work/Year 4/ACTL3141/CovidHospDataBrasil.csv")
# Data <- read_csv("CovidHospDataBrasil.csv")

#### Exploratory Data Analysis -----------------------------------
# age range
dfc <- cut(Data$age, breaks=c(-1, 4, 9, 19, 29, 39, 49, 59, 69, 79, 89, Inf), labels=paste("Age", 1:11, sep=""))
df <- cbind(Data,dfc)
ggplot(df,aes(x=dfc)) + geom_bar()
# better plot of age 
ggplot(df,aes(x=age, color = sex)) + 
  geom_histogram(binwidth = 5, aes(y = ..density..), colour=2,fill="white") + 
  geom_density(colour=2, lwd = 1) 

# simple stats
table(df$covidDeath) # mortality rate
mean(df$age)

# comparison between F and M
ggplot(df, aes(x=age, color=sex, fill=sex)) +
  geom_histogram(aes(y=..density..), position="identity", binwidth = 2,alpha=0.6)+
  geom_density(alpha=0) +
  scale_color_manual(values=c("#999999", "#1E90FF", "#1E90FF")) +
  scale_fill_manual(values=c("#999999", "#1E90FF", " #1E90FF")) +
  labs(title = "Distribution by Age and Gender",x = "Age", y = "Density")

#getting age summary
table(df$dfc)
Age_death <- df %>% select(dfc,covidDeath)
table(Age_death)

zf <- Data %>% filter(age <= 4)
summary(zf)
# 521 obs. 72 deaths 449 survived
# lethality rate 72/521

fn <- Data %>% filter(age <= 9 & age >= 5)
summary(fn)

tn <- Data %>% filter(age <= 19 & age >= 10)
summary(tn)

twn <- Data %>% filter(age <= 29 & age >= 20)
summary(twn)

thn <- Data %>% filter(age <= 39 & age >= 30)
summary(thn)

fon <- Data %>% filter(age <= 49 & age >= 40)
summary(fon)

fin <- Data %>% filter(age <= 59 & age >= 50)
summary(fin)

sn <- Data %>% filter(age <= 69 & age >= 60)
summary(sn)

sen <- Data %>% filter(age <= 79 & age >= 70)
summary(sen)

en <- Data %>% filter(age <= 89 & age >= 80)
summary(en)

n <- Data %>% filter(age >= 90)
summary(n)

# gender summary
table(df$sex)
FM <- df %>% select(sex,covidDeath)
table(FM)

# vaccine summary
table(df$vaccine)
Vac <- df %>% select(vaccine,covidDeath)
table(Vac)

vy <- Data %>% filter(vaccine == TRUE)

# symptoms and comorbidities
per <- apply(df, 2, function(x) sum(x == TRUE) / length(x))
per <- as.data.frame(t(per))
per <- per %>% select(-c(dateBirth,age,sex,vaccine,dateHosp,dateEndObs,covidDeath,icu,dateAdmIcu,dateDisIcu,dfc))

#symptoms
sym <- per %>% select(c(fever,cough,sorethroat,dyspnoea,respdistress,oxygensat,diarrhea,vomit))
ns <- c("Fever","Cough","Sore Throat","Dyspnoea","Respiratory distress", 
        "Blood oxygen saturation < 95%","Diarrhea","Vomit")
sym <- as.data.frame(t(sym))
vs <- sym$V1
sym <- cbind(ns,vs)
sym <- as.data.frame(sym)
sym$vs <- as.numeric(sym$vs)
sym$ns <- factor(sym$ns, levels=sym$ns[order(sym$vs,decreasing = TRUE)])
ggplot(sym, aes(x=ns, y=vs,fill=ns)) + 
  geom_bar(stat = "identity", show.legend = FALSE) + 
  labs(x="Symptoms", y="Percentage") 
# alternative colour scheme
ggplot(sym, aes(x=ns, y=vs,fill=ns)) + 
  geom_bar(stat = "identity", show.legend = FALSE) + 
  labs(x="", y="Percentage",title="Symptoms") + 
  scale_fill_brewer(palette = "Blues") + 
  scale_y_continuous(labels = scales::percent) +
  coord_flip() +
  theme(axis.text.y = element_text(size=9))

Symptoms_p <- ggplot(sym, aes(x=ns, y=vs,fill=ns)) + 
  geom_bar(stat = "identity", show.legend = FALSE) + 
  labs(x="", y="Percentage of Patients",title="Symptoms") + 
  scale_color_manual(values=c(rep("#13C2EB",8))) +
  scale_fill_manual(values= c(rep("#13C2EB",8))) +
  scale_y_continuous(labels = scales::label_percent(accuracy = 1L)) +
  coord_flip() +
  theme(axis.text.y = element_text(size=9))

#comorbidities
com <- per %>% select(-c(fever,cough,sorethroat,dyspnoea,respdistress,oxygensat,diarrhea,vomit))
nc <- c("Cardiovascular disease","Hematologic disease","Down's syndrome","Liver disease",
        "Asthma","Diabetes","Neurological disease","Pneumopathy","Immunodeficiencies",
        "Renal disease","Obesity")
com <- as.data.frame(t(com))
vc <- com$V1
com <- cbind(nc,vc)
com <- as.data.frame(com)
com$vc <- as.numeric(com$vc)
com$nc <- factor(com$nc, levels=com$nc[order(com$vc,decreasing = TRUE)])
Com_p <- ggplot(com, aes(x=nc, y=vc,fill=nc)) + 
  geom_bar(stat = "identity", show.legend = FALSE) + 
  labs(x="", y="Percentage of Patients",title="Comorbidities") + 
  scale_color_manual(values=c(rep("#13C2EB",11))) +
  scale_fill_manual(values= c(rep("#13C2EB",11))) +
  scale_y_continuous(labels = scales::label_percent(accuracy = 1L)) +
  coord_flip() +
  theme(axis.text.y = element_text(size=9))

# Combining symptoms and comorbidities plots
ggarrange(Symptoms_p, Com_p, nrow = 1)

# could possibly get all the values
table(df$cough)
CD <- df %>% select(cough,covidDeath)
table(CD)

#icu
table(df$icu)
ICU <- df %>% select(icu,covidDeath)
table(ICU)

#cardio
table(df$cardio)
CDe <- df %>% select(cardio,covidDeath)
table(CDe)

#hematologic
table(df$hematologic)
HM <- df %>% select(hematologic,covidDeath)
table(HM)

#down
table(df$downsyn)
DS <- df %>% select(downsyn,covidDeath)
table(DS)

#hepatic
table(df$hepatic)
HP <- df %>% select(hepatic,covidDeath)
table(HP)

#asthma
table(df$asthma)
AS <- df %>% select(asthma,covidDeath)
table(AS)

#diabetes
table(df$diabetes)
DB <- df %>% select(diabetes,covidDeath)
table(DB)

#neurol
table(df$neurological)
NR <- df %>% select(neurological,covidDeath)
table(NR)

#pneum
table(df$pneumopathy)
PN <- df %>% select(pneumopathy,covidDeath)
table(PN)

#immuno
table(df$immuno)
IM <- df %>% select(immuno,covidDeath)
table(IM)

#renal
table(df$renal)
RN <- df %>% select(renal,covidDeath)
table(RN)

#obesity
table(df$obesity)
OB <- df %>% select(obesity,covidDeath)
table(OB)

#### Mortality and Survival Analysis ----------------------------------------------
df_time <- df %>% mutate(os_days = as.numeric(difftime(dateEndObs,dateHosp,units="days")))
cens <- Surv(df_time$os_days,df_time$covidDeath)

#KM Estimation
fit <- survfit(cens ~ 1, conf.int = 0.95, conf.type="log")
plot(fit, main = "KM estimate with 95% confidence intervals", xlab = "time (days)",ylab = "S(x)")
summary(fit)

#NA Estimation
fit2 <- survfit(cens ~ 1, conf.int = 0.95, type="fh")
plot(fit2, main = "NA estimate with 95% confidence intervals", xlab = "time", ylab = "S(x)")
summary(fit2)

#Commorbidities (generic)
Sym <- df %>% select(-c(dateBirth,age,sex,vaccine,dateHosp,dateEndObs,covidDeath,icu,dateAdmIcu,dateDisIcu,dfc))

comm <- Sym %>% select(-c(fever,cough,sorethroat,dyspnoea,respdistress,oxygensat,diarrhea,vomit))
# get all rows with at least one true value (have at least one commorbidities)
id <- which(apply(comm, 1, any))

with_comm <- df_time[id,]
without_comm <- df_time[-id,]

censwith_comm <- Surv(with_comm$os_days,with_comm$covidDeath)
fitwith_comm <- survfit(censwith_comm ~ 1, conf.int = 0.95)

censwithout_comm <- Surv(without_comm$os_days,without_comm$covidDeath)
fitwithout_comm <- survfit(censwithout_comm ~ 1, conf.int = 0.95)

plot(fitwith_comm, main = "Commorbidities vs no commorbidities KM estimates", xlab = "Time (days)", ylab = "S(x)", col = "blue")
lines(fitwithout_comm, col = 'red')
legend("topright", c("Commorbidities", "No commorbidities"), col = c("blue", "red"), lty = 1)
# not much difference maybe cuz going to hospital already a severe case

table(with_comm$covidDeath) # 0.7248513
table(without_comm$covidDeath) # 0.4605651

# simpler and better way of graphing
newdf <- df_time %>% mutate(Commorbidities = FALSE)
newdf$Commorbidities[id] = TRUE

# some extra stats for no comm vs comm
table(newdf$Commorbidities)
nocomm <- newdf %>% select(Commorbidities,covidDeath)
table(nocomm)

fittest <- survfit(Surv(os_days,covidDeath) ~ Commorbidities, data = newdf)

COMg <- autoplot(survfit(Surv(os_days,covidDeath) ~ Commorbidities, data = newdf), conf.int = 0.95, censor = FALSE) +
  labs(x = "Time (days)", y = "S(x)", colour="Commorbidities", title = "Commorbidities KM Estimates") +
  guides(fill = FALSE) + 
  theme(legend.position = c(.95, .95),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6)) +
  scale_x_continuous(limits= c(0,100))

# Gender
Male <- df_time %>% filter(sex == "M")
Female <- df_time %>% filter(sex == "F")

censMale <- Surv(Male$os_days,Male$covidDeath)
fitMale <- survfit(censMale ~ 1, conf.int = 0.95)

censFemale <- Surv(Female$os_days,Female$covidDeath)
fitFemale <- survfit(censFemale ~ 1, conf.int = 0.95)

plot(fitMale, main = "Male vs Female KM estimates", xlab = "Time (days)", ylab = "S(x)", col = "blue")
lines(fitFemale, col = 'red')
legend("topright", c("Male", "Female"), col = c("blue", "red"), lty = 1)

#better way to plot gender survival analysis
SEXg <- autoplot(survfit(Surv(os_days,covidDeath) ~ sex, data = df_time), conf.int = 0.95, censor = FALSE) +
  labs(x = "Time (days)", y = "S(x)", colour="Gender", title = "Male vs Female KM Estimates") +
  guides(fill = FALSE) + 
  theme(legend.position = c(.95, .95),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6)) +
  scale_x_continuous(limits= c(0,100))

# zoom in on 50 to 100 (probs refer to this in the appendix)
autoplot(survfit(Surv(os_days,covidDeath) ~ sex, data = df_time), conf.int = 0.95, censor = FALSE) +
  labs(x = "Time (days)", y = "S(x)", colour="Gender", title = "Male vs Female KM Estimates") +
  guides(fill = FALSE) + 
  facet_zoom(xlim = c(50, 125),ylim = c(0.05,0.25)) 
# or
autoplot(survfit(Surv(os_days,covidDeath) ~ sex, data = df_time), conf.int = 0.95, censor = FALSE) +
  labs(x = "Time (days)", y = "S(x)", colour="Gender", title = "Male vs Female KM Estimates") +
  guides(fill = FALSE) + 
  coord_cartesian(xlim = c(50,125),ylim = c(0.05,0.25))
  # roughly after 120 days estimated survival provs are the same
 

# vaccination status 
VAXg <- autoplot(survfit(Surv(os_days,covidDeath) ~ vaccine, data = df_time), conf.int = 0.95, censor = FALSE) +
  labs(x = "Time (days)", y = "S(x)", colour="Vaccine", title = "Vaccine KM Estimates") +
  guides(fill = FALSE) + 
  theme(legend.position = c(.95, .95),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6)) +
  scale_x_continuous(limits= c(0,100))
# result no vax is better. Most likely something to do with the data where 
# more people who were vaccinated had commorbidities?????

# crosscheck vaccine rates with commorbidities
check <- newdf %>% select(vaccine,Commorbidities)
table(check)
# vast majority of patients who were vaccinated did have commorbidities 
# thus km estimates not true for full population
checkdf <- cbind(df_time,newdf$Commorbidities)
# commorbidities true
checkdfT <- checkdf %>% filter(`newdf$Commorbidities`== TRUE)
# plot KM for vax
autoplot(survfit(Surv(os_days,covidDeath) ~ vaccine, data = checkdfT), conf.int = 0.95, censor = FALSE) +
  labs(x = "Time (days)", y = "S(x)", colour="Vaccine", title = "Vaccine for those with Commorbidities KM Estimates") +
  guides(fill = FALSE) + 
  theme(legend.position = c(.95, .95),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6)) +
  scale_x_continuous(limits= c(0,100))

# commorbidities false
checkdfF <- checkdf %>% filter(`newdf$Commorbidities`== FALSE)
autoplot(survfit(Surv(os_days,covidDeath) ~ vaccine, data = checkdfF), conf.int = 0.95, censor = FALSE) +
  labs(x = "Time (days)", y = "S(x)", colour="Vaccine", title = "Vaccine for those with no Commorbidities KM Estimates") +
  guides(fill = FALSE) + 
  theme(legend.position = c(.95, .95),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6)) +
  scale_x_continuous(limits= c(0,200))

# no clear evidence for my hypthesis. Maybe just because longer days in 
# hospital means more likely to die if vaccinated
# check lethatlity rate with and without vaccine.
# split the data by age above and below 60
ga <- newdf %>% group_by(age) %>% summarise(mean(vaccine))
# high vaccine rate for 75 and above 
sixfiveplus <- newdf %>% filter(age>65)
sfp <- autoplot(survfit(Surv(os_days,covidDeath) ~ vaccine, data = sixfiveplus), conf.int = 0.95, censor = FALSE) +
  labs(x = "Time (days)", y = "S(x)", colour="Vaccine", title = "Vaccine for those 65+ KM Estimates") +
  guides(fill = FALSE) + 
  theme(legend.position = c(.95, .95),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6)) +
  scale_x_continuous(limits= c(0,100))

#age
age65 <- newdf %>% mutate(above65 = 1*(age>65)+0*(age<=65),
                          above65 = as.logical(above65))
a65 <- autoplot(survfit(Surv(os_days,covidDeath) ~ above65, data = age65), conf.int = 0.95, censor = FALSE) +
  labs(x = "Time (days)", y = "S(x)", colour="Over 65", title = "65+ KM Estimates") +
  guides(fill = FALSE) + 
  theme(legend.position = c(.95, .95),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6)) +
  scale_x_continuous(limits= c(0,100))

a65_50 <- autoplot(survfit(Surv(os_days,covidDeath) ~ above65, data = age65), conf.int = 0.95, censor = FALSE) +
  labs(x = "Time (days)", y = "S(x)", colour="Over 65", title = "65+ KM Estimates") +
  guides(fill = FALSE) + 
  theme(legend.position = c(.95, .95),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6)) +
  scale_x_continuous(limits= c(0,50))

p_a65 <- ggsurvplot(survfit(Surv(os_days,covidDeath) ~ above65, data = age65), conf.int = TRUE, 
           legend.title = '', legend.labs = c('65 or younger', 'Older than 65'), legend = 'bottom',
           ggtheme = theme_bw(),
           xlim = c(0,100),
           break.x.by = 50,
           xlab = 'Number of Days',
           title = 'Kaplan-Meier Estimator by Elderly (>65)')

#icu 
ICUg <- autoplot(survfit(Surv(os_days,covidDeath) ~ icu, data = df_time), conf.int = 0.95, censor = FALSE) +
  labs(x = "Time (days)", y = "S(x)", colour="ICU", title = "ICU KM Estimates") +
  guides(fill = FALSE) + 
  theme(legend.position = c(.95, .95),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6)) +
  scale_x_continuous(limits= c(0,100))

# Fever
FEVg <- autoplot(survfit(Surv(os_days,covidDeath) ~ fever, data = df_time), conf.int = 0.95, censor = FALSE) +
  labs(x = "Time (days)", y = "S(x)", colour="Fever", title = "Fever KM Estimates") +
  guides(fill = FALSE) + 
  theme(legend.position = c(.95, .95),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6)) +
  scale_x_continuous(limits= c(0,100))

# Cough
COUg <- autoplot(survfit(Surv(os_days,covidDeath) ~ cough, data = df_time), conf.int = 0.95, censor = FALSE) +
  labs(x = "Time (days)", y = "S(x)", colour="Cough", title = "Cough KM Estimates") +
  guides(fill = FALSE) + 
  theme(legend.position = c(.95, .95),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6)) +
  scale_x_continuous(limits= c(0,100))

# Sore throat
STg <- autoplot(survfit(Surv(os_days,covidDeath) ~ sorethroat, data = df_time), conf.int = 0.95, censor = FALSE) +
  labs(x = "Time (days)", y = "S(x)", colour="Sore Throat", title = "Sore Throat KM Estimates") +
  guides(fill = FALSE) + 
  theme(legend.position = c(.95, .95),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6)) +
  scale_x_continuous(limits= c(0,100))

# dyspnoea
DYSg <- autoplot(survfit(Surv(os_days,covidDeath) ~ dyspnoea, data = df_time), conf.int = 0.95, censor = FALSE) +
  labs(x = "Time (days)", y = "S(x)", colour="Dsynoea", title = "Dsynoea KM Estimates") +
  guides(fill = FALSE) + 
  theme(legend.position = c(.95, .95),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6)) +
  scale_x_continuous(limits= c(0,100))

# Respiratory distress syndrome
RESg <- autoplot(survfit(Surv(os_days,covidDeath) ~ respdistress, data = df_time), conf.int = 0.95, censor = FALSE) +
  labs(x = "Time (days)", y = "S(x)", colour="Respiratory distress syndrome", title = "Respiratory Distress Syndrome KM Estimates") +
  guides(fill = FALSE) + 
  theme(legend.position = c(.95, .95),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6)) +
  scale_x_continuous(limits= c(0,100))

# oxygen saturation
OXYg <- autoplot(survfit(Surv(os_days,covidDeath) ~ oxygensat, data = df_time), conf.int = 0.95, censor = FALSE) +
  labs(x = "Time (days)", y = "S(x)", colour="Blood oxygen saturation < 95%", title = "Blood oxygen saturation < 95% KM Estimates") +
  guides(fill = FALSE) + 
  theme(legend.position = c(.95, .95),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6)) +
  scale_x_continuous(limits= c(0,100))

# Diarrhea
DIAg <- autoplot(survfit(Surv(os_days,covidDeath) ~ diarrhea, data = df_time), conf.int = 0.95, censor = FALSE) +
  labs(x = "Time (days)", y = "S(x)", colour="Diarrhea", title = "Diarrhea KM Estimates") +
  guides(fill = FALSE) + 
  theme(legend.position = c(.95, .95),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6)) +
  scale_x_continuous(limits= c(0,100))

# vomit
VOMg <- autoplot(survfit(Surv(os_days,covidDeath) ~ vomit, data = df_time), conf.int = 0.95, censor = FALSE) +
  labs(x = "Time (days)", y = "S(x)", colour="Vomit", title = "Vomit KM Estimates") +
  guides(fill = FALSE) + 
  theme(legend.position = c(.95, .95),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6)) +
  scale_x_continuous(limits= c(0,100))

# cardiovacular disease
CARg <- autoplot(survfit(Surv(os_days,covidDeath) ~ cardio, data = df_time), conf.int = 0.95, censor = FALSE) +
  labs(x = "Time (days)", y = "S(x)", colour="Cardiovascular Disease", title = "Cardiovascular Disease KM Estimates") +
  guides(fill = FALSE) + 
  theme(legend.position = c(.95, .95),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6)) +
  scale_x_continuous(limits= c(0,100))

# Hematologic diseases
HEMg <- autoplot(survfit(Surv(os_days,covidDeath) ~ hematologic, data = df_time), conf.int = 0.95, censor = FALSE) +
  labs(x = "Time (days)", y = "S(x)", colour="Hematologic Disease", title = "Hematologic Disease KM Estimates") +
  guides(fill = FALSE) + 
  theme(legend.position = c(.95, .95),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6)) +
  scale_x_continuous(limits= c(0,100))

# Down's syndrome
DOWg <- autoplot(survfit(Surv(os_days,covidDeath) ~ downsyn, data = df_time), conf.int = 0.95, censor = FALSE) +
  labs(x = "Time (days)", y = "S(x)", colour="Down's syndrome", title = "Down's syndrome KM Estimates") +
  guides(fill = FALSE) + 
  theme(legend.position = c(.95, .95),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6)) +
  scale_x_continuous(limits= c(0,100))

# Hepatic
HEPg <- autoplot(survfit(Surv(os_days,covidDeath) ~ hepatic, data = df_time), conf.int = 0.95, censor = FALSE) +
  labs(x = "Time (days)", y = "S(x)", colour="Any liver Diseases", title = "Any Liver Diseases KM Estimates") +
  guides(fill = FALSE) + 
  theme(legend.position = c(.95, .95),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6)) +
  scale_x_continuous(limits= c(0,100))

# Asthma
ASTg <- autoplot(survfit(Surv(os_days,covidDeath) ~ asthma, data = df_time), conf.int = 0.95, censor = FALSE) +
  labs(x = "Time (days)", y = "S(x)", colour="Asthma", title = "Asthma KM Estimates") +
  guides(fill = FALSE) + 
  theme(legend.position = c(.95, .95),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6)) +
  scale_x_continuous(limits= c(0,100))

# Diabetes
DIAg <- autoplot(survfit(Surv(os_days,covidDeath) ~ diabetes, data = df_time), conf.int = 0.95, censor = FALSE) +
  labs(x = "Time (days)", y = "S(x)", colour="Diabetes", title = "Diabetes KM Estimates") +
  guides(fill = FALSE) + 
  theme(legend.position = c(.95, .95),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6)) +
  scale_x_continuous(limits= c(0,100))

# Neurological
NEUg <- autoplot(survfit(Surv(os_days,covidDeath) ~ neurological, data = df_time), conf.int = 0.95, censor = FALSE) +
  labs(x = "Time (days)", y = "S(x)", colour="Any Neurological Diseases", title = "Any Neurological Diseases KM Estimates") +
  guides(fill = FALSE) + 
  theme(legend.position = c(.95, .95),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6)) +
  scale_x_continuous(limits= c(0,100))

#pneumopathy
PMEg <- autoplot(survfit(Surv(os_days,covidDeath) ~ pneumopathy, data = df_time), conf.int = 0.95, censor = FALSE) +
  labs(x = "Time (days)", y = "S(x)", colour="Pneumopathy", title = "Pneumopathy KM Estimates") +
  guides(fill = FALSE) + 
  theme(legend.position = c(.95, .95),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6)) +
  scale_x_continuous(limits= c(0,100))

#immuno
IMMg <- autoplot(survfit(Surv(os_days,covidDeath) ~ immuno, data = df_time), conf.int = 0.95, censor = FALSE) +
  labs(x = "Time (days)", y = "S(x)", colour="Any Immunodeficiencies", title = "Any Immunodeficiencies KM Estimates") +
  guides(fill = FALSE) + 
  theme(legend.position = c(.95, .95),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6)) +
  scale_x_continuous(limits= c(0,100))

# renal
RENg <- autoplot(survfit(Surv(os_days,covidDeath) ~ renal, data = df_time), conf.int = 0.95, censor = FALSE) +
  labs(x = "Time (days)", y = "S(x)", colour="Any Renal Diseases", title = "Any Renal Diseases KM Estimates") +
  guides(fill = FALSE) + 
  theme(legend.position = c(.95, .95),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6)) +
  scale_x_continuous(limits= c(0,100))

# obesity
OBEg <- autoplot(survfit(Surv(os_days,covidDeath) ~ obesity, data = df_time), conf.int = 0.95, censor = FALSE) +
  labs(x = "Time (days)", y = "S(x)", colour="Obesity", title = "Obesity KM Estimates") +
  guides(fill = FALSE) + 
  theme(legend.position = c(.95, .95),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6)) +
  scale_x_continuous(limits= c(0,100))

#symptoms
ggarrange(FEVg,COUg,STg,DYSg,RESg,OXYg,DIAg,VOMg,ncol=2,nrow=4)
ggarrange(CARg,HEMg,DOWg,HEPg,ASTg,DIAg,NEUg,PMEg,IMMg,RENg,ncol = 2,nrow = 5)
OBEg

#log rank and peto-peto test
survdiff(Surv(os_days,covidDeath) ~ sex, data = df_time, rho = 0)
survdiff(Surv(os_days,covidDeath) ~ sex, data = df_time, rho = 1)
survdiff(Surv(os_days,covidDeath) ~ young, data = bin, rho = 0)
survdiff(Surv(os_days,covidDeath) ~ young, data = bin, rho = 1)
survdiff(Surv(os_days,covidDeath) ~ mid, data = bin, rho = 0)
survdiff(Surv(os_days,covidDeath) ~ mid, data = bin, rho = 1)
survdiff(Surv(os_days,covidDeath) ~ old, data = bin, rho = 0)
survdiff(Surv(os_days,covidDeath) ~ old, data = bin, rho = 1)
survdiff(Surv(os_days,covidDeath) ~ icu, data = df_time, rho = 0)
survdiff(Surv(os_days,covidDeath) ~ icu, data = df_time, rho = 1)
survdiff(Surv(os_days,covidDeath) ~ vaccine, data = df_time, rho = 0)
survdiff(Surv(os_days,covidDeath) ~ vaccine, data = df_time, rho = 1)
survdiff(Surv(os_days,covidDeath) ~ fever, data = df_time, rho = 0)
survdiff(Surv(os_days,covidDeath) ~ fever, data = df_time, rho = 1)
survdiff(Surv(os_days,covidDeath) ~ cough, data = df_time, rho = 0)
survdiff(Surv(os_days,covidDeath) ~ cough, data = df_time, rho = 1)
survdiff(Surv(os_days,covidDeath) ~ sorethroat, data = df_time, rho = 0)
survdiff(Surv(os_days,covidDeath) ~ sorethroat, data = df_time, rho = 1)
survdiff(Surv(os_days,covidDeath) ~ dyspnoea, data = df_time, rho = 0)
survdiff(Surv(os_days,covidDeath) ~ dyspnoea, data = df_time, rho = 1)
survdiff(Surv(os_days,covidDeath) ~ respdistress, data = df_time, rho = 0)
survdiff(Surv(os_days,covidDeath) ~ respdistress, data = df_time, rho = 1)
survdiff(Surv(os_days,covidDeath) ~ oxygensat, data = df_time, rho = 0)
survdiff(Surv(os_days,covidDeath) ~ oxygensat, data = df_time, rho = 1)
survdiff(Surv(os_days,covidDeath) ~ diarrhea, data = df_time, rho = 0)
survdiff(Surv(os_days,covidDeath) ~ diarrhea, data = df_time, rho = 1)
survdiff(Surv(os_days,covidDeath) ~ vomit, data = df_time, rho = 0)
survdiff(Surv(os_days,covidDeath) ~ vomit, data = df_time, rho = 1)
survdiff(Surv(os_days,covidDeath) ~ cardio, data = df_time, rho = 0)
survdiff(Surv(os_days,covidDeath) ~ cardio, data = df_time, rho = 1)
survdiff(Surv(os_days,covidDeath) ~ hematologic, data = df_time, rho = 0)
survdiff(Surv(os_days,covidDeath) ~ hematologic, data = df_time, rho = 1)
survdiff(Surv(os_days,covidDeath) ~ downsyn, data = df_time, rho = 0)
survdiff(Surv(os_days,covidDeath) ~ downsyn, data = df_time, rho = 1)
survdiff(Surv(os_days,covidDeath) ~ hepatic, data = df_time, rho = 0)
survdiff(Surv(os_days,covidDeath) ~ hepatic, data = df_time, rho = 1)
survdiff(Surv(os_days,covidDeath) ~ asthma, data = df_time, rho = 0)
survdiff(Surv(os_days,covidDeath) ~ asthma, data = df_time, rho = 1)
survdiff(Surv(os_days,covidDeath) ~ diabetes, data = df_time, rho = 0)
survdiff(Surv(os_days,covidDeath) ~ diabetes, data = df_time, rho = 1)
survdiff(Surv(os_days,covidDeath) ~ neurological, data = df_time, rho = 0)
survdiff(Surv(os_days,covidDeath) ~ neurological, data = df_time, rho = 1)
survdiff(Surv(os_days,covidDeath) ~ pneumopathy, data = df_time, rho = 0)
survdiff(Surv(os_days,covidDeath) ~ pneumopathy, data = df_time, rho = 1)
survdiff(Surv(os_days,covidDeath) ~ immuno, data = df_time, rho = 0)
survdiff(Surv(os_days,covidDeath) ~ immuno, data = df_time, rho = 1)
survdiff(Surv(os_days,covidDeath) ~ renal, data = df_time, rho = 0)
survdiff(Surv(os_days,covidDeath) ~ renal, data = df_time, rho = 1)
survdiff(Surv(os_days,covidDeath) ~ obesity, data = df_time, rho = 0)
survdiff(Surv(os_days,covidDeath) ~ obesity, data = df_time, rho = 1)

#Cox Regression
bin <- df_time %>% 
  mutate(fever = as.numeric(fever),
         cough = as.numeric(cough),
         sorethroat = as.numeric(sorethroat),
         dyspnoea = as.numeric(dyspnoea),
         respdistress = as.numeric(respdistress),
         oxygensat = as.numeric(oxygensat),
         diarrhea = as.numeric(diarrhea),
         vomit = as.numeric(vomit),
         cardio = as.numeric(cardio),
         hematologic = as.numeric(hematologic),
         downsyn = as.numeric(downsyn),
         hepatic = as.numeric(hepatic),
         asthma = as.numeric(asthma),
         diabetes = as.numeric(diabetes),
         neurological = as.numeric(neurological),
         pneumopathy = as.numeric(pneumopathy),
         immuno = as.numeric(immuno),
         renal = as.numeric(renal),
         obesity = as.numeric(obesity),
         vaccine = as.numeric(vaccine),
         old = 1*(age>80) + 0*(age<=80), #>80
         mid = 1*(age<=80)&(age>60) + 0*(age<=60), #60<x<=80
         young = 1*(age<=60)&(age>40) + 0*(age<=40), #40<x<=60
         mid = as.numeric(mid),
         young = as.numeric(young),
         icu = as.numeric(icu)) 

censbin <- Surv(bin$os_days,bin$covidDeath)
attach(bin)
Cox.reg <- coxph(censbin ~ fever + cough + sorethroat + dyspnoea + respdistress
                 + oxygensat + diarrhea + vomit + cardio + hematologic + downsyn
                 + hepatic + asthma + diabetes + neurological + pneumopathy
                 + immuno + renal + obesity + vaccine + old + mid + young 
                 + sex + icu)
summary(Cox.reg)

ggforest(Cox.reg, data=bin)


#### Mulitstate Model -------------------------------------------------------
df_time3 <- df_time %>% 
  mutate(TH = as.numeric(difftime(dateAdmIcu,dateHosp,units="days"))) %>%
  mutate(TI = as.numeric(difftime(dateDisIcu,dateAdmIcu,units="days")))

# split data for age
#>80
old <- df_time3 %>% filter(age>80)
# calculate the transition rates
table(old$icu)
sum(old$icu) # n = 8926
sum(old$TH,na.rm=TRUE) # 15667

old1 <- old %>% mutate(D = as.numeric(difftime(dateAdmIcu,dateHosp,units="days")))
old_HI <- (sum(old$icu)-length(which(old1$D == 0)))/sum(old$TH,na.rm=TRUE)
old2 <- old %>% filter(icu==FALSE)
old_HD <- sum(old2$covidDeath)/sum(old$TH,na.rm=TRUE) 
old_HC <- length(old2$covidDeath[old2$covidDeath== FALSE])/sum(old$TH,na.rm=TRUE)

old3 <- old %>% filter(icu==TRUE) %>%
  mutate(I = as.numeric(difftime(dateEndObs,dateDisIcu,units="days")))
old_IH <- (length(old3$I) - length(which(old3$I==0)))/sum(old$TI,na.rm=TRUE)
old_ID <- sum(old3$covidDeath)/sum(old$TI,na.rm=TRUE)
old_IC <- length(old3$covidDeath[old3$covidDeath== FALSE])/sum(old$TI,na.rm=TRUE)

#60<x<=80
mid <- df_time3 %>% filter((age<=80)&(age>60))
mid1 <- mid %>% mutate(D = as.numeric(difftime(dateAdmIcu,dateHosp,units="days")))
mid_HI <- (sum(mid$icu)-length(which(mid1$D == 0)))/sum(mid$TH,na.rm=TRUE)
mid2 <- mid %>% filter(icu==FALSE)
mid_HD <- sum(mid2$covidDeath)/sum(mid$TH,na.rm=TRUE) 
mid_HC <- length(mid2$covidDeath[mid2$covidDeath== FALSE])/sum(mid$TH,na.rm=TRUE)

mid3 <- mid %>% filter(icu==TRUE) %>%
  mutate(I = as.numeric(difftime(dateEndObs,dateDisIcu,units="days")))
mid_IH <- (length(mid3$I) - length(which(mid3$I==0)))/sum(mid$TI,na.rm=TRUE)
mid_ID <- sum(mid3$covidDeath)/sum(mid$TI,na.rm=TRUE)
mid_IC <- length(mid3$covidDeath[mid3$covidDeath== FALSE])/sum(mid$TI,na.rm=TRUE)

#40<x<=60
young <- df_time3 %>% filter((age<=60)&(age>40))
young1 <- young %>% mutate(D = as.numeric(difftime(dateAdmIcu,dateHosp,units="days")))
young_HI <- (sum(young$icu)-length(which(young1$D == 0)))/sum(young$TH,na.rm=TRUE)
young2 <- young %>% filter(icu==FALSE)
young_HD <- sum(young2$covidDeath)/sum(young$TH,na.rm=TRUE) 
young_HC <- length(young2$covidDeath[young2$covidDeath== FALSE])/sum(young$TH,na.rm=TRUE)

young3 <- young %>% filter(icu==TRUE) %>%
  mutate(I = as.numeric(difftime(dateEndObs,dateDisIcu,units="days")))
young_IH <- (length(young3$I) - length(which(young3$I==0)))/sum(young$TI,na.rm=TRUE)
young_ID <- sum(young3$covidDeath)/sum(young$TI,na.rm=TRUE)
young_IC <- length(young3$covidDeath[young3$covidDeath== FALSE])/sum(young$TI,na.rm=TRUE)

#x<=40
baby <- df_time3 %>% filter(age<=40)
baby1 <- baby %>% mutate(D = as.numeric(difftime(dateAdmIcu,dateHosp,units="days")))
baby_HI <- (sum(baby$icu)-length(which(baby1$D == 0)))/sum(baby$TH,na.rm=TRUE)
baby2 <- baby %>% filter(icu==FALSE)
baby_HD <- sum(baby2$covidDeath)/sum(baby$TH,na.rm=TRUE) 
baby_HC <- length(baby2$covidDeath[baby2$covidDeath== FALSE])/sum(baby$TH,na.rm=TRUE)

baby3 <- baby %>% filter(icu==TRUE) %>%
  mutate(I = as.numeric(difftime(dateEndObs,dateDisIcu,units="days")))
baby_IH <- (length(baby3$I) - length(which(baby3$I==0)))/sum(baby$TI,na.rm=TRUE)
baby_ID <- sum(baby3$covidDeath)/sum(baby$TI,na.rm=TRUE)
baby_IC <- length(baby3$covidDeath[baby3$covidDeath== FALSE])/sum(baby$TI,na.rm=TRUE)

# all ages
groupage <- df_time3 %>% 
  mutate(D = as.numeric(difftime(dateAdmIcu,dateHosp,units="days"))) %>%
  mutate(I = as.numeric(difftime(dateEndObs,dateDisIcu,units="days"))) %>%
  group_by(age)


HI <- c()
HD <- c()
HC <- c()
IH <- c()
ID <- c()
IC <- c()
for (i in 0:114) {
  test <- df_time3 %>% filter(age==i)
  test1 <- test %>% mutate(D = as.numeric(difftime(dateAdmIcu,dateHosp,units="days")))
  test_HI <- (sum(test$icu)-length(which(test1$D == 0)))/sum(test$TH,na.rm=TRUE)
  HI[i+1] <- test_HI
  test2 <- test %>% filter(icu==FALSE)
  test_HD <- sum(test2$covidDeath)/sum(test$TH,na.rm=TRUE)
  HD[i+1] <- test_HD
  test_HC <- length(test2$covidDeath[test2$covidDeath== FALSE])/sum(test$TH,na.rm=TRUE)
  HC[i+1] <- test_HC
  
  test3 <- test %>% filter(icu==TRUE) %>%
    mutate(I = as.numeric(difftime(dateEndObs,dateDisIcu,units="days")))
  test_IH <- (length(test3$I) - length(which(test3$I==0)))/sum(test$TI,na.rm=TRUE)
  test_ID <- sum(test3$covidDeath)/sum(test$TI,na.rm=TRUE)
  test_IC <- length(test3$covidDeath[test3$covidDeath== FALSE])/sum(test$TI,na.rm=TRUE)
  IH[i+1] <- test_IH
  ID[i+1] <- test_ID
  IC[i+1] <- test_IC
}

age <- c(0:114)
allage <- cbind(age,HI,HD,HC,IH,ID,IC)
allage <- as.data.frame(allage)
allage <- allage[1:101,]

HIg <- ggplot(allage) + geom_point(aes(x=age,y=HI)) +
  labs(x= "Age", y="Transition rate", title ="Hospital to ICU" )
HDg <- ggplot(allage) + geom_point(aes(x=age,y=HD)) +
  labs(x= "Age", y="Transition rate", title ="Hospital to Death" )
HCg <- ggplot(allage) + geom_point(aes(x=age,y=HC)) +
  labs(x= "Age", y="Transition rate", title ="Hospital to Cure" )
IHg <- ggplot(allage) + geom_point(aes(x=age,y=IH)) +
  labs(x= "Age", y="Transition rate", title ="ICU to Hospital" )
IDg <- ggplot(allage) + geom_point(aes(x=age,y=ID)) +
  labs(x= "Age", y="Transition rate", title ="ICU to Death" )
ICg <- ggplot(allage) + geom_point(aes(x=age,y=IC)) +
  labs(x= "Age", y="Transition rate", title ="ICU to Cure" )

ggarrange(HIg,HDg,HCg,IHg,IDg,ICg,ncol=2,nrow=3)

ggplot(allage) + geom_point(aes(x=age,y=HD)) +
  labs(x= "Age", y="Transition rate", title ="Hospital to Death Transition Rate by Age")
