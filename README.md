# ODIN-FOOD
##### ODIN INTERVENTION PAPER #####
##### Load Dataset #####
setwd("O:/Public/ODIN-projekt/Results/Data analyses/Scripts ODIN/Artikel 2 hovedartikel/Data som tekstfiler")

##### Textfile load #####
odin1 <- read.delim("260218.txt")

##### Excludion of participants #####
odin0 <- subset(odin1, Baseline.25OHD<125)# dropper outlier
odin <- subset(odin0, Ferie_47N == 2)

##### Exploring the dataset #####
#check variabel names
names(odin)
#check min, max, means, na's
summary(odin1)
summary(odin0)
summary(odin)
View(odin)
#check if factors are factors or correct
str(odin)
odin$Rygning <- as.factor(odin$Rygning)
odin$P_piller_1 <- as.factor(odin$P_piller_1)
odin$Alkohol <- as.factor(odin$Alkohol)
odin$Født_dk <- as.factor(odin$Født_dk)
odin$Aktiv_dagligdag_ikke.i.arb_1 <- as.factor(odin$Aktiv_dagligdag_ikke.i.arb_1)
odin$Aktiv_på_arb_1 <- as.factor(odin$Aktiv_på_arb_1)
odin$Aktiv_fritid_1 <- as.factor(odin$Aktiv_fritid_1)
odin$Arbejde_ude <- as.factor(odin$Arbejde_ude)
odin$Tid_ude_hverdag <- as.factor(odin$Tid_ude_hverdag)
odin$Tid_ude_weekend <- as.factor(odin$Tid_ude_weekend)
odin$Tid_sol <- as.factor(odin$Tid_sol)
odin$Beklæd_sommer <- as.factor(odin$Beklæd_sommer)
odin$Tørklæde <- as.factor(odin$Tørklæde)
odin$Let_beklæd_sommer <- as.factor(odin$Let_beklæd_sommer)
odin$Solcreme_hvor.ofte <- as.factor(odin$Solcreme_hvor.ofte)
odin$Solcreme_faktor <- as.factor(odin$Solcreme_faktor)
odin$Hudtype <- as.factor(odin$Hudtype)
odin$Ferie_sol_1 <- as.factor(odin$Ferie_sol_1)
odin$Ferie_sol_2 <- as.factor(odin$Ferie_sol_2)
str(odin$Ug.day.group)
f <- odin$Ug.day.group
as.numeric(levels(f))[f]# this one is a factor and I want to convert to numeric
str(odin$Ug.day.group)#kode ovenfor virkede ikke
str(odin)

#Age median, min, max
summary(odin$Alder_1us)
sd(odin$Alder_1us)
#Baseline vitamin D status summary
summary(odin$Baseline.25OHD)
#Boxplot of Baseline status among DK and Pakistan women
boxplot(Baseline.25OHD ~ Ethinicitet, data= odin, las = 1, ylab = "Baseline vitamin D status", col = 2:5, xaxt = "n")
axis(1 ,at = c(1,2), labels = c('DK', 'PA'))
#boxplot(Baseline.25OHD ~ Ethinicitet, data= odin)#uden farver
##### Variables tested for independence #####
#test af variables uafhængighed ved en Chi2 test.
library(MASS)
tbl1 = table(odin$Ethinicitet, odin$Tørklæde) 
tbl1 
chisq.test(tbl1) 
#P-værdien er lav, significant. Det betyder at de to variable er meget ens og dermed kan dække over hinanden i en model, vælg kun den ene i modellen

#Her testes Etnicitet op imod variablen tid_sol (vælger man skygge eller sol)
tbl2 = table(odin$Ethinicitet, odin$Tid_sol)
tbl2
chisq.test(tbl2)
#Igen er P-værdien er lav, significant. Tid_sol skal derfor nok ikke med i den fulde lineære model

#tester lige påklædningsvariablene:
tbl3 = table(odin$Ethinicitet, odin$Let_beklæd_sommer)
tbl3
chisq.test(tbl3, simulate.p.value = TRUE)
# Warning message:
#In chisq.test(tbl3) : Chi-squared approximation may be incorrect
#Ved ikke hvorfor den ikke virker her...

tbl4 = table(odin$Ethinicitet, odin$Beklæd_sommer)
tbl4
chisq.test(tbl4)
# Warning message:
#In chisq.test(tbl3) : Chi-squared approximation may be incorrect
#pga at nogle grupper har 0 som værdi

tbl5 = table(odin$Ethinicitet, odin$Tid_ude_weekend)
tbl5
chisq.test(tbl5)
#significant (warning pga 0 i nogle grupper)

tbl6 = table(odin$Ethinicitet, odin$Tid_ude_hverdag)
tbl6
chisq.test(tbl6)
#significant (warning pga 0 i nogle grupper)

tbl7 = table(odin$Ethinicitet, odin$Solcreme_hvor.ofte)
tbl7
chisq.test(tbl7)
# significante, variablene er for ens

#De der med warning kan istedet testes i en exact test.
fisher.test(Ethinicitet, Tid_ude_hverdag = NULL, workspace = 200000, hybrid = FALSE,
            control = list(), or = 1, alternative = "two.sided",
            conf.int = TRUE, conf.level = 0.95,
            simulate.p.value = FALSE, B = 2000)

##### Ddiff outcome variable #####
#ændringen i status: slut status-start status
odin$Ddiff <- odin$Followup.25OHD-odin$Baseline.25OHD

##### Intervention groups #####
#new variable for the four groups in the intervention - hvordan?
odin$intgroups[odin$Ethinicitet=="DK" & odin$kode=="OR"]<-1
odin$intgroups[odin$Ethinicitet=="DK" & odin$kode=="HV"]<-2
odin$intgroups[odin$Ethinicitet=="P" & odin$kode=="OR"]<-3
odin$intgroups[odin$Ethinicitet=="P" & odin$kode=="HV"]<-4
summary(odin$intgroups)
str(odin$intgroups)
odin$intgroups <- as.factor(odin$intgroups)
str(odin$intgroup)
#View(odin$Ddiff) irrterende kode fordi den åbner nyt vindue, men ok til at se på en variabel
summary(odin$Ddiff)
summary(odin$Ethinicitet)
#boxplot 4 grupper- viser beriget og placebo for sig og etnicitet i hver sin:
boxplot(Ddiff ~ kode * Ethinicitet, data=odin, las = 1, ylab = "Change in vitamin D status", col = 2:5, xaxt = "n")
#labels på de 4 grupper
axis(1 ,at = c(1,2), labels = c('Danish Placebo','Danish Fortified')) 
axis(1 ,at = c(3,4), labels = c('Pakistani Placebo','Pakistani Fortified'))

#make dateset for placebo and fortified groups regardless of ethnicity
odinfort <- subset(odin, kode =="OR") #n=64 (efter outlier og rejsende er fjernet)
odinplac <- subset(odin, kode =="HV") #n=72
#make new datasets with Danish and Pakistani in each using total dataset odin
odindan <- subset(odin, Ethinicitet =="DK") #n=66
odinpak <- subset(odin, Ethinicitet =="P") #n=70 (efter outlier fjernet)

#4 groups: intervention groups
odinfortdk <- subset(odinfort, Ethinicitet =="DK") #(n=34)ny:31
odinfortP <- subset(odinfort, Ethinicitet =="P") #(n=34)ny: 33
odinplacdk <- subset(odinplac, Ethinicitet =="DK") #(n=37)ny: 35
odinplacP <- subset(odinplac, Ethinicitet =="P") #(n=37)ny: 37

#the decrease of the placebo individuals
summary(odinplac$Ddiff)
sd(odinplac$Ddiff, na.rm=TRUE)

##### NORMAL distribution #####
#Normal distribution
qqnorm(odin$Alder_1us)
qqline(odin$Alder_1us)#Not norm distr
qqnorm(odin$Vægt_Baseline)
qqline(odin$Vægt_Baseline)#norm distr
qqnorm(odin$BMI_1us)
qqline(odin$BMI_1us)#norm distr
qqnorm(odin$Fedt._1us)
qqline(odin$Fedt._1us)#norm distr
qqnorm(odin$Baseline.25OHD)
qqline(odin$Baseline.25OHD)#norm distr
qqnorm(odin$Followup.25OHD)
qqline(odin$Followup.25OHD)
qqnorm(odin$PTH.Baseline..pmol.L.)
qqline(odin$PTH.Baseline..pmol.L.)#normal
qqnorm(odin$PTH.Followup..pmol.L.)
qqline(odin$PTH.Followup..pmol.L.)#normal
qqnorm(odin$D.vitamin.indtag)
qqline(odin$D.vitamin.indtag)#not normal
qqnorm(odin$Ug.day)
qqline(odin$Ug.day)#not normal

#test of the non normally distributed in the two ethnicgroups (age, D-vit indtag, compliance)
kruskal.test(odin$Ethinicitet, odin$Alder_1us)#no diff in age between the paki and dk
kruskal.test(odin$Ethinicitet, odin$D.vitamin.indtag)#no diff
kruskal.test(odin$Ethinicitet, odin$Ug.day)#no difference

##### Drop-outs #####
#test if the drop outs were different in the two ethnic groups
#New variable for continued intervention or discontinued
summary(odin$Only.baseline.measures)
str(odin$Only.baseline.measures)
odin$Only.baseline.measures <- as.factor(odin$Only.baseline.measures)

#Pearson's Chi2 test
tbl21 = table(odin$Ethinicitet, odin$Only.baseline.measures)
tbl21
chisq.test(tbl21)# No significant diff in drop outs between ethnicities, P=0.67

##### TABLES #####

##### Baseline table 2 #####
#DK
summary(odindan)
sd(odindan$Alder_1us)
sd(odindan$Vægt_Baseline)
summary(odindan$BMI_1us)
sd(odindan$BMI_1us, na.rm=TRUE)
sd(odindan$Fedt._1us, na.rm=TRUE)
summary(odindan$Baseline.25OHD)
sd(odindan$Baseline.25OHD)
sd(odindan$PTH.Baseline..pmol.L.)
summary(odindan$Ug.day.group)
sd(odindan$Ug.day.group, na.rm=TRUE)
summary(odindan$D.vitamin.indtag)
summary(odindan$Tørklæde)
summary(odindan$Calcium.indtag)

summary(odindan$D.kalk.tilskud)
summary(odindan$Multivitamin.tilskud)
summary(odindan$Rygning)
summary(odindan$Alkohol)
summary(odindan$Tørklæde)

#Pakistani (table 2)
summary(odinpak)
summary(odinpak$Født_dk)
sd(odinpak$Alder_1us)
summary(odinpak$Vægt_Baseline)
sd(odinpak$Vægt_Baseline)
summary(odinpak$BMI_1us)
sd(odinpak$BMI_1us, na.rm=TRUE)
summary(odinpak$Fedt._1us)
sd(odinpak$Fedt._1us, na.rm=TRUE)
summary(odinpak$Baseline.25OHD)
sd(odinpak$Baseline.25OHD)

summary(odinpak$PTH.Baseline..pmol.L.)
sd(odinpak$PTH.Baseline..pmol.L.)
summary(odinpak$D.vitamin.indtag)
summary(odinpak$D.kalk.tilskud)
summary(odinpak$Multivitamin.tilskud)
summary(odinpak$Rygning)
summary(odinpak$Alkohol)
summary(odinpak$Tørklæde)
summary(odinpak$Calcium.indtag)

summary(odinpak)
sd(odinpak$Alder_1us)
sd(odinpak$Vægt_Baseline)
sd(odinpak$BMI_1us, na.rm=TRUE)
sd(odinpak$Fedt._1us, na.rm=TRUE)
sd(odinpak$Baseline.25OHD)
sd(odinpak$PTH.Baseline..pmol.L.)
sd(odinpak$D.vitamin.indtag)
sd(odinpak$Ug.day, na.rm=TRUE)

##### Table 3 intervention effect #####

#ttest for diff in the followup groups and a plot, de er forskellige
t.test(odinfortdk$Followup.25OHD, odinfortP$Followup.25OHD)#sign diff
#table 3 øverste del - Baseline
summary(odinplacdk$Baseline.25OHD)
sd(odinplacdk$Baseline.25OHD, na.rm=TRUE)
summary(odinfortdk$Baseline.25OHD)
sd(odinfortdk$Baseline.25OHD, na.rm=TRUE)
summary(odinplacP$Baseline.25OHD)
sd(odinplacP$Baseline.25OHD, na.rm=TRUE)
summary(odinfortP$Baseline.25OHD)
sd(odinfortP$Baseline.25OHD, na.rm=TRUE)

#Follow-up
summary(odinplacdk$Followup.25OHD)
sd(odinplacdk$Followup.25OHD, na.rm=TRUE)
summary(odinfortdk$Followup.25OHD)
sd(odinfortdk$Followup.25OHD, na.rm=TRUE)
summary(odinplacP$Followup.25OHD)
sd(odinplacP$Followup.25OHD, na.rm=TRUE)
summary(odinfortP$Followup.25OHD)
sd(odinfortP$Followup.25OHD, na.rm=TRUE)

summary(odinplacP$Followup.25OHD)
sd(odinplacP$Followup.25OHD, na.rm=TRUE)
summary(odinfortP$Followup.25OHD)
sd(odinfortP$Followup.25OHD, na.rm=TRUE)
summary(odinpak$Followup.25OHD)
sd(odinpak$Followup.25OHD, na.rm=TRUE)


##### test of diff in baseline characteristics #####
#test of the non normally distributed in the two ethnicgroups (age, D-vit indtag, compliance)
kruskal.test(odin$Ethinicitet, odin$Alder_1us)#no diff in age between the paki and dk
kruskal.test(odin$Ethinicitet, odin$D.vitamin.indtag)#no diff
kruskal.test(odin$Ethinicitet, odin$Ug.day)#no difference

#T-tests for baseline tables:
t.test(odindan$Vægt_Baseline,odinpak$Vægt_Baseline)#no diff
t.test(odindan$BMI_1us,odinpak$BMI_1us)#sign diff
t.test(odindan$Fedt._1us,odinpak$Fedt._1us)#sign diff
t.test(odindan$Baseline.25OHD,odinpak$Baseline.25OHD)#No diff
t.test(odindan$PTH.Baseline..pmol.L.,odinpak$PTH.Baseline..pmol.L.)#sign diff
#Ttest for diff in baseline status between the two fortified groups
t.test(odinfortdk$Baseline.25OHD, odinfortP$Baseline.25OHD)#no diff
#Ttest for diff in baseline vitD between the two placebo groups
t.test(odinplacdk$Baseline.25OHD, odinplacP$Baseline.25OHD)#no diff
#test of difference in the eaten vitD from the 4 foods =compliance (non norm distr)
kruskal.test(odinfort$Ethinicitet, odinfort$Ug.day)#no diff in compliance intake between DK and P

#change in the placebo groups after intervention
t.test(odinplacdk$Baseline.25OHD, odinplacdk$Followup.25OHD)#no diff
t.test(odinplacP$Baseline.25OHD, odinplacP$Followup.25OHD)#diff
#test of group differences of the categorical variables
tbl20 = table(odin$Ethinicitet, odin$Rygning)
tbl20
chisq.test(tbl20)# smoking er ikke sign forskellig

tbl21 = table(odin$Ethinicitet, odin$Født_dk)
tbl21
chisq.test(tbl21, simulate.p.value = TRUE)# burde give forskel, men kan muligvis ikke testes da den ene gruppe har 0

tbl22 = table(odin$Ethinicitet, odin$Alkohol)
tbl22
chisq.test(tbl22, simulate.p.value = TRUE)

tbl23 = table(odin$Ethinicitet, odin$Tørklæde)
tbl23
chisq.test(tbl23, simulate.p.value = TRUE)

tbl23 = table(odin$Ethinicitet, odin$D.kalk.tilskud)
tbl23
chisq.test(tbl23, simulate.p.value = TRUE)

tbl23 = table(odin$Ethinicitet, odin$Multivitamin.tilskud)
tbl23
chisq.test(tbl23, simulate.p.value = TRUE)#ikke sign


##### BMI/vægt changes baseline vs endpoint ######

summary(odindan$Vægt_Baseline)
summary(odinpak$Vægt_Baseline)
summary(odindan$Vægt_Followup)
summary(odinpak$Vægt_Followup)

#Ttest for diff in baseline status and foollow up - vægt
t.test(odindan$Vægt_Followup, odindan$Vægt_Baseline)#no diff

##### PTH #####
# PTH changes from baseline to follow-up
summary(odinplacdk$PTH.Baseline..pmol.L.)
summary(odinplacdk$PTH.Followup..pmol.L.)
t.test(odinplacdk$PTH.Followup, odinplacdk$PTH.Baseline)#no diff

summary(odinplacP$PTH.Baseline..pmol.L.)
summary(odinplacP$PTH.Followup..pmol.L.)
t.test(odinplacP$PTH.Followup, odinplacP$PTH.Baseline)#no diff
#correlation between PTH and 25(OH)D
cor.test(odin$Baseline.25OHD,odin$PTH.Baseline..pmol.L.,method = "pearson")
cor.test(odin$Followup.25OHD,odin$PTH.Followup..pmol.L.,method = "pearson")#hvad giver mening at se på, baseline eller followup?
cor.test(odindan$Baseline.25OHD,odindan$PTH.Baseline..pmol.L.,method = "pearson")
cor.test(odinpak$Baseline.25OHD,odinpak$PTH.Baseline..pmol.L.,method = "pearson")
#graf for correlationen mellem PTH og 25(OH)D
plot(odinpak$PTH.Baseline..pmol.L.~odinpak$Baseline.25OHD,
     xlab="Baseline 25(OH)D (nmol/L)", ylab="Baseline serum PTH (pmol/L)")
lines(lowess(odinpak$Baseline.25OHD,odinpak$PTH.Baseline..pmol.L.))

##### Compliance different between DK and PA #####

#Ttest for diff in baseline status and foollow up - vægt
t.test(odindan$Compliance, odinpak$Compliance)#Significant Forskel

# PTH changes from baseline to follow-up
summary(odinplacdk$PTH.Baseline..pmol.L.)
summary(odinplacdk$PTH.Followup..pmol.L.)
t.test(odinplacdk$PTH.Followup, odinplacdk$PTH.Baseline)#no diff

summary(odinplacP$PTH.Baseline..pmol.L.)
summary(odinplacP$PTH.Followup..pmol.L.)
t.test(odinplacP$PTH.Followup, odinplacP$PTH.Baseline)#no diff
#correlation between PTH and 25(OH)D
cor.test(odin$Baseline.25OHD,odin$PTH.Baseline..pmol.L.,method = "pearson")
cor.test(odin$Followup.25OHD,odin$PTH.Followup..pmol.L.,method = "pearson")#hvad giver mening at se på, baseline eller followup?
cor.test(odindan$Baseline.25OHD,odindan$PTH.Baseline..pmol.L.,method = "pearson")
cor.test(odinpak$Baseline.25OHD,odinpak$PTH.Baseline..pmol.L.,method = "pearson")
#graf for correlationen mellem PTH og 25(OH)D
plot(odinpak$PTH.Baseline..pmol.L.~odinpak$Baseline.25OHD,
     xlab="Baseline 25(OH)D (nmol/L)", ylab="Baseline serum PTH (pmol/L)")
lines(lowess(odinpak$Baseline.25OHD,odinpak$PTH.Baseline..pmol.L.))


##### Below 10, 30, 50 nmol/L #####
#How many below 10, 25 and 50 nmol/L for table
#baseline
odin$statusgrBase[odin$Baseline.25OHD<10]<-1
odin$statusgrBase[odin$Baseline.25OHD>=10 & odin$Baseline.25OHD<30]<-2
odin$statusgrBase[odin$Baseline.25OHD>=30 & odin$Baseline.25OHD<50]<-3
odin$statusgrBase[odin$Baseline.25OHD>=50]<-4
tbl1 = table(odin$intgroups, odin$statusgr) 
tbl1
odin$statusgrBase <- as.factor(odin$statusgrBase)
str(odin$statusgrBase)
summary(odin$statusgrBase)
#followup status
odin$statusgrFollow[odin$Followup.25OHD<10]<-1
odin$statusgrFollow[odin$Followup.25OHD>=10 & odin$Followup.25OHD<30]<-2
odin$statusgrFollow[odin$Followup.25OHD>=30 & odin$Followup.25OHD<50]<-3
odin$statusgrFollow[odin$Followup.25OHD>=50]<-4
tbl2 = table(odin$intgroups, odin$statusgrFollow) 
tbl2

##### binær variabel >< 30 nmol/L at follow-up #####
odin$cut30FU[odin$Followup.25OHD>=30]<-1
odin$cut30FU[odin$Followup.25OHD<30]<-0
str(odin$cut30FU)
odin$cut30FU <- as.factor(odin$cut30FU)
summary(odin$cut30FU)

summary(odin$cut30B)
odin$cut30E[odin$Followup.25OHD<30]
odin$cut50B[odin$Baseline.25OHD<50]
odin$cut50E[odin$Followup.25OHD<50]

##### Logistist regression #####
#brug den binære variabel summary(odin$cut30FU)
#0=<30 og 1=>=30
#EXP af estimate værdi for at få odds ratio

model49 <- glm(cut30FU ~ Baseline.25OHD + Ethinicitet + kode, family=binomial,data=odin)
summary(model49)
confint(model49)
exp(exp(-0.91241)#=0.4015 pakistani ethnicity
exp(4.63239)#=102.7594 study group OR
exp(0.13382)#=1.1431 Baseline vitD status

#gammel test af det samme! vælg hvilken dur

model83<-glm(cut50~Baseline.25OHD + Ethinicitet + kode + BMI_1us + Fedt._1us + 
               PTH.Baseline..pmol.L.+ Ethinicitet:kode, data = odin,family = binomial(link = logit))
summary(model83)
model84<-glm(cut50~Baseline.25OHD + Ethinicitet + kode + BMI_1us + Fedt._1us + 
               PTH.Baseline..pmol.L., data = odin,family = binomial(link = logit))
summary(model84)
exp(4.17614)
exp(-1.44396)
#% istedet for counts (ser ikke helt korrekt ud i output)
(prop.table(table(odin$intgroups, odin$statusgrFollow))*10)
chisq.test(tbl1, simulate.p.value = TRUE)
summary(odin$Followup.25OHD)# minimum D status = 11 nmol/L derfor er gr 1 ikke vist i tabellen (Table 1 ovenfor)
##### INTERVENTION EFFECT #####
#change in the placebo groups after intervention
t.test(odinplacdk$Baseline.25OHD, odinplacdk$Followup.25OHD)#no diff
t.test(odinplacP$Baseline.25OHD, odinplacP$Followup.25OHD)# sign decrease
t.test(odinplac$Baseline.25OHD, odinplac$Followup.25OHD)
##### Tukey HSD #####
TukeyHSD(aov(Followup.25OHD ~ intgroups, data = odin))#mellem de 4 grupper med P-value
summary(odin$intgroups)

##### ANOVA #####

##### ANCOVA #####
#DENNE MODEL BRUGES - ANCOVA (Model 1 i ODIN deliverrable report):
Model34 <- lm(Ddiff ~ Baseline.25OHD + intgroups + PTH.Baseline..pmol.L. +
                BMI_1us + Fedt._1us, data = odin)
summary(Model34)
##### Rate constants #####
#rate constants calculated
summary(odinfortdk$Baseline.25OHD)
summary(odinfortP$Baseline.25OHD)
summary(odinfortdk$Followup.25OHD)
summary(odinfortP$Followup.25OHD)
sd(odinfortdk$Followup.25OHD, na.rm=TRUE)
sd(odinfortP$Followup.25OHD, na.rm=TRUE)

#####MODEL 1 AND 2 IN PAPER 2 11.01.18#####

#MODEL1 #
Model86 <- lm(Ddiff ~ Baseline.25OHD + Ethinicitet + kode  
              + Ethinicitet:kode, data = odin)
summary(Model86)
confint(Model86)
#MODEL 2
Model87 <- lm(Ddiff ~ Baseline.25OHD + Ethinicitet + kode + Compliance + PTH.Baseline..pmol.L. + BMI_1us  
              + Ethinicitet:kode, data = odin)
summary(Model87)
confint(Model87)
##### Fodnote 1 beregning #####
#til beregning af forskellen mellem en deltager i paki og DK gr (plus CI)
#Snyde r til at spytte referencegruppen ud i output ved at fjerne en variabel
Model86B <- lm(Ddiff ~ Baseline.25OHD + kode 
               + Ethinicitet:kode, data = odin)
summary(Model86B)
confint(Model86B)
#snyde modellen så den spytter referencegruppen ud for 
Model86c <- lm(Ddiff ~ Baseline.25OHD + Ethinicitet 
               + Ethinicitet:kode, data = odin)
summary(Model86c)
confint(Model86c)

##### MODEL TESTS #####
leveneTest(Model86) # Denne test virker ikke når man har to variable inde i modellen. Derfor er det kun wallyplot og norm ford.
qqwrap <- function(x,y, ...) {qqnorm(y, main="",...); abline(a=0, b=1)}
wallyplot(Model86, FUN=qqwrap)
wallyplot(Model86)

##### SJ PLOT af Model 1 #####
#plots af effekterne i de 4 grupper (model1)
#install.packages('sjPlot', dependencies = TRUE)
#install.packages('sjlabelled', dependencies = TRUE)
#install.packages('sjmisc', dependencies = TRUE)
#install.packages('ggplot2', dependencies = TRUE)
#install.packages('colorspace', dependencies = TRUE)
#install.packages('lazyeval', dependencies = TRUE)
#install.packages('arm', dependencies = TRUE)
#install.packages('nloptr', dependencies = TRUE)
#install.packages('broom', dependencies = TRUE)
#install.packages('dplyr', dependencies = TRUE)
#install.packages('purrr', dependencies = TRUE)
#install.packages('mnormt', dependencies = TRUE)
#install.packages('survey', dependencies = TRUE)
#install.packages('ggeffects', dependencies = TRUE)
#install.packages('prediction', dependencies = TRUE)
#install.packages('haven', dependencies = TRUE)
#install.packages('snakecase', dependencies = TRUE)
#install.packages('stringdist', dependencies = TRUE)
#install.packages('bayesplot', dependencies = TRUE)
#install.packages('glmmTMB', dependencies = TRUE)
#install.packages('htmlwidgets', dependencies = TRUE)
#install.packages('httpuv', dependencies = TRUE)
library(sjPlot)
library(sjlabelled)
library(sjmisc)
library(ggplot2)
data(odin)

##### FIG 3 PLOT in Paper 2 #####

fit <- lm(Ddiff ~ Baseline.25OHD + Ethinicitet 
          + Ethinicitet:kode, data = odin)
plot_model(fit, "est",vline.color = "blue",colors ="blue", width =  0.3, sort.est = TRUE, 
           transform = NULL, show.values = TRUE, value.offset = .3, 
           title = "Change in vitamin D status", 
           terms = c("EthinicitetP:kodeOR", "EthinicitetDK:kodeOR"))
summary(fit)
