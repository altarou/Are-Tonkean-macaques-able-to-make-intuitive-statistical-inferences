########################################################################################################
########## R script for article "Are Tonkean macaques able to make intuitive statistical inferences"###
########################################################################################################
######################################################################################################## 
########################################################################################################

##### Packages needed #####

library(tidyverse) # version 2.0.0
library(ggplot2) # version 3.4.4      
library(lme4) # version 1.1-35.1      
library(dplyr) # version 1.1.4
library(tidyr) # version 1.3.0
library(rstatix) # version 0.7.2
library(gmodels) # version 2.18.1.1
library(lmerTest) # version 3.1-3     
library(readr) # version 2.1.4
library(cowplot) # version 1.1.1
library(car) # version 3.1-2          
library(MASS) # version 7.3-60
library(DHARMa) # version 0.4.6
library(corrplot) # version 0.92
library(performance) # version 0.13.0
library(see) # version 0.8.4
library(patchwork) # version 1.2.0
library(Rmisc) # version 1.5.1
library(glmulti) # version 1.0.8
library(rJava) # version 1.0-11 
library(ggpubr) # version 0.6.0 
library(Hmisc) # version 5.1-3
library(sjPlot)

# Write your directory here

setwd("Write your directory")
getwd()

##### Upload data #####
data <- read.csv2(file = "RawData_Test - Reviewed.csv", header = TRUE, sep = ';', dec = ',', stringsAsFactors = FALSE)

data$Park <- as.factor(data$Park)
data$ID_individual <- as.factor(data$ID_individual)
data$Age <- as.factor(data$Age)
data$Sex <- as.factor(data$Sex)
data$Phase <- as.factor(data$Phase)
data$Test_condition <- as.factor(data$Test_condition)
data$Session <- as.factor(data$Session)
data$Trial <- as.factor(data$Trial)
data$Experimenter_ID <- as.factor(data$Experimenter_ID)
data$Experimenter_position <- as.factor(data$Experimenter_position)
data$Arms_position <- as.factor(data$Arms_position)
data$Fav_Jar_position <- as.factor(data$Fav_Jar_position)
data$Side_Success <- as.factor(data$Side_Success)
data$Selected_side <- as.factor(data$Selected_side)
data$Selected_reward <- as.factor(data$Selected_reward)
data$Selected_side_binary <- as.numeric(data$Selected_side_binary)
data$Success <- as.numeric(data$Success)
data$ROR <- as.numeric(data$ROR)
data$RQP <- as.numeric(data$RQP)
data$RQB <- as.numeric(data$RQB)
data$Raisin <- as.factor(data$Raisin)

data$Test_condition <- fct_relevel(data$Test_condition, c("cond1","cond3","cond4","cond6","cond2a","cond5a","cond2b","cond5b"))
levels(data$Test_condition)
str(data)
summary(data)


########################
##### FIGURE 2 & 3 #####
########################

## Caculation of performance / individual / conditions
NbSuccess.Cond.Ind <- data %>%group_by(ID_individual,Test_condition) %>%
  dplyr::summarise(NbSucces=sum(Success),N=n()) %>%
  dplyr::mutate(Perf= NbSucces/N)

## Adding column Status  complete/incomplete at table with performance / condition / individual to be added in graphs
NbSuccess.Cond.Ind$Status <- "status"

for (i in 1:nrow(NbSuccess.Cond.Ind)){
  if (NbSuccess.Cond.Ind$N[i]>=20){
    NbSuccess.Cond.Ind$Status[i] <- "complete"
  }
  else(NbSuccess.Cond.Ind$Status[i]<- "incomplete")
}

## Creating a table containing n= total number of trials per conditions, to be written in graphs
TableTest <- NbSuccess.Cond.Ind %>%
  group_by(Test_condition)%>%
  dplyr::summarise(NbTrial = sum(N))%>%
  dplyr::mutate(NumberTrial = paste("n=", NbTrial))

## Graphic layout
Th <-theme(axis.title=element_text(12),axis.text.x=element_text(size=12),axis.text.y=element_text(size=15),legend.title=element_text(size=15),legend.text=element_text(size=15),plot.title = element_text(hjust=0.5,size=20))

########################
### Graph Figure 2 #####
########################

##Selecting only conditions that showed statistical inferences
NbSuccess.Cond.Test <- NbSuccess.Cond.Ind %>% filter(Test_condition=="cond1"|Test_condition=="cond3"|Test_condition=="cond4"|Test_condition=="cond6")
Table.Cond.Test <- TableTest %>% filter(Test_condition=="cond1"|Test_condition=="cond3"|Test_condition=="cond4"|Test_condition=="cond6")

x11()
Figure2 <- ggplot(NbSuccess.Cond.Test)+ geom_boxplot(aes(x=Test_condition, y=Perf),width = 0.5)+
  geom_jitter(aes(x=Test_condition, y=Perf, color=ID_individual, shape=Status),size=2.5,width=0.3,height=0)+
  geom_hline(yintercept=0.50, linetype="dashed", color="black", linewidth=0.7)+
  scale_shape_manual(values=c(19,1))+
  scale_color_manual(values = c("purple","blue","chocolate4","deepskyblue4","azure4","darkgreen","green","red","#ffcc00ff","aquamarine","#ff6600ff","chartreuse3","magenta"))+
  geom_label(data=Table.Cond.Test, aes(x=Test_condition, y=1.2,label=NumberTrial))+
  scale_y_continuous(name="Jar A choice rate", breaks=seq(from=0,to=1,by=0.25), limits=c(0,1.3))+
  scale_x_discrete(labels=c("1-Reversed proportions","3-Ctrl-preferred items quantity","4-Ctrl-non preferred items quantity","6-Ctrl-Opaque"))+
  ggtitle("Figure 2")+
  xlab("Conditions")+Th
print(Figure3)

# Saving graph
svg("Figure2.svg", width = 9, height = 9.5) 
print(Figure2)
dev.off()

########################
### Graph Figure 3 #####
########################

##Selecting only conditions that showed inferences based on preferred items quantities
NbSuccess.Cond.Ctrl <- NbSuccess.Cond.Ind %>% filter(Test_condition!="cond1"& Test_condition!="cond3"&Test_condition!="cond4"&Test_condition!="cond6")
Table.Cond.Ctrl <- TableTest %>% filter(Test_condition!="cond1"& Test_condition!="cond3"& Test_condition!="cond4"&Test_condition!="cond6")

x11()
Figure3 <- ggplot(NbSuccess.Cond.Ctrl)+ geom_boxplot(aes(x=Test_condition, y=Perf),width = 0.5)+
  geom_jitter(aes(x=Test_condition, y=Perf, color=ID_individual, shape=Status),size=2.5,width=0.3, height=0)+
  geom_hline(yintercept=0.50, linetype="dashed", color="black", linewidth=0.7)+
  scale_shape_manual(values=c(19,1))+
  scale_color_manual(values = c("purple","blue","chocolate4","deepskyblue4","azure4","darkgreen","green","red","#ffcc00ff","aquamarine","#ff6600ff","chartreuse3","magenta"))+
  geom_label(data=Table.Cond.Ctrl, aes(x=Test_condition, y=1.2,label=NumberTrial))+
  scale_y_continuous(name="Succes rate", breaks=seq(from=0,to=1,by=0.25), limits=c(0,1.3))+
  scale_x_discrete(labels=c("2a-Quantity-proportions decorrelated","5a-Quantity-Ctrl","2b-Quantity-proportions decorrelated","5b-Quantity-Ctrl"))+
  ggtitle("Figure 3")+
  xlab("Conditions")+ Th

print(Figure3)

# Saving graph
svg("Figure3.svg", width = 9, height = 9.5) 
print(Figure3)
dev.off()

#####################################################
##### Group analyses associated to Figure 2 & 3 #####
#####################################################

## Creating a table of performance / condition
Tab.Wilcx.Cond <- NbSuccess.Cond.Ind %>%
  group_by(Test_condition)%>%
  dplyr::summarise(SumSuccess= sum(NbSucces), SumN = sum(N))%>%
  dplyr::mutate(Perf=SumSuccess/SumN)

## two-tailed Wilcoxon signed rank-test comparing median results of each condition chance level mu = 0,5

Condition <- c("cond1", "cond3", "cond4", "cond6", "cond2a", "cond5a", "cond2b", "cond5b")
Tab.Wilcx.Cond$TestWilcoxon <- "p-value"
Tab.Wilcx.Cond$Vpara <- "V"

for (i in 1:length(Condition)){
  NbSuccess.Cond <- NbSuccess.Cond.Ind %>% filter(Test_condition==Condition[i])
  Test_Wilcoxon <- wilcox.test(NbSuccess.Cond$Perf, alternative='two.sided', mu=0.5, paired = FALSE)
  Tab.Wilcx.Cond$TestWilcoxon[i] <- Test_Wilcoxon[["p.value"]]
  Tab.Wilcx.Cond$Vpara[i] <- Test_Wilcoxon[[1]]
}

print(Tab.Wilcx.Cond)

## two-tailed paired Wilcoxon signed rank-test : comparing results between 2 conditions of interest
# switching table
TablePaired <- NbSuccess.Cond.Ind %>% 
  dplyr::select(ID_individual,Test_condition,Perf)%>% 
  pivot_wider(names_from = Test_condition, values_from = Perf)

## Condition 1 vs Condition 4
median(TablePaired$cond1 - TablePaired$cond4, na.rm=TRUE)
wilcox.test(TablePaired$cond1, TablePaired$cond4, alternative='two.sided', paired=TRUE)
wilcox.test(TablePaired$cond1, TablePaired$cond4, alternative='greater', paired=TRUE)

## Condition 1 vs Condition 3
median(TablePaired$cond1 - TablePaired$cond3, na.rm=TRUE)
wilcox.test(TablePaired$cond1, TablePaired$cond3, alternative='two.sided', paired=TRUE)
wilcox.test(TablePaired$cond1, TablePaired$cond3, alternative='greater', paired=TRUE)

## Condition 3 vs Condition 4
median(TablePaired$cond3 - TablePaired$cond4, na.rm=TRUE)
wilcox.test(TablePaired$cond3, TablePaired$cond4, alternative='two.sided', paired=TRUE)
wilcox.test(TablePaired$cond4, TablePaired$cond3, alternative='greater', paired=TRUE)


###################
##### Table 3 #####
###################

## Running different models for individual data
#List of models
model_formula <- list(
  glm1 = Selected_side_binary ~ scale(LogRQP) + scale(LogRQB) + scale(LogROR),
  glm2 = Selected_side_binary ~ scale(LogRQP) + scale(LogRQB),
  glm3 = Selected_side_binary ~ scale(LogRQP) + scale(LogROR),
  glm4 = Selected_side_binary ~ scale(LogRQB) + scale(LogROR),
  glm5 = Selected_side_binary ~ scale(LogRQP),
  glm6 = Selected_side_binary ~ scale(LogRQB),
  glm7 = Selected_side_binary ~ scale(LogROR)
)

#List of individuals
Individual <- c("Abricot","Alaryc","Barnabe","Eric","Ficelle","Horus","Nema","Nereis","Olli","Walt")
model_ind <- vector("list", length(Individual))
diff_model <- vector("list", length(model_formula))
results_all <- list()

#Extracting coefficient of interest for each model for each individual in tables
for (j in 1:length(model_formula)){
  model_ind <- vector("list", length(Individual))
  formula <- model_formula[[j]]
  model_name <- names(model_formula)[j]
  results_ind <- list()
  
  for (i in 1:length(Individual)){
    IndGLM <- dataGLM %>% filter(ID_individual==Individual[i])
    Modele.Full <- glm(formula, family = binomial, data = IndGLM)
    model_ind[[i]] <- Modele.Full
    coeff <- summary(Modele.Full)$coefficients
    coeff_df <- as.data.frame(coeff)
    results_ind[[Individual[i]]] <- coeff_df
    file_name <- paste0("coefficients_", Individual[i], "_", model_name, ".csv")
    write.table(coeff_df, file = file_name,sep=";", row.names = TRUE)
  }
  
  names(model_ind) <- Individual
  diff_model[[j]]<-model_ind
  results_all[[model_name]] <- results_ind
}

## Saving BIC for each model for each individual in one table
tabBIC <- data.frame()
for (i in 1:length(diff_model)){
  listBIC <- lapply(diff_model[[i]], BIC)
  names(listBIC) <- Individual
  listBIC <- as.data.frame(listBIC)
  tabBIC <- rbind(tabBIC, listBIC)
}
rownames(tabBIC)<- c("Model_Log_RQP_RQB_ROR","Model_Log_RQP_RQB","Model_Log_RQP_ROR","Model_Log_RQB_ROR","Model_Log_RQP","Model_Log_RQB","Model_Log_ROR" )
write.table(tabBIC,"tabBIC.csv",sep=";",row.names=TRUE)


####################
##### Figure S1 ####
####################

GrapeGroupe <- data %>%filter(ID_individual=="Eric"|ID_individual=="Ficelle"|ID_individual=="Horus"|ID_individual=="Olli"|ID_individual=="Walt")%>% 
  filter(Test_condition=="cond1"|Test_condition=="cond2a"|Test_condition=="cond3"|Test_condition=="cond4")%>%
  group_by(Test_condition)
GrapeGroupe$Success <- as.factor(GrapeGroupe$Success)

x11()
GrapheGrape <- ggplot(GrapeGroupe)+ geom_bar(aes(x=Raisin, fill=Success), position = "dodge")+facet_wrap(~Test_condition)+
  ggtitle("Performance in function of obtaining a grape per condition")+
  scale_x_discrete(labels=c("NO", "YES"))+
  theme_minimal()

svg("GrapheGrape.svg", width = 10, height = 10) 
print(GrapheGrape)
dev.off()

###################
##### Table S2 ####
###################

## Performance depending on whether a dried grape was given or not in this session in conditions of interest

GrapeProp <- data%>% filter(ID_individual=="Eric"|ID_individual=="Ficelle"|ID_individual=="Horus"|ID_individual=="Olli"|ID_individual=="Walt")%>%
  filter(Test_condition!="cond2b"&Test_condition!="cond6") %>%
  group_by(Test_condition,Raisin)%>%
  dplyr::summarise(TotSucces=sum(Success),N=n())%>%
  mutate(TotFail = N-TotSucces)%>%
  dplyr::select(Test_condition,Raisin,TotSucces, TotFail)

GrapeProp <- as.data.frame(GrapeProp)

# Chi-square test Condition 1
GrapeProp.Groupe.1 <- GrapeProp%>% filter(Test_condition=="cond1")%>% dplyr::select(TotSucces, TotFail)
chisq.test(GrapeProp.Groupe.1)

# Chi-square test Condition 2a
GrapeProp.Groupe.2a <- GrapeProp%>% filter(Test_condition=="cond2a")%>% dplyr::select(TotSucces, TotFail)
chisq.test(GrapeProp.Groupe.2a)

# Chi-square test Condition 3
GrapeProp.Groupe.3 <- GrapeProp%>% filter(Test_condition=="cond3")%>% dplyr::select(TotSucces, TotFail)
chisq.test(GrapeProp.Groupe.3)

# Chi-square test Condition 4
GrapeProp.Groupe.4 <- GrapeProp%>% filter(Test_condition=="cond4")%>% dplyr::select(TotSucces, TotFail)
chisq.test(GrapeProp.Groupe.4)

# Chi-square test Condition 5a
GrapeProp.Groupe.5a <- GrapeProp%>% filter(Test_condition=="cond5a")%>% dplyr::select(TotSucces, TotFail)
chisq.test(GrapeProp.Groupe.5a)

# Chi-square test Condition 5b
GrapeProp.Groupe.5b <- GrapeProp%>% filter(Test_condition=="cond5b")%>% dplyr::select(TotSucces, TotFail)
chisq.test(GrapeProp.Groupe.5b)

###################
##### Table S3 #####
###################
#Weber's law testing models analyses

## Creating new colum : WL.Diff.Peanut = Higher QP - Lower QP // with QP = peanuts quantity in one jar
## Creating new colum : WL.Ratio.Peanut = Lower QP / (QP jar A + QP jar B) // with QP = peanuts quantity in one jar

data$WL.Diff.Peanuts <- 0
data$WL.Ratio.Peanuts <- 0

for (i in 1:nrow(data)){
  if (data$Test_condition[i]=="cond1" | data$Test_condition[i]=="cond4"){
    data$WL.Diff.Peanuts[i] <- data$Total_peanuts_potA[i] - data$Total_peanuts_potB[i]
    data$WL.Ratio.Peanuts[i] <- data$Total_peanuts_potB[i] / (data$Total_peanuts_potA[i]+data$Total_peanuts_potB[i])
  }
  else if (data$Test_condition[i]!="cond1" & data$Test_condition[i]!="cond4"){
    data$WL.Diff.Peanuts[i] <- data$Total_peanuts_potB[i] - data$Total_peanuts_potA[i]
    data$WL.Ratio.Peanuts[i] <- data$Total_peanuts_potA[i] / (data$Total_peanuts_potB[i]+data$Total_peanuts_potA[i])
  }
}

## Creating new colum : MorePeanut.Selected = Choice for the highest QP (coded 1)

data$MorePeanut.Selected <- 0

for (i in 1:nrow(data)){
  if (data$Test_condition[i]=="cond1" | data$Test_condition[i]=="cond4"){
    if (data$Success[i]==1){
      data$MorePeanut.Selected [i] <- 1
    }
    else if (data$Success[i]==0){
      data$MorePeanut.Selected [i] <- 0
    }
  }
  else if (data$Test_condition[i]!="cond1" & data$Test_condition[i]!="cond4"){
    if (data$Success[i]==1){
      data$MorePeanut.Selected [i] <- 0
    }
    else if (data$Success[i]==0){
      data$MorePeanut.Selected [i] <- 1
    }
  }
}

data$MorePeanut.Selected <- as.factor(data$MorePeanut.Selected)

#check data
str(data)
unique(data$WL.Diff.Peanuts)
unique(data$WL.Ratio.Peanuts)

## Selection of data without cond6 (Ctrl-Opaque)
data.WL <- data %>% 
  filter(Test_condition!="cond6")
#check data
unique(data.WL$Test_condition)

#### Results model (1)

Null.WeberLaw <- glm(MorePeanut.Selected ~ 1, family = binomial, data = data.WL)
Model1 <- glm(MorePeanut.Selected  ~ WL.Diff.Peanuts, family = binomial, data = data.WL)
Anova(Model1)
summary(Model1)

#### Results model (2)

Null.WeberLaw <- glm(MorePeanut.Selected ~ 1, family = binomial, data = data.WL)
Model2 <- glm(MorePeanut.Selected  ~ WL.Ratio.Peanuts, family = binomial, data = data.WL)
Anova(Model2)
summary(Model2)

tab_model(Model1,Model2,transform = NULL,show.ci=F,show.se=T,show.re.var = F,  show.ngroups = F,show.r2=F,show.icc=F,show.aic =T, string.se ="SE",show.obs = F,string.est ="Estimates",
          show.stat = T,
          file = "Weber_law_table.xls")

####################
##### Table S4 #####
###################
#Model selection with group data to compare effect of ROR, RQP and RQB and experimental factors on jar's choice

## Selection of data without cond6 (Ctrl-Opaque)
dataGLM <- data %>% 
  filter(Test_condition!="cond6")
dataGLM$Selected_side_binary <- as.factor(dataGLM$Selected_side_binary)

#Adding columns log ROR, RQP, RQB
dataGLM <- dataGLM %>%
  mutate(LogROR = log10(ROR),
         LogRQP = log10(RQP),
         LogRQB = log10(RQB))
#Model (A)
Full.ExpeFactor <- lme4::glmer(Selected_side_binary ~ scale(LogRQP)+ scale(LogRQB)+scale(LogROR) + Arms_position + Fav_Jar_position +  Experimenter_ID + Session + (1|Experimenter_position)+ (1|ID_individual), family = binomial, data = dataGLM)
summary(Full.ExpeFactor)
vif(Full.ExpeFactor)
check_convergence(Full.ExpeFactor)
check_residuals(Full.ExpeFactor)

#Model (B)
RQP.ROR.ExpeFactor <- lme4::glmer(Selected_side_binary ~ scale(LogRQP)+ scale(LogROR) + Arms_position + Fav_Jar_position +  Experimenter_ID + Session + (1|Experimenter_position)+ (1|ID_individual), family = binomial, data = dataGLM)
summary(RQP.ROR.ExpeFactor)
vif(RQP.ROR.ExpeFactor)
check_convergence(RQP.ROR.ExpeFactor)
check_residuals(RQP.ROR.ExpeFactor)

#Model (C)
RQB.ROR.ExpeFactor<- lme4::glmer(Selected_side_binary ~ scale(LogRQB)+ scale(LogROR) + Arms_position + Fav_Jar_position +  Experimenter_ID + Session + (1|Experimenter_position)+ (1|ID_individual), family = binomial, data = dataGLM)
summary(RQB.ROR.ExpeFactor)
vif(RQB.ROR.ExpeFactor)
check_convergence(RQB.ROR.ExpeFactor)
check_residuals(RQB.ROR.ExpeFactor)

#Model (D)
RQP.RQB.ExpeFactor<- lme4::glmer(Selected_side_binary ~ scale(LogRQP)+ scale(LogRQB) + Arms_position + Fav_Jar_position +  Experimenter_ID + Session + (1|Experimenter_position)+ (1|ID_individual), family = binomial, data = dataGLM)
summary(RQP.RQB.ExpeFactor)
vif(RQP.RQB.ExpeFactor)
check_convergence(RQP.RQB.ExpeFactor)
check_residuals(RQP.RQB.ExpeFactor)

#Saving Table
tab_model(Full.ExpeFactor, RQP.ROR.ExpeFactor,RQB.ROR.ExpeFactor,RQP.RQB.ExpeFactor,transform = NULL,show.ci=F,show.se=T,show.re.var = F,  show.ngroups = F,show.r2=F,show.icc=F,show.aic =F, string.se ="SE",show.obs = F,string.est ="Estimates",
          show.stat = T,
          dv.labels = c("Model (A)", "Model (B)", "Model (C)", "Model (D)"),
          file = "TableS4.xls")


#####################
##### Figure S5 #####
#####################

## Mean performance per trial per condition of the group

Learning <- data %>% filter(Phase=="Test") %>%
  group_by(Test_condition,Trial) %>% 
  dplyr::summarise(NbSucces.Trial=sum(Success),N=n())%>%
  dplyr::mutate(Perf.Trial= NbSucces.Trial/N)%>%
  dplyr::mutate(RoundedPerf = round(Perf.Trial, digits =2))
Learning <- as.data.frame(Learning)
Learning$Trial <- as.integer(Learning$Trial)

str(Learning)

## Testing correlation between performance and trial number
# Test conditions 1, 3, 4 and 6
x11()
Learning.1.3.4.6 <- Learning %>% filter(Test_condition=="cond1"|Test_condition=="cond3"|Test_condition=="cond4"|Test_condition=="cond6")

GrapheLearning.1.3.4.6 <- ggplot(Learning.1.3.4.6, aes(x=Trial, y=Perf.Trial)) +
  facet_grid(~ Test_condition) +
  geom_point(shape=1) +    
  geom_smooth(method=lm,   
              se=FALSE, level = .95, colour = "#045a8d")+
  geom_smooth(colour="#bdc9e1", se=FALSE)+
  theme_bw() +
  labs(x = "Trial", y = "Performance") +
  expand_limits(y=c(0,1)) +
  scale_x_continuous(breaks=seq(1, 20, 1)) +
  scale_y_continuous(breaks=seq(0, 1, 0.1))+
  stat_cor(method = "pearson", label.x = 5, label.y = 1.1)

print(GrapheLearning.1.3.4.6)

# Saving graph
svg("GrapheLearning.1.3.4.6.svg", width = 15, height = 5) 
print(GrapheLearning.1.3.4.6)
dev.off()

# Control conditions 2a, 2b, 5a, 5b
Learning.2.5 <- Learning %>% filter(Test_condition=="cond2a"|Test_condition=="cond2b"|Test_condition=="cond5a"|Test_condition=="cond5b")
x11()
GrapheLearning.2.5 <- ggplot(Learning.2.5, aes(x=Trial, y=Perf.Trial)) +
  facet_grid(~ Test_condition) +
  geom_point(shape=1) +    
  geom_smooth(method=lm,   
              se=FALSE, level = .95, colour = "#045a8d")+
  geom_smooth(colour="#bdc9e1", se=FALSE)+
  theme_bw() +
  labs(x = "Trial", y = "Performance") +
  expand_limits(y=c(0,1)) +
  scale_x_continuous(breaks=seq(1, 20, 1)) +
  scale_y_continuous(breaks=seq(0, 1, 0.1))+
  stat_cor(method = "pearson", label.x = 5, label.y = 1.1)

print(GrapheLearning.2.5)

# Saving graph
svg("GrapheLearning.2.5.svg", width = 15, height = 5) 
print(GrapheLearning.2.5)
dev.off()


####################
##### TABLE S5 #####
###################
#Individual performances across conditions --> binomial analyses

## Caculation of performance / individual / conditions
NbSuccess.Cond.Ind <- data %>%group_by(ID_individual,Test_condition) %>%
  dplyr::summarise(NbSucces=sum(Success),N=n()) %>%
  dplyr::mutate(Perf= NbSucces/N)

## Creating table with p-value of binomial test for each individual / condition

NbSuccess.Cond.Ind$Pvalue_2sided <- "initialisation"
NbSuccess.Cond.Ind$Pvalue_greater <- "initialisation"
NbSuccess.Cond.Ind

for (i in 1:nrow(NbSuccess.Cond.Ind)){
  n <- NbSuccess.Cond.Ind$NbSucces[i]
  Tot <- NbSuccess.Cond.Ind$N[i]
  Test_Binom2sided <- binom.test(n,Tot,p=0.5,alternative="two.sided")
  Test_BinomGreater <- binom.test(n,Tot,p=0.5,alternative="greater")
  NbSuccess.Cond.Ind$Pvalue_2sided[i] <- Test_Binom2sided[["p.value"]]
  NbSuccess.Cond.Ind$Pvalue_greater[i] <- Test_BinomGreater[["p.value"]]
}
#check data
NbSuccess.Cond.Ind <- as.data.frame(NbSuccess.Cond.Ind)
str(NbSuccess.Cond.Ind)

### Results Table S5
write.table(NbSuccess.Cond.Ind,"Table S5.csv",sep=";",row.names=FALSE)


