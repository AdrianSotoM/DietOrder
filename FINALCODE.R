################################################################################
################################ SET UP ########################################
################################################################################
##### Working Directory #####
# Working directory setup. Of course, this will be a different path for you.
setwd("C:/Users/L03512880/Dropbox/UIEM/LEAD/Proyectos/Dietorder")

##### Packages ##### 
# Packages setup 
pacman::p_load(dplyr,tidyr,ggstatsplot,readxl,tableone,easystats,ggplot2, 
               patchwork,MASS,see,performance,conflicted,haven) 

# Solving duplicate functions conflicts 
conflict_prefer("select","dplyr") 
conflict_prefer("filter", "dplyr") 
conflict_prefer("mutate","dplyr") 

###### Data Upload ##### 
# Datasets were obtained from the publicly available repository at: 
# https://osf.io/fjykq/ 
baseline <- read_sas("Data/baseline.sas7bdat",NULL) 
deltabc <- read_sas("Data/deltabc.sas7bdat",NULL) 
chamberhip <- read_sas("Data/chamberhip.sas7bdat",NULL) 
eatingratedaily <- read_sas("Data/eatingratedaily.sas7bdat",NULL) 
hungrysatiety <- read_sas("Data/hungrysatiety.sas7bdat",NULL) 
insulinindex <- read_sas("Data/insulinindex.sas7bdat",NULL)  
ketoneblood <- read_sas("Data/ketoneblood.sas7bdat",NULL) 
labblood <- read_sas("Data/labblood.sas7bdat",NULL) 
metaboprofile <- read_sas("Data/metaboprofile.sas7bdat",NULL) 
enint <- read_sas("Data/foodsnackbydiet.sas7bdat",NULL) 
foodsnackday <- read_sas("Data/foodsnackday.sas7bdat",NULL) 

################################################################################
############################# TABLE 1 ##########################################
# Baseline Demographics, calculated using publicly available data (missing # 
# data from four participants due to lack of consent for general release) #
##### Table Data #####
sum(baseline$Sex=="Female") 

mean(baseline$Age) 
sd(baseline$Age) 

mean(baseline$Height) 
sd(baseline$Height) 

mean(baseline$Weight) 
sd(baseline$Weight) 

mean(baseline$BMI) 
sd(baseline$BMI) 

mean(baseline$FM) 
sd(baseline$FM) 

mean(baseline$FatContent) 
sd(baseline$FatContent) 

mean(baseline$REE) 
sd(baseline$REE) 

################################################################################
######################### CARRYOVER EFFECT ANALYSES ############################ 
################################################################################
###### Which participants started with which diet? ###### 

LCDfirst <- baseline %>% filter(FirstDiet=="LC") #n = 10 
unique(LCDfirst$SubjectID) 

LFDfirst <- baseline %>% filter(FirstDiet=="LF") #n = 6 
unique(LFDfirst$SubjectID) 

### Creating a list to subset future datasets by diet order 
# Low-carb first 
lcf <- c("LCLF002", "LCLF003", "LCLF005", "LCLF006", "LCLF010", "LCLF013", 
        "LCLF014", "LCLF021", "LCLF022", "LCLF023") 

# Low-fat first 
lff <- c("LCLF001","LCLF009","LCLF015","LCLF017","LCLF018","LCLF020") 
enint2 <- enint %>% left_join(baseline, by = "SubjectID") %>% 
  select(SubjectID, Age, Sex, BMI, REE, Diet,FirstDiet,AveEI) %>%  
  mutate(Time=case_when(FirstDiet=="LC"& Diet=="LC"~"period1", 
                        FirstDiet=="LC"& Diet=="LF"~"period2", 
                        FirstDiet=="LF"& Diet=="LF"~"period1", 
                        FirstDiet=="LF"& Diet=="LC"~"period2")) %>% 
  mutate(periodversion=case_when(FirstDiet=="LC"& Diet=="LC"~"LCfirst", 
                                 FirstDiet=="LC"& Diet=="LF"~"LFsecond", 
                                 FirstDiet=="LF"& Diet=="LF"~"LFfirst", 
                                 FirstDiet=="LF"& Diet=="LC"~"LCsecond"))  

##### Carryover Effect Tests ##### 
EILMM2 <- lm(AveEI~Diet+Time+Diet*Time,data=enint2) 
summary(EILMM2) 

### Adjusted Model ### 
### A mixed linear model was used to account for a longitudinal design. 
EILMM <- lmerTest::lmer(AveEI~Diet*Time+Sex+BMI+REE+(1|SubjectID),data=enint2) 
summary(EILMM) 

##### What better predicts energy intake Diet or Diet Order? ##### 
# Creating a diet order variable 
enint <- enint %>% mutate(lcf = enint$SubjectID %in% lcf) 

### As Hall et al. reported, a ~690kcal/d in favor of the LFD is observed. 
Diet <- lm(AveEI~Diet,data=enint) 
summary(Diet) 

### But the Diet Order Effect is much larger. 
DietOrderM <- lm(AveEI~lcf,data=enint) 
summary(DietOrderM) 
compare_performance(Diet,DietOrderM) 

##### Did those who start with a LCD lose more fat? ##### 
### Creating a Boolean low-carbohydrate first variable in the relevant dataset. 
deltabc <- deltabc %>% mutate(lcf=deltabc$SubjectID %in% lcf)  

### Filtering out data from the 1st period 
ph2deltabc <- deltabc %>% filter(DietPhase==2 & DEXAVisit==3) 
fatchlcfm <- lm(DeltaFM~lcf,data=ph2deltabc) 
summary(fatchlcfm) 

##### What is a better predictor of fat-loss? Diet or diet order? ##### 
Diet_on_Fat_Loss <- lm(DeltaFM~Diet,data=deltabc) 

# As Hall et al reported, LFD was associated with more fat-loss. 
summary (Diet_on_Fat_Loss) 

# But the Diet Order Effect again is much larger. 
Diet_Order_on_Fat_Loss <- lm(DeltaFM~lcf,data=deltabc) 
summary(Diet_Order_on_Fat_Loss) 
compare_performance(Diet_on_Fat_Loss,Diet_Order_on_Fat_Loss) 

##### Did those who start with a LCD achieve higher BHB levels on a LCD? ##### 
### Creating a Boolean low-carbohydrate first variable in the relevant dataset. 

bhb <- ketoneblood %>% mutate(lcf = ketoneblood$SubjectID %in% lcf) %>% 
  
### Note that there are no data from participants: 
# LCLF001 (LFD first) and LCLF002, LCLF003, LCLF005, LCLF006 (LCD first). 
### Filtering out data from the low-fat period. 
filter(Diet=="LC") 

### Hypothesis test 
BHBm <- lm(Value ~ lcf, data=bhb) 
summary(bhb) 

### Did those who start with a LCD oxidize more fat (assessed by RQ) with a LFD?
### Creating a Boolean low-carbohydrate first variable in the relevant dataset. 
chamberhip <- chamberhip %>% mutate(lcf = chamberhip$SubjectID %in% lcf) 

### Filtering out data from the low-carb period 
rqwithLF <- chamberhip %>% filter(Diet=="LF") 

### Hypothesis test 
RQm <- lm(TotalRQ~lcf,data=rqwithLF) 
summary(RQm) 

################################################################################ 
############################ C-PEPTIDE ANALYSES ################################
################################################################################ 
##### Does C-peptide change predict Energy Intake? ##### 
### Filtering out energy intake data from the 1st period by keeping the low carb 
### data for those in whom "low-carb first" is false or keeping the low fat data 
### for those in whom "low-carb first# is true. 
enint2ndperiod <- enint %>% filter(lcf == F & Diet=="LC" | lcf== T & Diet=="LF") 

### Creating a Boolean low-carbohydrate first variable in the relevant dataset. 
labblood <- labblood %>% mutate(lcf = labblood$SubjectID %in% lcf) %>% 

### Excluding subject 4 (the participant who dropped out due to hypoglycemia). 
  
filter(!SubjectID=="LCLF004") 

### Creating "Baseline", "period 1", and "period 2" variables. 
labblood <- labblood %>% mutate(period=(case_when(Day==1~"BL", 
                                                  Day<=15 & Day > 1 ~"period1", 
                                                  Day==29~"period2"))) 

### Filtering C-peptide values. 
cpeptide <- labblood %>% filter(ObservationName=="C-Peptide, Serum") 

### Filtering Baseline C-peptide values. 
blcpeptide <- cpeptide %>% filter(period=="BL")  

### Filtering period 1 C-peptide values. 
ph1cpeptide <- cpeptide %>% filter(period=="period1") %>%  

### Calculating the change in fasting C-peptide secretion. 
  mutate(deltacpep=Value-blcpeptide$Value)

### Merging the relevant C-peptide and energy intake datasets. 
cpepenint2ndph <- merge(ph1cpeptide,enint2ndperiod,by="SubjectID") 

### Hypothesis test. 

cpepeim <- lm(AveEI~deltacpep,data=cpepenint2ndph) 
summary(cpepeim) 

##### Does C-peptide change predict Fat change? ##### 
### Merging the relevant C-peptide and body composition datasets. 
cpepfatloss <- merge(ph1cpeptide,ph2deltabc,by="SubjectID") 

### Hypothesis test. 
cpepfatchm <- lm(DeltaFM~deltacpep,data=cpepfatloss) 
summary(cpepfatchm) 

################################################################################ 
############################### FIGURES ######################################## 
################################################################################ 
##### Figure 1 ##### 
eidailylc <- foodsnackday %>%  
  mutate(lcf = foodsnackday$SubjectID %in% lcf) %>%  
  mutate(Day=ifelse(DietOrder=="LC",Day,Day-14)) %>%  
  filter(Diet=="LC") %>%  
  group_by(lcf, Day) %>% 
  summarize(MeanEI = mean(EI),SE = sd(EI) / sqrt(n())) 

Fig1A.1 <-  
  ggplot(eidailylc,aes(x=Day,y=MeanEI,fill=factor(lcf),color=factor(lcf)))+ 
  geom_line() + 
  geom_point() + 
  geom_errorbar(aes(ymin = MeanEI - SE, ymax = MeanEI + SE), width = 0.2) +  
  labs( 
    x = "Day",                     
    y = "Energy Intake (kcal/day)",          
    title = "Mean Energy Intake during Low-Carb by Diet Order", 
    fill = "Low-Carb First", 
    color = "Low-Carb First") + 
  theme_minimal() + 
  ggplot2::scale_y_continuous( 
    limits = c(0,4500), 
    breaks = seq(0, 4500, by= 500))+ 
  ggplot2::scale_x_continuous( 
    limits = c(0,15), 
    breaks = seq(0, 15, by= 1))+ 
  ggplot2::theme( 
    plot.background = element_blank(), 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), 
    panel.border = element_blank(), 
    legend.position = "none", 
    plot.title = element_text(size=16,face = "bold"), 
    plot.title.position = "plot", 
    plot.margin = margin(1,1,1,1,unit="cm"), 
    axis.line = element_line(color = "black",size = 1, linetype = "solid"), 
    axis.title.y = element_text(size=14,colour = "black",face="bold",margin = margin(r=0.5,unit="cm")), 
    axis.title.x = element_text(size=14,colour = "black",face="bold",margin = margin(r=0.5,unit="cm")), 
    axis.text = element_text(size=14,colour = "black", margin = margin(t=1,r=1,unit="cm"))) 
Fig1A.1 

ggsave("Fig1A.1. Daily Energy Intake During the LCD.tiff", 
       units="cm", width=15, height=15, dpi=1000, compression = 'lzw') 

fig1atests <- lm(eidailylc$MeanEI~eidailylc$lcf) 
summary(fig1atests) 

eidailylf <- foodsnackday %>% 
  mutate(lcf = foodsnackday$SubjectID %in% lcf) %>%  
  mutate(Day=ifelse(DietOrder=="LF",Day,Day-14)) %>%  
  filter(Diet=="LF") %>%  
  group_by(lcf, Day) %>% 
  summarize(MeanEI = mean(EI),SE = sd(EI) / sqrt(n())) 

Fig1B.1 <-  
  ggplot(eidailylf,aes(x=Day,y=MeanEI,fill=factor(lcf,levels = c(T,F)), 
                       color=factor(lcf,levels = c(T,F))))+ 
  geom_line() + 
  geom_point() + 
  geom_errorbar(aes(ymin = MeanEI - SE, ymax = MeanEI + SE), width = 0.2) +  
  labs( 
    x = "Day",                     
    y = "Energy Intake (kcal/day)", 
    title = "Mean Energy Intake during Low-Fat by Diet Order", 
    fill = "Low-Carb First", 
    color = "Low-Carb First") + 
  theme_minimal() + 
  ggplot2::scale_y_continuous( 
    limits = c(0,4500), 
    breaks = seq(0, 4500, by= 500))+ 
  ggplot2::scale_x_continuous( 
    limits = c(0,15), 
    breaks = seq(0, 15, by= 1))+ 
  ggplot2::theme( 
    plot.background = element_blank(), 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), 
    panel.border = element_blank(), 
    plot.title = element_text(size=16,face = "bold"), 
    plot.title.position = "plot", 
    plot.margin = margin(1,1,1,1,unit="cm"), 
    legend.position = "none", 
    axis.line = element_line(color = "black",size = 1, linetype = "solid"), 
    axis.title.y = element_text(size=14,colour = "black",face="bold",margin = margin(r=0.5,unit="cm")), 
    axis.title.x = element_text(size=14,colour = "black",face="bold",margin = margin(r=0.5,unit="cm")), 
    axis.text = element_text(size=14,colour = "black", margin = margin(t=1,r=1,unit="cm"))) 
Fig1B.1 
ggsave("Fig1B.1. Daily Energy Intake During the LFD.tiff", 
       units="cm", width=15, height=15, dpi=1000, compression = 'lzw') 

fig1btests <- lm(eidailylf$MeanEI~eidailylf$lcf) 
summary(fig1btests) 

##### Figure 2 ##### 
dxaslc <- deltabc %>% filter(Diet=="LC") %>%  
  group_by(lcf, DEXAVisit) %>% 
  summarize(MeanDeltaFM = mean(DeltaFM),SE = sd(DeltaFM) / sqrt(n())) 

# To view mean body composition changes.
View(dxaslc) 

Fig2A.1 <- ggplot(dxaslc,aes(x=DEXAVisit,y=MeanDeltaFM,fill=factor(lcf),color=factor(lcf)))+ 
  geom_line() + 
  geom_point() + 
  geom_errorbar(aes(ymin = MeanDeltaFM - SE, ymax = MeanDeltaFM + SE), width = 0.2) + 
  theme_minimal() + 
  labs( 
    x = "DXA Scan",                     
    y = "Delta Fat Mass",          
    title = "Mean Change in Fat Mass During LCD by Diet Order", 
    fill = "Low-Carb First", 
    color = "Low-Carb First") +  
  ggplot2::scale_y_continuous( 
    limits = c(-2,2), 
    breaks = seq(-2,2, by= 0.5))+ 
  ggplot2::scale_x_continuous( 
    limits = c(1,3), 
    breaks = seq(1,3, by= 1))+ 
  ggplot2::theme(  
    plot.background = element_blank(), 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), 
    panel.border = element_blank(), 
    legend.position = "none", 
    plot.title = element_text(size=16,face = "bold"), 
    plot.title.position = "plot", 
    plot.margin = margin(1,1,1,1,unit="cm"), 
    axis.line = element_line(color = "black",size = 1, linetype = "solid"), 
    axis.title.y = element_text(size=14,colour = "black",face="bold",margin = margin(r=0.5,unit="cm")), 
    axis.title.x = element_text(size=14,colour = "black",face="bold",margin = margin(r=0.5,unit="cm")), 
    axis.text = element_text(size=14,colour = "black", margin = margin(t=1,r=1,unit="cm"))) 
Fig2A.1 

ggsave("Fig2A. Fat Change During the LCD by Diet Order.tiff", 
       units="cm", width=15, height=15, dpi=1000, compression = 'lzw') 

deltabclc <- deltabc %>% filter(Diet=="LC") 

fig2atests1 <- lm(deltabclc$DeltaFM~deltabclc$lcf) 
summary(fig2atests1) 

dxaslf <- deltabc %>% filter(Diet=="LF") %>%  
  group_by(lcf, DEXAVisit) %>% 
  summarize(MeanDeltaFM = mean(DeltaFM),SE = sd(DeltaFM) / sqrt(n())) 

Fig2B.1 <- ggplot(dxaslf,aes(x=DEXAVisit,y=MeanDeltaFM, 
                             fill=factor(lcf,levels = c(T,F)), 
                             color=factor(lcf,levels = c(T,F))))+ 
  geom_line() + 
  geom_point() + 
  geom_errorbar(aes(ymin = MeanDeltaFM - SE, ymax = MeanDeltaFM + SE), width = 0.2) + 
  theme_minimal() + 
  labs( 
     x = "DXA Scan",                     
     y = "Delta Fat Mass",          
     title = "Mean Change in Fat Mass During LFD by Diet Order", 
     fill = "Low-Carb First", 
     color = "Low-Carb First") +  
     ggplot2::scale_y_continuous( 
     limits = c(-2,2), 
     breaks = seq(-2,2, by= 0.5))+ 
     ggplot2::scale_x_continuous( 
     limits = c(1,3), 
     breaks = seq(1,3, by= 1))+ 
    ggplot2::theme( 
    plot.background = element_blank(), 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), 
    panel.border = element_blank(), 
    legend.position = "none", 
    plot.title = element_text(size=16,face = "bold"), 
    plot.title.position = "plot", 
    plot.margin = margin(1,1,1,1,unit="cm"), 
    axis.line = element_line(color = "black",linewidth = 1, linetype = "solid"), 
    axis.title.y = element_text(size=14,colour = "black",face="bold",margin = margin(r=0.5,unit="cm")), 
    axis.title.x = element_text(size=14,colour = "black",face="bold",margin = margin(r=0.5,unit="cm")), 
    axis.text = element_text(size=14,colour = "black", margin = margin(t=1,r=1,unit="cm"))) 
Fig2B.1 

ggsave("Fig2B. Energy Intake During the LFD by Diet Order.tiff", 
       units="cm", width=15, height=15, dpi=1000, compression = 'lzw') 

deltabclf <- deltabc %>% filter(Diet=="LF") 

fig2btests2 <- lm(deltabclf$DeltaFM~deltabclf$lcf) 
summary(fig2btests2) 

##### Figure 3 ##### 
Fig3A <-  
  ggplot(bhb,aes(x=factor(lcf,level=c(T,F)),y=Value,fill=factor(lcf),color=factor(lcf)))+ 
  geom_boxplot(alpha=0.5,outlier.shape = NA )+ 
  labs( 
    x = "Diet Order", 
    y = "\u03B2HB blood levels (mmol/L)",          
    title = "Figure 3A", 
    subtitle = "\u03B2HB blood levels during the Low-Carbohydrate Diet", 
    colour = "Low Carbohydrate Diet First", 
    fill= "Low Carbohydrate Diet First") + 
  ggplot2::scale_x_discrete(labels = c("Low-Carb First","Low-Carb Second"))+ 
  theme_minimal() + 
  ggplot2::theme( 
    plot.background = element_blank(), 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), 
    panel.border = element_blank(), 
    legend.position = "none", 
    plot.title = element_text(size=16,face = "bold"), 
    plot.title.position = "plot", 
    plot.margin = margin(1,1,1,1,unit="cm"), 
    axis.line = element_line(color = "black",linewidth = 1, linetype = "solid"), 
    axis.title.y = element_text(size=14,colour = "black",face="bold",margin = margin(r=0.5,unit="cm")), 
    axis.title.x = element_text(size=14,colour = "black",face="bold",margin = margin(r=0.5,unit="cm")), 
    axis.text = element_text(size=14,colour = "black", margin = margin(t=1,r=1,unit="cm")))+ 
  ggsignif::geom_signif(comparisons = list(c("FALSE", "TRUE")),color="black",map_signif_level = T) 
Fig3A 

ggsave("Fig3A. Daily Energy Intake During the LCD.tiff", 
       units="cm", width=15, height=15, dpi=1000, compression = 'lzw') 

fig3atests <- lm(bhb$Value~bhb$lcf) 
summary(fig3atests) 

Fig3B <-  
  ggplot(rqwithLF,aes(x=factor(lcf,level=c(T,F)),y=TotalRQ,fill=factor(lcf),color=factor(lcf)))+ 
  geom_boxplot(alpha=0.5)+ 
  labs( 
    x = "Diet Order", 
    y = "Total RQ", 
    title = "Figure 3B", 
    subtitle = "Total RQ during the Low-Fat Diet", 
    colour = "Low Carbohydrate Diet First", 
    fill= "Low Carbohydrate Diet First") + 
    ggplot2::scale_x_discrete(labels = c("Low-Carb First","Low-Carb Second"))+ 
    theme_minimal() + 
    ggplot2::theme( 
    plot.background = element_blank(), 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), 
    panel.border = element_blank(), 
    legend.position = "none", 
    plot.title = element_text(size=16,face = "bold"), 
    plot.title.position = "plot", 
    plot.margin = margin(1,1,1,1,unit="cm"), 
    axis.line = element_line(color = "black",linewidth = 1, linetype = "solid"), 
    axis.title.y = element_text(size=14,colour = "black",face="bold",margin = margin(r=0.5,unit="cm")), 
    axis.title.x = element_text(size=14,colour = "black",face="bold",margin = margin(r=0.5,unit="cm")), 
    axis.text = element_text(size=14,colour = "black", margin = margin(t=1,r=1,unit="cm")))+ 
    ggsignif::geom_signif(comparisons = list(c("FALSE", "TRUE")),color="black",map_signif_level = T) 
Fig3B 

ggsave("Fig3B. Daily Energy Intake During the LCD.tiff", 
       units="cm", width=15, height=15, dpi=1000, compression = 'lzw') 

fig3btests <- lm(rqwithLF$TotalRQ~rqwithLF$lcf) 
summary(fig3btests) 

##### Figure 4 ##### 
Fig4A <- ggscatterstats( 
  data  = cpepenint2ndph, 
  x     = deltacpep, 
  y     = AveEI, 
  xlab  = "Period I Change in Fasting C-peptide", 
  ylab  = "Period II Mean Energy Intake (kcal/day)", 
  title = "Figure 4A", 
  subtitle = "\u03B2=1780 kcal/day per ng/ml of change in C-petide, SE=524, p=0.004, R²=41%", 
  results.subtitle = F, 
  marginal = F, 
  smooth.line.args = list(size = 1.5, 
                          color = "black", 
                          method = "lm", 
                          formula = y ~ x))+ 
  ggplot2::scale_x_discrete(labels = c("Low-Fat Diet","Low-Carb Diet"))+ 
  ggplot2::scale_x_continuous(limits = c(-1.5,0),breaks = seq(-1.5,0, by= 0.5)) 
Fig4A 
ggsave("Fig4A. Cpep vs Enint.tiff", units="cm", width=15, height=15, dpi=1000,
       compression = 'lzw') 

Fig4B <- ggscatterstats( 
  data  = cpepfatloss, 
  x     = deltacpep, 
  y     = DeltaFM, 
  xlab  = "period I Change in Fasting C-peptide", 
  ylab  = "Fat loss during period II (kg)", 
  title = "Figure 4B", 
  subtitle = "\u03B2=1.4 kg per ng/ml of change in C-petide, SE=0.55, p=0.03, R²=26%", 
  results.subtitle = F,  
  marginal = F, 
  smooth.line.args = list(size = 1.5, 
                          color = "black", 
                          method = "lm", 
                          formula = y ~ x))+ 
  ggplot2::scale_x_discrete(labels = c("Low-Fat Diet","Low-Carb Diet"))+ 
  ggplot2::scale_x_continuous(limits = c(-1.5,0),breaks = seq(-1.5,0, by= 0.5)) 
Fig4B 

ggsave("Fig4B.CpepvsFat Loss.tiff", 
       units="cm", width=15, height=15, dpi=1000, compression = 'lzw') 

###############################################################################
############################### PERIOD TESTS  #################################
###############################################################################
##### Obtaining Period data ##### 
eidaily1 <- foodsnackday %>% filter(Diet=="LC" & DietOrder=="LC"| 
                                      Diet=="LF" & DietOrder=="LF") %>% 
                             filter(WeekOnDiet=="Week2") %>%  
                             select(EI,Diet,SubjectID) 

eidailylc1 <- eidaily1 %>% filter(Diet=="LC") 
eidailylf1 <- eidaily1 %>% filter(Diet=="LF") 

eidaily2 <- foodsnackday %>% filter(Diet=="LF" & DietOrder=="LC"| 
                                      Diet=="LC" & DietOrder=="LF") %>% 
                             filter(WeekOnDiet=="Week2") %>%  
                             select(EI,Diet,SubjectID) 

eidailylc2 <- eidaily2 %>% filter(Diet=="LC") 
eidailylf2 <- eidaily2 %>% filter(Diet=="LF") 

##### Additional Carry-over Effects ##### 
p1m1 <- lmerTest::lmer(EI~Diet+(1|SubjectID),data=eidaily1) 
summary(p1m1) 

p2m1 <- lmerTest::lmer(EI~Diet+(1|SubjectID),data=eidaily2) 
summary(p2m1) 

t.test(eidailylc1$EI,eidailylc2$EI) 
t.test(eidailylf1$EI,eidailylf2$EI) 
t.test(eidailylc1$EI,eidailylf1$EI) 
t.test(eidailylc2$EI,eidailylf2$EI) 

################################################################################
##### PLEASE ADDRESS CORRESPONDENCE ABOUT THIS CODE TO adrian.soto@tec.mx
################################################################################ 