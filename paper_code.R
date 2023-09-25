################################################################################
################################ SET UP ########################################
################################################################################
##### Working Directory #####
# Working directory setup. Of course, this will be a different path for you.
setwd("C:/Users/adria/Dropbox/UIEM/LEAD/Proyectos/Dietorder")

# Confirm you have "pacman" installed. If you don't have "pacman" but want
# to install it, remove the # in the line below and press "Enter".
# install.packages("pacman") 

##### Packages #####
# Packages setup
pacman::p_load(dplyr,tidyr,ggstatsplot,readxl,tableone,easystats,ggplot2,
               patchwork,MASS,see,performance,conflicted,haven,mediation)

# Solving duplicate functions conflicts
conflict_prefer("select","dplyr")
conflict_prefer("filter", "dplyr")

###### Data Upload #####
# All datasets were obtained from the publicly available repository at:
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
############################# TABLE 1 #####################################
################################################################################
##### Baseline Values of the available participants #####
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

###### Which patients started with which diet? ######
LCDfirst <- baseline %>% filter(FirstDiet=="LC") #n = 10
unique(LCDfirst$SubjectID)
LFDfirst <- baseline %>% filter(FirstDiet=="LF") #n = 6?
unique(LFDfirst$SubjectID)

### Creating a list to subset future datasets by diet order
# Low-carb first
lcf <- c("LCLF002", "LCLF003", "LCLF005", "LCLF006", "LCLF010", "LCLF013", 
         "LCLF014", "LCLF021", "LCLF022", "LCLF023")

# Low-fat first
lff <- c("LCLF001","LCLF009","LCLF015","LCLF017","LCLF018","LCLF020")

################################################################################
######## ARE TWO WEEKS ENOUGH TIME FOR DIETARY PHYSIOLOGICAL ADAPTATION? #######
################################################################################
##### Did those who started with a LCD have lower Energy Intake in Phase 2? ####
### Creating a Boolean low-carbohydrate first variable in the relevant dataset.
enint <- enint %>% mutate(lcf = enint$SubjectID %in% lcf)

### Filtering out energy intake data from the 1st phase by keeping the low carb
### data of those in whom "low-carb first" is false or keeping the low fat data
### in those in whom "low-carb first# is true.
enint2ndphase <- enint %>% filter(lcf == F & Diet=="LC" | lcf== T & Diet=="LF")

### Testing if diet order predicts average energy intake in the filtered data.
### Hypothesis test.
m1 <- lm(AveEI~lcf,data=enint2ndphase)
summary(m1)

##### What is a better predictor of energy intake? Diet or diet order? #####
Diet_on_EI <- lm(AveEI~Diet,data=enint)

# As Hall et al reported, LFD was associated with less energy intake.
summary(Diet_on_EI)

# But having started with LCD is also associated with less energy intake.
Diet_Order_on_EI <- lm(AveEI~lcf,data=enint)
summary(Diet_Order_on_EI)

# Comparing both explanations
compexpei <- compare_performance(Diet_Order_on_EI,Diet_on_EI)

##### Did those who started with a LCD lose more fat in Phase 2? #####
### Creating a Boolean low-carbohydrate first variable in the relevant dataset.
deltabc <- deltabc %>% mutate(lcf=deltabc$SubjectID %in% lcf) 

### Filtering out data from the 1st phase
ph2deltabc <- deltabc %>% filter(DietPhase==2 & DEXAVisit==3)

m2 <- lm(DeltaFM~lcf,data=ph2deltabc)
summary(m2)

##### What is a better predictor of fat-loss? Diet or diet order? #####
Diet_on_Fat_Loss <- lm(DeltaFM~Diet,data=deltabc)
# As Hall et al reported, LFD was associated with more fat-loss.
summary(Diet_on_Fat_Loss)

# But having started with LCD is also associated with fat-loss.
Diet_Order_on_Fat_Loss <- lm(DeltaFM~lcf,data=deltabc)
summary(Diet_Order_on_Fat_Loss)

# Comparing both explanations
compexpfl <- compare_performance(Diet_Order_on_Fat_Loss,Diet_on_Fat_Loss)

##### Did those who started with a LCD reach higher BHB levels on a LCD? #####
### Creating a Boolean low-carbohydrate first variable in the relevant dataset.
bhb <- ketoneblood %>% mutate(lcf = ketoneblood$SubjectID %in% lcf) %>% 
  
### Please note that, for reasons we ignore, the is no data from participants:
# LCLF001 (LFD first) and LCLF002, LCLF003, LCLF005, LCLF006 (LCD first).
  
### Filtering out data from the low-fat phase.
filter(Diet=="LC")

### Hypothesis test
m3 <- lm(Value ~ lcf, data=bhb)
summary(m3)

##### Did those who started with a LCD oxidize more fat with a LFD? #####
### Creating a Boolean low-carbohydrate first variable in the relevant dataset.
chamberhip <- chamberhip %>% mutate(lcf = chamberhip$SubjectID %in% lcf)

### Filtering out data from the low-carb phase
rqwithLF <- chamberhip %>% filter(Diet=="LF")

### Hypothesis test
m4 <- lm(TotalRQ~lcf,data=rqwithLF)
summary(m4)

################################################################################
########## DO THESE DATA CONTRADICT THE CARBOHYDRATE-INSULIN MODEL? ############
################################################################################
##### Does C-peptide change predict Energy Intake? #####
### Creating a Boolean low-carbohydrate first variable in the relevant dataset.
labblood <- labblood %>% mutate(lcf = labblood$SubjectID %in% lcf) %>%
  
### Excluding subject 4 (the participant who dropped out due to hypoglycemia).
  filter(!SubjectID=="LCLF004")

### Creating "Baseline", "Phase 1", and "Phase 2" variables.
labblood <- labblood %>% mutate(Phase=(case_when(Day==1~"BL",
                                                 Day<=15 & Day > 1 ~"Phase1",
                                                 Day==29~"Phase2")))

### Filtering C-peptide values.
cpeptide <- labblood %>% filter(ObservationName=="C-Peptide, Serum")

### Filtering Baseline C-peptide values.
blcpeptide <- cpeptide %>% filter(Phase=="BL") 

### Filtering Phase 1 C-peptide values.
ph1cpeptide <- cpeptide %>% filter(Phase=="Phase1") %>% 
  
### Calculating the change in fasting C-peptide secretion.
mutate(deltacpep=Value-blcpeptide$Value)

### Merging the relevant C-peptide and energy intake datasets.
cpepenint2ndph <- merge(ph1cpeptide,enint2ndphase,by="SubjectID")

### Hypothesis test.
m5 <- lm(AveEI~deltacpep,data=cpepenint2ndph)
summary(m5)

##### Does C-peptide change predict Fat change? #####
### Merging the relevant C-peptide and body composition datasets.
cpepfatloss <- merge(ph1cpeptide,ph2deltabc,by="SubjectID")

### Hypothesis test.
m6 <- lm(DeltaFM~deltacpep,data=cpepfatloss)
summary(m6)

##### Does Mass Intake in Phase 1 predict Energy Intake in Phase 2? #####
### Filtering out energy intake data from the 2nd phase by keeping the low fat
### data of those in whom "low-carb first" is false or keeping the low carb data
### in those in whom "low-carb first# is true.
enint1stphase <- enint %>% filter(lcf == F & Diet=="LF" | lcf== T & Diet=="LC")

### Hypothesis test.
m7 <- lm(enint2ndphase$AveEI~enint1stphase$AveMass)
summary(m7)

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

Fig1C <- plot(compexpei)
Fig1C + scale_colour_manual(values=c("#808080","#00BFC4"))

##### Figure 2 #####
dxaslc <- deltabc %>% filter(Diet=="LC") %>% 
  group_by(lcf, DEXAVisit) %>%
  summarize(MeanDeltaFM = mean(DeltaFM),SE = sd(DeltaFM) / sqrt(n()))

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
fig2atests <- lm(deltabclc$DeltaFM~deltabclc$lcf)
summary(fig2atests)

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
    axis.line = element_line(color = "black",size = 1, linetype = "solid"),
    axis.title.y = element_text(size=14,colour = "black",face="bold",margin = margin(r=0.5,unit="cm")),
    axis.title.x = element_text(size=14,colour = "black",face="bold",margin = margin(r=0.5,unit="cm")),
    axis.text = element_text(size=14,colour = "black", margin = margin(t=1,r=1,unit="cm")))
Fig2B.1
ggsave("Fig2B. Energy Intake During the LFD by Diet Order.tiff",
       units="cm", width=15, height=15, dpi=1000, compression = 'lzw')

deltabclf <- deltabc %>% filter(Diet=="LF")
fig2btests <- lm(deltabclf$DeltaFM~deltabclf$lcf)
summary(fig2btests)

Fig2C <- plot(compexpfl)
Fig2C + scale_colour_manual(values=c("#808080","#00BFC4"))

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
    axis.line = element_line(color = "black",size = 1, linetype = "solid"),
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
    axis.line = element_line(color = "black",size = 1, linetype = "solid"),
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
  xlab  = "Phase I Change in Fasting C-peptide",
  ylab  = "Phase II Mean Energy Intake (kcal/day)",
  title = "Figure 3A",
  subtitle = "\u03B2=1780, SE=524, p=0.004, R²=41%",
  results.subtitle = F, 
  marginal = F,
  smooth.line.args = list(size = 1.5,
                          color = "black",
                          method = "lm",
                          formula = y ~ x,
                          na.rm = TRUE))+
  ggplot2::scale_x_discrete(labels = c("Low-Fat Diet","Low-Carb Diet"))+
  ggplot2::scale_x_continuous(limits = c(-1.5,0),breaks = seq(-1.5,0, by= 0.5))
Fig4A
ggsave("Fig4A. Cpep vs Enint.tiff",
       units="cm", width=15, height=15, dpi=1000, compression = 'lzw')


Fig4B <- ggscatterstats(
  data  = cpepfatloss,
  x     = deltacpep,
  y     = DeltaFM,
  xlab  = "Phase I Change in Fasting C-peptide",
  ylab  = "Fat loss during Phase II (kg)",
  title = "Figure 4B",
  subtitle = "\u03B2=1.4, SE=0.55, p=0.03, R²=26%",
  results.subtitle = F, 
  marginal = F,
  smooth.line.args = list(size = 1.5,
                          color = "black",
                          method = "lm",
                          formula = y ~ x,
                          na.rm = TRUE))+
  ggplot2::scale_x_discrete(labels = c("Low-Fat Diet","Low-Carb Diet"))+
  ggplot2::scale_x_continuous(limits = c(-1.5,0),breaks = seq(-1.5,0, by= 0.5))
Fig4B
ggsave("Fig4B.CpepvsFat Loss.tiff",
       units="cm", width=15, height=15, dpi=1000, compression = 'lzw')

################################################################################
### PLEASE, ADDRESS ALL CORRESPONDENCE ABOUT THIS CODE TO adrian.soto@tec.mx ###
################################################################################