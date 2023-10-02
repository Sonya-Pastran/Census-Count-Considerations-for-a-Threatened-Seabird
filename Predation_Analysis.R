rm(list = ls())

#Libraries-----
{
  library(readr)
  library(tidyverse)
  library(magrittr)
  library(lubridate)
  library(hms)
  library(ggplot2)
  library(Matrix)
  library(lme4)
  library(ggeffects)
  library(glmmTMB)
  library(ggeffects)
  library(sjlabelled)
  library(AICcmodavg)
  library(car)
  library(performance)
  library(sjstats)
  library(sjPlot)
  library(sjlabelled)
  library(sjmisc)
  library(broom.mixed)
  library(writexl)
  library(officer)
  library(effects)
  library(ltm)
}


#load the dataset
Counts_Pred_dataset <- read_csv("~/Masters_Publish_Pedation/Datasets/Counts_Pred_dataset_paired.csv")

df<-as.data.frame(Counts_Pred_dataset)

#Center and scale numeric variables
df[c(8,12,13)] <- lapply(df[c(8,12,13)], function(x) c(scale(x)))


#Set the factors
df$year<-as.factor(df$year)
df$pairs<-as.factor(df$pairs)
df$Status<-as.factor(df$Status)
df$TransectSection<-as.factor(df$TransectSection)
df$ShoreProx<-as.factor(df$ShoreProx)

#Correlations

cor(df$Fish, df$Eagle)
biserial.cor(df$Eagle, df$year)
biserial.cor(df$Fish, df$year)
biserial.cor(df$Eagle, df$ShoreProx)
biserial.cor(df$Eagle, df$ShoreSide)
biserial.cor(df$Fish, df$Status)
biserial.cor(df$Eagle, df$Status)
biserial.cor(df$Fish, df$ShoreSide)



#Run Global Model
mod <- glmer(MAMU~Status + Eagle + Fish + year + ShoreProx + ShoreSide + 
               Status*ShoreProx + ShoreProx*ShoreSide +
               year*Status +Eagle*Status+Eagle*year+year*Fish+
               (1|pairs) , data = df, family = poisson)





#Make table and plot displaying global model results
summary(mod)
tidy_table2 <- tidy(mod, conf.int = TRUE)

#doc <- read_docx()
#doc <- doc %>%
 # body_add_table(tidy_table)
#print(doc, target = "tidy_table2.docx")

#Plot historgam of eagles relationship to kites

EAGLE<-Counts_Pred_dataset%>%
  filter(ShoreProx!="Offshore")

EAGLE$year<-as.factor(EAGLE$year)
EAGLE$Status<-as.factor(EAGLE$Status)

# Create a boxplot

png("C:/Users/PastranS/Documents/Masters_Publish_Pedation/Revisions/Re-submission/Figue 4.png", width = 4200,height = 3200,units = "px", res = 600, bg= "white")

ggplot(EAGLE, aes(x = year, y = Eagle, fill = Status)) +
  geom_boxplot() +
  stat_summary(fun=mean, geom="point", shape=20, size=3, color="gray", position=position_dodge(width=0.75)) +
  labs(
    title = "",
    x = "Year",
    y = "Real Eagles (count/shoreside)"
  ) +
  scale_fill_manual(values = c("KITEdown" = "blue", "KITEup" = "red")) +
  theme_minimal()

dev.off()

#make into a figure

# Reorder the terms (put interaction terms first)
tidy_table2 <- tidy_table2[-c(1, 14), ]


# Reorder the terms

tidy_table2 <- tidy_table2 %>%
  mutate(term = ifelse(term == "Eagle", "Real Eagle counts", term))

tidy_table2 <- tidy_table2 %>%
  mutate(term = ifelse(term == "Fish", "Fish School counts", term))

tidy_table2 <- tidy_table2 %>%
  mutate(term = ifelse(term == "StatusKITEup", "Kite status: Up", term))

tidy_table2 <- tidy_table2 %>%
  mutate(term = ifelse(term == "ShoreProxOnshore", "Shore proximity: Inshore", term))

tidy_table2 <- tidy_table2 %>%
  mutate(term = ifelse(term == "year2019", "Year: 2019", term))

tidy_table2 <- tidy_table2 %>%
  mutate(term = ifelse(term == "ShoreSideS", "Shoreside: South", term))


tidy_table2 <- tidy_table2 %>%
  mutate(term = ifelse(term == "ShoreProxOnshore:ShoreSideS","Shore proximity: Inshore x Shoreside: South", term))

tidy_table2 <- tidy_table2 %>%
  mutate(term = ifelse(term == "StatusKITEup:year2019","Kite status: Up x Year: 2019", term))

tidy_table2 <- tidy_table2 %>%
  mutate(term = ifelse(term == "StatusKITEup:ShoreProxOnshore","Kite status: Up x Shore proximity: Inshore", term))

tidy_table2 <- tidy_table2 %>%
  mutate(term = ifelse(term == "StatusKITEup:ShoreSideS","Kite status: Up x Shoreside: South", term))

tidy_table2 <- tidy_table2 %>%
  mutate(term = ifelse(term == "StatusKITEup:Eagle","Kite status: Up x Real Eagle counts", term))

tidy_table2 <- tidy_table2 %>%
  mutate(term = ifelse(term == "Fish:year2019", "Fish School counts x Year: 2019", term))

tidy_table2 <- tidy_table2 %>%
  mutate(term = ifelse(term == "Eagle:year2019", "Real Eagle counts x Year: 2019", term))


desired_order <- c("Shore proximity: Inshore x Shoreside: South","Kite status: Up x Year: 2019","Kite status: Up x Shore proximity: Inshore",
                   "Kite status: Up x Shoreside: South","Kite status: Up x Real Eagle counts",	
                   "Fish School counts x Year: 2019",	"Real Eagle counts x Year: 2019","Shoreside: South",
                   "Year: 2019","Shore proximity: Inshore","Fish School counts","Real Eagle counts","Kite status: Up")



tidy_table2 <- tidy_table2 %>%
  mutate(term = factor(term, levels = desired_order))


#Figure 3


png("C:/Users/PastranS/Documents/Masters_Publish_Pedation/Revisions/Re-submission/Figue 3.png", width = 4200,height = 3200,units = "px", res = 600, bg= "white")


ggplot(tidy_table2, aes(x = term, y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_pointrange() +
  geom_hline(aes(yintercept = 0), colour = "black", linetype ="dotted")+
  labs(x = "", y = "Estimate", title = "") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10), plot.title = element_text(hjust = 0.5)) +
  coord_flip()

dev.off()

#figure 6: Eagle and kite interaction

png("C:/Users/PastranS/Documents/Masters_Publish_Pedation/Revisions/Re-submission/Figue6C.png", width = 4200,height = 3200,units = "px", res = 600, bg= "white")


plotA<-plot_model(mod, type = "pred", terms = c("Eagle","year"), title = "A",axis.title = c("Real Eagle", "Predicted Murrelets (counts/shoreside)"))
plotA

dev.off()

png("C:/Users/PastranS/Documents/Masters_Publish_Pedation/Revisions/Re-submission/Figue6B.png", width = 4200,height = 3200,units = "px", res = 600, bg= "white")


plot<-plot_model(mod, type = "int", terms = c("year","Eagle"), title = "B",axis.title = c("Real Eagle", "Predicted Murrelets (counts/shoreside)"))
plotB<-plot[4]
plotB

dev.off()

png("C:/Users/PastranS/Documents/Masters_Publish_Pedation/Revisions/Re-submission/Figue6A.png", width = 4200,height = 3200,units = "px", res = 600, bg= "white")


p<-ggpredict(mod, terms = c("year", "Status"), title = "")
plotC<-plot(p) 
plotC + labs(y = "Predicted Murrelets (counts/shoreside", x = "Year", title = "C")

dev.off()

plot_grid(plotA, plotB, labels = c("A"), ncol = 1)


##Just change terms to look at differnt effects
#Run KITE Global Model
Counts_Pred_dataset_KITE <- read_csv("~/Masters_Publish_Pedation/Datasets/Pred_dataset_KITE.csv")
df<-as.data.frame(Counts_Pred_dataset_KITE)

#center and scale numeric variables
df[c(16,12)] <- lapply(df[c(16,12)], function(x) c(scale(x))) 

#Set the factors
df$year<-as.factor(df$year)
df$pairs<-as.factor(df$pairs)
df$Status<-as.factor(df$Status)
df$TransectSection<-as.factor(df$TransectSection)
df$ShoreProx<-as.factor(df$ShoreProx)
df$ShoreSide<-as.factor(df$ShoreSide)


mod <- glmmTMB(MAMUbuffprop~Status +FISHbuffprop + year + ShoreSide+ year*Status+
               FISHbuffprop*year+ (1|pairs), data = df, family = beta_family())

summary(mod)



tidy_table <- tidy(mod, conf.int = TRUE)
tidy_table <- tidy_table[-c(1, 8), ]

tidy_table <- tidy_table %>%
  mutate(term = ifelse(term == "FISHbuffprop", "Proportion of Fish in Zones", term))

tidy_table <- tidy_table %>%
  mutate(term = ifelse(term =="FISHbuffprop:year2019","Proportion of Fish in Zones x year2019", term))

tidy_table <- tidy_table %>%
  mutate(term = ifelse(term == "StatusKITEup:year2019","Kite status: Up x Year: 2019", term))


tidy_table <- tidy_table %>%
  mutate(term = ifelse(term == "ShoreSideS", "Shoreside: South", term))

tidy_table <- tidy_table %>%
  mutate(term = ifelse(term == "year2019", "Year: 2019", term))

tidy_table <- tidy_table %>%
  mutate(term = ifelse(term == "StatusKITEup", "Kite status: Up", term))

print(tidy_table)

#doc <- read_docx()
#doc <- doc %>%
#  body_add_table(tidy_table)
#print(doc, target = "tidy_table3.docx")

#Figure 5

# Reorder the terms (put interaction terms first)
desired_order <- c("Proportion of Fish in Zones x year2019","Kite status: Up x Year: 2019","Fish Proportion in Zones x year2019"
                   ,"Shoreside: South","Year: 2019","Proportion of Fish in Zones","Kite status: Up")

# Reorder the terms
tidy_table <- tidy_table %>%
  mutate(term = factor(term, levels = desired_order))


png("C:/Users/PastranS/Documents/Masters_Publish_Pedation/Revisions/Re-submission/Figue5.png", width = 4200,height = 3200,units = "px", res = 600, bg= "white")

ggplot(tidy_table, aes(x = term, y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_pointrange() +
  geom_hline(aes(yintercept = 0), colour = "black", linetype ="dotted")+
  labs(x = "", y = "Estimate", title = "") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10), plot.title = element_text(hjust = 0.5)) +
  coord_flip()

dev.off()


plot_model(mod, type = "pred", terms = c("Status"), title = "") ##Just change terms to look at differnt effects


#Control test for BOAT, COUNTS MAMU

Counts_Pred_control <- read_csv("~/Masters_Publish_Pedation/Datasets/Counts_Pred_control.csv")

df<-as.data.frame(Counts_Pred_control)

#Center and scale
df[c(8,12,13)] <- lapply(df[c(8,12,13)], function(x) c(scale(x))) 

#Set the factors
df$year<-as.factor(df$year)
df$pairs<-as.factor(df$pairs)
df$Status<-as.factor(df$Status)
df$TransectSection<-as.factor(df$TransectSection)
df$ShoreProx<-as.factor(df$ShoreProx)

str(df)
mod <- glmer(MAMU~Status + Eagle + Fish + ShoreProx + TransectSection + Status*ShoreProx +Status*Eagle +(1|pairs) , data = df, family = poisson)

summary(mod) #insignificant 

tidy_table <- tidy(mod, conf.int = TRUE)
# Control test for Kites
Counts_Pred_dataset_KITE_control <- read_csv("~/Masters_Publish_Pedation/Datasets/Pred_dataset_KITE_control.csv")

df<-as.data.frame(Counts_Pred_dataset_KITE_control)

#center and scale numeric variables
df[c(16,12)] <- lapply(df[c(16,12)], function(x) c(scale(x))) 

#Set the factors
df$year<-as.factor(df$year)
df$pairs<-as.factor(df$pairs)
df$Status<-as.factor(df$Status)
df$TransectSection<-as.factor(df$TransectSection)
df$ShoreProx<-as.factor(df$ShoreProx)


mod <- glmmTMB(MAMUbuffprop~Status +FISHbuffprop + ShoreSide + (1|pairs), data = df, family = beta_family())

summary(mod) #insignificant 

tidy_table <- tidy(mod, conf.int = TRUE)


### Adding in historgam for eagles

df<-df%>%
  filter(ShoreProx!="Offshore")

ggplot(df, aes(x = Eagle)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  labs(title = "Histogram of Eagles Counted", x = "Eagles Count", y = "Frequency") +
  theme_minimal()


### within survey day analysis

Counts_byside <- read_csv("~/Masters_Publish_Pedation/Datasets/Counts_Pred_control_byside.csv")

df<-as.data.frame(Counts_byside)

#center and scale numeric variables
df[c(8,12,13)] <- lapply(df[c(8,12,13)], function(x) c(scale(x))) 
#Set the factors
df$year<-as.factor(df$year)
df$pairs<-as.factor(df$pairs)
df$Status<-as.factor(df$Status)
df$TransectSection<-as.factor(df$TransectSection)
df$ShoreProx<-as.factor(df$ShoreProx)

# only look if they shifted between shoresides so remove offshore

df<- df%>%
  filter(ShoreProx!= "Offshore")

mod <- glmer(MAMU~Status + Eagle + Fish + ShoreSide +Status*Eagle+
               (1|pairs) , data = df, family = poisson)

summary(mod) #insignificant 

tidy_table <- tidy(mod, conf.int = TRUE)


ggplot(tidy_table, aes(x = term, y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_pointrange() +
  coord_flip() +
  labs(x = "", y = "Estimate", title = "") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10), plot.title = element_text(hjust = 0.5))

