########################################################
####Opgave 3 – Spørgeskema og Cumulative Link Models####
########################################################

library(ggplot2)
library(ordinal)
library(RColorBrewer)
library(dplyr)
library(tidyr)
library(gridExtra)

################################################
####Opgave 3.1 – Illustration af spørgsmålet####
################################################

###################################
####Data Preparation & Cleaning####
###################################

regnskab <- read.csv2("regnskab.csv", fileEncoding = "latin1")

regnskab_endelig <- regnskab

regnskab_endelig <- regnskab_endelig[,c(1:2,4,194:198,206:210,218:222)]

regnskab_kolonner <- c("Lånemulighed","Antal_Ansatte","Etableringsdato","Balance_2020","Balance_2019","Balance_2018",
                       "Balance_2017","Balance_2016","Afkastningsgrad_2020","Afkastningsgrad_2019",
                       "Afkastningsgrad_2018","Afkastningsgrad_2017","Afkastningsgrad_2016","Soliditetsgrad_2020",
                       "Soliditetsgrad_2019","Soliditetsgrad_2018","Soliditetsgrad_2017","Soliditetsgrad_2016")

colnames(regnskab_endelig) <- regnskab_kolonner

# Fjerner besvarelserne "Ved ikke" fra dataframe - i alt 51 obs svarende til cirka 1 % - (51 / 4484) * 100 = 1.1 %
regnskab_endelig <- subset(regnskab_endelig, Lånemulighed != "Ved ikke")

unique(regnskab_endelig$Lånemulighed)

regnskab_endelig$Lånemulighed[grep("^Dårlig$", regnskab_endelig$Lånemulighed)] <- "Dårlige"

# Juster rækkefølgen
regnskab_endelig$Lånemulighed <- factor(regnskab_endelig$Lånemulighed, 
                                        levels = c("Meget gode", "Gode", "Neutrale", 
                                                   "Dårlige", "Meget dårlige"))

regnskab_endelig$Etableringsdato <- as.Date(regnskab_endelig$Etableringsdato, format = "%d-%m-%Y")

# Fjerner alle NA værdier fra datasættet - i alt 62 obs svarende til cirka 1 % (62 / 4395) * 100 = 1.3 %
regnskab_endelig <- na.omit(regnskab_endelig)

########################################
####Diagram fordelt på antal ansatte####
########################################

ggplot(regnskab_endelig, aes(x = Lånemulighed, fill = Lånemulighed)) + 
  geom_bar() + 
  labs(title = "Fordeling af Lånemulighed", 
       x = "Lånemulighed", 
       y = "Antal observationer") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set3") +
  geom_text(stat = 'count', aes(label = scales::percent(..count../sum(..count..))), 
            vjust = -0.5, size = 5)

###########################################
####Opgave 3.2 – Cumulative Link Models####
###########################################

###########################################
####Cumulative Link Models af udvikling####
###########################################

regnskab_udvikling <- regnskab_endelig[,c(1,9:13)]

for (i in 2016:2019) {
  regnskab_udvikling[[paste("udvikling_", i, "_", i+1, sep = "")]] <- regnskab_udvikling[[paste("Afkastningsgrad_", i+1, sep = "")]] - regnskab_udvikling[[paste("Afkastningsgrad_", i, sep = "")]]
}

for (row in 1:nrow(regnskab_udvikling)) {
  udvikling <- c()
  
  for (i in 2016:2019) {
    udvikling <- c(udvikling, regnskab_udvikling[row, paste("udvikling_", i, "_", i+1, sep = "")])
  }
  
  gennemsnit_udvikling <- mean(udvikling)
  
  regnskab_udvikling[row, "gennemsnit_udvikling"] <- gennemsnit_udvikling
}

udvikling_clm <- clm(Lånemulighed ~ Afkastningsgrad_2020 + gennemsnit_udvikling, data = regnskab_udvikling)

summary(udvikling_clm)

###################################################
####Cumulative Link Models af nøgletal for 2020####
###################################################

regnskab_endelig$log_Balance_2020 <- log(regnskab_endelig$Balance_2020)

regnskab_endelig$VirksomhedensAlder <- floor(as.numeric(difftime(Sys.Date(), regnskab_endelig$Etableringsdato, units = "weeks")) / 52.25)

regnskab_clm <- clm(Lånemulighed ~ Antal_Ansatte + VirksomhedensAlder + log_Balance_2020 + 
                      Afkastningsgrad_2020 + Soliditetsgrad_2020, data = regnskab_endelig)

summary(regnskab_clm)

regnskab_clm2 <- clm(Lånemulighed ~ Antal_Ansatte +  log_Balance_2020, data = regnskab_endelig)

summary(regnskab_clm2)


#########################################################
####Opgave 3.3 – Illustration af forklarende variable####
#########################################################

######################################
####Illustration af Afkastninsgrad####
######################################

regnskab_endelig$Lånemuligheder <- ifelse(regnskab_endelig$Lånemulighed %in% c("Meget gode", "Gode"), 
                                               "Meget gode/Gode", 
                                               ifelse(regnskab_endelig$Lånemulighed %in% c("Meget dårlige", "Dårlige"), 
                                                      "Meget dårlige/Dårlige", 
                                                      "Neutral"))

regnskab_endelig$Lånemuligheder <- factor(regnskab_endelig$Lånemuligheder,
                                               levels = c("Meget dårlige/Dårlige", "Neutral", "Meget gode/Gode"))

gennemsnit_afkastning <- regnskab_endelig %>%
  group_by(Lånemuligheder) %>%
  summarise(
    gennemsnit_2020 = mean(Afkastningsgrad_2020, na.rm = TRUE),
    gennemsnit_2019 = mean(Afkastningsgrad_2019, na.rm = TRUE),
    gennemsnit_2018 = mean(Afkastningsgrad_2018, na.rm = TRUE),
    gennemsnit_2017 = mean(Afkastningsgrad_2017, na.rm = TRUE),
    gennemsnit_2016 = mean(Afkastningsgrad_2016, na.rm = TRUE)
  )

gennemsnit_afkastning_long <- gennemsnit_afkastning %>%
  pivot_longer(cols = starts_with("gennemsnit"), 
               names_to = "År", 
               values_to = "Afkastningsgrad") %>%
  mutate(År = gsub("gennemsnit_", "", År))

ggplot(gennemsnit_afkastning_long, aes(x = År, y = Afkastningsgrad, fill = Lånemuligheder)) +
  geom_bar(stat = "identity", position = "dodge") +  # position = "dodge" placerer søjlerne ved siden af hinanden
  labs(x = "År", y = "Afkastningsgrad", title = "Gennemsnit af Afkastningsgrad pr. år og Lånemulighed") +
  scale_fill_manual(values = c("Meget dårlige/Dårlige" = "#6C6C6C", 
                               "Neutral" = "#B4B4B4", 
                               "Meget gode/Gode" = "#1F77B4")) +  # Farver for hver Lånemulighed gruppe
  scale_y_continuous(breaks = seq(0, max(gennemsnit_afkastning_long$Afkastningsgrad, na.rm = TRUE), by = 5)) +  # Juster intervallerne på y-aksen
  theme_minimal() +
  theme(
    legend.position = "top",
    axis.text.x = element_text(size = 12, angle = 45, hjust = 1),  # Drej x-aksen for bedre læsbarhed
    axis.title.y = element_text(size = 14),
    axis.title.x = element_text(size = 14),
    plot.margin = margin(t = 10, r = 0, b = 0, l = 0)
  )

######################################
####Illustration af Soliditetsgrad####
######################################

gennemsnit_soliditet <- regnskab_endelig %>%
  group_by(Lånemuligheder) %>%
  summarise(
    gennemsnit_2020 = mean(Soliditetsgrad_2020, na.rm = TRUE),
    gennemsnit_2019 = mean(Soliditetsgrad_2019, na.rm = TRUE),
    gennemsnit_2018 = mean(Soliditetsgrad_2018, na.rm = TRUE),
    gennemsnit_2017 = mean(Soliditetsgrad_2017, na.rm = TRUE),
    gennemsnit_2016 = mean(Soliditetsgrad_2016, na.rm = TRUE)
  )

gennemsnit_soliditet_long <- gennemsnit_soliditet %>%
  pivot_longer(cols = starts_with("gennemsnit"), 
               names_to = "År", 
               values_to = "Soliditetsgrad") %>%
  mutate(År = gsub("gennemsnit_", "", År)) 

ggplot(gennemsnit_soliditet_long, aes(x = År, y = Soliditetsgrad, fill = Lånemuligheder)) +
  geom_bar(stat = "identity", position = "dodge") +  # position = "dodge" placerer søjlerne ved siden af hinanden
  labs(x = "År", y = "Soliditetsgrad", title = "Gennemsnit af Soliditetsgrad pr. år og Lånemulighed") +
  scale_fill_manual(values = c("Meget dårlige/Dårlige" = "#6C6C6C", 
                               "Neutral" = "#B4B4B4", 
                               "Meget gode/Gode" = "#1F77B4")) +  # Farver for hver Lånemulighed gruppe
  scale_y_continuous(breaks = seq(0, max(gennemsnit_soliditet_long$Soliditetsgrad, na.rm = TRUE), by = 5)) +  # Juster intervallerne på y-aksen
  theme_minimal() +
  theme(
    legend.position = "top",
    axis.text.x = element_text(size = 12, angle = 45, hjust = 1),  # Drej x-aksen for bedre læsbarhed
    axis.title.y = element_text(size = 14),
    axis.title.x = element_text(size = 14),
    plot.margin = margin(t = 10, r = 0, b = 0, l = 0)
  )

###############################
####Illustration af Balance####
###############################

regnskab_balance <- regnskab_endelig[,c(4:8,21)]
regnskab_balance$log_Balance_2020 <- log(regnskab_balance$Balance_2020)
regnskab_balance$log_Balance_2019 <- log(regnskab_balance$Balance_2019)
regnskab_balance$log_Balance_2018 <- log(regnskab_balance$Balance_2018)
regnskab_balance$log_Balance_2017 <- log(regnskab_balance$Balance_2017)
regnskab_balance$log_Balance_2016 <- log(regnskab_balance$Balance_2016)

gennemsnit_balance <- regnskab_balance %>%
  group_by(Lånemuligheder) %>%
  summarise(
    gennemsnit_2020 = mean(log_Balance_2020, na.rm = TRUE),
    gennemsnit_2019 = mean(log_Balance_2019, na.rm = TRUE),
    gennemsnit_2018 = mean(log_Balance_2018, na.rm = TRUE),
    gennemsnit_2017 = mean(log_Balance_2017, na.rm = TRUE),
    gennemsnit_2016 = mean(log_Balance_2016, na.rm = TRUE)
  )

gennemsnit_balance_long <- gennemsnit_balance %>%
  pivot_longer(cols = starts_with("gennemsnit"), 
               names_to = "År", 
               values_to = "Balance") %>%
  mutate(År = gsub("gennemsnit_", "", År)) 

ggplot(gennemsnit_balance_long, aes(x = År, y = Balance, fill = Lånemuligheder)) +
  geom_bar(stat = "identity", position = "dodge") +  # position = "dodge" placerer søjlerne ved siden af hinanden
  labs(x = "År", y = "Balance (log)", title = "Gennemsnit af Balance log og Lånemulighed") +
  scale_fill_manual(values = c("Meget dårlige/Dårlige" = "#6C6C6C", 
                               "Neutral" = "#B4B4B4", 
                               "Meget gode/Gode" = "#1F77B4")) +  # Farver for hver Lånemulighed gruppe
  scale_y_continuous(breaks = seq(0, max(gennemsnit_balance_long$Balance, na.rm = TRUE), by = 5)) +  # Juster intervallerne på y-aksen
  theme_minimal() +
  theme(
    legend.position = "top",
    axis.text.x = element_text(size = 12, angle = 45, hjust = 1),  # Drej x-aksen for bedre læsbarhed
    axis.title.y = element_text(size = 14),
    axis.title.x = element_text(size = 14),
    plot.margin = margin(t = 10, r = 0, b = 0, l = 0)
)
