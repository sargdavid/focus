# Project: Early Hormonal Therapy (EHT) treatment effect on 
#          first 5 years of life in boys who are affected with 47, XXY (Klinefelter syndrome)
# Created: 03/24/2017
# Authors: Davit Sargsyan, Lyudvig Petrosyan
#**********************************************************
# Header----
DATA_HOME <- "C:/Users/dsargsy/Dropbox/Projects with Davit and Lyudvig/Data"
require(data.table)
require(xlsx)
require(ggplot2)

# Data----
# Age when treatment began----
trt.age <- data.table(read.xlsx(file = file.path(DATA_HOME, "age of treatment and key for abbreviations.xlsx"),
                                sheetName = "Average Age of Tx",
                                rowIndex = 1:50,
                                colIndex = 1:4))


# 1. EHT----
# a. Pretreatment
eht.pret <- data.table(read.xlsx(file = file.path(DATA_HOME, "de-identified EHT.xlsx"),
                                 sheetName = "Pre T",
                                 rowIndex = 1:28,
                                 colIndex = 1:20))
eht.pret$VISIT <- "PreT"
eht.pret$EHT <- "EHT"
eht.pret

# b. Nearest post treatment
eht.post0 <- data.table(read.xlsx(file = file.path(DATA_HOME, "de-identified EHT.xlsx"),
                                  sheetName = "Nearest Post T",
                                  rowIndex = 1:33,
                                  colIndex = 1:20))
eht.post0$VISIT <- "Post0"
eht.post0$EHT <- "EHT"
eht.post0

# c. Post 24 month
eht.post24 <- data.table(read.xlsx(file = file.path(DATA_HOME, "de-identified EHT.xlsx"),
                                   sheetName = "Post 24mo.",
                                   rowIndex = 1:25,
                                   colIndex = 1:20))
eht.post24$VISIT <- "Post24"
eht.post24$EHT <- "EHT"
eht.post24

# d. Post 36 month
eht.post36 <- data.table(read.xlsx(file = file.path(DATA_HOME, "de-identified EHT.xlsx"),
                                   sheetName = "Post 36mo",
                                   rowIndex = 1:36,
                                   colIndex = 1:20))
eht.post36$VISIT <- "Post36"
eht.post36$EHT <- "EHT"
eht.post36

# e. Post 60 month
eht.post60 <- data.table(read.xlsx(file = file.path(DATA_HOME, "de-identified EHT.xlsx"),
                                   sheetName = "Post 60mo",
                                   rowIndex = 1:28,
                                   colIndex = 1:20))
eht.post60$VISIT <- "Post60"
eht.post60$EHT <- "EHT"
eht.post60

# f. 36 and under
eht.upto36 <- data.table(read.xlsx(file = file.path(DATA_HOME, "de-identified EHT.xlsx"),
                                   sheetName = "36 mo and under",
                                   rowIndex = 1:47,
                                   colIndex = 1:20))
eht.upto36$VISIT <- "Upto36"
eht.upto36$EHT <- "EHT"
eht.upto36

# g. 37 to 71
eht.37to71 <- data.table(read.xlsx(file = file.path(DATA_HOME, "de-identified EHT.xlsx"),
                                   sheetName = "37 to 71 mo.",
                                   rowIndex = 1:29,
                                   colIndex = 1:20))
eht.37to71$VISIT <- "From37to71"
eht.37to71$EHT <- "EHT"
eht.37to71

# Combine EHT----
eht <- rbindlist(list(eht.pret,
                      eht.post0,
                      eht.post24,
                      eht.post36,
                      eht.post60))
eht$Family.. <- as.numeric(as.character(eht$Family..))
eht$CA.in.Months <- as.numeric(as.character(eht$CA.in.Months))
eht$PLS.AC <- as.numeric(as.character(eht$PLS.AC))
eht$PLS.EC <- as.numeric(as.character(eht$PLS.EC))
eht$EL1 <- as.numeric(as.character(eht$EL1))
eht$RL1 <- as.numeric(as.character(eht$RL1))
eht$EOWPVT1 <- as.numeric(as.character(eht$EOWPVT1))
eht$ROWPVT1 <- as.numeric(as.character(eht$ROWPVT1))
eht$Bayley.MDI <- as.numeric(as.character(eht$Bayley.MDI))
eht$Bayley.PDI <- as.numeric(as.character(eht$Bayley.PDI))
eht$Bayley.Language <- as.numeric(as.character(eht$Bayley.Language))
eht$VMI1 <- as.numeric(as.character(eht$VMI1))
eht$X.Visual.Perception.1. <- as.numeric(as.character(eht$X.Visual.Perception.1.))
eht$Motor.Coordination1 <- as.numeric(as.character(eht$Motor.Coordination1))
eht$VIQ.VCI <- as.numeric(as.character(eht$VIQ.VCI))
eht$VISIT <- factor(eht$VISIT,
                    levels = c("PreT",
                               "Post0",
                               "Post24",
                               "Post36",
                               "Post60"))
eht$EHT <- factor(eht$EHT, levels = unique(eht$EHT))

# Merge with start-of-treatment age
eht <- merge(eht, trt.age[, c(1, 4), with = FALSE],
             by = "Family..")
eht$Family.. <- factor(eht$Family..)

setkey(eht, Family.., VISIT)
eht
summary(eht)

# 2. No EHT----
# a. Matched 7 month (?)
noeht.m7 <- data.table(read.xlsx(file = file.path(DATA_HOME, "de-identified No EHT.xlsx"),
                                 sheetName = "Matched 7mo",
                                 rowIndex = 1:19,
                                 colIndex = 1:20))
noeht.m7$VISIT <- "Matched7"
noeht.m7$EHT <- "No EHT"
noeht.m7

# b. Matched 17 month
noeht.m17 <- data.table(read.xlsx(file = file.path(DATA_HOME, "de-identified No EHT.xlsx"),
                                  sheetName = "Matched 17mo",
                                  rowIndex = 1:36,
                                  colIndex = 1:20))
noeht.m17$VISIT <- "Matched17"
noeht.m17$EHT <- "No EHT"
noeht.m17

# c. Matched 24 month
noeht.m24 <- data.table(read.xlsx(file = file.path(DATA_HOME, "de-identified No EHT.xlsx"),
                                  sheetName = "Matched 24mo",
                                  rowIndex = 1:33,
                                  colIndex = 1:20))
noeht.m24$VISIT <- "Matched24"
noeht.m24$EHT <- "No EHT"
noeht.m24

# d. Matched 17 month
noeht.upto36 <- data.table(read.xlsx(file = file.path(DATA_HOME, "de-identified No EHT.xlsx"),
                                     sheetName = "under 36mo",
                                     rowIndex = 1:99,
                                     colIndex = 1:20))
noeht.upto36$VISIT <- "Upto36"
noeht.upto36$EHT <- "No EHT"
noeht.upto36

# e. Matched 17 month
noeht.37to71 <- data.table(read.xlsx(file = file.path(DATA_HOME, "de-identified No EHT.xlsx"),
                                     sheetName = "37 to 71",
                                     rowIndex = 1:62,
                                     colIndex = 1:20))
noeht.37to71$VISIT <- "From37to71"
noeht.37to71$EHT <- "No EHT"
noeht.37to71

# Combine No EHT----
noeht <- rbindlist(list(noeht.m7,
                        noeht.m17,
                        noeht.m24))
# noeht.upto36,
# noeht.37to71))
noeht$Family.. <- factor(noeht$Family..)
noeht$CA.in.Months <- as.numeric(as.character(noeht$CA.in.Months))
noeht$PLS.AC <- as.numeric(as.character(noeht$PLS.AC))
noeht$PLS.EC <- as.numeric(as.character(noeht$PLS.EC))
noeht$EL1 <- as.numeric(as.character(noeht$EL1))
noeht$RL1 <- as.numeric(as.character(noeht$RL1))
noeht$EOWPVT1 <- as.numeric(as.character(noeht$EOWPVT1))
noeht$ROWPVT1 <- as.numeric(as.character(noeht$ROWPVT1))
noeht$Bayley.MDI <- as.numeric(as.character(noeht$Bayley.MDI))
noeht$Bayley.PDI <- as.numeric(as.character(noeht$Bayley.PDI))
noeht$Bayley.Language <- as.numeric(as.character(noeht$Bayley.Language))
noeht$VMI1 <- as.numeric(as.character(noeht$VMI1))
noeht$X.Visual.Perception.1. <- as.numeric(as.character(noeht$X.Visual.Perception.1.))
noeht$Motor.Coordination1 <- as.numeric(as.character(noeht$Motor.Coordination1))
noeht$VIQ.VCI <- as.numeric(as.character(noeht$VIQ.VCI))
noeht$VISIT <- factor(noeht$VISIT,
                      levels = c("Matched7",
                                 "Matched17",
                                 "Matched24"))
noeht$EHT <- factor(noeht$EHT, levels = unique(noeht$EHT))

setkey(noeht, Family.., VISIT)
noeht
summary(eht)

# Combine EHT and No EHT----
dt1 <- rbindlist(list(eht.upto36,
                      eht.37to71,
                      noeht.upto36,
                      noeht.37to71))
dt1$Family.. <- factor(dt1$Family..)
dt1$CA.in.Months <- as.numeric(as.character(dt1$CA.in.Months))
dt1$PLS.AC <- as.numeric(as.character(dt1$PLS.AC))
dt1$PLS.EC <- as.numeric(as.character(dt1$PLS.EC))
dt1$EL1 <- as.numeric(as.character(dt1$EL1))
dt1$RL1 <- as.numeric(as.character(dt1$RL1))
dt1$EOWPVT1 <- as.numeric(as.character(dt1$EOWPVT1))
dt1$ROWPVT1 <- as.numeric(as.character(dt1$ROWPVT1))
dt1$Bayley.MDI <- as.numeric(as.character(dt1$Bayley.MDI))
dt1$Bayley.PDI <- as.numeric(as.character(dt1$Bayley.PDI))
dt1$Bayley.Language <- as.numeric(as.character(dt1$Bayley.Language))
dt1$VMI1 <- as.numeric(as.character(dt1$VMI1))
dt1$X.Visual.Perception.1. <- as.numeric(as.character(dt1$X.Visual.Perception.1.))
dt1$Motor.Coordination1 <- as.numeric(as.character(dt1$Motor.Coordination1))
dt1$VIQ.VCI <- as.numeric(as.character(dt1$VIQ.VCI))
dt1$VISIT <- factor(dt1$VISIT,
                    levels = c("Upto36",
                               "From37to71"))
dt1$EHT <- factor(dt1$EHT, levels = unique(dt1$EHT))

setkey(dt1, Family.., VISIT)
dt1
summary(dt1)
# Plots----
i=8 # Preschool Language Scale Auditory Comprehension 

# a. EHT
tmp1 <- subset(eht, 
               c(!is.na(eht[, i, with = FALSE])),
               select = c(1, 7, i, 21))
names(tmp1)[3] <- "y"
tmp1[, nvis := 1:.N,
     by = Family..]

t1 <- table(tmp1$Family..,
            tmp1$VISIT)
t1
write.csv(t1, file = "tmp/t1.csv")

# tiff(filename = paste("tmp/", names(dt1)[i], ".1.tiff", sep = ""),
#      height = 5,
#      width = 10,
#      units = 'in',
#      res = 300,
#      compression = "lzw+p")
p1 <- ggplot(tmp1,
             aes(x = as.numeric(VISIT),
                 y = y,
                 colour = Family..,
                 group = Family..)) +
  geom_rect(xmin = -Inf,
            xmax = Inf,
            ymin = 85,
            ymax = 115,
            fill = "white",
            alpha = 0.9,
            color = "black",
            linetype = "dashed") +
  geom_line(size = 0.1,
            col = "black") +
  geom_point(size = 3,
             alpha = 0.5) +
  theme(legend.position = "none") +
  scale_x_continuous("Visit",
                     breaks = 1:nlevels(tmp$VISIT),
                     labels = levels(tmp$VISIT)) +
  scale_y_continuous(names(eht)[i]) +
  ggtitle("EHT")
print(p1)
# graphics.off()

# b. No EHT
tmp2 <- subset(noeht, 
               c(!is.na(noeht[, i, with = FALSE])),
               select = c(1, 7, i, 21))
names(tmp2)[3] <- "y"
tmp2[, nvis := 1:.N,
     by = Family..]

t2 <- table(tmp2$Family..,
            tmp2$VISIT)
t2
sum(t2[, 1]/nrow(t2))
write.csv(t2, file = "tmp/t2.csv")

# tiff(filename = paste("tmp/", names(dt1)[i], ".2.tiff", sep = ""),
#      height = 5,
#      width = 10,
#      units = 'in',
#      res = 300,
#      compression = "lzw+p")
p2 <- ggplot(tmp2,
             aes(x = as.numeric(VISIT),
                 y = y,
                 colour = Family..,
                 group = Family..)) +
  geom_rect(xmin = -Inf,
            xmax = Inf,
            ymin = 85,
            ymax = 115,
            fill = "white",
            alpha = 0.9,
            color = "black",
            linetype = "dashed") +
  geom_line(size = 0.1,
            col = "black") +
  geom_point(size = 3,
             alpha = 0.5) +
  theme(legend.position = "none") +
  scale_x_continuous("Visit",
                     breaks = 1:nlevels(tmp$VISIT),
                     labels = levels(tmp$VISIT)) +
  scale_y_continuous(names(eht)[i]) +
  ggtitle("No EHT")
print(p2)
# graphics.off()

# c. Combined
tmp3 <- subset(dt1, 
               c(!is.na(dt1[, i, with = FALSE])),
               select = c(1, 7, i, 21, 22))
names(tmp3)[3] <- "y"
tmp3[, nvis := 1:.N,
     by = Family..]

t3 <- table(tmp3$Family..,
            tmp3$VISIT)
t3
write.csv(t3, file = "tmp/t3.csv")

p3 <- ggplot(tmp3,
             aes(x = as.numeric(VISIT),
                 y = y,
                 colour = Family..,
                 group = Family..)) +
  facet_wrap(~ EHT,
             nrow = 1) +
  geom_rect(xmin = -Inf,
            xmax = Inf,
            ymin = 85,
            ymax = 115,
            fill = "white",
            alpha = 0.9,
            color = "black",
            linetype = "dashed") +
  geom_line(size = 0.1,
            col = "black") +
  geom_point(size = 3,
             alpha = 0.5) +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45,
                                   hjust = 1)) +
  scale_x_continuous("Visit",
                     breaks = 1:nlevels(tmp3$VISIT),
                     labels = levels(tmp3$VISIT)) +
  scale_y_continuous(names(eht)[i]) +
  ggtitle("EHT + No EHT")
print(p3)

setkey(tmp3, Family.., EHT)
keep.id <- as.character(unique(tmp3$Family..[tmp3$nvis == 2]))
tmp4 <- subset(tmp3, Family.. %in% keep.id)
setkey(tmp4, Family.., EHT)

# NOTE: remove ID_654: in both, EHT (37-71) and No HT (<37)
tmp4 <- subset(tmp4, Family.. != 654)
p4 <- ggplot(tmp4,
             aes(x = as.numeric(VISIT),
                 y = y,
                 colour = Family..,
                 group = Family..)) +
  facet_wrap(~ EHT,
             nrow = 1) +
  geom_rect(xmin = -Inf,
            xmax = Inf,
            ymin = 85,
            ymax = 115,
            fill = "white",
            alpha = 0.9,
            color = "black",
            linetype = "dashed") +
  geom_line(size = 0.1,
            col = "black") +
  geom_point(size = 3,
             alpha = 0.5) +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45,
                                   hjust = 1)) +
  scale_x_continuous("Visit",
                     breaks = 1:nlevels(tmp3$VISIT),
                     labels = levels(tmp3$VISIT)) +
  scale_y_continuous(names(eht)[i]) +
  ggtitle("EHT + No EHT, Coplete Pairs")
print(p4)

# Calculate differences: Delta = (37to71) - (<37)
tmp4
tmp4[, dtime := y[VISIT == "From37to71"] - y[VISIT == "Upto36"],
     by = Family..]

tmp5 <- unique(subset(tmp4, select = c(1, 5, 7)))
tmp5

boxplot(tmp5$dtime ~ tmp5$EHT)

p5 <- ggplot(tmp5) +
  geom_boxplot(aes(x = EHT,
                   y = dtime,
                   outlier.shape = NA)) +
  geom_point(aes(x = EHT,
                 y = dtime,
                 group = Family..,
                 colour = Family..),
             size = 3,
             alpha = 0.6,
             position = position_dodge(0.3)) + 
  guides(colour = guide_legend(title = "ID",
                               title.position="top",
                               nrow = 1)) +
  theme(legend.position = "none") +
  scale_x_discrete("Treatment") +
  scale_y_continuous(paste("Delta", names(eht)[i])) +
  ggtitle("Change in Scores Over Time: (37 to 71 mo) - (<37 mo)")
print(p5)

# Statistics----
# Linear mixed effect model for differences in changes over time in two treatments
require(lme4)
m1 <- lmer(y ~ EHT + VISIT + (1 | Family..),
     data = tmp4)
summary(m1)

# extract coefficients
coefs <- data.frame(coef(summary(m1)))
# use normal distribution to approximate p-value
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
coefs

m1 <- lmer(y ~ EHT*VISIT + (1 | Family..),
           data = tmp4)
summary(m1)

# extract coefficients
coefs <- data.frame(coef(summary(m1)))
# use normal distribution to approximate p-value
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
coefs