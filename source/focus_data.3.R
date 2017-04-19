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
setkey(trt.age)
trt.age

# 1. EHT----
eht <- data.table(read.xlsx(file = file.path(DATA_HOME, "de-identified EHT.xlsx"),
                            sheetName = "All Visits",
                            rowIndex = 1:226,
                            colIndex = 1:20))
setkey(eht, Family.., D.O.B)
eht[, N := 1:.N, by = Family..]
eht[, 8:20] <- lapply(eht[, 8:20], function(a) as.numeric(as.character(a)))
eht$byear <- substr(eht$D.O.B, 1, 4)
eht
addmargins(table(eht$Family.., eht$N))

hist(eht$CA.in.Months, 
     100,
     xlab = "CA in Months",
     main = "Frequency of EHT Patients' Age at Visits")
abline(v = 55, lty = 2)
text(x = 55, y = 6, label = "55")

# Age groups +/- 6 months and aggregate----
dt36 <- droplevels(subset(eht, CA.in.Months >= 30 & CA.in.Months < 42 ))
dt36[, N := 1:.N, by = Family..]
addmargins(table(dt36$Family.., dt36$N))
dt36.avg <- dt36[, lapply(.SD, mean, na.rm = TRUE), by = Family.., .SDcols = names(dt36)[8:20]]
dt36.avg$age <- "30 to 41 months"

dt48 <- droplevels(subset(eht, CA.in.Months >= 42 & CA.in.Months < 54 ))
dt48[, N := 1:.N, by = Family..]
addmargins(table(dt48$Family.., dt48$N))
dt48.avg <- dt48[, lapply(.SD, mean, na.rm = TRUE), by = Family.., .SDcols = names(dt48)[8:20]]
dt48.avg$age <- "42 to 53 months"

dt60 <- droplevels(subset(eht, CA.in.Months >= 54))
dt60[, N := 1:.N, by = Family..]
addmargins(table(dt60$Family.., dt60$N))
dt60.avg <- dt60[, lapply(.SD, mean, na.rm = TRUE), by = Family.., .SDcols = names(dt60)[8:20]]
dt60.avg$age <- "54 months and over"

# Merge averages----
ceht <- rbindlist(list(dt36.avg, dt48.avg, dt60.avg))
ceht$age <- factor(ceht$age,
                   levels = c("30 to 41 months",
                              "42 to 53 months",
                              "54 months and over"))
# Merge with DOB
ceht <- merge(ceht, unique(eht[, c(1, 22)]), by = "Family..")
ceht$Family.. <- factor(ceht$Family..)

# Plots----
for (i in 2:14) {
  tmp <- ceht[, c(1, i, 15), with = FALSE]
  names(tmp) <- c("id", "y", "x")
  
  tiff(filename = paste("tmp/eht_", names(ceht)[i], ".tiff", sep = ""),
       height = 8,
       width = 8,
       units = 'in',
       res = 300,
       compression = "lzw+p")
  p1 <-   ggplot(data = tmp) +
    scale_x_discrete("Age group") + 
    scale_y_continuous("Score") + 
    ggtitle(paste("EHT:", names(ceht)[i])) +
    geom_rect(xmin = -Inf,
              xmax = Inf,
              ymin = 85,
              ymax = 115,
              fill = "white",
              alpha = 0.9,
              color = "black",
              linetype = "dashed") +
    geom_boxplot(aes(x = x,
                     y = y,
                     outlier.shape = NA)) +
    geom_point(aes(x = x,
                   y = y,
                   group = id,
                   colour = id),
               size = 3,
               alpha = 0.6,
               position = position_dodge(0.3)) + 
    geom_line(aes(x = x,
                  y = y,
                  group = id,
                  colour = id),
              size = 2,
              alpha = 0.6,
              position = position_dodge(0.3)) + 
    guides(colour = guide_legend(title = "ID",
                                 title.position="top",
                                 nrow = 5)) +
    theme(legend.position = "top",
          axis.text.x = element_text(angle = 45,
                                     hjust = 1))
  print(p1)
  
  graphics.off()
}

# 2. No EHT----
noeht <- data.table(read.xlsx(file = file.path(DATA_HOME, "de-identified No EHT.xlsx"),
                            sheetName = "All visits",
                            rowIndex = 1:297,
                            colIndex = 1:20))
setkey(noeht, Family.., D.O.B)
noeht[, N := 1:.N, by = Family..]
noeht[, 8:20] <- lapply(noeht[, 8:20], function(a) as.numeric(as.character(a)))
noeht$byear <- substr(noeht$D.O.B, 1, 4)
noeht
addmargins(table(noeht$Family.., noeht$N))

hist(noeht$CA.in.Months, 
     100,
     xlab = "CA in Months",
     main = "Frequency of No EHT Patients' Age at Visits")
abline(v = 55, lty = 2)
text(x = 55, y = 6, label = "55")

# Age groups +/- 6 months and aggregate----
dt36 <- droplevels(subset(noeht, CA.in.Months >= 30 & CA.in.Months < 42 ))
dt36[, N := 1:.N, by = Family..]
addmargins(table(dt36$Family.., dt36$N))
dt36.avg <- dt36[, lapply(.SD, mean, na.rm = TRUE), by = Family.., .SDcols = names(dt36)[8:20]]
dt36.avg$age <- "30 to 41 months"

dt48 <- droplevels(subset(noeht, CA.in.Months >= 42 & CA.in.Months < 54 ))
dt48[, N := 1:.N, by = Family..]
addmargins(table(dt48$Family.., dt48$N))
dt48.avg <- dt48[, lapply(.SD, mean, na.rm = TRUE), by = Family.., .SDcols = names(dt48)[8:20]]
dt48.avg$age <- "42 to 53 months"

dt60 <- droplevels(subset(noeht, CA.in.Months >= 54))
dt60[, N := 1:.N, by = Family..]
addmargins(table(dt60$Family.., dt60$N))
dt60.avg <- dt60[, lapply(.SD, mean, na.rm = TRUE), by = Family.., .SDcols = names(dt60)[8:20]]
dt60.avg$age <- "54 months and over"

# Merge averages----
noceht <- rbindlist(list(dt36.avg, dt48.avg, dt60.avg))
noceht$age <- factor(noceht$age,
                     levels = c("30 to 41 months",
                                "42 to 53 months",
                                "54 months and over"))
# Merge with DOB
noceht <- merge(noceht, unique(noeht[, c(1, 22)]), by = "Family..")
noceht$Family.. <- factor(noceht$Family..)

# Plots----
for (i in 2:14) {
  tmp <- noceht[, c(1, i, 15), with = FALSE]
  names(tmp) <- c("id", "y", "x")
  
  tiff(filename = paste("tmp/noeht_", names(noceht)[i], ".tiff", sep = ""),
       height = 8,
       width = 8,
       units = 'in',
       res = 300,
       compression = "lzw+p")
  p1 <- ggplot(data = tmp) +
    scale_x_discrete("Age group") + 
    scale_y_continuous("Score") + 
    ggtitle(paste("No EHT:", names(noceht)[i])) +
    geom_rect(xmin = -Inf,
              xmax = Inf,
              ymin = 85,
              ymax = 115,
              fill = "white",
              alpha = 0.9,
              color = "black",
              linetype = "dashed") +
    geom_boxplot(aes(x = x,
                     y = y,
                     outlier.shape = NA)) +
    geom_point(aes(x = x,
                   y = y,
                   group = id,
                   colour = id),
               size = 3,
               alpha = 0.6,
               position = position_dodge(0.3)) + 
    geom_line(aes(x = x,
                  y = y,
                  group = id,
                  colour = id),
              size = 2,
              alpha = 0.6,
              position = position_dodge(0.3)) + 
    guides(colour = guide_legend(title = "ID",
                                 title.position="top",
                                 nrow = 5)) +
    theme(legend.position = "top",
          axis.text.x = element_text(angle = 45,
                                     hjust = 1))
  print(p1)
  
  graphics.off()
}

# 3. Combine EHT and No EHT
ceht$eht <- "EHT"
noceht$eht <- "No EHT"
dt1 <- rbindlist(list(ceht, noceht))
dt1$eht <- factor(dt1$eht,
                  levels = c("EHT", "No EHT"))

dt1.60 <- droplevels(subset(dt1, age == "54 months and over"))

# Compare treatments----
i = 14
tmp <- dt1.60[, c(1, i, 15:17), with = FALSE]
names(tmp)[1:2] <- c("id", "y")

ggplot(data = tmp) +
  scale_x_discrete("Treatment") + 
  scale_y_continuous("Score") + 
  ggtitle(paste(names(dt1.60)[i], "at 52 months and above")) +
  geom_rect(xmin = -Inf,
            xmax = Inf,
            ymin = 85,
            ymax = 115,
            fill = "white",
            alpha = 0.9,
            color = "black",
            linetype = "dashed") +
  geom_boxplot(aes(x = eht,
                   y = y,
                   outlier.shape = NA)) +
  geom_point(aes(x = eht,
                 y = y,
                 group = id,
                 colour = id),
             size = 3,
             alpha = 0.6,
             position = position_dodge(0.3)) + 
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45,
                                   hjust = 1))

m1 <- lm(y ~ eht, data = tmp)
summary(m1)

m2 <- lm(y ~ eht + byear, data = tmp)
summary(m2)

m3 <- lm(y ~ eht + as.numeric(byear), data = tmp)
summary(m3)