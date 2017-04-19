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
require(lmerTest)

# Data----
# Treatment annotation----
trt.anno <- fread(file = file.path(DATA_HOME, "key for abbreviations.csv"))

# Load data----
## 1. EHT----
eht <- data.table(read.xlsx(file = file.path(DATA_HOME, "de-identified EHT.xlsx"),
                            sheetName = "All Visits",
                            rowIndex = 1:214,
                            colIndex = c(1, 3, 6:21)))
eht$D.O.B <- as.Date(eht$D.O.B, format = "%Y/%m/%d")
eht$D.O.V <- as.Date(eht$D.O.V, format = "%Y/%m/%d")
eht$Group <- "EHT"
eht

## 2. No EHT----
noeht <- data.table(read.xlsx(file = file.path(DATA_HOME, "de-identified No EHT.xlsx"),
                              sheetName = "All visits",
                              rowIndex = 1:297,
                              colIndex = c(1, 3, 6:21)))
noeht$D.O.B <- as.Date(noeht$D.O.B, format = "%Y/%m/%d")
noeht$D.O.V <- as.Date(noeht$D.O.V, format = "%Y/%m/%d")
noeht$Group <- "No EHT"
noeht

## 3. Controls----
ctrl <- data.table(read.xlsx(file = file.path(DATA_HOME, "de-identified controls.xlsx"),
                             sheetName = "All Visits",
                             rowIndex = 1:21,
                             colIndex = c(1, 3, 5:20)))
ctrl$D.O.B <- as.Date(ctrl$D.O.B, format = "%Y/%m/%d")
ctrl$D.O.V <- as.Date(ctrl$D.O.V, format = "%Y/%m/%d")
ctrl$Group <- "Control"
ctrl

# 4. Combine all treatment groups----
dt1 <- rbindlist(list(eht, noeht, ctrl))
dt1[, 4:18] <- lapply(dt1[, 4:18], function(a) as.numeric(as.character(a)))
dt1$Family.. <- factor(dt1$Family..)
dt1$Group <- factor(dt1$Group,
                    c("Control",
                      "No EHT",
                      "EHT"))
setkey(dt1, Family.., D.O.B)
dt1[, N := 1:.N, by = Family..]
dt1$byear <- substr(dt1$D.O.B, 1, 4)

dt1

# Plots
for (i in 5:18) {
  tmp <- dt1[, c(1, 4, i, 19, 21), with = FALSE]
  names(tmp) <- c("id", "month", "y", "grp", "year")
  tmp <- subset(tmp, !is.na(tmp$y))
  
  tiff(filename = paste("tmp/", names(dt1)[i], ".tiff", sep = ""),
       height = 6,
       width = 18,
       units = 'in',
       res = 300,
       compression = "lzw+p")
  p1 <- ggplot(data = tmp) +
    facet_wrap(~ grp,
               nrow = 1) +
    geom_rect(xmin = -Inf,
              xmax = Inf,
              ymin = 85,
              ymax = 115,
              fill = "white",
              alpha = 0.9,
              color = "black",
              linetype = "dashed") +
    scale_x_continuous("Age (months)") + 
    scale_y_continuous("Score") + 
    ggtitle(trt.anno$`Full Name`[i - 4]) +
    geom_point(aes(x = month,
                   y = y,
                   group = id,
                   colour = id),
               size = 2,
               alpha = 0.6) + 
    geom_line(aes(x = month,
                  y = y,
                  group = id,
                  colour = id),
              size = 1,
              alpha = 0.6) + 
    theme(legend.position = "none")
  print(p1)
  
  graphics.off()
}

# Separate 18 to 36 mo----
d1836 <- droplevels(subset(dt1, CA.in.Months >= 18 & CA.in.Months <= 36))
d1836

# Plots
for (i in 5:13) {
  tmp <- d1836[, c(1, 4, i, 19, 21), with = FALSE]
  names(tmp) <- c("id", "month", "y", "grp", "year")
  tmp <- subset(tmp, !is.na(tmp$y))
  
  tiff(filename = paste("tmp/", names(dt1)[i], "_18to36mo.tiff", sep = ""),
       height = 6,
       width = 18,
       units = 'in',
       res = 300,
       compression = "lzw+p")
  p1 <- ggplot(data = tmp) +
    facet_wrap(~ grp,
               nrow = 1) +
    geom_rect(xmin = -Inf,
              xmax = Inf,
              ymin = 85,
              ymax = 115,
              fill = "white",
              alpha = 0.9,
              color = "black",
              linetype = "dashed") +
    scale_x_continuous("Age (months)") + 
    scale_y_continuous("Score") + 
    ggtitle(trt.anno$`Full Name`[i - 4]) +
    geom_point(aes(x = month,
                   y = y,
                   group = id,
                   colour = id),
               size = 2,
               alpha = 0.6) + 
    geom_line(aes(x = month,
                  y = y,
                  group = id,
                  colour = id),
              size = 1,
              alpha = 0.6) + 
    theme(legend.position = "none")
  print(p1)
  
  graphics.off()
}

# Mixed effect
# c(5:13)
i=13
tmp <- droplevels(d1836[Group != "Control", c(1, 4, i, 19, 21), with = FALSE])
tmp
names(tmp) <- c("id", "month", "y", "grp", "year")
tmp <- subset(tmp, !is.na(tmp$y))
m1 <- lmer(y ~ grp + (1 | id) + (1 | year), data = tmp)
summary(m1)
trt.anno$`Full Name`

# Over 36 mo----
dover36 <- droplevels(subset(dt1, CA.in.Months > 36))
dover36
# Plots
for (i in c(5, 6, 9, 10, 14:18)) {
  tmp <- dover36[, c(1, 4, i, 19, 21), with = FALSE]
  names(tmp) <- c("id", "month", "y", "grp", "year")
  tmp <- subset(tmp, !is.na(tmp$y))
  
  tiff(filename = paste("tmp/", names(dt1)[i], "_over36mo.tiff", sep = ""),
       height = 6,
       width = 18,
       units = 'in',
       res = 300,
       compression = "lzw+p")
  p1 <- ggplot(data = tmp) +
    facet_wrap(~ grp,
               nrow = 1) +
    geom_rect(xmin = -Inf,
              xmax = Inf,
              ymin = 85,
              ymax = 115,
              fill = "white",
              alpha = 0.9,
              color = "black",
              linetype = "dashed") +
    scale_x_continuous("Age (months)") + 
    scale_y_continuous("Score") + 
    ggtitle(trt.anno$`Full Name`[i - 4]) +
    geom_point(aes(x = month,
                   y = y,
                   group = id,
                   colour = id),
               size = 2,
               alpha = 0.6) + 
    geom_line(aes(x = month,
                  y = y,
                  group = id,
                  colour = id),
              size = 1,
              alpha = 0.6) + 
    theme(legend.position = "none")
  print(p1)
  
  graphics.off()
}

# Mixed effect
# c(5, 6, 9, 10, 14:18)
i=18
tmp <- droplevels(dover36[Group != "Control", c(1, 4, i, 19, 21), with = FALSE])
tmp
names(tmp) <- c("id", "month", "y", "grp", "year")
tmp <- subset(tmp, !is.na(tmp$y))
m1 <- lmer(y ~ grp + (1 | id) + (1 | year), data = tmp)
summary(m1)
trt.anno$`Full Name`

# Separate 18 mo and over----
d18over <- droplevels(subset(dt1, CA.in.Months >= 18))
d18over
# Plots
for (i in c(5, 6, 9, 10)) {
  tmp <- d18over[, c(1, 4, i, 19, 21), with = FALSE]
  names(tmp) <- c("id", "month", "y", "grp", "year")
  tmp <- subset(tmp, !is.na(tmp$y))
  
  tiff(filename = paste("tmp/", names(dt1)[i], "_18mo.over.tiff", sep = ""),
       height = 6,
       width = 18,
       units = 'in',
       res = 300,
       compression = "lzw+p")
  p1 <- ggplot(data = tmp) +
    facet_wrap(~ grp,
               nrow = 1) +
    geom_rect(xmin = -Inf,
              xmax = Inf,
              ymin = 85,
              ymax = 115,
              fill = "white",
              alpha = 0.9,
              color = "black",
              linetype = "dashed") +
    scale_x_continuous("Age (months)") + 
    scale_y_continuous("Score") + 
    ggtitle(trt.anno$`Full Name`[i - 4]) +
    geom_point(aes(x = month,
                   y = y,
                   group = id,
                   colour = id),
               size = 2,
               alpha = 0.6) + 
    geom_line(aes(x = month,
                  y = y,
                  group = id,
                  colour = id),
              size = 1,
              alpha = 0.6) + 
    theme(legend.position = "none")
  print(p1)
  
  graphics.off()
}

# Mixed effect
# c(5, 6, 9, 10)
i=10
tmp <- droplevels(d18over[Group != "Control", c(1, 4, i, 19, 21), with = FALSE])
tmp
names(tmp) <- c("id", "month", "y", "grp", "year")
tmp <- subset(tmp, !is.na(tmp$y))
m1 <- lmer(y ~ grp + (1 | id) + (1 | year), data = tmp)
summary(m1)
trt.anno$`Full Name`