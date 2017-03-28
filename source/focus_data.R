# Project: Early Hormonal Therapy (EHT) treatment effect on 
#          first 5 years of life in boys who are affected with 47, XXY (Klinefelter syndrome)
# Created: 03/24/2017
# Authors: Davit Sargsyan, Lyudvig Petrosyan
#**********************************************************
# Header----
DATA_HOME <- "C:/git_local/data/focus"
require(data.table)
require(xlsx)
require(ggplot2)

# Data----
# 1. Controls----
dt.ctrl.36_under <- data.table(read.xlsx(file = file.path(DATA_HOME, "de-identified controls.xlsx"),
                                         sheetName = "36 and under",
                                         rowIndex = 1:12,
                                         colIndex = 1:20,
                                         colClasses = "numeric"))
dt.ctrl.36_under

dt.ctrl.37_71 <- data.table(read.xlsx(file = file.path(DATA_HOME, "de-identified controls.xlsx"),
                                      sheetName = "37 to 71 months",
                                      rowIndex = 1:10,
                                      colIndex = 1:20))
dt.ctrl.37_71

# 2. EHT----
dt.eht <- data.table(read.xlsx(file = file.path(DATA_HOME, "de-identified EHT.xlsx"),
                               sheetName = "All Visits",
                               rowIndex = 1:223,
                               colIndex = 1:20))
dt.eht$EHT <- "EHT"
dt.eht

# 3. No EHT----
dt.noeht <- data.table(read.xlsx(file = file.path(DATA_HOME, "de-identified No EHT.xlsx"),
                                 sheetName = "All visits",
                                 rowIndex = 1:302,
                                 colIndex = 1:20))
dt.noeht$EHT <- "No EHT"
dt.noeht

# 4. Combine----
dt1 <- rbindlist(list(dt.eht,
                      dt.noeht))
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
dt1$EHT <- factor(dt1$EHT, levels = unique(dt1$EHT))
setkey(dt1, Family.., CA.in.Months)
dt1
summary(dt1)

# Plots----
for (i in 8:20) {
  tmp <- subset(dt1, 
                c(!is.na(dt1[, i, with = FALSE])),
                select = c(1, 7, i, 21))
  names(tmp)[3] <- "y"
  tmp[, nvis := 1:.N,
      by = Family..]
  id.keep <- tmp$Family..[tmp$nvis == 2]
  
  tmp <- droplevels(subset(tmp, Family.. %in% id.keep))
  
  tiff(filename = paste("tmp/", names(dt1)[i], ".1.tiff", sep = ""),
       height = 5,
       width = 10,
       units = 'in',
       res = 300,
       compression = "lzw+p")
  p1 <- ggplot(tmp,
               aes(x = CA.in.Months,
                   y = y,
                   colour = Family..,
                   group = Family..)) +
    facet_wrap(~ EHT,
               nrow = 1) +
    geom_line(size = 0.1,
              col = "black") +
    geom_point(size = 3,
               alpha = 0.5) +
    theme(legend.position = "none") +
    scale_x_continuous("Chronological Age (Months)") +
    scale_y_continuous(names(dt1)[i])
  print(p1)
  graphics.off()
  
  tiff(filename = paste("tmp/", names(dt1)[i], ".2.tiff", sep = ""),
       height = 5,
       width = 10,
       units = 'in',
       res = 300,
       compression = "lzw+p")
  p2 <- ggplot(tmp,
               aes(x = nvis,
                   y = y,
                   colour = Family..,
                   group = Family..)) +
    facet_wrap(~ EHT,
               nrow = 1) +
    geom_line(position = position_dodge(0.3),
              size = 0.1,
              col = "black") +
    geom_point(position = position_dodge(0.3),
               size = 3,
               alpha = 0.5) +
    theme(legend.position = "none") +
    scale_x_continuous("Visit Number",
                       breaks = 1:11,
                       labels = 1:11) +
    scale_y_continuous(names(dt1)[i])
  print(p2)
  graphics.off()
}