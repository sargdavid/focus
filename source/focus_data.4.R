# Project: Early Hormonal Therapy (EHT) treatment effect on 
#          first 5 years of life in boys who are affected with 47, XXY (Klinefelter syndrome)
# Created: 03/24/2017
# Authors: Davit Sargsyan, Lyudvig Petrosyan
# Last Modified: 05/24/2017
#**********************************************************
# Header----
DATA_HOME <- "C:/Users/dsargsy/Dropbox/Projects with Davit and Lyudvig/Data"
require(data.table)
require(xlsx)
require(ggplot2)
require(lmerTest)

# Data----
# Treatment annotation----
# Data file version: 04/18/2017
trt.anno <- fread(file = file.path(DATA_HOME, "key for abbreviations.csv"))

# Load data----
## 1. EHT----
# Data file version: 05/12/2017
eht <- data.table(read.xlsx(file = file.path(DATA_HOME, 
                                             "de-identified EHT Clean, Finalized 5.12.xlsx"),
                            sheetName = "All Visits",
                            rowIndex = 1:214,
                            colIndex = c(1, 3:19)))
eht$D.O.B <- as.Date(eht$D.O.B, format = "%Y/%m/%d")
eht$D.O.V <- as.Date(eht$D.O.V, format = "%Y/%m/%d")
eht$Group <- "EHT"
eht

length(unique(eht$Family..))

## 2. No EHT----
# Data file version: 05/12/2017
noeht <- data.table(read.xlsx(file = file.path(DATA_HOME,
                                               "de-identified No EHT Clean, Finalized 5.12.xlsx"),
                              sheetName = "All visits",
                              rowIndex = 1:297,
                              colIndex = c(1, 3:19)))
noeht$D.O.B <- as.Date(noeht$D.O.B, format = "%Y/%m/%d")
noeht$D.O.V <- as.Date(noeht$D.O.V, format = "%Y/%m/%d")
noeht$Group <- "No EHT"
noeht

length(unique(noeht$Family..))

## 3. Controls----
# Data file version: 03/27/2017
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

# Plots----
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

# Plots----
for (i in 5:12) {
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

# Sample sizes----
out.N.1836 <- list()
for (i in 5:12) {
  tmp <- d1836[, c("Family..", "Group", names(d1836)[i]), with = FALSE]
  names(tmp)[3] <- "y"
  tmp <- unique(droplevels(tmp[!is.na(y) &
                                 Group != "Control",
                               -c("y")]))
  out.N.1836[[i-4]] <- c(addmargins(table(tmp$Group)))
}
out.N.1836 <- do.call("rbind", out.N.1836)
out.N.1836 <- data.table(Scale = trt.anno[1:8, 2],
                         Age = "18 to 36 month",
                         out.N.1836)
out.N.1836

# Mixed effect----
out.1836 <- list()
for (i in 5:12) {
  tmp <- droplevels(d1836[Group != "Control", c(1, 4, i, 19, 21), with = FALSE])
  names(tmp) <- c("id", "month", "y", "grp", "year")
  tmp <- subset(tmp, !is.na(tmp$y))
  out.1836[[i - 4]] <- summary(lmer(y ~ grp + (1 | id) + (1 | year), data = tmp))
  names(out.1836)[i - 4] <- trt.anno$`Full Name`[i - 4]
}
out.1836

est.1836 <- lapply(out.1836, 
                   function(a) {
                     tmp <- a$coefficients
                     c(no.eht = tmp[1, 1],
                       est = tmp[2, 1],
                       lb = tmp[2, 1] - 1.96*tmp[2, 2],
                       ub = tmp[2, 1] + 1.96*tmp[2, 2],
                       pval = tmp[2, 5])
                   })
est.1836 <- do.call("rbind", 
                    est.1836)
tmp <- rownames(est.1836)
est.1836 <- data.table(test = tmp,
                       age = "18 to 36 month",
                       est.1836)
est.1836$test <- factor(est.1836$test,
                        levels = unique(est.1836$test))
est.1836

# Plot estimates----
tiff(filename = "tmp/est_18to36mo.tiff",
     height = 8,
     width = 6,
     units = 'in',
     res = 300,
     compression = "lzw+p")
ggplot(est.1836,
       aes(x = test,
           y = est,
           colour = test,
           group = test)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymax = lb,
                    ymin = ub),
                width =.4,
                size = 1) +
  geom_hline(yintercept = 0,
             linetype = "dashed") +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 75,
                                   hjust = 1)) +
  scale_x_discrete("Test") +
  scale_y_continuous("Mean Difference") +
  ggtitle("18 to 36 Month") +
  guides(fill = guide_legend(title = "Treatment",
                             title.position = "top",
                             nrow = 1))
graphics.off()

# Over 36 mo----
dover36 <- droplevels(subset(dt1, CA.in.Months > 36))
dover36

# Plots----
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

# Sample sizes----
out.N.over36 <- list()
for (i in c(5, 6, 9, 10, 14:18)) {
  tmp <- dover36[, c("Family..", "Group", names(dover36)[i]), with = FALSE]
  names(tmp)[3] <- "y"
  tmp <- unique(droplevels(tmp[!is.na(y) &
                                 Group != "Control",
                               -c("y")]))
  out.N.over36[[i-4]] <- c(addmargins(table(tmp$Group)))
}
out.N.over36 <- do.call("rbind", out.N.over36)
out.N.over36 <- data.table(Scale = trt.anno[c(5, 6, 9, 10, 14:18) - 4, 2],
                           Age = "Over 36 month",
                           out.N.over36)
out.N.over36

# Mixed effect----
out.over36 <- list()
for (i in c(5, 6, 9, 10, 14:18)) {
  tmp <- droplevels(dover36[Group != "Control", c(1, 4, i, 19, 21), with = FALSE])
  names(tmp) <- c("id", "month", "y", "grp", "year")
  tmp <- subset(tmp, !is.na(tmp$y))
  out.over36[[i - 4]] <- summary(lmer(y ~ grp + (1 | id) + (1 | year), data = tmp))
  names(out.over36)[i - 4] <- trt.anno$`Full Name`[i - 4]
}

est.over36 <- lapply(out.over36, 
                     function(a) {
                       tmp <- a$coefficients
                       c(no.eht = tmp[1, 1],
                         est = tmp[2, 1],
                         lb = tmp[2, 1] - 1.96*tmp[2, 2],
                         ub = tmp[2, 1] + 1.96*tmp[2, 2],
                         pval = tmp[2, 5])
                     })
est.over36 <- do.call("rbind", 
                      est.over36)
tmp <- rownames(est.over36)
est.over36 <- data.table(test = tmp,
                         age = "Over 36 month",
                         est.over36)
est.over36$test <- factor(est.over36$test,
                          levels = unique(est.over36$test))
est.over36

# Plot estimates
tiff(filename = "tmp/est_over36mo.tiff",
     height = 8,
     width = 6,
     units = 'in',
     res = 300,
     compression = "lzw+p")
ggplot(est.over36,
       aes(x = test,
           y = est,
           colour = test,
           group = test)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymax = lb,
                    ymin = ub),
                width =.4,
                size = 1) +
  geom_hline(yintercept = 0,
             linetype = "dashed") +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 75,
                                   hjust = 1)) +
  scale_x_discrete("Test") +
  scale_y_continuous("Mean Difference") +
  ggtitle("Over 36 Month") +
  guides(fill = guide_legend(title = "Treatment",
                             title.position = "top",
                             nrow = 1))
graphics.off()

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

# Sample sizes----
out.N.18over <- list()
for (i in c(5, 6, 9, 10)) {
  tmp <- d18over[, c("Family..", "Group", names(d18over)[i]), with = FALSE]
  names(tmp)[3] <- "y"
  tmp <- unique(droplevels(tmp[!is.na(y) &
                                 Group != "Control",
                               -c("y")]))
  out.N.18over[[i-4]] <- c(addmargins(table(tmp$Group)))
}
out.N.18over <- do.call("rbind", out.N.18over)
out.N.18over <- data.table(Scale = trt.anno[c(5, 6, 9, 10) - 4, 2],
                           Age = "18 to 36 month",
                           out.N.18over)
out.N.18over

# Mixed effect
out.18over <- list()
for (i in c(5, 6, 9, 10)) {
  tmp <- droplevels(d18over[Group != "Control", c(1, 4, i, 19, 21), with = FALSE])
  names(tmp) <- c("id", "month", "y", "grp", "year")
  tmp <- subset(tmp, !is.na(tmp$y))
  out.18over[[i - 4]] <- summary(lmer(y ~ grp + (1 | id) + (1 | year), data = tmp))
  names(out.18over)[i - 4] <- trt.anno$`Full Name`[i - 4]
}

est.18over <- lapply(out.18over, 
                     function(a) {
                       tmp <- a$coefficients
                       c(no.eht = tmp[1, 1],
                         est = tmp[2, 1],
                         lb = tmp[2, 1] - 1.96*tmp[2, 2],
                         ub = tmp[2, 1] + 1.96*tmp[2, 2],
                         pval = tmp[2, 5])
                     })
est.18over <- do.call("rbind", 
                      est.18over)
tmp <- rownames(est.18over)
est.18over <- data.table(test = tmp,
                         age = "18 month and over",
                         est.18over)
est.18over$test <- factor(est.18over$test,
                          levels = unique(est.18over$test))
est.18over

# Plot estimates
tiff(filename = "tmp/est_18over.tiff",
     height = 8,
     width = 6,
     units = 'in',
     res = 300,
     compression = "lzw+p")
ggplot(est.18over,
       aes(x = test,
           y = est,
           colour = test,
           group = test)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymax = lb,
                    ymin = ub),
                width =.4,
                size = 1) +
  geom_hline(yintercept = 0,
             linetype = "dashed") +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 75,
                                   hjust = 1)) +
  scale_x_discrete("Test") +
  scale_y_continuous("Mean Difference") +
  ggtitle("18 Month and Over") +
  guides(fill = guide_legend(title = "Treatment",
                             title.position = "top",
                             nrow = 1))
graphics.off()

# Save tables----
t1.N <- rbindlist(list(out.N.1836,
                       out.N.over36,
                       out.N.18over))
t1.N
write.csv(t1.N,
          file = "tmp/t1.N.csv",
          row.names = FALSE)

t2.est <- rbindlist(list(est.1836,
                         est.over36,
                         est.18over))
t2.est
write.csv(t2.est,
          file = "tmp/t2.est.csv",
          row.names = FALSE)

# AD HOC 05/24/2017----
foo <- function(x) {
  paste(range(x,
              na.rm = TRUE), collapse = ":")
}

# a. 18 to 36----
# Mean age of each group
d1836[, mu.age := mean(CA.in.Months),
      by = Group]
d1836[, sd.age := sd(CA.in.Months),
      by = Group]

t.mu.age <- data.table(age = "18 to 36 month",
                       unique(subset(d1836,
                                     select = c(19, 22, 23))))
t.mu.age

# Mean of each score for each treatment
cols <- names(d1836)[5:18]
cols.out <- paste(cols, "mu", sep = ".")

d1836[ , 
       (cols.out) := lapply(.SD, 
                            "mean", 
                            na.rm = TRUE),
       .SDcols = cols,
       by = Group]

t.mu.scores <- data.table(age = "18 to 36 month",
                          unique(subset(d1836,
                                        select = c(19, 24:37))))
t.mu.scores

# Range of each score for each treatment
cols.out <- paste(cols, "range", sep = ".")

d1836[ , 
       (cols.out) := lapply(.SD, 
                            "foo"),
       .SDcols = cols,
       by = Group]

t.range.scores <- data.table(age = "18 to 36 month",
                             unique(subset(d1836,
                                           select = c(19, 38:51))))
t.range.scores

# b. Over 36 month----
# Mean age of each group
dover36[, mu.age := mean(CA.in.Months),
        by = Group]
dover36[, sd.age := sd(CA.in.Months),
        by = Group]

t2.mu.age <- data.table(age = "Over 36 month",
                        unique(subset(dover36,
                                      select = c(19, 22, 23))))
t2.mu.age

# Mean of each score for each treatment
cols <- names(dover36)[5:18]
cols.out <- paste(cols, "mu", sep = ".")

dover36[ , 
         (cols.out) := lapply(.SD, 
                              "mean", 
                              na.rm = TRUE),
         .SDcols = cols,
         by = Group]

t2.mu.scores <- data.table(age = "Over 36 month",
                           unique(subset(dover36,
                                         select = c(19, 24:37))))
t2.mu.scores

# Range of each score for each treatment
cols.out <- paste(cols, "range", sep = ".")

dover36[ , 
         (cols.out) := lapply(.SD, 
                              "foo"),
         .SDcols = cols,
         by = Group]

t2.range.scores <- data.table(age = "Over 36 month",
                              unique(subset(dover36,
                                            select = c(19, 38:51))))
t2.range.scores

# c. 18 month and over----
# Mean age of each group
d18over[, mu.age := mean(CA.in.Months),
        by = Group]
d18over[, sd.age := sd(CA.in.Months),
        by = Group]

t3.mu.age <- data.table(age = "18 month and over",
                        unique(subset(d18over,
                                      select = c(19, 22, 23))))
t3.mu.age

# Mean of each score for each treatment
cols <- names(d18over)[5:18]
cols.out <- paste(cols, "mu", sep = ".")

d18over[ , 
         (cols.out) := lapply(.SD, 
                              "mean", 
                              na.rm = TRUE),
         .SDcols = cols,
         by = Group]

t3.mu.scores <- data.table(age = "18 month and over",
                           unique(subset(d18over,
                                         select = c(19, 24:37))))
t3.mu.scores

# Range of each score for each treatment
cols.out <- paste(cols, "range", sep = ".")

d18over[ , 
         (cols.out) := lapply(.SD, 
                              "foo"),
         .SDcols = cols,
         by = Group]

t3.range.scores <- data.table(age = "18 month and over",
                              unique(subset(d18over,
                                            select = c(19, 38:51))))
t3.range.scores

# Combine and save
# a. Age
t3.age <- rbindlist(list(t.mu.age,
                         t2.mu.age,
                         t3.mu.age))
t3.age
write.csv(t3.age,
          file = "tmp/t3.age.csv",
          row.names = FALSE)

# b. Mean scores
t4.mu.scores <- rbindlist(list(t.mu.scores,
                               t2.mu.scores,
                               t3.mu.scores))
t4.mu.scores
write.csv(t4.mu.scores,
          file = "tmp/t4.mu.scores.csv",
          row.names = FALSE)

# c. Ranges of scores
t5.range.scores <- rbindlist(list(t.range.scores,
                                  t2.range.scores,
                                  t3.range.scores))
t5.range.scores
write.csv(t5.range.scores,
          file = "tmp/t5.range.scores.csv",
          row.names = FALSE)