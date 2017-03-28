# Project: XXY
# Created: 03/24/2017
# Authors: Davit Sargsyan, Lyudvig Petrosyan
#**********************************************************
# Header----
DATA_HOME <- "C:/git_local/data/focus"
require(data.table)
require(xlsx)
dt.ctrl.36_under <- data.table(read.xlsx(file = file.path(DATA_HOME, "de-identified controls.xlsx"),
                                        sheetName = "36 and under",
                                        rowIndex = 1:12,
                                        colIndex = 1:20,
                                        colClasses = "numeric"))
names(dt.ctrl.36_under) <- dt.ctrl.36_under

dt.ctrl.37_71 <- data.table(read.xlsx(file = file.path(DATA_HOME, "de-identified controls.xlsx"),
                                        sheetName = "37 to 71 months",
                                        rowIndex = 1:10,
                                        colIndex = 1:20))
dt.ctrl.37_71