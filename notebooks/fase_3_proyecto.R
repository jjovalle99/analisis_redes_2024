# install.packages("readxl")
# install.packages("ergm")
# install.packages("network")
# install.packages("dplyr")
# install.packages('tidyr')

# Librerias
library(readxl)
library(ergm)
library(network)
library(dplyr)
library(tidyr)


# Data Path
path <- "data/"

# Arcos
arcos <- read_excel(paste(path, "Arcos.xlsx", sep = ""))
print(paste("Numero de filas en Arcos: ", nrow(arcos)))
print(paste("Numero de columnas en Arcos: ", ncol(arcos)))
# View(arcos)

# Nodos
nodos <- read_excel(paste(path, "Nodos.xlsx", sep = ""))
print(paste("Numero de filas en Nodos: ", nrow(nodos)))
print(paste("Numero de columnas en Nodos: ", ncol(nodos)))
# View(nodos)

# Crear Red
arcos_no_loops <- arcos[arcos$Source != arcos$Target, ] # Quito loops i.e. Source = Target
net <- network(arcos_no_loops, matrix.type = "edgelist", directed = TRUE)
print(paste("Numero de arcos:", network.edgecount(net)))
print(paste("Numero de nodos:", network.size(net)))

# ERGM Modelo 1 - fundingrounds
nodos <- nodos %>%
    mutate(`Number of Funding Rounds` = (`Number of Funding Rounds` - min(`Number of Funding Rounds`, na.rm = TRUE)) / 
            (max(`Number of Funding Rounds`, na.rm = TRUE) - min(`Number of Funding Rounds`, na.rm = TRUE)), # Normalizo
            `Number of Funding Rounds` = ifelse(is.na(`Number of Funding Rounds`), 0, `Number of Funding Rounds`)) # Reemplazo NAs por 0

net %v% "FundingRounds" <- nodos$`Number of Funding Rounds`
model1 <- ergm(net ~ edges + nodematch("FundingRounds"))
summary(model1)

gof_control <- control.gof.ergm(nsim = 100)
gf1 <- gof(model1, control = gof_control)
plot(gf1)

# ERGM Modelo 2  - fundingrounds + hq_location
nodos$`Headquarters Location`[is.na(nodos$`Headquarters Location`)] <- "Unknown" # reemplazo NAs por "Unknown"
net %v% "HQ_Location" <- nodos$`Headquarters Location`

model2 <- ergm(net ~ edges + nodematch("FundingRounds") + nodematch("HQ_Location"))
summary(model2)

gf2 <- gof(model2, control = gof_control)
plot(gf2)

# ERGM Modelo 3  - fundingrounds + hq_location + industry
nodos$`Industry Groups`[is.na(nodos$`Industry Groups`)] <- "Unknown" # reemplazo NAs por "Unknown"
net %v% "Industry" <- nodos$`Industry Groups`
model3 <- ergm(net ~ edges + nodematch("FundingRounds") + nodematch("HQ_Location") + nodematch("Industry"))
summary(model3)

gf3 <- gof(model3, control = gof_control)
plot(gf3)

# ERGM Modelo 4  - fundingrounds + hq_location + industry + total funding
nodos <- nodos %>%
    mutate(`Total Funding Amount Currency (in USD)` = (`Total Funding Amount Currency (in USD)` - min(`Total Funding Amount Currency (in USD)`, na.rm = TRUE)) / 
            (max(`Total Funding Amount Currency (in USD)`, na.rm = TRUE) - min(`Total Funding Amount Currency (in USD)`, na.rm = TRUE)), # Normalizo
            `Total Funding Amount Currency (in USD)` = ifelse(is.na(`Total Funding Amount Currency (in USD)`), 0, `Total Funding Amount Currency (in USD)`)) # Reemplazo NAs por 0

net %v% "TotalFunding" <- nodos$`Total Funding Amount Currency (in USD)`
model4 <- ergm(net ~ edges + nodematch("FundingRounds") + nodematch("HQ_Location") + nodematch("Industry") + nodematch("TotalFunding"))
summary(model4)

gf4 <- gof(model4, control = gof_control)
plot(gf4)