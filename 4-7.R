# creating a useable dataset
#set working directory
setwd('~/UN Project')

#read in disease and death estimates
cancer <- read.csv('GHE2015_Deaths-2015-country50-59.csv', skip=6,
                   stringsAsFactors = FALSE)
codes <- (cancer[1,8:ncol(cancer)])
countries <- colnames(cancer)[8:ncol(cancer)]


data.x <- data.frame(t(rbind(codes,
                     countries,
                     cancer[cancer$X.1=="Malignant neoplasms" & cancer$Sex=="Persons",
                            8:ncol(cancer),]
)),stringsAsFactors = FALSE)
colnames(data.x) <- c("Country.Code","Country","Total.rate")
data.x$Total.rate <- as.numeric(data.x$Total.rate)



#rename disease column
colnames(cancer)[6] <- "Disease"
colnames(cancer)[7] <- "Disease Subset"

all.canc <- data.frame(
  (cancer[cancer$X.1=="Malignant neoplasms" & cancer$Sex=="Persons",
                   8:ncol(cancer)]))


all.canc2 <- rbind(all.canc, codes)
all.canc2 <- data.frame(t(all.canc2), stringsAsFactors = FALSE)
colnames(brain2) <- c("cancer.rate", "country.code")
all.canc2$cancer.rate <- as.numeric(all.canc2$cancer.rate)


# have the total cancer rate for each country, now need normalized rate

pop <- data.frame(
  rbind(colnames(cancer)[8:ncol(cancer)], #no idea how this works but it did
        (cancer[3, 8:ncol(cancer)])), stringsAsFactors = FALSE
)
# getting rid of commas
pop[2,] <- as.numeric(gsub(",", "", pop[2,]))
pop <- data.frame(t(pop), stringsAsFactors = FALSE)
colnames(pop) <- c('country.name', 'pop')

#merge population data
data.x <- merge(data.x, pop, by.x="Country",by.y="country.name")

#create column for cancer rate per thousand
data.x$pop <- as.numeric(data.x$pop)

#calculate deaths per 100,000
data.x$per.thou <- (data.x$Total.rate / data.x$pop) * 100000

write.csv(data.x, file='C:/Users/cjp77/Documents/UN Project/total rate.csv')

# load in data for specific cancers

all.canc <- data.frame((rbind(cancer[68:103,
                                    5:ncol(cancer),]
)),stringsAsFactors = FALSE)

colnames(all.canc)[1:3] <- c('X.1','X.2','X.3')
rownames(all.canc) <- do.call(paste, c(all.canc[c("X.1", "X.2",'X.3')], sep = ""))

all.canc <-as.data.frame(t(all.canc), stringsAsFactors = FALSE)
# replace first three rows
#colnames(all.canc) <- (all.canc['NewCol',])
all.canc <- (all.canc[c(-1,-2,-3),])

#convert data frame to numeric
for (i in 1:ncol(all.canc)){
  all.canc[,i] <- as.numeric(all.canc[,i])
}

# remove last two rows??
#all.canc <- all.canc[c(-187,-188),]
all.canc$Country <- rownames(all.canc)


# merge with total cancer rate
all.canc <- merge(data.x, all.canc, by.x="Country",by.y="Country")

#######################################################################################

# SDG data
sdg.country <- read.csv("SDGCountry.csv")
colnames(sdg.country)[1] <- "Country.Code"
#isolate variables i want
sdg.country <- sdg.country[,c("Country.Code","Region","Income.Group")]
#merge with all.canc
merged <- merge(all.canc,sdg.country,by.x="Country.Code",by.y="Country.Code")

sdg.data <- read.csv("SDGData.csv")
colnames(sdg.data) <- c("Country.Name","Country.Code","Indicator.Name","Indicator.Code",
                        seq(1990,2016))

# next step: merge data for one indicator with cancer rates
# here : C02 emissions (kg per ppp $ of GDP)
sdg.EN.ATM.CO2E.PP.GD <- sdg.data[sdg.data$Indicator.Code=="EN.ATM.CO2E.PP.GD",]

#merge with cancer data
x.co2 <- merge(merged, sdg.EN.ATM.CO2E.PP.GD)

# GDP data
per.cap.gdp <- read.csv('~/UN Project/per capita GDP from UNdata.csv')
#subset 2014 values
gdp2014 <- subset(per.cap.gdp, per.cap.gdp$Year==2014)

write.csv(x.co2, file='C:/Users/cjp77/Documents/UN Project/x.co2.csv')

######################################################################################
#  same code, but for 0-4 age group
######################################################################################

#read in disease and death estimates
cancer0 <- read.csv('GHE2015_Deaths-2015-country0-4.csv', skip=6,
                   stringsAsFactors = FALSE)
codes <- (cancer0[1,8:ncol(cancer0)])
countries <- colnames(cancer0)[8:ncol(cancer0)]


data.x <- data.frame(t(rbind(codes,
                             countries,
                             cancer0[cancer0$X.1=="Malignant neoplasms" & cancer0$Sex=="Persons",
                                    8:ncol(cancer0),]
)),stringsAsFactors = FALSE)
colnames(data.x) <- c("Country.Code","Country","Total.rate")
data.x$Total.rate <- as.numeric(data.x$Total.rate)



#rename disease column
colnames(cancer0)[6] <- "Disease"
colnames(cancer0)[7] <- "Disease Subset"

all.canc0 <- data.frame(
  (cancer0[cancer0$X.1=="Malignant neoplasms" & cancer0$Sex=="Persons",
          8:ncol(cancer0)]))


all.canc0 <- rbind(all.canc, codes)
colnames(all.canc0) <- c("cancer.rate", "country.code")
all.canc0$cancer.rate <- as.numeric(all.canc0$cancer.rate)


# have the total cancer rate for each country, now need normalized rate

pop0 <- data.frame(
  rbind(colnames(cancer0)[8:ncol(cancer0)], #no idea how this works but it did
        (cancer0[3, 8:ncol(cancer0)])), stringsAsFactors = FALSE
)
# getting rid of commas
pop0[2,] <- as.numeric(gsub(",", "", pop0[2,]))
pop0 <- data.frame(t(pop0), stringsAsFactors = FALSE)
colnames(pop0) <- c('country.name', 'pop')

#merge population data
data.x0 <- merge(data.x, pop0, by.x="Country",by.y="country.name")

#create column for cancer rate per thousand
data.x0$pop <- as.numeric(data.x0$pop)

#calculate deaths per 100,000
data.x0$per.thou <- (data.x0$Total.rate / data.x0$pop) * 100000
#write.csv(data.x, file='C:/Users/cjp77/Documents/UN Project/total rate.csv')

# load in data for specific cancers

all.canc0 <- data.frame((rbind(cancer0[68:103,
                                     5:ncol(cancer0),]
)),stringsAsFactors = FALSE)

colnames(all.canc0)[1:3] <- c('X.1','X.2','X.3')
rownames(all.canc0) <- do.call(paste, c(all.canc0[c("X.1", "X.2",'X.3')], sep = ""))

all.canc0 <-as.data.frame(t(all.canc0), stringsAsFactors = FALSE)
# replace first three rows
#colnames(all.canc) <- (all.canc['NewCol',])
all.canc0 <- (all.canc0[c(-1,-2,-3),])

#convert data frame to numeric
for (i in 1:ncol(all.canc)){
  all.canc[,i] <- as.numeric(all.canc[,i])
}

# remove last two rows??
#all.canc <- all.canc[c(-187,-188),]
all.canc0$Country <- rownames(all.canc0)


# merge with total cancer rate
all.canc0 <- merge(data.x0, all.canc0, by.x="Country",by.y="Country")

#######################################################################################

# SDG data
sdg.country <- read.csv("SDGCountry.csv")
colnames(sdg.country)[1] <- "Country.Code"
#isolate variables i want
sdg.country <- sdg.country[,c("Country.Code","Region","Income.Group")]
#merge with all.canc
merged0 <- merge(all.canc0,sdg.country,by.x="Country.Code",by.y="Country.Code")

sdg.data <- read.csv("SDGData.csv")
colnames(sdg.data)[1:4] <- c("Country.Name","Country.Code","Indicator.Name","Indicator.Code")

# next step: merge data for one indicator with cancer rates
# here : C02 emissions (kg per ppp $ of GDP)
sdg.EN.ATM.CO2E.PP.GD <- sdg.data[sdg.data$Indicator.Code=="EN.ATM.CO2E.PP.GD",]

#merge with cancer data
sdg0 <- merge(merged0, sdg.EN.ATM.CO2E.PP.GD)

# GDP data
per.cap.gdp <- read.csv('~/UN Project/per capita GDP from UNdata.csv')
#subset 2014 values
gdp2014 <- subset(per.cap.gdp, per.cap.gdp$Year==2014)

write.csv(x.co2, file='C:/Users/cjp77/Documents/UN Project/x.co2.csv')



library(plotly)
plot_ly(x.co2, x=~pop, y=~per.thou, color=~Region)











