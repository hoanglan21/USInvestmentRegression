library(tidyverse)

worldbankreport <- readxl::read_xlsx("RAWDATA_BZ.xlsx",
                        col_names = T)
worldbankreport <- worldbankreport %>% 
  filter(Year != "DB2016") %>% 
  filter(Year != "DB2017")

worldbankreport <- worldbankreport[, which(colMeans(!is.na(worldbankreport)) > 0.5)]

?match
match(data2010$Countries, worldbankreport$Economy)

data2010$`All industries`[match(data2010$Countries, worldbankreport$Economy)]


USInvestment <- replicate(nrow(worldbankreport), -1 )

worldbankreport <- cbind(worldbankreport,USInvestment )

worldbankreport$USInvestment


countNames <- data2010$Countries[2:nrow(data2010)]

for(i in 1:length(countNames)){
  
  countryName <- countNames[i]
  
  econ <- data2010 %>%
    filter(Countries  == countryName) %>%
    select(`All industries`)
  
  worldbankreport$USInvestment[worldbankreport$Economy == countryName 
                               & worldbankreport$Year == "DB2010"] <- econ
}

worldbankreport$USInvestment
colnames(worldbankreport)

worldbankreport2010 <- worldbankreport[0,]

for (i in 1:(nrow(worldbankreport))) {
  
  
  
  temp <- worldbankreport[i, ]
  if (temp$USInvestment != -1) {
    worldbankreport2010 <- rbind(worldbankreport2010, temp)
  }
  
}
  



econ <- data2010 %>% 
  filter(Countries  == "Canada") %>% 
  select(`All industries`)

worldbankreport$USInvestment[worldbankreport$Economy == "Canada" & worldbankreport$Year == "DB2010"] <- econ



worldbankreport$USInvestment=data2010$`All industries`[match(data2010$Countries, worldbankreport$Economy)]




worldbankreport$USInvestment

worldbankreport2010 %>% write_csv(path = "/Users/lanpham/Documents/ECON314/wbr2010.csv")

worldbankreport2010$USInvestment <-  worldbankreport2010$USInvestment %>% unlist

worldbankreport2010[3:ncol(worldbankreport2010)] <- worldbankreport2010[3:ncol(worldbankreport2010)] %>% as.numeric()

worldbankreport2010[ ,3:ncol(worldbankreport2010)] <- sapply(worldbankreport2010[ ,3:ncol(worldbankreport2010)],as.numeric)


linear <- lm(USInvestment ~`Overall DTF`, data= worldbankreport2010)
linear2 <- lm(USInvestment ~ `Starting a business-DTF` , data= worldbankreport2010)

linear2 %>% summary()


worldbankreport2010%>% colnames

linear %>% summary
