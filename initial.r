library(tidyverse)

a <- function(index) {
  
  df <- readxl::read_xlsx("RAWDATA_OFI_2016_BEA_Position Abroad 2010-2015.xlsx", 
                          sheet = index,
                          col_names = F)
  
  df[4,(5:12)]
  df[3, (5:12)] <- df[4,(5:12)]
  
  df <- df[-1, ]
  df <- df[-1, ]
  
  colnames(df) <- c("Countries", "All industries", "Mining", 
                          "Food", "Chemical", "Primary and fabricated metals",	"Machinery",
                          "Computers and electronic products", "Electrical equipment, appliances, and components",	
                          "Transportation equipment",	"Other manufacturing",	"Wholesale trade",
                          "Information",	"Depository institutions",	
                          "Finance (except depository institutions) and insurance",
                          "Professional, scientific, and technical services",	"Holding companies (nonbank)",
                          "other industries")
  
  df <- df [rowSums(is.na(df)) != ncol(df),]
  
  row.names.remove <- c("Other")
  
  row.names(df)
  
  df <- df %>% 
    filter(Countries != "Other")
  
  df <- df %>% 
    filter(Countries != "Africa") %>% 
    filter(Countries != "Other Western Hemisphere") %>% 
    filter(Countries != "Asia and Pacific") %>%
    filter(Countries != "South America") %>%
    filter(Countries != "Europe 1") %>%
    filter(Countries != "Latin America and Other Western Hemisphere") %>% 
    filter(Countries != "Middle East") %>%
    filter(Countries != "[Millions of dollars]") %>% 
    filter(Countries != "Central America")
  
  df <- df[, -12]
  
  df <- df[, -4]
  
  df <- df[-c(59:63),]
  
  
  return(df)
}

data2010 <- a(1)

data2010 %>% View()

data2010 %>% write_csv(path = "/Users/lanpham/Documents/ECON314/data2010.csv")




