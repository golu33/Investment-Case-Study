#Investment Case Group Project - DSC Group 32#
#-------------------------------------------------------------------------------------------------------------------------
#   Pre-Requisites :

#1. Set the working directory

setwd("C:/Users/admin/Desktop/Case Study")

#  All files for input data should be placed in this directory as well as R source code

#2. Installing the below packages

    #install.packages("tidyr")
    #install.packages("dplyr")
    #install.packages("sqldf")
    #install.packages("gdata")
    #install.packages("stringr")

#3. Library files

    library(tidyr)
    library(dplyr)
    library(stringr)

#-------------------------------------------------------------------------------------------------------------------------

#   CHECK POINT 1 : DATA CLEANSING  =>

#1  Creating Companies Data Frame from companies.txt
    #Map the string variable to "NA" and character variable to be stored as character

    companies <- read.delim("companies.txt",header = TRUE,sep = "\t",na.strings = "",stringsAsFactors = FALSE)
    summary(companies)
    View(companies)

#2  Creating Rounds2 Data Frame from rounds2.csv 
    #Map the string variable to "NA" and character variable to be stored as character

    rounds2 <- read.csv("rounds2.csv",header = TRUE,na.strings = "",stringsAsFactors = FALSE)
    summary(rounds2)
    View(rounds2)

#3  Check the Variable Attributes 
    # Companies Data Frame
    str(companies)
    summary(companies)

    # Rounds2 Data Frame
    str(rounds2)
    summary(rounds2)
    
#4  Convert the Companies Column to lower case and remove the spaces both in lead and trail
    
    companies$permalink <- trimws(tolower(companies$permalink))
    View(companies$permalink)
    
    rounds2$company_permalink <- trimws(tolower(rounds2$company_permalink))
    View(rounds2$company_permalink)
    
#-------------------------------------------------------------------------------------------------------------------------

# Table 1.1: Understand the Data Set 


#1.1 
#   How many unique companies are present in rounds2?    

    nrow(data.frame(unique(rounds2$company_permalink)))
    
    count(distinct(rounds2,company_permalink))
#   A tibble: 1 x 1
#    n
#    <int>
#     1 66368
    

#1.2
#   How many unique companies are present in companies?        

    nrow(data.frame(unique(companies$permalink)))
    
    count(distinct(companies,permalink))
#   A tibble: 1 x 1
#    n
#    <int>
#     1 66368

        
#1.3
#   In the companies data frame, which column can be used as the unique key for each company? 
    #Write the name of the column.
    
    #permalink

    
#1.4
#   Are there any companies in the rounds2 file which are not present in companies? 
    #Answer yes or no: Y/N
    
    
    sum(rounds2$company_permalink %in% companies$permalink)
    a = sum(rounds2$company_permalink %in% companies$permalink)
    b = nrow(rounds2)
    c <- if (a == b) {print("N")} else {print("Y")}
    paste ("Are there any companies in the rounds2 file which are not present in companies ? Answer",c )
    
    #Matching the permalink Column from rounds2 data frame and companies data frame and executing the above.

        
#1.5    
#   Merge the two data frames so that all variables (columns) in the companies frame are added to the 
    # rounds2 data frame. Name the merged frame master_frame. How many observations are present in master_frame?
    
#   To create master_frame, convert unique columns to lower or upper case in both data frames 
    # and merge it using left outer join.

    companies$permalink <- tolower(companies$permalink)
    rounds2$company_permalink <- tolower(rounds2$company_permalink)
    master_frame <- merge(rounds2,companies,by.x = "company_permalink",by.y = "permalink",all.x=TRUE)
    summary(master_frame) 
    View(master_frame)

#   master_frame has 114949 Observations of 15 variables

    
#-------------------------------------------------------------------------------------------------------------------------
#   CHECK POINT 2 : Funding Type Analysis =>
    
#1. Clean the required data columns required for analysis.
    
    #1.1 Change case of funding type column
    
    master_frame$funding_round_type <- trimws(tolower(master_frame$funding_round_type))
    summary(master_frame$funding_round_type)
    View(master_frame$funding_round_type)
    
   
#-------------------------------------------------------------------------------------------------------------------------
    
#   Table 2.1: Average Values of Investments for Each of these Funding Types
    
    funding_group <-group_by(master_frame, funding_round_type)
    summary(funding_group)
    View(funding_group)
    
    #FG_summary = funding_group_summary
    FG_summary <-summarise(funding_group, mean(raised_amount_usd, na.rm = T))
    colnames(FG_summary) <- c("Funding type", "Average_Funding")
    View(FG_summary)
    
#2.1
    #Average funding amount of venture type
    
    cat("Average funding amount of venture type = ",as.numeric(FG_summary[which(FG_summary$funding_round_type=="venture"),2]))
    
    #Average funding amount of venture type =  11748949

#2.2
    #Average funding amount of angel type
    
    cat("Average funding amount of angel type = ",as.numeric(FG_summary[which(FG_summary$funding_round_type=="angel"),2]))
    
    #Average funding amount of angel type =  958694.5
    
#2.3
    #Average funding amount of seed type
    
    cat("Average funding amount of seed type = ",as.numeric(FG_summary[which(FG_summary$funding_round_type=="seed"),2]))
    
    #Average funding amount of seed type =  719818
    
#2.4
    #Average funding amount of private equity type
    
    cat("Average funding amount of private equity type = ",as.numeric(FG_summary[which(FG_summary$funding_round_type=="private_equity"),2]))

    #Average funding amount of private equity type =  73308593
    
    
2.5
    #Considering that Spark Funds wants to invest between 5 to 15 million USD per investment round, which investment type is the most suitable for it? 
      
    FG_summary_criteria <- filter(FG_summary,Average_Funding >=5000000 & Average_Funding<=15000000)
    FT<-FG_summary_criteria$funding_round_type

#-------------------------------------------------------------------------------------------------------------------------
#   CHECK POINT 3 : Country Analysis =>
    
    
    funding_group$funding_round_type<- trimws(tolower(funding_group$funding_round_type))
    funding_group_ven<-filter(funding_group,funding_round_type=="venture")
    country_group_ven<-group_by(funding_group_ven,country_code)
    cntry_smry_ven<-summarise(country_group_ven,sum(raised_amount_usd,na.rm=TRUE))
    colnames(cntry_smry_ven) <- c("Country", "Total_Funding")
    
    #Arrange Country and Funding in descending order
    cntry_smry_ven<-arrange(cntry_smry_ven, desc(Total_Funding))
    View(cntry_smry_ven)

#3.1 
    #Spark Funds wants to see the top nine countries which have received the highest total funding 
    #(across ALL sectors for the chosen investment type)
    
    top9_ven<-head(cntry_smry_ven,9)
    View(top9_ven)
   
 
#3.2    
    #For the chosen investment type, make a data frame named top9 with the top nine countries 
    #(based on the total investment amount each country has received)
    
#1  group by country and summarizing
    country_group<-group_by(master_frame,country_code)
    cntry_smry<-summarise(country_group,sum(raised_amount_usd,na.rm=TRUE))
    
#2  Changing Column Names
    colnames(cntry_smry) <- c("Country", "Total_Funding")
    
#3  Arrange is descending order
    cntry_smry1<-arrange(cntry_smry, desc(Total_Funding))
    View(cntry_smry1)
    
#4  top 9 data frame from the Total Country summary
    top9<-head(cntry_smry1,9)
    View(top9)
#-------------------------------------------------------------------------------------------------------------------------    
#Table 3.1: Analysing the Top 3 English-Speaking Countries
    
#Analysing the Top 9 Countries with Countries_where_English_is_an_official_language.pdf
    
    
    #1. Top English-speaking country	        - > USA (United States of America)    
    #2. Second English-speaking country	      - > GBR (Great Britain)
    #3. Third English-speaking country	      - > INB (India)

#-------------------------------------------------------------------------------------------------------------------------
#   CHECK POINT 4 : Sector Analysis 1 =>
    
#1  Reading the mapping file
    mapping <- read.csv("mapping.csv",header = TRUE,stringsAsFactors = FALSE,na.strings = "")
    str(mapping)
    
    new_mapping <- gather(mapping, main_sector, main_sec_val, 2:10)
    str(new_mapping)
    
    new_mapping <- new_mapping[!(new_mapping$main_sec_val == 0), ]
    new_mapping <-  new_mapping[, -3]
    colnames(new_mapping) <- c("primary_sector", "main_sector")
    View(new_mapping)
    
#2  #Extract the primary sector of each category list from the category_list column
    master_frame$category_list <- trimws(master_frame$category_list)
    master_frame$primary_sector <- sub("[|].*","",master_frame$category_list)
    View(master_frame)

#-------------------------------------------------------------------------------------------------------------------------
#   CHECK POINT 5 : Sector Analysis 6 =>    
    
    #Master_frame and new_mapping - Left Join
    company_sec_map<-left_join(master_frame, new_mapping,by=NULL)
    View(company_sec_map)
    
    #Country Codes to Lower Case
    company_sec_map$country_code<- trimws(tolower(company_sec_map$country_code))
    company_sec_map$funding_round_type<- trimws(tolower(company_sec_map$funding_round_type))
    View(company_sec_map)

#-------------------------------------------------------------------------------------------------------------------------    
#   Table 5.1 : Sector-wise Investment Analysis
    

#1  Filter Top 3 countries USA, GBR, IND by raised_amount_usd and funding type= venture
    
    D1_tab<-filter(company_sec_map, country_code == "usa",funding_round_type=="venture",raised_amount_usd>5000000 & raised_amount_usd<15000000)
    View(D1_tab)
    
    D2_tab<-filter(company_sec_map, country_code == "gbr",funding_round_type=="venture",raised_amount_usd>5000000 & raised_amount_usd<15000000)
    View(D2_tab)
    
    D3_tab<-filter(company_sec_map, country_code == "ind",funding_round_type=="venture",raised_amount_usd>5000000 & raised_amount_usd<15000000)
    View(D3_tab)
    
#1  Count by No. of investments for USA, GBR and IND by funding type = Venture
    
    D1_tab_n=count(D1_tab)
    View(D1_tab_n)
    
    D2_tab_n=count(D2_tab)
    View(D2_tab_n)
    
    D3_tab_n=count(D3_tab)
    View(D3_tab_n)

#3  Count Sum of investments for USA, GBR and IND by funding type = Venture
      
    D1_tab_total=sum(D1_tab$raised_amount_usd,na.rm=TRUE)
    View( D1_tab_total)
    D2_tab_total=sum(D2_tab$raised_amount_usd,na.rm=TRUE)
    View( D2_tab_total)
    D3_tab_total=sum(D3_tab$raised_amount_usd,na.rm=TRUE)
    View( D3_tab_total)
    

#4  Sector wise raised amount for USA by funding type=venture
    
    D1_tab_sec_map<-group_by(D1_tab,main_sector)
    D1_tab_sec_map1<-summarise(D1_tab_sec_map,sum(raised_amount_usd,na.rm=TRUE))
    
    D1_tab_sec_map_cnt<-count(D1_tab_sec_map,sort=TRUE)
    
    D1_tab_sec_map1$Country<-"USA"
    D1_tab_sec_map_cnt$Country<-"USA"
    
    colnames(D1_tab_sec_map1) <- c("Sector", "Total_Funding","Country")
    colnames(D1_tab_sec_map_cnt) <- c("Sector", "No_of_investments","Country")
    
    D1_tab_sec_map2<-arrange(D1_tab_sec_map1,desc(Total_Funding))
    
#5  Top 3 sectors by investment amount
    top3_tab1<-head(D1_tab_sec_map2,3)
    View(top3_tab1)
    
#6  Top 3 sectors by number of investment
    top3_tab1_cnt<-head(D1_tab_sec_map_cnt,3)
    View(top3_tab1_cnt)
    
    write.csv(top3_tab1,file="top3_tab1.csv",sep=",")
    write.csv(top3_tab1_cnt,file="top3_tab1_cnt.csv",sep=",")
    
    
#7  Sector wise raised amount for GBR for funding type=venture
    D2_tab_sec_map<-group_by(D2_tab,main_sector)
    D2_tab_sec_map1<-summarise(D2_tab_sec_map,sum(raised_amount_usd,na.rm=TRUE))
    
    D2_tab_sec_map_cnt<-count(D2_tab_sec_map,sort=TRUE)
    
    D2_tab_sec_map1$Country<-"Great Britain"
    D2_tab_sec_map_cnt$Country<-"Great Britain"
    
    colnames(D2_tab_sec_map1) <- c("Sector", "Total_Funding","Country")
    colnames(D2_tab_sec_map_cnt) <- c("Sector", "No_of_Investments","Country")
    
    D2_tab_sec_map2<-arrange(D2_tab_sec_map1,desc(Total_Funding))
    
#8  Top 3 sectors by investment amount
    top3_tab2<-head(D2_tab_sec_map2,3)
    View(top3_tab2)
    
#9  Top 3 sectors by number of investment
    top3_tab2_cnt<-head(D2_tab_sec_map_cnt,3)
    View(top3_tab2_cnt)
    
    write.csv(top3_tab2,file="top3_tab2.csv",sep=",")
    write.csv(top3_tab2_cnt,file="top3_tab2_cnt.csv",sep=",")
    

#10 Sector wise raised amount for IND for funding type=venture
    D3_sec_map<-group_by(D3_tab,main_sector)
    D3_sec_map1<-summarise(D3_sec_map,sum(raised_amount_usd,na.rm=TRUE))
    
    #Calculating the sector wise raised amount  for ind for funding type=venture
    D3_tab_sec_map<-group_by(D3_tab,main_sector)
    D3_tab_sec_map1<-summarise(D3_tab_sec_map,sum(raised_amount_usd,na.rm=TRUE))
    
    D3_tab_sec_map_cnt<-count(D3_tab_sec_map,sort=TRUE)
    
    D3_tab_sec_map1$Country<-"India"
    D3_tab_sec_map_cnt$Country<-"India"
    
    colnames(D3_tab_sec_map1) <- c("Sector", "Total_Funding","Country")
    colnames(D3_tab_sec_map_cnt) <- c("Sector", "No_of_Investments","Country")
    
    D3_tab_sec_map2<-arrange(D3_tab_sec_map1,desc(Total_Funding))
    
# 11 Top 3 sectors by investment amount
    top3_tab3<-head(D3_tab_sec_map2,3)
    View(top3_tab3)
    
# 12  Top 3 sectors by number of investment
    top3_tab3_cnt<-head(D3_tab_sec_map_cnt,3)
    View(top3_tab3_cnt)
    
    write.csv(top3_tab3,file="top3_tab3.csv",sep=",")
    write.csv(top3_tab3_cnt,file="top3_tab3_cnt.csv",sep=",")
    

#13 Top main sector in USA which is "others" is analyzed for top 3 companies
    
    D4_tab<-filter(D1_tab_sec_map,main_sector=="Others")
    D4_tab_n=count(D4_tab)
    # No of companies in this category
    View(D4_tab_n)
    
    D4_investment<-group_by(D4_tab,name)
    D4_investment_det<-summarise(D4_investment,sum(raised_amount_usd,na.rm=TRUE))
    colnames(D4_investment_det) <- c("Company", "Total_Funding")
    arrange(D4_investment_det,desc(Total_Funding))
    
#14 Second Biggest main sector in USA which is "Cleantech...Semiconductors" is analyzed for top 3 companies

    D5_tab<-filter(D1_tab_sec_map,main_sector=="Cleantech...Semiconductors")
    D5_tab_n=count(D5_tab)
    # No of companies in this category
    View(D5_tab_n)
    
    D5_investment<-group_by(D5_tab,name)
    D5_investment_det<-summarise(D5_investment,sum(raised_amount_usd,na.rm=TRUE))
    colnames(D5_investment_det) <- c("Company", "Total_Funding")
    arrange(D5_investment_det,desc(Total_Funding))
    
#14 Third Biggest main sector in USA which is "Social..Finance..Analytics..Advertising" is analyzed for no of investments
    
    D10_tab<-filter(D1_tab_sec_map,main_sector=="Social..Finance..Analytics..Advertising")
    D10_tab_n=count(D10_tab)
    
    # No of companies in this category
    View(D10_tab_n)
    
#15 Top main sector in GBR which is "Others" is analyzed for top 3 companies
    
    D6_tab<-filter(D2_tab_sec_map,main_sector=="Others")
    D6_tab_n=count(D6_tab)
   
     # No of companies in this category
    View(D6_tab_n)
    
    D6_investment<-group_by(D6_tab,name)
    D6_investment_det<-summarise(D6_investment,sum(raised_amount_usd,na.rm=TRUE))
    colnames(D6_investment_det) <- c("Company", "Total_Funding")
    arrange(D6_investment_det,desc(Total_Funding))
    
#16 SECOND BIGGEST main sector in GBR which is "Cleantech...Semiconductors" is analyzed for top 3 companies
    
    D7_tab<-filter(D2_tab_sec_map,main_sector=="Cleantech...Semiconductors")
    D7_tab_n=count(D7_tab)
    
    # No of companies in this category
    View(D7_tab_n)
    
    D7_investment<-group_by(D7_tab,name)
    D7_investment_det<-summarise(D7_investment,sum(raised_amount_usd,na.rm=TRUE))
    colnames(D7_investment_det) <- c("Company", "Total_Funding")
    arrange(D7_investment_det,desc(Total_Funding))
    
#17 Third Biggest main sector in GBR which is "Social..Finance..Analytics..Advertising" is analyzed for no of investments
    
    D11_tab<-filter(D2_tab_sec_map,main_sector=="Social..Finance..Analytics..Advertising")
    D11_tab_n=count(D11_tab)
    
    # No of companies in this category
    View(D11_tab_n)
    
#18 Top main sector in INDIA which is "others" is analyzed for top 3 companies
    
    D8_tab<-filter(D3_tab_sec_map,main_sector=="Others")
    D8_tab_n=count(D8_tab)
    # No of companies in this category
    View(D8_tab_n)
    
    D8_investment<-group_by(D8_tab,name)
    D8_investment_det<-summarise(D8_investment,sum(raised_amount_usd,na.rm=TRUE))
    colnames(D8_investment_det) <- c("Company", "Total_Funding")
    arrange(D8_investment_det,desc(Total_Funding))
    
#19 Second Biggest main sector in INDIA which is "News..Search.and.Messaging" is analyzed for top 3 companies
    
    D9_tab<-filter(D3_tab_sec_map,main_sector=="News..Search.and.Messaging")
    D9_tab_n=count(D9_tab)
    # No of companies in this category
    View(D9_tab_n)
    
    D9_investment<-group_by(D9_tab,name)
    D9_investment_det<-summarise(D9_investment,sum(raised_amount_usd,na.rm=TRUE))
    colnames(D9_investment_det) <- c("Company", "Total_Funding")
    arrange(D9_investment_det,desc(Total_Funding))

#20 Third Biggest main sector in GBR which is "Social..Finance..Analytics..Advertising" is analyzed for no of investments    
    D12_tab<-filter(D3_tab_sec_map,main_sector=="Social..Finance..Analytics..Advertising")
    D12_tab_n=count(D12_tab)
    # No of companies in this category
    View(D12_tab_n)
    
    