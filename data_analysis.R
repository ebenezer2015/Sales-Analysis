library(dplyr)
library(ggplot2)
library(lubridate)


buyers <- read.csv("buyers.csv")
sales <- read.csv("sales_data.csv")
managers <- read.csv("managers.csv")
state <- read.csv("state_data.csv") 
finance <- read.csv("financial_statement.csv")

################################
### Spliting the Date Column
################################
datetxt <- sales$Date
datetxt <- as.Date(datetxt, format = "%d/%m/%Y")
df <- data.frame(date = datetxt,
                 year = as.numeric(format(datetxt, format = "%Y")),
                 month = as.numeric(format(datetxt, format = "%m")),
                 day = as.numeric(format(datetxt, format = "%d")))


datetxt_2 <- finance$Date
datetxt_2 <- as.Date(datetxt_2, format = "%d/%m/%Y")
df_2 <- data.frame(date = datetxt_2,
                   year = as.numeric(format(datetxt_2, format = "%Y")),
                   month = as.numeric(format(datetxt_2, format = "%m")),
                   day = as.numeric(format(datetxt_2, format = "%d")))
###########################
## Swap the Column ######
########################
df$month_new = df$day
df$day_new = df$month

df = df[-c(3,4)]

df_2$month_new = df_2$day
df_2$day_new = df_2$month

df_2 = df_2[-c(3,4)]
###############################
#### Dates by Merging Data ###
##############################
sales$Date <-ISOdate(year = df$year, month = df$month_new, day = df$day_new)

finance$Date <-ISOdate(year = df_2$year, month = df_2$month_new, day = df_2$day_new)


####################################
### 2016/17 Financial Year Data ####
####################################

sales_new <- sales %>% 
  select(Date, Chain, Postcode, Category, Total.Units, Sale.Price, Cost.Price) %>%
  filter(Date >= as.Date("2016-07-01") & Date <= as.Date("2017-07-01"))

finance_new <- finance %>% 
  select(Date,Financial.Year,FY.Qtr,FY.Month)%>%
  filter(Date >= as.Date("2016-07-01") & Date <= as.Date("2017-07-01"))

#### Merging the CSV files ####
###############################
sales_state = merge(sales_new, state)
sales_state_buyers = merge(sales_state, buyers)
sales_state_buyers_managers = merge(sales_state_buyers,managers)
full_data = merge(sales_state_buyers_managers,finance_new)


#####################################
### Additional calculated columns ###
#####################################
full_data$Revenue <- full_data$Total.Units*full_data$Sale.Price
full_data$total_cost <- full_data$Total.Units*full_data$Cost.Price
full_data$Profit <- full_data$Revenue - full_data$total_cost

#################################
#### Re-arrange column names ####
#################################
col_order <- c("Date","Manager","Buyer","State","Postcode","Suburb","Financial.Year","FY.Qtr","FY.Month",
               "Category","Chain","Total.Units","Sale.Price","Cost.Price","Revenue","total_cost","Profit")
full_data <- full_data[, col_order]

###########################################
### Required Key Performance Indicators ###
###########################################
Totalsales = paste0("£",format(round(sum(full_data$Revenue)/1000000,1), trim=TRUE),"M")
GrossProfit = paste0("£",format(round(sum(full_data$Profit)/1000000,1), trim=TRUE),"M")
GP_Percentage = paste0(round(sum(full_data$Profit)/sum(full_data$Revenue)*100,2),"%")
Total_Order = sum(full_data$Total.Units)

state_with_the_highest_sales <- full_data %>% 
  group_by(State) %>% 
  summarize(Sales=sum(Revenue),
            Units_Sold =sum(Total.Units)) %>% 
  arrange(desc(Sales)) %>% top_n(1) %>% select(State)


mgr_with_the_highest_sales <- full_data %>%
  group_by(Manager) %>%
  summarize(Sales=sum(Revenue)) %>%
  arrange(desc(Sales)) %>% top_n(1) %>% select(Manager)


#######################
### Required Plots ####
######################
#1. Sales Analysis by Buyers
buyers_sales_table <- full_data %>%
  group_by(Buyer) %>%
  summarize(Sales=sum(Revenue),
            count= n()) %>%
  mutate(percent = Sales / sum(Sales),
         percentlabel = paste0(round(percent*100), "%"))


Buyer_sales_plot <- ggplot(buyers_sales_table, 
                           aes(x = reorder(buyers_sales_table$Buyer, buyers_sales_table$percent),
                               y = percent)) + 
  geom_bar(stat = "identity", 
           fill = "indianred3", 
           color = "black") +
  geom_text(aes(label = percentlabel), 
            hjust = -0.25) +
  labs(x = "Buyer", 
       y = "Percent", 
       title  = "Sales Analysis by Buyers")+
  coord_flip()


#2. Sales Analysis by Managers
Managers_sales_table <- full_data %>%
  group_by(Manager) %>%
  summarize(Sales=sum(Revenue),
            count= n()) %>%
  mutate(percent = Sales / sum(Sales),
         percentlabel = paste0(round(percent*100), "%"))

Managers_sales_plot <- ggplot(Managers_sales_table, 
                              aes(x = reorder(Manager, percent),
                                  y = Managers_sales_table$percent)) + 
  geom_bar(stat = "identity", 
           fill = "indianred3", 
           color = "black") +
  geom_text(aes(label = percentlabel), 
            hjust = -0.25) +
  labs(x = "Manager", 
       y = "Percent", 
       title  = "Sales Analysis by Managers")+
  coord_flip()

Quaterly_sales_table <- full_data %>%
  group_by(FY.Qtr) %>%
  summarize(Sales=sum(Revenue),
            count= n()) %>%
  mutate(percent = Sales / sum(Sales),
         #Sales_in_Million = paste0(round(Quaterly_sales_table$Sales/1000000,1),"M"),
         sales_in_million = round(Sales/1000000,1),
         #percentlabel = paste0(round(percent*100), "%"))
         percent_of_totalsales = round(percent*100))