##############################################################################################
#Changing directory
##############################################################################################

setwd("C:\\Users\\vanda\\OneDrive\\Desktop")
getwd()

##############################################################################################
# loading csv data to dataframe 
##############################################################################################
df<-read.csv("C:\\Users\\vanda\\OneDrive\\Desktop\\Superstore Online Sales data.csv")
df

df[df=='']<-NA #converting Null to Na

#############################################################################################
#Getting Familiar With Data
#############################################################################################
dim(df)  #Finding shape of data
colnames(df)  #name of columns

head(df)# Checking the head of the dataset

tail(df)# Checking the tail of the dataset

str(df)  #Finding structure of data
summary(df)#getting summary of data
sapply(df, class)  # show classes of all columns



df_orginal<-df  #make a copy

#################################################################################################
#Data Cleaning 
#################################################################################################

#Removing Unwanted Columns

unique(df[c("Country")]) #checking for unique value in column country
df[]
# here i am removing column named 'country' as it has only one value 'United States' 
df[,c("Country")]<- NULL
colnames(df)

#Column named "Postal.Code"  does not have any meaningful information regarding this project so i am going to drop that column aswell
df[,c("Postal.Code")]<- NULL


#Changing the column names by replacing '.' dot to '_'
names(df)<-chartr(".","_",names(df))
df


#While importing data the datatype of Order.Date and Ship.Date is character , converting the datatype from character to date

Order_Date<-as.Date(df$Order_Date,"%m/%d%Y")
class(Order_Date)
Ship_Date<-as.Date(df$Ship_Date,"%m%d%Y")
class(Ship_Date)



# number of missing values:
colSums(is.na(df))
#Percentage of missing values:
round(colMeans(is.na(df))*100,2)#2:digit=2

#Finding duplicated observations
duplicated(df)

sum(duplicated((df)))#--> there is 1 duplicated data 
which(duplicated(df))

r1<-which(duplicated(df))
df<-df[-r1,]

#to get unique values of attributes in each column
library(dplyr)
sapply(df,n_distinct)

# how many times each states are repeated in data set

dt<-df %>% 
  count(State)



###########################################################################################################################

# Q1. What is the Distribution of each region in the data set?

counts <- table(df$Region)
barplot(counts, main="Orders by Region",
        xlab="Region",ylab = "Count",col = 'red')

###########################################################################################################################

#Q2 What is the Percentage distribution of category in the data set ?
count<-table(df$Category)
count
# Pie Chart with Percentages
slices <- c(count[1], count[2], count[3])
lbls <- c("Furniture", "Office Supplies", "Technology")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(slices,labels = lbls, col=rainbow(length(lbls)),
    main="Percentage by Category")

###########################################################################################################################

#Q3 What is the Distribution of Order Quantity in the data set?

#Five number Summary
summary(df$Quantity)

#Box plot for univariate analysis of quantity
boxplot(df$Quantity,xlab="Quantity",col="pink")

############################################################################################################################

#Q4 What is the distribution of Profit/Loss in the data set ? 

summary(df$Profit)

hist(df$Profit, breaks=100,xlab="Profit", 
     main="Histogram of Profit/Loss",xlim=c(-1000, 1000),col="blue")


############################################################################################################################
#Q5 What is the correlation between Sales,Quantity, Discount and Profit?

library(corrplot)

mycor <-df[,c(12,13,14,15)] #Selecting sales,quantity,discount and profit column
View(mycor)                 # to view the selected data
mycor.cor=cor(mycor)
mycor.cor                  #correlation table
corrplot(mycor.cor)
corrplot(mycor.cor, tl.col = "red", bg = "White", tl.srt = 10,      #plotting correlation for visualization
         title = "\n\n Correlation of sales,Quantity, Discount and Profit\n",
         addCoef.col = "black", type = "full")             
###########################################################################################################################

#Q6 What is the %distribution of sales by category and which subcategories are classified in to which category ?

library(ggplot2)
library(dplyr)
library(webr)

dk<-as.data.frame(df)

PV <-dk%>% group_by(Category,Sub_Category) %>% summarise(n=sum(Profit))
print(PV)
PV <-dk%>% group_by(Category,Sub_Category) %>% summarise(n=sum(Sales))
print(PV)
PieDonut(PV,aes(Category, Sub_Category,count=n),title="Sales by Category")
PieDonut(PV,aes(Category, Sub_Category,count=n),title="Profit by Category")       

############################################################################################################################
#Q7 What is the best selling category with maximum quantity, sales and profit?

tbl<-aggregate(. ~ Category, # Keep some variables
          df[ , c("Sales", "Profit", "Quantity","Category")],
          sum)
tbl

#Visualization
library(plotly)
library(ggplot2)
options(scipen=100000)
fig1<-ggplot(tbl,                                      # Grouped barplot using ggplot2
             aes(x = Category,
                 y = Sales,
                 fill = Sales)) +
  geom_bar(stat = "identity",
           position = "dodge")


fig2<-ggplot(tbl,                                      # Grouped barplot using ggplot2
             aes(x = Category,
                 y = Profit,
                 fill = Profit)) +
  geom_bar(stat = "identity",
           position = "dodge")

fig3<-ggplot(tbl,                                      # Grouped barplot using ggplot2
             aes(x = Category,
                 y = Quantity,
                 fill = Quantity)) +
  geom_bar(stat = "identity",
           position = "dodge")
fig6 <- subplot(fig1, fig2, fig3,titleX=TRUE,titleY = TRUE,margin=0.05)

####################################################################################################################################

#Q8 What is the best selling Sub Category ? Which Sub category has maximum sales and profit? 
tbl2<-aggregate(. ~ Sub_Category, 
               df[ , c("Sales", "Profit","Sub_Category")],
               sum)
tbl2

#Visualization

fig4<-ggplot(tbl2,                                      # Grouped barplot using ggplot2
             aes(x =Sub_Category,
                 y = Sales,
                 fill = Sales)) +
  geom_bar(stat = "identity",
           position = "dodge" )+ scale_fill_continuous(type = "viridis")+ theme(axis.text.x = element_text(angle=90, vjust=.5, hjust=1))


fig5<-ggplot(tbl2,                                      # Grouped barplot using ggplot2
             aes(x = Sub_Category,
                 y = Profit,
                 fill = Profit)) +
  geom_bar(stat = "identity",
           position = "dodge")+ scale_fill_continuous(type = "viridis")+theme(axis.text.x = element_text(angle=90, vjust=.5, hjust=1))


fig6 <- subplot(fig4, fig5, titleX = TRUE,titleY = TRUE,margin=0.05) %>% 
  layout(title = 'Side By Side Subplots')
fig6
#####################################################################################################################################################

#WQ9. Is there any relationship between Sales and discount? And Discount and Profit?

library(ggplot2)
#Summary
cor(df$Sales,df$Discount,method = "spearman")
cor(df$Sales,df$Discount)  # pearson 

#Sales Vs Discount
ggplot()+geom_point(data=df,aes(x=Discount,y=Sales,color=Sub_Category))+ggtitle("Relation Between Sales and Discount")
# Profit Vs discount v
ggplot()+geom_point(data=df,aes(x=Discount,y=Profit,color=Sub_Category))+ggtitle("Relation Between Profit and Discount")
#####################################################################################################################################################################
#Q10. What is the distribution of profit each year and which year has the highest profit ?
  
library(dplyr)
library(lubridate)
year(df$Order_Date)
df$year <- format(as.Date(df$Order_Date, format="%m/%d/%Y"),"%Y")
temp = df %>%  group_by(`year`) %>% summarise(YearlyProfit = sum(Profit)) %>% 
  mutate(Profitability = ifelse(YearlyProfit  > 80000, "Highly Profitable", 
                                ifelse(YearlyProfit   < 50000, "Unprofitable", "Profitable")))


#visualization
Library(ggplot2)
fig2<-ggplot(temp,                             # Grouped barplot using ggplot2
             aes(x = year,
                 y = YearlyProfit,
                 fill = YearlyProfit)) +
  geom_bar(stat = "identity",
           position = "dodge")+ggtitle("Profit Achieved By Year")


###################################################################################################################################################################
#Q11.Which is the preferable shipment mode by customers in each segment ?

#Summary
tbl17<-table(df$Ship_Mode,df$Segment)
tbl17

#test of independancy
chisq.test(tbl17)

#Visualization
library(ggplot2)
ggplot(data = df, aes(x = Quantity, y = Sales, fill = Ship_Mode) )+ geom_bar(stat = "identity")
+ scale_fill_brewer(palette="Spectral")

############################################################################################################################################################
#Q12.What is the distribution of sales and profit margin in each region?
  
TotalSales<-aggregate(Sales~Region,df,sum)

library(ggplot2)
qplot(Region,Sales, data = df, 
      geom="boxplot", fill = Region,ylim=c(1,500))

#Profit margin in each region

df$Profit_Margin<-(df$Profit/df$Sales)*100
head(df,10)

p<-ggplot(df,aes(x=Profit_Margin,fill=Region,color=Category))
+geom_histogram(position="identity",alpha=0.5)
p <- p + labs(title = "Profit Margin by Region for Each Category")
p

#############################################################################################################################################################

#Q13. What is the distribution of orders by State and Which are the top 10 states with highest profit?

#Distribution of product category by State

Library(ggplot2)
ggplot(df, aes(x=State, fill=Category)) + 
  geom_bar() + 
  theme_bw()+ 
  labs(title="Product Category by State")+ 
  theme(axis.text.x = 
          element_text(angle=90, vjust=.5, hjust=1))

#Top 10 states with highest profit

library(dplyr)
tbl2<-aggregate(. ~ State, 
                df[ , c("Sales", "Profit","State")],
                sum)
tbl2
dt<-top_n(tbl2, 10, Profit) 
ggplot(dt, aes(State, Profit, fill = State)) +     # Using default colors
  geom_bar(stat = "identity")+ ggtitle("Top 10 States with Highest Profit")


###################################################################################################################################################

#Q14. What is the distribution of different categories ordered in different Segments?

tbl <- table(df$Segment,df$Category) # the contingency table
tbl


#Test of independency
chisq.test(tbl) #Test of independancy

#Visualization of Bar Plot with Colors and Legend

counts <- table(df$Segment,df$Category)
barplot(counts, main="Segment Vs Category",
        xlab="Category", col=c("blue","yellow","red"),
        legend = rownames(counts), 
        beside=TRUE,args.legend = list(x = "topright", inset = c(- 0.05,0))) 

###################################################################################################################################################

#Q15 Sales and Profit Comparison between 10 top customers

library(dplyr)
tbl2<-aggregate(. ~ Customer_Name, # Keep some variables
                df[ , c("Sales", "Profit","Customer_Name")],
                sum)
tbl2
dk<-top_n(tbl2, 10, Sales) 
dt<-top_n(tbl2, 10, Profit) 
p<-ggplot(dk, aes(x=Customer_Name, fill=Sales, color=Customer_Name)) +
  geom_histogram(aes(y=..density..),position="identity", alpha=0.5)+
  geom_density(alpha=0.1,bin=50)
p
library(ggplot2)
p <-ggplot(dk, aes(Customer_Name, Sales))
p +geom_bar(stat = "identity", aes(fill = Sales), position = "dodge") +geom_line(aes(x=Customer_Name, y=Profit),stat="identity")
xlab("Months") + ylab("Count") +
  ggtitle("Sales of 10 best customer") +
  theme_bw()

q<-ggplot(dt, aes(Customer_Name, Profit))
q +geom_bar(stat = "identity", aes(fill = Profit), position = "dodge") +
  xlab("Customer Name") + ylab("Profit") +
  ggtitle("Profit From 10 best customer") +
  theme_bw()
fig <- subplot(p,q, nrows = 2, shareY = TRUE) %>% 
  layout(title = "Profit from 10 best Customers")

##############################################################################################################################################################

