
library("dplyr")
library("ggplot2")

library(tm)
library(SnowballC)
library(wordcloud)

investment_master<-read.csv('F:\\WSU\\Fall 2017\\Data Science\\Project\\CrunchBaseRProj\\crunchbase-data-master\\investments.csv',na.strings=c('',' '))

investment_master<-na.omit(investment_master)
attach(investment_master)

inv_cat<-investment_master[which(!is.na(company_category_list)),] %>% group_by(company_category_list) %>% summarise(Count=n()) %>% arrange(desc(Count)) %>% slice(1:20)

inv_cat_top10<-inv_cat[1:20,]

"Company Category" <-inv_cat_top10$company_category_list

"Country List" <- c('USA')


a=-inv_cat_top10$Count

abc<-ggplot(data=inv_cat_top10,aes(x=reorder(company_category_list,-Count),y=Count,fill=`Company Category`)) 


abc + geom_bar(stat = "identity",width = 0.6) + labs(title="Top 10 invested product categories", x="Company Category",y="Number of Investments") + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

# inv_fac <- investment_master %>% select(company_category_list,investor_country_code) %>% group_by(investor_country_code) %>% summarise(Count=n())

length(`Company Category`)

length(inv_fac$company_category_list)

inv_fac<- investment_master[which(company_category_list %in% `Company Category` & investment_master$investor_country_code %in% `Country List`),c(3,10)]

length(inv_fac$company_category_list)


ggplot(data=inv_fac,aes(x=company_category_list)) + geom_histogram(stat="count") + facet_grid(investor_country_code~.)+ theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

# + facet_grid()

attach(investment_master)

abc<-count(investment_master,company_name)

temp<- investment_master %>% select(company_name,raised_amount_usd) %>% group_by(company_name)%>% summarise(TotalFunded=sum(raised_amount_usd)) %>% filter(is.numeric(TotalFunded),!is.na(TotalFunded))


pal2 <- brewer.pal(9,"Oranges")

wordcloud(words=temp$company_name,freq = as.numeric(temp$TotalFunded) )
