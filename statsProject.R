data <- read.csv("Web_Scrapped_websites.csv", na.strings = c("", "NA", "-"), stringsAsFactors = F)
data$Traffic_Rank <- as.numeric(data$Traffic_Rank)

data$Avg_Daily_Visitors<- gsub(" ", "", data$Avg_Daily_Visitors, fixed = TRUE)
data$Avg_Daily_Visitors<- as.numeric(data$Avg_Daily_Visitors)

data$Avg_Daily_Pageviews<- gsub(" ", "", data$Avg_Daily_Pageviews, fixed = TRUE)
data$Avg_Daily_Pageviews<- as.numeric(data$Avg_Daily_Pageviews)

# Central Limit Theoram for Avg_Daily_Visitors: One Sample has 50 observations, 135 samples
dataCLT<- data %>% group_by(country) %>% summarise(average = mean(Avg_Daily_Visitors))
dataCLT<- dataCLT %>% filter (average != "NA")
qqnorm(dataCLT$average)

# Data cleaning for the social media columns where values were mentioned in 'K' for thousand and 'M' for millions

web_data<-na.omit(data) %>% select("Website" ,"Avg_Daily_Visitors","Facebook_likes","Twitter_mentions","Google_pluses","LinkedIn_mentions","Pinterest_pins","StumbleUpon_views","Traffic_Rank")
dataSocial<-na.omit(data) %>% select("Website" ,"Avg_Daily_Visitors","Facebook_likes","Twitter_mentions","Google_pluses","LinkedIn_mentions","Pinterest_pins","StumbleUpon_views","Traffic_Rank")
for(j in 3:8){
  for(i in 1 : length(dataSocial$Website)){
    if(str_sub(dataSocial[i,j],-1,-1)=="K"){
      dataSocial[i,j]<-as.numeric(str_sub(dataSocial[i,j],1,-2))*1000
    }
    else if(str_sub(dataSocial[i,j],-1,-1)=="M"){
      dataSocial[i,j]<-as.numeric(str_sub(dataSocial[i,j],1,-2))*1000000
    }
    else
      dataSocial[i,j]<-as.numeric(str_sub(dataSocial[i,j],1,str_length(dataSocial[i,j])))
  }}
for(i in 3:8){dataSocial[i]<-as.numeric(unlist(dataSocial[i]))}

dataUS<- data %>%  filter(country == "United States")

# Multivariateanalysis using GGPairs:

ggpairs(dataSocial, columns = 3:9)

# Distribution of average daily visitors
hist(log10(data$Avg_Daily_Visitors))
qqnorm(data$Avg_Daily_Visitors)
qqline(data$Avg_Daily_Visitors)

# Distribution of Average Daily Page Views
hist(log10(data$Avg_Daily_Pageviews))
qqnorm(data$Avg_Daily_Pageviews)
qqline(data$Avg_Daily_Pageviews)

# Distribution of Daily Reach 
hist(log(data$Daily_Pageviews_per_user))
qqnorm(data$Daily_Pageviews_per_user)
qqline(data$Daily_Pageviews_per_user)

# Correlation, covariance of COuntry Rank vs Avg Daily Visitors for USA
cor(dataUS$Country_Rank, dataUS$Avg_Daily_Visitors)
cov(dataUS$Country_Rank, dataUS$Avg_Daily_Visitors)
plot(dataUS$Country_Rank, dataUS$Avg_Daily_Visitors)
abline(lm(dataUS$Avg_Daily_Visitors~dataUS$Country_Rank))


# correlation, covariance of country rank vs average views
cor(dataUS$Country_Rank, dataUS$Avg_Daily_Pageviews)
cov(dataUS$Country_Rank, dataUS$Avg_Daily_Pageviews)
plot(dataUS$Country_Rank, dataUS$Avg_Daily_Pageviews)
abline(lm(dataUS$Avg_Daily_Pageviews ~ dataUS$Country_Rank))


# correlation of country rank vs traffic rank
plot(dataUS$Country_Rank, dataUS$Traffic_Rank)
abline(lm(dataUS$Traffic_Rank~ dataUS$Country_Rank))

# Categorical Data Summary for Trustworthiness, Child Safety and Privacy
dataCat <- na.omit(dataUS) %>% 
            select("Trustworthiness", "Child_Safety", "Privacy")

# Trustworthiness
dataTrust <- dataCat %>% group_by(Trustworthiness=as.factor(dataCat$Trustworthiness)) %>% tally()
ggplot(dataTrust, aes(x=Trustworthiness,y=n))
  +geom_bar(stat="identity", aes(fill = Trustworthiness))
  + ggtitle("Trustworthiness of websites in US")
  +labs(x="Trustworthiness",y="Count")+theme(plot.title = element_text(hjust = 0.5))

# Child Safety
dataChild <- dataCat %>% 
              group_by(Child_Safety=as.factor(dataCat$Child_Safety)) %>% 
              tally()

ggplot(dataChild, aes(x=Child_Safety,y=n))
  + geom_bar(stat="identity", aes(fill = Child_Safety))
  + ggtitle("Child Safety of websites in US")
  + labs(x="Child Safety",y="Count")
  + theme(plot.title = element_text(hjust = 0.5))

# Privacy
dataPrivacy <- dataCat %>% 
                group_by(Privacy=as.factor(dataCat$Privacy)) %>% 
                tally()

ggplot(dataPrivacy, aes(x=Privacy,y=n))+
    geom_bar(stat="identity", aes(fill = Privacy))+ 
    ggtitle("Privacy of websites in US")+
    labs(x="Privacy",y="Count")+
    theme(plot.title = element_text(hjust = 0.5))

