library('tidyr')
library('dplyr')
library('ggplot2')
library('readr')
life_expectancy <- read_csv("UNdata.csv")
head(life_expectancy)

subdata <- life_expectancy %>%
  filter(Year=="2000-2005") %>%
  select(`Country or Area`,Subgroup,Value) %>%
  spread(Subgroup,Value)

ggplot(data=subdata,aes(x=Male,y=Female))+geom_point(colour="white",fill="chartreuse3",shape=21,alpha=.55,size=5) + 
  geom_abline(intercept=0,linetype="dashed")+ 
  scale_x_continuous(limits=c(35,85)) + 
  scale_y_continuous(limits=c(35,85)) +
  labs(title="Life expectancy at Birth by country",subtitle="Years: 2000-2005",caption="Source: United Nations Statistics Division",x="Males",y="Females")

top_female <- subdata %>%
  arrange(Female-Male) %>%
  head(3)
top_male <- subdata %>%
  arrange(Male-Female) %>%
  head(3)

ggplot(data=subdata,aes(x=Male,y=Female,label=`Country or Area`))+geom_point(colour="white",fill="chartreuse3",shape=21,alpha=.55,size=5) + 
  geom_abline(intercept=0,linetype="dashed")+ 
  scale_x_continuous(limits=c(35,85)) + 
  scale_y_continuous(limits=c(35,85)) +
  labs(title="Life expectancy at Birth by country",subtitle="Years: 2000-2005",caption="Source: United Nations Statistics Division",x="Males",y="Females")+
  geom_text(data=top_female,size=3)+ 
  geom_text(data=top_male,size=3) + 
  theme_bw()


head(life_expectancy)

subdata2 <- life_expectancy %>%
  filter(Year %in% c("1985-1990","2000-2005")) %>%
  mutate(Sub_Year=paste(Subgroup,Year,sep='_')) %>%
  mutate(Sub_Year=gsub("-","_",Sub_Year)) %>%
  select(-Subgroup,-Year) %>%
  spread(Sub_Year,Value) %>%
  mutate(diff_Female=Female_2000_2005-Female_1985_1990,diff_Male=Male_2000_2005-Male_1985_1990)


ggplot(data=subdata2,aes(x=diff_Male,y=diff_Female,label=`Country or Area`))+geom_point(colour="white",fill="chartreuse3",shape=21,alpha=.55,size=5) + 
  geom_abline(intercept=0,linetype="dashed")+ 
  scale_x_continuous(limits=c(-25,25)) + 
  scale_y_continuous(limits=c(-25,25))  + 
  labs(title="Life expectancy at Birth by country",subtitle="Years: 2000-2005 to 1985-1990",caption="Source: United Nations Statistics Division",x="Males",y="Females")+
  theme_bw() + 
  geom_hline(yintercept=0,linetype=2) + 
  geom_vline(xintercept=0,linetype=2) + 
  geom_text(data=top,size=3) +
  geom_text(data=bottom,size=3)

top <- subdata2 %>%
  arrange(diff_Male+diff_Female) %>%
  head(3)
bottom <- subdata2 %>%
  arrange(diff_Male+diff_Female) %>%
  tail(3)







