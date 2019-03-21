
library(dplyr)
library(plotly)
library(ggplot2)
library(stringr)
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(wordcloud2)
library(ggrepel)
setwd('/NYCDA/Python/dhotel/')
tp1 = read.csv('tp1.CSV')


#####################
####DAta Cleanig#####
#####################


t = read.csv('d1_d7.csv')
t2 = read.csv('d8_d20.csv')
t3 = read.csv('dhotel2_Bay_Lake_Tower.csv')
dhotel = rbind(t, t2, t3)
##swith date
dhotel$ratingDate = as.character(dhotel$ratingDate)
dhotel$rdate = as.Date(dhotel$ratingDate, '%d-%b-%y')
dhotel$months= as.character(months(dhotel$rdate))  
dhotel$year = as.numeric(year(dhotel$rdate))  
dhotel$year2 = str_sub(dhotel$ratingDate, -2)
dhotel$year4 = as.Date(dhotel$year2, '%y')
dhotel$year5 = year(dhotel$year4)

##switch month
c1 = c('January', "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")
c2 = c('1','2','3','4','5','6','7','8','9','10','11','12')
for (i in 1:12) { dhotel$month = gsub(c1[i], c2[i], dhotel$month)}
dhotel$month = as.numeric(dhotel$month)


dhotel$star = substr(dhotel$stars, start = 1, stop = 3)
dhotel$bubble = substr(dhotel$ratingbubble, 25, 26)
dhotel$bubble = as.numeric(dhotel$bubble)/10
dhotel$Hotel = gsub("Disney's", '', dhotel$Hotel)

write.csv(dhotel, file = 'dhotel.csv')
dhotel = read.csv('dhotel.csv')

### Combine price #######

p1 = read.csv('hotelprice.csv')
p1$Hotel = gsub("Disney's", '', p1$Hotel)
colnames(p1) = c('Hotel', 'pfrom', 'pto')
write.csv(p1, 'hotelprice.csv')

tp1 = merge(dhotel,p1, by = 'Hotel')
write.csv(tp1, file = 'tp1.csv')

test = tp1 %>% group_by(., Hotel, pfrom, pto) %>% summarise( n())
tp1

#################################


##@@1 star and customer review (relationship between star and review )
dhotel %>% 
  group_by(., Hotel, star)  %>% select(., Hotel, star, bubble) %>%
  summarise(., review = mean(bubble)) %>%
  plot_ly(., x = ~Hotel, y = ~star, type = 'bar', name = 'Stars') %>%
  add_trace(., y = ~review, name = 'Customer review') %>%
  layout(yaxis = list(title = 'Points'), barmode = 'group')

##@ review by stars

dhotel %>% 
  group_by(., star)  %>% select(., star, bubble) %>%
  summarise(., review = mean(bubble)) %>%
  plot_ly(., x = ~star, y = ~review, type = 'bar', name = 'Star') %>%
  layout(yaxis = list(title = 'Review'), xaxis = list(title = 'Star'), barmode = 'group')


## @@ hotel reviews by year
s = dhotel %>% 
  group_by(., Hotel, year5)  %>%
  summarise(., review = mean(bubble)) %>%
  

#single hotel
test = dhotel %>% 
  group_by(., Hotel, year5)  %>%
  filter(., Hotel == "Disney's Animal Kingdom Lodge") %>%
  summarise(., review = mean(bubble)) %>%
  arrange(., desc(year5)) %>%
  plot_ly(., x = ~year5, y = ~review, type = 'scatter', name = 'Review', mode = 'lines') %>%
  layout(yaxis = list(title = 'Type'), xaxis = list(title = 'Points'))

library(dplyr)

###@@### all hotel review by year

tp1 %>% 
  group_by(., Hotel, year5)  %>%
  summarise(., review = mean(bubble)) %>%
  ggplot(aes(x = year5, y = review)) +
  geom_line(aes(color = Hotel)) +
  facet_wrap(~ Hotel)+
  xlab('Year') + ylab('Review Bubble')

####@@ hotel price & star scatter

# @@ price from
tp1 %>% 
  group_by(., star, pfrom)%>%
  summarise(., number = n()) %>% 
  ggplot(aes(x =pfrom, y = star )) +
  geom_jitter(color = star, size = c(4)) +
  xlab('Price From') + ylab('Stars')+
  scale_fill_discrete(name = "Price")

# @@ price to
tp1 %>% 
  group_by(., star, bubble)%>%
  summarise(., number = as.character(n())) %>% 
  ggplot(aes(x =bubble, y = star)) +
  geom_jitter(aes(color = as.character(star), size = number)) + xlab('Review') + ylab('Stars')


## Price vs Stars
tp1 %>% 
  group_by(., star, pfrom, pto)%>%
  summarise(., number = n()) %>%
  ggplot(aes( x = range(100,2300), y = star)) +
  geom_jitter(aes(x = pfrom, color = as.character(star)), size = c(4)) +
  geom_jitter(aes(x = pto, color = as.character(star)), shape = c(15), size = c(4)) +
  xlab('Price') + ylab('Stars')

##@@@ price reviews scatter ####


tp1 %>%
  group_by(., Hotel, year5, pfrom) %>%
  summarise(., review = round(mean(bubble),1)) %>%
  mutate(Review = as.character(review)) %>% 
  filter(., year5 == '2017') %>%
  ggplot(aes(x = pfrom, y = review)) +
  geom_jitter(aes(color = Review), size = c(5), shape = c(17)) +
  geom_text_repel(aes(x = pfrom, y = review, label = Hotel), color = 'navy') +
  xlab('Price From') + ylab('Review') + ggtitle('Price Review Distribution') +
  scale_fill_discrete(name = "Review")

  
##@@ star price box 
tp1 %>%
  group_by(., star, year5, pfrom) %>%
  summarise(., review = round(mean(bubble),1)) %>%
  filter(., year5 == '2017') %>%
  ggplot(aes(x = star, y = pfrom)) +
  geom_boxplot(aes(fill = as.character(star))) +
  scale_fill_discrete(name = "Stars") +
  labs(x = 'Stars', y = 'Price From')

###################
### Wordcloud #####
###################

tp1 = read.csv('tp1.CSV')
tpx = readLines('review_titl.txt')
hcorpus <- Corpus(VectorSource(tpx))
hcorpus <- tm_map(hcorpus, PlainTextDocument)
hcorpus = tm_map(hcorpus, tolower)
hcorpus <- tm_map(hcorpus, removePunctuation)
hcorpus <- tm_map(hcorpus, removeWords, c('stay','disney', 'resort', 'hotel',
                                          'love','fantastic', 'locat',
                                          'good','best', 'love',stopwords('english')))
hcorpus <- tm_map(hcorpus, stemDocument)

myDTM = TermDocumentMatrix(hcorpus)
m = as.matrix(myDTM)
v = sort(rowSums(m),decreasing=TRUE)
d = data.frame(word = names(v),freq=v)
head(d, 10)

####micky
set.seed(1)

############################3

s = tp1 %>% select(., bubblec, partial_entry, year5) %>%
  filter(., bubblec == '1'| bubblec == '2', year5 == '2016') 
write.table(s$partial_entry, file = 'review_low.text')
tpx = readLines('review_low.text')
hcorpus <- Corpus(VectorSource(tpx))
hcorpus <- tm_map(hcorpus, PlainTextDocument)
hcorpus = tm_map(hcorpus, tolower)
hcorpus <- tm_map(hcorpus, removePunctuation)
hcorpus <- tm_map(hcorpus, removeWords, stopwords('english'))
hcorpus = tm_map(hcorpus, removeWords, c('stay','disney', 'resort', 'hotel',
                                         'love','fantastic', 'locat','night','first','check',
                                         'good','best', 'love', 'one',
                                         'year', 'day', 'famili', 'get',
                                         'just', 'servic', 'arriv', 'book',
                                         'trip', 'year', 'park', 'experi'
                                         ,'disappoint', 'like', 'book', 'got',
                                         'two'))
hcorpus <- tm_map(hcorpus, stemDocument)

myDTM = TermDocumentMatrix(hcorpus)
m = as.matrix(myDTM)
v = sort(rowSums(m),decreasing=TRUE)
d = data.frame(word = names(v),freq=v)
head(d, 40)

d %>% filter(., word == 'room' | word == 'servic' | word == 'clean' | word == 'properti'| word == 'bed'|
word == 'disappoint'| word == 'never' | word == 'staff' | word == 'front' |word == 'small'| word == 'food' | word == 'desk') %>%
ggplot(aes(x = word, y = freq)) + 
geom_col(fill = c('skyblue')) + coord_flip() + ylab('Frequency') + xlab('Word')

wordcloud2(d, figPath = 'micky2.png', size = 2)

write.csv(d, file = 'complain.csv')

##########################
#####Another Woud Cloud###
##########################
set.seed(1234)
wordcloud(d$word, freq = d$freq, min.freq = 1, max.words = 500, random.order = FALSE,
          rot.per=0.35, colors=brewer.pal(8, "Dark2"))
##########################


s = tp1 %>% select(., bubblec, partial_entry, year5) %>%
  filter(., bubblec == '1', year5 == '2017') %>%
  



tp1$bubblec = as.character(tp1$bubble)
tp1 %>% 
  group_by(., bubblec, pfrom, pto)%>%
  summarise(., ave = mean(bubblec)) %>%
  ggplot(aes( x = range(100,2300), y = bubblec)) +
  geom_jitter(aes(x = pfrom, color = bubblec)) +
  xlab('Price') + ylab('Reviews')


dhotel %>% 
  group_by(., Hotel, star)  %>% select(., Hotel, star, bubble) %>%
  summarise(., review = mean(bubble)) %>%
  plot_ly(., x = ~Hotel, y = ~star, type = 'scatter', name = 'Stars', mode = 'lines') %>%
  add_trace(., y = ~review, name = 'Customer review') %>%
  layout(yaxis = list(title = 'Point'))












