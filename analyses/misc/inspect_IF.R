# inspect distribution impact factor
library(dplyr)
library(ggplot2)

data<- read.csv('data/derived-data/DAFNEE_db_with_impactact_Factor_filled.csv', header=T)

#after manual filling of IFs missing with Clarivate, theres n=37 missing for IF 2023 and n=61 missing for 5y IF

sub<- data %>% select(oa_source_issn_l:institution_type, X2023.JIF, X5.Year.JIF)
sub$X2023.JIF<- as.numeric(sub$X2023.JIF)
sub$X5.Year.JIF<- as.numeric(sub$X5.Year.JIF)

sub %>% group_by(publisher_type) %>% summarise(min=min(X2023.JIF, na.rm=TRUE),
                                               max=max(X2023.JIF, na.rm=TRUE),
                                               mean=mean(X2023.JIF, na.rm=TRUE),
                                               med=median(X2023.JIF, na.rm=TRUE),
                                               n=sum(!is.na(X2023.JIF)),
                                               nan=sum(is.na(X2023.JIF)))

sub %>% group_by(business_model) %>% summarise(min=min(X2023.JIF, na.rm=TRUE),
                                                      max=max(X2023.JIF, na.rm=TRUE),
                                                      mean=mean(X2023.JIF, na.rm=TRUE),
                                                      med=median(X2023.JIF, na.rm=TRUE),
                                                      n=sum(!is.na(X2023.JIF)),
                                                      nan=sum(is.na(X2023.JIF)))

sub %>% group_by(business_model) %>% summarise(min=min(X2023.JIF, na.rm=TRUE),
                                               max=max(X2023.JIF, na.rm=TRUE),
                                               mean=mean(X2023.JIF, na.rm=TRUE),
                                               med=median(X2023.JIF, na.rm=TRUE),
                                               n=sum(!is.na(X2023.JIF)),
                                               nan=sum(is.na(X2023.JIF)))

sub %>% group_by(publisher_type) %>% summarise(n=n())

nIF5y<- sum(!is.na(sub$X5.Year.JIF))
nIF23<- sum(!is.na(sub$X2023.JIF))

sub %>% ggplot(., aes(x=X5.Year.JIF)) + 
  #geom_histogram() +
  geom_histogram(binwidth=1) +
  facet_wrap(~ publisher_type) +
  ggtitle(paste('5 year IF, n=', nIF5y, sep='')) +
  theme_bw() 
sub %>% ggplot(., aes(x=X2023.JIF)) + 
    #geom_histogram() +
    geom_histogram(binwidth=1) +
    facet_wrap(~ publisher_type) +
  ggtitle(paste('IF 2023, n=', nIF23, sep='')) +
  theme_bw() 


sub %>% ggplot(., aes(x=publisher_type, y=X5.Year.JIF)) + 
  geom_violin(fill='seashell') +
  ggtitle(paste('5 year IF, n=', nIF5y, sep='')) +
  theme_bw() 
sub %>% ggplot(., aes(x=publisher_type, y=X2023.JIF)) + 
  geom_violin(fill='seashell') +
  ggtitle(paste('IF 2023, n=', nIF23, sep='')) +
  theme_bw() 
