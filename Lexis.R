#library(LexisPlotR)
library(timeDate)

Deaths_lexis <- read_table2("Desktop/Deaths_lexis.txt", na = ".", skip = 2) %>%
  filter(Age!="110+") %>%
  mutate(Age = as.integer(Age))

#str(Deaths_lexis)
#View(Deaths_lexis)  
#summary(Deaths_lexis)

#lexis.grid(year.start = 1950, year.end = 2018, age.start = 0, age.end = 100)
lexis.grid(year.start = 1950, year.end = 2018, age.start = 0, age.end = 30) %>%
  lexis.age(10) %>%
  lexis.year(2000) %>%
  lexis.cohort(1985) %>%
  lexis.lifeline(entry = "1991-09-23", exit = "2010-06-11") +
  theme_grey() +
  labs(title = "Lexis diagram", subtitle = "Hungary: 1950 to 2018, sex ratio",
       caption="Data from mortality.org") +
  scale_x_date(name = "Year", date_labels = "%Y", 
               breaks = seq(as.Date("1950/1/1"), as.Date("2019/1/1"), "10 years"),
               limits = as.Date(c("1950-01-01", "2019-01-01"),format="%F")) +
  scale_y_continuous(name="Age", limits = c(0, 100), breaks=seq(0, 100, 10)) 
  


mydata <- prepare.hmd("/home/calzzone/Desktop/Deaths_lexis.txt")
mydata$`F:M` <- mydata$Female / mydata$Male
mydata$`log10(F:M)` <- log10(mydata$`F:M`)
lexis.grid(year.start = 2000, year.end = 2018, age.start = 0, age.end = 5) %>%
  lexis.hmd(hmd.data = mydata, column = "Total") 

lexis.grid(year.start = 1950, year.end = 2018, age.start = 0, age.end = 100) %>%
  lexis.hmd(hmd.data = mydata, column = "log10(F:M)") +
  #lexis.cohort(1998) +
  theme_pubclean() +
  theme(legend.position = "right", 
        legend.title = element_text(size=8),
        legend.text = element_text(size=8),
        legend.key.height = unit(1, "cm") )+
  labs(title = "Lexis diagram", subtitle = "Hungary: 1950 to 2018, sex ratio",
       caption="Data from mortality.org") +
  scale_fill_distiller(palette = "RdYlBu", limits = c(-1,1), na.value = "white", 
                       breaks=log10(c(0.1, 0.2, 0.5, 1, 2, 5, 10)),
                       labels=c("-10x", "-2x", "-5x", "1x", "2x", "5x", "10x")) +
  scale_x_date(name = "Year", date_labels = "%Y", 
               breaks = seq(as.Date("1950/1/1"), as.Date("2019/1/1"), "10 years"),
               limits = as.Date(c("1950-01-01", "2019-01-01"),format="%F")) +
  scale_y_continuous(name="Age", limits = c(0, 100), breaks=seq(0, 100, 10)) 
  




