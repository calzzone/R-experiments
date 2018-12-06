library(LexisPlotR)
library(timeDate)

mydata <- prepare.hmd("/home/calzzone/Desktop/Deaths_lexis.txt")
mydata$`F:M` <- mydata$Female / mydata$Male
mydata$`log10(F:M)` <- log10(mydata$`F:M`)

diagrams <- list()
for (year.start in seq(1950, 2018, 10)) {
  print(year.start)
  index <- 1 + (year.start  - 1950) / 10
  year.end = year.start + 10
  lexis_1 <- lexis.grid(year.start = year.start, year.end = year.end, 
                          age.start = 0, age.end = 50) %>%
    lexis.hmd(hmd.data = mydata, column = "log10(F:M)") 
  
  lexis.plot_1 <- lexis_1 + theme_void() +
    theme(legend.position = "none", plot.margin = unit(c(-0.5, -0.5, -0.5, -0.5), "cm") )+
    scale_fill_distiller(palette = "RdYlBu", limits = c(-1,1), na.value = "white", 
                         breaks=log10(c(0.1, 0.2, 0.5, 1, 2, 5, 10)),
                         labels=c("-10x", "-2x", "-5x", "1x", "2x", "5x", "10x")) +
    scale_x_date(name = "Year", date_labels = "%Y", 
                 breaks = seq(as.Date("1950/1/1"), as.Date("2019/1/1"), "10 years"),
                 limits = as.Date(c("1950-01-01", "2019-01-01"),format="%F")) +
    scale_y_continuous(name="Age", limits = c(0, 100), breaks=seq(0, 100, 5)) 
    
    lexis_2 <- lexis.grid(year.start = year.start, year.end = year.end, 
                          age.start = 50, age.end = 100) %>%
      lexis.hmd(hmd.data = mydata, column = "log10(F:M)")
    lexis.plot_2 <- lexis_2 + theme_void() +
    theme(legend.position = "none", plot.margin = unit(c(-0.5, -0.5, -0.5, -0.5), "cm") )+
            scale_fill_distiller(palette = "RdYlBu", limits = c(-1,1), na.value = "white", 
                                 breaks=log10(c(0.1, 0.2, 0.5, 1, 2, 5, 10)),
                                 labels=c("-10x", "-2x", "-5x", "1x", "2x", "5x", "10x")) +
            scale_x_date(name = "Year", date_labels = "%Y", 
                         breaks = seq(as.Date("1950/1/1"), as.Date("2019/1/1"), "10 years"),
                         limits = as.Date(c("1950-01-01", "2019-01-01"),format="%F")) +
            scale_y_continuous(name="Age", limits = c(0, 100), breaks=seq(0, 100, 5)) 
          
    
        diagrams[[index]] <- list(lexis_1, lexis_2, lexis.plot_1, lexis.plot_2)

}

plotlist <- list()
for (i in seq(1, 7))
  plotlist[[i]] <- diagrams[[i]][[4]]
for (i in seq(1, 7))
  plotlist[[i+8]] <- diagrams[[i]][[3]]


g <- cowplot::plot_grid(plotlist = plotlist, nrow = 2)
plot (g)
