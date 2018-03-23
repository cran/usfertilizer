## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- eval=FALSE---------------------------------------------------------
#  # install.package("devtools") # In case you have not installed it.
#  devtools::install_github("wenlong-liu/usfertilizer")

## ---- eval= FALSE--------------------------------------------------------
#  install.packages("usfertilizer")
#  # not available yet.

## ---- message=FALSE, warning=FALSE---------------------------------------
require(usfertilizer)
require(tidyverse)
data("us_fertilizer_county")

## ------------------------------------------------------------------------
glimpse(us_fertilizer_county)

## ------------------------------------------------------------------------
# plot the top 10 nitrogen application in year 1990.
# Reorder to make the plot more cleanner.
year_plot = 2008
us_fertilizer_county %>%
  filter(Fertilizer == "N" & Year == year_plot) %>%
  top_n(10, Quantity) %>%
  ggplot(aes(x=reorder(paste(County,State, sep = ","), Quantity), Quantity, fill = Quantity))+
  scale_fill_gradient(low = "blue", high = "darkblue")+
  geom_col()+
  ggtitle(paste("Top 10 counties with most fertilizer application in the year of", year_plot)) + 
  coord_flip()+
  theme_bw()

## ------------------------------------------------------------------------
# plot the top 10 states with P application in year 1980.
# Reorder to make the plot more cleanner.
year_plot = 1980
us_fertilizer_county %>%
  filter(Fertilizer == "P" & Year == 1980) %>% 
  group_by(State) %>% 
  summarise(p_application = sum(Quantity)) %>% 
  as.data.frame() %>% 
  top_n(10, p_application) %>%
  ggplot(aes(x=reorder(State, p_application), p_application))+
  scale_fill_gradient(low = "blue", high = "darkblue")+
  geom_col()+
  ggtitle(paste("Top 10 States with most Phosphrus application in the year of", year_plot)) + 
  scale_y_continuous(name = "Phosphrus from commecial fertilization (kg)")+
  scale_x_discrete(name = "States")+
  theme_bw()+
  coord_flip()
  

## ---- message=F, warning=F-----------------------------------------------
year_plot = seq(1945, 2010, 1)
states = c("NC","SC")

us_fertilizer_county %>% 
  filter(State %in% states & Year %in% year_plot &
           Farm.Type == "farm") %>% 
  group_by(State, Year, Fertilizer) %>% 
  summarise(Quantity = sum(Quantity)) %>% 
  ggplot(aes(x = as.numeric(Year), y = Quantity, color=State)) +
  geom_point() +
  geom_line()+
  scale_x_continuous(name = "Year")+
  scale_y_continuous(name = "Nutrient input quantity (kg)")+
  facet_wrap(~Fertilizer, scales = "free", ncol = 2)+
  ggtitle("Estimated nutrient inputs into arable lands by commercial fertilizer\nfrom 1945 to 2010 in Carolinas")+
  theme_bw()


## ------------------------------------------------------------------------
us_fertilizer_county %>% 
  select(Year) %>% 
   group_by(Year) %>% 
  filter(row_number() ==1) %>% 
  arrange(Year)

