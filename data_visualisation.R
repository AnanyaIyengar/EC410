
#PREPARING DATA FOR ANALYSIS

library(readxl)
library(ggplot2)
library(dplyr)
library(plotly)
library(gridExtra)
library(viridis)
library(hrbrthemes)

net_state_value_added <- read_excel("net_state_va_constant_prices.XLSX")

states <- c("Andhra Pradesh", "Bihar", "Gujarat", "Haryana", "Karnataka", "Maharashtra", "Odisha", "Punjab", "Rajasthan", "Tamil Nadu", "Uttar Pradesh", "West Bengal")
selected_net_value_added <- net_state_value_added%>%dplyr::select(one_of(states))
selected_net_value_added <- selected_net_value_added[-c(4, 11, 17, 25, 35), ]
colnames(selected_net_value_added) = c("andhra_pradesh", "bihar", "gujarat", "haryana", "karnataka", "maharashtra", "odisha", "punjab", "rajasthan", "tamil_nadu", "uttar_pradesh", "west_bengal" )
selected_net_value_added <- as.data.frame(selected_net_value_added)
typeof(selected_net_value_added)



year = seq(1991, 2020, 1)
year <- as.Date(ISOdate(year, 3, 31))
year <- as.Date(ISOdate(year, 4, 1))
print(year)

###########################################################
###GROWTH OF STATE LEVEL GVA AT CURRENT PRICES OVER TIME###
###########################################################

#CREATING INTERACTIVE GRAPHS USING ggplot2 

selected_net_value_added<-selected_net_value_added%>%dplyr::mutate(year = as.Date(ISOdate(year, 3, 31)))

plot1 <- ggplot(selected_net_value_added, aes(year, andhra_pradesh)) + geom_line(color  = "aquamarine4", size = 1.5) + geom_area(fill = "aquamarine1") + scale_x_date() + scale_y_continuous() + theme_minimal() + ggtitle("Andhra Pradesh") + ylab("Net VA") 
plot1

plot2 <- ggplot(selected_net_value_added, aes(year, bihar)) + geom_line(color  = "coral4", size = 1.5) + geom_area(fill = "coral1") + scale_x_date() + scale_y_continuous() + theme_minimal() + ggtitle("Bihar") + ylab("Net VA")
plot2

selected_net_value_added$gujarat <- as.numeric(selected_net_value_added$gujarat)
plot3 <- ggplot(selected_net_value_added, aes(year, gujarat)) + geom_line(color = "aquamarine4", size = 1.5) + geom_area(fill = "aquamarine1") + scale_x_date() + scale_y_continuous() + theme_minimal() + ggtitle("Gujarat") + ylab("Net VA")
plot3

plot4 <- ggplot(selected_net_value_added, aes(year, haryana)) + geom_line(color  = "aquamarine4", size = 1.5) + geom_area(fill = "aquamarine1") + scale_x_date() + scale_y_continuous() + theme_minimal() + ggtitle("Haryana") + ylab("Net VA")
plot4


plot5 <- ggplot(selected_net_value_added, aes(year, karnataka)) + geom_line(color  = "aquamarine4", size = 1.5) + geom_area(fill = "aquamarine1") + scale_x_date() + scale_y_continuous() + theme_minimal() + ggtitle("Karnataka") + ylab("Net VA")
plot5

selected_net_value_added$maharashtra <- as.numeric(selected_net_value_added$maharashtra)
plot6 <- ggplot(selected_net_value_added, aes(year, maharashtra)) + geom_line(color  = "aquamarine4", size = 1.5) + geom_area(fill = "aquamarine1") + scale_x_date() + scale_y_continuous() + theme_minimal() + ggtitle("Maharashtra")+ ylab("Net VA")
plot6

plot7 <- ggplot(selected_net_value_added, aes(year, odisha)) + geom_line(color  = "aquamarine4", size = 1.5) + geom_area(fill = "aquamarine1") + scale_x_date() + scale_y_continuous() +  theme_minimal() + ggtitle("Odisha") + ylab("Net VA")
plot7

plot8 <- ggplot(selected_net_value_added, aes(year, punjab)) + geom_line(color  = "aquamarine4", size = 1.5) + geom_area(fill = "aquamarine1") + scale_x_date() + scale_y_continuous() + theme_minimal() + ggtitle("Punjab") + ylab("Net VA")
plot8

plot9 <- ggplot(selected_net_value_added, aes(year, rajasthan)) + geom_line(color  = "coral4", size = 1.5) + geom_area(fill = "coral1") + scale_x_date() + scale_y_continuous() + theme_minimal() + ggtitle("Rajasthan") + ylab("Net VA")
plot9

plot10 <- ggplot(selected_net_value_added, aes(year, tamil_nadu)) + geom_line(color  = "aquamarine4", size = 1.5) + geom_area(fill = "aquamarine1") + scale_x_date() + scale_y_continuous() + theme_minimal() + ggtitle("Tamil Nadu") + ylab("Net VA")
plot10

plot11 <- ggplot(selected_net_value_added, aes(year, uttar_pradesh)) + geom_line(color  = "coral4", size = 1.5) + geom_area(fill = "coral1") + scale_x_date() + scale_y_continuous() + theme_minimal() + ggtitle("Uttar Pradesh")+ ylab("Net VA")
plot11

plot12 <- ggplot(selected_net_value_added, aes(year, west_bengal)) + geom_line(color  = "aquamarine4", size = 1.5) + geom_area(fill = "aquamarine1") + scale_x_date() + scale_y_continuous() + theme_minimal() + ggtitle("West Bengal")+ ylab("Net VA")
plot12

bimaru_states <- grid.arrange(plot2, plot9, plot11)
non_bimaru_states <- grid.arrange(plot1, plot3, plot5, plot6, plot7, plot8, plot10, plot12, nrow = 4)



#MAKING THE GRAPHS INTERACTIVE USING ggplotly : CAN BE VIEWED ON HTML PAGE

interactive1 <- ggplotly(plot1)
interactive1 <- ggplotly(plot2)
interactive3 <- ggplotly(plot3)
interactive4 <- ggplotly(plot4)
interactive5 <- ggplotly(plot5)
interactive6 <- ggplotly(plot6)
interactive7 <- ggplotly(plot7)
interactive8 <- ggplotly(plot8)
interactive9 <- ggplotly(plot9)
interactive10 <- ggplotly(plot10)
interactive11 <- ggplotly(plot11)
interactive12 <- ggplotly(plot12)

##########################################################
###CONTRIBUTION OF DIFFERENT SECTORS TO STATE LEVEL GVA###
##########################################################

#CREATING DATA FRAMES FOR 2017-2021

sector_wise_contribution <- read_excel("sector_wise_contribution.XLSX")

#COMPARING RELATIVE CONTRIBUTIONS OF AGRICULTURE AND INDUSTRY TO STATE NET VALUE ADDED FOR THE PERIOD 2017-2020

plot13 <- ggplot(sector_wise_contribution) + theme_gray() + facet_wrap(.~ as.factor(state), ncol = 3, scales = "free_y") + geom_line(aes(x = year, y = agri), size = 1, colour = "lightblue3") + geom_point(aes(x = year, y = agri), size = 1.5, colour = "blue3") + geom_line(aes(x = year, y = manufacturing), size = 1, colour = "coral1") + geom_point(aes(x = year, y = manufacturing), size = 1.5, colour = "coral4") + xlab("Year") + ylab("Contribution to NVA") + ggtitle("Contribution of Agriculture and Manufacturing to States' NVA") + labs(caption = "Data: RBI", subtitle = "(Red line graphs correspond to Manufacturing)")
plot13

#CONSTRUCTION ABSORBS LABOUR: WHAT HAPPENS TO CONSTRUCTION SHARES OVER TIME? COMPARING WITH MANUFACTURING AND AGRICULTURE

plot14 <- ggplot(sector_wise_contribution) + theme_gray() + geom_point(aes(as.factor(state), construction, colour = year), size = 2.5) + xlab("States") + ylab("Construction") + ggtitle("Construction's Contribution to States' NVA") + labs(caption = "Data: RBI") 
plot14

plot15 <- ggplot(sector_wise_contribution) + theme_gray() + geom_point(aes(construction, manufacturing, colour = as.factor(state)), size = 2) + geom_smooth(aes(construction, manufacturing), method = lm, se = FALSE, colour = "black") + xlab("Construction NVA") + ylab("Manufacturing NVA") + ggtitle("Comparing Construction and Manufacturing") + labs(caption = "Data: RBI")
plot15

plot16 <- ggplot(sector_wise_contribution) + theme_gray() + geom_point(aes(construction, agri, colour = as.factor(state)), size = 2) + geom_smooth(aes(construction, agri), method = lm, se = FALSE, colour = "black") + xlab("Construction NVA") + ylab("Agriculture NVA") + ggtitle("Comparing Construction and Agriculture") + labs(caption = "Data: RBI") 
plot16

#COMPARING STATE CONTRIBUTIONS FOR DIFFERENT SECTORS

plot17 <- ggplot(sector_wise_contribution) + theme_gray() + geom_area(aes(x = year, y = agri, fill = state ), colour = "white", size = 0.5, alpha = 0.6) + xlab("Year") + ylab("Agriculture NVA") + ggtitle("Statewise Contribution to Agriculture NVA") + labs(caption = "Data: RBI") + scale_fill_viridis(discrete = T)
plot17

plot18 <- ggplot(sector_wise_contribution) + theme_gray() + geom_area(aes(x = year, y = manufacturing, fill = state ), colour = "white", size = 0.5, alpha = 0.6) + xlab("Year") + ylab("Manufacturing NVA") + ggtitle("Statewise Contribution to Manufacturing NVA") + labs(caption = "Data: RBI") + scale_fill_viridis(discrete = T)
plot18

plot19 <- ggplot(sector_wise_contribution) + theme_gray() + geom_area(aes(x = year, y = construction, fill = state ), colour = "white", size = 0.5, alpha = 0.6) + xlab("Year") + ylab("Construction NVA") + ggtitle("Statewise Contribution to Construction NVA") + labs(caption = "Data: RBI") + scale_fill_viridis(discrete = T)
plot19

plot20 <- ggplot(sector_wise_contribution) + theme_gray() + geom_area(aes(x = year, y = trade_hotels, fill = state ), colour = "white", size = 0.5, alpha = 0.6) + xlab("Year") + ylab("Trade and Hotels NVA") + ggtitle("Statewise Contribution to Trade/Hotels NVA") + labs(caption = "Data: RBI") + scale_fill_viridis(discrete = T)
plot20

plot21 <- ggplot(sector_wise_contribution) + theme_gray() + geom_area(aes(x = year, y = finance, fill = state ), colour = "white", size = 0.5, alpha = 0.6) + xlab("Year") + ylab("Financial Services NVA") + ggtitle("Statewise Contribution to FinServ NVA") + labs(caption = "Data: RBI") + scale_fill_viridis(discrete = T)
plot21

plot22 <- ggplot(sector_wise_contribution) + theme_gray() + geom_area(aes(x = year, y = public_admin, fill = state ), colour = "white", size = 0.5, alpha = 0.6) + xlab("Year") + ylab("Public Administration NVA") + ggtitle("Statewise Contribution to Public Admin NVA") + labs(caption = "Data: RBI") + scale_fill_viridis(discrete = T)
plot22





