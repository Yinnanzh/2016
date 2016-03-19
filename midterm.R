
# 1
library(readr)

t1 <- read_csv("https://raw.githubusercontent.com/datasciencelabs/data/master/data.csv")

#2
library(dplyr)
sum(t1$am==0)

#3

temp <- t1[which(t1$am==1),"mpg"]
mean(temp$mpg)

#4

t2 <- read_csv("https://raw.githubusercontent.com/datasciencelabs/data/master/midterm_data.csv")

library(ggplot2)

t3 <- t2 %>% filter(date>'2015-08-22')
ggplot(t3, aes(x,y)) + geom_point()

#5
x <- c(5,5,8,9,10,10,11,11,11,12,12,13,14,14,14,14,15,15,16,16,17,17,19,20,21)

M <- replicate (10^4,median(sample(x,5,replace=T)))
sd(M)

#6
master <- read_csv("https://raw.githubusercontent.com/datasciencelabs/data/master/Master.csv")
player_info <- master %>% select(playerID, nameFirst, nameLast, birthYear, height)

player_info %>% filter(birthYear>=1980) %>% summarize(mean(height))

#7
player.1980 <- player_info %>% filter(birthYear>=1980)
height.sample <- sample(player.1980$height,100)
mean(height.sample)+c(-1,1)*qnorm(0.975)*sd(height.sample)/10

#8
salaries <- read_csv("https://raw.githubusercontent.com/datasciencelabs/data/master/Salaries.csv")
length(unique(salaries$playerID))

#9

#10
ny_airquality <- 
        read_csv("https://raw.githubusercontent.com/datasciencelabs/data/master/ny_airquality.csv")

library(tidyr)
#use separate to split date into year month and day
air <- ny_airquality %>% separate(Date, c("year", "month", "day"), sep = "-",
                        convert = TRUE, fill = "right") 

#use summarize, average temperature for each month
air.month <- air %>% group_by(month) %>% summarize(Temp.avg=mean(Temp))

#11
master <- read_csv("https://raw.githubusercontent.com/datasciencelabs/data/master/Master.csv")
player_info <- master %>% select(playerID, nameFirst, nameLast, birthYear, height)
salaries <- read_csv("https://raw.githubusercontent.com/datasciencelabs/data/master/Salaries.csv")


# group_by, summarize, create a table with 1 row for each player that shows their avg salary
tab <- salaries %>% group_by(playerID) %>% summarize(salary.avg=mean(salary))

# Use one of the dplyr join functions to add average salaries to the player_info table
player.salary <- left_join(player_info, tab,by="playerID")

player.salary %>% filter(birthYear==1971) %>%arrange(desc(salary.avg)) 



