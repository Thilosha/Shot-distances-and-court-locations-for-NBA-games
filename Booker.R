library(nbastatR)
phoBos <- 21601076
Sys.setenv(VROOM_CONNECTION_SIZE=500072)

phx <- teams_shots(teams = "Phoenix Suns",
                   seasons = 2017)
library(dplyr)
library(stringr)
DBshots <- 
  phx %>% filter(namePlayer=="Devin Booker") %>% 
  filter(idGame==phoBos) 
library(ggplot2)
library(gganimate)
install.packages("installr")
library(installr)
updateR()
library(gganimate)
library(lubridate)
library(ggalt)
library(artyfarty)
DBshots <- DBshots %>% mutate(gtime=ms(as.character(paste(minutesRemaining,secondsRemaining,sep = ":")))) %>% 
  mutate(time_chron=case_when(
    numberPeriod==1~ms("12:00")-gtime,
    numberPeriod==2~ms("24:00")-gtime,
    numberPeriod==3~ms("36:00")-gtime,
    numberPeriod==4~ms("48:00")-gtime,
    numberPeriod==5~ms("52:00")-gtime))
DBshots <- DBshots %>% 
  mutate(distTrans=if_else(distanceShot==0,0.8,distanceShot))
scoringD <- 
  ggplot(DBshots)+
  geom_hline(yintercept = 23.9,linetype=2,color="gray")+
  annotate("text",label="from downtown!",x=700,26.5,size=5,alpha=0.5,color="grey70")+
  geom_vline(xintercept = as.numeric(ms("48:00")),linetype=3,color="red")+
  geom_lollipop(aes(x=time_chron,y=distTrans,
                    color=isShotMade))+
  labs(y="shot distance (feet) \n *excludes dunks and free throws",
       x="time (minutes)", title = "Devin Booker vs. Boston - March 24 2017")+
  scale_x_time(breaks = ms(c("12:00","24:00","36:00","48:00")))+
  scale_color_manual(values = c("#00529b","#cc4b4b"),labels=c("made","missed"))+
  theme_monokai_full()+
  theme(text = element_text(size = 19),
        panel.grid.major.x = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size=19))+
  guides(color = guide_legend(override.aes = list(size = 4)))+
  transition_states(idEvent)+shadow_mark()
scoringAnim <- animate(scoringD,height=800,width=800)
scoringAnim
source("https://raw.githubusercontent.com/toddwschneider/ballr/master/plot_court.R")
source("https://raw.githubusercontent.com/toddwschneider/ballr/master/court_themes.R")
plot_court()
court_points <- court_points %>% mutate_if(is.numeric,~.*10)
DBcourt <- 
  ggplot(DBshots, aes(x=locationX, y=locationY+45)) + 
  scale_fill_manual(values = c("#00529b","#cc4b4b"),guide=FALSE)+
  geom_path(data = court_points,
            aes(x = x, y = y, group = desc),
            color = "black")+
  coord_equal()+
  geom_point(aes(fill=isShotMade),pch=21,size=4,color="white") +
  xlim(-260, 260) +
  theme_monokai_full()+
  labs(title="Shot location",x="",
       y="",
       caption = "by @LuisDVerde\nNBA data accessed with nbastatr")+
  theme(text = element_text(size = 19),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        plot.caption = element_text(color="white"))+
  transition_states(idEvent)+shadow_mark()
DBcourtanim <- animate(DBcourt,height=800,width=800,bg="#272822")


# Combine plots side by side, by Patrick Toche
combine_gifs <- function(plot1, plot2) {
  # read the plots and store them
  plot1 <- image_read(plot1) 
  plot2 <- image_read(plot2) 
  # sync the number of frames in each plot
  n1 = length(plot1)
  n2 = length(plot2)
  # match number of frames of the two plots
  if (!(n1 == n2)) plot1 <- rep(plot1, n2) 
  if (!(n1 == n2)) plot2 <- rep(plot2, n1)
  # initialize the combined plot
  p <- image_append(c(plot1[1], plot2[1]))
  # grow the combined plot frame by frame
  n = ifelse(n1 == n2, n1, n1 * n2) 
  n = min(1000, n)  # set max to 1000
  for (i in 2:(n-1)) {
    tmp <- image_append(c(plot1[i], plot2[i]))
    p <- c(p, tmp)
  }
  return(p) 
}
library(magick)
DBvsBos <- combine_gifs(scoringAnim,DBcourtanim)
DBvsBos %>% image_write("booker.gif")

