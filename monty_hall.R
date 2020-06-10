#THE MONTY HALL PROBLEM MONTE CARLO STYLE

monty_monte <- function (runs) {
#Allocate car behind one of three doors (car) and random selection of door by contestant (selected)
doors <- data.frame("car" = sample(1:3, runs, replace=T),
                    "selected" = sample(1:3, runs, replace=T))

door_vec <- c(1,2,3)
`%notin%` <- negate(`%in%`)

open_func <- function(x,y) {
  i <- door_vec %notin% c(x,y)
  ifelse(length(door_vec[i])==1, door_vec[i], sample(door_vec[i],1))
}

switch_func <- function(x,y) {
  i <- door_vec %notin% c(x,y)
  ifelse(length(door_vec[i])==1,door_vec[i], sample(door_vec[i],1))
}

#Open a non-selected door without a car behind it
doors$opened <- mapply(open_func, x=doors$car, y=doors$selected)

#Player switches to non opened door
doors$switched <- mapply(switch_func, x=doors$opened, y=doors$selected)

#Win car?
doors$win <- doors$switched==doors$car

mean(doors$win)     

}

#Monte Carlo simulation
runs <- seq(10,5000, by=1)
monty_hall <- sapply(runs, monty_monte)
monty_df <- data.frame(runs, monty_hall)
ggplot(monty_df, aes(runs, monty_hall)) + 
  geom_point(alpha=0.5, col="darkorange") +
  geom_smooth(col="black") +
  theme_few() +
  ylim(c(0,1)) +
  labs(x="simulation runs", y="probability of winning car by switching",
       subtitle="Three doors. A car behind one. Contestant chooses a door. Host opens one of the remaining doors - no car. Switch or stay?", title="The Monty Hall problem - Monte Carlo style",
       caption="@lauriejhopkins") +
  theme(legend.position="none", text = element_text(size=15, family="Gill Sans"), plot.title=element_text(size=25),
        panel.grid.major.y = element_line(size=0.1, color="grey70")) 


