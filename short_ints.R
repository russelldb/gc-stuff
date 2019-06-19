## R script will run on selection.
##
## This chart is to look at the performance of a session of short hard
## intervals, like 30/30, or 40/20. With short, hard intervals like
## this, they're usally run for 5-30 minutes, in a block. No-one has
## the wherewithall to hit interval for each one, so usually you end
## up with a user defined interval with lots of on/off bursts in it.
##
## The questions I want to answer are:
## 1. what is the average "on" power for the whole session?
## 2. did I execute the intervals at around the prescribed power?
## 3. did I pace the session well?
## 4. how does one session compare to another? (TODO!)
##
## The chart attempts to answer these questions by grabbing all the
## efforts(matches) from GC for the selected activity(s) and plotting
## them. It also plots a line for the average, and an lm trend line
## for the whole session.
##
## install the packages this chart depends on
packages.to.install <- c("ggplot2", "ggrepel", "lambda.tools")

for(p in packages.to.install)
{
    print(p)
    if (suppressWarnings(!require(p, character.only = TRUE))) {
        install.packages(p, repos = "http://lib.stat.cmu.edu/R/CRAN")
        library(p, character.only=TRUE)
    }
}

library(ggplot2)
library(ggrepel)
library(lambda.tools)

round_to_five <- function(N) {
    5 * round(N/5)
}

my_y_lab <- function(Lims) {
    round_to_five(trunc(seq(Lims[1], Lims[2], 5)))
}

get_intervals <- function(activity=0) {
    GC.activity.intervals(type=c("EFFORTS"), activity=activity)
}

## each activity should be a list(pair) of 'activity' and 'color'. Use
## 'color' to match GCs compare color
plot_intervals <- function(p, i) {
    ## TODO extract to constants for easier customisation by users
    ## TODO this should be ZONE based too!

    ## get the short efforts (28 to 33 secs)
    II <- i[i$Duration > 28 & i$Duration < 33, ]

    if(nrow(II) > 1) {
	II$Effort_Num <- 1:nrow(II)
	II$Lab <- trunc(II$Average_Power)
	df_av <- data.frame(y_values=mean(II$Average_Power))
	p <- p + geom_point(data=II, mapping=aes(x=Effort_Num, y=Average_Power, color=series_color))
        p <- p + geom_smooth(data=II, mapping=aes(x=Effort_Num, y=Average_Power, color=series_color))
        p <- p + geom_hline(data=df_av, mapping=aes(yintercept=y_values, color=II$series_color[1]), linetype="dashed")
        ## TODO figure out these scales
        ## p <- p + scale_x_continuous(breaks = 1:nrow(II))
        ## p <- p + scale_y_continuous( breaks = my_y_lab)
        p <- p + geom_label_repel(data=II, mapping=aes(x=Effort_Num, y=Average_Power, label = Lab), box.padding = 0.5,
                                  point.padding = 0.5,
                                  label.padding = 0.5,
                                  label.size = 0.75,
                                  segment.color = 'grey50')
        p
    }
}


add_intervals_to_plot <- function(Activity, Plot) {
    ## get the POSIXct for the activity
    DT <- Activity$activity$time[1]
    I <- get_intervals(activity=DT)
    Color <- Activity$color
    I$series_color <- Color
    plot_intervals(Plot, I)
}

## the `main` bit of the script

## docs say, if COMPARE panel is ON and there are activities in
## COMPARE panel, then AL will be a list of activities. If COMPARE is
## OFF then AL will have the current single activity. Weirdly though,
## the list WILL be empty
AL <- GC.activity(compare=TRUE)

## make a blank plot to add dataframes to, and iterate the activities
p <- ggplot()

for(LI in AL) {
    p <- add_intervals_to_plot(LI, p)
}

print(p)
