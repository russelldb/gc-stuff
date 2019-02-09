## R script will run on selection.
##
## GC.activity(compare=FALSE)
## GC.metrics(all=FALSE,
##            compare=FALSE)
##
## Get the current ride or metrics
##
library(ggplot2)
library(ggrepel)
round_to_five <- function(N) {
	5 * round(N/5)
}
my_y_lab <- function(Lims) {
 round_to_five(trunc(seq(Lims[1], Lims[2], 5)))
}

get_intervals <- function(activity=0) {
    GC.activity.intervals(type=c("EFFORTS"), activity=activity)
}

plot_intervals <- function(i) {
## TODO this should be ZONE based too!
   II <- i[i$Duration > 28 & i$Duration < 33, ]

if(nrow(II) > 1) {
	II$Effort_Num <- 1:nrow(II)
	II$Lab <- trunc(II$Average_Power)
	df_av <- data.frame(y_values=mean(II$Average_Power))
	p <- ggplot(II, aes(x=Effort_Num, y=Average_Power)) + geom_point(color="pink", size=3) + geom_smooth()
p <- p + geom_hline(data=df_av, mapping=aes(yintercept=y_values), linetype="dashed", colour="blue", size=1)
p <- p + scale_x_continuous(breaks = 1:nrow(II))
p <- p + scale_y_continuous( breaks = my_y_lab)
p <- p + geom_label_repel(aes(label = Lab), box.padding = 0.5,
			point.padding = 0.5,
			label.padding = 0.5,
			label.size = 0.75,
			segment.color = 'grey50')
p
}
}

AL <- GC.activity(compare=TRUE)
I <- get_intervals()
p <- plot_intervals(I)
print(p)

