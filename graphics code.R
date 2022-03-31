# graphics code


#***************************************
# options within a graph
# to find color names
colors()

# to find the number for the symbols
# note, 95 is a good one for lines
plot(0:11,0:11,type="n",xlab="",ylab="") # sets up plot area
k=-1 # change to -1, 29, 59, 89, 119, 149, 179, 209, or 239 for more
for(i in c(2,5,8)) {
  for(j in 10:1) {
    k=k+1
    points(i,j,pch=k,cex=2)
    text(i-1,j,labels=as.character(k))
    }
  }

# to change the dimensions of a graph
# before the plot function:
par(mar=c(4, 4, 2, 0.5))
# numbers are for the number of lines at the (bottom, left, top, right) of the plot area
# default is (5.1, 4.1, 4.1, 2.1)
# to check what the settings are currently set to
par()$mar

# to change the orientation of the x axis labels, use las= in the plot statement: 0=always parallel to axis, 1=always horizontal, 2=always perpendicular to the axis, 3=always vertical




#***************************************
# exporting graphics
pdf(file="file/path/file name.pdf")
dev.off()

jpeg(filename="file/path/file name.jpg", width=2000, height=1600, res=300)
dev.off()
# you can leave off the width, height, and res - these ones work well for presentations





#***************************************
# axes
# dates
plot(dframe$date, dframe$var, xaxt="n")
axis.Date(1, at=seq(as.Date("2010/5/1"), as.Date("2011/10/31"), "months"), format="%m/%y")
# note: seq(as.Date(first date in sequence), as.Date(last date in sequence), by, the way you want the date formatted)

# axis label orientation
las=2
# in plot statement: (0=always parallel to axis, 1=always horizontal, 2=always perpendicular to axis, 3=always vertical)

# to make multiple line x axis labels
# suppress the axis in the plot statement with xaxt="n"
# then add an axis back in with the first line of labels
axis(1, at=c(1,2,3,4,5), labels=c("lable1","label2","label3"))
# then add in the next line of the labels and play with the number of spaces between each word to make them line up correctly (the line will be centered on the graph)
mtext("lab1       lab2         lab3", side=1, line=2)





#***************************************
# inserting text onto a graph
text(x=4, y=3500, "y=3.976x+1.185, R2=0.6125", col="blue", pos=4)
# the x and y are coordinates on the actual graph, so tailor to the specific graph
# pos is the position of the text from the x,y - 1=below, 2=left, 3=above, 4=right
# If you specify pos, you can specify offset= in percent of character width - good for labeling points



#***************************************
# inserting the equation, R2 and p value of a regression linear model into the graph
LM1 <- lm(dframe$yvar~dframe$xvar)
LM1sum <- summary(LM1)
coef <- LM1sum[["coefficients"]]
m=coef[2]
b=coef[1]
p = coef[8]
r2 <- LM1sum[["r.squared"]]
text(x=50, y=30, paste("y=", signif(m, digits=4), "x + ", signif(b, digits=4), sep=""), col="blue", pos=4)
text(x=50, y=20, paste("R2=", signif(r2, digits=4), sep=""), col="blue", pos=4)
text(x=50, y=10, paste("p value=", signif(p, digits=4), sep=""), col="blue", pos=4)





#***************************************
# index plot - plots values of a variable in the order they appear in a dataset - great for a first look for outliers, etc
plot(dframe$variable)
# to then find the outliers
ix=which(dframe$variable>number)
dframe$variable[ix] # this will print the values that are greater than n
dframe[ix] # this will print the rows where the values are greater than n




#***************************************
# normal scatterplot
plot(dframe$xvar, dframe$yvar1, pch=16, col="blue", main="Title of Graph", xlab="x axis label", ylab="y axis label")




#***************************************
# scatterplot by group
library(car)
scatterplot(yvar ~ xvar | group_var, data=dframe, xlab="x axis label", ylab="y axis label", main="Title of Graph", smooth=F, ylim=c(0, 15), legend.coords="topleft", grid=F)
# can use legend.coords="NULL" and then a legend statement to define your own legend instead of the automatically designated format
# colors follow the sequence in palette().  to get these colors in a legend or another series, create a vector called colors containing palette(), then designate the colors in the legend or plot statement as col=colors
colors=palette()
legend(x="topright", y="NULL", c("1st y var","2nd y var"), pch=c(1:2), col=colors)
# ncol=2 would have the legend be in 2 columns




#***************************************
# scatterplot by group in a lattice
library(lattice)
xyplot(yvar ~ xvar | groupvar, data=dframe,main="title", ylab="y axis", xlab="x axix")
# can have muliple grouping variables by using yvar ~ xvar | grpvar1 * gvar2




#***************************************
# normal line graph

# order variables to plot points connected by a line - not real sure this is needed
ix=order(dframe$xvar)

plot(dframe$xvar, dframe$yvar1, pch=16, col="blue", main="Title of Graph", xlab="x axis label", ylab="y axis label")
lines(dframe$xvar[ix], dframe$yvar1[ix], col="blue")
points(dframe$xvar, dframe$yvar2, pch=2, col="red")
lines(dframe$xvar[ix],dframe$yvar2[ix], col="red")
legend(x="topright", y="NULL", c("1st y var","2nd y var"), pch=c(16,2), col=c("blue","red"))




#***************************************
# point plot with a regression line

plot(dframe$xvar, dframe$yvar, main="Title of Graph", xlab="x axis label", ylab="y axis label", xlim=c(0, 6), ylim=c(0, 6))
# add the regression line
abline(lm(dframe$yvar~dframe$xvar))
# see regression equation pieces (y=(num under variable)x + (num under intercept))
LM1 <- lm(dframe$yvar~dframe$xvar)
# see linear model stats (R2, p-value)
summary(LM1)



#**************************************
#add regression info to graph (equation and r2) to ggplot

equation.label <- function(dframe){
  m <- lm(y ~ x, dframe);
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                   list(a = format(coef(m)[1], digits = 3), 
                        b = format(coef(m)[2], digits = 3), 
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));                 
}

plot + geom_text(x = -20, y = 25, label = equation.label(dframe), parse = TRUE, hjust=0)




#***************************************
# to add 1:1 line or reference line to a graph

# create y=0 line
y1linex=c(0, 20)
y1liney=c(0, 0)
# after plot code:
lines(y1linex, y1liney, col="gray")

# another option to create a 1:1 line
# after plot code:
lines(c(-1,6), c(-1,6), col="gray", lty="dashed")




#***************************************
# box plot by a grouping variable

boxplot(var1 ~ group_var, data=dframe, main="Title of Graph", ylab="y axis label", ylim=c(0,3))
# x axis is labeled with the values in group_var
# if you want a box plot of all values, use a grouping variable of something that's common to all rows and use xlab="x axis label" to label the x axis




#***************************************
# to order groups on x axis of box or bar plot
# before the plot statement, make the grouping variable a factor in the order you want them to show up
dframe$group_var=factor(dframe$group_var, c("group1", "group2", "group3", "group4"))




#***************************************
# bar chart

# data must be arranged in a matrix with a particular structure
# deposition_R_demo.R has an example

barplot(matrix1, main="Title of Graph", ylab="y axis label", col=c("blue","red"), legend = rownames(matrix1), beside=T, axis.lty=1, ylim=c(0, 5))
# beside=F would create a stacked bar chart

# can also skip the matrix and just use vectors if you only need one series of bars
names <- dframe$sitename
yvar <- ea_mn$yvar
barplot(yvar, main="graph title", ylab="y label", names.arg=names, horiz=F)
# to get names to display perpendicular to axis, specify par(las=2) before the boxplot command


#***************************************
# multiple ggplots (like par(mfrows) but for ggplot)

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


#*********************************
#add final theme for formatting ggplots
final_theme=theme_bw() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        plot.title=element_text(size=16, face='bold', hjust=0.5)) #save as a grom

p + final_theme # where p is your plot grom

#*********************************
# add text to ggplot (with italics)
p + annotate("text", x = 4, y = 25,
             label = "paste(italic(R) ^ 2, \" = .75\")", parse = TRUE)

#*********************************
# add line segment
p + annotate("segment", x = 4, xend=6, y = 25, yend=25, linetype=2, col='grey')

#*********************************
# The palette with black:
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
#black, yellow/orange, aqua blue, forest green, yellow, drk blue, red/orange, pink/purple

scale_color_colorblind() #if 8 values or less    

scale_color_manual(values=c("#000000", "#999999", "#997300", "#ffbf00", "#173fb5", "#587CE9", "#a5b8f3", "#004d13",
                            "#00e639", "#66ff8c", "#00664b", "#009E73", "#00e6a8", "#8d840c", "#d4c711", "#f5ee89", "#005180", "#0081cc", "#66c7ff")) #so you can adjust

#*********************************
# add special characters to facet labels:
summary$var1 <- factor(summary$var1, c('13C %C', 'pig', 'gloeo'))
summary$var1_lab <- factor(summary$var1, labels=c(expression(paste(delta^{13},'C & %C')), 'Pigments', expression(paste(italic('Gloeo.')))))

#plot
ggplot(summary, aes(x=var2, y=corr_value)) +
  facet_grid(var1_lab ~., labeller=label_parsed)

#*********************************
# add special characters to axis titles:

ggplot(summary, aes(x=var2, y=corr_value)) +
  scale_x_discrete(labels=parse(text=c(expression(paste(delta^{13},'C & %C')), 'Pigments', expression(paste(italic('Gloeo.'))), 'Land', 'Population', 'Climate')))

#*********************************
#add n details to boxplots
# function for number of observations 
give.n <- function(x){
  return(c(y = -2, label = length(x))) 
  # experiment with the multiplier to find the perfect position
}

# function for mean labels
mean.n <- function(x){
  return(c(y = median(x)*0.97, label = round(mean(x),2))) 
  # experiment with the multiplier to find the perfect position
}

ggplot(fog_flux_comp_9mo, aes(x=landuse_auto, y=fog_pct_of_tot, fill=above_below)) +
  geom_boxplot() +
  facet_grid(above_below~.) +
    stat_summary(fun.data = give.n, geom = "text", fun.y = median) #fun.data is one of the functions above and fun.y (or fun.x) is the placement (median or mean, depending)


#*********************************
# make space between facets and add corner labels for manuscripts, also, export at desired resolution/size

tiff('zone break summary box plot for pub.tiff', width=3600, height=2400, res=600)

plot <- ggplot(summary, aes(x=var2, y=corr_value))+
  theme(panel.spacing = unit(2, "lines")) 

plot

grid.text("a", x = unit(0.09, "npc"), y = unit(0.98, "npc"), gp=gpar(fontsize=9))
grid.text("b", x = unit(0.09, "npc"), y = unit(0.67, "npc"), gp=gpar(fontsize=9))
grid.text("c", x = unit(0.09, "npc"), y = unit(0.36, "npc"), gp=gpar(fontsize=9))

dev.off()

#-------
#force axis label onto two lines in ggplot for factors
plot +
  scale_x_discrete(labels=function(x) sub(" "," \n",x,fixed=TRUE))
#where the character in the "" is what splits the line. in this case it is a single space