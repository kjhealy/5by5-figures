library(ggplot2)
library(Hmisc)
library(MASS)

### --------------------------------------------------
### Cosmetics
### --------------------------------------------------
makeFootnote <- function(footnoteText=
                         format(Sys.time(), "%d %b %Y"),
                         size= .7, color= grey(.5))
{
   require(grid)
   pushViewport(viewport())
   grid.text(label= footnoteText ,
             x = unit(1,"npc") - unit(2, "mm"),
             y= unit(2, "mm"),
             just=c("right", "bottom"),
             gp=gpar(cex= size, col=color))
   popViewport()
}

credit <- function() {
  return(makeFootnote("\nKieran Healy. http://kieranhealy.org"))
}

### --------------------------------------------------
### Hypercritical
### --------------------------------------------------
data <- read.csv("data/Hypercritical.csv", header=TRUE)
library(gdata)
 data$Goal <- reorder.factor(data[, "Goal"], new.order=rev(levels(data$Goal)))
detach(package:gdata)

## Unpslit series with loess smoother
png(file="figures/hypercritical-shows-univ.png",height=800,width=1200,
    res=100, pointsize=9)
p <- ggplot(data, aes(x=Show, y=Mins))
p + xlab("Show Number") +
  ylab("Length of Show in Minutes") +
  geom_smooth(method="loess", size=1.5, alpha=0.2) +
  geom_vline(xintercept=50.5) + geom_point(size=2) + theme_bw() +
  opts(title="Did John's effort to shorten shows have any effect?",
       legend.position="top")
credit()
dev.off()

## Split series with loess lines
png(file="figures/hypercritical-shows-loess.png",height=800,width=1200,
    res=100, pointsize=9)
p <- ggplot(data, aes(x=Show, y=Mins, color=Goal))
p1 <- p + geom_point(size=2) + xlab("Show Number") +
  ylab("Length of Show in Minutes") + geom_smooth(method="loess",
  aes(group=Goal, fill=Goal), size=1.5, alpha=0.2) + geom_vline(xintercept=50.5)
p1 + theme_bw() +
  opts(title="Did John's effort to shorten shows have any effect?",
       legend.position="top")
credit()
dev.off()


## Split series with lm fits
png(file="figures/hypercritical-shows-lm.png",height=800,width=1200,
    res=100, pointsize=9)
p <- ggplot(data, aes(x=Show, y=Mins, color=Goal))
p1 <- p + geom_point(size=2) + xlab("Show Number") +
  ylab("Length of Show in Minutes") + geom_smooth(method="lm",
  aes(group=Goal, fill=Goal), size=2, alpha=0.2) + geom_vline(xintercept=50.5)
p1 + theme_bw() +
  opts(title="Did John's effort to shorten shows have any effect?",
       legend.position="top")
credit()
dev.off()

## Split series with lm lines, and a smoother for the unsplit series
png(file="figures/hypercritical-shows.png",height=800,width=1200,
    res=100, pointsize=9)
p <- ggplot(data, aes(x=Show, y=Mins, color=Goal))
p1 <- p + geom_point(size=2) + xlab("Show Number") +
  ylab("Length of Show in Minutes") + geom_smooth(method="lm",
  aes(group=Goal, fill=Goal), size=2, alpha=0.2) + geom_vline(xintercept=50.5)
p1 + geom_smooth(aes(x=Show, y=Mins, color=NULL, group=NULL), fill="blue",
                     alpha=0.1, method="loess") + theme_bw() +
  opts(title="Did John's effort to shorten shows have any effect?",
       legend.position="top")
credit()
dev.off()


### --------------------------------------------------
### All Shows
### --------------------------------------------------
data <- read.csv("data/shows.csv", header=TRUE)
with(data, by(Minutes, Show, summary))

## NB: Create the figures/ folder before running the code

## All the data, with a linear trend line
png(file="figures/5by5-show-lengths.png",height=800,width=1200,
    res=100, pointsize=9)
p <- ggplot(data, aes(x=Episode, y=Minutes))
p + geom_smooth(method="lm", size=1.5) +
  geom_point(size=1.5) + xlab("Episode Number") +
  ylab("Length in Minutes") +
    opts(title="Trends in Episode Length for Five 5by5 Shows") + theme_bw()
credit()
dev.off()

## Break out by show, with cubic spline smoothers
png(file="figures/5by5-shows-comp2.png",height=800,width=1200,
    res=100, pointsize=9)
p <- ggplot(data, aes(x=Episode, y=Minutes, group=Show, color=Show))
p + geom_smooth(aes(fill=Show), method="rlm", formula=y~ns(x,3)) +
  geom_point(size=1.5) + xlab("Episode Number") +
  ylab("Length in Minutes") +
    opts(title="Trends in Episode Length by Show") + theme_bw()
credit()
dev.off()

## Variance
png(file="figures/5by5-variance.png", height=600, width=1000, res=120,
    pointsize=14)
p <- ggplot(data,aes(x=Show,y=Minutes))
p + geom_point(color="darkred", alpha=0.3, size=3) + theme_bw() + xlab("") +
  ylab("Show Length in Minutes") +
    opts(title="Variance in Episode Length Across Five Shows") + coord_flip()
dev.off()
