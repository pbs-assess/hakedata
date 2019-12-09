## Make sure to source("a.r") first, as it creates the
## ss.ac, ft.ac, jv,ac, ss.num, ft.num, and jv.num objects.
source("plotBubbles.R")
.END_YEAR <- 2019
figure.dir <- "figures"

plot.age.bubbles <- function(ac,
                             num,
                             fig.dir,
                             fn,
                             title = ""){
  ## Age composition bubble plot
  ## ac is the age comp data frame
  ## num is the number sampled for age data frame
  ## fn is the filename (assumed png)

  currpar <- par()
  on.exit(par(currpar))
  res <- 300
  width <- 7
  height <- 8
  units <- "in"

  if(!dir.exists(fig.dir)){
    dir.create(fig.dir)
  }
  png(file.path(fig.dir, fn),
      res = res,
      width = width,
      height = height,
      units = units)
  par(mar = c(5.1, 4.1, 5.1, 1.1))
  plotBubbles(t(ac),
              dnam = TRUE,
              hide0 = TRUE,
              size = 0.07,
              xlab = "Year",
              ylab = "Age",
              xlim = c(2005, .END_YEAR),
              axes = FALSE,
              clrs = c("blue","green","red"),
              las = 1,
              main = title)
  axis(3, at = num[,1], labels = num[,2])
  dev.off()
}

plot.age.bubbles.by.month <- function(ac,
                                      num,
                                      fig.dir,
                                      fn,
                                      title = ""){
  ## Age composition bubble plots by month.
  ## ac is the age comp data frame
  ## num is the number sampled for age data frame
  ## fn is the filename (assumed png)

  currpar <- par()
  on.exit(par(currpar))
  res <- 300
  width <- 7
  height <- 8
  units <- "in"

  if(!dir.exists(fig.dir)){
    dir.create(fig.dir)
  }
  png(file.path(fig.dir, fn),
      res = res,
      width = width,
      height = height,
      units = units)
  par(mar = c(5.1, 4.1, 5.1, 2.1))
  plotBubbles(t(ac),
              dnam = TRUE,
              hide0 = TRUE,
              size = 0.07,
              xlab = "Month",
              ylab = "Age",
              xlim = c(1, 15),
              axes = FALSE,
              clrs = c("blue","green","red"),
              las = 1,
              main = title)
  axis(3, at = num[,2], labels = num[,3])
  ##axis(1, at = rownames(ac), labels = month.abb[as.numeric(rownames(ac))])
  dev.off()
}

plot.age.bubbles(ft.ac,
                 ft.num,
                 figure.dir,
                 "freezer-trawlers-ac.png",
                 "Freezer Trawlers Canada")

plot.age.bubbles(ss.ac,
                 ss.num,
                 figure.dir,
                 "shoreside-ac.png",
                 "Shoreside Canada")

plot.age.bubbles(jv.ac,
                 jv.num,
                 figure.dir,
                 "joint-venture-ac.png",
                 "Joint Venture Canada")

for(yr in 2005:.END_YEAR){
  plot.age.bubbles.by.month(get(as.character(yr), ft.ac.mn),
                            ft.num.mn[ft.num.mn[,1] == yr,],
                            figure.dir,
                            paste0("freezer-trawlers-ac-by-month-", yr, ".png"),
                            paste0("Freezer Trawlers ", yr))

  plot.age.bubbles.by.month(get(as.character(yr), ss.ac.mn),
                            ss.num.mn[ss.num.mn[,1] == yr,],
                            figure.dir,
                            paste0("shoreside-ac-by-month-", yr, ".png"),
                            paste0("Shoreside ", yr))
}
