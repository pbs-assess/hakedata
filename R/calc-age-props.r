## C Grandin February 2017
## Create weighted age proportions for Hake from three fisheries, Shoreside,
## Freezer Trawler, and JV.

## For the by-month calculations
require(lubridate)

## Avoid warning about write.table not appending a column to the file
## in write.csv
options(warn = -1)

## .LW.CUTOFF - If you want to estimate yearly alpha and beta for the
##              length-weight relationship, and there are this many or more
##              specimens for a year with both length and weight data, then
##              they will be estimated using those records. If there are less
##              than this many that have both length and weight, a fishery-wide
##              (all years) global estimate will be used instead.
.LW.CUTOFF = 100

## Freezer Trawler Vessel IDs for GFBio
VIKING.ENTERPRISE <- 568
NORTHERN.ALLIANCE <- 592
OSPREY.NO.1       <- 569
RAW.SPIRIT        <- 595

filter.areas <- function(## d = Data frame
                         d){
  ## Filter out the areas not in the Hake assessment
  ## Note that 4B Minor 20 = Juan De Fuca Strait, the only minor area included
  ## in offshore Hake stock.
  return(subset(d, MAJOR_STAT_AREA_CODE == 3 | ## 3C
                   MAJOR_STAT_AREA_CODE == 4 | ## 3D
                   MAJOR_STAT_AREA_CODE == 5 | ## 5A
                   MAJOR_STAT_AREA_CODE == 6 | ## 5B
                   MAJOR_STAT_AREA_CODE == 7 | ## 5C
                   MAJOR_STAT_AREA_CODE == 8 | ## 5D
                   MAJOR_STAT_AREA_CODE == 9 | ## 5E
                   (MAJOR_STAT_AREA_CODE == 1 & MINOR_STAT_AREA_CODE == 20)))
}

filter.ages <- function(d){
  ## Remove ageless records from the data 'd' and return resulting data frame
  return(d[!is.na(d$SPECIMEN_AGE),])
}

filter.fisheries <- function(## d = Data frame
                             d,
                             ## vids is a vector of vessel IDs to filter
                             vids = NULL,
                             ## If include.vids is TRUE, include records for
                             ## the given 'vids'. If FALSE, include all records
                             ## except for those in 'vids'
                             include.vids = TRUE
                             ){
  ## Filter data according to vessel ID numbers (vids).
  ## Returns a data frame containing the filtered data
  ## If 'vids' is null, then 'd' will just be returned.
  if(is.null(vids)){
    return(d)
  }else{
    if(include.vids){
      return(d[d$VESSEL_ID %in% vids, ])
    }else{
      return(d[!(d$VESSEL_ID %in% vids), ])
    }
  }
}

add.zeros <- function(## d = Data frame
                      d,
                      ## plus is the value to fill zeros in to, e.g. if d has
                      ## values for ages 2,3,4,5,6,7 but plus = 10, the output
                      ## will have ages for 2,3,4,5,6,7,8,9,10, but 8,9, and 10
                      ## will be zeros.
                      plus = 10){
  ## Add ages to input frequency vector and set value to zero
  ## for ages which do not exist in the input data frame d

  tmpd <- d
  ages <- as.numeric(names(d))
  ageMax <- plus
  ## Use the plus group as the limiter or use the maximum age if it is larger
  ## than the plus group age
  if(max(ages) > ageMax){
    ageMax <- max(ages)
  }
  for(age in 1:ageMax){
    if(!(age %in% ages)){
      ## Set this age to zero
      tmpd <- c(tmpd, 0)
      ages <- c(ages,age)
    }
  }
  names(tmpd) <- ages
  ## Sort the ages, must sort by names on the vector
  tmpd <- tmpd[order(as.numeric(names(tmpd)))]
  return(tmpd)
}

est.lw.params <- function(## d = Data frame
                          d){
  ## Estimate the length/weight parameters for the dataset d
  ## Returns the estimated parameters alpha (a) and beta (b) for these data
  ## (w=al^b)

  ## Filter data for individuals with both length and weight
  d <- d[!is.na(d$Length_cm),]
  d <- d[!is.na(d$Weight_g),]
  w <- d$Weight_g
  l <- d$Length_cm
  fit <- nls(w ~ a*l^b,
             start = c(a = 0.5, b = 2.0),
             control = list(tol = 0.1, maxiter = 500))
  return(coefficients(fit))
}

apply.weights <- function(## prop is the age proportions as a named vector
                          prop,
                          ## dat is a data frame representing one sample
                          dat,
                          ## lwParams is a named vector containing the alpha (a)
                          ## and beta (b) parameter estimates to use for
                          ## estimating sample weights from lengths. Output of
                          ## coeffiecients(nls).
                          lw.params,
                          ## weightAdj is Weight adjustment, sample weights
                          ## calculated from LW models will be divided by this
                          weightAdj                  = 1,
                          useSampleWeightIfAvailable = TRUE,
                          ## useRawPropsWhenNeeded - If there are no weights,
                          ## lengths, or sample weights, return the input
                          ## proportions.
                          useRawPropsWhenNeeded      = TRUE
                          ){
  ## Apply the weighting scheme to the proportions at age found in prop
  ## (a named vector) and return the new, weighted proportions when possible.
  ## The weighting scheme is as follows:
  ##  If there is a sample weight and 'useSampleWeightIfAvailable' is TRUE,
  ##   multiply the proportions by the ratio of the catch weight to the sample
  ##   weight.
  ##
  ##  If 'useSampleWeightsIfAvailable' is FALSE, the empirical weights or
  ##   length/weight relationship described below will be used.
  ##
  ##  If there is no sample weight, calculate a sample weight:
  ##   1. If there are empirical weights, sum them and call it the sample weight
  ##   2. If no individual weight data, extract length data and apply the
  ##      length/weight relationship to each length to get the weight of each
  ##      fish, then sum those to get the sample weight.
  ##   3. Multiply 'prop' by the ratio of the catch weight to the sample weight
  ##
  ##  If there are no weights, no lengths, and no sample weight, and if
  ##   'useRawPropsWhenNeeded' is TRUE then the raw, unweighted proportions will
  ##   be returned.
  ##
  ##  Returns one of:
  ##  1. The new, weighted proportions.
  ##  2. If there are no weights, no lengths, and no sample weights and
  ##     'useRawPropsWhenNeeded' is TRUE then returns the input proportions.
  ##  3. If there are no weights, no lengths, and no sample weights and
  ##     'useRawPropsWhenNeeded' is FALSE then returns NULL.

  sampleWeight <- NULL
  catchWeight <- dat$CATCH_WEIGHT[1]
  if(useSampleWeightIfAvailable){
    sampleWeight <- dat$SAMPLE_WEIGHT[1]
  }
  w <- dat$Weight_g
  l <- dat$Length_cm
  if(!(any(is.na(w)))){
    ## Weight data present
    if(is.na(sampleWeight)) sampleWeight <- sum(w)
  }else{
    ## Try to use length data
    if(!(any(is.na(l)))){
      if(is.na(sampleWeight)){
        sampleWeight <- sum(lw.params[1]*l^lw.params[2])
      }
    }else{
      if(is.na(sampleWeight)){
        ## sampleWeight, length and weight data are all null in this case
        if(useRawPropsWhenNeeded){
          return(props)
        }else{
          return(NULL)
        }
      }
    }
  }
  weightedProp <- prop * catchWeight / (sampleWeight / weightAdj)
  return(weightedProp)
}

calcAgeProps <- function(## d is a data frame
                         d = NULL,
                         ## minus is a minus group limit for age
                         minus,
                         ## plus is a plus group limit for age
                         plus,
                         ## lw.params is a named vector containing the alpha (a)
                         ## and beta (b) parameter estimates to use for
                         ## estimating sample weights from lengths. Output of
                         ## coeffiecients(nls).
                         lw.params,
                         ## weightAdj is Weight adjustment, sample weights
                         ## calculated from LW models will be divided by this
                         weight.adj = 1,
                         ## use.sample.weight.if.avail - Use the empirical
                         ## sample weight if it is in the data
                         use.sample.weight.if.avail = TRUE,
                         ## use.raw.props.when.needed - If there are no weights,
                         ## lengths, or sample weights, return the input
                         ## proportions.
                         use.raw.props.when.needed = TRUE,
                         ## use.yearly.lw.params - Use LW parameter estimates by
                         ## year where data allow (both weight and length
                         ## samples exist)
                         use.yearly.lw.params = FALSE,
                         ## lw.param.cutoff - See the top of this file
                         lw.param.cutoff = .LW.CUTOFF,
                         ## {roduce the age proportions by month (of sample date)
                         verbose = TRUE
                         ){
  ## Calculate the age proportions weighted by weight of sampled catch/sum of
  ## weight of sampled catch by year (and month if by.month = TRUE). Returns
  ## a data frame with the number of rows equal to the number of years (or months) in the
  ## input data frame d. minus/plus are age accumulator groups.

  if(is.null(d)){
    return(NULL)
  }
  ret <- data.frame()
  years <- sort(unique(d$Year))
  ## allAgeProps holds the age proportions by year, one row for each year (or month)
  allAgeProps <- NULL
  for(yr in years){
    yrdat <- d[d$Year == yr,]
    yrCatchWeight <- sum(yrdat$CATCH_WEIGHT)
    ## Now get all unique samples for this year
    samples <- sort(unique(yrdat$SAMPLE_ID))
    ## Set up a data frame for summing sample totals by age
    ## yrAgeProps holds a row for each sample taken that year
    yrAgeProps <- NULL
    if(verbose){
      cat("Processing year: ",yr,", #samples = ",length(samples),"\n",sep="")
    }
    yrlw.params <- lw.params
    if(use.yearly.lw.params){
      ## Check to see if the number of weights and lengths which are not NA are
      ## equal or more than the cutoff
      goodInds <- !(is.na(yrdat$Length_cm) | is.na(yrdat$Weight_g))
      numGoodInds <- sum(goodInds, na.rm=TRUE)
      if(numGoodInds >= lw.param.cutoff){
        if(verbose){
          cat(" Enough length-weight pairs (",numGoodInds,") for ",yr,
              " which meets or exceeds cutoff of ",lw.param.cutoff,"\n",sep="")
        }
        ## Filter yrdat so that NA's are removed and then estimate LW parameters
        yrlw.params <- est.lw.params(yrdat[goodInds,])
      }else{
        if(verbose){
          cat(" Not enough length-weight pairs (",numGoodInds,") for ",yr,
              " to meet cutoff of ",lw.param.cutoff,
              ", using global LW estimate.\n",sep="")
        }
      }
    }
    if(verbose){
      cat(" L/W relationship, alpha = ", yrlw.params[1],
          ", beta = ", yrlw.params[2], "\n")
    }
    for(samp in samples){
      sampdat <- yrdat[yrdat$SAMPLE_ID == samp,]

      ## Get age proportions for this year and sample
      sampProps <- table(sampdat$SPECIMEN_AGE)

      ## Fill in the ages that do not exist in the data by adding a column for
      ## each and setting to zero.
      sampProps <- add.zeros(sampProps, plus = plus)

      ## Get the ages found for this year and sample
      sampAges <- as.numeric(names(sampProps))

      ## If there are ages greater than or equal to the plus group,
      ## merge them by summing them.
      sampProps[sampAges==plus] <- sum(sampProps[sampAges>=plus])

      ## Same for the minus group
      sampProps[sampAges==minus] <- sum(sampProps[sampAges<=minus])

      ## Slice the sample proportions vector because the minus and plus groups
      ## have already been summed and added to the correct minus/plus age bin.
      sampProps <- sampProps[minus:plus]
      weightedSampProps <- apply.weights(sampProps,
                                         sampdat,
                                         yrlw.params,
                                         weight.adj,
                                         use.sample.weight.if.avail,
                                         use.raw.props.when.needed)
      if(!is.null(weightedSampProps)){
        yrAgeProps <- rbind(yrAgeProps, weightedSampProps)
      }
    }
    if(verbose) cat("\n")

    ## Sum the rows for this year, and save the weighted age proportions.
    yrAgePropsSum <- apply(yrAgeProps, 2, sum)
    allAgeProps <- rbind(allAgeProps, yrAgePropsSum)
  }
  rownames(allAgeProps) <- years

  ## Normalize the ages for each year by elements / row sums
  ageSums <- apply(allAgeProps, 1, sum)
  allAgeProps <- t(allAgeProps)
  allAgeProps <- t(allAgeProps / rep(ageSums,each=nrow(allAgeProps)))
  if(verbose){
    cat("*******************************************************\n")
  }
  return(allAgeProps)
}

calcAgeProps.bymonth <- function(## d is a data frame
                                 d = NULL,
                                 ## minus is a minus group limit for age
                                 minus,
                                 ## plus is a plus group limit for age
                                 plus,
                                 ## lw.params is a named vector containing the alpha (a)
                                 ## and beta (b) parameter estimates to use for
                                 ## estimating sample weights from lengths. Output of
                                 ## coeffiecients(nls).
                                 lw.params,
                                 ## weightAdj is Weight adjustment, sample weights
                                 ## calculated from LW models will be divided by this
                                 weight.adj = 1,
                                 ## use.sample.weight.if.avail - Use the empirical
                                 ## sample weight if it is in the data
                                 use.sample.weight.if.avail = TRUE,
                                 ## use.raw.props.when.needed - If there are no weights,
                                 ## lengths, or sample weights, return the input
                                 ## proportions.
                                 use.raw.props.when.needed = TRUE,
                                 ## use.yearly.lw.params - Use LW parameter estimates by
                                 ## year where data allow (both weight and length
                                 ## samples exist)
                                 use.yearly.lw.params = FALSE,
                                 ## lw.param.cutoff - See the top of this file
                                 lw.param.cutoff = .LW.CUTOFF,
                                 ## {roduce the age proportions by month (of sample date)
                                 verbose = TRUE
                                 ){
  ## Calculate the age proportions weighted by weight of sampled catch/sum of
  ## weight of sampled catch by year (and month if by.month = TRUE). Returns
  ## a data frame with the number of rows equal to the number of years (or months) in the
  ## input data frame d. minus/plus are age accumulator groups.

  if(is.null(d)){
    return(NULL)
  }
  ret <- data.frame()
  years <- sort(unique(d$Year))
  ## all.age.props holds the age proportions by year, one row for each year (or month)
  all.age.props <- list()
  ind <- 1
  for(yr in years){
    yr.age.props <- NULL
    yr.dat <- d[d$Year == yr,]
    if(use.yearly.lw.params){
      ## Check to see if the number of weights and lengths which are not NA are
      ## equal or more than the cutoff
      goodInds <- !(is.na(yr.dat$Length_cm) | is.na(yr.dat$Weight_g))
      numGoodInds <- sum(goodInds, na.rm = TRUE)
      if(numGoodInds >= lw.param.cutoff){
        if(verbose){
          cat(" Enough length-weight pairs (",numGoodInds,") for ",yr,
              " which meets or exceeds cutoff of ",lw.param.cutoff,"\n",sep="")
        }
        ## Filter yr.dat so that NA's are removed and then estimate LW parameters
        lw.params <- est.lw.params(yr.dat[goodInds,])
      }else{
        if(verbose){
          cat(" Not enough length-weight pairs (",numGoodInds,") for ",yr,
              " to meet cutoff of ",lw.param.cutoff,
              ", using global LW estimate.\n",sep="")
        }
      }
    }
    if(verbose){
      cat(" L/W relationship, alpha = ", lw.params[1],
          ", beta = ", lw.params[2], "\n")
    }

    months <- sort(unique(yr.dat$Month))
    for(mn in months){
      mon.dat <- yr.dat[yr.dat$Month == mn,]
      mon.catch.weight <- sum(mon.dat$CATCH_WEIGHT)
      ## Now get all unique samples for this year/month
      samples <- sort(unique(mon.dat$SAMPLE_ID))
      ## Set up a data frame for summing sample totals by age
      ## mon.age.props holds a row for each sample taken that year/month
      mon.age.props <- NULL
      if(verbose){
        cat("Processing year: ", yr, ", month: ",
            mn, ", #samples = ", length(samples), "\n", sep = "")
      }

      for(samp in samples){
        samp.dat <- mon.dat[mon.dat$SAMPLE_ID == samp,]

        ## Get age proportions for this year/month and sample
        samp.props <- table(samp.dat$SPECIMEN_AGE)

        ## Fill in the ages that do not exist in the data by adding a column for
        ## each and setting to zero.
        samp.props <- add.zeros(samp.props, plus = plus)

        ## Get the ages found for this year and sample
        samp.ages <- as.numeric(names(samp.props))

        ## If there are ages greater than or equal to the plus group,
        ## merge them by summing them.
        samp.props[samp.ages==plus] <- sum(samp.props[samp.ages>=plus])

        ## Same for the minus group
        samp.props[samp.ages==minus] <- sum(samp.props[samp.ages<=minus])

        ## Slice the sample proportions vector because the minus and plus groups
        ## have already been summed and added to the correct minus/plus age bin.
        samp.props <- samp.props[minus:plus]
        weighted.samp.props <- apply.weights(samp.props,
                                             samp.dat,
                                             lw.params,
                                             weight.adj,
                                             use.sample.weight.if.avail,
                                             use.raw.props.when.needed)
        ## Add column for the number of samples
        if(!is.null(weighted.samp.props)){
          mon.age.props <- rbind(mon.age.props, weighted.samp.props)
        }
      }
      if(verbose) cat("\n")

      ## Sum the rows for this year, and save the weighted age proportions.
      mon.age.props.sum <- apply(mon.age.props, 2, sum)
      yr.age.props <- rbind(yr.age.props, mon.age.props.sum)
    }
    rownames(yr.age.props) <- months
    ## Normalize the ages for each year by elements / row sums
    yr.age.sums <- apply(yr.age.props, 1, sum)
    yr.age.props <- t(yr.age.props)
    yr.age.props <- t(yr.age.props / rep(yr.age.sums, each = nrow(yr.age.props)))
    all.age.props[[ind]] <- yr.age.props
    ind <- ind + 1
  }
  names(all.age.props) <- years
  if(verbose){
    cat("*******************************************************\n")
  }
  return(all.age.props)
}

get.num.hauls <- function(## d is a data frame
                          d){
  ## Return a data frame of the year
  ##  and number of hauls (sets)

  if(is.null(d)) return(NULL)
  years <- sort(unique(d$Year))
  numhauls <- NULL
  for(yr in years){
    yrdat <- d[d$Year == yr,]
    trips <- unique(yrdat$TRIP_ID)
    nh <- 0
    for(trip in trips){
      tripdat <- yrdat[yrdat$TRIP_ID == trip,]
      sets <- unique(tripdat$setnumber)
      nh <- nh + length(sets)
    }
    numhauls <- rbind(numhauls, c(yr,nh))
  }
  return(numhauls)
}

get.num.hauls.bymonth <- function(## d is a data frame
                          d){
  ## Return a data frame of the year and month
  ##  and number of hauls (sets)

  if(is.null(d)) return(NULL)
  years <- sort(unique(d$Year))
  numhauls <- NULL
  for(yr in years){
    yrdat <- d[d$Year == yr,]
    months <- sort(unique(yrdat$Month))
    for(mn in months){
      mondat <- yrdat[yrdat$Month == mn,]
      trips <- unique(mondat$TRIP_ID)
      nh <- 0
      for(trip in trips){
        tripdat <- mondat[mondat$TRIP_ID == trip,]
        sets <- unique(tripdat$setnumber)
        nh <- nh + length(sets)
      }
      numhauls <- rbind(numhauls, c(yr, mn, nh))
    }
  }
  return(numhauls)
}

get.num.trips <- function(## d is a data frame
                          d = NULL){
  ## Returns a data frame of the year and the number of trips for that year
  if(is.null(d)) return(NULL)
  years <- sort(unique(d$Year))
  numtrips <- NULL
  for(yr in years){
    yrdat <-d[d$Year == yr,]
    trips <- unique(yrdat$TRIP_ID)
    nt <- length(trips)
    numtrips <- rbind(numtrips, c(yr,nt))
  }
  return(numtrips)
}

get.num.trips.bymonth <- function(## d is a data frame
                                  d = NULL){
  ## Returns a data frame of the year and month and the
  ##  number of trips for that year
  if(is.null(d)) return(NULL)
  years <- sort(unique(d$Year))
  numtrips <- NULL
  for(yr in years){
    yrdat <- d[d$Year == yr,]
    months <- sort(unique(yrdat$Month))
    for(mn in months){
      mondat <- yrdat[yrdat$Month == mn,]
      trips <- unique(mondat$TRIP_ID)
      nt <- length(trips)
      numtrips <- rbind(numtrips, c(yr, mn, nt))
    }
  }
  return(numtrips)
}

make.num.hauls.trips <- function(## d is a data frame
                                 d = NULL,
                                 ## filterHauls - If TRUE, return number of hauls
                                 ## data.frame, if FALSE return number of trips
                                 ## data.frame
                                 filterHauls = TRUE,
                                 ## ageSampled - If TRUE, return number of
                                 ## trips/hauls which have been age sampled. If
                                 ## FALSE, return total number of trips/hauls.
                                 ageSampled  = TRUE,
                                 by.month = FALSE
                                 ){
  ## Return a data frame with the number of hauls or trips by year found in data frame 'd'
  if(is.null(d)){
    return(NULL)
  }
  a <- filter.areas(d)
  if(ageSampled){
    a <- filter.ages(a)
  }
  if(filterHauls){
    if(by.month){
      return(get.num.hauls.bymonth(a))
    }else{
      return(get.num.hauls(a))
    }
  }else{
    if(by.month){
      return(get.num.trips.bymonth(a))
    }else{
      return(get.num.trips(a))
    }
  }
}

make.age.comps <- function(## d is a data frame
                           d = NULL,
                           ## minus is a minus group limit for age
                           minus,
                           ## plus is a plus group limit for age
                           plus,
                           ## weight.adj is Weight adjustment, sample weights
                           ## calculated from LW models will be divided by this
                           weight.adj = 1,
                           ## use.yearly.lw.params - Use LW parameter estimates by
                           ## year where data allow (both weight and length
                           ## samples exist)
                           use.yearly.lw.params = FALSE,
                           ## use.raw.props.when.needed - If there are no weights,
                           ## lengths, or sample weights, return the input
                           ## proportions.
                           use.raw.props.when.needed = TRUE,
                           by.month = FALSE,
                           verbose = TRUE
                         ){
  ## Make age compositions for data frame 'd'
  if(is.null(d)){
    return(NULL)
  }
  a <- filter.areas(d)
  ## Estimate Length/Weight parameters on all data (not just aged) in the
  ## assessment areas
  lw.params <- est.lw.params(a)
  b <- filter.ages(a)
  if(by.month){
    p <- calcAgeProps.bymonth(b,
                              minus = minus,
                              plus = plus,
                              lw.params = lw.params,
                              weight.adj = weight.adj,
                              use.sample.weight.if.avail = TRUE,
                              use.raw.props.when.needed = use.raw.props.when.needed,
                              use.yearly.lw.params = use.yearly.lw.params,
                              verbose = verbose)
  }else{
      p <- calcAgeProps(b,
                        minus = minus,
                        plus = plus,
                        lw.params = lw.params,
                        weight.adj = weight.adj,
                        use.sample.weight.if.avail = TRUE,
                        use.raw.props.when.needed = use.raw.props.when.needed,
                        use.yearly.lw.params = use.yearly.lw.params,
                        verbose = verbose)
  }
  return(p)
}

load.raw.data <- function(fn.dom,
                          fn.jv,
                          ## ft.vids are the Freezer Trawler GFBio vessel ids
                          ft.vids){
  ## Store raw data in the global environment
  ## Each of the three fisheries are also seperated in this step
  d.dom <- read.csv(fn.dom,
                    header = TRUE,
                    stringsAsFactors = FALSE)
  d.jv <- read.csv(fn.jv,
                   header = TRUE,
                   stringsAsFactors = FALSE)

  ## The domestic data needs to be broken up into shoreside and freezer
  ## trawler data
  d.ft <- filter.fisheries(d.dom,
                           ft.vids,
                           include.vids = TRUE)
  d.ss <- filter.fisheries(d.dom,
                           ft.vids,
                           include.vids = FALSE)
  invisible(list(d.ft, d.ss, d.jv))
}

make.all.age.comps <- function(d.ft,
                               d.ss,
                               d.jv,
                               ## minus is a minus group limit for age
                               minus,
                               ## plus is a plus group limit for age
                               plus,
                               ## weight.adj is Weight adjustment, sample weights
                               ## calculated from LW models will be divided by this
                               weight.adj = 1,
                               ## use.yearly.lw.params - Use LW parameter estimates by
                               ## year where data allow (both weight and length
                               ## samples exist)
                               use.yearly.lw.params = FALSE,
                               ## use.raw.props.when.needed - If there are no weights,
                               ## lengths, or sample weights, return the input
                               ## proportions.
                               use.raw.props.when.needed = TRUE,
                               by.month = FALSE,
                               verbose = TRUE){
  ## Make age compositions for all three Hake fisheries in the global
  ## environment
  ## Assumes 'load.raw.data' has been run so the global data are available
  if(verbose){
    cat("Calculating Shoreside age comps.....\n")
  }
  ## Shoreside age proportions
  ac.ss <- make.age.comps(d.ss,
                          minus,
                          plus,
                          weight.adj,
                          use.yearly.lw.params = use.yearly.lw.params,
                          use.raw.props.when.needed = use.raw.props.when.needed,
                          by.month = by.month,
                          verbose)
  if(verbose){
    cat("Calculating Freezer Trawler age comps.....\n")
  }
  ## Freezer trawler age proportions
  ac.ft <- make.age.comps(d.ft,
                          minus,
                          plus,
                          weight.adj,
                          use.yearly.lw.params = use.yearly.lw.params,
                          use.raw.props.when.needed = use.raw.props.when.needed,
                          by.month = by.month,
                          verbose)
  if(verbose){
    cat("Calculating JV age comps.....\n")
  }
  ## Joint venture age proportions
  ac.jv <- make.age.comps(d.jv,
                          minus,
                          plus,
                          weight.adj,
                          use.yearly.lw.params = use.yearly.lw.params,
                          use.raw.props.when.needed = use.raw.props.when.needed,
                          by.month = by.month,
                          verbose)
  invisible(list(ac.ss, ac.ft, ac.jv))
}

make.all.num.hauls.trips <- function(d.ft,
                                     d.ss,
                                     d.jv,
                                     ## ageSampled - If TRUE, return number of
                                     ## trips/hauls which have been age sampled. If
                                     ## FALSE, return total number of trips/hauls.
                                     ageSampled = TRUE,
                                     by.month = FALSE,
                                     verbose = TRUE
                                     ){
  ## Make data frames for the number of hauls (Freezer Trawlers and JV) or
  ## number of trips (Shoreside) in the global environment
  ## Assumes 'load.raw.data' has been run so the global data are available
  if(verbose){
    cat("Calculating Shoreside number of hauls/trips.....\n")
  }
  ss.num <- make.num.hauls.trips(d.ss,
                                 filterHauls = FALSE,
                                 ageSampled = ageSampled,
                                 by.month)
  if(verbose){
    cat("Calculating Freezer Trawler number of hauls/trips.....\n")
  }
  ft.num <- make.num.hauls.trips(d.ft,
                                 filterHauls = TRUE,
                                 ageSampled = ageSampled,
                                 by.month)
  if(verbose){
    cat("Calculating JV number of hauls/trips.....\n")
  }
  jv.num <- make.num.hauls.trips(d.jv,
                                 filterHauls = TRUE,
                                 ageSampled = ageSampled,
                                 by.month)
  list(ss.num, ft.num, jv.num)
}

save.age.dat <- function(fn,
                         ac,
                         num){
  ## Save the age comp (ac) and number of samples aged (num)
  ##  into filename fn. Delete the old file first

  if(file.exists(fn)){
    unlink(fn, force = TRUE)
  }
  if(!is.list(ac)){
    ac <- list(ac)
  }
  lapply(1:length(ac),
         function(x){
           write(names(ac)[[x]], fn, append = TRUE)
           if(ncol(num) == 3){
             ## By-month
             yr <- names(ac)[[x]]
             mn <- rownames(ac[[x]])
             ac[[x]] <- cbind(ac[[x]], num[num[,1] == yr &
                                           num[,2] %in% mn, 3])
           }else{
             ## Assume two columns, therefore not by month
             ac[[x]] <- cbind(ac[[x]], num[,2])
           }
           colnames(ac[[x]])[length(colnames(ac[[x]]))] <- "N"
           write.table(ac[[x]],
                       fn,
                       quote = FALSE,
                       col.names = NA,
                       row.names = TRUE,
                       sep = ",",
                       append = TRUE)})
}

add.month.col <- function(d){
  ## Add the column 'Month' to the data based on the sample date
  Month <- month(d$SAMPLE_DATE)
  cbind(Month, d)
}

## -----------------------------------------------------------------------------
## -----------------------------------------------------------------------------
## Script

ft.vids  <- c(VIKING.ENTERPRISE,
              NORTHERN.ALLIANCE,
              OSPREY.NO.1,
              RAW.SPIRIT)

d <- load.raw.data("hake_domestic_obs_len_wt_age.csv",
                   "hake_jv_obs_len_wt_age.csv",
                   ft.vids)
## -----------------------------------------------------------------------------
## By year
ss.out.fn <- "canada-shoreside-age-data.csv"
ft.out.fn <- "canada-freezer-trawler-age-data.csv"
jv.out.fn <- "canada-joint-venture-age-data.csv"
d.ft <- d[[1]]
d.ss <- d[[2]]
d.jv <- d[[3]]
ac <- make.all.age.comps(d.ft,
                         d.ss,
                         d.jv,
                         minus = 1,
                         plus = 15,
                         weight.adj = 1000,
                         use.yearly.lw.params = TRUE,
                         use.raw.props.when.needed = TRUE,
                         by.month = FALSE,
                         verbose = TRUE)
ss.ac <- ac[[1]]
ft.ac <- ac[[2]]
jv.ac <- ac[[3]]

num <- make.all.num.hauls.trips(d.ft,
                                d.ss,
                                d.jv,
                                ageSampled = TRUE,
                                by.month = FALSE,
                                verbose = TRUE)
ss.num <- num[[1]]
ft.num <- num[[2]]
jv.num <- num[[3]]

save.age.dat(ss.out.fn,
             ss.ac,
             ss.num)
save.age.dat(ft.out.fn,
             ft.ac,
             ft.num)
save.age.dat(jv.out.fn,
             jv.ac,
             jv.num)

## -----------------------------------------------------------------------------
## By year and month
ss.out.fn <- "canada-shoreside-age-data-by-month.csv"
ft.out.fn <- "canada-freezer-trawler-age-data-by-month.csv"
jv.out.fn <- "canada-joint-venture-age-data-by-month.csv"
d.ft.mn <- add.month.col(d[[1]])
d.ss.mn <- add.month.col(d[[2]])
d.jv.mn <- add.month.col(d[[3]])

ac <- make.all.age.comps(d.ft.mn,
                         d.ss.mn,
                         d.jv.mn,
                         minus = 1,
                         plus = 15,
                         weight.adj = 1000,
                         use.yearly.lw.params = TRUE,
                         use.raw.props.when.needed = TRUE,
                         by.month = TRUE,
                         verbose = TRUE)
ss.ac.mn <- ac[[1]]
ft.ac.mn <- ac[[2]]
jv.ac.mn <- ac[[3]]

num <- make.all.num.hauls.trips(d.ft.mn,
                                d.ss.mn,
                                d.jv.mn,
                                ageSampled = TRUE,
                                by.month = TRUE,
                                verbose = TRUE)
ss.num.mn <- num[[1]]
ft.num.mn <- num[[2]]
jv.num.mn <- num[[3]]

save.age.dat(ss.out.fn,
             ss.ac.mn,
             ss.num.mn)
save.age.dat(ft.out.fn,
             ft.ac.mn,
             ft.num.mn)
save.age.dat(jv.out.fn,
             jv.ac.mn,
             jv.num.mn)
