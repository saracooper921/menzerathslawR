#' imp function
#' Loads all wav files in specified working directory into named list
#'
#' @param mypath Directory in which files are stored
#' @return Dataframe of audio files
imp <- function(mypath) {
  tmp.list.1 <- list.files(mypath, pattern = ".wav$")
  tmp.list.2 <- list(length = length(tmp.list.1))


  for (i in 1:length(tmp.list.1))
  {
    tmp.list.2[[i]]<-readWave(tmp.list.1[i],...)
  }

  names(tmp.list.2)<-tmp.list.1
  tmp.list.2
}

#' coolBlueHotRed function
#' Create a vector of n contiguous colors
#'
#' @param n the number of colors (≥ 1) to be in the palette
#' @param alpha the alpha transparency, a number in [0,1]
#' @return a default color palette for the kohonen heatmaps
coolBlueHotRed <- function(n, alpha = 1) {
  rainbow(n, end=4/6, alpha=alpha)[n:1]
}

#' visualize function
#'
#' Plots sound envelope. Marks calls and silences with duration.
#'
#' @param x A single imported audio file or list of imported audio files
#' @return plot of sound envelope with marked duration of calls and silences
visualize <- function(x) {
  for (i in x) {
    timer(i, threshold = 3, msmooth = c(400,90), dmin = 0.1)
  }
}

#' duration function
#'
#' Needed in Global Environment to activate call duration function
#' @param x An audio file
#' @return Duration of individual vocalizations in seconds
duration <- function(x) {
  t <- timer(x, threshold = 3, msmooth = c(400,90), dmin = 0.1, plot = FALSE)
  t$s
}

#' callduration function
#'  Measures duration of individual vocalizations
#'
#' @param xs  A single imported audio file or list of imported audio files
#' @return A list of individual vocalization durations in seconds. Information for list is retrieved by duration function.
callduration <- function(xs) {
  durations <- sapply(xs, duration)
  durations
}

#' meanduration function
#'
#' Needed in Global Environment to activate meancallduration function
#' @param x An imported audio file
#' @return mean duration of calls in an audio file
meanduration <- function(x) {
  t <- timer(x, threshold = 3, msmooth = c(400,90), dmin = 0.1, plot = FALSE)
  mean(t$s)
}

#' meancallduration function
#'
#' Measures mean duration of vocalizations
#' @param xs  A single imported audio file or list of imported audio files
#' @return A list of mean vocalization durations in seconds. Information for list is retrieved by duration function.
meancallduration <- function(xs) {
  durations <- sapply(xs, meanduration)
  durations
}

#' seqmeasure function
#'
#' Needed in Global Environment to activate seqsize function
#' @param x An audio file
#' @return Number of vocalizations per audio file
seqmeasure <- function(x) {
  t <- timer(x, threshold = 3, msmooth = c(400,90), dmin = 0.1, plot = FALSE)
  length(t$s)
}

#' seqsize function
#'
#' Measures number of vocalizations per audio file
#' @param x A single imported audio file or a list of imported audio files
#' @return A list of the number of vocalizations per audio file
seqsize <- function(xs) {
  seqs <- sapply(xs, seqmeasure)
  seqs
}

#' silence function
#'
#' Needed in Global Environment to activate silenceduration function
#' @param x An imported audio file
#' @return Duration of individual periods of silence between vocalizations
silence <- function(x) {
  t <- timer(x, threshold = 3, msmooth = c(400,90), dmin = 0.1, plot = FALSE)
  t$p
}

#' silenceduration function
#'
#'  Measures duration of silence (in seconds) between vocalizations
#' @param xs  A single imported audio file or list of imported audio files
#' @return A list of individual periods of silence. Information for list is retrieved by silence function.
silenceduration <- function(xs) {
  quiets <- sapply(xs, silence)
  quiets
}


#' meansilence function
#'
#' Needed in Global Environment to activate meansilenceduration function
#' @param x An imported audio file
#' @return Mean duration of silence per audio file
meansilence <- function(x) {
  t <- timer(x, threshold = 3, msmooth = c(400,90), dmin = 0.1, plot = FALSE)
  mean(t$p)
}

#' meansilenceduration function
#'
#' Measures mean duration of silence between vocalizations
#' @param xs  A single imported audio file or list of imported audio files
#' @return A list of mean duration (in seconds) of silence between vocalizations. Information for list is retrieved by meansilence function.
meansilenceduration <- function(xs) {
  quiets <- sapply(xs, meansilence)
  quiets
}

#' silencemeasure function
#'
#' Needed in Global Environment to activate silencesize function
#' @param x An audio file
#' @return Number of periods of silence per audio file
silencemeasure <- function(x) {
  t <- timer(x, threshold = 3, msmooth = c(400,90), dmin = 0.1, plot = FALSE)
  length(t$p)
}

#' silencesize function
#'
#' Measures number of silences between vocalizations per audio file
#' @param x A single imported audio file or a list of imported audio files
#' @return A list of the number of silence periods per audio file
silencesize <- function(xs) {
  sils <- sapply(xs, silencemeasure)
  sils
}

#' menzerath function
#'
#' This function determines whether a dataset exhibits Menzerath's law by running the sequence and constituent variables through a Kohonen neural network. The output is a heatmap of each variables' distribution and a Spearman's rank correlation test on the relationship between the variables. A negative correlation suggests that the data exhibits Menzerath's law.
#' @param x A matrix created using the resulting lists from the seqsize function and either the meancallduration or meansilenceduration functions
#' @return Two heatmaps, one for sequence size and one for constituent size, and the results of a Spearman's rank correlation test between the given variables
menzerath <- function(x) {
  set.seed(1)
  m.grid = somgrid(xdim = 11, ydim = 11, topo="hexagonal")
  m.som = som(x, grid=m.grid, rlen=100, alpha=c(0.05,0.01))
  plot(m.som, type = "property", property = m.som$codes[[1]][,1], main = "Sequence Size", palette.name=coolBlueHotRed)
  plot(m.som, type = "property", property = m.som$codes[[1]][,2], main = "Call Duration", palette.name=coolBlueHotRed)
  cor.test(m.som$codes[[1]][,1], m.som$codes[[1]][,2], method = "spearman")
}
