
# ggplot style ------------------------------------------------------------

gstyle <- function(base_size = 5, base_family = "sans"){
  theme_bw(base_size = base_size, base_family = base_family) +
    theme(
      # axis
      axis.title = element_text( size = rel(1.4) ),
      axis.text = element_text( size = rel(1.2), angle = 0,  colour = "grey10"),
      axis.text.x = element_text( margin = margin(2,0,1,0,"mm")),
      axis.text.y.right = element_text(margin=margin(0,2,0,2,"mm")),
      axis.text.y = element_text( margin = margin(0,2,0,2,"mm")),
      axis.ticks.length = unit( rel(-1), "mm"),
      axis.ticks = element_line( size = rel(0.5)),
      # grids 
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(), 
      panel.border = element_rect(colour = "grey10"),
      # strip
      strip.background = element_rect(colour = "white", fill = "white", size = rel(1.1)),
      strip.text.x = element_text(colour = "black", angle = 0, size = rel(1.1), hjust = 0.5, vjust = 0.5),
      # legend
      legend.justification=c(0,1), 
      legend.position=c(0.02,0.98),
      legend.key = element_rect(colour = 'white', fill = 'white', linetype='dashed', size = 0.1),
      legend.key.size = unit( rel(2), "mm"), 
      #legend.key.height = unit( rel(1), "mm"),
      legend.text = element_text( size = rel(1.2)),
      legend.title = element_text(size=rel(1.5))
    )
}

sinca_cols <- c( `Abies alba` = "#00b159",`Fagus sylvatica` = "#d11141",  `Carpinus betulus`= "#00aedb",
  `Acer pseudoplatanus`     = "#f37735",`yellow`     = "#ffc425",`light grey` = "#cccccc",`dark grey`  = "#8c8c8c",
  `gap` = "#00b159",`major` = "#d11141",`moderate` = "#f37735", `no event` = "#cccccc",  `release` = "#d11141")

sinca_fill <- function(...){
  scale_fill_manual(values = sinca_cols, ...)
}

sinca_fill_alpha <- function( ...){
  scale_fill_manual(values = alpha(sinca_cols, 0.5), ...)
}

sinca_color <- function(...){
  scale_color_manual(values = sinca_cols, ...)
}


# disturbance history detection -------------------------------------------

f_m1 <- function(x, m = 10) {
  #' @description M1 growht
  if ( length(x) < m) { 
    out <- rep(NA_real_, length(x) )
  } else {
    out <- rollapply( x, width = m, FUN = mean, fill = NA, align = "right", na.rm = T, partial = F)
  }
  out
}

f_m2 <- function(x, m = 10) {
  #' @description M2 growht
  if ( length(x) < m) {
    out <- rep(NA_real_, length(x) )
  } else {
    out <- rollapply( lead(x, 1), width = m, FUN = mean, fill = NA, align = "left", na.rm = T, partial = F)
  }
  out
}

f_pg <- function(x, m = 10) {
  #' @description prior growht
  if ( length(x) < m) {
    out <- rep(NA_real_, length(x) )
  } else {
    out <- rollapply( lag(x, 1), width = m, FUN = mean, fill = NA, align = "right", na.rm = T, partial = F)
  }
  out
}


peakDetection <- function(x, threshold, mindist = 20, nups = 2){
  #' @description identify the index of year when release event occur
  #' @param x a vector of absolute increase change
  #' @param threshold a minimum ai value in mm
  #' @param mindist  minimum distance between two consecutive peaks in years
  #' @param nups number of increasing steps before the peak
  
  x <- ifelse(is.na(x), -0.2, x)
  
  x <- findpeaks(x, 
    minpeakheight = threshold,
    minpeakdistance = mindist,
    nups = nups) 
  
  if(is.null(x)){
    NA
  }else{
    matrix(x, ncol = 4)[,2]
  }
}

keepRelease <- function(year, type, n = 20){
  #' @description calculate the distance between gap origin and releases
  #' @param year the vector of years for event
  #' @param type type of the event (release or gap)
  #' @param n number of years to be checked
  
  keep <- rep('yes', length(year))
  
  if(any(type %in% 'gap')){
    diffyear <- year - year[type %in% 'gap']
    keep[diffyear < n & type %in% 'release'] <- 'no'
  }
  keep
}

mdsFun <- function(ca, k = 30, bw = 5, st = 7){
  #' @description return a vector of the fited KDE function
  #' @param ca arranged vector of the canopy area values
  #' @param k a windows length, default 30
  #' @param bw a smoothing bandwidth to be used, default = 5
  #' @param st a standartization value, to scale back to canopy area
  
  rollapply( ca, 
             width = k,
             FUN = function(x){n <- length(x); density(1:n, weights = x, bw = bw, n = n)$y[round((n+1)/2)]* 100/st},
             fill = 0,
             align = "center",
             partial = TRUE)
}
