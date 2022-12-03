#' @title Overbooking tickets calculator
#'
#' @param N N variable for total seats available
#' @param gamma Probability the plane will be overbooked
#' @param p Probability of a 'show'
#'
#' @return Makes two plots for discrete and continous cases, prints lists of n tickets to be sold for both cases
#' @export
#'

ntickets <- function (N, gamma, p){
  layout(matrix(c(1,2), ncol=1))
  n <- seq(N, floor(N + N/10), by = 1)
  dis <- 1- gamma -pbinom(N,n,p)

  dmin <- which.min(abs(dis))
  min <- dis[dmin]
  nd = N+dmin -1

  plot(n,dis, pch =18, bg ="Black", type ="b",ylab="Objective", main="Objective Vs n find optimal tickets sold (Discrete)")
  abline(h=0, v=nd, col= c("Red","Red"), lwd=2)
  #---------------------------------------------------
  con <- 1- gamma -pnorm(N,n*p, sqrt(n*p*(1-p)))
  cmin <- which.min(abs(con))
  minc <- con[cmin]

  nc <-N +(N- qnorm(1-gamma, N*p, sqrt(N*p*(1-p))))

  plot(n, con, pch=18, bg="Black", type="l", ylab="Objective",  main="Objective Vs n find optimal tickets sold (Continous)")
  abline(h=0,v = nc)
  mainlist <- list(nd =nd, nc = nc, N=N, p=p, gamma=gamma)
  print(mainlist)
}
#── R CMD check results ──────────────────────── MATH4753FALLmeye0060 0.1.0 ────
#Duration: 10.6s

#0 errors ✔ | 0 warnings ✔ | 0 notes ✔

#R CMD check succeeded
