library(rbenchmark)

n = 1e3
rho = runif(1,-1,1)



benchmark(
  
  {
    autoreg <- function(n, rho)
    {
      out <- 0
      for(t in 2:n)
      {
        error <- rnorm(1)
        error <- rho*out[t-1] + error
        out <- c(out, error)
      }
      return(out)
    }
    autoreg(n,rho)
  }
  ,
  {
    autoreg_fast <- function(no,rho)
    {
      out <- vector(length=no)
      out[1] <- 0
      error <- rnorm(n = (no-1))
      for(i in 2:no)
      {
        out[i] <- out[i-1]*rho + error[i-1]
      }
      return(out)
    }
    autoreg_fast(n,rho)
  }
  
  ,replications=1000)

