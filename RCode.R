
library("FRACTION")
#install.packages("FRACTION")

sieveOfEratosthenes <- function(num){
  if(num < 4){
    return("Please enter a natural number that is larger than 3.")
    break
  }
  values <- rep(TRUE, num)
  values[1] <- FALSE
  prev.prime <- 2
  for(i in prev.prime:sqrt(num)){
    values[seq.int(2 * prev.prime, num, prev.prime)] <- FALSE
    prev.prime <- prev.prime + min(which(values[(prev.prime + 1) : num]))
  }
  return(which(values))
}

pollardPminusOne <- function(n, B = 4){
  if(n < 3){
    return("Please enter a value that is larger than 2.")
    break
  }
  else{
    if(n %% 2 != 0){
      a = 2
    }
    else{
      a = floor(sqrt(n))+1
    }
    m = sieveOfEratosthenes(B)[length(sieveOfEratosthenes(B))]
    if( m == B){
      primes = sieveOfEratosthenes(B)[-length(sieveOfEratosthenes(B))]
    }else{
      primes = sieveOfEratosthenes(B)
    }
    for(i in 1:length(primes)){
      p = primes[i]
      e = 1
      while(p^e < B){
        primes[i] = p^e
        e = e+1
        if(((p^e) < B) | ((p^e) == B)){
          primes[i] = p^e 
        }
      }
    }
    counterOne = 0
    counterN = 0
    for(i in 1:length(primes)){
      b = (a^primes[i]) %% n
      f = gcd((b-1),n)
      if((1 < f) & (f < n)){
        return(f)
        break
      }else if(f==1){
        counterOne = counterOne + 1
      }else if(f==n){
        counterN = counterN + 1
      }else{
        return("Failure")
        break
      }
    }
    if(counterOne == length(primes)){
      pollardPminusOne(n, B = B+1) 
    }else{
      return(f)
    }
    
  }
}


pollardPminusOne(1)
pollardPminusOne(2)
pollardPminusOne(3)
pollardPminusOne(4)
pollardPminusOne(5)
pollardPminusOne(6)
pollardPminusOne(7)
pollardPminusOne(8)
pollardPminusOne(9)
pollardPminusOne(10)
pollardPminusOne(12)
pollardPminusOne(27)
pollardPminusOne(91)
pollardPminusOne(119)
pollardPminusOne(437)
pollardPminusOne(667)
pollardPminusOne(899)

