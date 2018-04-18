

simpson2 = function(fun, a,b, n) {
  if (n%%2 != 0) stop("En la regla de Simpson, n es par!")
  h = (b-a)/n
  i1 = seq(1, n-1, by = 2) # impares
  i2 = seq(2, n-2, by = 2) # pares
  y = fun(a+(0:n)*h) # f(a), f(a+h),...,f(a+i*h),...
  h/3 * ( fun(a) + fun(b) + 4*sum(y[i1]) + 2*sum(sum(y[i2]) ) )
}
#--- Pruebas
f = function(x) sqrt(1+cos(x)^2)


composite.simpson <- function(f, a, b, n) {
  if (is.function(f) == FALSE) {
    stop('f must be a function with one parameter (variable)')
  }
  h <- (b - a) / n
  xj <- seq.int(a, b, length.out = n + 1)
  xj <- xj[-1]
  xj <- xj[-length(xj)]
  approx <- (h / 3) * (f(a) + 2 * sum(f(xj[seq.int(2, length(xj), 2)])) + 4 * sum(f(xj[seq.int(1, length(xj), 2)])) + f(b))
  return(approx)
}

f3 <- function(x) {
  return(sqrt(1+cos(x)^2))
}

simpson2(f, 0, 2, 1000)
composite.simpson(f3, 0, 2, 30)


