f = function(x) ((x^6/84)-(3*cos(2*x)/8))
curve(f , 0, 3);abline(h=0, v=0, lty=3)
require(rootSolve)
ceros = uniroot.all(f, c(0*0.2,1*0.2,2*0.2,3*0.2), tol = 1e-15, maxiter=10)
ceros

f1 <- function(x) (x^6/84)
f2 <- function(x) -(3*cos(2*x)/8)
plot(f1 , 0, 3);abline(h=0, v=0, lty=3)
plot(f2 , 0, 3);abline(h=0, v=0, lty=3)
try(uniroot.all(f1, c(0*0.2,1*0.2,2*0.2,3*0.2)))
try(uniroot.all(f2, c(0*0.2,1*0.2,2*0.2,3*0.2)))
##--> error: f() .. end points not of opposite sign

## where as  'extendInt="yes"'  simply first enlarges the search interval:
u1 <- uniroot(f1, c(0,3),extendInt="yes", trace=1)
u2 <- uniroot(f2, c(0,3), extendInt="yes", trace=2)
stopifnot(all.equal(u1$root, 11, tolerance = 1e-5),
          all.equal(u2$root, 12, tolerance = 6e-6))