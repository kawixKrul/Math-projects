n = readline()
n = as.integer(n)

xi <- function(i) 2*i/n

e <- function(x, i) ifelse(x > xi(i-1) && x <= xi(i), n/2*x - i + 1,
         ifelse(x > xi(i) && x < xi(i+1), -n/2*x + i + 1, 0))

de <- function(x, i) ifelse(x > xi(i-1) && x <= xi(i), n/2,
         ifelse(x > xi(i) && x < xi(i+1), -n/2, 0))

integrate <- function(f, a, b) (b-a)/2 * (f((b-a)/2*(1/sqrt(3))+(b+a)/2) + f((b-a)/2*(-1/sqrt(3)) +(b+a)/2))

dudv <- function(i, j) function(x) de(x, i)*de(x, j)

B <- function(i, j) integrate(dudv(i,j), max(0, xi(i-1), xi(j-1)), min(xi(i+1), xi(j+1))) - e(0, i)*e(0, j)

L <- function(i) -20*e(0, i)

solution <- function() {
    MB <- matrix(sapply(1:n, function(i) sapply(1:n, function(j) B(j - 1, i - 1))), nrow = n, ncol = n)

    ML <- sapply(1:n, function(i) L(i - 1))

    result = solve(MB, ML)

    return (function(x, v = result) sum(sapply(1:n, function(i) v[i] * e(x, i - 1))))
}

show_plot <- function() {
    u = solution()
    xs <- seq(0, 2, 2/n)
    ys <- mapply(u, xs)

    plot(xs, ys, main = 'solution plot', xlab = '', ylab = '', type = 'l')
}

show_plot()