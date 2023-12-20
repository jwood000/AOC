get_inputs <- function(file) {
    path  <- "/Users/josephwood/Playground/AoC/2023/9/"
    input <- data.table::fread(file.path(path, file), header = FALSE)
    res <- as.matrix(input)
    colnames(res) <- NULL
    res
}

aoc_9.1 <- function(file = "input.txt") {
    mat <- get_inputs(file)

    sum(apply(mat, 1, \(x) {
        s <- x[length(x)]

        while (any(x != 0)) {
            x <- diff(x)
            s <- s + x[length(x)]
        }

        return(s)
    }))
}

aoc_9.2 <- function(file = "input.txt") {
    mat <- get_inputs(file)

    v <- apply(mat, 1, \(x) {
        s <- x[1L]

        while (any(x != 0)) {
            x <- diff(x)
            s <- c(s, x[1L])
        }

        t <- s

        for (i in (length(s) - 1L):1) {
            t[i] <- s[i] - t[i + 1L]
        }

        return(t[1])
    })

    return(sum(v))
}

cat("part 1:",  aoc_9.1(), "\n")
cat("part 2:",  aoc_9.2(), "\n")
