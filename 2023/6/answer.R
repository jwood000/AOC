get_inputs <- function(file) {
    path  <- "/Users/josephwood/Playground/AoC/2023/6/"
    input <- readLines(file.path(path, file), warn = FALSE)

    my_time <- gsub("^Time:\\s+", "", input[1])
    my_time <- as.integer(strsplit(gsub("\\s+", ",", my_time), ",")[[1]])

    record <- gsub("^Distance:\\s+", "", input[2])
    record <- as.integer(strsplit(gsub("\\s+", ",", record), ",")[[1]])

    list(
        time = my_time,
        record = record
    )
}

aoc_6.1 <- function(file = "input.txt") {
    res <- get_inputs(file)
    prod(sapply(seq_along(res$time), \(x) {
        t <- res$time[x]
        sum((t - seq_len(t)) * seq_len(t) > res$record[x])
    }))
}

aoc_6.2 <- function(file = "input.txt") {
    res <- get_inputs(file)
    my_time <- as.numeric(paste(res$time, collapse = ""))
    record  <- as.numeric(paste(res$record, collapse = ""))

    ## (t - x) * x > r
    ##
    ## tx - x^2 = r     ===>>>     (-b +- sqrt(b^2 - 4ac)) / 2a
    ##
    ## a = -1
    ## b = t
    ## c = -r

    x_1 <- (-my_time + sqrt(my_time^2 - 4 * record)) / (-2)
    x_2 <- (-my_time - sqrt(my_time^2 - 4 * record)) / (-2)

    return(x_2 - x_1)
}

cat("part 1:",  aoc_6.1(), "\n")
cat("part 2:",  aoc_6.2(), "\n")
