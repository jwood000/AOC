get_n_win <- function(input) {
    winning <- gsub("Card\\s+\\d+:\\s+", "", input$V1)
    winning <- gsub("\\s+", ",", winning)
    myguess <- gsub("\\s+", ",", input$V2)
    winning <- Map(as.integer, strsplit(winning, ","))
    myguess <- Map(as.integer, strsplit(myguess, ","))
    lengths(mapply(intersect, winning, myguess))
}

aoc_4.1 <- function(file = "input.txt") {
    path  <- "/Users/josephwood/Playground/AoC/2023/4/"
    input <- data.table::fread(file.path(path, file), header = FALSE)
    n_win <- get_n_win(input)
    sum(2^(n_win[n_win > 0] - 1L))
}

aoc_4.2 <- function(file = "input.txt") {
    path  <- "/Users/josephwood/Playground/AoC/2023/4/"
    input <- data.table::fread(file.path(path, file), header = FALSE)
    n_win <- get_n_win(input)

    v <- rep(1L, length(n_win))

    for (i in seq_along(v)[-length(v)]) {
        len <- n_win[i]
        if (len) v[(i + 1L):(i + len)] <- v[(i + 1L):(i + len)] + v[i]
    }

    sum(v)
}

cat("part 1:",  aoc_4.1(), "\n")
cat("part 2:",  aoc_4.2(), "\n")
