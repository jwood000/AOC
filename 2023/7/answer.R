get_inputs <- function(file) {
    path  <- "/Users/josephwood/Playground/AoC/2023/7/"
    input <- data.table::fread(file.path(path, file), header = FALSE)

    hands <- gsub("", " ", input$V1)
    hands <- gsub("A", "14", hands)
    hands <- gsub("K", "13", hands)
    hands <- gsub("Q", "12", hands)
    hands <- gsub("J", "11", hands)
    hands <- trimws(gsub("T", "10", hands))

    list(
        hands = Map(as.integer, strsplit(hands, " ")),
        bid = input$V2
    )
}

aoc_7.1 <- function(file = "input.txt") {
    res <- get_inputs(file)

    ## Hands will be ranked based first off of type from 7 - 1, then
    ## every two decimal places will be used to determine strength
    ## of every card. E.g. AK4AA will be:
    ##
    ## type is 4 (three of a kind)
    ## first card is 14
    ## second card is 13
    ## third card is 4
    ## fourth card is 14
    ## fifth card is 14
    ##
    ## The result is:
    ##
    ##   041413041414

    types <- c("11111", "1112", "122", "113", "23", "14", "5")

    v <- vapply(res$hands, \(x) {
        t  <- paste(sort(table(x)), collapse = "")
        match(t, types) * 100^5 + sum(x * 100^(4:0))
    }, FUN.VALUE = 1e10)

    sum(rank(v) * res$bid)
}

aoc_7.2 <- function(file = "input.txt") {
    res <- get_inputs(file)

    ## "J" is 11
    ##
    ## Remove "J" from every hand, determine the best hand sans "J",
    ## add "J" to maximize score... re-score with "J" = 1

    types <- c("11111", "1112", "122", "113", "23", "14", "5")

    v <- vapply(res$hands, \(x) {
        t <- if (all(x == 11)) {
            "5"
        } else {
            y <- sort(table(x[x != 11]))
            y[length(y)] <- y[length(y)] + sum(x == 11)
            paste(y, collapse = "")
        }

        x[x == 11] <- 1
        match(t, types) * 100^5 + sum(x * 100^(4:0))
    }, FUN.VALUE = 1e10)

    sum(rank(v) * res$bid)
}

cat("part 1:",  aoc_7.1(), "\n")
cat("part 2:",  aoc_7.2(), "\n")
