aoc_1.1 <- function() {
    path <- "/Users/josephwood/Playground/AoC/2023/1/"
    input <- read.table(file.path(path, "input.txt"),
                        sep = "\n", stringsAsFactors = FALSE)
    only_digit <- gsub("[a-z]", "", input$V1)
    first <- gsub("^(\\d).*", "\\1", only_digit)
    last  <- gsub(".*(\\d)$", "\\1", only_digit)
    sum(as.integer(paste0(first, last)))
}

aoc_1.2 <- function() {
    path <- "/Users/josephwood/Playground/AoC/2023/1/"
    input <- read.table(file.path(path, "input.txt"),
                        sep = "\n", stringsAsFactors = FALSE)
    words <- c("one", "two", "three", "four", "five",
               "six", "seven", "eight", "nine")

    lst <- lapply(seq_along(words),
                  \(w) gsub(words[w], as.character(w), input$V1))

    get_idx <- function(my_pattern, lst) {
        lst_pattern <- lapply(lst, \(x) nchar(gsub(my_pattern, "\\1", x)))
        num_char <- do.call(rbind, lst_pattern)
        my_min   <- Reduce(pmin, lst_pattern)
        sapply(seq_along(my_min), \(x) which(num_char[, x] == my_min[x])[1])
    }

    word_idx_before <- get_idx("^([a-z]*?)\\d.*", lst)
    word_idx_after  <- get_idx(".*?\\d([a-z]*)$", lst)

    sum(sapply(seq_along(word_idx_after), \(x) {
        only_digit_before <- gsub("[a-z]", "", lst[[word_idx_before[x]]][x])
        first <- gsub("^(\\d).*", "\\1", only_digit_before)

        only_digit_after <- gsub("[a-z]", "", lst[[word_idx_after[x]]][x])
        last  <- gsub(".*(\\d)$", "\\1", only_digit_after)

        as.integer(paste0(first, last))
    }))
}

cat("The sum of all of the calibration values for part 1 is:",  aoc_1.1(), "\n")
cat("The sum of all of the calibration values for part 2 is:",  aoc_1.2(), "\n")
