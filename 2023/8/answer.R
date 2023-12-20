options(scipen = 99)

get_inputs <- function(file) {
    path  <- "/Users/josephwood/Playground/AoC/2023/8/"
    input <- readLines(file.path(path, file), warn = FALSE)

    ist <- gsub("R", "2", input[1])
    ist <- gsub("L", "1", ist)
    ist <- as.integer(strsplit(ist, "")[[1]])
    len <- length(ist)

    org <- gsub("= |\\(|\\)|,", "", input[3:length(input)])
    map <- do.call(rbind, strsplit(org, "\\s"))
    nms <- map[, 1]
    map <- map[, 2:3]
    rownames(map) <- nms

    list(
        instructions = ist,
        len = len,
        map = map
    )
}

get_idx <- function(i, len) {
    idx <- i %% len
    if (idx) idx else len
}

get_steps <- function(map, ist, len, st) {

    i <- 0L
    s <- st
    ed <- gsub("A", "Z", st)

    while (substr(s, 3, 3) != "Z") {
        i <- i + 1L
        s <- map[s, ist[get_idx(i, len)]]
    }

    return(i)
}

aoc_8.1 <- function(file = "input.txt") {
    res <- get_inputs(file)
    ist <- res$instructions
    len <- res$len
    map <- res$map
    get_steps(map, ist, len, "AAA")
}

get_lcm <- function(v) {
    fac <- RcppAlgos::primeFactorize(v)
    tbl <- unlist(lapply(fac, table))
    mxp <- vapply(split(tbl, names(tbl)), max,
                  FUN.VALUE = 1L, USE.NAMES = TRUE)
    prod(as.numeric(names(mxp))^mxp)
}

aoc_8.2 <- function(file = "input.txt") {
    res <- get_inputs(file)
    ist <- res$instructions
    len <- res$len
    map <- res$map

    st <- grep("[A-Z][A-Z]A$", rownames(map), value = TRUE)
    v  <- sapply(st, get_steps, map = map, ist = ist, len = len)
    return(get_lcm(v))
}

cat("part 1:",  aoc_8.1(), "\n")
cat("part 2:",  aoc_8.2(), "\n")
