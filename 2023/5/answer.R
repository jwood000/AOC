Rcpp::sourceCpp("/Users/josephwood/Playground/AoC/2023/5/helper.cpp")

get_res <- function(map, id, i, n) {
    if (n == 1L && i == length(map)) {
        return(id)
    } else if (n == 0L && i == 1L) {
        return(id)
    } else {
        return(map_val(map, id, i + (-1L)^(n + 1L), as.logical(n)))
    }
}

map_val <- function(map, id, i, is_fwd = TRUE) {
    num <- as.integer(is_fwd)
    v   <- sapply(map[[i]], '[', num + 1L)
    idx <- which(v <= id)

    if (length(idx)) {
        m <- map[[i]][idx]
        m <- m[[which.max(v[idx])]]

        if (sum(m[c(num + 1L, 3L)]) > id) {
            n_id <- id + ((-1L)^num) * m[2] + ((-1L)^(num + 1L)) * m[1]
            return(get_res(map, n_id, i, num))
        }
    }

    return(get_res(map, id, i, num))
}

get_inputs <- function(file) {
    path  <- "/Users/josephwood/Playground/AoC/2023/5/"
    input <- readLines(file.path(path, file), warn = FALSE)
    input <- input[nchar(input) > 0]

    headers <- grep("^[a-z]", input)[-1]
    numbers <- mapply(\(x, y) x:y,
                      headers + 1L,
                      c(headers[-1] - 1L, length(input)))

    list(
      maps  = lapply(numbers, \(x) Map(as.numeric, strsplit(input[x], "\\s"))),
      seeds = Map(as.numeric,
                  strsplit(gsub("seeds:\\s+", "", input[1]), "\\s"))[[1]]
    )
}

aoc_5.1 <- function(file = "input.txt") {
    res <- get_inputs(file)
    min(sapply(res$seeds, map_val, map = res$maps, i = 1, is_fwd = TRUE))
}

aoc_5.2 <- function(file = "input.txt") {
    res <- get_inputs(file)
    len <- length(res$maps)
    mat <- do.call(rbind, lapply(res$maps, \(x) do.call(rbind, x)))
    min_rng <- which.min(sapply(res$maps[[len]], "[", 1))

    low  <- res$maps[[len]][[min_rng]][1]
    high <- low + res$maps[[len]][[min_rng]][3]

    seed_low  <- res$seeds[seq(1L, length(res$seeds), 2L)]
    seed_high <- seed_low + res$seeds[seq(2L, length(res$seeds), 2L)]
    s_v <- c(0L, cumsum(lengths(res$maps)))

    FindMinLoc(mat, s_v, seed_low, seed_high, low, high, len - 1L)
}

cat("part 1:",  aoc_5.1(), "\n")
cat("part 2:",  aoc_5.2(), "\n")
