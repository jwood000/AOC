aoc_3.1 <- function() {
    path  <- "/Users/josephwood/Playground/AoC/2023/3/"
    input <- data.table::fread(file.path(path, "input.txt"), header = FALSE)

    s   <- strsplit(input$V1, "")
    mat <- do.call(cbind, s)
    nr  <- nrow(mat)

    special <- grep("[[:digit:]]|\\.", mat, invert = TRUE, useBytes = TRUE)
    digits  <- grep("[[:digit:]]", mat, useBytes = TRUE)

    ## Find all possible orientations:
    ##
    ##       u1 u2 u3
    ##       u4  x u5
    ##       u6 u7 u8
    ##
    ## Assume idx is the position of x.
    ##
    ## u1 is idx - (nr + 1L)
    ## u2 is idx - nr
    ## u3 is idx - (nr - 1L)
    ## u4 is idx - 1
    ## u5 is idx + 1
    ## u6 is idx + (nr - 1L)
    ## u7 is idx + nr
    ## u8 is idx + (nr + 1L)
    ##
    ## Now we find all digits indices that intersect with the 8 cases above

    pos <- as.integer(c(-nr + (-1:1), -1, 1, nr + (-1:1)))
    spc <- lapply(pos, \(p) intersect(p + special, digits))
    all_idx <- Reduce(union, spc)

    ## Now that we have all of the digits touching special characters, we
    ## need to find all of the digits touching these digits. These will simply
    ## be digits that are +/- nr from idx.

    get_sol <- function(v) {
        sort(Reduce(
            union,
            lapply((-1:1), \(x) intersect(v + x, digits))
        ))
    }

    sol <- get_sol(all_idx)

    ## Because the maximum number of consecutive digits is 3, we need to call
    ## get_sol on itself to ensure we get that last digit.
    sol <- get_sol(sol)

    sum(vapply(seq_along(sol), \(s) {
        x <- sol[s]
        not_checked <- s == 1L || x - sol[s - 1L] > 1L

        if (sol[s + 1L] - x == 1L && not_checked) {
            t   <- sol[s:(s + 3L)] - x
            len <- rle(diff(t))$lengths[1]
            idx <- t[1:(len + 1L)] + x
            as.integer(paste(mat[idx], collapse = ""))
        } else if (not_checked) {
            as.integer(mat[x])
        } else {
            0L
        }
    }, FUN.VALUE = 1L))
}

aoc_3.2 <- function() {
    path  <- "/Users/josephwood/Playground/AoC/2023/3/"
    input <- data.table::fread(file.path(path, "input.txt"), header = FALSE)

    s   <- strsplit(input$V1, "")
    mat <- do.call(cbind, s)
    nr  <- nrow(mat)

    asterisk <- grep("\\*", mat, useBytes = TRUE)
    digits   <- grep("[[:digit:]]", mat, useBytes = TRUE)

    ## For the next part we will find all possible orientations of digits
    ## around an asterisk. Because there are 8 positions around a given
    ## position and two digits can fill any of those 8 positions, our initial
    ## starting point will have choose(8, 2) possibilities.
    ##
    ## Initial combination grid:
    ##
    ##          (i - nr - 1)   (i - nr)   (i - nr + 1)
    ##          (i - 1)         i         (i + 1)
    ##          (i + nr - 1)   (i + nr)   (i + nr + 1)
    v <- c(-nr - 1L, -nr, -nr + 1L, -1L, 1L, nr - 1L, nr, nr + 1L)
    combs <- combn(v, 2)

    ## Because we need a space in between, we can eliminate some of these
    ## checks. E.g.
    ##
    ##          u1 u2
    ##              x
    combs <- combs[, apply(combs, 2, diff) > 1]

    hits <- do.call(
        cbind,
        lapply(asterisk, \(a) {
            t <- a + combs
            t <- t[, apply(t, 2, \(x) all(x %in% digits)), drop = FALSE]
            t <- t[, !duplicated(t[1, ], fromLast = TRUE), drop = FALSE]
            t[, !duplicated(t[2, ]), drop = FALSE]
        })
    )

    get_v_from_idx <- function(idx) {
        loc <- which(abs(digits - idx) <= 2)

        if (length(loc) > 1 && diff(digits[loc])[1] == 2) {
            digits[loc[-1]]
        } else if (length(loc) > 1 && any(diff(digits[loc]) == 2)) {
            digits[loc[-length(loc)]]
        } else {
            digits[loc]
        }
    }

    get_num <- function(idx) as.integer(paste(mat[idx], collapse = ""))

    sum(apply(hits, 2, \(x) {
        n1 <- get_v_from_idx(x[1])
        n2 <- get_v_from_idx(x[2])
        if (identical(n1, n2)) 0 else get_num(n1) * get_num(n2)
    }))
}

cat("part 1:",  aoc_3.1(), "\n")
cat("part 2:",  aoc_3.2(), "\n")
