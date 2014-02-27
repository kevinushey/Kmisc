addDebugOutput <- function(lines=Kmisc::scan.cb()) {
    lines_escaped <- gsub('(?<!\\\\)"', '\\\\"', lines, perl=TRUE)
    new <- unlist( lapply(lines, function(line) {
        c(
            line,
            paste("std::cout << \"", line, "\\n\";")
        )
    }) )
    Kmisc::cat.cb(new, sep="\n")
}
