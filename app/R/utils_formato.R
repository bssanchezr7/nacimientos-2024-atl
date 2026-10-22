# Formato de números con la convención colombiana (igual que en el libro)
num <- function(x, acc = 1) scales::number(x, accuracy = acc, big.mark = ".", decimal.mark = ",")
pct <- function(x, acc = 0.1) paste(num(x, acc), "%")
