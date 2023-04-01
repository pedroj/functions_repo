# ----------------------------------------------------------------
# tohex: Code for converting a text string to hexadecimal values 
# and output a text string.
# 3 December 2023. Sevilla. Pedro Jordano.
# ----------------------------------------------------------------
## First version 12 Jan 2022 Revised 3 December 2023
# ----------------------------------------------------------------
tohex <- function(mystring)  {
# 
  require(stringr)
  hex_raw<- str_flatten(charToRaw(mystring))
  hex_raw
}
