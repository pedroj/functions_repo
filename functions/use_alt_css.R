# Using an alternative css file for knitr.
# I do this by creating the standard output file, then dropping the 
# header and css code at the top in R:
tmp <- readLines("your.html") 
tmp <- tmp[-c(1:50)] # or however many lines it is before the css ends

write(tmp,"your.html")

# Then I use pandoc to add my own css in a standalone file
system("pandoc -s -S your.html -c your.css -o output.html")
