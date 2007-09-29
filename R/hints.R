### Name: hints
### Title: List functions that act on an object
### Aliases: hints
### Keywords: methods

### ** Examples

m <- lm(BOD)
hints(m)
# print hints as a "LaTeX-style tagged description"
print(hints(class="glm"),style="list")
## Not run: 
##D library(xtable)
##D xtable(hints(m))
## End(Not run)



