# @arguments x: an object 
#X hints()
#X hints(class="mle")
#X hints(a)

#hints <- function(x, class=base::class(x), load.libraries=FALSE) {
hints <- function(x, class=base::class(x)) {    
  all.methods <- function(classes) {
    methods <- do.call(rbind,lapply(classes, function(x) {
        m <- methods(class=x)
        t(sapply(as.vector(m), methodsplit)) #m[attr(m, "info")$visible]
    }))
    if(length(methods) > 0) rownames(methods[!duplicated(methods[,1]),])
   }

   methodsplit <- function(m) {
    parts <- strsplit(m, "\\.")[[1]]
    if (length(parts) == 1) {
        c(name=m, class="")
    } else{
        c(name=paste(parts[-length(parts)], collapse="."), class=parts[length(parts)])
    } 
   }
   
    if (missing(x) && missing(class)) {
        cat(
        "How to use the hints function:\n",
        "If you want to find out what you can do with: \n",
        "    * an object, x, use hints(x)\n",
        "    * a   class, y, use hints(class=\"y\")\n",
        sep="")
        return(invisible())
    }

#    if (load.libraries) {
#        load.package <- function(x) eval(substitute(library(x),list(x=x)))
#        capture.output(sapply(.packages(all.available=T), load.package))
#    }
    
    db <- eval(utils:::.hsearch_db())
    if (is.null(db)) {
        help.search("abcd!", rebuild=TRUE, agrep=FALSE)
        db <- eval(utils:::.hsearch_db())
    }                                               

    base <- db$Base
    alias <- db$Aliases
    key <- db$Keywords
    is.character(class)  # generates an error if class is not of type character             
    regexp <- paste("\\<", class, "\\>", sep="", collapse="|")
    extras <- help.search(regexp, package=.packages(), agrep=FALSE)$matches

    m <- all.methods(class=class)

    if(is.null(m) & dim(extras)[1] == 0) stop("No methods for this class")

    hints <- c("...Methods","Package","Task")
    if(!is.null(m)){
        m_id <- alias[match(m, alias[,1]), 2]
        keywords <- lapply(m_id, function(id) key[key[,2] %in% id, 1])
        f.names <- cbind(m, base[match(m_id, base[,3]), 4])
        f.names <- unlist(lapply(1:nrow(f.names), function(i) {
            if (is.na(f.names[i, 2])) return(f.names[i, 1])
            a <- methodsplit(f.names[i, 1])
            b <- methodsplit(f.names[i, 2])
            if (a[1] == b[1]) f.names[i, 2] else f.names[i, 1]
        }))  
        hints <- cbind(f.names, base[match(m_id, base[,3]), c(1,5)])
        hints <- hints[order(tolower(hints[,1])),]
        hints <- rbind( c("...Methods","Package","Task"), hints)
        }                                
    
    extras.new <- if(is.null(m)) extras[,1] else setdiff(setdiff(extras[, 1], m), f.names)
    extrahints <- extras[match(extras.new, extras[, 1]), c(1,3,2)]
    extrahints <- rbind( c("...Other functions","",""),extrahints)
    output <- rbind(hints,extrahints)
    output <- data.frame(output[,2:3],row.names=output[,1])
    colnames(output) <- c(" "," ")
    class(output) <- c("hints","data.frame")
    output
}

print.hints <- function(x, right=FALSE, ...){
	opt <- options(width=120)
	on.exit(options(opt))
	print.data.frame(x, right=right, ...)
	invisible()
}

  
xtable.hints <- function(x,align="lll",...) {
	xtable.data.frame(x,align=align,...)
}


