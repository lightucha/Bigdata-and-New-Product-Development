#####################################################################################
### Project : Big Basket case study
### Script  : Case4_bigbasket.R
### Description : Big Basket product recommendation case study
#####################################################################################

#####################################################################################
### Setting up environment
#####################################################################################

# Load library  
pkgs <- c("arules", "arulesViz", "lsa")
sapply(pkgs, require, character.only = T)

# Load data
bb <- read.csv(url("http://idisk.unist.ac.kr:8081/api.link/3d_baLIAGabJQecP.csv"))

# Frame data into transaction type
t.bb <- as(split(bb[, "Description"], bb[, "Order"]), "transactions")


#####################################################################################
### Descriptive stats
#####################################################################################

crossTable(t.bb, measure = 'count', sort = T)[1:10, 1:10]
crossTable(t.bb, measure = 'lift',  sort = T)[1:10, 1:10]


#####################################################################################
### Rule mining - content based recommendation
#####################################################################################

# Binary coding
u.od <- unique(bb$Order)
u.ds <- unique(bb$Description)
bb.b <- matrix(0, length(u.od), length(u.ds))

for(i in 1:length(u.od)){
  id.r <- which(bb$Order == u.od[i])
  id.d <- match(u.ds, bb[id.r, "Description"])  
  bb.b[i, !is.na(id.d)] <- 1
}

rownames(bb.b) <- u.od
colnames(bb.b) <- as.character(u.ds)

# Cosine similarity
# Warning! This will take a few minutes to run
cos.sim <- cosine(t(bb.b))

# Identify the most similar 5 orders
identify_similar_orders <- function(inst, n){
  l       <- length(inst)
  inst.b  <- rep(1, length(u.ds))
  inst.b[is.na(match(colnames(bb.b), inst))] <- 0
  top.n   <- sort(apply(bb.b, 1, function(x) cosine(inst.b, x)), decreasing = T)[1:n]
  top.n   <- if(sum(top.n > 0) == length(top.n)) top.n else top.n[-which(top.n == 0)]
  id.s    <- !is.na(match(row.names(bb.b), names(top.n)))
  results <- bb.b[id.s, which(colSums(bb.b[id.s,, drop = F]) > 0), drop = F]
  return(results)
}

# Run: Find the most similar 5 orders with Raw Rice & Sugar
identify_similar_orders(c("Raw Rice", "Sugar"), 5)


#####################################################################################
### Rule mining - collaborative filtering
#####################################################################################

rules <- apriori(t.bb, parameter = list(support = .001, confidence = .5, 
                                        minlen = 2, target = 'rules'))

# Search rules for all
inspect(sort(rules, by = 'lift', decreasing = T)[1:100])

# Search rules for "Chips" with confidence > .8
inspect(subset(rules, subset = lhs %in% "Chips" & confidence > .8))

# Search rules for "Banana + Eggs" with confidence > .7
inspect(subset(rules, subset = lhs %ain% c("Banana", "Eggs") & confidence > .8))

# Search rules for "Baby" + "Beans" 
inspect(subset(rules, subset = lhs %pin% "Baby" & rhs %pin% "Beans"))

