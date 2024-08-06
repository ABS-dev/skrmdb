library(data.table)
library(skrmdb)


data(titration)
titer <- data.table(titration)
titer[, log_dil := -log10(dil)]

#notice the new method of calling

results <- skrmdb.all(positive + total ~ log_dil | Vial + Operator, titer)


# Observe the results of the possible error messages
results

# response increase: is the resopnse (e.g. positive) and increasing function? (If false, )
# duplicate.dilutsions: Does the same dilution show up more than once in the test
# even.dilutions: are the dilutions evenly spaced
# monotonic: is the reponse (e.g. positive) evenly spaced)
# bracket.midpoint: If FALSE, the response does not bracket 50% and so the values cannot be trusted.


# the functions can be called one at a time, as we did before.  The old methods of feeding data
# to the functions still work, but are depreciated.

result.sk <- SpearKarb(positive + total ~ log_dil, titer[Vial == 1 & Operator == "TK"])
result.db <- DragBehr(positive + total ~ log_dil, titer[Vial == 1 & Operator == "TK"])
result.rm <- ReedMuench(positive + total ~ log_dil, titer[Vial == 1 & Operator == "TK"])
result.sk
result.db
result.rm

result.sk$ed
result.sk$eval
result.sk$messages
result.sk$var

?skrmdb.all
?skrmdb::DragBehr
?skrmdb::titration

