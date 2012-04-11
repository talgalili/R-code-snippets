# install.packages("diagram")

source("http://www.r-statistics.com/wp-content/uploads/2011/11/binary.tree_.for_.binomial.game_.r.txt") # loading the function


png(filename = "binom_tree_1.png",
		width = 550, height = 480)
binary.tree.for.binomial.game(2) # creating a tree for B(2,0.5)
dev.off()


png(filename = "binom_tree_2.png",
		width = 1600, height = 480)
binary.tree.for.binomial.game(4) # creating a tree for B(4,0.5)
dev.off()


png(filename = "binom_tree_3.png",
		width = 950, height = 480)
binary.tree.for.binomial.game(3, 0.8, first_box_text = c("Tossing an unfair coin", "(3 times)"), left_branch_text = c("Failure", "Playing again"), right_branch_text = c("Success", "Playing again"),
															left_leaf_text = c("Failure", "Game ends"), right_leaf_text = c("Success",
																																															"Game ends"), cex = 0.8, rescale_radx = 1.2, rescale_rady = 1.2,
															box_color = "lightgrey", shadow_color = "darkgrey", left_arrow_text = c("Tails n(P = 0.2)"),
															right_arrow_text = c("Heads n(P = 0.8)"), distance_from_arrow = 0.04)
dev.off()

