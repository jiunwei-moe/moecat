nextQuestion <- function(items, responses, theta) {
	bank <- cbind(1, questions["difficulty"], 0, 1)

	if (length(items) == 0) {
		current_item = sample(startItems(bank, nrItems=5)$items, 1)
	  	return(list(current_item = current_item,
	  		current_item_question = as.character(questions[current_item, "content"]),
	  		current_item_answer = as.numeric(questions[current_item, "correct_answer"]),
	  		theta = 0.0,
	  		sem = "N/A"))
	}

	theta <- thetaEst(bank[items,], responses)
	sem <- semTheta(theta, bank[items,])
	current_item <- nextItem(bank, theta=theta, out=items)$item

  	return(list(current_item = as.numeric(current_item),
  		current_item_question = as.character(questions[current_item, "content"]),
  		current_item_answer = as.numeric(questions[current_item, "correct_answer"]),
  		theta = theta,
  		sem = sem))
}