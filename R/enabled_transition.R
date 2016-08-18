
#' @title Enabled Transition
#'
#' @description Check if a transition is currently enabled
#'
#' @param PN A Petri Net
#' @param transition A Transition
#'
#' @export enabled_transition




enabled_transition <- function(PN,transition) {
	all(pre_set(PN, transition) %in% PN$marking)
}
