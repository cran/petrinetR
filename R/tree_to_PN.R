
#' @title tree_to_PN
#'
#' @description Create of petri net from a process tree.
#'
#' @param tree The process tree to be converted
#' @param name A name, which will be used to indicated the start and end transitions of the petri net.
#'
#' @export tree_to_PN




tree_to_PN <- function(tree, name) {
	if(is.null(tree$children)) {
		return(sequence_to_PN(tree$name, name))
	}
	else {
		child_nets <- list()
		for(i in 1:length(tree$children)) {
			child_nets[[i]] <- Recall(tree$children[[i]], name = tree$children[[i]]$name)
		}

		if(grepl("choice", tree$name)){
			child_nets %>%
				lapply(places) %>%
				bind_rows() %>%
				mutate(id = ifelse(grepl("start_", id), paste0("start_",tree$name), paste0("end_",tree$name))) %>%
				unique -> places
			child_nets %>%
				lapply(transitions) %>%
				bind_rows() -> transitions
			child_nets %>%
				lapply(flows) %>%
				bind_rows() %>%
				mutate(from = ifelse(grepl("start_", from), paste0("start_",tree$name), ifelse(grepl("end_", from), paste0("end_",tree$name), from)),
					   to = ifelse(grepl("start_", to), paste0("start_",tree$name), ifelse(grepl("end_", to),paste0("end_",tree$name), to))) -> flows
			return(create_PN(places, transitions, flows, paste0("start_",tree$name)))
		} else if(grepl("parallel", tree$name )) {
			child_nets %>%
				lapply(places) %>%
				bind_rows() -> places

			child_nets %>%
				lapply(transitions) %>%
				bind_rows() %>%
				bind_rows(data.frame(id = c(paste0("inv_start_", tree$name),paste0("inv_end_", tree$name)), stringsAsFactors = F)) -> transitions
			child_nets %>%
				lapply(flows) %>%
				bind_rows() %>%
				bind_rows(data.frame(from = paste0("inv_start_", tree$name), to = places$id[grepl("start_", places$id)])) %>%
				bind_rows(data.frame(from = places$id[grepl("end_", places$id)], to = paste0("inv_end_", tree$name))) %>%
				bind_rows(data.frame(from = paste0("start_",tree$name),to = paste0("inv_start_", tree$name))) %>%
				bind_rows(data.frame(from = paste0("inv_end_", tree$name),to = paste0("end_",tree$name))) -> flows

			places %>% bind_rows(data.frame(id = c(paste0("start_",tree$name),paste0("end_",tree$name)), stringsAsFactors = F)) -> places
			return(create_PN(places, transitions, flows, paste0("start_",tree$name)))


		}
		else if(grepl("sequence", tree$name)) {
			#for(i in 1:(length(child_nets))-1) {
			#	child_nets[[i]] %>% places() %>% mutate(id = gsub(tree$children[i]$name,
			#													  paste0(tree$children[i]$name,"_", paste0(tree$children[i+1]$name))))
			#}

			child_nets %>% lapply(places) %>%
				bind_rows() -> places
			child_nets %>% lapply(transitions) %>%
				bind_rows() -> transitions
			child_nets %>% lapply(flows) %>%
				bind_rows() -> flows

			for(i in 1:(length(child_nets)-1)) {
				oldname <- paste0("end_",tree$children[[i]]$name)
				newname <- paste0(tree$children[[i]]$name, "_", tree$children[[i+1]]$name)
				places %>% mutate(id = gsub(paste0("^",oldname,"$"),newname,id)) -> places
				flows %>% mutate(to = gsub(paste0("^",oldname,"$"),newname,to)) -> flows
			}
			for(i in 2:(length(child_nets))) {
				oldname <- paste0("start_",tree$children[[i]]$name)
				newname <- paste0(tree$children[[i-1]]$name, "_", tree$children[[i]]$name)
				places %>% mutate(id = gsub(paste0("^",oldname,"$"),newname,id)) -> places
				flows %>% mutate(from = gsub(paste0("^",oldname,"$"),newname,from)) -> flows
			}

			places %>% unique() -> places
			oldname <- paste0("^start_",tree$children[[1]]$name, "$")
			places %>% mutate(id = gsub(oldname, "start", id)) -> places
			flows %>% mutate(from = gsub(oldname, "start", from)) -> flows

			oldname <- paste0("^end_",tree$children[[length(tree$children)]]$name, "$")
			places %>% mutate(id = gsub(oldname, "end", id)) -> places
			flows %>% mutate(to = gsub(oldname, "end", to)) -> flows

			return(create_PN(places, transitions, flows, "start"))
		}
	}
}
