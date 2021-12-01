library(shellpipes)
library(dplyr)

commandEnvironments()


## Generate binned observations
binfun <- function(model_df, focal, bins=100, response="status", groups="services", ...) {
	
	vartype <- class(model_df[[focal]])
	
	if (vartype %in% c("numeric", "integer", "logical", "vector")) {
		check_df <- (model_df
			%>% select_at(c(groups, focal, response))
			%>% group_by_at(groups)
			%>% arrange_at(focal, .by_group = TRUE)
			%>% mutate(bin=ceiling(row_number()*bins/nrow(.)))
			%>% ungroup()
			%>% group_by_at(c(groups, "bin"))
			%>% summarise_all(mean)
			%>% ungroup()
			%>% rename(fit=all_of(response))
			%>% mutate(model="observed")
		)
	} else {
		check_df <- (model_df
			%>% select_at(c(groups, focal, response))
			%>% group_by_at(c(groups, focal))
			%>% summarise_at(response, mean)
			%>% ungroup()
			%>% rename(fit=all_of(response))
			%>% mutate(model="observed")
		)
	}

	if (focal != groups) {
		check_df <- (check_df 
			%>% mutate(services=paste0("services: ", services))
		)
	}
	attr(check_df, "vartype") <- vartype 
	return(check_df)
}

all_predictors <- colnames(sim_df_cont_joint_long)
all_predictors <- all_predictors[!all_predictors %in% c("values", "hhid", "iid")]

observed_props_list <- sapply(all_predictors, function(x){
	out <- binfun(sim_df_cont_joint_long, x, bins=150, response="values", groups="services")
	return(out)

}, simplify=FALSE)
observed_props_list

saveVars(observed_props_list)


