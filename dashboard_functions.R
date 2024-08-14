library(tidyverse)
library(fst)
library(grid)
library(ggrepel)
library(gridExtra)
library(gt)
library(gtExtras)
library(webshot2)
library(shadowtext)
keeper_stats <- read.fst("rda//keeper_stats.fst")
understat_shots <- read.fst("rda//understat_shots.fst")
fbref_shots <- read.fst("rda//fbref_shots.fst")

# Function to get the mode (most common value)
get_mode <- function(x) {
	ux <- unique(x)
	tab <- tabulate(match(x, ux))
	max_tab <- max(tab)
	modes <- ux[tab == max_tab]
	paste(modes, collapse = ",")
}

# Function to get the ammount of decimal places a number has
decimalplaces <- function(x) {
	if ((x %% 1) != 0) {
		nchar(strsplit(sub('0+$', '', as.character(x)), ".", fixed=TRUE)[[1]][[2]])
	} else {
		return(0)
	}
}

# Columns that need p.90 wrangling
p90_columns <- {c(
	"Cmp_Total", "Att_Total", "TotDist_Total", 
	"PrgDist_Total", "Cmp_Short", "Att_Short", 
	"Cmp_Medium", "Att_Medium", "Cmp_Long", 
	"Att_Long", "Ast", "xAG", "xA", "KP", 
	"Final_Third", "PPA", "CrsPA", "PrgP", "pass_attacking_threat", 
	"final_third_efficiency", "prog_passing_proportion", "WPE_total", 
	"WPE_short", "WPE_medium", "WPE_long", "Att", "Live_Pass_Types", 
	"Dead_Pass_Types", "FK_Pass_Types", "TB_Pass_Types", "Sw_Pass_Types", 
	"Crs_Pass_Types", "TI_Pass_Types", "CK_Pass_Types", "In_Corner_Kicks", 
	"Out_Corner_Kicks", "Str_Corner_Kicks", "Cmp_Outcomes", "Off_Outcomes", 
	"Blocks_Outcomes", "Tkl_Tackles", "TklW_Tackles", "Def 3rd_Tackles", 
	"Mid 3rd_Tackles", "Att 3rd_Tackles", "Tkl_Challenges", "Att_Challenges", 
	"Lost_Challenges", "Blocks_Blocks", 
	"Sh_Blocks", "Pass_Blocks", "Int", "Tkl+Int", "Clr", "Err", 
	"Touches_Touches", "Def Pen_Touches", "Def 3rd_Touches", 
	"Mid 3rd_Touches", "Att 3rd_Touches", "Att Pen_Touches", 
	"Live_Touches", "Att_Take_Ons", "Succ_Take_Ons", 
	"Tkld_Take_Ons", "Carries_Carries", 
	"TotDist_Carries", "PrgDist_Carries", "PrgC_Carries", 
	"Final_Third_Carries", "CPA_Carries", "Mis_Carries", "Dis_Carries", 
	"Rec_Receiving", "PrgR_Receiving", "prog_carrying_proportion", 
	"poss_efficiency_ratio", "carrying_threat_index", "CrdY", "CrdR", 
	"2CrdY", "Fls", "Fld", "Off", "Crs", "TklW", "PKwon", "PKcon", 
	"OG", "Recov", "Won_Aerial_Duels", "Lost_Aerial_Duels", 
	"prog_action_ratio", 
	"attacking_pass_participation"
)}

# Columns that need mean wrangling
mean_columns <- {c(
	"Cmp_percent_Total", "Cmp_percent_Short", 
	"Cmp_percent_Medium", "Cmp_percent_Long", 
	"Tkl_percent_Challenges", "Succ_percent_Take_Ons", 
	"Tkld_percent_Take_Ons", "Won_percent_Aerial_Duels",
	"SoT_Percentage"
)}

# Columns to merge by the outfield players' datasets
merge_by_columns <- c("Player", "Team", "Match_Date", "MatchURL", "Min", "First_Pos", "Team_Formation", "League")


# Function to filter and merge outfield data
filter_outfield_data <- function(mins_per_match){
	pass_stats <- read.fst("rda//pass_stats.fst")
	pass_types_stats <- read.fst("rda//pass_types_stats.fst")
	defense_stats <- read.fst("rda//defense_stats.fst")
	possession_stats <- read.fst("rda//possession_stats.fst")
	misc_stats <- read.fst("rda//misc_stats.fst")
	
	# Matching columns between outfield players' datasets
	matching_columns <- {Reduce(intersect, list(
		colnames(pass_stats),
		colnames(pass_types_stats),
		colnames(defense_stats),
		colnames(possession_stats),
		colnames(misc_stats)
	))}
	
	# Columns to remove from the outfield players' datasets before merging (to avoid duplicates)
	columns_to_filter <- setdiff(matching_columns, merge_by_columns)
	
	
	# Filter datasets
	filtered_pass_stats <- pass_stats %>%
		filter(Min >= mins_per_match) %>%
		select(-all_of(columns_to_filter))
	
	filtered_pass_types_stats <- pass_types_stats %>%
		filter(Min >= mins_per_match) %>%
		select(-all_of(columns_to_filter))
	
	filtered_defense_stats <- defense_stats %>%
		filter(Min >= mins_per_match) %>%
		select(-all_of(columns_to_filter))
	
	filtered_possession_stats <- possession_stats %>%
		filter(Min >= mins_per_match) %>%
		select(-all_of(columns_to_filter))
	
	filtered_misc_stats <- misc_stats %>%
		filter(Min >= mins_per_match) %>%
		select(-all_of(columns_to_filter), -Int)
	
	
	# Merge outfield players' stats
	outfield_ply_stats <- merge(filtered_pass_stats, filtered_pass_types_stats, by = merge_by_columns, all = T)
	outfield_ply_stats <- merge(outfield_ply_stats, filtered_defense_stats, by = merge_by_columns, all = T)
	outfield_ply_stats <- merge(outfield_ply_stats, filtered_possession_stats, by = merge_by_columns, all = T)
	outfield_ply_stats <- merge(outfield_ply_stats, filtered_misc_stats, by = merge_by_columns, all = T) %>%
		distinct(across(all_of(merge_by_columns)), .keep_all = TRUE)
	
	
	# Create new statistics for outfield players
	filtered_outfield <- outfield_ply_stats %>% mutate(PrgP = ifelse(is.na(PrgP), 0, PrgP),
																										 Carries_Carries = ifelse(is.na(Carries_Carries), 0, Carries_Carries),
																										 Att_Total = ifelse(is.na(Att_Total), 0, Att_Total),
																										 Final_Third = ifelse(is.na(Final_Third), 0, Final_Third),
																										 PPA = ifelse(is.na(PPA), 0, PPA),
																										 TB_Pass_Types = ifelse(is.na(TB_Pass_Types), 0, TB_Pass_Types),
																										 prog_action_ratio = round((PrgC_Carries + PrgP) / (Carries_Carries + Att_Total), 2),
																										 attacking_pass_participation = round((Final_Third + PPA + TB_Pass_Types) / Att_Total, 2)) %>%
		select(-ends_with(".y")) %>%
		rename_with(~gsub("\\.x$", "", .), ends_with(".x")) # Remove duplicated columns
	
	return(filtered_outfield)
}


# Function to wrangle and create datasets of outfield data
wrangle_outfield_data <- function(player_sel = NULL,
																	player_date = NULL,
																	ply_filter_positions = NULL,
																	player_positions = NULL,
																	ply_filter_formations = NULL,
																	player_formations = NULL,
																	cp_leagues = NULL,
																	cp_date = NULL,
																	cp_played_90s = NULL,
																	cp_filter_positions = NULL,
																	cp_positions = NULL,
																	cp_filter_formations = NULL,
																	cp_formations = NULL){
	# Create Player and Comp Pool outfield datasets
	player_stats <- {filtered_outfield %>%
			filter(Player == player_sel,
						 Match_Date >= player_date[1] & Match_Date <= player_date[2],
						 if (ply_filter_positions){First_Pos %in% player_positions}else{TRUE},
						 if (ply_filter_formations){Team_Formation %in% player_formations}else{TRUE}) %>%
			select(-Team_Formation, -League) %>%
			distinct(Player, Team, Match_Date, MatchURL, .keep_all = TRUE)}
	
	comp_pool_stats <- {filtered_outfield %>%
			filter(Player != player_sel,
						 League %in% cp_leagues,
						 Match_Date >= cp_date[1] & Match_Date <= cp_date[2],
						 if (cp_filter_positions){First_Pos %in% cp_positions}else{TRUE},
						 if (cp_filter_formations){Team_Formation %in% cp_formations}else{TRUE}) %>%
			select(-Team_Formation, -League) %>%
			distinct(Player, Team, Match_Date, MatchURL, .keep_all = TRUE)}
	
	
	# Create Player and Comp Pool fbref shooting datasets
	player_fb_shots <- {fbref_shots %>% 
			filter(Player %in% player_stats$Player &
						 	Date %in% player_stats$Match_Date &
						 	Squad %in% player_stats$Team,
						 MatchURL %in% player_stats$MatchURL)}
	
	comp_pool_fb_shots <- {fbref_shots %>% 
			filter(Player %in% comp_pool_stats$Player &
						 	Date %in% comp_pool_stats$Match_Date &
						 	Squad %in% comp_pool_stats$Team,
						 MatchURL %in% comp_pool_stats$MatchURL)}
	
	
	# Create Player and Comp Pool fbref SCA datasets
	player_fb_SCA <- {fbref_shots %>% 
			filter(Player_SCA_1 %in% player_stats$Player &
						 	Date %in% player_stats$Match_Date &
						 	Squad %in% player_stats$Team,
						 MatchURL %in% player_stats$MatchURL)}
	
	comp_pool_fb_SCA <- {fbref_shots %>% 
			filter(Player_SCA_1 %in% comp_pool_stats$Player &
						 	Date %in% comp_pool_stats$Match_Date &
						 	Squad %in% comp_pool_stats$Team,
						 MatchURL %in% comp_pool_stats$MatchURL)}
	
	
	# Create Player and Comp Pool fbref per match shooting stats
	player_perMatch_shots <- {player_fb_shots %>%
			select(Player, Squad, Date, MatchURL, xG, PSxG, SVA, Outcome) %>%
			group_by(Player, Squad, Date, MatchURL) %>%
			summarise(
				xG = sum(xG, na.rm = T),
				PSxG = sum(PSxG, na.rm = T),
				SVA = sum(SVA, na.rm = T),
				Shots = n(),
				SoT = sum(Outcome == "Saved" | Outcome == "Goal", na.rm = T),
				shots_blocked = sum(Outcome == "Blocked", na.rm = TRUE),
				shots_off_target = sum(Outcome == "Off Target", na.rm = TRUE),
				shots_saved = sum(Outcome == "Saved", na.rm = TRUE),
				shots_scored = sum(Outcome == "Goal", na.rm = TRUE),
				shots_woodwork = sum(Outcome == "Woodwork", na.rm = TRUE),
				shots_saved_off_target = sum(Outcome == "Saved off Target", na.rm = TRUE),
				SoT_Percentage = round((SoT/Shots) * 100, 1)
			)}
	
	comp_pool_perMatch_shots <- {comp_pool_fb_shots %>%
			select(Player, Squad, Date, MatchURL, xG, PSxG, SVA, Outcome) %>%
			group_by(Player, Squad, Date, MatchURL) %>%
			summarise(
				xG = sum(xG, na.rm = T),
				PSxG = sum(PSxG, na.rm = T),
				SVA = sum(SVA, na.rm = T),
				Shots = n(),
				SoT = sum(Outcome == "Saved" | Outcome == "Goal", na.rm = T),
				shots_blocked = sum(Outcome == "Blocked", na.rm = TRUE),
				shots_off_target = sum(Outcome == "Off Target", na.rm = TRUE),
				shots_saved = sum(Outcome == "Saved", na.rm = TRUE),
				shots_scored = sum(Outcome == "Goal", na.rm = TRUE),
				shots_woodwork = sum(Outcome == "Woodwork", na.rm = TRUE),
				shots_saved_off_target = sum(Outcome == "Saved off Target", na.rm = TRUE),
				SoT_Percentage = round((SoT/Shots) * 100, 1)
			)}
	
	
	# Create Player and Comp Pool fbref per match SCA
	player_perMatch_SCA <- {player_fb_SCA %>%
			select(Player_SCA_1, Squad, Date, MatchURL, Event_SCA_1, xG) %>%
			mutate(SCA_PassLive = case_when(Event_SCA_1 == "Pass (Live)" ~ 1, TRUE ~ 0),
						 SCA_PassDead = case_when(Event_SCA_1 == "Pass (Dead)" ~ 1, TRUE ~ 0),
						 SCA_Shot = case_when(Event_SCA_1 == "Shot" ~ 1, TRUE ~ 0),
						 SCA_Fouled = case_when(Event_SCA_1 == "Fouled" ~ 1, TRUE ~ 0),
						 SCA_TakeOn = case_when(Event_SCA_1 == "Take-On" ~ 1, TRUE ~ 0),
						 SCA_Def_Action = case_when(Event_SCA_1 == "Tackle" ~ 1,
						 													 Event_SCA_1 == "Interception" ~ 1,
						 													 TRUE ~ 0),
						 
						 SCA_xG_PassLive = case_when(Event_SCA_1 == "Pass (Live)" ~ xG, TRUE ~ 0),
						 SCA_xG_PassDead = case_when(Event_SCA_1 == "Pass (Dead)" ~ xG, TRUE ~ 0),
						 SCA_xG_Shot = case_when(Event_SCA_1 == "Shot" ~ xG, TRUE ~ 0),
						 SCA_xG_Fouled = case_when(Event_SCA_1 == "Fouled" ~ xG, TRUE ~ 0),
						 SCA_xG_TakeOn = case_when(Event_SCA_1 == "Take-On" ~ xG, TRUE ~ 0),
						 SCA_xG_Def_Action = case_when(Event_SCA_1 == "Tackle" ~ xG,
						 															Event_SCA_1 == "Interception" ~ xG,
						 															TRUE ~ 0)
			) %>%
			select(Player_SCA_1, Squad, Date, MatchURL,
						 SCA_PassLive, SCA_PassDead, SCA_Shot, SCA_Fouled, SCA_TakeOn, SCA_Def_Action, 
						 SCA_xG_PassLive, SCA_xG_PassDead, SCA_xG_Shot, SCA_xG_Fouled, SCA_xG_TakeOn, SCA_xG_Def_Action) %>%
			group_by(Player_SCA_1, Squad, Date, MatchURL) %>%
			summarise(
				across(everything(), ~ sum(.x, na.rm = T), .names = "{.col}"),
				SCA_total = sum(SCA_PassLive, SCA_PassDead, SCA_Shot, SCA_Fouled, SCA_TakeOn, SCA_Def_Action, na.rm = T),
				SCA_xG_total = sum(SCA_xG_PassLive, SCA_xG_PassDead, SCA_xG_Shot, SCA_xG_Fouled, SCA_xG_TakeOn, SCA_xG_Def_Action, na.rm = T)
			)}
	
	comp_pool_perMatch_SCA <- {comp_pool_fb_SCA %>%
			select(Player_SCA_1, Squad, Date, MatchURL, Event_SCA_1, xG) %>%
			mutate(SCA_PassLive = case_when(Event_SCA_1 == "Pass (Live)" ~ 1, TRUE ~ 0),
						 SCA_PassDead = case_when(Event_SCA_1 == "Pass (Dead)" ~ 1, TRUE ~ 0),
						 SCA_Shot = case_when(Event_SCA_1 == "Shot" ~ 1, TRUE ~ 0),
						 SCA_Fouled = case_when(Event_SCA_1 == "Fouled" ~ 1, TRUE ~ 0),
						 SCA_TakeOn = case_when(Event_SCA_1 == "Take-On" ~ 1, TRUE ~ 0),
						 SCA_Def_Action = case_when(Event_SCA_1 == "Tackle" ~ 1,
						 													 Event_SCA_1 == "Interception" ~ 1,
						 													 TRUE ~ 0),
						 
						 SCA_xG_PassLive = case_when(Event_SCA_1 == "Pass (Live)" ~ xG, TRUE ~ 0),
						 SCA_xG_PassDead = case_when(Event_SCA_1 == "Pass (Dead)" ~ xG, TRUE ~ 0),
						 SCA_xG_Shot = case_when(Event_SCA_1 == "Shot" ~ xG, TRUE ~ 0),
						 SCA_xG_Fouled = case_when(Event_SCA_1 == "Fouled" ~ xG, TRUE ~ 0),
						 SCA_xG_TakeOn = case_when(Event_SCA_1 == "Take-On" ~ xG, TRUE ~ 0),
						 SCA_xG_Def_Action = case_when(Event_SCA_1 == "Tackle" ~ xG,
						 															Event_SCA_1 == "Interception" ~ xG,
						 															TRUE ~ 0)
			) %>%
			select(Player_SCA_1, Squad, Date, MatchURL,
						 SCA_PassLive, SCA_PassDead, SCA_Shot, SCA_Fouled, SCA_TakeOn, SCA_Def_Action, 
						 SCA_xG_PassLive, SCA_xG_PassDead, SCA_xG_Shot, SCA_xG_Fouled, SCA_xG_TakeOn, SCA_xG_Def_Action) %>%
			group_by(Player_SCA_1, Squad, Date, MatchURL) %>%
			summarise(
				across(everything(), ~ sum(.x, na.rm = T), .names = "{.col}"),
				SCA_total = sum(SCA_PassLive, SCA_PassDead, SCA_Shot, SCA_Fouled, SCA_TakeOn, SCA_Def_Action, na.rm = T),
				SCA_xG_total = sum(SCA_xG_PassLive, SCA_xG_PassDead, SCA_xG_Shot, SCA_xG_Fouled, SCA_xG_TakeOn, SCA_xG_Def_Action, na.rm = T)
			)}
	
	
	# Align column names before merging
	names(player_stats)[names(player_stats) == "Team"] <- "Squad"
	names(player_stats)[names(player_stats) == "Match_Date"] <- "Date"
	
	names(comp_pool_stats)[names(comp_pool_stats) == "Team"] <- "Squad"
	names(comp_pool_stats)[names(comp_pool_stats) == "Match_Date"] <- "Date"
	
	names(player_perMatch_SCA)[names(player_perMatch_SCA) == "Player_SCA_1"] <- "Player"
	
	names(comp_pool_perMatch_SCA)[names(comp_pool_perMatch_SCA) == "Player_SCA_1"] <- "Player"
	
	# Add the shooting statistics to the outfield stats dataset
	merged_player_stats <- {merge(merge(player_stats, player_perMatch_shots,
																			by = c("Player", "Squad", "Date", "MatchURL"),
																			all = TRUE),
																player_perMatch_SCA,
																by = c("Player", "Squad", "Date", "MatchURL"),
																all = TRUE)}
	
	merged_comp_pool_stats <- {merge(merge(comp_pool_stats, comp_pool_perMatch_shots,
																				 by = c("Player", "Squad", "Date", "MatchURL"),
																				 all = TRUE),
																	 comp_pool_perMatch_SCA,
																	 by = c("Player", "Squad", "Date", "MatchURL"),
																	 all = TRUE)}
	
	
	# Shot columns to wrangle
	shot_columns <- c(setdiff(names(player_perMatch_shots), c("Player", "Squad", "Date", "MatchURL", "SoT_Percentage")),
										setdiff(names(player_perMatch_SCA), c("Player", "Squad", "Date", "MatchURL")))
	
	# List of players that played the same or more amount of 90s requested
	cp_over_minimum_90s <- {merged_comp_pool_stats %>% 
			select(Player, Squad, Min) %>%
			group_by(Player, Squad) %>%
			summarise(
				Min = sum(Min, na.rm = T)/90,
			) %>% filter(Min >= cp_played_90s)}
	
	# Wrangle players' and comp pool stats
	p90_player <- {merged_player_stats %>% 
			select(Player, Min, First_Pos,
						 all_of(p90_columns), all_of(mean_columns), all_of(shot_columns)) %>%
			group_by(Player) %>%
			summarise(
				Min = sum(Min, na.rm = T)/90,
				across(all_of(p90_columns), ~ round(sum(.x, na.rm = TRUE) / Min, 2)),
				across(all_of(mean_columns), ~ round(mean(.x, na.rm = TRUE), 1)),
				across(all_of(shot_columns), ~ round(sum(.x, na.rm = TRUE) / Min, 2))
			)}
	
	p90_comp_pool <- {merged_comp_pool_stats %>% 
			filter(Player %in% cp_over_minimum_90s$Player & Squad %in% cp_over_minimum_90s$Squad) %>%
			select(Player, Min,
						 all_of(p90_columns), all_of(mean_columns), all_of(shot_columns)) %>%
			group_by(Player) %>%
			summarise(
				Min = sum(Min, na.rm = T)/90,
				across(all_of(p90_columns), ~ round(sum(.x, na.rm = TRUE) / Min, 2)),
				across(all_of(mean_columns), ~ round(mean(.x, na.rm = TRUE), 1)),
				across(all_of(shot_columns), ~ round(sum(.x, na.rm = TRUE) / Min, 2))
			)}
	
	
	# Create player percentiles dataset
	player_percentiles <- {tibble(Statistic = setdiff(names(p90_player),
																										c("Player", "Min", "Att", "Crs_Pass_Types", "Cmp_Outcomes",
																											"Off_Outcomes", "Blocks_Outcomes", "TklW")
	)
	) %>%
			rowwise() %>% 
			mutate(p90 = p90_player[[Statistic]],
						 Average = round(mean(p90_comp_pool[[Statistic]], na.rm = T), 2),
						 Percentile = round(mean(mean(p90_comp_pool[[Statistic]] < p90_player[[Statistic]], na.rm = T),
						 												mean(p90_comp_pool[[Statistic]] <= p90_player[[Statistic]], na.rm = T)
						 ) * 100
						 ),
						 perc_value = Percentile
			)}
	
	
	# Change statistic names and order
	player_percentiles$Statistic <- {c("Passes Cmp", "Passes Attempted", "Total Passing Distance", "Prog. Passing Distance",
																		 "Short Passes Cmp", "Short Passes Attempted",
																		 "Medium Passes Cmp", "Medium Passes Attempted",
																		 "Long Passes Cmp", "Long Passes Attempted",
																		 "Assists", "xAG", "xA", "Key Passes", "Passes into Final Third", "Passes into Pen. Area", "Crosses into Pen. Area",
																		 "Prog. Passes", "Pass Att Threat", "Final Third Efficiency", "Prog. Passing Proportion",
																		 "WPE Total", "WPE Short", "WPE Medium", "WPE Long", "Live Passes", "Dead Passes", "Free Kick Passes", "Through Ball Passes",
																		 "Switch Passes", "Throw In Passes", "Corner Kick Passes", "In Corner Kicks", "Out Corner Kicks", "Straight Corner Kicks",
																		 "Tackles", "Tackles Won", "Def 3rd Tackles", "Mid 3rd Tackles", "Att 3rd Tackles", "Challenges Tackled Cmp", "Challenges Tackles Attempted",
																		 "Lost Challenges", "Blocks", "Shots Blocked", "Passes Blocked", "Interceptions", "Tackles + Int.", "Clearances", "Errors (leading to Opp Shot)",
																		 "Touches", "Def Pen. Touches", "Def 3rd Touches", "Mid 3rd Touches", "Att 3rd Touches", "Att Pen. Touches", "Live Touches", "Attempted Take Ons",
																		 "Successful Take Ons", "Tackled Take Ons", "Carries", "Total Carrying Distance", "Prog. Carrying Distance", "Prog. Carries",
																		 "Carries into Final Third", "Carries into Pen. Area", "Miscontrols", "Dispossessed", "Passes Received", "Prog. Passes Received",
																		 "Prog. Carrying Proportion", "Possession Efficiency Ratio", "Carrying Threat Index", "Yellow Cards", "Red Cards", "2nd Yellow Cards",
																		 "Fouls Made", "Fouls Drawn", "Offsides", "Crosses", "Penalties Won", "Penalties Conceded", "Own Goals", "Loose Balls Recovered",
																		 "Aerial Duels Won", "Aerial Duels Lost", "Prog. Action Ratio", "Att Pass Participation", "Passes Cmp (%)",
																		 "Short Passes Cmp (%)", "Medium Passes Cmp (%)", "Long Passes Cmp (%)", "Challenges Tackles (%)",
																		 "Successful Take Ons (%)", "Tackled Take Ons (%)", "Aerial Duels Won (%)", "Shots on Target (%)",
																		 "xG", "PSxG", "xSVA", "Shots",
																		 "Shots on Target", "Shots Blocked", "Shots Off Target", "Shots Saved", "Goals", "Shots Woodwork",
																		 "Shots Saved Off Target", "SCA Live Passes", "SCA Dead Passes", "SCA Shots", "SCA Fouled",
																		 "SCA Take Ons", "SCA Def. Actions", "SCA:xG Live Passes", "SCA:xG Dead Passes",
																		 "SCA:xG Shots", "SCA:xG Fouled", "SCA:xG Take Ons", "SCA:xG Def. Actions",
																		 "SCA", "SCA:xG")}
	
	
	# Change negative statistics' percentiles
	negative_statistics <- {c("Miscontrols", "Dispossessed", "Errors", "Tackled Take Ons", "Tackled Take Ons (%)",
														"Own Goals", "Offsides", "Shots Off Target", "Shots Blocked", "Shots Saved Off Target",
														"Shots Saved", "Shots Woodwork", "Lost Challenges", "Yellow Cards", "Red Cards", "2nd Yellow Cards",
														"Fouls Made", "Penalties Conceded", "Aerial Duels Lost")}
	player_percentiles <- player_percentiles %>%
		mutate(Percentile = case_when(Statistic %in% negative_statistics ~ 100 - Percentile,
																	TRUE ~ Percentile))
	
	
	return(list(
		player_percentiles = player_percentiles,
		merged_player_stats = merged_player_stats,
		player_fb_shots = player_fb_shots,
		comp_pool_fb_shots = comp_pool_fb_shots,
		p90_comp_pool = p90_comp_pool,
		p90_player = p90_player
	))
}


create_dashboards <- function(wrangled_datasets, selected_compared_players){
	# Separate the list of datasets
	player_percentiles <- wrangled_datasets$player_percentiles
	merged_player_stats <- wrangled_datasets$merged_player_stats
	player_fb_shots <- wrangled_datasets$player_fb_shots
	comp_pool_fb_shots <- wrangled_datasets$comp_pool_fb_shots
	p90_comp_pool <- wrangled_datasets$p90_comp_pool
	p90_player <- wrangled_datasets$p90_player
	
	
	# Separate statistics by stat type
	passing_statistics <- {c("xAG", "xA", "Assists",
													 "Passes Cmp", "Passes Attempted", "Passes Cmp (%)",
													 "Short Passes Cmp", "Short Passes Attempted", "Short Passes Cmp (%)",
													 "Medium Passes Cmp", "Medium Passes Attempted", "Medium Passes Cmp (%)",
													 "Long Passes Cmp", "Long Passes Attempted", "Long Passes Cmp (%)",
													 "Prog. Passes", "Total Passing Distance", "Prog. Passing Distance",
													 "Key Passes", "Passes into Final Third", "Passes into Pen. Area", "Crosses",
													 "Through Ball Passes")}
	
	possession_statistics <- {c("Touches", "Def Pen. Touches", "Def 3rd Touches", "Mid 3rd Touches", "Att 3rd Touches", "Att Pen. Touches","Live Touches",
															"Attempted Take Ons", "Successful Take Ons", "Successful Take Ons (%)", "Tackled Take Ons", "Tackled Take Ons (%)",
															"Carries", "Total Carrying Distance", "Prog. Carrying Distance", "Prog. Carries", "Fouls Drawn", "Penalties Won",
															"Carries into Final Third", "Carries into Pen. Area", "Miscontrols", "Dispossessed", "Offsides", "Own Goals",
															"Passes Received", "Prog. Passes Received", "Loose Balls Recovered")}
	
	defense_statistics <- {c("Tackles", "Tackles Won", "Def 3rd Tackles", "Mid 3rd Tackles", "Att 3rd Tackles",
													 "Challenges Tackled Cmp", "Challenges Tackles Attempted", "Challenges Tackles (%)",
													 "Blocks", "Shots Blocked", "Passes Blocked", "Interceptions", "Tackles + Int.", "Clearances",
													 "Aerial Duels Won", "Aerial Duels Lost", "Aerial Duels Won (%)",
													 "Errors (leading to Opp Shot)", "Fouls Made", "Penalties Conceded")}
	
	shooting_statistics <- {c("Goals", "xG", "PSxG", "xSVA", "Shots", "Shots on Target", "Shots on Target (%)", "Shots Off Target",
														"SCA", "SCA:xG", "SCA Take Ons", "SCA:xG Take Ons", "SCA Live Passes", "SCA:xG Live Passes", "SCA Def. Actions", "SCA:xG Def. Actions")}
	
	
	# Create palette using reference values
	reference_colors <- c("#af3434","#af4e4e", "#af5a5a", "#af6969", "#af7676", "#af7a7a", "#af9e9e", "#af8080",
												"#af9494", "#af9e9e", "#af9e9e", "#aaafaa", "#aaafaa", "#aaafaa", "#aaafaa", "#8aaf8a",
												"#80af80", "#8aaf8a", "#8aaf8a", "#70af70", "#69af69", "#5faf5f", "#5aaf5a", "#58af58",
												"#50af50", "#4eaf4e", "#4baf4b", "#48af48", "#46af46", "#41af41", "#3aaf3a", "#34af34")
	color_ramp <- colorRampPalette(reference_colors)
	gradient_palette <- color_ramp(101)
	
	
	
	
	# SHOOTING -----
	
	# Shooting percentiles table
	shooting_percentiles <- {player_percentiles %>%
			filter(Statistic %in% shooting_statistics) %>%
			mutate(Statistic = factor(Statistic, levels = rev(shooting_statistics)),
						 Percentile = case_when(Percentile == 100 ~ 99, TRUE ~ Percentile),
						 perc_value = Percentile) %>%
			arrange(Statistic) %>%
			mutate(p90 = if_else(grepl("%", Statistic), paste0(p90, "%"), as.character(p90))) %>%
			ggplot() +
			geom_col(aes(x = Percentile, y = paste(Statistic, paste("(", p90, ")", sep = "")),
									 fill = Percentile), width = 0.5) +
			geom_text(data = . %>% filter(Percentile != 0),
								aes(label = Percentile, x = Percentile, y = paste(Statistic, paste("(", p90, ")", sep = ""))),
								hjust = -0.3, size = rel(5.2)) +
			scale_fill_gradientn(colors = gradient_palette) +
			scale_x_continuous(limits = c(0, 105), expand = c(0, 0)) +
			theme_minimal() +
			theme(
				legend.position = "none",          # Remove color legend
				axis.title = element_blank(),    # Remove x-axis title
				axis.text.x = element_blank(),     # Remove x-axis text
				axis.ticks.x = element_blank(),     # Remove x-axis ticks
				axis.line.y = element_blank(),     # Remove y-axis line
				axis.ticks.y = element_blank(),     # Remove y-axis ticks
				panel.grid.major = element_blank(), # Remove major grid lines
				panel.grid.minor = element_blank(),  # Remove minor grid lines
				axis.text.y = element_text(size = rel(1.7))
			)}
	
	# Shooting consistency over time
	sh_consistency_plot <- {merged_player_stats %>%
			select(Date, SVA, xG) %>%
			filter(!is.na(SVA) & !is.na(xG)) %>%
			pivot_longer(cols = c(SVA, xG), names_to = "Variable", values_to = "Value") %>%
			ggplot(aes(x = Date, y = Value, color = Variable, group = Variable)) +
			geom_line(linewidth = 1) +  # Making the lines a little wider
			geom_hline(yintercept = 0, color = "red", linetype = "dotted") +  # Adding a faded red line at y=0
			annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = 0, fill = "red", alpha = 0.1) +  # Adding a faded red area below y=0
			scale_color_manual(values = c("SVA" = "black", "xG" = "#C57B57")) +  # Manually changing the colors
			theme_minimal() +
			theme(
				axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),  # Making the text in x axis vertical
				plot.title = element_text(hjust = 0.5, size = rel(1.3)),  # Centering and enlarging the title
				legend.title = element_blank(),  # Removing the legend title
				legend.position = "top",  # Positioning the legend at the top
				legend.justification = "center",  # Centering the legend
				legend.direction = "horizontal",  # Making the legend horizontal
				axis.title.x = element_blank(),  # Removing the x axis title
				axis.title.y = element_blank(),  # Removing the y axis title
				text = element_text(size = 12)  # Making the text in both axis and legend bigger
			) +
			labs(title = "Shooting Consistency Over Time")  # Adding the centered title
	}
	
	# Vertical scatterplot shooting distances
	shot_distances_plot <- {ggplot() +
			geom_point(data = player_fb_shots, aes(x = Outcome, y = Distance, color = Outcome, size = xG), alpha = 0.5) +
			scale_y_reverse(breaks = seq(0, max(player_fb_shots$Distance), by = 5)) +
			scale_x_discrete(limits = c("Goal", "Saved", "Saved off Target", "Off Target", "Blocked", "Woodwork")) +
			scale_color_manual(values = c("Goal" = "#005826", 
																		"Saved" = "#003B73", 
																		"Saved off Target" = "#538DC2", 
																		"Off Target" = "#DB162F", 
																		"Blocked" = "#8C00FF", 
																		"Woodwork" = "#C45B00")) +
			scale_size_continuous(breaks = c(0.05, 0.15, 0.25, 0.55),
														labels = c(1, 2, 3.5, 5),
														range = c(2.5, 7)) +
			geom_segment(data = {comp_pool_fb_shots %>%
					mutate(outcome_level = case_when(Outcome == "Goal" ~ 1, 
																					 Outcome == "Saved" ~ 2, 
																					 Outcome == "Saved off Target" ~ 3, 
																					 Outcome == "Off Target" ~ 4, 
																					 Outcome == "Blocked" ~ 5, 
																					 Outcome == "Woodwork" ~ 6)) %>%
					group_by(Outcome) %>%
					summarise(
						outcome_level = mean(outcome_level),
						y = mean(Distance),
						x1 = outcome_level - 0.5,
						x2 = outcome_level + 0.5
					)},
					aes(x = x1, y = y, xend = x2, yend = y, color = Outcome), linetype = "dashed") +
			theme_minimal() +
			labs(title = "Shooting Distance from Goal (in meters)") +
			theme(
				legend.position = "none",
				axis.text.x = element_text(size = 12, margin = margin(t = 2), angle = 90, vjust = 0.5, hjust = 1),
				axis.text.y = element_text(size = 12),
				axis.title.y = element_text(size = 14),
				axis.title.x = element_blank(),
				plot.title = element_text(hjust = 0.5, size = rel(1.3))
			)}
	
	# Goals scored per xG
	selected_compared_players <- c("Nicolas Jackson", "Erling Haaland", "Samuel Omorodion", "Victor Osimhen")
	goals_scored_per_xG <- {ggplot() +
			geom_hline(aes(yintercept = mean(p90_comp_pool$shots_scored, na.rm = T)), linetype = "dashed", alpha = 0.5) +
			geom_vline(aes(xintercept = mean(p90_comp_pool$xG, na.rm = T)), linetype = "dashed", alpha = 0.5) +
			geom_point(data = p90_comp_pool %>% select(shots_scored, xG) %>% filter(xG > 0), aes(x = xG, y = shots_scored), alpha = 0.25, size = 2) +
			geom_point(data = p90_player %>% select(shots_scored, xG), aes(x = xG, y = shots_scored), color = "red", size = 6, alpha = 0.25) +
			geom_point(data = p90_player %>% select(shots_scored, xG), aes(x = xG, y = shots_scored), color = "red", size = 2) +
			geom_text_repel(data = p90_comp_pool %>%
												arrange(desc(shots_scored)) %>%
												slice_head(n = 5) %>%
												bind_rows(p90_comp_pool %>% filter(Player %in% selected_compared_players)) %>%
												distinct(Player, .keep_all = TRUE) %>%
												select(Player, shots_scored, xG),
											aes(x = xG, y = shots_scored, label = Player),
											min.segment.length = 0, 
											seed = 42, 
											box.padding = 0.5,
											max.overlaps = Inf,
											nudge_x = .01,
											nudge_y = .01,
											color = "grey50") +
			theme_minimal() +
			theme(
				plot.title = element_text(hjust = 0.5, size = rel(1.3))
			) +
			labs(title = "Relationship between Goals Scored and xG (per 90)") +
			xlab("xG") + ylab("Goals Scored")}
	
	# Shot Locations
	img <- png::readPNG("images/pitch_background.png")
	shot_locations <- {understat_shots %>%
			filter(player == player_sel) %>%
			ggplot(aes(x = Y, y = X)) +
			ggpubr::background_image(img) +
			geom_point(alpha = 0.5, aes(size = xG, color = result)) +
			scale_color_manual(
				name = "Shot Result", # Add a legend title
				values = c(
					"Goal" = "#005826",
					"SavedShot" = "#003B73",
					"BlockedShot" = "#8C00FF",
					"MissedShots" = "#DB162F",
					"ShotOnPost" = "#C45B00",
					"OwnGoal" = "black"
				),
				labels = c(
					"Goal" = "Goal",
					"SavedShot" = "Saved",
					"BlockedShot" = "Blocked",
					"MissedShots" = "Off Target",
					"ShotOnPost" = "Woodwork",
					"OwnGoal" = "Own"
				)
			) +
			scale_size_continuous(breaks = c(0.05, 0.15, 0.25, 0.55),
														labels = c(1, 2, 3.5, 5),
														range = c(2.5, 7)) +
			scale_x_reverse(limits = c(0.999, 0), expand = c(0, 0)) +
			scale_y_continuous(limits = c(0.6, 0.999), expand = c(0, 0)) +
			coord_fixed(ratio = 1.745656) +
			guides(size = guide_none(), color = guide_legend(override.aes = list(size = 5))) + # Keep size guide removed
			theme_void() +
			theme(plot.title = element_text(hjust = 0.5, size = 12),
						legend.position = "bottom",
						legend.justification = "center",
						text = element_text(size = 12),
						legend.title = element_blank()
			) +
			labs(title = "Shot Locations")}
	
	# Arrange them using grid.arrange()
	shooting_dashboard <- {grid.arrange(
		textGrob(paste(player_sel, "- Shooting"), gp=gpar(fontsize=20, fontface="bold")),  # Title
		shooting_percentiles, goals_scored_per_xG, shot_locations, shot_distances_plot,
		layout_matrix = rbind(
			c(1, 1, 1),
			c(2, 3, 5), 
			c(2, 4, 5)
		),
		heights = c(0.1, 1, 1),
		widths = c(1.3, 1.1, 0.6)
	)}
	
	# Save shooting dashboard
	ggsave("images//shooting_dashboard.png", plot = shooting_dashboard, width = 22, height = 14, dpi = 300, units = "in")
	
	
	
	# PASSING -----
	
	# Passing percentiles table
	passing_percentiles <- {player_percentiles %>%
			filter(Statistic %in% passing_statistics) %>%
			mutate(Statistic = factor(Statistic, levels = rev(passing_statistics)),
						 Percentile = case_when(Percentile == 100 ~ 99, TRUE ~ Percentile),
						 perc_value = Percentile) %>%
			arrange(Statistic) %>%
			mutate(p90 = if_else(grepl("%", Statistic), paste0(p90, "%"), as.character(p90))) %>%
			ggplot() +
			geom_col(aes(x = Percentile, y = paste(Statistic, paste("(", p90, ")", sep = "")),
									 fill = Percentile), width = 0.5) +
			geom_text(data = . %>% filter(Percentile != 0),
								aes(label = Percentile, x = Percentile, y = paste(Statistic, paste("(", p90, ")", sep = ""))),
								hjust = -0.3, size = rel(5.2)) +
			scale_fill_gradientn(colors = gradient_palette) +
			scale_x_continuous(limits = c(0, 105), expand = c(0, 0)) +
			theme_minimal() +
			theme(
				legend.position = "none",          # Remove color legend
				axis.title = element_blank(),    # Remove x-axis title
				axis.text.x = element_blank(),     # Remove x-axis text
				axis.ticks.x = element_blank(),     # Remove x-axis ticks
				axis.line.y = element_blank(),     # Remove y-axis line
				axis.ticks.y = element_blank(),     # Remove y-axis ticks
				panel.grid.major = element_blank(), # Remove major grid lines
				panel.grid.minor = element_blank(),  # Remove minor grid lines
				axis.text.y = element_text(size = rel(1.7))
			)}
	
	
	# Short pass completion circular barplot
	short_pass_completion <- {data.frame(
		category = c("Success", "Missed"),
		count = c(p90_player$Cmp_percent_Short, 100 - p90_player$Cmp_percent_Short)
	) %>%
			mutate(fraction = count / 100,
						 ymax = cumsum(fraction),
						 ymin = c(0, head(ymax, n = 1)),
						 color = case_when(
						 	count >= 90 ~ "#20CE60",
						 	count < 90 & count >= 80 ~ "#66FF66",
						 	count < 80 & count >= 70 ~ "#B3FFB3",
						 	count < 70 & count >= 60 ~ "#D9F29B",
						 	count < 60 & count >= 50 ~ "#FFD966",
						 	count < 50 & count >= 40 ~ "#FFB366",
						 	count < 40 & count >= 30 ~ "#FF9966",
						 	count < 30 & count >= 20 ~ "#FF5956",
						 	count < 20 ~ "#DF3E3B"
						 ),
						 color = c(color[1], "#FFFFFF"))}
	plot_short_pass_cmp <- {ggplot(short_pass_completion, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3)) +
			geom_rect(color="transparent", aes(fill = color), linewidth=0.5, position = "identity") +
			coord_polar(theta="y") +
			scale_fill_identity() +
			xlim(c(-1, 4)) +
			theme_void() +
			theme(legend.position = "none") +
			annotate(geom = 'text', x = -0.98, y = 0,
							 label = paste0(short_pass_completion$count[1], "%", "\nShort", sep = ""), size = rel(12))}
	
	
	# Medium pass completion circular barplot
	medium_pass_completion <- {data.frame(
		category = c("Success", "Missed"),
		count = c(p90_player$Cmp_percent_Medium, 100 - p90_player$Cmp_percent_Medium)
	) %>%
			mutate(fraction = count / 100,
						 ymax = cumsum(fraction),
						 ymin = c(0, head(ymax, n = 1)),
						 color = case_when(
						 	count >= 90 ~ "#20CE60",
						 	count < 90 & count >= 80 ~ "#66FF66",
						 	count < 80 & count >= 70 ~ "#B3FFB3",
						 	count < 70 & count >= 60 ~ "#D9F29B",
						 	count < 60 & count >= 50 ~ "#FFD966",
						 	count < 50 & count >= 40 ~ "#FFB366",
						 	count < 40 & count >= 30 ~ "#FF9966",
						 	count < 30 & count >= 20 ~ "#FF5956",
						 	count < 20 ~ "#DF3E3B"
						 ),
						 color = c(color[1], "#FFFFFF"))}
	plot_medium_pass_cmp <- {ggplot(medium_pass_completion, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3)) +
			geom_rect(color="transparent", aes(fill = color), linewidth=0.5, position = "identity") +
			coord_polar(theta="y") +
			scale_fill_identity() +
			xlim(c(-1, 4)) +
			theme_void() +
			theme(legend.position = "none") +
			annotate(geom = 'text', x = -0.98, y = 0,
							 label = paste0(medium_pass_completion$count[1], "%", "\nMedium", sep = ""), size = rel(12))}
	
	
	# Short pass completion circular barplot
	long_pass_completion <- {data.frame(
		category = c("Success", "Missed"),
		count = c(p90_player$Cmp_percent_Long, 100 - p90_player$Cmp_percent_Long)
	) %>%
			mutate(fraction = count / 100,
						 ymax = cumsum(fraction),
						 ymin = c(0, head(ymax, n = 1)),
						 color = case_when(
						 	count >= 90 ~ "#20CE60",
						 	count < 90 & count >= 80 ~ "#66FF66",
						 	count < 80 & count >= 70 ~ "#B3FFB3",
						 	count < 70 & count >= 60 ~ "#D9F29B",
						 	count < 60 & count >= 50 ~ "#FFD966",
						 	count < 50 & count >= 40 ~ "#FFB366",
						 	count < 40 & count >= 30 ~ "#FF9966",
						 	count < 30 & count >= 20 ~ "#FF5956",
						 	count < 20 ~ "#DF3E3B"
						 ),
						 color = c(color[1], "#FFFFFF"))}
	plot_long_pass_cmp <- {ggplot(long_pass_completion, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3)) +
			geom_rect(color="transparent", aes(fill = color), linewidth=0.5, position = "identity") +
			coord_polar(theta="y") +
			scale_fill_identity() +
			xlim(c(-1, 4)) +
			theme_void() +
			theme(legend.position = "none") +
			annotate(geom = 'text', x = -0.98, y = 0,
							 label = paste0(long_pass_completion$count[1], "%", "\nLong", sep = ""), size = rel(12))}
	
	
	# Progressive distance per Progressive passes (scatterplot, p90)
	prog_distance_per_passes <- {ggplot() +
			geom_hline(aes(yintercept = mean(p90_comp_pool$PrgP, na.rm = T)), linetype = "dashed", alpha = 0.5) +
			geom_vline(aes(xintercept = mean(p90_comp_pool$PrgDist_Total, na.rm = T)), linetype = "dashed", alpha = 0.5) +
			geom_point(data = p90_comp_pool %>% select(PrgP, PrgDist_Total) %>% filter(PrgDist_Total > 0), aes(x = PrgDist_Total, y = PrgP), alpha = 0.25, size = 2) +
			geom_point(data = p90_player %>% select(PrgP, PrgDist_Total), aes(x = PrgDist_Total, y = PrgP), color = "red", size = 6, alpha = 0.25) +
			geom_point(data = p90_player %>% select(PrgP, PrgDist_Total), aes(x = PrgDist_Total, y = PrgP), color = "red", size = 2) +
			geom_text_repel(data = p90_comp_pool %>%
												arrange(desc(PrgP)) %>%
												slice_head(n = 5) %>%
												bind_rows(p90_comp_pool %>% filter(Player %in% selected_compared_players)) %>%
												distinct(Player, .keep_all = TRUE) %>%
												select(Player, PrgP, PrgDist_Total),
											aes(x = PrgDist_Total, y = PrgP, label = Player),
											min.segment.length = 0, 
											seed = 42, 
											box.padding = 0.5,
											max.overlaps = Inf,
											nudge_x = .01,
											nudge_y = .01,
											color = "grey50") +
			theme_minimal() +
			theme(
				plot.title = element_text(hjust = 0.5, size = rel(1.3))
			) +
			labs(title = "Prog. Passes and Distance (per 90)") +
			xlab("Progressive Distance") + ylab("Progressive Passes")}
	
	
	# Weighted Pass EFficiency vertical barplot
	WPE_data <- {player_percentiles %>%
			filter(Statistic %in% c("WPE Total", "WPE Short", "WPE Medium", "WPE Long")) %>%
			mutate(Statistic = sub("WPE ", "", Statistic))}
	WPE_plot <- {
		WPE_data %>%
			ggplot(aes(x = p90, y = Statistic)) +
			geom_col(fill = "#076fa2", width = 0.3) +
			scale_x_continuous(
				limits = c(0, 4),
				breaks = seq(0, 4, by = 1), 
				expand = c(0, 0.01)
			) +
			scale_y_discrete(limits = c("Long", "Medium", "Short", "Total")) +
			geom_segment(
				data = WPE_data %>%
					mutate(y_level = factor(Statistic, levels = c("Long", "Medium", "Short", "Total")),
								 y1 = as.numeric(y_level) - 0.25, yend = as.numeric(y_level) + 0.25),
				aes(x = Average, y = y1, xend = Average, yend = yend), 
				linetype = "dashed"
			) +
			geom_shadowtext(
				aes(x = p90, y = Statistic, label = p90),
				hjust = -0.3,
				colour = "#076fa2",
				bg.colour = "white",
				bg.r = 0.2,
				size = rel(5)
			) +
			labs(title = "Weighted Pass Efficiency") +
			theme(
				panel.background = element_rect(fill = "white"),
				panel.grid.major.x = element_line(color = "#A8BAC4", linewidth = 0.3),
				axis.ticks.length = unit(0, "mm"),
				axis.title = element_blank(),
				axis.text = element_text(size = rel(1.3)),
				plot.title = element_text(hjust = 0.5, size = rel(1.3))
			)
	}
	
	
	# Arrange them using grid.arrange()
	passing_dashboard <- {grid.arrange(
		textGrob(paste(player_sel, "- Passing"), gp=gpar(fontsize=20, fontface="bold")),  # Title
		passing_percentiles, arrangeGrob(plot_short_pass_cmp, plot_medium_pass_cmp, plot_long_pass_cmp, ncol = 3),
		prog_distance_per_passes, WPE_plot,
		layout_matrix = rbind(
			c(1, 1, 1),
			c(2, 3, 3), 
			c(2, 4, 5)
		),
		heights = c(0.1, 1, 1.9),
		widths = c(1.2, 0.90, 0.90)
	)}
	
	# Save passing dashboard
	ggsave("images//passing_dashboard.png", plot = passing_dashboard, width = 22, height = 14, dpi = 300, units = "in")
	
	
	# POSSESSION -------------------------------------------------------------------
	# Passing percentiles table
	possession_percentiles <- {player_percentiles %>%
			filter(Statistic %in% possession_statistics) %>%
			mutate(Statistic = factor(Statistic, levels = rev(possession_statistics)),
						 Percentile = case_when(Percentile == 100 ~ 99, TRUE ~ Percentile),
						 perc_value = Percentile) %>%
			arrange(Statistic) %>%
			mutate(p90 = if_else(grepl("%", Statistic), paste0(p90, "%"), as.character(p90))) %>%
			ggplot() +
			geom_col(aes(x = Percentile, y = paste(Statistic, paste("(", p90, ")", sep = "")),
									 fill = Percentile), width = 0.5) +
			geom_text(data = . %>% filter(Percentile != 0),
								aes(label = Percentile, x = Percentile, y = paste(Statistic, paste("(", p90, ")", sep = ""))),
								hjust = -0.3, size = rel(5.2)) +
			scale_fill_gradientn(colors = gradient_palette) +
			scale_x_continuous(limits = c(0, 105), expand = c(0, 0)) +
			theme_minimal() +
			theme(
				legend.position = "none",          # Remove color legend
				axis.title = element_blank(),    # Remove x-axis title
				axis.text.x = element_blank(),     # Remove x-axis text
				axis.ticks.x = element_blank(),     # Remove x-axis ticks
				axis.line.y = element_blank(),     # Remove y-axis line
				axis.ticks.y = element_blank(),     # Remove y-axis ticks
				panel.grid.major = element_blank(), # Remove major grid lines
				panel.grid.minor = element_blank(),  # Remove minor grid lines
				axis.text.y = element_text(size = rel(1.7))
			)}
	
	
	# Touches per zone, waffle plot
	touches_data <- {as.data.frame(t(c(p90_player$`Def 3rd_Touches`, p90_player$`Def Pen_Touches`,
																		 p90_player$`Mid 3rd_Touches`, p90_player$`Att 3rd_Touches`,
																		 p90_player$`Att Pen_Touches`)))}
	colnames(touches_data) <- c("Def 3rd", "Def Pen", "Mid 3rd", "Att 3rd", "Att Pen")
	touches_vector <- c(touches_data$`Def Pen`, touches_data$`Def 3rd`, touches_data$`Mid 3rd`, touches_data$`Att 3rd`, touches_data$`Att Pen`)
	names(touches_vector) <- c("Def Pen", "Def 3rd", "Mid 3rd", "Att 3rd", "Att Pen")
	touches_waffle_plot <- {waffle(touches_vector,
																 rows = 5,
																 colors = c("#d95f02", "#1b9e77", "#7570b3", "#e7298a", "#66a61e"),
																 legend_pos = "bottom",
																 keep = T) +
			ggtitle("Touches in Different Zones of the Football Pitch") +
			theme(plot.title = element_text(size = rel(1.3), hjust = 0.5))}
	
	
	# Successful take ons, circular barplot
	succ_take_ons <- {data.frame(
		category = c("Success", "Missed"),
		count = c(p90_player$Succ_percent_Take_Ons, 100 - p90_player$Succ_percent_Take_Ons)
	) %>%
			mutate(fraction = count / 100,
						 ymax = cumsum(fraction),
						 ymin = c(0, head(ymax, n = 1)),
						 color = case_when(
						 	count >= 90 ~ "#20CE60",
						 	count < 90 & count >= 80 ~ "#66FF66",
						 	count < 80 & count >= 70 ~ "#B3FFB3",
						 	count < 70 & count >= 60 ~ "#D9F29B",
						 	count < 60 & count >= 50 ~ "#FFD966",
						 	count < 50 & count >= 40 ~ "#FFB366",
						 	count < 40 & count >= 30 ~ "#FF9966",
						 	count < 30 & count >= 20 ~ "#FF5956",
						 	count < 20 ~ "#DF3E3B"
						 ),
						 color = c(color[1], "#FFFFFF"))}
	plot_succ_take_ons <- {ggplot(succ_take_ons, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3)) +
			geom_rect(color="transparent", aes(fill = color), linewidth=0.5, position = "identity") +
			coord_polar(theta="y") +
			scale_fill_identity() +
			xlim(c(-1, 4)) +
			theme_void() +
			theme(legend.position = "none") +
			annotate(geom = 'text', x = -0.98, y = 0,
							 label = paste0(succ_take_ons$count[1], "%", "\nTake-Ons", sep = ""), size = rel(12))}
	
	
	# Progressive distance per Progressive carries (scatterplot, p90)
	prog_distance_per_carries <- {ggplot() +
			geom_hline(aes(yintercept = mean(p90_comp_pool$PrgC_Carries, na.rm = T)), linetype = "dashed", alpha = 0.5) +
			geom_vline(aes(xintercept = mean(p90_comp_pool$PrgDist_Carries, na.rm = T)), linetype = "dashed", alpha = 0.5) +
			geom_point(data = p90_comp_pool %>% select(PrgC_Carries, PrgDist_Carries) %>% filter(PrgDist_Carries > 0), aes(x = PrgDist_Carries, y = PrgC_Carries), alpha = 0.25, size = 2) +
			geom_point(data = p90_player %>% select(PrgC_Carries, PrgDist_Carries), aes(x = PrgDist_Carries, y = PrgC_Carries), color = "red", size = 6, alpha = 0.25) +
			geom_point(data = p90_player %>% select(PrgC_Carries, PrgDist_Carries), aes(x = PrgDist_Carries, y = PrgC_Carries), color = "red", size = 2) +
			geom_text_repel(data = p90_comp_pool %>%
												arrange(desc(PrgC_Carries)) %>%
												slice_head(n = 5) %>%
												bind_rows(p90_comp_pool %>% filter(Player %in% selected_compared_players)) %>%
												distinct(Player, .keep_all = TRUE) %>%
												select(Player, PrgC_Carries, PrgDist_Carries),
											aes(x = PrgDist_Carries, y = PrgC_Carries, label = Player),
											min.segment.length = 0, 
											seed = 42, 
											box.padding = 0.5,
											max.overlaps = Inf,
											nudge_x = .01,
											nudge_y = .01,
											color = "grey50") +
			theme_minimal() +
			theme(
				plot.title = element_text(hjust = 0.5, size = rel(1.3))
			) +
			labs(title = "Prog. Carries and Distance (per 90)") +
			xlab("Progressive Distance") + ylab("Progressive Carries")}
	
	
	# Arrange them using grid.arrange()
	possession_dashboard <- {grid.arrange(
		textGrob(paste(player_sel, "- Possession"), gp=gpar(fontsize=20, fontface="bold")),  # Title
		possession_percentiles, prog_distance_per_carries, plot_succ_take_ons, touches_waffle_plot,
		layout_matrix = rbind(
			c(1, 1, 1),
			c(2, 3, 3), 
			c(2, 4, 5)
		),
		heights = c(0.1, 1, 1),
		widths = c(1.3, 0.85, 0.85)
	)}
	
	# Save possession dashboard
	ggsave("images//possession_dashboard.png", plot = possession_dashboard, width = 22, height = 14, dpi = 300, units = "in")
	
	
	# DEFENSE ----------------------------------------------------------------------
	# Passing percentiles table
	defense_percentiles <- {player_percentiles %>%
			filter(Statistic %in% defense_statistics) %>%
			mutate(Statistic = factor(Statistic, levels = rev(defense_statistics)),
						 Percentile = case_when(Percentile == 100 ~ 99, TRUE ~ Percentile),
						 perc_value = Percentile) %>%
			arrange(Statistic) %>%
			mutate(p90 = if_else(grepl("%", Statistic), paste0(p90, "%"), as.character(p90))) %>%
			ggplot() +
			geom_col(aes(x = Percentile, y = paste(Statistic, paste("(", p90, ")", sep = "")),
									 fill = Percentile), width = 0.5) +
			geom_text(data = . %>% filter(Percentile != 0),
								aes(label = Percentile, x = Percentile, y = paste(Statistic, paste("(", p90, ")", sep = ""))),
								hjust = -0.3, size = rel(5.2)) +
			scale_fill_gradientn(colors = gradient_palette) +
			scale_x_continuous(limits = c(0, 105), expand = c(0, 0)) +
			theme_minimal() +
			theme(
				legend.position = "none",          # Remove color legend
				axis.title = element_blank(),    # Remove x-axis title
				axis.text.x = element_blank(),     # Remove x-axis text
				axis.ticks.x = element_blank(),     # Remove x-axis ticks
				axis.line.y = element_blank(),     # Remove y-axis line
				axis.ticks.y = element_blank(),     # Remove y-axis ticks
				panel.grid.major = element_blank(), # Remove major grid lines
				panel.grid.minor = element_blank(),  # Remove minor grid lines
				axis.text.y = element_text(size = rel(1.7))
			)}
	
	
	# Tackles per zone, vertical barplot
	tackles_per_zone_plot <- {as_tibble(data.frame(
		Zone = c("Def 3rd", "Mid 3rd", "Att 3rd"),
		Tackles = c(p90_player$`Def 3rd_Tackles`, p90_player$`Mid 3rd_Tackles`, p90_player$`Att 3rd_Tackles`),
		Average = c(mean(p90_comp_pool$`Def 3rd_Tackles`, na.rm = T),
								mean(p90_comp_pool$`Mid 3rd_Tackles`, na.rm = T),
								mean(p90_comp_pool$`Att 3rd_Tackles`, na.rm = T)),
		x1 = c(0.5, 1.5, 2.5),
		x2 = c(1.5, 2.5, 3.5)
	)) %>%
			ggplot(aes(x = Zone, y = Tackles)) +
			scale_x_discrete(limits = c("Def 3rd", "Mid 3rd", "Att 3rd")) +
			scale_fill_manual(values = c("Def 3rd" = "#1b9e77",
																	 "Mid 3rd" = "#7570b3",
																	 "Att 3rd" = "#e7298a")) + 
			geom_col(aes(fill = Zone), width = 0.3) +
			geom_segment(aes(x = x1, y = Average, xend = x2, yend = Average, color = Zone), linetype = "dashed") +
			theme_minimal() +
			labs(title = "Tackles in Different Zones of the Football Pitch") +
			theme(
				legend.position = "none",
				axis.text.x = element_text(size = 12),
				axis.text.y = element_text(size = 12),
				axis.title.y = element_text(size = 14),
				axis.title.x = element_blank(),
				plot.title = element_text(hjust = 0.5, size = rel(1.3))
			)}
	
	
	# Successful tackled challenges, circular barplot
	succ_tackled_challenges <- {data.frame(
		category = c("Success", "Missed"),
		count = c(p90_player$Tkl_percent_Challenges, 100 - p90_player$Tkl_percent_Challenges)
	) %>%
			mutate(fraction = count / 100,
						 ymax = cumsum(fraction),
						 ymin = c(0, head(ymax, n = 1)),
						 color = case_when(
						 	count >= 90 ~ "#20CE60",
						 	count < 90 & count >= 80 ~ "#66FF66",
						 	count < 80 & count >= 70 ~ "#B3FFB3",
						 	count < 70 & count >= 60 ~ "#D9F29B",
						 	count < 60 & count >= 50 ~ "#FFD966",
						 	count < 50 & count >= 40 ~ "#FFB366",
						 	count < 40 & count >= 30 ~ "#FF9966",
						 	count < 30 & count >= 20 ~ "#FF5956",
						 	count < 20 ~ "#DF3E3B"
						 ),
						 color = c(color[1], "#FFFFFF"))}
	plot_succ_tackled_challenges <- {ggplot(succ_tackled_challenges, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3)) +
			geom_rect(color="transparent", aes(fill = color), linewidth=0.5, position = "identity") +
			coord_polar(theta="y") +
			scale_fill_identity() +
			xlim(c(-1, 4)) +
			theme_void() +
			theme(legend.position = "none") +
			annotate(geom = 'text', x = -0.98, y = 0,
							 label = paste0(succ_tackled_challenges$count[1], "%", "\nChallenges", sep = ""), size = rel(12))}
	
	
	# Arrange them using grid.arrange()
	defense_dashboard <- {grid.arrange(
		textGrob(paste(player_sel, "- Defense"), gp=gpar(fontsize=20, fontface="bold")),  # Title
		defense_percentiles, tackles_per_zone_plot, plot_succ_tackled_challenges,
		layout_matrix = rbind(
			c(1, 1),
			c(2, 3), 
			c(2, 4)
		),
		heights = c(0.1, 1, 1),
		widths = c(1, 1)
	)}
	
	# Save possession dashboard
	ggsave("images//defense_dashboard.png", plot = defense_dashboard, width = 22, height = 14, dpi = 300, units = "in")
}