library(fst)
library(worldfootballR)
library(tidyverse)


load_keepers_data <- function(){
	keeper_stats <- {
		load_fb_advanced_match_stats(
			country = c("ITA", "ESP", "GER", "ENG", "FRA"),
			gender = "M",
			tier = "1st",
			season_end_year = 2024,
			stat_type = "keeper",
			team_or_player = "player"
		)  %>% select(-Tkl_Tackles, -TklW_Tackles, -Tkl_Challenges, -`Tkl+Int`, -Tkl_percent_Challenges,
									-`Def 3rd_Tackles`, -`Mid 3rd_Tackles`, -`Att 3rd_Tackles`, -Att_Challenges,
									-Lost_Challenges, -Blocks_Blocks, -Sh_Blocks, -Pass_Blocks, -Int, -Clr, -Err)
	}
	write.fst(keeper_stats, "rda//keeper_stats.fst")
}

load_fbref_shots <- function(){
	fbref_shots <- {
		load_fb_match_shooting(
			country = c("ITA", "ESP", "GER", "ENG", "FRA"),
			gender = "M",
			tier = "1st",
			season_end_year = 2024
		) %>%
			mutate(xG = as.numeric(xG),
						 PSxG = as.numeric(PSxG),
						 SVA = case_when(is.na(PSxG) ~ 0 - xG,
						 								TRUE ~ PSxG - xG),
						 Half = case_when(
						 	as.numeric(str_extract(Minute, "^[0-9]+")) <= 45 ~ "first",
						 	as.numeric(str_extract(Minute, "^[0-9]+")) > 45 & as.numeric(str_extract(Minute, "^[0-9]+")) <= 90 ~ "second",
						 	as.numeric(str_extract(Minute, "^[0-9]+")) > 90 ~ "et"
						 ),
						 Minute = str_replace_all(Minute, "\\+", " + ") %>%
						 	purrr::map_dbl(~ eval(parse(text = .))),
						 Distance = as.numeric(Distance)*0.9144)
	} 
	write.fst(fbref_shots, "rda//fbref_shots.fst")
}

load_shots_understat <- function(){
	premier_league_shots <- load_understat_league_shots("EPL") %>% mutate(
		lastAction = case_when(is.na(lastAction) ~ last_action,
													 !is.na(lastAction) ~ lastAction),
		
		xG = case_when(is.na(xG) ~ x_g,
									 !is.na(xG) ~ xG),
		
		X = case_when(is.na(X) ~ x,
									!is.na(X) ~ X),
		
		Y = case_when(is.na(Y) ~ y,
									!is.na(Y) ~ Y),
		
		shotType = case_when(is.na(shotType) ~ shot_type,
												 !is.na(shotType) ~ shotType),
	) %>% select(-last_action, -x_g, -x, -y, -shot_type)
	
	laliga_shots <- load_understat_league_shots("La liga")
	
	bundesliga_shots <- load_understat_league_shots("Bundesliga")
	
	seriea_shots <- load_understat_league_shots("Serie A")
	
	ligue1_shots <- load_understat_league_shots("Ligue 1")
	
	
	# Add column for team and rival
	understat_shots <- understat_shots %>% mutate(team = case_when(h_a == "h" ~ home_team,
																																 h_a == "a" ~ away_team,
																																 is.na(h_a) & home_away == "h" ~ home_team,
																																 is.na(h_a) & home_away == "a" ~ away_team,
																																 is.na(h_a) & is.na(home_away) ~ "neutral_venue"),
																								rival_team = case_when(h_a == "h" ~ away_team,
																																			 h_a == "a" ~ home_team,
																																			 is.na(h_a) & home_away == "h" ~ away_team,
																																			 is.na(h_a) & home_away == "a" ~ home_team,
																																			 is.na(h_a) & is.na(home_away) ~ "neutral_venue"))
	
	
	understat_shots <- understat_shots %>%
		separate(date, into = c("date", "hour"), sep = " ")
	
	
	understat_shots$date <- as.Date(understat_shots$date)
	understat_shots$hour <- format(strptime(understat_shots$hour, format="%H:%M:%S"), "%H:%M:%S")
	
	
	understat_shots <- bind_rows(
		premier_league_shots,
		laliga_shots,
		bundesliga_shots,
		seriea_shots,
		ligue1_shots
	)
	write.fst(understat_shots, "rda//understat_shots.fst")
}

load_outfield_players_data <- function(){
	pass_stats <- {load_fb_advanced_match_stats(
		country = c("ITA", "ESP", "GER", "ENG", "FRA"),
		gender = "M",
		tier = "1st",
		season_end_year = 2024,
		stat_type = "passing",
		team_or_player = "player"
	) %>% select(-Tkl_Tackles, -TklW_Tackles, -Tkl_Challenges, -`Tkl+Int`, -Tkl_percent_Challenges,
							 -`Def 3rd_Tackles`, -`Mid 3rd_Tackles`, -`Att 3rd_Tackles`, -Att_Challenges,
							 -Lost_Challenges, -Blocks_Blocks, -Sh_Blocks, -Pass_Blocks, -Int, -Clr, -Err) %>%
			separate(Pos, into = c("First_Pos", "Second_Pos"), sep = ",", extra = "drop", fill = "right") %>%
			mutate(pass_attacking_threat = case_when(Final_Third > 0 ~ round(xAG / Final_Third, 2),
																							 TRUE ~ 0),
						 final_third_efficiency = case_when(Final_Third > 0 ~ round(KP / Final_Third, 2),
						 																	 TRUE ~ 0),
						 prog_passing_proportion = round(PrgDist_Total/TotDist_Total, 2),
						 WPE_total = case_when(Cmp_Total > 0 ~ round((Cmp_Total / Att_Total) * log(Att_Total + 1), 2),
						 											TRUE ~ 0),
						 WPE_short = case_when(Cmp_Short > 0 ~ round((Cmp_Short / Att_Short) * log(Att_Short + 1), 2),
						 											TRUE ~ 0),
						 WPE_medium = case_when(Cmp_Medium > 0 ~ round((Cmp_Medium / Att_Medium) * log(Att_Medium + 1), 2),
						 											 TRUE ~ 0),
						 WPE_long = case_when(Cmp_Long > 0 ~ round((Cmp_Long / Att_Long) * log(Att_Long + 1), 2),
						 										 TRUE ~ 0),
						 Rival = case_when(Home_Away == "Home" ~ Away_Team,
						 									Home_Away == "Away" ~ Home_Team,
						 									TRUE ~ NA),
						 Team_Formation = case_when(Home_Away == "Home" ~ Home_Formation,
						 													 Home_Away == "Away" ~ Away_Formation,
						 													 TRUE ~ NA))}
	
	pass_types_stats <- {load_fb_advanced_match_stats(
		country = c("ITA", "ESP", "GER", "ENG", "FRA"),
		gender = "M",
		tier = "1st",
		season_end_year = 2024,
		stat_type = "passing_types",
		team_or_player = "player"
	) %>% select(-Tkl_Tackles, -TklW_Tackles, -Tkl_Challenges, -`Tkl+Int`, -Tkl_percent_Challenges,
							 -`Def 3rd_Tackles`, -`Mid 3rd_Tackles`, -`Att 3rd_Tackles`, -Att_Challenges,
							 -Lost_Challenges, -Blocks_Blocks, -Sh_Blocks, -Pass_Blocks, -Int, -Clr, -Err) %>%
			separate(Pos, into = c("First_Pos", "Second_Pos"), sep = ",", extra = "drop", fill = "right") %>%
			mutate(Rival = case_when(Home_Away == "Home" ~ Away_Team,
						 									Home_Away == "Away" ~ Home_Team,
						 									TRUE ~ NA),
						 Team_Formation = case_when(Home_Away == "Home" ~ Home_Formation,
						 													 Home_Away == "Away" ~ Away_Formation,
						 													 TRUE ~ NA))}
	
	defense_stats <- {
		load_fb_advanced_match_stats(
			country = c("ITA", "ESP", "GER", "ENG", "FRA"),
			gender = "M",
			tier = "1st",
			season_end_year = 2024,
			stat_type = "defense",
			team_or_player = "player"
		) %>%
			separate(Pos, into = c("First_Pos", "Second_Pos"), sep = ",", extra = "drop", fill = "right") %>%
			mutate(Rival = case_when(Home_Away == "Home" ~ Away_Team,
						 									Home_Away == "Away" ~ Home_Team,
						 									TRUE ~ NA),
						 Team_Formation = case_when(Home_Away == "Home" ~ Home_Formation,
						 													 Home_Away == "Away" ~ Away_Formation,
						 													 TRUE ~ NA))
	}
	
	possession_stats <- {
		load_fb_advanced_match_stats(
			country = c("ITA", "ESP", "GER", "ENG", "FRA"),
			gender = "M",
			tier = "1st",
			season_end_year = 2024,
			stat_type = "possession",
			team_or_player = "player"
		) %>%
			separate(Pos, into = c("First_Pos", "Second_Pos"), sep = ",", extra = "drop", fill = "right") %>%
			mutate(Mis_Carries = ifelse(is.na(Mis_Carries), 0, Mis_Carries),
						 Dis_Carries = ifelse(is.na(Dis_Carries), 0, Dis_Carries),
						 PrgC_Carries = ifelse(is.na(PrgC_Carries), 0, PrgC_Carries),
						 Final_Third_Carries = ifelse(is.na(Final_Third_Carries), 0, Final_Third_Carries),
						 CPA_Carries = ifelse(is.na(CPA_Carries), 0, CPA_Carries),
						 prog_carrying_proportion = round(PrgDist_Carries/TotDist_Carries, 2),
						 poss_efficiency_ratio = case_when((Mis_Carries + Dis_Carries) > 0 ~ round(Touches_Touches / (Mis_Carries + Dis_Carries), 2),
						 																	TRUE ~ Touches_Touches),
						 carrying_threat_index = case_when(Carries_Carries > 0 ~ round((PrgC_Carries + Final_Third_Carries + CPA_Carries) / Carries_Carries, 2),
						 																	TRUE ~ 0),
						 Rival = case_when(Home_Away == "Home" ~ Away_Team,
						 									Home_Away == "Away" ~ Home_Team,
						 									TRUE ~ NA),
						 Team_Formation = case_when(Home_Away == "Home" ~ Home_Formation,
						 													 Home_Away == "Away" ~ Away_Formation,
						 													 TRUE ~ NA))
	}
	
	misc_stats <- {
		load_fb_advanced_match_stats(
			country = c("ITA", "ESP", "GER", "ENG", "FRA"),
			gender = "M",
			tier = "1st",
			season_end_year = 2024,
			stat_type = "misc",
			team_or_player = "player"
		) %>%
			separate(Pos, into = c("First_Pos", "Second_Pos"), sep = ",", extra = "drop", fill = "right") %>%
			mutate(Rival = case_when(Home_Away == "Home" ~ Away_Team,
															 Home_Away == "Away" ~ Home_Team,
															 TRUE ~ NA),
						 Team_Formation = case_when(Home_Away == "Home" ~ Home_Formation,
						 													 Home_Away == "Away" ~ Away_Formation,
						 													 TRUE ~ NA))
	}
	
	write.fst(pass_stats, "rda//pass_stats.fst")
	write.fst(pass_types_stats, "rda//pass_types_stats.fst")
	write.fst(defense_stats, "rda//defense_stats.fst")
	write.fst(possession_stats, "rda//possession_stats.fst")
	write.fst(misc_stats, "rda//misc_stats.fst")
}


load_keepers_data()
load_fbref_shots()
load_shots_understat()
load_outfield_players_data()
