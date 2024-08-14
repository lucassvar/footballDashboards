source("dashboard_functions.R")

# FILTERS ----------------------------------------------------------------------
# General Filters
mins_per_match = 10

# Player Filters
player_sel = "Julián Álvarez"		# Player selected
player_date = c("2023-07-11", "2024-07-11")		# Date range

ply_filter_positions = F		# Filter positions???
player_positions = c("FW")		# Positions

ply_filter_formations = F 		# Filter formations?
player_formations = c("4-2-3-1")		# Formations


# Comparison Pool Filters
cp_leagues = c("Premier League", "La Liga", "Fußball-Bundesliga", "Serie A", "Ligue 1")		# Leagues
cp_date = c("2023-07-11", "2024-07-11")		# Date range
cp_played_90s = 10		# 90s played

cp_filter_positions	= T	# Filter positions???
cp_positions = c("FW")		# Positions

cp_filter_formations = F 		# Filter formations?
cp_formations = c("4-2-3-1")		# Formations

# Comp Pool layers to highlight in the plot
selected_compared_players <- c("Nicolas Jackson", "Erling Haaland", "Samuel Omorodion", "Victor Osimhen")


# DATA -------------------------------------------------------------------------
filtered_outfield <- filter_outfield_data(mins_per_match)



wrangled_datasets <- wrangle_outfield_data(player_sel,
																					 player_date,
																					 ply_filter_positions,
																					 player_positions,
																					 ply_filter_formations,
																					 player_formations,
																					 cp_leagues,
																					 cp_date,
																					 cp_played_90s,
																					 cp_filter_positions,
																					 cp_positions,
																					 cp_filter_formations,
																					 cp_formations)


# PLOTS ------------------------------------------------------------------------
create_dashboards(wrangled_datasets)