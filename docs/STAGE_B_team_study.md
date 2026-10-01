# Stage B: do strong teams differ in style?

## Question
Do top teams occupy a different region of style space than bottom teams, does the roster variety differ, and does the
gap change across eras?

## Units and measures
- **Team-season** is the unit, about 30 teams by 23 seasons.
- **Quality** is mean point margin per game, computed from player points in 10. Steadier than win percentage.
- **Centroid** is the minutes-weighted mean style score of the team. **Dispersion** is the minutes-weighted mean
  distance of players from that centroid. The ecological parallel is community composition and functional diversity.
- **Tiers** are the top and bottom cfg$tier_n teams by margin within each season, the rest are middle.

## Steps in 12_team_style.R
1. Centroids, dispersion, and coverage (share of team minutes that carry a score).
2. Attach margin.
3. Tiers and era blocks (early 2003 to 2009, mid 2010 to 2016, late 2017 onward).
4. Top-minus-bottom gap by season, with a plot. Ordination plot by era.
5. Continuous model, margin on centroids and dispersion with season effects, overall and by era.
6. PERMANOVA on the top versus bottom contrast, permuting within season, plus betadisper for spread.
7. Rerun without talent-heavy components flagged in 11.

## How to read the results
- **Location test significant, spread test not.** The tiers differ in where they sit. Clean result.
- **Both significant.** Tiers differ in spread too. Read location with care.
- **Effect vanishes after dropping talent-heavy components.** The finding is about quality, not style.
- **Effect changes across eras.** This is the interesting version. Describe how, using the gap plot.

## Cautions
- Good teams have better players. Style components that load on usage and scoring volume partly measure talent.
- Associations only. Nothing here shows that adopting a style causes winning.
- Midseason trades make team membership fuzzy. Stints with fewer than the minutes floor carry no score.
- About 690 team-seasons, but neighboring seasons of one franchise are not independent. Treat p-values as a guide.
- The tier cutoff is arbitrary. The continuous model is the check on it.

## Outputs
output/team_style.rds, output/12_style_gap.png, output/12_ordination.png,
output/12_permanova_by_era.csv, output/12_regression_by_era.csv.

## Possible extensions
- Convex hull volume or effective number of roles as richer diversity measures.
- Procrustes comparison of eras.
- Lineup-level or five-man composition using tracking or play-by-play data.
