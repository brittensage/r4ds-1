#
#
#
#
#
#
#
#
#
#| message: false
library(tidyverse)
#
#
#
#| cache: true
#| message: false
music <- read_csv("data/music.csv")
#
#
#
names(music)
#
#
#
music |>
  select(starts_with("artist")) |>
  glimpse()
#
#
#
music |>
  select(
    artist.name,
    artist.location,
    artist.latitude,
    artist.longitude,
    artist.terms,
    artist.familiarity,
    artist.hotttnesss,
    song.title,
    song.year
  ) |>
  slice_head(n = 8)
#
#
#
billboard |>
  select(artist, track, date.entered, wk1:wk4)
#
#
#
billboard |>
  summarize(
    earliest = min(date.entered),
    latest = max(date.entered)
  )
#
#
#
billboard_long <- billboard |>
  pivot_longer(
    cols = starts_with("wk"),
    names_to = "week",
    names_prefix = "wk",
    values_to = "rank",
    names_transform = list(week = as.integer),
    values_drop_na = TRUE
  )

billboard_long |>
  group_by(artist, track) |>
  filter(any(rank <= 10)) |>
  ungroup() -> top10_long

notable_songs <- tibble(
  artist = c("Madonna", "Lonestar", "Creed"),
  track = c("Music", "Amazed", "Higher")
)

highlighted_songs <- top10_long |>
  semi_join(notable_songs, by = c("artist", "track"))

ggplot(top10_long, aes(x = week, y = rank, group = interaction(artist, track))) +
  geom_line(color = "gray70", alpha = 0.4, na.rm = TRUE) +
  geom_line(
    data = highlighted_songs,
    aes(color = paste(artist, track, sep = " - ")),
    linewidth = 0.9,
    na.rm = TRUE
  ) +
  scale_y_reverse() +
  labs(
    title = "Top-10 song rankings over time",
    x = "Week",
    y = "Rank",
    color = "Notable songs"
  )
#
#
#
billboard |>
  summarize(
    across(
      c(wk1, wk4, wk10, wk20, wk40, wk76),
      list(
        missing = ~ sum(is.na(.x)),
        present = ~ sum(!is.na(.x))
      )
    )
  ) |>
  pivot_longer(
    everything(),
    names_to = c("week", ".value"),
    names_sep = "_"
  )
#
#
#
rank_comparison <- billboard |>
  mutate(
    rank_change = wk6 - wk1,
    result = case_when(
      is.na(wk6) ~ "missing by week 6",
      rank_change < 0 ~ "improved",
      rank_change == 0 ~ "stayed the same",
      rank_change > 0 ~ "got worse"
    )
  )

rank_comparison |>
  count(result, name = "songs")

rank_comparison |>
  summarize(
    songs_with_both_weeks = sum(!is.na(rank_change)),
    median_rank_change = median(rank_change, na.rm = TRUE)
  )
#
#
#
billboard_long |>
  group_by(artist, track) |>
  arrange(week, .by_group = TRUE) |>
  summarize(re_entered = any(diff(week) > 1), .groups = "drop") |>
  count(re_entered)
#
#
#
song_summary <- billboard_long |>
  group_by(artist, track) |>
  summarize(
    first_rank = rank[which.min(week)],
    best_rank = min(rank),
    first_week_at_best = min(week[rank == min(rank)]),
    total_weeks = n(),
    .groups = "drop"
  )

song_summary

top10_songs <- billboard_long |>
  group_by(artist, track) |>
  summarize(top10_weeks = sum(rank <= 10), .groups = "drop") |>
  inner_join(song_summary, by = c("artist", "track")) |>
  filter(best_rank <= 10)

bind_rows(
  fastest_to_number_one = top10_songs |>
    filter(best_rank == 1) |>
    arrange(first_week_at_best, artist, track) |>
    slice_head(n = 1),
  slowest_to_number_one = top10_songs |>
    filter(best_rank == 1) |>
    arrange(desc(first_week_at_best), artist, track) |>
    slice_head(n = 1),
  longest_top10_run = top10_songs |>
    arrange(desc(total_weeks), artist, track) |>
    slice_head(n = 1),
  .id = "notable"
) |>
  select(
    notable, artist, track, first_rank, best_rank,
    first_week_at_best, total_weeks
  )
#
#
#
#
