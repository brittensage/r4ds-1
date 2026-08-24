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
  ggplot(aes(x = week, y = rank, group = track)) +
  geom_line(alpha = 0.15, na.rm = TRUE) +
  scale_y_reverse() +
  labs(
    title = "Song rankings over time",
    x = "Week",
    y = "Rank"
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
#
