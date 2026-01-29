pointsMap <- function(df = expTmt.table, size) {
  geom_point(
    data = df,
    aes(Longitude, Latitude, col = Tmt_sugar_name, shape = Included),
    size = size
  )
}