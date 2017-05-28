##Examples from the assignment
map_data <- get_map("Louisiana", zoom = 6, maptype = "toner-background")

base_map <- ggmap(map_data, extent = "device")

base_map +
    geom_hurricane(data = katrina, aes(x = longitude, y = latitude,
                                       r_ne = ne, r_se = se,
                                       r_nw = nw, r_sw = sw,
                                       fill = wind_speed,
                                       color = wind_speed)) +
    scale_color_manual(name = "Wind speed (kts)",
                       values = c("red", "orange", "yellow")) +
    scale_fill_manual(name = "Wind speed (kts)",
                      values = c("red", "orange", "yellow"))
