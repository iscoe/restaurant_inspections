require(ggplot2)
require(ggmap)
require(maps)

raleigh <- get_map(location = "raleigh", zoom=12)

inspections <- dat #readRDS("DATA/food_inspections.Rds")

inspections = na.omit(inspections)
inspections = inspections[!apply(inspections, 1, function(x) any(x=="")),] 

#risk_colors <- list("Risk 1 (High)"="red", "Risk 2 (Medium)"="orange", "Risk 3 (Low)"="green", "All"="black")
risks = unique(inspections$Risk)

#inspections = subset(inspections, Risk=="Risk 3 (Low)")

mapPoints <- ggmap(raleigh) + geom_point(data=inspections, 
                                     aes(x=longitude, y=latitude),
                                     color=#ffffff,
                                     size=0.8,
                                     alpha=0.5)

mapPoints

