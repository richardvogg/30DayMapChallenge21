library(leaflet)


leaflet() %>%
  addProviderTiles(provider = providers$Stamen.TonerBackground) %>%
  addRectangles(-160,-60,160,80) %>% # World
  addRectangles(-99.36,19.05,-98.94,19.59, label="Mexico City: 22",
                labelOptions = labelOptions(noHide = T,direction="left")) %>%
  addRectangles(-82,-58,-30,15, label="South America: 15",
                labelOptions = labelOptions(noHide = T,direction="right")) %>%
  addRectangles(-26,35,37.5,70) %>% # Europe
  addRectangles(-75.64, -55.61, -66.96, -17.58,label="Chile: 1,12",
                labelOptions = labelOptions(noHide = T,direction="right")) %>%
  addRectangles(5.99, 47.30, 15.02, 54.98, label="Germany: 2,5,17,18,23",
                labelOptions = labelOptions(noHide = T,direction="right")) %>%
  addRectangles(-19,-35,51,38) %>% # Africa
  addRectangles(-16.9,27.9,-16.1,28.7,label="Canary Islands: 19,20",
                labelOptions = labelOptions(noHide = T,direction="left")) %>%
  addRectangles(2.22,48.82,2.47,48.90,label="Paris: 9",
                labelOptions = labelOptions(noHide = T,direction="left")) %>%
  addRectangles(-3,-0.7,16,23, label="Ghana, Burkina Faso, Niger: 25",
                labelOptions = labelOptions(noHide = T,direction="left")) %>%
  addLabelOnlyMarkers(90,-50,label="World: 6,7,8,27,29,30",
                      labelOptions = labelOptions(noHide = T,direction="right")) %>%
  addLabelOnlyMarkers(5,70,label="Europe: 3",
                      labelOptions = labelOptions(noHide = T,direction="top")) %>%
  addLabelOnlyMarkers(51, 5, label="Africa: 4,10",
      labelOptions = labelOptions(noHide = T,direction="right"))



#Chile: 1,12
#Germany: 2,5,17,18,23
#France: 9
#SouthAmerica: 15
#Ghana, Burkina Faso, Niger: 25
#Mexico: 22
#World: 6,7,8,27,30
#Canary Islands: 19,20
#Europe: 3
#Africa: 4,10