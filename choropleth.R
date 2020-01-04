library(plotly)
library(dplyr)
df <- read.csv("https://raw.githubusercontent.com/plotly/datasets/master/2011_us_ag_exports.csv")
df$hover <- with(df, paste(state, '<br>', "Beef", beef, "Dairy", dairy, "<br>",
                           "Fruits", total.fruits, "Veggies", total.veggies,
                           "<br>", "Wheat", wheat, "Corn", corn))
# give state boundaries a white border
l <- list(color = toRGB("white"), width = 2)
# specify some map projection/options
g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showlakes = TRUE,
  lakecolor = toRGB('white')
)


p <- plot_geo(df, locationmode = 'USA-states') %>%
  add_trace(
    z = ~total.exports, text = ~hover, locations = ~code,
    color = ~total.exports, colors = 'Purples'
  ) %>%
  colorbar(title = "Millions USD") %>%
  layout(
    title = '2011 US Agriculture Exports by State<br>(Hover for breakdown)',
    geo = g
  )

# Create a shareable link to your chart
# Set up API credentials: https://plot.ly/r/getting-started
chart_link = plotly_POST(p, filename="choropleth/ag")
chart_link


#colors red to yellow YlOrRd

print(y)
print(p)

write.csv(df, "E:\\chart30choropleth.csv")



############
##########


#code replicated for chart 30


library(plotly)
library(dplyr)

retailchoropleth$hover <- with(retailchoropleth, paste(state, '<br>', "Change from 3 months ago is ", coct17))
retailchoropleth$statetext <- with(retailchoropleth, paste(code))
# give state boundaries a white border
l <- list(color = toRGB("white"), width = 2)
# specify some map projection/options
g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showlakes = TRUE,
  lakecolor = toRGB('white')
)


p <- plot_geo(retailchoropleth, locationmode = 'USA-states') %>%
  add_trace(
    z = ~coct17, text = ~paste(hover), locations = ~code,
    color = ~coct17, colors = greentored
              )%>%
  #add_text(retailchoropleth,text = ~paste(code), hoverinfo = "text"
   #                     ) %>% 
  colorbar(title = "%Change from 3months ago - Number of Employees") %>%
  layout(
    title = 'Retail trade Employees- %change from 3months ago<br>(Hover for change in the number of employees)',
    geo = g
  )


print(p)
greentored <- c("#FF0000","#FFFF00","#00FF00") 
f <- colorRamp(c("#FF0000","#00FF00"))
colors <- f(chart30choropleth$pc_3m_oct)
  greentored <- c("#FF0000","#FF9a00","#FFFF00","#00FF00","#228B22") 
# Create a shareable link to your chart
# Set up API credentials: https://plot.ly/r/getting-started
chart_link = plotly_POST(p, filename="choropleth/ag")
chart_link

#sample piece to attempt
add_markers(text = ~paste(name, "<br />", pop), hoverinfo = "text",
            data = maps::canada.cities)

install.packages(c("maps","mapdata")
ggplot2::map_data("world", "usa") %>%
  group_by(group) %>%
  plot_ly(x = ~long, y = ~lat) %>%
  add_polygons(hoverinfo = "none") %>%
  add_markers(text = ~paste(name, "<br />", pop), hoverinfo = "text",
              data = maps::usa.cities) %>%
  layout(showlegend = FALSE)


import plotly.plotly as py
import plotly.graph_objs as go

import pandas as pd
df = pd.read_csv('https://raw.githubusercontent.com/plotly/datasets/master/2014_ebola.csv')
df.head()

cases = []
colors = ['rgb(239,243,255)','rgb(189,215,231)','rgb(107,174,214)','rgb(33,113,181)']
months = {6:'June',7:'July',8:'Aug',9:'Sept'}

for i in range(6,10)[::-1]:
  cases.append(go.Scattergeo(
    lon = df[ df['Month'] == i ]['Lon'], #-(max(range(6,10))-i),
    lat = df[ df['Month'] == i ]['Lat'],
    text = df[ df['Month'] == i ]['Value'],
    name = months[i],
    marker = dict(
      size = df[ df['Month'] == i ]['Value']/50,
      color = colors[i-6],
      line = dict(width = 0)
    ),
  ) )

cases[0]['text'] = df[ df['Month'] == 9 ]['Value'].map('{:.0f}'.format).astype(str)+' '+\
df[ df['Month'] == 9 ]['Country']
cases[0]['mode'] = 'markers+text'
cases[0]['textposition'] = 'bottom center'

inset = [
  go.Choropleth(
    locationmode = 'country names',
    locations = df[ df['Month'] == 9 ]['Country'],
    z = df[ df['Month'] == 9 ]['Value'],
    text = df[ df['Month'] == 9 ]['Country'],
    colorscale = [[0,'rgb(0, 0, 0)'],[1,'rgb(0, 0, 0)']],
    autocolorscale = False,
    showscale = False,
    geo = 'geo2'
  ),
  go.Scattergeo(
    lon = [21.0936],
    lat = [7.1881],
    text = ['Africa'],
    mode = 'text',
    showlegend = False,
    geo = 'geo2'
  )
  ]

layout = go.Layout(
  title = 'Ebola cases reported by month in West Africa 2014<br> \
  Source: <a href="https://data.hdx.rwlabs.org/dataset/rowca-ebola-cases">\
  HDX</a>',
  geo = dict(
    resolution = 50,
    scope = 'africa',
    showframe = False,
    showcoastlines = True,
    showland = True,
    landcolor = "rgb(229, 229, 229)",
    countrycolor = "rgb(255, 255, 255)" ,
    coastlinecolor = "rgb(255, 255, 255)",
    projection = dict(
      type = 'Mercator'
    ),
    lonaxis = dict( range= [ -15.0, -5.0 ] ),
    lataxis = dict( range= [ 0.0, 12.0 ] ),
    domain = dict(
      x = [ 0, 1 ],
      y = [ 0, 1 ]
    )
  ),
  geo2 = dict(
    scope = 'africa',
    showframe = False,
    showland = True,
    landcolor = "rgb(229, 229, 229)",
    showcountries = False,
    domain = dict(
      x = [ 0, 0.6 ],
      y = [ 0, 0.6 ]
    ),
    bgcolor = 'rgba(255, 255, 255, 0.0)',
  ),
  legend = dict(
    traceorder = 'reversed'
  )
)

fig = go.Figure(layout=layout, data=cases+inset)
py.iplot(fig, validate=False, filename='West Africa Ebola cases 2014')




#colors red to yellow YlOrRd

print(y)
print(p)

write.csv(df, "E:\\chart30choropleth.csv")




###### Mainstreet chart 30 choropleth

library(plotly)
library(dplyr)

mainstreetchoropleth$hover <- with(mainstreetchoropleth, paste(state, '<br>', "Change from 3 months ago is ", c_3m_oct))
# give state boundaries a white border
l <- list(color = toRGB("white"), width = 2)
# specify some map projection/options
g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showlakes = TRUE,
  lakecolor = toRGB('white')
)


p <- plot_geo(mainstreetchoropleth, locationmode = 'USA-states') %>%
  add_trace(
    z = ~coct, text = ~hover, locations = ~code,
    color = ~coct, colors = greentored
  ) %>%
  colorbar(title = "Change from 3months ago - Number of Employees") %>%
  layout(
    title = 'Mainstreet Employees- change from 3months ago<br>(Hover for change in the number of employees)',
    geo = g
  )

f <- colorRamp(c("red", "green"))
colors <- f(mainstreetchoropleth$pc_3m_oct)
greentored <- c("#FF0000","#FFFF00","#00FF00")

# Create a shareable link to your chart
# Set up API credentials: https://plot.ly/r/getting-started
chart_link = plotly_POST(p, filename="choropleth/ag")
chart_link


#colors red to yellow YlOrRd

print(y)
print(p)

write.csv(df, "E:\\mainstreetchoropleth.csv")



#code for chart 28 unemployment rate


library(plotly)
library(dplyr)

unemp$hover <- with(unemp, paste(state, '<br>', "Change from 3 months ago is ", coct))
unemp$statetext <- with(unemp, paste(code))
# give state boundaries a white border
l <- list(color = toRGB("white"), width = 2)
# specify some map projection/options
g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showlakes = TRUE,
  lakecolor = toRGB('white')
)


p <- plot_geo(unemp, locationmode = 'USA-states') %>%
  add_trace(
    z = ~coct, text = ~paste(hover), locations = ~code,
    color = ~coct, colors = greentored
  )%>%
  #add_text(unemp,text = ~paste(code), hoverinfo = "text"
  #                     ) %>% 
  colorbar(title = "%Change from 3months ago - Number of Employees") %>%
  layout(
    title = 'Retail trade Employees- %change from 3months ago<br>(Hover for change in the number of employees)',
    geo = g
  )


print(p)

greentored <- c("#FF0000","#FFFF00","#00FF00") 
f <- colorRamp(c("#FF0000","#00FF00"))
colors <- f(chart30choropleth$pc_3m_oct)
greentored <- c("#FF0000","#FF9a00","#FFFF00","#00FF00","#228B22") 