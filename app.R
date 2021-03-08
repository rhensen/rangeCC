library(shiny)
library(lwgeom)
library(data.table)
library(soilDB)
library(tidyverse)
library(sf)
library(sp)
library(rgdal)
library(rgeos)
library(shinythemes)
library(leaflet)
library(shinybusy)
library(rhandsontable)
#library(mapview)
library(leaflet.extras)
library(htmlwidgets)
#https://github.com/AugustT/shiny_geolocation
#^ for adding locaton to app to be used on a mobile device. 


ret.wss<- function(shp){
    shp<- shp %>% st_as_sf()
    shp<- st_buffer(shp,0)
    ras<- mukey.wcs(shp, db = c("gssurgo"), res = 30, quiet = FALSE)
    mukeys<- raster::unique(ras$gSSURGO.map.unit.keys)
    poly<- fetchSDA_spatial(mukeys,by.col = "mukey",
                            method = "feature",
                            geom.src = "mupolygon",
                            db = "SSURGO")
    spoly<-st_as_sf(poly)
    shp<- st_transform(shp, st_crs(spoly))
    
    intersect_pct <- st_intersection(shp, spoly) %>% 
        dplyr::mutate(intersect_area = st_area(.))  # create new column with shape area
    #dplyr::select(Name, mukey,areasymbol, intersect_area,geometry) # only select columns needed to merge
    tota <- mutate(shp, total_area = st_area(shp)) %>% dplyr::select(Name, total_area)  %>%   # only select columns needed to merge
        st_drop_geometry()
    
    # Merge by county name
    merg <- merge(intersect_pct,tota, by = "Name")
    # Calculate coverage
    merg_2 <- merg%>% 
        mutate(coverage = as.numeric(intersect_area/total_area),
               acre_area = as.numeric(intersect_area)*0.000247105,
               total_acre = as.numeric(total_area)*0.000247105)
    s<-list()
    for(i in 1:length(mukeys))
    {
        q <- "SELECT areasymbol,component.mukey, muname,component.rsprod_l,component.rsprod_r,component.rsprod_h
FROM legend
INNER JOIN mapunit ON mapunit.lkey = legend.lkey
INNER JOIN component ON component.mukey = mapunit.mukey
WHERE component.mukey = " 
        m<- paste(mukeys[i],  collapse=", ")
        options(useFancyQuotes = F)
        m<-sapply(strsplit(m, '[, ]+'), function(x) toString(sQuote(x)))
        q2<- paste(q, m, sep = "")
        s[[i]] <- SDA_query(q2)
    }
    soils<- rbindlist(s)
    #soilnam<- soils[complete.cases(soils),]
    soilnam<- soils
    
    
    soilf<- merge(merg_2,soilnam, by= c("mukey","areasymbol"))
    soilf<- soilf%>% dplyr::group_by(Name,muname,mukey)%>% dplyr::summarise_at(vars(rsprod_l,rsprod_r,rsprod_h,acre_area,total_acre), mean, na.rm=T)
    soilf2<- ungroup(soilf)%>% dplyr::mutate(soilf, id = row_number())
    return(soilf2)
}
retnum.wss<- function(fin,fu, fe, ccpm,ts){
    fin[is.na(fin)]<- 0
    fin2<- fin
    fin2<- fin2 %>%summarise_at(vars(rsprod_l,rsprod_r,rsprod_h), function(col){fin$acre_area*col})
    fin2<- cbind(fin$Name,fin2)
    names(fin2)[1]<- "Name" 
    fin2<- fin2%>% group_by(Name)%>%summarize_at(vars(rsprod_l,rsprod_r,rsprod_h),sum)
    fin.aum<- fin2 %>% group_by(Name)%>%mutate_at(vars(rsprod_l,rsprod_r,rsprod_h),function(col){((col*fe*fu)/(((ccpm*1.1)/30.4)*(ts)))})
    names(fin.aum)[2:4]<- c("Low","Average","High")
    return(fin.aum)
}
retday.wss<- function(fin,fu, fe, ccpm,h){
    fin[is.na(fin)]<- 0
    fin2<- fin
    fin2<- fin2 %>%summarise_at(vars(rsprod_l,rsprod_r,rsprod_h), function(col){fin$acre_area*col})
    fin2<- cbind(fin$Name,fin2)
    names(fin2)[1]<- "Name" 
    fin2<- fin2%>% group_by(Name)%>%summarize_at(vars(rsprod_l,rsprod_r,rsprod_h),sum)
    fin.aum<- fin2 %>% group_by(Name)%>%summarize_at(vars(rsprod_l,rsprod_r,rsprod_h),function(col){((col*fe*fu)/h)/((ccpm*1.1)/30.4)})
    names(fin.aum)[2:4]<- c("Low","Average","High")
    fin.aum[,c(2:4)] <- lapply(fin.aum[,c(2:4)], function(x) ifelse(x>365, 365, x))
    return(fin.aum)
}
retac<- function(shp){
    shp<- shp %>% st_as_sf()
    shp<- st_transform(shp, 4326)
    shp<- st_buffer(shp,0)
    tota <- mutate(shp, total_area = st_area(shp), acres=as.numeric(total_area*0.000247105))%>%   st_drop_geometry()
    return(tota)
}

shpfilefunc<- function(input){  
    req(input)
    shpDF <- input
    pwd <- getwd()
    updir <- dirname(shpDF$datapath[1])
    setwd(updir)
    for (i in 1:nrow(shpDF)) {
        file.rename(shpDF$datapath[i], shpDF$name[i])
    }
    shpName <- shpDF$name[grep(x = shpDF$name, pattern = "*.shp")]
    shpPath <- paste(updir, shpName, sep = "/")
    setwd(pwd)
    shpFile <- readOGR(shpPath)
    return(shpFile)
}
kmlfilefunc<- function(input){  
    req(input)
    kmlFile <- readOGR(input$datapath)
    return(kmlFile)
}

drawmap<- function(input){ 
    x <- input
    y<- rbindlist(x)
    y<- unique(y)
    names(y)<- c("lon","lat")
    polygon <- y %>%
        st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
        summarise(geometry = st_combine(geometry)) %>%
        st_cast("POLYGON")
    polygon$Name<- "Pasture 1"
    polygon<- as(polygon, 'Spatial')
    return(polygon)}


mf<- c(851,913,988,1064,1125,1186,1247,1295,547,638,832,913,1368,1155,135,182,228,152,152,182,547,182,182)
mfcnam<- c("1000lb cow - dry", "1000lb cow - calf","1100lb cow - calf", "1200lb cow - calf","1300lb cow - calf",
           "1400lb cow - calf","1500lb cow - calf", "Cattle Bull - mature", "Weaned Calf", "Yearling Cattle (600-800lbs)", "Two-Year-Old Cattle (800-1000lbs)",
           "Bison Cow - mature", "Bison Bull- mature", 
           "Horse - mature", "Sheep - dry", "Sheep - lamb", "Sheep Ram", "Goat - mature", "White Tail Deer","Mule Deer","Elk", "Pronghorn", "Bighorn Sheep")
names(mf)<- mfcnam


values <- list() 
DF <- data.frame()
setHot <- function(x) 
    values[["hot"]] <<- x 




# Define UI for data upload app ----
ui <- fluidPage(theme = shinytheme("cerulean"),h1("Rangeland Carrying Capacity Tool", align = "center"),
                sidebarLayout(
                    
                    # Sidebar panel for inputs ----
                    sidebarPanel(
                        
                        # Input: Select a file ----
                        fileInput(inputId = "shp", label = "Import  KML file", multiple = T, accept = c('.shp', '.dbf','.sbn', '.sbx', '.shx', '.prj', ".kml")),
                        #fileInput(inputId = "kml", label = "Import KML File ", multiple = F, accept = c(".kml")),
                        
                        selectInput("type", "Choose Your Calculation",
                                    choices=  c("Number of Animals", "Use Days"),
                                    selected = "Number of Animals", multiple = F
                        ),
                        p("NOTE: Estimates assume pastures are in a non-irrigated, mostly native vegetation state", align = "left"),
                        
                        sliderInput("slid1","Choose Consumption Rate (%)",0,100,50,5),
                        sliderInput("slid2","Choose Percent Edible (%)",0,100,80,5),
                        selectInput("slid3", "Choose Animal Class",
                                    choices=  mf, multiple = F, selectize = F
                        ),
                        #sliderInput("slid4","Choose Duration (days)",1,365,30,1),
                        #checkboxInput(inputId = "typeh", label = strong("Calculate Number of Animals"), value = FALSE),
                        #checkboxInput(inputId = "typed", label = strong("Calculate Use Days"), value = FALSE),
                        conditionalPanel(condition = "input.type == 'Number of Animals'",
                                         shiny::sliderInput("slid4","Choose Duration (days)",1,365,30,1)
                        ),
                        conditionalPanel(condition = "input.type == 'Use Days'",
                                         shiny::numericInput("head", "Input number of animals",10,0,10000,1)
                        ),
                        selectInput("scen", "Forage Production Level",
                                    choices=  c("Low","Average","High"),
                                    selected = "Average", multiple = F),

                    actionButton("help", "Help",icon("question"), 
                                 style="color: #317eac; background-color: #317eac; border-color: #2fa4e7; font-size:130%")),
                
                    # Main panel for displaying outputs ----
                    mainPanel(
                        
                        
                    tabsetPanel(type = "tabs",
                                    tabPanel("Instructions",
                                             br(),
                                             br(),
                                             h4("Steps",align = "left"),
                                             h5("Step 1: Create a map of of your pastures.",align = "left"),
                                             HTML('&emsp;'),h5("You can create a polygon in the Draw tab",align = "left",style = "display: inline;"),
                                             br(),
                                             HTML('&emsp;'),h5("Or create a pasture map file by going to:",align = "center", style = "display: inline;"), 
                                             a('https://www.google.com/maps/about/mymaps/',href = "https://www.google.com/maps/about/mymaps/",target="_blank", rel="noopener noreferrer",style = "display: inline;"),
                                             br(),
                                             HTML('&emsp;'),
                                             a('For help with creating your pastures on google, click here', href = "http://reidhensen.info/wp-content/uploads/2021/02/read.me_-1.pdf",target="_blank", rel="noopener noreferrer",style = "display: inline;"),
                                             br(),
                                             HTML('&emsp;'),h5("Upload your downloaded .KML file to this app",align = "left",style = "display: inline;"),
                                             h5("Step 2: Select your calculation type, forage utlization rate, percent of the total forage that is edible, animal type, and forage scenario",  align = "left"),
                                             h5("Step 3: Click on the Carrying Capacity Map Tab to see capacity estimates and download your map. Map will appear once the loading spinner has stopped. 
                                                Adjusting either the forage conditions or stocking parameter will update the map. Explore stocking rates under different scenarios!",  align = "left"),
                                             h5("Step 4: If needed, adjust the forage estimates in the Edit Tab to more accurately reflect production conditions.",  align = "left"),
                                             br(),
                                             br(),
                                             h4("Notes",align = "left"),
                                             h5("1. This tool is not meant to replace on the ground forage assessments. The forage production in pastures of interest is likely to differ from these estimates and when managing animals, 
                                              on the ground forage assessments are still needed to ensure proper stocking.",  align = "left"),
                                             h5("2. All 'use day' estimates are in animal days, with respect to above ground forage production. Specific attention should be paid to when these pastures are grazed. For example, 500 head for ~12 days means there are approximately 12 days of use for the year, 
                                             but that does not mean that 12 consective days will be the proper way to stock it. 
                                             Consult local experts for more information on season of use.",  align = "left"),
                                             h5("3. Production estimates come from the NRCS ssurgo database.",  align = "left"),
                                        
                                
                                br(),
                                br(),
                                p('App built and designed by Reid Hensen. See more at',style = "display: inline;"), 
                                a('reidhensen.info',href = "http://www.reidhensen.info/",target="_blank", rel="noopener noreferrer",style = "display: inline;")
                                    ),                                                                                                                                          
                                    tabPanel("Draw Pastures",
                                             leafletOutput("map2", "100%", 650)),
                                    tabPanel("Carrying Capacity Map",  
                                             h4("Carrying Capacity By Pasture", align = "center"),
                                             downloadButton("dl","Download Map"),
                                             leafletOutput("grazmap",width = "100%", height = 600)),
                                    tabPanel("Edit Forage Estimates", h5("Click on a shape and edit its forage levels by editting the cells that pop up, then press save to update.", align = "center"),
                                             p("Error will disappear once you click and edit an ecological site", align = "center"),
                                             leafletOutput("map",width = "100%", height = 600),
                                             p("NOTE: Mukey = SSURGO data key, rsprod_l = low forage conditions, rsprod_r = average forage conditions, rsprod_h = high forage conditions"),
                                             p("Values are in lbs/acre/year"),
                                             rHandsontableOutput("hot"),
                                             br(),
                                             actionButton("save","Save"))
                                ),
                                
                        add_busy_spinner(spin = "fading-circle"),
                        
                        
                    )
                    
                )
)



# Define server logic to read selected file ----
server <- function(input, output,session) {
   
    dataModal <- function(failed = FALSE) {
        modalDialog(
            span('Forage estimates are just that, estimates. These may differ from on the ground assessments.
                 Please email reid.hensen@colostate.edu for assistance or to report an error or mistake in the app. 
                 Production Data come from the NRCS SSURGO database. Pastures sections with no available data are treated as 0s.'),
            footer = tagList(
                modalButton("Got it"),
            )
        )
    }
    
    observeEvent(input$help, {
        showModal(dataModal())
    })
    uploadfile <- reactive({
        if(is_empty(input$map2_draw_new_feature)){
            return(kmlfilefunc(input$shp))
        }
        else({
             drawmap(input$map2_draw_new_feature$geometry$coordinates[[1]])
              })
    })
    fin<- reactive({ 
        #if(is_empty(input$map2_draw_new_feature)){
        st_transform(ret.wss(uploadfile()),4326)
    })
    fin2<- reactive({return({as.data.frame(fin()) %>% group_by(mukey) %>% summarize_at(vars(rsprod_l,rsprod_r,rsprod_h), mean) %>% 
            dplyr::filter(mukey==input$map_shape_click['id'])})
    })
    f3<- reactive({return({as.data.frame(fin())%>% dplyr::select(id,Name,mukey,acre_area,rsprod_l,rsprod_r,rsprod_h)})
    })
    
    output$map <-  renderLeaflet({
        if (is_empty(uploadfile())) {leaflet()}
        else{
                dd<- merge(fin(),values[["fin4"]], by=c("id","Name","mukey"))
                dd[!is.na(dd$rsprod_l.y),c("rsprod_l.x")]<- NA
                dd[!is.na(dd$rsprod_r.y),c("rsprod_r.x")]<- NA
                dd[!is.na(dd$rsprod_h.y),c("rsprod_h.x")]<- NA
                dd<- dd%>% mutate(rsprod_l = coalesce(rsprod_l.x,rsprod_l.y),
                                  rsprod_r = coalesce(rsprod_r.x,rsprod_r.y),
                                  rsprod_h = coalesce(rsprod_h.x,rsprod_h.y),
                                  acre_area=acre_area.x) %>% select(-acre_area.x, -acre_area.y,-rsprod_l.x,-rsprod_l.y,-rsprod_r.x,-rsprod_r.y,-rsprod_h.x,-rsprod_h.y)
                
                labels <- sprintf(
                    "<strong>%s</strong><br/>%g acres<br/>%g Average lbs/acre/year",
                    dd$Name,round(dd$acre_area,2),round(dd$rsprod_r,2)) %>% lapply(htmltools::HTML)
                
                pal<-colorNumeric("YlGnBu", dd$rsprod_r) 
                leaflet(dd) %>%
                    addPolygons(stroke = TRUE, fillOpacity = 0.2, smoothFactor = 0.5,
                                color = "black", opacity = .5,weight = 1.5, fillColor = ~pal(dd$rsprod_r) , label = labels, layerId= ~dd$mukey
                    ) %>% 
                    addProviderTiles(providers$Esri.WorldImagery)            
        }
                })
    values <- reactiveValues()
    
    ## Handsontable
    output$hot <- renderRHandsontable({
        tryCatch({
        rhandsontable(fin2(), useTypes = T, stretchH = "all") 
        }, error=function(cond) {print("error2: waiting on upload ")}, warning= function(cond)
        {print("warning: waiting on click")})
    })
    observe({
        tryCatch({
        values[["fin4"]] <- f3()
        }, error=function(cond) {print("error2: waiting on upload")})
    })
    observeEvent(input$save,{
            tryCatch({
            DF <- (hot_to_r(input$hot))
            setHot(DF)
            row<- as.integer(input$map_shape_click['id'])
            values[["fin3"]]<- DF
            values[["fin4"]]<-merge(values[["fin4"]],values[["fin3"]], by=c("mukey"), all.x=T)
            values$fin4[!is.na(values$fin4$rsprod_l.y),c("rsprod_l.x")]<- NA
            values$fin4[!is.na(values$fin4$rsprod_r.y),c("rsprod_r.x")]<- NA
            values$fin4[!is.na(values$fin4$rsprod_h.y),c("rsprod_h.x")]<- NA
            values[["fin4"]]<- values$fin4%>% mutate(rsprod_l = coalesce(rsprod_l.x,rsprod_l.y),
                                                     rsprod_r = coalesce(rsprod_r.x,rsprod_r.y),
                                                     rsprod_h = coalesce(rsprod_h.x,rsprod_h.y)) %>% 
                dplyr::select(id,Name,mukey,acre_area,rsprod_l,rsprod_r,rsprod_h)
            #%>% mutate_if(rsprod_l=!is.na(rsprod_l.y),rsprod_r=!is.na(rsprod_r.y),rsprod_h=!is.na(rsprod_h.y))%>% dplyr::select(id,Name,acre_area=acre_area.x,rsprod_l,rsprod_r,rsprod_h)
            }, error=function(e) {print("error")},warning= function(w)
            {print("warning: waiting on click")})
            }, 
        ignoreInit=F)
    output$grazmap<- renderLeaflet({
        tryCatch({
            if(input$type=="Number of Animals"){
                plotdat<- retnum.wss(values$fin4,input$slid1/100,input$slid2/100,as.numeric(input$slid3),input$slid4)
                plotdat<- plotdat[,c("Name", input$scen)]
                shp2<- st_as_sf(uploadfile())
                centers <- data.frame(gCentroid(uploadfile(), byid = TRUE))
                centers$Name <- uploadfile()$Name
                new<-  merge(plotdat,centers, by="Name")
                new$lab<- paste("~",round(new[,2],0), " Head", sep= "")
                dayz<- paste(input$slid4, "Days" , sep=" ")
                new$lab2<- paste(dayz,new$lab, sep="<br/>")
                new$num<- new[,2]
                ac<- retac(uploadfile())
                mapp<- merge(shp2,ac, by="Name")
                mapp<- merge(mapp,new, by="Name")
                #center<- st_centroid(st_as_sf(uploadfile()()))
                #mapp<- merge(uploadfile()(),ac, by="Name")
                labels2 <- paste(sprintf(
                    "<strong>%s</strong><br/>%g acres <br/>",
                    mapp$Name, round(mapp$acres,0)),new$lab2, sep="") %>% lapply(htmltools::HTML)
                
                pal<-colorNumeric("YlGnBu", mapp$num) 
                values$map<- leaflet(mapp) %>%
                    addPolygons(stroke = TRUE, fillOpacity = 0.5, smoothFactor = 0.5,
                                color = "black", opacity = 1, fillColor = ~pal(mapp$num), layerId= mapp$Name
                    ) %>% 
                    addLabelOnlyMarkers(data = new,
                                        lng = ~x, lat = ~y, label = ~labels2,
                                        labelOptions = labelOptions(noHide = TRUE, direction = 'top',textsize = "12px"))%>% 
                    addProviderTiles(providers$Esri.WorldTopoMap)
                values$map
                }
            else{
                plotdat<-  retday.wss(values$fin4,input$slid1/100,input$slid2/100,as.numeric(input$slid3),input$head)
                plotdat<- plotdat[,c("Name", input$scen)]
                shp2<- st_as_sf(uploadfile())
                centers <- data.frame(gCentroid(uploadfile(), byid = TRUE))
                centers$Name <- uploadfile()$Name
                new<-  merge(plotdat,centers, by="Name")
                new$lab<- paste("~",round(new[,2],0), " Days", sep= "")
                new[which(new[,2]==365),"lab"]<-  paste(">",round(new[,2],0), " Days", sep= "")
                headz<- paste(input$head, " Head" , sep=" ")
                new$lab2<- paste(new$lab,headz, sep="<br/>")
                new$num<- new[,2]
                ac<- retac(uploadfile())
                mapp<- merge(shp2,ac, by="Name")
                mapp<- merge(mapp,new, by="Name")
                #center<- st_centroid(st_as_sf(uploadfile()()))
                #mapp<- merge(uploadfile()(),ac, by="Name")
                labels2 <- paste(sprintf(
                    "<strong>%s</strong><br/>%g acres <br/>",
                    mapp$Name, round(mapp$acres,0)),new$lab2, sep="") %>% lapply(htmltools::HTML)
                
                pal<-colorNumeric("YlGnBu", mapp$num) 
                values$map<- leaflet(mapp) %>%
                    addPolygons(stroke = TRUE, fillOpacity = 0.5, smoothFactor = 0.5,
                                color = "black", opacity = 1, fillColor = ~pal(mapp$num), layerId= mapp$Name
                    ) %>% 
                    addLabelOnlyMarkers(data = new,
                                        lng = ~x, lat = ~y, label = ~labels2,
                                        labelOptions = labelOptions(noHide = TRUE, direction = 'top',textsize = "12px"))%>% 
                    addProviderTiles(providers$Esri.WorldTopoMap)
                values$map
            }
            
    }, error=function(cond) {leaflet()})
    })
    output$text<- renderText(input$type)
    output$dl <- downloadHandler(
        filename = paste0(Sys.Date()
                           , "_rangeCC"
                           , ".html"
        )
        , content = function(file) {
            saveWidget(
                widget = values$map
                , file = file
            )
            #webshot(x =  values$map
                     #, file = file
                     #, cliprect = "viewport" # the clipping rectangle matches the height & width from the viewing port
                     #, selfcontained = FALSE # when this was not specified, the function for produced a PDF of two pages: one of the leaflet map, the other a blank page.
            #)
        } # end of content() function
    )
    
    output$map2<- renderLeaflet({
        l <-  leaflet() %>%
            setView(-96, 37.8, 4)
        
        esri <- grep("^Esri", providers, value = TRUE)
        
        for (provider in esri[4:5]) {
            l <- l %>% addProviderTiles(provider, group = provider)
        }
        l %>%
        addProviderTiles(providers$Esri.WorldTopoMap) %>%
        addDrawToolbar(
            targetGroup = "Draw",
            editOptions = editToolbarOptions(
                selectedPathOptions = selectedPathOptions()
            ),
            polylineOptions=F,
            circleOptions=F,
            circleMarkerOptions=F,
            markerOptions=F,
            rectangleOptions = F
            
        )  %>%
        addLayersControl(
            baseGroups = names(esri)[4:5],
            options = layersControlOptions(collapsed = FALSE)
        ) %>%
        htmlwidgets::onRender("
    function(el, x) {
      var myMap = this;
      myMap.on('baselayerchange',
        function (e) {
          myMap.minimap.changeLayer(L.tileLayer.provider(e.name));
        })
    }")
    })
    
}

# Create Shiny app ----
shinyApp(ui, server)
