library("RMySQL")
library(shiny)
library(plotly)
library(DBI)
library(DT)
library(shinythemes)
library(bslib)

#connect to MySQL Workbench

mydb <- dbConnect(RMySQL::MySQL(), user = 'username', password = 'password123', 
                  dbname = 'final', host = 'localhost', port = 3306)

#assign variable for state input

states <- dbGetQuery(mydb, 'select distinct proj_st from property_1')
states$proj_st <- paste0("'",states$proj_st,"'")

#fix column names

#dbGetQuery(mydb, 'ALTER TABLE unit_characteristics_1 
  #RENAME COLUMN li_units TO Total_Number_of_Low_Income_Units,
  #RENAME COLUMN n_1br TO Number_of_1br_Units,
  #RENAME COLUMN n_2br TO Number_of_2br_Units,
  #RENAME COLUMN n_3br TO Number_of_3br_Units,
  #RENAME COLUMN n_4br TO Number_of_4br_Units')

#add dummy column

#dbGetQuery(mydb, 'ALTER TABLE unit_characteristics_1
#ADD COLUMN No_Selection VARCHAR(15)')
#dbGetQuery(mydb, 'UPDATE unit_characteristics_1 SET No_Selection = "Yes"')
#dbGetQuery(mydb, 'ALTER TABLE unit_characteristics_1 RENAME COLUMN No_Selection TO No_br_Selection')
dbGetQuery(mydb, 'UPDATE unit_characteristics_1
  SET No_br_Selection = REPLACE(No_br_Selection,"Yes","1")')

#assign variable for unit input

unit_char = dbGetQuery(mydb, "select Total_Number_of_Low_Income_Units,
                       Number_of_1br_Units,
                       Number_of_2br_Units,
                       Number_of_3br_Units,
                       Number_of_4br_Units,
                       No_br_Selection
                       from unit_characteristics_1")
select_br = colnames(unit_char)

#change column names

#dbGetQuery(mydb, 'ALTER TABLE target_pops_1 
  #RENAME COLUMN trgt_fam TO Families,
  #RENAME COLUMN trgt_eld TO Elderly,
  #RENAME COLUMN trgt_dis TO Disabled,
  #RENAME COLUMN trgt_hml TO Homeless,
  #RENAME COLUMN trgt_other TO Other')

#adjust values to yes or no

dbGetQuery(mydb, 'UPDATE target_pops_1
  SET Families = REPLACE(Families,"1","Yes")')
dbGetQuery(mydb, 'UPDATE target_pops_1
  SET Families = REPLACE(Families,"2","No")')
dbGetQuery(mydb, 'UPDATE target_pops_1
  SET Elderly = REPLACE(Elderly,"1","Yes")')
dbGetQuery(mydb, 'UPDATE target_pops_1
  SET Elderly = REPLACE(Elderly,"2","No")')
dbGetQuery(mydb, 'UPDATE target_pops_1
  SET Disabled = REPLACE(Disabled,"1","Yes")')
dbGetQuery(mydb, 'UPDATE target_pops_1
  SET Disabled = REPLACE(Disabled,"2","No")')
dbGetQuery(mydb, 'UPDATE target_pops_1
  SET Homeless = REPLACE(Homeless,"1","Yes")')
dbGetQuery(mydb, 'UPDATE target_pops_1
  SET Homeless = REPLACE(Homeless,"2","No")')
dbGetQuery(mydb, 'UPDATE target_pops_1
  SET Other = REPLACE(Other,"1","Yes")')
dbGetQuery(mydb, 'UPDATE target_pops_1
  SET Other = REPLACE(Other,"2","No")')

#add dummy column

#dbGetQuery(mydb, 'ALTER TABLE target_pops_1
#ADD COLUMN No_Selection VARCHAR(15)')
#dbGetQuery(mydb, 'ALTER TABLE target_pops_1 RENAME COLUMN No_Selection TO No_population_Selection')
#dbGetQuery(mydb, 'UPDATE target_pops_1 SET No_population_Selection = "Yes"')

#variable for population input

pop_serve = dbGetQuery(mydb, "select Families, Elderly, Disabled, Homeless, Other, No_population_Selection from target_pops_1")
select_pop = colnames(pop_serve)

#change column names

#dbGetQuery(mydb, 'ALTER TABLE financing_1 
  #RENAME COLUMN home TO HOME,
  #RENAME COLUMN tcap TO TCAP,
  #RENAME COLUMN cdbg TO CDBG,
  #RENAME COLUMN htf TO HTF,
  #RENAME COLUMN hopevi TO HOPEVI,
  #RENAME COLUMN tcep TO TCEP,
  #RENAME COLUMN qozf TO QOZF')

#update values to yes or no

dbGetQuery(mydb, 'UPDATE financing_1
  SET HOME = REPLACE(HOME,"1","Yes")')
dbGetQuery(mydb, 'UPDATE financing_1
  SET HOME = REPLACE(HOME,"2","No")')
dbGetQuery(mydb, 'UPDATE financing_1
  SET TCAP = REPLACE(TCAP,"1","Yes")')
dbGetQuery(mydb, 'UPDATE financing_1
  SET TCAP = REPLACE(TCAP,"2","No")')
dbGetQuery(mydb, 'UPDATE financing_1
  SET CDBG = REPLACE(CDBG,"1","Yes")')
dbGetQuery(mydb, 'UPDATE financing_1
  SET CDBG = REPLACE(CDBG,"2","No")')
dbGetQuery(mydb, 'UPDATE financing_1
  SET HTF = REPLACE(HTF,"1","Yes")')
dbGetQuery(mydb, 'UPDATE financing_1
  SET HTF = REPLACE(HTF,"2","No")')
dbGetQuery(mydb, 'UPDATE financing_1
  SET HOPEVI = REPLACE(HOPEVI,"1","Yes")')
dbGetQuery(mydb, 'UPDATE financing_1
  SET HOPEVI = REPLACE(HOPEVI,"2","No")')
dbGetQuery(mydb, 'UPDATE financing_1
  SET TCEP = REPLACE(TCEP,"1","Yes")')
dbGetQuery(mydb, 'UPDATE financing_1
  SET TCEP = REPLACE(TCEP,"2","No")')
dbGetQuery(mydb, 'UPDATE financing_1
  SET QOZF = REPLACE(QOZF,"1","Yes")')
dbGetQuery(mydb, 'UPDATE financing_1
  SET QOZF = REPLACE(QOZF,"2","No")')

#create dummy column

#dbGetQuery(mydb, 'ALTER TABLE financing_1
#ADD COLUMN No_Selection VARCHAR(15)')
#dbGetQuery(mydb, 'ALTER TABLE financing_1 RENAME COLUMN No_Selection TO No_finance_Selection')
#dbGetQuery(mydb, 'UPDATE financing_1 SET No_finance_Selection = "Yes"')

#assign variable for finance input

finance = dbGetQuery(mydb, "select HOME, TCAP, CDBG, HTF, HOPEVI, TCEP, QOZF, No_finance_Selection from financing_1")
select_finance = colnames(finance)

#create intro panel

intro_panel = tabPanel(
  "Welcome!",
  titlePanel(h1("Welcome to the LIHTC Database", align = "center")),
  br(),
  img(src = "https://image.shutterstock.com/image-photo/miniature-wooden-toy-houses-trees-600w-1374358496.jpg", style="display: block; margin-left: auto; margin-right: auto;"),
  br(),
  p("LIHTC properties are typically multifamily dwellings, such as apartment buildings, that are financed and constructed (or, in many cases, existing buildings are rehabilitated or renovated) through public-private partnerships. The organizations that construct and/or renovate and manage these buildings receive federal tax credits. LIHTC properties serve a very important purpose by providing affordable housing options to low-income individuals and families."),
  br(),
  p("Information in this database comes from the United States Department of Housing and Urban Development Office of Policy Development and Research. The database includes project address, number of units and low-income units, number of bedrooms, year the credit was allocated, year the project was placed in service, whether the project was new construction or rehab, type of credit provided, and other sources of project financing."),
  br(),
  p(a(href = "https://www.huduser.gov/portal/datasets/lihtc/property.html", "Access the original HUD LIHTC Database using this link."))
)

#create property search panel

prop_search = tabPanel("Property Search",
                       titlePanel("Property Search"),
                       sidebarLayout(
                         sidebarPanel(
                           selectInput(
                             inputId = "proj_st_id",
                             label = 'Select a state:',
                             choices = states$proj_st,
                             selected = "AL"
                           ),
                           selectInput(
                             inputId = 'table_br',
                             label = 'Select number of rooms',
                             choices = select_br,
                             selected = 'No_br_Selection'
                           ),
                           selectInput(
                             inputId = 'table_pop',
                             label = 'Select population served',
                             choices = select_pop,
                             selected = 'No_population_Selection'
                           ),
                           selectInput(
                             inputId = 'table_finance',
                             label = 'Select type of financing',
                             choices = select_finance,
                             selected = 'No_finance_Selection'
                           )
                         ),
                         mainPanel(
                           DTOutput(outputId = 'property'
                           )
                         )
                       )
)

#create property search map panel

prop_search_map = tabPanel(
  "Property Search Map",
  titlePanel("Property Search Map"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = 'map_st_id',
        label = 'Select a state:',
        choices = states$proj_st,
        selected = "AL"
      ),
      selectInput(
        inputId = 'map_br',
        label = 'Select number of rooms',
        choices = select_br,
        selected = 'No_br_Selection'
      ),
      selectInput(
        inputId = 'map_pop',
        label = 'Select population served',
        choices = select_pop,
        selected = 'No_population_Selection'
      ),
      selectInput(
        inputId = 'map_finance',
        label = 'Select type of financing',
        choices = select_finance,
        selected = 'No_finance_Selection'
      )
    ),
    mainPanel(
      plotlyOutput(
        outputId = 'map', width='100%', height='600px' )
    )
  )
)

#create ui

ui = fluidPage(theme = shinytheme("flatly"),
               navbarPage(
                 "Low-Income Housing Tax Credit Database",
                 intro_panel,
                 prop_search,
                 prop_search_map
               ))

#create server with outputs

server <- function(input, output, session) {
  output$property <- renderDT(
    data <- dbGetQuery(
      conn = mydb,
      statement = 
        paste0('SELECT p.project AS Project,
        p.proj_add AS Address,
        p.proj_cty AS City,
        p.proj_st AS State,
        p.proj_zip AS Zip,
        u.Total_Number_of_Low_Income_Units,
        u.Number_of_1br_Units,
        u.Number_of_2br_Units,
        u.Number_of_3br_Units,
        u.Number_of_4br_Units,
        t.Families,
        t.Elderly,
        t.Disabled,
        t.Homeless,
        t.Other,
        f.HOME,
        f.TCAP, 
        f.CDBG,
        f.HTF,
        f.HOPEVI,
        f.TCEP,
        f.QOZF
      FROM property_1 AS p
      JOIN unit_characteristics_1 AS u
                ON p.property_id = u.property_id
      JOIN target_pops_1 AS t
                ON p.property_id = t.property_id
      JOIN financing_1 AS f
                ON p.property_id = f.property_id
               WHERE proj_st =', input$proj_st_id, '
               AND ', input$table_br, '>= 1
               AND ', input$table_pop, ' = \'Yes\'
               AND ', input$table_finance, ' = \'Yes\''
        )
    )
  )
  
  output$map <- renderPlotly(
    {
      d2 = dbGetQuery(
        conn = mydb,
        statement = 
          paste0('SELECT s.latitude, s.longitude,
          p.project AS Project,
        p.proj_add AS Address,
        p.proj_cty AS City,
        p.proj_st AS State,
        p.proj_zip AS Zip,
        u.Total_Number_of_Low_Income_Units,
        u.Number_of_1br_Units,
        u.Number_of_2br_Units,
        u.Number_of_3br_Units,
        u.Number_of_4br_Units,
        t.Families,
        t.Elderly,
        t.Disabled,
        t.Homeless,
        t.Other,
        f.HOME,
        f.TCAP, 
        f.CDBG,
        f.HTF,
        f.HOPEVI,
        f.TCEP,
        f.QOZF
             FROM state_information_1 AS s
                INNER JOIN property_1 AS p on s.property_id = p.property_id
                INNER JOIN unit_characteristics_1 AS u ON s.property_id = u.property_id 
                INNER JOIN target_pops_1 AS t ON s.property_id = t.property_id
                INNER JOIN financing_1 AS f ON s.property_id = f.property_id
                 WHERE proj_st =', input$map_st_id, '
               AND ', input$map_br, '>= 1
               AND ', input$map_pop, ' = \'Yes\'
               AND ', input$map_finance, ' = \'Yes\''
          )
      )
      plot_geo(d2, lat = ~latitude, lon = ~longitude) %>%
        add_markers(text = ~paste(Project, Address, City, State, Zip, sep = "<br />"), symbol = I("circle"), size = I(8), color = I('SteelBlue'), hoverinfo = "text") %>%
        layout(title = 'Please wait as the map loads. It may take a few moments.<br>Hover the pointer over a symbol to see the property information.<br>Click and drag, or zoom in to better view the properties.', 
               geo = list(
                 scope = 'usa',
                 projection = list(type = 'albers usa'),
                 showland = TRUE,
                 landcolor = toRGB("gray85"),
                 subunitcolor = toRGB("gray75"),
                 countrycolor = toRGB("gray75"),
                 countrywidth = 0.5,
                 subunitwidth = 0.5
               )
        )
    } 
  )
}

#run app

shinyApp(ui, server)