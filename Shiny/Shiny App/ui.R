shinyUI(fluidPage(
    titlePanel("My Shiny App"),
    sidebarLayout(
        sidebarPanel(
        h3("Menu")),
        mainPanel(
            h1("First level title"),
            h2("Second level title"),
            p("This famous (Fisher’s or Anderson’s)",
              a("iris",
                href="http://stat.ethz.ch/R-manual/R-devel/library/datasets/html/iris.html") ,
              "data set gives the measurements in centimeters of the variables sepal length and width and petal length and width, respectively, for 50 flowers from each of 3 species of iris.  The species are",
              em("Iris setosa,"),
            br(),
            h2("Analysis"
              strong("versicolor,"),
              "and",
              em("virginica.")),
            br(),
            h2("Analysis")))))
