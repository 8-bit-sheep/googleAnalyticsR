# contents of a file
## ui.R ##
htmlTemplate("template.html",
             button = actionButton("action", "Action"),
             slider = sliderInput("x", "X", 1, 100, 50)
)