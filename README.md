# GridEmissionsTracker
Shiny App for looking at the emissions of the UK's electricity grid by source, based on balancing reports mechanism data.
https://www.bmreports.com/bmrs/?q=help/about-us

The code takes the data from a URL that shows how much electricity the different generation types.

The emissions from each of these sources are then calculated using standard emissions factors.

It shows how the different generation types get used through out the day and how the emissions from our Electricity system change as a result.


This publicly available data is mainly used by the major energy companies to trade electricity
https://www.elexon.co.uk/wp-content/uploads/2015/10/beginners_guide_to_trading_arrangements_v5.0.pdf


but I also thought it was interesting so I started working on this


The tool can be accessed at
https://cdpsawbridge.shinyapps.io/GridEmissionsTracker/


On the todo list:

+add a live grid emissions intensity tracker plot, bascially a line of tCO2e/KWh.

+add weather information

+try and find total consumption figures, and use to infer emissions of unmetered generation.
