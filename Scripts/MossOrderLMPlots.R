# Figures for Napier BIOL 385 Paper (Fall 2020)
# Hailey Napier
# December 2020

# ****Run MossOrderLM.R first for data****

# 1.0 Plot MAT and MAP pointrange plots
MATdf <-OrderCoefPlotDF %>%
  filter(OrderCoefPlotDF$Parameter == "log1p(MAT_Kelvin)")

OrdCoefPlot_MAT <- ggplot() + 
  geom_pointrange(data = MATdf, 
                  aes(x = Order, 
                      y = Coefficient, 
                      ymin = LowLim, 
                      ymax = UpperLim), 
                  col = "black") +
  geom_errorbar(data = MATdf, 
                aes(x = Order, 
                    y = Coefficient, 
                    ymin = LowLim, 
                    ymax = UpperLim), 
                width = 0.5, 
                col = "black")  +
  geom_hline(aes(yintercept = 0), linetype = "dashed", col = "blue") +
  ylab("β log(MAT)") +
  ggtitle("Temperature Coefficients") + 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5)) 

OrdCoefPlot_MAT


MAPdf <- OrderCoefPlotDF %>%
  filter(OrderCoefPlotDF$Parameter == "log1p(MAP)")

OrdCoefPlot_MAP <- ggplot() + 
  geom_pointrange(data = MAPdf, 
                  aes(x = Order, 
                      y = Coefficient, 
                      ymin = LowLim, 
                      ymax = UpperLim), 
                  col = "black") +
  geom_errorbar(data = MAPdf, 
                aes(x = Order, 
                    y = Coefficient, 
                    ymin = LowLim, 
                    ymax = UpperLim), 
                width = 0.5, 
                col = "black")  +
  geom_hline(aes(yintercept = 0), linetype = "dashed", col = "blue") +
  ylab("β log(MAP)") +
  ggtitle("Precipitation Coefficients") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  
        plot.title = element_text(hjust = 0.5))

OrdCoefPlot_MAP

