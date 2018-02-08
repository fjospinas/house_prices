# Carga de datos
library(openxlsx)
library(ggplot2)

# Directorio de trabajo y archivo de datos
workdir = 'PATH'
file = 'Houses_data.xlsx'

# Estructuras de carpetas para los plots
dir.create(file.path(workdir, "plots"), showWarnings = FALSE)

# Carga de datos
df = openxlsx::read.xlsx(file.path(workdir, file))
attach(df)



# Add new vars
r = sqrt(coordx^2 + coordy^2)
theta = atan2(coordy, coordx)
square = cut(theta, breaks = c(-pi, -pi/2, 0, pi/2, pi), labels = c(3, 4, 1, 2))
df = cbind(df, theta, square)

# Graficos descriptivos
gsub(pattern = "xvarx", replacement = var, x = "Histograma de la variable \n\"xvarx\"")

cols = colnames(df)
cat(paste(cols, collapse = "\", \""))
continuous_vars = c("crimen", "residencial", "industrial", "rio", "oxidonitroso", "cuartos", "distanciaempleo", "movilidad", "impuestos", "tasaeducativa", "pobreza", "valor", "coordx", "coordy")
categorical_vars = c()

for(var in continuous_vars){
  print(var)
  ggplot(df, aes_string(var))+
    geom_histogram(fill="#0066CC", colour="black") +
    labs(y = "Conteo", title=gsub(pattern = "xvarx", replacement = var, x = "Histograma de la variable \n\"xvarx\""))+
    theme(plot.title = element_text(hjust = 0.5))
  ggsave(file.path(workdir, "plots", gsub("xvarx", var, "xvarx.png")), width = 20, height = 20, units = "cm")
}

ggplot(df, aes(x=coordx, y=coordy)) + 
  geom_point() +
  labs(y = "Coordenada y", x = "Coordenada x", title= "Coordenadas respecto al centro de la ciudad\n punto (0,0)")+
  theme(plot.title = element_text(hjust = 0.5))
ggsave(file.path(workdir, "plots", "scatter_positions.png"), width = 20, height = 20, units = "cm")

# Regresión normal

#Validación de supuestos distribucionales
shapiro.test(df$valor) # La distrubición de Y no es normal

cols = colnames(df)
paste(cols, collapse = ' + ')

linear_model = lm(valor ~ crimen + residencial + industrial + rio + oxidonitroso + cuartos + distanciaempleo + movilidad + impuestos + tasaeducativa + pobreza + r + square)
summary(linear_model)
plot(density(linear_model$residuals))
shapiro.test(linear_model$residuals) # Tampoco hay normalidad sobre los residuales

par(mfrow=c(2,2))
plot(linear_model)
mean(linear_model$residuals)

# Regresión Gamma
par(mfrow=c(1,1))
plot(density(df$valor))

# First model
gamma_glm1 = glm(valor ~ crimen + residencial + industrial + factor(rio) + oxidonitroso + cuartos + distanciaempleo + movilidad + impuestos + tasaeducativa + pobreza +  r + square, family = Gamma, data = df)
summary(gamma_glm1)

# Second model
gamma_glm2 = glm(valor ~ crimen + residencial + industrial + factor(rio) + oxidonitroso + cuartos + distanciaempleo + movilidad + impuestos + tasaeducativa + pobreza , family = Gamma, data = df)
summary(gamma_glm2)

# Second model
gamma_glm3 = glm(valor ~ crimen + factor(rio) + oxidonitroso + cuartos + distanciaempleo + movilidad + impuestos + tasaeducativa + pobreza , family = Gamma, data = df)
summary(gamma_glm3)

# Second model
gamma_glm4 = glm(valor ~ -1 +crimen  + oxidonitroso + cuartos + distanciaempleo + factor(movilidad) + impuestos + tasaeducativa + pobreza , family = Gamma, data = df)
summary(gamma_glm4)
plot(gamma_glm4)

table(df$residencial)

