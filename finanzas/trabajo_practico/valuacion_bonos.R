library(quantmod)
library(fmdates)

getSymbols("ko", from = "2015-01-01", to = "2019-07-31")


# Datos del bono
precio_actual <- 95.50
valor_nominal <- 100
tasa_cupon <- 0.06
num_pagos_cupon <- 4
tasa_descuento_actual <- 0.08

# Función para calcular el precio del bono
precio_bono <- function(y) {
  (tasa_cupon / y) * (1 - (1 + y)^-num_pagos_cupon) + (valor_nominal / (1 + y)^num_pagos_cupon)
}
precio_bono(c(0, 1))

# Calculo de la tasa de rendimiento actual
tasa_rendimiento_actual <- uniroot(precio_bono, c(0, 1))$root

# Salida del resultado
cat("La tasa de rendimiento actual del bono es:", round(tasa_rendimiento_actual, 4))



# Valor actual de un bono

# Datos del bono
valor_nominal <- 100
tasa_cupon_anual <- 0.12
num_pagos_cupon_anual <- 2
tasa_descuento <- 0.10

# Datos calculados
tiempo_vencimiento <- 2.5
pagos_cupon_inicial <- 0.5
# Cálculo del valor presente de los flujos de efectivo del cupón

flujo_efectivo_cupon <- rep(valor_nominal * tasa_cupon_anual / num_pagos_cupon_anual, ceiling(tiempo_vencimiento))

valor_presente_cupon <- flujo_efectivo_cupon / (1 + tasa_descuento / num_pagos_cupon_anual)^(pagos_cupon_inicial:tiempo_vencimiento)


# Cálculo del valor presente del valor nominal al vencimiento
valor_presente_vn <- valor_nominal / (1 + tasa_descuento)^tiempo_vencimiento

# Cálculo del valor actual del bono
valor_actual_bono <- sum(c(valor_presente_cupon, valor_presente_vn))

# Salida del resultado
cat("El valor actual del bono es:", round(valor_actual_bono, 2))


year_frac("2020-05-15", "2020-08-15", "30/360us")
0.25 * 360 / 180
