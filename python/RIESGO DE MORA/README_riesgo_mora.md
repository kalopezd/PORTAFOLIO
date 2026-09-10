# Predicción de Riesgo de Mora con Regresión Logística

Proyecto personal de clasificación en Python para predecir si un cliente entrará en mora, a partir de variables financieras básicas.

## Objetivo

Construir un modelo que identifique clientes con alto riesgo de mora, priorizando minimizar los falsos negativos (clientes en riesgo que el modelo no detecta), un error especialmente costoso en un contexto financiero.

## Datos

Dataset simulado de 500 clientes, generado con `numpy` para fines de práctica, con las siguientes variables:

- `edad`
- `ingresos`
- `deuda`
- `score_crediticio`
- `prestamos` (número de préstamos activos)
- `mora` (variable objetivo: 1 = en mora, 0 = al día)

La variable `mora` se construyó con una regla simple (deuda alta en relación a ingresos + score crediticio bajo), para simular un patrón realista de incumplimiento.

## Herramientas

- `pandas`, `numpy` — manejo y generación de datos
- `matplotlib`, `seaborn` — visualización exploratoria
- `scikit-learn` — modelado (regresión logística), partición de datos y métricas

## Proceso

1. **Análisis exploratorio (EDA)**: revisión de la distribución de la variable `mora` y relación entre variables (por ejemplo, ingresos vs. deuda).
2. **Detección de desbalance de clases**: solo el 24% de los clientes estaban en mora, lo que sesgaba el modelo hacia predecir "no mora" en todos los casos.
3. **Modelado**: regresión logística con `class_weight='balanced'` para corregir el desbalance y darle más peso a la clase minoritaria (clientes en mora).
4. **Evaluación**: exactitud, reporte de clasificación y matriz de confusión.

## Resultados

- **Accuracy**: 91%
- **Recall clase mora**: ~0.90 (mejoró significativamente tras aplicar `class_weight='balanced'`)
- La corrección del desbalance redujo los falsos negativos, es decir, clientes en riesgo que el modelo no habría detectado.

## Nota

Los datos son simulados y se usaron únicamente con fines de práctica y aprendizaje; no corresponden a información real de ningún banco o cliente.
