#Importamos librerías necesarias
#cd carpeta con los archivo asociado a la API(En este caso es:"C:\Users\karen\Desktop\Proyecto API")
#.\.venv\Scripts\python.exe -m fastapi dev#CORRER API
# Luego abre:      http://127.0.0.1:8000/docs
#Nota_Debemos instalar la librerias en entomo vitual:
# .\.venv\Scripts\python.exe -m pip install "libreria a instalar"
from datetime import timedelta#Sumar o restar fechas
import time
import pandas as pd
import traceback
from enum import Enum#Lista cerrada
from fastapi import FastAPI, HTTPException#API
from trm import data_trm #Importamo clase del archivo trm

cache_segundos=3600#refresca cada hora
ultima_carga=0



#Creamos una clase periodo
class periodos (str,Enum):
    semana="semana"
    mes="mes"
    anio="anio"

DIAS={"semana":7,"mes":30,"anio":365}
app=FastAPI(title="API TRM")
fetcher=data_trm(limit=1000)

def obtener_datos():
    global ultima_carga
    if fetcher.data is None or time.time() -ultima_carga>cache_segundos:
        fetcher.fetch_data()
        ultima_carga=time.time()
    if fetcher.data is None or fetcher.data.empty:#503,servicio no disponible": la API de origen falló
        raise HTTPException(status_code=503,detail="No se encontraron registros")
    return fetcher.data
@app.get("/")
def saludo ():
    return{ "Message":"API de la TRM"}

@app.get("/trm/ultima")
def trm_ultima():
    df=obtener_datos()
    fila=df.iloc[0]
    return {
        "fecha":fila["fecha_desde"].strftime("%Y-%m-%d"),#Metodo de instancia  para datetime
        "trm COP": float(fila["trm_COP"])
    }

@app.get("/trm/historico")
def trm_historico(limit:int=5):
    if limit <1  or limit >500:
        raise HTTPException(status_code=400,detail="Limit entre 1 y 500")
    df=obtener_datos().head(limit)
    return [
        {"fecha_desde":f.strftime("%Y-%m-%d"),"trm_cop":float(v)}
        for f, v in zip(df["fecha_desde"], df["trm_COP"])
    ]

@app.get("/trm/resumen/{periodo}")#variable
def trm_resumen(periodo: periodos):#valor url y Enum
    df = obtener_datos()
    corte = df["fecha_desde"].max() - timedelta(days=DIAS[periodo.value])
    ventana = df[df["fecha_desde"] >= corte]
    primero=float(ventana.iloc[-1]["trm_COP"])
    ultimo=float(ventana.iloc[0]["trm_COP"])
    return {
        "periodo":periodo.value,
        "registros":len(ventana),
        "promedio":round(float(ventana["trm_COP"].mean()),2),
        "minimo":float(ventana["trm_COP"].min()),
        "maximo":float(ventana["trm_COP"].max()),
        "variacion_pct":round((ultimo/primero-1)*100,2),


    }
import traceback

@app.get("/convertir")
def convertir(usd: float, fecha: str | None = None):
    if usd <= 0:
        raise HTTPException(status_code=400, detail="USD debe ser mayor a 0")
    df = obtener_datos()

    if fecha:
        try:
            f = pd.to_datetime(fecha)
        except (ValueError, TypeError):
            raise HTTPException(status_code=400, detail="Fecha no válida. Use formato AAAA-MM-DD")
        filas = df[df["fecha_desde"] <= f]
        if filas.empty:
            raise HTTPException(status_code=404, detail="No hay TRM para esa fecha")
        fila = filas.iloc[0]
    else:
        fila = df.iloc[0]

    trm = float(fila["trm_COP"])
    return {
        "usd": usd,
        "trm_usada": trm,
        "fecha_trm": fila["fecha_desde"].strftime("%Y-%m-%d"),
        "total_cop": round(usd * trm, 2),
    }