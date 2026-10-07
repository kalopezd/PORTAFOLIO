#Buscamos el endpoint acerca de los datos de  la TRM
#https://www.datos.gov.co/resource/32sa-8pi3.json ENDPOINT ECONTRADO
import requests#Librería para realizar peticiones HTTP
import pandas as pd
from datetime import datetime
#Creamos una clase que me permita crear un objeto para descargar el archivo TRM
class data_trm:
    trm = "https://www.datos.gov.co/resource/32sa-8pi3.json"#RUTA DEL ARCHIVO JSON(ENDPOINT)
    def __init__(self,limit:int=1000):#indicammos las caracteristicas de mi objeto
        self.limit=limit#Cuando consulte los datos de TRM, obtendré 1000 registros
        self.data=None
    def fetch_data(self, order_by: str = "vigenciadesde DESC") -> pd.DataFrame:#lo que consultemos lo convertimos en dataframe
            params = {
                "$limit": self.limit,
                "$order": order_by,#Definimos los parametro de consulta
                }
            try:
                response=requests.get(self.trm,params=params,timeout=15)
                response.raise_for_status()#Indicamos advertencias
            except requests.exceptions.RequestException as e:
                print(f"Error a conectar con la API:{e}")
                return pd.DataFrame()
            registros = response.json()#convierte regitros en formato json

            if not registros:
                print("La API no devolvió registros")
                return pd.DataFrame()
            df=pd.DataFrame(registros)
            self.data=self._clean_data(df)
            return self.data
    def _clean_data(self,df:pd.DataFrame) -> pd.DataFrame :
        df=df.copy()
        if "valor" in df.columns:
            df['valor']=pd.to_numeric(df["valor"],errors="coerce")
        for col in ["vigenciadesde","vigenciahasta"]:
            if col in df.columns:
                df[col]=pd.to_datetime(df[col],errors="coerce")
        df=df.rename(columns={
                "valor":"trm_COP",
                "vigenciadesde":"fecha_desde",
                "vigenciahasta":"fecha_hasta",
                })
        df=df.dropna(subset="trm_COP")
        return df.sort_values("fecha_desde",ascending=False).reset_index(drop=True)
    def get_latest(self) -> float:
        if self.data is None or self.data.empty:
            raise ValueError( "Primero debe ejecutar fetch_data()")
        return  self.data.iloc[0]["trm_COP"]#Me aseguro de que tengo un dataframe no vacío
    #antes de obtener el ultimo valor de trm

if __name__=="__main__":
    fetcher=data_trm(limit=1000)
    df_trm=fetcher.fetch_data()
    if not df_trm.empty:
        print(f"Se descargaron {len(df_trm)} registros")
        print(f"TRM reciente: ${fetcher.get_latest():,.2f} COP")
        print(df_trm.head())
        df_trm.to_csv("trm_historico.csv", index=False)
        print("Datos guardados en trm_historico.csv")


