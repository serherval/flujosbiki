#!/usr/bin/env python3
"""
Captura un snapshot de BIKI Valladolid (feed GBFS oficial) y lo añade a
data/historico.csv con las columnas que lee la app Shiny:
    station_id, nombre, mecanicas, electricas, timestamp

Replica la lógica de la versión anterior de la app en R:
  - station_information -> nombre de cada parada
  - vehicle_types       -> propulsion_type "human" = mecánica, "electric" = eléctrica
  - station_status      -> vehicle_types_available (nº de bicis por tipo)

Rotación mensual: historico.csv contiene solo el mes en curso. Cuando cambia
el mes, el archivo se mueve a data/archivo/historico_AAAA-MM.csv y se regenera
data/archivo/indice.txt (lista de archivos, para que la app los pueda ofrecer).

Solo usa la biblioteca estándar de Python (no hay que instalar nada).
"""

import csv
import json
import os
import sys
import time
import urllib.request
from datetime import datetime
from zoneinfo import ZoneInfo

BASE_URL = (
    os.environ.get("GBFS_BASE_URL")
    or "https://valladolid.publicbikesystem.net/customer/gbfs/v2/es"
).rstrip("/")
CSV_PATH = os.environ.get("CSV_PATH") or "data/historico.csv"
ARCHIVO_DIR = os.path.join(os.path.dirname(CSV_PATH) or ".", "archivo")
COLUMNAS = ["station_id", "nombre", "mecanicas", "electricas", "timestamp"]


def get_gbfs(feed, intentos=3):
    url = f"{BASE_URL}/{feed}"
    ultimo_error = None
    for i in range(intentos):
        try:
            req = urllib.request.Request(
                url, headers={"User-Agent": "biki-valladolid-historico/1.0"}
            )
            with urllib.request.urlopen(req, timeout=30) as r:
                return json.load(r)
        except Exception as e:  # noqa: BLE001
            ultimo_error = e
            time.sleep(2 * (i + 1))
    sys.exit(f"No se pudo leer {url}: {ultimo_error}")


def mes_del_archivo(path):
    """Mes (AAAA-MM) de la primera fila de datos del CSV; timestamp = última columna."""
    with open(path, encoding="utf-8", newline="") as f:
        next(f, None)  # cabecera
        fila = next(f, "")
    if not fila.strip():
        return None
    ts = next(csv.reader([fila]))[-1]
    return ts[:7] if len(ts) >= 7 else None


def rotar_si_cambia_mes(mes_actual):
    if not os.path.exists(CSV_PATH) or os.path.getsize(CSV_PATH) == 0:
        return
    mes = mes_del_archivo(CSV_PATH)
    if not mes or mes == mes_actual:
        return
    os.makedirs(ARCHIVO_DIR, exist_ok=True)
    destino = os.path.join(ARCHIVO_DIR, f"historico_{mes}.csv")
    if os.path.exists(destino):
        # Ya existía un archivo de ese mes (p. ej. relanzado a mano): se une.
        with open(CSV_PATH, encoding="utf-8", newline="") as src, open(
            destino, "a", encoding="utf-8", newline=""
        ) as dst:
            next(src, None)  # sin cabecera duplicada
            dst.write(src.read())
        os.remove(CSV_PATH)
    else:
        os.replace(CSV_PATH, destino)
    archivos = sorted(
        n for n in os.listdir(ARCHIVO_DIR)
        if n.startswith("historico_") and n.endswith(".csv")
    )
    with open(os.path.join(ARCHIVO_DIR, "indice.txt"), "w", encoding="utf-8") as f:
        f.write("\n".join(archivos) + "\n")
    print(f"Rotado {mes} -> {destino}")


def main():
    info = get_gbfs("station_information")["data"]["stations"]
    tipos = get_gbfs("vehicle_types")["data"]["vehicle_types"]
    status = get_gbfs("station_status")["data"]["stations"]

    nombres = {str(s["station_id"]): s.get("name", "") for s in info}
    propulsion = {t["vehicle_type_id"]: t.get("propulsion_type", "") for t in tipos}
    ahora = datetime.now(ZoneInfo("Europe/Madrid")).strftime("%Y-%m-%d %H:%M:%S")

    filas = []
    for s in status:
        sid = str(s["station_id"])
        mec = ele = 0
        disponibles = s.get("vehicle_types_available")
        if disponibles:
            for v in disponibles:
                p = propulsion.get(v["vehicle_type_id"], "")
                if p == "human":
                    mec += v["count"]
                elif p == "electric":
                    ele += v["count"]
        else:
            # Alternativa si la estación no trae el desglose por tipo
            ele = s.get("num_ebikes_available", 0) or 0
            mec = max((s.get("num_bikes_available", 0) or 0) - ele, 0)
        filas.append([sid, nombres.get(sid, sid), mec, ele, ahora])

    if not filas:
        sys.exit("El feed no devolvió estaciones; no se escribe nada.")

    rotar_si_cambia_mes(ahora[:7])

    nuevo = not os.path.exists(CSV_PATH) or os.path.getsize(CSV_PATH) == 0
    os.makedirs(os.path.dirname(CSV_PATH) or ".", exist_ok=True)
    with open(CSV_PATH, "a", newline="", encoding="utf-8") as f:
        w = csv.writer(f, lineterminator="\n")
        if nuevo:
            w.writerow(COLUMNAS)
        w.writerows(filas)

    print(f"{len(filas)} estaciones añadidas a {CSV_PATH} ({ahora})")


if __name__ == "__main__":
    main()
