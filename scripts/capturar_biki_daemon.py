#!/usr/bin/env python3

import csv
import json
import os
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

INTERVALO = 30

COLUMNAS = [
    "station_id",
    "nombre",
    "mecanicas",
    "electricas",
    "timestamp",
]


def get_gbfs(feed, intentos=3):
    url = f"{BASE_URL}/{feed}"
    ultimo_error = None

    for intento in range(intentos):
        try:
            req = urllib.request.Request(
                url,
                headers={
                    "User-Agent": "biki-valladolid-historico/1.0"
                },
            )

            with urllib.request.urlopen(req, timeout=30) as r:
                return json.load(r)

        except Exception as e:
            ultimo_error = e

            if intento < intentos - 1:
                espera = 2 * (intento + 1)

                print(
                    f"Error leyendo {feed}: {e}. "
                    f"Reintentando en {espera}s...",
                    flush=True,
                )

                time.sleep(espera)

    raise RuntimeError(
        f"No se pudo leer {url}: {ultimo_error}"
    )


def nombre_tipo(vt):
    n = vt.get("name") or ""

    if isinstance(n, list):
        n = n[0].get("text", "") if n else ""

    return n


def clasificar(vt):
    prop = (vt.get("propulsion_type") or "").lower()

    if prop == "human":
        return "mec"

    if prop.startswith("electric"):
        return "ele"

    nombre = nombre_tipo(vt).lower()

    if "electr" in nombre or nombre.startswith("e-"):
        return "ele"

    return "otro"


def cargar_metadata():
    print("Cargando información de estaciones...", flush=True)

    info = get_gbfs("station_information")["data"]["stations"]

    print("Cargando tipos de vehículo...", flush=True)

    tipos = get_gbfs("vehicle_types")["data"]["vehicle_types"]

    nombres = {
        str(s["station_id"]): s.get("name", "")
        for s in info
    }

    clase = {
        t["vehicle_type_id"]: clasificar(t)
        for t in tipos
    }

    print(
        f"Metadata cargada: {len(nombres)} estaciones, "
        f"{len(tipos)} tipos de vehículo.",
        flush=True,
    )

    return nombres, clase


def obtener_snapshot(nombres, clase):
    status = get_gbfs("station_status")["data"]["stations"]

    if not status:
        raise RuntimeError(
            "station_status no devolvió estaciones."
        )

    ahora = datetime.now(
        ZoneInfo("Europe/Madrid")
    ).strftime("%Y-%m-%d %H:%M:%S")

    filas = []
    estado = {}

    sin_clasificar = 0

    for s in status:

        sid = str(s["station_id"])

        mec = 0
        ele = 0

        disponibles = s.get("vehicle_types_available")

        if disponibles:

            for v in disponibles:

                cantidad = v.get("count", 0) or 0

                c = clase.get(
                    v["vehicle_type_id"],
                    "otro",
                )

                if c == "mec":
                    mec += cantidad

                elif c == "ele":
                    ele += cantidad

                else:
                    sin_clasificar += cantidad

        else:

            ele = (
                s.get("num_ebikes_available", 0)
                or 0
            )

            mec = max(
                (
                    s.get("num_bikes_available", 0)
                    or 0
                ) - ele,
                0,
            )

        estado[sid] = (mec, ele)

        filas.append(
            [
                sid,
                nombres.get(sid, sid),
                mec,
                ele,
                ahora,
            ]
        )

    return filas, estado, sin_clasificar


def leer_ultimo_estado():
    if (
        not os.path.exists(CSV_PATH)
        or os.path.getsize(CSV_PATH) == 0
    ):
        return None

    ultimo_timestamp = None
    estado = {}

    try:

        with open(
            CSV_PATH,
            encoding="utf-8",
            newline="",
        ) as f:

            reader = csv.DictReader(f)

            for row in reader:

                timestamp = row.get("timestamp")

                if not timestamp:
                    continue

                if timestamp != ultimo_timestamp:
                    estado = {}
                    ultimo_timestamp = timestamp

                estado[str(row["station_id"])] = (
                    int(row["mecanicas"]),
                    int(row["electricas"]),
                )

    except Exception as e:

        print(
            f"AVISO: no se pudo recuperar el último estado: {e}",
            flush=True,
        )

        return None

    if estado:
        print(
            f"Último estado recuperado: "
            f"{len(estado)} estaciones "
            f"({ultimo_timestamp}).",
            flush=True,
        )

    return estado or None


def mes_del_archivo(path):
    with open(
        path,
        encoding="utf-8",
        newline="",
    ) as f:

        next(f, None)
        fila = next(f, "")

    if not fila.strip():
        return None

    ts = next(csv.reader([fila]))[-1]

    return ts[:7] if len(ts) >= 7 else None


def actualizar_indice():

    archivos = sorted(
        n
        for n in os.listdir(ARCHIVO_DIR)
        if n.startswith("historico_")
        and n.endswith(".csv")
    )

    with open(
        os.path.join(
            ARCHIVO_DIR,
            "indice.txt",
        ),
        "w",
        encoding="utf-8",
    ) as f:

        if archivos:
            f.write(
                "\n".join(archivos)
                + "\n"
            )


def rotar_si_cambia_mes(mes_actual):

    if (
        not os.path.exists(CSV_PATH)
        or os.path.getsize(CSV_PATH) == 0
    ):
        return

    mes = mes_del_archivo(CSV_PATH)

    if not mes or mes == mes_actual:
        return

    os.makedirs(
        ARCHIVO_DIR,
        exist_ok=True,
    )

    destino = os.path.join(
        ARCHIVO_DIR,
        f"historico_{mes}.csv",
    )

    if os.path.exists(destino):

        with open(
            CSV_PATH,
            encoding="utf-8",
            newline="",
        ) as src, open(
            destino,
            "a",
            encoding="utf-8",
            newline="",
        ) as dst:

            next(src, None)
            dst.write(src.read())

        os.remove(CSV_PATH)

    else:

        os.replace(
            CSV_PATH,
            destino,
        )

    actualizar_indice()

    print(
        f"Rotado {mes} -> {destino}",
        flush=True,
    )


def guardar_snapshot(filas):

    nuevo = (
        not os.path.exists(CSV_PATH)
        or os.path.getsize(CSV_PATH) == 0
    )

    os.makedirs(
        os.path.dirname(CSV_PATH) or ".",
        exist_ok=True,
    )

    with open(
        CSV_PATH,
        "a",
        newline="",
        encoding="utf-8",
    ) as f:

        writer = csv.writer(
            f,
            lineterminator="\n",
        )

        if nuevo:
            writer.writerow(COLUMNAS)

        writer.writerows(filas)


def main():

    print(
        "========================================",
        flush=True,
    )

    print(
        "Capturador continuo BIKI Valladolid",
        flush=True,
    )

    print(
        f"Intervalo: {INTERVALO} segundos",
        flush=True,
    )

    print(
        f"CSV: {CSV_PATH}",
        flush=True,
    )

    print(
        "========================================",
        flush=True,
    )

    while True:

        try:

            nombres, clase = cargar_metadata()

            break

        except Exception as e:

            print(
                f"ERROR cargando metadata: {e}",
                flush=True,
            )

            print(
                "Reintentando en 30 segundos...",
                flush=True,
            )

            time.sleep(INTERVALO)

    estado_anterior = leer_ultimo_estado()

    while True:

        inicio = time.time()

        try:

            ahora = datetime.now(
                ZoneInfo("Europe/Madrid")
            )

            mes_actual = ahora.strftime("%Y-%m")

            rotar_si_cambia_mes(mes_actual)

            filas, estado_actual, sin_clasificar = (
                obtener_snapshot(
                    nombres,
                    clase,
                )
            )

            if (
                estado_anterior is None
                or estado_actual != estado_anterior
            ):

                guardar_snapshot(filas)

                total_mec = sum(
                    fila[2]
                    for fila in filas
                )

                total_ele = sum(
                    fila[3]
                    for fila in filas
                )

                timestamp = filas[0][4]

                print(
                    f"[{timestamp}] CAMBIO -> "
                    f"{len(filas)} estaciones guardadas. "
                    f"Total: {total_mec} mecánicas, "
                    f"{total_ele} eléctricas.",
                    flush=True,
                )

                if sin_clasificar:

                    print(
                        f"AVISO: {sin_clasificar} bicicletas "
                        f"de tipos sin clasificar.",
                        flush=True,
                    )

                estado_anterior = estado_actual

            else:

                timestamp = filas[0][4]

                print(
                    f"[{timestamp}] Sin cambios.",
                    flush=True,
                )

        except Exception as e:

            print(
                f"ERROR durante la captura: {e}",
                flush=True,
            )

        transcurrido = time.time() - inicio

        espera = max(
            0,
            INTERVALO - transcurrido,
        )

        time.sleep(espera)


if __name__ == "__main__":
    main()
