#!/bin/bash

set -euo pipefail

cd /home/sergioh93/flujosbiki

# Evitar dos sincronizaciones simultáneas
exec 9>/tmp/biki-sync.lock
flock -n 9 || exit 0

# Añadir únicamente los datos (incluye historico.csv e históricos diarios archivados)
git add data

# Si no hay cambios, no hacer nada
if git diff --cached --quiet; then
    exit 0
fi

# Crear el commit local con las capturas
git commit -m "Capturas BIKI $(TZ=Europe/Madrid date '+%Y-%m-%d %H:%M')"

# GitHub puede haber recibido cambios externos (por ejemplo, una actualización
# de app.R desde GitHub). Reaplicar nuestro commit de datos sobre main antes
# de hacer push para evitar que una actualización externa bloquee el servicio.
git pull --rebase origin main

# Subir las capturas ya rebasadas.
git push origin main
