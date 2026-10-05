#!/bin/bash

set -e

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

# Crear commit
git commit -m "Capturas BIKI $(TZ=Europe/Madrid date '+%Y-%m-%d %H:%M')"

# El capturador puede seguir escribiendo historico.csv después del commit.
# No hacemos pull --rebase aquí porque exigiría un working tree limpio y
# bloquearía la sincronización mientras el daemon sigue capturando.
git push origin main
