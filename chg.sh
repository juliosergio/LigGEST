#! /bin/bash
# ===============
# JSS: chg.sh
#     Cambia la base de datos por una recientemente
#     descargadas
# USO:
#      ./chg.sh MisLinks
# ================
nam="${1}.rds"
rback.sh $nam ~checo/Descargas/${nam}
