#!/bin/bash
#====================================================================
# SIMULATION JCL PJ01CLT sur Mac
# Equivalent des etapes IEBGENER et IDCAMS
#====================================================================

echo "========================================"
echo "  SIMULATION JCL PJ01CLT"
echo "========================================"
echo ""

# Couleurs
GREEN='\033[0;32m'
RED='\033[0;31m'
NC='\033[0m' # No Color

# Dossier de donnees
DATA_DIR="data"
mkdir -p $DATA_DIR

#--------------------------------------------------------------------
# ETAPE 1: Equivalent IEBGENER - Creation du fichier PS
#--------------------------------------------------------------------
echo "ETAPE1 - IEBGENER: Creation fichier CLIENT"

# Extraire les donnees inline du JCL
grep -E "^[0-9]{3}[0-9]{2}[0-9]{2}" PJ01CLT.jcl > $DATA_DIR/CLIENT.dat

if [ $? -eq 0 ]; then
    COUNT=$(wc -l < $DATA_DIR/CLIENT.dat | tr -d ' ')
    echo -e "  ${GREEN}RC=0000${NC} - $COUNT enregistrements crees"
else
    echo -e "  ${RED}RC=0012${NC} - Erreur extraction"
    exit 1
fi

#--------------------------------------------------------------------
# ETAPE 2: Equivalent IDCAMS DEFINE - Definition ESDS (simule)
#--------------------------------------------------------------------
echo ""
echo "ETAPE2 - IDCAMS DEFINE CLUSTER: Definition ESDS"
echo "  (Simule - GnuCOBOL utilise des fichiers sequentiels)"
echo -e "  ${GREEN}RC=0000${NC} - ESDS simule pret"

#--------------------------------------------------------------------
# ETAPE 3: Equivalent IDCAMS REPRO - Chargement (deja fait)
#--------------------------------------------------------------------
echo ""
echo "ETAPE3 - IDCAMS REPRO: Chargement donnees"
echo -e "  ${GREEN}RC=0000${NC} - Donnees chargees dans $DATA_DIR/CLIENT.dat"

#--------------------------------------------------------------------
# ETAPE 4: Equivalent IDCAMS PRINT - Affichage
#--------------------------------------------------------------------
echo ""
echo "ETAPE4 - IDCAMS PRINT: Affichage du contenu"
echo "----------------------------------------"
cat -n $DATA_DIR/CLIENT.dat
echo "----------------------------------------"
echo -e "  ${GREEN}RC=0000${NC} - Print termine"

#--------------------------------------------------------------------
# RESUME
#--------------------------------------------------------------------
echo ""
echo "========================================"
echo "  JOB PJ01CLT TERMINE"
echo "  Fichier cree: $DATA_DIR/CLIENT.dat"
echo "  Taille: $(wc -c < $DATA_DIR/CLIENT.dat) octets"
echo "  Records: $(wc -l < $DATA_DIR/CLIENT.dat)"
echo "========================================"
