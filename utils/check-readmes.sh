#!/bin/bash
#
# check-readmes.sh - Vérifie la cohérence des READMEs avec le contenu réel
#
# Usage: ./utils/check-readmes.sh
#

# Couleurs
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Répertoire racine du projet
PROJECT_ROOT="$(cd "$(dirname "$0")/.." && pwd)"

echo -e "${BLUE}========================================${NC}"
echo -e "${BLUE}   Audit des READMEs - poei-cobol${NC}"
echo -e "${BLUE}========================================${NC}"
echo ""

ERRORS=0
WARNINGS=0

# Fonction pour compter les fichiers
count_files() {
    local dir="$1"
    local pattern="$2"
    find "$dir" -name "$pattern" 2>/dev/null | wc -l | tr -d ' '
}

# Fonction pour afficher le statut
print_status() {
    local status="$1"
    local message="$2"
    case $status in
        "OK")
            echo -e "  ${GREEN}✅${NC} $message"
            ;;
        "WARN")
            echo -e "  ${YELLOW}⚠️ ${NC} $message"
            ((WARNINGS++))
            ;;
        "ERROR")
            echo -e "  ${RED}❌${NC} $message"
            ((ERRORS++))
            ;;
    esac
}

echo -e "${BLUE}1. Module Z/OS TSO${NC}"
echo "   Répertoire: cours/zos-tso/"
ZOS_COURS_COUNT=$(count_files "$PROJECT_ROOT/cours/zos-tso" "*.md")
ZOS_COURS_COUNT=$((ZOS_COURS_COUNT - 1)) # Exclure README.md
if [ "$ZOS_COURS_COUNT" -eq 5 ]; then
    print_status "OK" "Chapitres cours: $ZOS_COURS_COUNT fichiers"
else
    print_status "WARN" "Chapitres cours: $ZOS_COURS_COUNT fichiers (attendu: 5)"
fi

# Vérifier les fichiers manquants
for i in 01 02 03 04 05; do
    if [ ! -f "$PROJECT_ROOT/cours/zos-tso/$i-"*.md ]; then
        print_status "ERROR" "Chapitre $i manquant"
    fi
done

echo ""
echo -e "${BLUE}2. Module JCL${NC}"
echo "   Répertoire: cours/jcl/"
JCL_COURS_COUNT=$(count_files "$PROJECT_ROOT/cours/jcl" "*.md")
JCL_COURS_COUNT=$((JCL_COURS_COUNT - 1)) # Exclure README.md
if [ "$JCL_COURS_COUNT" -ge 1 ]; then
    print_status "OK" "Chapitres cours: $JCL_COURS_COUNT fichiers"
else
    print_status "WARN" "Aucun chapitre JCL trouvé"
fi

# Vérifier chapitres 01, 02 et 03
for i in 01 02 03; do
    if [ -f "$PROJECT_ROOT/cours/jcl/$i-"*.md ]; then
        print_status "OK" "Chapitre $i présent"
    else
        print_status "WARN" "Chapitre $i manquant"
    fi
done

# Vérifier exercices JCL
JCL_EX_COUNT=$(count_files "$PROJECT_ROOT/exercices/jcl" "*.md")
JCL_EX_COUNT=$((JCL_EX_COUNT - 1)) # Exclure README.md
if [ "$JCL_EX_COUNT" -ge 1 ]; then
    print_status "OK" "Exercices: $JCL_EX_COUNT fichiers"
else
    print_status "WARN" "Aucun exercice JCL trouvé"
fi

echo ""
echo -e "${BLUE}3. Module COBOL - Cours${NC}"
echo "   Répertoire: cours/cobol/"
COBOL_COURS_COUNT=$(count_files "$PROJECT_ROOT/cours/cobol" "*.md")
COBOL_COURS_COUNT=$((COBOL_COURS_COUNT - 1)) # Exclure README.md
if [ "$COBOL_COURS_COUNT" -eq 12 ]; then
    print_status "OK" "Chapitres cours: $COBOL_COURS_COUNT fichiers"
else
    print_status "WARN" "Chapitres cours: $COBOL_COURS_COUNT fichiers (attendu: 12)"
fi

echo ""
echo -e "${BLUE}4. Module COBOL - Exercices${NC}"
echo "   Répertoire: exercices/cobol/"
COBOL_EX_DIRS=$(find "$PROJECT_ROOT/exercices/cobol" -type d -name "chapitre-*" | wc -l | tr -d ' ')
COBOL_CBL_COUNT=$(count_files "$PROJECT_ROOT/exercices/cobol" "*.cbl")
print_status "OK" "Chapitres exercices: $COBOL_EX_DIRS répertoires"
print_status "OK" "Programmes COBOL: $COBOL_CBL_COUNT fichiers .cbl"

echo ""
echo -e "${BLUE}5. Module CICS - Cours${NC}"
echo "   Répertoire: cours/cics/"
CICS_COURS_COUNT=$(count_files "$PROJECT_ROOT/cours/cics" "*.md")
CICS_COURS_COUNT=$((CICS_COURS_COUNT - 1)) # Exclure README.md
if [ "$CICS_COURS_COUNT" -eq 3 ]; then
    print_status "OK" "Chapitres cours: $CICS_COURS_COUNT fichiers"
else
    print_status "WARN" "Chapitres cours: $CICS_COURS_COUNT fichiers (attendu: 3)"
fi

echo ""
echo -e "${BLUE}6. Exercices Z/OS TSO${NC}"
echo "   Répertoire: exercices/zos-tso/"
ZOS_EX_COUNT=$(count_files "$PROJECT_ROOT/exercices/zos-tso" "*.md")
ZOS_EX_COUNT=$((ZOS_EX_COUNT - 1)) # Exclure README.md
if [ "$ZOS_EX_COUNT" -ge 1 ]; then
    print_status "OK" "Fichiers exercices: $ZOS_EX_COUNT"
else
    print_status "WARN" "Aucun fichier exercice trouvé"
fi

echo ""
echo -e "${BLUE}7. Projet Fil Rouge${NC}"
echo "   Répertoire: projets/fil-rouge/"
FR_DIRS=$(find "$PROJECT_ROOT/projets/fil-rouge" -type d -name "ex*" | wc -l | tr -d ' ')
FR_JCL=$(count_files "$PROJECT_ROOT/projets/fil-rouge" "*.jcl")
FR_CBL=$(count_files "$PROJECT_ROOT/projets/fil-rouge" "*.cbl")

if [ "$FR_DIRS" -eq 21 ]; then
    print_status "OK" "Exercices: $FR_DIRS répertoires"
else
    print_status "WARN" "Exercices: $FR_DIRS répertoires (attendu: 21)"
fi
print_status "OK" "Fichiers JCL: $FR_JCL"
print_status "OK" "Programmes COBOL: $FR_CBL"

# Vérifier ex10
if [ -d "$PROJECT_ROOT/projets/fil-rouge/ex10-editer-index-alternatif" ]; then
    print_status "OK" "Ex10 présent (ex10-editer-index-alternatif)"
elif [ -d "$PROJECT_ROOT/projets/fil-rouge/ex10-"* ]; then
    print_status "WARN" "Ex10 présent mais nom différent"
else
    print_status "ERROR" "Ex10 manquant!"
fi

echo ""
echo -e "${BLUE}========================================${NC}"
echo -e "${BLUE}   Résumé${NC}"
echo -e "${BLUE}========================================${NC}"
echo ""

# Tableau récapitulatif
echo "   Module             | Cours | Exercices"
echo "   -------------------|-------|----------"
printf "   Z/OS TSO           | %5s | %s fichiers\n" "$ZOS_COURS_COUNT" "$ZOS_EX_COUNT"
printf "   JCL                | %5s | %s fichiers\n" "$JCL_COURS_COUNT" "$JCL_EX_COUNT"
printf "   COBOL              | %5s | %s .cbl\n" "$COBOL_COURS_COUNT" "$COBOL_CBL_COUNT"
printf "   CICS               | %5s | (à venir)\n" "$CICS_COURS_COUNT"
printf "   Fil Rouge          |   -   | %s JCL, %s CBL\n" "$FR_JCL" "$FR_CBL"
echo ""

if [ $ERRORS -eq 0 ] && [ $WARNINGS -eq 0 ]; then
    echo -e "${GREEN}✅ Tous les READMEs sont à jour!${NC}"
    exit 0
elif [ $ERRORS -eq 0 ]; then
    echo -e "${YELLOW}⚠️  $WARNINGS avertissement(s) - vérifiez les détails ci-dessus${NC}"
    exit 0
else
    echo -e "${RED}❌ $ERRORS erreur(s) et $WARNINGS avertissement(s) trouvés${NC}"
    exit 1
fi
