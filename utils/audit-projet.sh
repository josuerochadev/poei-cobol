#!/bin/bash
#
# audit-projet.sh - Audit complet du projet poei-cobol
#
# Vérifie :
# - Structure des modules (cours et exercices)
# - Présence des fichiers attendus
# - Liens de navigation entre chapitres
# - Cohérence du README principal
#
# Usage: ./utils/audit-projet.sh
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
echo -e "${BLUE}   Audit du projet poei-cobol${NC}"
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

# Vérifier chapitres 01, 02, 03 et 04
for i in 01 02 03 04; do
    if [ -f "$PROJECT_ROOT/cours/jcl/$i-"*.md ]; then
        print_status "OK" "Chapitre $i présent"
    else
        print_status "WARN" "Chapitre $i manquant"
    fi
done

# Vérifier répertoires et fichiers exercices JCL
JCL_JCL_COUNT=$(count_files "$PROJECT_ROOT/exercices/jcl" "*.jcl")
JCL_EX_DIRS=$(find "$PROJECT_ROOT/exercices/jcl" -type d -name "chapitre-*" 2>/dev/null | wc -l | tr -d ' ')
if [ "$JCL_JCL_COUNT" -ge 1 ]; then
    print_status "OK" "Fichiers JCL: $JCL_JCL_COUNT fichiers .jcl"
    print_status "OK" "Répertoires exercices: $JCL_EX_DIRS (chapitre-02 à 05)"
else
    print_status "WARN" "Aucun fichier .jcl trouvé"
fi

# Vérifier README.md dans chaque chapitre
for dir in "$PROJECT_ROOT/exercices/jcl/chapitre-"*/; do
    if [ -d "$dir" ]; then
        chapname=$(basename "$dir")
        if [ -f "$dir/README.md" ]; then
            print_status "OK" "$chapname/README.md présent"
        else
            print_status "WARN" "$chapname/README.md manquant"
        fi
    fi
done

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
echo -e "${BLUE}8. Vérification des liens de navigation${NC}"
echo "   Analyse des sections ## Navigation"

NAV_ERRORS=0
NAV_OK=0

# Fonction pour vérifier les liens dans un fichier
check_nav_links() {
    local file="$1"
    local dir=$(dirname "$file")

    # Extraire les liens markdown du fichier (format [texte](lien.md))
    local links=$(grep -oE '\[.*?\]\([^)]+\.md\)' "$file" 2>/dev/null | grep -oE '\([^)]+\.md\)' | tr -d '()')

    for link in $links; do
        # Ignorer les liens avec "-" (pas de lien)
        if [ "$link" = "-" ]; then
            continue
        fi

        # Résoudre le chemin relatif
        local target_path
        if [[ "$link" == ../* ]]; then
            target_path=$(cd "$dir" && cd "$(dirname "$link")" 2>/dev/null && pwd)/$(basename "$link")
        else
            target_path="$dir/$link"
        fi

        # Vérifier si le fichier existe
        if [ ! -f "$target_path" ]; then
            print_status "ERROR" "Lien cassé: $link dans $(basename "$file")"
            ((NAV_ERRORS++))
        else
            ((NAV_OK++))
        fi
    done
}

# Vérifier tous les modules
for module_dir in "$PROJECT_ROOT/cours/zos-tso" "$PROJECT_ROOT/cours/jcl" "$PROJECT_ROOT/cours/cobol" "$PROJECT_ROOT/cours/cics"; do
    if [ -d "$module_dir" ]; then
        module_name=$(basename "$module_dir")
        nav_count=0
        for mdfile in "$module_dir"/[0-9]*.md; do
            if [ -f "$mdfile" ] && grep -q "## Navigation" "$mdfile" 2>/dev/null; then
                ((nav_count++))
                check_nav_links "$mdfile"
            fi
        done
        if [ $nav_count -gt 0 ]; then
            print_status "OK" "$module_name: $nav_count chapitres avec navigation"
        else
            print_status "WARN" "$module_name: aucune section navigation trouvée"
        fi
    fi
done

if [ $NAV_ERRORS -eq 0 ]; then
    print_status "OK" "Tous les liens de navigation sont valides ($NAV_OK liens vérifiés)"
else
    print_status "ERROR" "$NAV_ERRORS lien(s) cassé(s) trouvé(s)"
    ((ERRORS+=NAV_ERRORS))
fi

echo ""
echo -e "${BLUE}9. Vérification du README principal${NC}"
echo "   Fichier: README.md"

README_FILE="$PROJECT_ROOT/README.md"
if [ -f "$README_FILE" ]; then
    README_ERRORS=0

    # Vérifier le nombre de chapitres Z/OS TSO dans la structure
    README_ZOS=$(grep -oE "zos-tso/.*\(([0-9]+) chapitres\)" "$README_FILE" | grep -oE "[0-9]+" | head -1)
    if [ -n "$README_ZOS" ]; then
        if [ "$README_ZOS" -eq "$ZOS_COURS_COUNT" ]; then
            print_status "OK" "Z/OS TSO: $README_ZOS chapitres (correct)"
        else
            print_status "ERROR" "Z/OS TSO: README indique $README_ZOS chapitres, réel: $ZOS_COURS_COUNT"
            ((README_ERRORS++))
        fi
    fi

    # Vérifier le nombre de chapitres JCL dans la structure
    README_JCL=$(grep -oE "jcl/.*\(([0-9]+) chapitres\)" "$README_FILE" | grep -oE "[0-9]+" | head -1)
    if [ -n "$README_JCL" ]; then
        if [ "$README_JCL" -eq "$JCL_COURS_COUNT" ]; then
            print_status "OK" "JCL: $README_JCL chapitres (correct)"
        else
            print_status "ERROR" "JCL: README indique $README_JCL chapitres, réel: $JCL_COURS_COUNT"
            ((README_ERRORS++))
        fi
    else
        print_status "WARN" "JCL: non mentionné dans la structure du README"
    fi

    # Vérifier le nombre de chapitres COBOL dans la structure
    README_COBOL=$(grep -oE "cobol/.*\(([0-9]+) chapitres\)" "$README_FILE" | grep -oE "[0-9]+" | head -1)
    if [ -n "$README_COBOL" ]; then
        if [ "$README_COBOL" -eq "$COBOL_COURS_COUNT" ]; then
            print_status "OK" "COBOL: $README_COBOL chapitres (correct)"
        else
            print_status "ERROR" "COBOL: README indique $README_COBOL chapitres, réel: $COBOL_COURS_COUNT"
            ((README_ERRORS++))
        fi
    fi

    # Vérifier le nombre de chapitres CICS dans la structure
    README_CICS=$(grep -oE "cics/.*\(([0-9]+) chapitres\)" "$README_FILE" | grep -oE "[0-9]+" | head -1)
    if [ -n "$README_CICS" ]; then
        if [ "$README_CICS" -eq "$CICS_COURS_COUNT" ]; then
            print_status "OK" "CICS: $README_CICS chapitres (correct)"
        else
            print_status "ERROR" "CICS: README indique $README_CICS chapitres, réel: $CICS_COURS_COUNT"
            ((README_ERRORS++))
        fi
    fi

    # Vérifier les tableaux de modules (compter les lignes avec ✅ dans chaque section)
    # Module Z/OS TSO
    ZOS_TABLE_LINES=$(sed -n '/### Module Z\/OS/,/### Module/p' "$README_FILE" | grep -c "| ✅")
    if [ "$ZOS_TABLE_LINES" -eq "$ZOS_COURS_COUNT" ]; then
        print_status "OK" "Tableau Z/OS TSO: $ZOS_TABLE_LINES lignes (correct)"
    else
        print_status "ERROR" "Tableau Z/OS TSO: $ZOS_TABLE_LINES lignes, attendu: $ZOS_COURS_COUNT"
        ((README_ERRORS++))
    fi

    # Module JCL (4 chapitres cours + 1 ligne TP sans cours)
    JCL_TABLE_LINES=$(sed -n '/### Module JCL/,/### Module/p' "$README_FILE" | grep -c "|")
    JCL_TABLE_LINES=$((JCL_TABLE_LINES - 2)) # Retirer header et separator
    if [ "$JCL_TABLE_LINES" -ge "$JCL_COURS_COUNT" ]; then
        print_status "OK" "Tableau JCL: $JCL_TABLE_LINES lignes (correct)"
    else
        print_status "ERROR" "Tableau JCL: $JCL_TABLE_LINES lignes, attendu: >= $JCL_COURS_COUNT"
        ((README_ERRORS++))
    fi

    # Module COBOL
    COBOL_TABLE_LINES=$(sed -n '/### Module COBOL/,/### Module/p' "$README_FILE" | grep -c "| ✅")
    if [ "$COBOL_TABLE_LINES" -eq "$COBOL_COURS_COUNT" ]; then
        print_status "OK" "Tableau COBOL: $COBOL_TABLE_LINES lignes (correct)"
    else
        print_status "ERROR" "Tableau COBOL: $COBOL_TABLE_LINES lignes, attendu: $COBOL_COURS_COUNT"
        ((README_ERRORS++))
    fi

    # Module CICS
    CICS_TABLE_LINES=$(sed -n '/### Module CICS/,/## Environnement/p' "$README_FILE" | grep -c "| ✅")
    if [ "$CICS_TABLE_LINES" -eq "$CICS_COURS_COUNT" ]; then
        print_status "OK" "Tableau CICS: $CICS_TABLE_LINES lignes (correct)"
    else
        print_status "ERROR" "Tableau CICS: $CICS_TABLE_LINES lignes, attendu: $CICS_COURS_COUNT"
        ((README_ERRORS++))
    fi

    if [ $README_ERRORS -eq 0 ]; then
        print_status "OK" "README principal cohérent avec le projet"
    else
        ((ERRORS+=README_ERRORS))
    fi
else
    print_status "ERROR" "README.md non trouvé à la racine"
    ((ERRORS++))
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
printf "   JCL                | %5s | %s .jcl, %s dirs\n" "$JCL_COURS_COUNT" "$JCL_JCL_COUNT" "$JCL_EX_DIRS"
printf "   COBOL              | %5s | %s .cbl\n" "$COBOL_COURS_COUNT" "$COBOL_CBL_COUNT"
printf "   CICS               | %5s | (à venir)\n" "$CICS_COURS_COUNT"
printf "   Fil Rouge          |   -   | %s JCL, %s CBL\n" "$FR_JCL" "$FR_CBL"
echo ""

if [ $ERRORS -eq 0 ] && [ $WARNINGS -eq 0 ]; then
    echo -e "${GREEN}✅ Audit réussi - projet conforme!${NC}"
    exit 0
elif [ $ERRORS -eq 0 ]; then
    echo -e "${YELLOW}⚠️  $WARNINGS avertissement(s) - vérifiez les détails ci-dessus${NC}"
    exit 0
else
    echo -e "${RED}❌ $ERRORS erreur(s) et $WARNINGS avertissement(s) trouvés${NC}"
    exit 1
fi
