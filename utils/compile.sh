#!/bin/bash
# Script de compilation COBOL

if [ $# -eq 0 ]; then
    echo "Usage: ./compile.sh <fichier.cbl>"
    exit 1
fi

FILENAME=$1
BASENAME=$(basename "$FILENAME" .cbl)

echo "Compilation de $FILENAME..."
cobc -x -debug -g "$FILENAME"

if [ $? -eq 0 ]; then
    echo "✓ Compilation réussie : $BASENAME"
    echo "Exécution : ./$BASENAME"
else
    echo "✗ Erreur de compilation"
    exit 1
fi
