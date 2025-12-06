# Exercices JCL

## Organisation

Les exercices sont organises par chapitre du cours JCL.

## Chapitres

### Chapitre 02 - Fichiers speciaux et parametres

| Fichier | Description |
|---------|-------------|
| `02-exercices-fichiers.md` | 4 exercices sur la manipulation de fichiers |

**Exercices couverts :**
1. Creation dataset avec donnees in-stream (IEBGENER)
2. Copie de dataset avec IEBGENER
3. Creation de fichier temporaire
4. Concatenation de datasets

---

### Chapitre 03 - Procedures

| Fichier | Description |
|---------|-------------|
| `03-exercices-procedures.md` | 4 exercices sur les procedures JCL |

**Exercices couverts :**
1. Procedure in-stream pour chargement ESDS
2. Deux procedures cataloguees successives
3. Procedures imbriquees
4. Parametrage symbolique multiple

---

### Chapitre 04 - Utilitaires *(a venir)*

### Chapitre 05 - Travaux pratiques *(a venir)*

## Prerequis

- Acces a un environnement z/OS (ou emulateur Hercules/TK4-)
- Userid TSO valide
- Droits de creation de datasets
- Connaissance des chapitres I et II du cours JCL

## Notes

Ces exercices sont concus pour etre executes sur un mainframe IBM z/OS.
Pour les environnements de formation, un emulateur comme Hercules/TK4-
ou IBM Z Development and Test Environment peut etre utilise.

### Adaptation pour Hercules/TK4-

- Remplacer `FTEST` par votre userid (ex: `HERC01`)
- Utiliser le volume `PUB001` pour les allocations
- Verifier les classes disponibles (CLASS=A, MSGCLASS=A generalement)
