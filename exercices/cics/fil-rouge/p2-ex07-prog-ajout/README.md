# Exercice 7 : Programme d'ajout (WRITE)

## Objectif

Creer le programme COBOL-CICS permettant d'ajouter un nouveau client dans le fichier VSAM.

## Fichiers

| Fichier | Description |
|---------|-------------|
| `PRGAJT.cbl` | Programme COBOL-CICS d'ajout |
| `CMPAJT.jcl` | JCL de compilation |

## Fonctionnalites

### Mode pseudo-conversationnel

- Premier passage : Affiche ecran vide pour saisie
- Passages suivants : Valide et enregistre le client
- PF3 : Quitter la transaction

### Controles effectues

| Champ | Controle |
|-------|----------|
| NUMCPT | Obligatoire, numerique (6 chiffres) |
| CODREG | Obligatoire, valeurs 01/02/03/04 |
| NOM | Obligatoire |
| SEXE | Obligatoire, valeurs M ou F |
| SITSO | Obligatoire, valeurs C/M/D/V |
| POSIT | Obligatoire, valeurs DB ou CR |

### Verification doublure

Avant l'ecriture, le programme verifie que le client n'existe pas deja (READ puis WRITE).

## Commandes CICS utilisees

| Commande | Usage |
|----------|-------|
| SEND MAP | Envoyer l'ecran |
| RECEIVE MAP | Recevoir la saisie |
| READ FILE | Verifier si client existe |
| WRITE FILE | Ecrire le nouveau client |
| RETURN TRANSID | Retour pseudo-conversationnel |

## Utilisation

### 1. Copier le source dans la library

```
ISPF 3.4 > ROCHA.CICS.SOURCE
Edit member PRGAJT
Copier le contenu de PRGAJT.cbl
```

### 2. Soumettre le JCL de compilation

```
ISPF 3.4 > ROCHA.CICS.SOURCE
Edit member CMPAJT (copier CMPAJT.jcl)
SUB (submit)
```

### 3. Verifier le resultat

- RC=0000 dans SDSF
- Membre PRGAJT present dans ROCHA.CICS.LOAD

## Structure du programme

| Paragraphe | Fonction |
|------------|----------|
| 0000-PRINCIPAL | Point d'entree, aiguillage |
| 1000-PREMIER-PASSAGE | Affichage ecran vide |
| 2000-TRAITEMENT | Reception et traitement |
| 2100-VALIDER-DONNEES | Controles de conformite |
| 2200-VERIFIER-DOUBLURE | Verification client existe |
| 2300-PREPARER-ENREGISTREMENT | Transfert MAP vers ENR |
| 2400-ECRIRE-CLIENT | WRITE VSAM |
| 9000-FIN-PROGRAMME | Message fin et RETURN |
