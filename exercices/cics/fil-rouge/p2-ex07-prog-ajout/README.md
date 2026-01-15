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

## Points techniques importants

### 1. Sauvegarde des donnees MAP (MODE=INOUT)

Avec `MODE=INOUT` et `STORAGE=AUTO` dans BMS, les zones input (I) et output (O) partagent la meme memoire. Il faut sauvegarder les donnees dans des variables WS- apres le `RECEIVE MAP` :

```cobol
      * SAUVEGARDE DES DONNEES AVANT ECRASEMENT PAR LOW-VALUES
           MOVE NUMCPTI   TO WS-NUMCPT
           MOVE SEXEI     TO WS-SEXE
           MOVE POSITI    TO WS-POSITION
```

### 2. PERFORM THRU pour les GO TO

Quand un paragraphe utilise `GO TO paragraphe-FIN`, il faut inclure le paragraphe FIN dans la plage du PERFORM avec `THRU` :

```cobol
           PERFORM 2000-TRAITEMENT THRU 2000-FIN
           PERFORM 2100-VALIDER-DONNEES THRU 2100-FIN
           PERFORM 2200-VERIFIER-DOUBLURE THRU 2200-FIN
```

Sans `THRU`, le `GO TO` sort du PERFORM et le programme continue sequentiellement au lieu de retourner a l'appelant.

### 3. ERASE sur les SEND MAP d'erreur

Pour que le message d'erreur s'affiche correctement, ajouter `ERASE` au SEND MAP :

```cobol
           IF ERREUR-DETECTEE
               EXEC CICS SEND MAP('MAPAJT')
                   MAPSET('CLIAJT')
                   ERASE
               END-EXEC
               GO TO 2000-FIN
           END-IF
```
