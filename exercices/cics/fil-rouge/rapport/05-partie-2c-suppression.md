# Partie 2c : Operations de Suppression (DELETE)

[< Partie 2b : Mise a jour](04-partie-2b-maj.md) | [Retour au sommaire](00-introduction.md) | [Partie 3 : Avancees >](06-partie-3-avancees.md)

---

Cette section couvre les exercices 12 a 15 : MAP de suppression, programme de suppression avec la commande DELETE, et deux variantes de transaction.

## Comparaison des commandes CICS pour suppression

| Commande | Usage | Prerequis |
|----------|-------|-----------|
| **DELETE** simple | Supprime directement par la cle | Aucun |
| **READ + DELETE** | Affiche avant suppression | READ pour confirmation visuelle |

---

## Exercice 12 : MAP pour suppression

### Enonce

Creer ou adapter la MAP precedente pour une operation de suppression de CLIENT dans le Data Set CLIENT.

### Mon travail

La MAP de suppression affiche les donnees du client a supprimer et demande confirmation avant la suppression.

### Captures d'ecran

<!-- ![pt2ex12-1](images-pt2/pt2ex12-1.png) -->

---

## Exercice 13 : Programme de suppression (DELETE)

### Enonce

Creer le PROGRAMME pour une operation de suppression d'un CLIENT dans le Data Set CLIENT en precisant le code CLIENT. Un controle de conformite de donnee et d'existence doit etre effectue.

### Resolution

**Programme : PRGSUP.cbl**

```cobol
       2000-SUPPRIMER-CLIENT.
           MOVE NUMCPTI TO CLI-NUMCPT

      * Verification existence
           EXEC CICS READ
               FILE('FCLIENT')
               INTO(ENR-CLIENT)
               RIDFLD(CLI-NUMCPT)
               RESP(WS-RESP)
           END-EXEC

           IF WS-RESP = DFHRESP(NOTFND)
               MOVE 'CLIENT INEXISTANT' TO MSGO
               EXIT PARAGRAPH
           END-IF

      * Suppression
           EXEC CICS DELETE
               FILE('FCLIENT')
               RIDFLD(CLI-NUMCPT)
               RESP(WS-RESP)
           END-EXEC

           IF WS-RESP = DFHRESP(NORMAL)
               MOVE 'SUPPRESSION EFFECTUEE' TO MSGO
           ELSE
               MOVE 'ERREUR SUPPRESSION' TO MSGO
           END-IF.
```

### Captures d'ecran

<!-- ![pt2ex13-1](images-pt2/pt2ex13-1.png) -->

---

## Exercice 14 : Transaction de suppression

### Resolution

```
CEDA DEFINE TRANSACTION(SUPP) GROUP(CLIGROUP)
     PROGRAM(PRGSUP)

CEDA INSTALL GROUP(CLIGROUP)
```

### Captures d'ecran

<!-- ![pt2ex14-1](images-pt2/pt2ex14-1.png) -->

---

## Exercice 15 : Suppression avec lecture prealable

### Enonce

Reprendre cette operation de suppression en la precedant par une operation de lecture. Definir une transaction independante de la precedente.

### Mon travail

Cette version affiche d'abord les donnees du client avant de demander confirmation pour la suppression. Cela permet a l'utilisateur de verifier qu'il supprime le bon client.

### Resolution

**Programme : PRGSUL.cbl** (Suppression avec Lecture)

```cobol
       01 WS-PHASE             PIC X(01).
          88 PHASE-SAISIE      VALUE '1'.
          88 PHASE-CONFIRM     VALUE '2'.

       2000-TRAITEMENT.
           EVALUATE TRUE
               WHEN PHASE-SAISIE
                   PERFORM 3000-RECHERCHER-CLIENT
               WHEN PHASE-CONFIRM
                   PERFORM 4000-CONFIRMER-SUPPRESSION
           END-EVALUATE.

       3000-RECHERCHER-CLIENT.
      * Lecture et affichage du client
           EXEC CICS READ FILE('FCLIENT')
               INTO(ENR-CLIENT)
               RIDFLD(CLI-NUMCPT)
               RESP(WS-RESP)
           END-EXEC
           IF WS-RESP = DFHRESP(NORMAL)
               PERFORM AFFICHER-DONNEES
               MOVE 'CONFIRMER SUPPRESSION (O/N) ?' TO MSGO
               MOVE '2' TO WS-PHASE
           END-IF.

       4000-CONFIRMER-SUPPRESSION.
      * Si confirmation, suppression
           IF CONFIRMI = 'O'
               EXEC CICS DELETE FILE('FCLIENT')
                   RIDFLD(CLI-NUMCPT)
                   RESP(WS-RESP)
               END-EXEC
               MOVE 'SUPPRESSION EFFECTUEE' TO MSGO
           ELSE
               MOVE 'SUPPRESSION ANNULEE' TO MSGO
           END-IF
           MOVE '1' TO WS-PHASE.
```

**Transaction independante :**

```
CEDA DEFINE TRANSACTION(SULE) GROUP(CLIGROUP)
     PROGRAM(PRGSUL)
```

### Captures d'ecran

<!-- ![pt2ex15-1](images-pt2/pt2ex15-1.png) -->

---

[< Partie 2b : Mise a jour](04-partie-2b-maj.md) | [Retour au sommaire](00-introduction.md) | [Partie 3 : Avancees >](06-partie-3-avancees.md)
