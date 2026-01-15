# Partie 3 : Operations Avancees

[< Partie 2c : Suppression](05-partie-2c-suppression.md) | [Sommaire](00-introduction.md) | [Conclusion >](07-conclusion.md)

---

Cette section couvre les exercices 16 a 19 : creation de clients generiques, navigation VSAM avec STARTBR/READNEXT/ENDBR, et statistiques par region.

## Commandes CICS pour navigation VSAM

| Commande | Usage | Description |
|----------|-------|-------------|
| **STARTBR** | Positionner le curseur | Demarre un parcours a partir d'une cle partielle |
| **READNEXT** | Lire l'enregistrement suivant | Lit sequentiellement apres STARTBR |
| **ENDBR** | Terminer le parcours | Libere les ressources du browse |

Ces commandes permettent de parcourir un fichier VSAM de maniere sequentielle, contrairement aux commandes READ/WRITE/REWRITE/DELETE qui travaillent sur un enregistrement specifique.

---

# Partie 3 : Operations avancees

## Exercice 16 : Creation de clients generiques

### Enonce

Sachant que le CODE CLIENT est sur six caracteres, creer cinq CLIENT avec une partie de leur code generique commencant par '111...', de meme '444...' et '777...'.

### Mon travail

J'ai cree 15 clients de test avec des codes generiques :
- 111001, 111002, 111003, 111004, 111005
- 444001, 444002, 444003, 444004, 444005
- 777001, 777002, 777003, 777004, 777005

Ces clients serviront aux tests des commandes STARTBR et READNEXT.

### Resolution

Utilisation de la transaction AJOU pour creer les 15 clients.

### Captures d'ecran

<!-- ![pt3ex16-1](images-pt3/pt3ex16-1.png) -->

---

## Exercice 17 : Suppression par code generique (STARTBR)

### Enonce

En utilisant les commandes adequates, supprimer les CLIENT dont le code generique est '111...'.

### Mon travail

J'ai utilise STARTBR pour positionner le curseur sur le premier client '111', puis READNEXT en boucle pour lire et supprimer chaque client commencant par '111'.

### Resolution

**Programme : PRGSDEL.cbl** (Suppression Generique)

```cobol
       2000-SUPPRIMER-GENERIQUE.
           MOVE '111000' TO WS-CLE-DEBUT

           EXEC CICS STARTBR
               FILE('FCLIENT')
               RIDFLD(WS-CLE-DEBUT)
               GTEQ
               RESP(WS-RESP)
           END-EXEC

           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'AUCUN CLIENT 111XXX TROUVE' TO MSGO
               EXIT PARAGRAPH
           END-IF

           PERFORM UNTIL WS-FIN-BROWSE = 'O'
               EXEC CICS READNEXT
                   FILE('FCLIENT')
                   INTO(ENR-CLIENT)
                   RIDFLD(WS-CLE-COURANTE)
                   RESP(WS-RESP)
               END-EXEC

               IF WS-RESP = DFHRESP(ENDFILE)
                   MOVE 'O' TO WS-FIN-BROWSE
               ELSE IF WS-CLE-COURANTE(1:3) NOT = '111'
                   MOVE 'O' TO WS-FIN-BROWSE
               ELSE
                   EXEC CICS DELETE
                       FILE('FCLIENT')
                       RIDFLD(WS-CLE-COURANTE)
                   END-EXEC
                   ADD 1 TO WS-COMPTEUR-SUP
               END-IF
           END-PERFORM

           EXEC CICS ENDBR FILE('FCLIENT') END-EXEC.
```

### Captures d'ecran

<!-- ![pt3ex17-1](images-pt3/pt3ex17-1.png) -->

---

## Exercice 18 : Lecture successive (READNEXT, ENDBR)

### Enonce

Faire une lecture successive des CLIENT dont le code generique est '222...' en utilisant la commande READNEXT et ENDBR.

### Mon travail

Ce programme illustre le parcours sequentiel d'un fichier VSAM avec positionnement generique :
1. STARTBR avec GTEQ pour se positionner sur le premier '222xxx'
2. READNEXT en boucle pour lire les suivants
3. Arret quand le code ne commence plus par '222'
4. ENDBR pour terminer le browse

### Resolution

**Programme : PRGLGEN.cbl** (Liste Generique)

```cobol
       2000-LISTER-GENERIQUE.
           MOVE '222000' TO WS-CLE-DEBUT
           MOVE 0 TO WS-COMPTEUR

           EXEC CICS STARTBR
               FILE('FCLIENT')
               RIDFLD(WS-CLE-DEBUT)
               GTEQ
               RESP(WS-RESP)
           END-EXEC

           PERFORM UNTIL WS-FIN-BROWSE = 'O'
               EXEC CICS READNEXT
                   FILE('FCLIENT')
                   INTO(ENR-CLIENT)
                   RIDFLD(WS-CLE-COURANTE)
                   RESP(WS-RESP)
               END-EXEC

               EVALUATE TRUE
                   WHEN WS-RESP = DFHRESP(ENDFILE)
                       MOVE 'O' TO WS-FIN-BROWSE
                   WHEN WS-CLE-COURANTE(1:3) NOT = '222'
                       MOVE 'O' TO WS-FIN-BROWSE
                   WHEN OTHER
                       PERFORM 3000-AFFICHER-LIGNE
                       ADD 1 TO WS-COMPTEUR
               END-EVALUATE
           END-PERFORM

           EXEC CICS ENDBR FILE('FCLIENT') END-EXEC

           DISPLAY 'TOTAL CLIENTS 222XXX : ' WS-COMPTEUR.
```

### Captures d'ecran

<!-- ![pt3ex18-1](images-pt3/pt3ex18-1.png) -->

---

## Exercice 19 : Statistiques par region

### Enonce

Elaborer une transaction permettant de calculer pour une REGION le nombre de CLIENT, la somme des montants des CLIENT Debiteurs et leur nombre et la somme des montants des CLIENT Crediteurs et leur nombre. Cette transaction aura en entree le code REGION et affichera les quatre informations specifiees ci-dessus.

### Mon travail

Cette transaction effectue un parcours complet du fichier pour calculer les statistiques d'une region donnee :
- Nombre total de clients de la region
- Nombre et somme des clients debiteurs (DB)
- Nombre et somme des clients crediteurs (CR)

J'utilise STARTBR/READNEXT pour parcourir tout le fichier et je filtre sur le code region.

### Resolution

**MAP BMS : CLISTAT.bms**

```
* Zone de saisie code region
         DFHMDF POS=(3,2),LENGTH=15,                                   X
               INITIAL='CODE REGION   :'
CODREG   DFHMDF POS=(3,18),LENGTH=2,                                   X
               ATTRB=(UNPROT,NUM,IC)

* Zones d'affichage des statistiques
         DFHMDF POS=(6,2),LENGTH=25,                                   X
               INITIAL='NOMBRE TOTAL CLIENTS    :'
NBTOT    DFHMDF POS=(6,28),LENGTH=5,ATTRB=(ASKIP)

         DFHMDF POS=(8,2),LENGTH=25,                                   X
               INITIAL='CLIENTS DEBITEURS       :'
NBDB     DFHMDF POS=(8,28),LENGTH=5,ATTRB=(ASKIP)
MTDB     DFHMDF POS=(8,35),LENGTH=12,ATTRB=(ASKIP)

         DFHMDF POS=(10,2),LENGTH=25,                                  X
               INITIAL='CLIENTS CREDITEURS      :'
NBCR     DFHMDF POS=(10,28),LENGTH=5,ATTRB=(ASKIP)
MTCR     DFHMDF POS=(10,35),LENGTH=12,ATTRB=(ASKIP)
```

**Programme : PRGSTAT.cbl**

```cobol
       WORKING-STORAGE SECTION.
       01 WS-STATS.
          05 WS-NB-TOTAL         PIC 9(05) VALUE 0.
          05 WS-NB-DEBITEURS     PIC 9(05) VALUE 0.
          05 WS-MT-DEBITEURS     PIC 9(10) VALUE 0.
          05 WS-NB-CREDITEURS    PIC 9(05) VALUE 0.
          05 WS-MT-CREDITEURS    PIC 9(10) VALUE 0.

       2000-CALCULER-STATS.
           INITIALIZE WS-STATS
           MOVE CODREGI TO WS-CODE-REGION

      * Verification region existante
           IF WS-CODE-REGION NOT = '01' AND '02' AND '03' AND '04'
               MOVE 'REGION INEXISTANTE, SAISIR CODE REGION' TO MSGO
               EXIT PARAGRAPH
           END-IF

      * Parcours du fichier
           MOVE LOW-VALUES TO WS-CLE-DEBUT
           EXEC CICS STARTBR FILE('FCLIENT')
               RIDFLD(WS-CLE-DEBUT)
               RESP(WS-RESP)
           END-EXEC

           PERFORM UNTIL WS-FIN-BROWSE = 'O'
               EXEC CICS READNEXT FILE('FCLIENT')
                   INTO(ENR-CLIENT)
                   RIDFLD(WS-CLE-COURANTE)
                   RESP(WS-RESP)
               END-EXEC

               IF WS-RESP = DFHRESP(ENDFILE)
                   MOVE 'O' TO WS-FIN-BROWSE
               ELSE
                   IF CLI-CODREG = WS-CODE-REGION
                       ADD 1 TO WS-NB-TOTAL
                       IF CLI-DEBITEUR
                           ADD 1 TO WS-NB-DEBITEURS
                           ADD CLI-SOLDE TO WS-MT-DEBITEURS
                       ELSE
                           ADD 1 TO WS-NB-CREDITEURS
                           ADD CLI-SOLDE TO WS-MT-CREDITEURS
                       END-IF
                   END-IF
               END-IF
           END-PERFORM

           EXEC CICS ENDBR FILE('FCLIENT') END-EXEC
           PERFORM 3000-AFFICHER-RESULTATS.

       3000-AFFICHER-RESULTATS.
           MOVE WS-NB-TOTAL      TO NBTOTO
           MOVE WS-NB-DEBITEURS  TO NBDBO
           MOVE WS-MT-DEBITEURS  TO MTDBO
           MOVE WS-NB-CREDITEURS TO NBCRO
           MOVE WS-MT-CREDITEURS TO MTCRO
           MOVE 'STATISTIQUES CALCULEES' TO MSGO.
```

**Transaction :**

```
CEDA DEFINE TRANSACTION(STAT) GROUP(CLIGROUP)
     PROGRAM(PRGSTAT)
```

### Captures d'ecran

<!-- ![pt3ex19-1](images-pt3/pt3ex19-1.png) -->

---

[< Partie 2c : Suppression](05-partie-2c-suppression.md) | [Sommaire](00-introduction.md) | [Conclusion >](07-conclusion.md)
