# Partie 2b : Opérations de Mise à Jour (REWRITE)

[< Partie 2a : Ajout](03-partie-2a-ajout.md) | [Retour au sommaire](00-introduction.md) | [Partie 2c : Suppression >](05-partie-2c-suppression.md)

---

Cette section couvre les exercices 9 à 11 : MAP de mise à jour, programme de modification avec la commande REWRITE, et définition de la transaction MAJO.

## Différence WRITE vs REWRITE

| Aspect | WRITE (Ajout) | REWRITE (Mise à jour) |
|--------|---------------|----------------------|
| Client | Ne doit PAS exister | DOIT exister |
| Clé | Nouvelle | Existante (non modifiable) |
| Prérequis | Aucun | READ UPDATE obligatoire |
| Erreur typique | DUPREC (doublon) | NOTFND (inexistant) |

---

## Exercice 9 : MAP pour mise à jour

### Énoncé

Créer ou adapter la MAP précédente pour une opération de mise à jour de CLIENT dans le Data Set CLIENT.

### Mon travail

J'ai créé une nouvelle MAP BMS (CLIMAJ) basée sur CLIAJT mais avec une particularité importante : la **gestion dynamique des attributs** du champ clé.

#### Pourquoi une gestion dynamique des attributs ?

En mise à jour, contrairement à l'ajout, le numéro de compte change de comportement au cours de la transaction :

1. **Phase 1 (Recherche)** : L'utilisateur doit pouvoir saisir le numéro du client à modifier → NUMCPT doit être **saisissable (UNPROT)**
2. **Phase 2 (Affichage)** : Une fois le client trouvé, son numéro s'affiche mais ne peut pas être modifié (la clé VSAM est immuable) → NUMCPT doit passer en **lecture seule (ASKIP)**
3. **Phase 3 (Modification)** : L'utilisateur modifie les autres champs et valide → NUMCPT reste **protégé (ASKIP)**

Cette gestion dynamique se fait dans le programme COBOL via le **suffixe 'A'** (Attribut) du copybook généré (voir Partie 1, Exercice 2 pour la structure DSECT et les suffixes L, F, A, I, O).

#### Comment modifier un attribut à l'exécution ?

Le copybook généré par l'assemblage BMS contient pour chaque champ nommé une variable suffixée `A` qui permet de changer son attribut dynamiquement :

```cobol
* Après affichage du client, protéger le numéro de compte
* DFHBMASK = X'20' = ASKIP (protégé, intensité normale)
MOVE DFHBMASK TO NUMCPTA
```

> **Important** : Les constantes d'attribut (DFHBMASK, DFHBMUNN, etc.) sont définies dans le copybook système `DFHBMSCA`. Il faut l'inclure dans le programme avec `COPY DFHBMSCA`.

### Résolution

**MAP BMS : CLIMAJ.bms**

Le code source est stocké dans `ROCHA.CICS.SOURCE(CLIMAJ)`. La structure reprend les mêmes concepts BMS que CLIAFF et CLIAJT (voir Partie 1, Exercice 2 pour les explications sur DFHMSD, DFHMDI, DFHMDF et les attributs).

**Maquette de l'écran MAPMAJ :**

Cette maquette (wireframe) représente la disposition des champs sur l'écran 24x80 :

```
+------------------------------------------------------------------------------+
|                         *** MISE A JOUR CLIENT ***                           |
|------------------------------------------------------------------------------|
|                                                                              |
|  NUMERO COMPTE : ______  (Clé - non modifiable)                              |
|  CODE REGION   : __     (01=PAR,02=MAR,03=LYO,04=LIL)                        |
|  NATURE COMPTE : __                                                          |
|  NOM           : __________                                                  |
|  PRENOM        : __________                                                  |
|  DATE NAISSANCE: ________  (AAAAMMJJ)                                        |
|  SEXE          : _  (M ou F)                                                 |
|  ACTIVITE PRO  : __                                                          |
|  SITUATION SOC : _  (C/M/D/V)                                                |
|  ADRESSE       : __________                                                  |
|  SOLDE         : __________                                                  |
|  POSITION      : __  (DB ou CR)                                              |
|                                                                              |
|                                                                              |
|------------------------------------------------------------------------------|
|  MESSAGE : ____________________________________________________________      |
|                                                                              |
|                                                                              |
|  ENTER=Valider  PF3=Quitter  CLEAR=Réinitialiser                             |
+------------------------------------------------------------------------------+
```

**Zones de la MAP :**

| Zone | Longueur | Attribut initial | Comportement dynamique |
|------|----------|------------------|------------------------|
| NUMCPT | 6 | UNPROT,NUM,IC | Devient ASKIP après recherche (via NUMCPTA) |
| CODREG | 2 | UNPROT,NUM | Reste saisissable |
| NATCPT | 2 | UNPROT,NUM | Reste saisissable |
| NOM | 10 | UNPROT | Reste saisissable |
| PRENOM | 10 | UNPROT | Reste saisissable |
| DATNA | 8 | UNPROT,NUM | Reste saisissable |
| SEXE | 1 | UNPROT | Reste saisissable |
| ACTPRO | 2 | UNPROT,NUM | Reste saisissable |
| SITSO | 1 | UNPROT | Reste saisissable |
| ADRESSE | 10 | UNPROT | Reste saisissable |
| SOLDE | 10 | UNPROT,NUM | Reste saisissable |
| POSIT | 2 | UNPROT | Reste saisissable |
| MSG | 60 | ASKIP,BRT | Zone message (affichage seul) |

### Constantes d'attribut BMS (DFHBMSCA)

Le copybook `DFHBMSCA` contient les constantes hexadécimales pour modifier les attributs à l'exécution :

| Constante | Valeur | Description | Usage typique |
|-----------|--------|-------------|---------------|
| DFHBMASK | X'20' | ASKIP - Protégé, intensité normale | Protéger un champ |
| DFHBMPRF | X'28' | ASKIP - Protégé, brillant | Mise en évidence |
| DFHBMUNN | X'4C' | UNPROT + NUM - Saisie numérique | Rendre saisissable (chiffres) |
| DFHBMUNP | X'40' | UNPROT - Saisie alphanumérique | Rendre saisissable (texte) |
| DFHBMFSE | X'08' | MDT forcé | Forcer la transmission |
| DFHBMPRO | X'20' | PROT - Protégé | Synonyme de DFHBMASK |

> **Rappel MDT** : Le MDT (Modified Data Tag) indique si un champ a été modifié. Avec FRSET dans la MAP, les MDT sont remis à zéro au SEND MAP. Seuls les champs modifiés par l'utilisateur sont transmis au RECEIVE MAP (voir Partie 1, Exercice 2).

**JCL d'assemblage : ASMMAJ.jcl**

Le JCL d'assemblage suit la même structure que ASMCLAF.jcl (voir Partie 1, Exercice 2). Seuls le nom du job (ROCHA09) et le membre source (CLIMAJ) changent.

### Définition CICS

La définition et l'installation du mapset suivent le même processus que pour les mapsets précédents (voir Partie 1, Exercice 4 pour les explications sur CEDA) :

```
CEDA DEFINE MAPSET(CLIMAJ) GROUP(CLIGROUP)
CEDA INSTALL MAPSET(CLIMAJ) GROUP(CLIGROUP)
```

### Vérification

```
CEDA VIEW MAPSET(CLIMAJ) GROUP(CLIGROUP)
```

> **Note** : `CEMT INQ MAPSET` n'existe pas dans CICS. Pour vérifier un mapset, utiliser `CEDA VIEW`.

### Captures d'écran

<!--
Suggestions de captures d'écran pour cet exercice :

1. pt2ex09-1 : Source BMS dans ISPF EDIT - ROCHA.CICS.SOURCE(CLIMAJ)
2. pt2ex09-2 : Soumission JCL assemblage BMS
3. pt2ex09-3 : SDSF - Job output avec RC=0000
4. pt2ex09-4 : Vérification ROCHA.CICS.LINK - copybook CLIMAJ généré
-->

---

## Exercice 10 : Programme de mise à jour (REWRITE)

### Énoncé

Créer le PROGRAMME pour une opération de mise à jour d'un CLIENT dans le Data Set CLIENT. Un contrôle de conformité de donnée et d'existence doit être effectué.

### Mon travail

J'ai développé le programme PRGMAJ qui gère la mise à jour des clients existants. Ce programme présente plusieurs différences importantes par rapport à PRGAJT (ajout).

#### Pourquoi un mode pseudo-conversationnel à 3 phases ?

Contrairement à l'ajout (2 phases), la mise à jour nécessite 3 phases distinctes car l'utilisateur doit d'abord **rechercher** le client avant de le **modifier** :

```
┌─────────────────────────────────────────────────────────────────┐
│ LANCEMENT TRANSACTION "MAJO"                                    │
└─────────────────────────────────────────────────────────────────┘
                            │
                            ▼
┌─────────────────────────────────────────────────────────────────┐
│ PHASE 1 : RECHERCHE (EIBCALEN = 0)                              │
│ ──────────────────────────────────────────────────────────────  │
│ → CICS lance le programme pour la première fois                 │
│ → Affichage écran vide avec NUMCPT saisissable (UNPROT)         │
│ → Le programme se TERMINE (RETURN TRANSID)                      │
│ → COMMAREA : WS-PHASE = '1', WS-NUMCPT-SAVED = SPACES           │
└─────────────────────────────────────────────────────────────────┘
                            │
        L'utilisateur saisit un numéro et appuie sur ENTRÉE
                            │
                            ▼
┌─────────────────────────────────────────────────────────────────┐
│ PHASE 2 : AFFICHAGE (WS-PHASE = '1' → '2')                      │
│ ──────────────────────────────────────────────────────────────  │
│ → CICS relance le programme                                     │
│ → READ du fichier pour vérifier existence                       │
│ → Si trouvé : affichage des données, NUMCPT passe en ASKIP      │
│ → Le programme se TERMINE (RETURN TRANSID)                      │
│ → COMMAREA : WS-PHASE = '2', WS-NUMCPT-SAVED = '000001'         │
└─────────────────────────────────────────────────────────────────┘
                            │
        L'utilisateur modifie les champs et appuie sur ENTRÉE
                            │
                            ▼
┌─────────────────────────────────────────────────────────────────┐
│ PHASE 3 : VALIDATION (WS-PHASE = '2' ou '3')                    │
│ ──────────────────────────────────────────────────────────────  │
│ → CICS relance le programme                                     │
│ → RECEIVE MAP des modifications                                 │
│ → Fusion avec données actuelles (champs non modifiés)           │
│ → Validation des données                                        │
│ → READ UPDATE + REWRITE atomiques                               │
│ → Retour en phase 1 pour nouveau client                         │
└─────────────────────────────────────────────────────────────────┘
```

Voir Partie 1, Exercice 3 pour les explications détaillées sur le mode pseudo-conversationnel et les variables EIB.

#### Pourquoi une COMMAREA étendue ?

En mise à jour, la COMMAREA doit sauvegarder plus d'informations qu'en ajout :

| Programme | Contenu COMMAREA | Raison |
|-----------|------------------|--------|
| PRGAJT (Ajout) | WS-FLAG-INIT (1 octet) | Distinguer premier passage |
| PRGMAJ (MAJ) | WS-PHASE (1 octet) + WS-NUMCPT-SAVED (6 octets) | Phase + numéro protégé |

**Pourquoi sauvegarder le numéro de compte ?**

Une fois le champ NUMCPT protégé (ASKIP), le terminal ne le transmet plus au programme lors du RECEIVE MAP. Or, on a besoin de ce numéro pour relire et modifier le client. La COMMAREA permet de le conserver entre les passages.

#### Pourquoi fusionner les modifications ?

C'est une différence majeure avec l'ajout. En mise à jour :

- L'utilisateur ne modifie que **certains** champs (ex: changer l'adresse uniquement)
- Les champs non modifiés ne sont pas transmis par le terminal (longueur = 0)
- Si on écrivait directement les valeurs reçues, on écraserait les autres champs avec des espaces !

**Solution** : Relire le client, puis ne remplacer que les champs dont la longueur > 0.

#### Pourquoi READ UPDATE + REWRITE dans le même paragraphe ?

En CICS, la commande `REWRITE` nécessite un `READ UPDATE` préalable dans la **même unité de travail (UOW)**.

**Problème** : En mode pseudo-conversationnel, chaque passage est une nouvelle tâche CICS → nouvelle UOW.

**Conséquence** : On ne peut PAS faire :
- Phase 2 : READ UPDATE (verrouillage)
- *-- Fin de tâche --*
- Phase 3 : REWRITE (échec car pas de verrouillage actif)

**Solution** : Faire les deux dans le même passage, juste avant l'écriture :

```
Phase 2 : READ simple (affichage) → Fin de tâche
Phase 3 : READ UPDATE + REWRITE (atomique) → Fin de tâche
```

### Résolution

**Programme : PRGMAJ.cbl**

Le code source est stocké dans `ROCHA.CICS.SOURCE(PRGMAJ)`. Voici les sections clés du programme.

**Structure de la COMMAREA étendue (WORKING-STORAGE) :**

```cobol
      *-----------------------------------------------------------------
      * ZONE DE COMMUNICATION (COMMAREA)
      * Sauvegarde la phase et le numéro de compte entre passages
      *-----------------------------------------------------------------
       01  WS-COMMAREA.
           05 WS-PHASE            PIC X(01) VALUE '1'.
              88 PHASE-RECHERCHE  VALUE '1'.
              88 PHASE-AFFICHAGE  VALUE '2'.
              88 PHASE-VALIDATION VALUE '3'.
           05 WS-NUMCPT-SAVED     PIC X(06) VALUE SPACES.
```

**LINKAGE SECTION (obligatoire pour recevoir la COMMAREA) :**

```cobol
       LINKAGE SECTION.
      *-----------------------------------------------------------------
      * ZONE COMMAREA PASSEE PAR CICS
      * OBLIGATOIRE pour accéder aux données du RETURN précédent
      *-----------------------------------------------------------------
       01  DFHCOMMAREA.
           05 LS-PHASE            PIC X(01).
           05 LS-NUMCPT-SAVED     PIC X(06).
```

**Point d'entrée avec gestion des phases :**

```cobol
       0000-PRINCIPAL.
           EVALUATE TRUE
               WHEN EIBCALEN = 0
      *            Premier appel - Phase recherche
                   PERFORM 1000-INIT-RECHERCHE
               WHEN EIBAID = DFHPF3
      *            PF3 - Fin de transaction
                   PERFORM 9000-FIN-PROGRAMME
               WHEN EIBAID = DFHCLEAR
      *            CLEAR - Réinitialiser
                   PERFORM 1000-INIT-RECHERCHE
               WHEN OTHER
      *            Traitement selon la phase en cours
                   MOVE DFHCOMMAREA TO WS-COMMAREA
                   PERFORM 2000-TRAITEMENT THRU 2000-FIN
           END-EVALUATE

      *    Retour pseudo-conversationnel
           EXEC CICS RETURN
               TRANSID('MAJO')
               COMMAREA(WS-COMMAREA)
               LENGTH(LENGTH OF WS-COMMAREA)
           END-EXEC.
```

**Paragraphe d'affichage avec protection du NUMCPT :**

```cobol
       3100-AFFICHER-CLIENT.
      *-----------------------------------------------------------------
      * Affiche les données du client dans la MAP
      * NUMCPT passe en ASKIP (protégé) - clé non modifiable
      *-----------------------------------------------------------------
           MOVE LOW-VALUES TO MAPMAJO

      *    Transfert des données vers la MAP
           MOVE CLI-NUMCPT   TO NUMCPTO
           MOVE CLI-CODREG   TO CODREGO
           MOVE CLI-NATCPT   TO NATCPTO
           MOVE CLI-NOM      TO NOMO
           MOVE CLI-PRENOM   TO PRENOMO
           MOVE CLI-DATNAISS TO DATNAO
           MOVE CLI-SEXE     TO SEXEO
           MOVE CLI-ACTPRO   TO ACTPROO
           MOVE CLI-SITSO    TO SITSOO
           MOVE CLI-ADRESSE  TO ADRESSEO
           MOVE CLI-SOLDE    TO SOLDEO
           MOVE CLI-POSITION TO POSITO

      *    IMPORTANT : Protéger le numéro de compte (clé non modifiable)
      *    DFHBMASK = X'20' = ASKIP (protégé, intensité normale)
           MOVE DFHBMASK TO NUMCPTA

           MOVE 'CLIENT TROUVE - MODIFIER ET VALIDER AVEC ENTER' TO MSGO

           EXEC CICS SEND MAP('MAPMAJ')
               MAPSET('CLIMAJ')
               ERASE
           END-EXEC.
```

**Paragraphe de fusion des modifications :**

```cobol
       4050-FUSIONNER-MODIFICATIONS.
      *-----------------------------------------------------------------
      * Fusionne les modifications de l'utilisateur avec les données
      * actuelles du client. Seuls les champs modifiés (longueur > 0)
      * remplacent les valeurs existantes.
      *-----------------------------------------------------------------
      *    Code région : si modifié, prendre la nouvelle valeur
           IF WS-CODREGL > 0
               MOVE WS-CODREG TO CLI-CODREG
           ELSE
               MOVE CLI-CODREG TO WS-CODREG
           END-IF

      *    Nom : si modifié, prendre la nouvelle valeur
           IF WS-NOML > 0
               MOVE WS-NOM TO CLI-NOM
           ELSE
               MOVE CLI-NOM TO WS-NOM
           END-IF

      *    Sexe : si modifié, prendre la nouvelle valeur
           IF WS-SEXEL > 0
               MOVE WS-SEXE TO CLI-SEXE
           ELSE
               MOVE CLI-SEXE TO WS-SEXE
           END-IF
      *    ... (même logique pour tous les champs)
```

**Paragraphe READ UPDATE + REWRITE atomique :**

```cobol
       4300-ECRIRE-MODIFICATION.
      *-----------------------------------------------------------------
      * Mise à jour de l'enregistrement avec READ UPDATE + REWRITE
      *
      * IMPORTANT : Le REWRITE nécessite un READ UPDATE préalable
      * dans la même unité de travail (UOW).
      *-----------------------------------------------------------------
      *    READ UPDATE pour verrouiller l'enregistrement
           EXEC CICS READ
               FILE('FCLIENT')
               INTO(ENR-CLIENT)
               RIDFLD(CLI-NUMCPT)
               UPDATE
               RESP(WS-RESP)
           END-EXEC

           IF WS-RESP NOT = DFHRESP(NORMAL)
               MOVE 'ERREUR VERROUILLAGE - REESSAYEZ' TO MSGO
               MOVE 'O' TO WS-ERREUR
               GO TO 4300-FIN
           END-IF

      *    Réappliquer les modifications sur l'enregistrement lu
           MOVE WS-CODREG    TO CLI-CODREG
           MOVE WS-NATCPT    TO CLI-NATCPT
           MOVE WS-NOM       TO CLI-NOM
           MOVE WS-PRENOM    TO CLI-PRENOM
           MOVE WS-DATNAISS  TO CLI-DATNAISS
           MOVE WS-SEXE      TO CLI-SEXE
           MOVE WS-ACTPRO    TO CLI-ACTPRO
           MOVE WS-SITSO     TO CLI-SITSO
           MOVE WS-ADRESSE   TO CLI-ADRESSE
           MOVE WS-SOLDE     TO CLI-SOLDE
           MOVE WS-POSITION  TO CLI-POSITION

      *    REWRITE - Mise à jour effective
           EXEC CICS REWRITE
               FILE('FCLIENT')
               FROM(ENR-CLIENT)
               RESP(WS-RESP)
           END-EXEC

           EVALUATE WS-RESP
               WHEN DFHRESP(NORMAL)
                   MOVE LOW-VALUES TO MAPMAJO
                   MOVE WS-NUMCPT TO NUMCPTO
                   MOVE 'MISE A JOUR EFFECTUEE - NOUVEAU OU PF3'
                       TO MSGO
      *            Retour en phase recherche pour nouveau client
                   MOVE '1' TO WS-PHASE
                   MOVE SPACES TO WS-NUMCPT-SAVED
               WHEN OTHER
                   MOVE 'ERREUR MISE A JOUR - CONTACTEZ SUPPORT' TO MSGO
                   MOVE 'O' TO WS-ERREUR
           END-EVALUATE.

       4300-FIN.
           EXIT.
```

**JCL de compilation : CMPMAJ.jcl**

Le JCL de compilation suit la même structure que CMPCLAF.jcl (voir Partie 1, Exercice 3). Seuls le nom du job (ROCHA10) et le membre source (PRGMAJ) changent.

> **Note** : Ce programme nécessite trois copybooks : `DFHAID` (touches fonction), `DFHBMSCA` (constantes attribut), et `CLIMAJ` (structure MAP).

### Structure du programme

| Paragraphe | Fonction |
|------------|----------|
| 0000-PRINCIPAL | Point d'entrée, aiguillage selon EIBCALEN et EIBAID |
| 1000-INIT-RECHERCHE | Affichage écran vide pour saisie numéro |
| 2000-TRAITEMENT | Aiguillage selon la phase en cours |
| 3000-RECHERCHER-CLIENT | Phase 1→2 : Recherche et affichage |
| 3100-AFFICHER-CLIENT | Transfert données vers MAP, protection NUMCPT |
| 4000-VALIDER-MODIFICATION | Phase 2/3 : Réception et validation |
| 4050-FUSIONNER-MODIFICATIONS | Fusion modifications/données actuelles |
| 4100-VALIDER-DONNEES | Contrôles de conformité |
| 4300-ECRIRE-MODIFICATION | READ UPDATE + REWRITE atomique |
| 9000-FIN-PROGRAMME | Message de fin et RETURN sans TRANSID |

### Commandes CICS utilisées

| Commande | Usage |
|----------|-------|
| SEND MAP | Envoyer l'écran (avec ERASE pour effacer) |
| RECEIVE MAP | Recevoir la saisie avec RESP pour MAPFAIL |
| READ FILE | Lecture simple (phase recherche/relecture) |
| READ UPDATE | Verrouillage pour REWRITE |
| REWRITE FILE | Mise à jour de l'enregistrement |
| RETURN TRANSID | Retour pseudo-conversationnel avec COMMAREA |
| SEND TEXT | Message de fin (sans MAP) |

### Messages d'erreur gérés

| Message | Contexte |
|---------|----------|
| SAISIR LE NUMERO DE COMPTE A MODIFIER | Premier passage |
| VEUILLEZ SAISIR UN NUMERO DE COMPTE | MAPFAIL en phase recherche |
| NUMERO DE COMPTE OBLIGATOIRE | Champ NUMCPT vide |
| NUMERO DE COMPTE DOIT ETRE NUMERIQUE | Caractères non numériques |
| CLIENT INEXISTANT - VERIFIEZ LE NUMERO | NOTFND lors du READ |
| CLIENT TROUVE - MODIFIER ET VALIDER | Affichage réussi |
| AUCUNE MODIFICATION - ENTREZ DES DONNEES | MAPFAIL en phase validation |
| CODE REGION INVALIDE (01/02/03/04) | Code différent des valeurs autorisées |
| NOM OBLIGATOIRE | Champ NOM vide après fusion |
| SEXE INVALIDE (M OU F) | Sexe différent de M ou F |
| SITUATION INVALIDE (C/M/D/V) | Situation non reconnue |
| POSITION INVALIDE (DB OU CR) | Position non reconnue |
| ERREUR VERROUILLAGE - REESSAYEZ | Échec du READ UPDATE |
| MISE A JOUR EFFECTUEE - NOUVEAU OU PF3 | REWRITE réussi |

### Définition CICS

```
CEDA DEFINE PROGRAM(PRGMAJ) GROUP(CLIGROUP) LANGUAGE(COBOL)
CEDA INSTALL PROGRAM(PRGMAJ) GROUP(CLIGROUP)
```

### Vérification

```
CEMT INQ PROGRAM(PRGMAJ)
```

Résultat attendu : `Prog(PRGMAJ) Cob Ena`

### Points importants

1. **COPY DFHBMSCA** : Copybook système contenant les constantes d'attribut (DFHBMASK, DFHBMUNN, etc.). Obligatoire pour modifier dynamiquement les attributs des champs.

2. **Sauvegarde du NUMCPT dans la COMMAREA** : Une fois protégé (ASKIP), le champ n'est plus transmis par le terminal. On doit le conserver dans WS-NUMCPT-SAVED pour les phases suivantes.

3. **READ simple en phase recherche** : La première lecture n'utilise pas UPDATE car le verrouillage ne persisterait pas après la fin de tâche (mode pseudo-conversationnel).

4. **READ UPDATE + REWRITE atomiques** : Les deux commandes doivent être dans le même paragraphe, exécutées séquentiellement, pour garantir que le verrouillage est actif au moment du REWRITE.

5. **Retour en phase 1 après succès** : Après une mise à jour réussie, le programme réinitialise la COMMAREA pour permettre la modification d'un autre client sans relancer la transaction.

6. **PERFORM THRU avec GO TO** : Comme pour PRGAJT (voir Partie 2a, Exercice 7), la clause THRU permet aux GO TO de rester dans la plage du PERFORM et de retourner correctement à l'appelant.

### Captures d'écran

<!--
Suggestions de captures d'écran pour cet exercice :

1. pt2ex10-1 : Source COBOL dans ISPF EDIT - ROCHA.CICS.SOURCE(PRGMAJ)
2. pt2ex10-2 : Soumission JCL CMPMAJ - compilation du programme
3. pt2ex10-3 : SDSF - Job output avec RC=0000 pour compilation
4. pt2ex10-4 : Écran phase 1 - saisie numéro de compte (NUMCPT saisissable)
5. pt2ex10-5 : Écran phase 2 - affichage client (NUMCPT protégé, visible mais grisé)
6. pt2ex10-6 : Écran phase 3 - message "MISE A JOUR EFFECTUEE"
7. pt2ex10-7 : Vérification avec AFFI - données modifiées visibles
-->

---

## Exercice 11 : Transaction de mise à jour

### Énoncé

Définir une transaction indépendante de la précédente pour appeler le programme de mise à jour.

### Mon travail

La transaction MAJO est le point d'entrée utilisateur pour la mise à jour des clients. Comme pour AFFI et AJOU, elle fait le lien entre le code saisi par l'utilisateur et le programme COBOL-CICS à exécuter.

#### Architecture CICS - Liaison des ressources

```
+-------------+     +-------------+     +-------------+
| TRANSACTION | --> | PROGRAMME   | --> | MAPSET      |
|    MAJO     |     |   PRGMAJ    |     |   CLIMAJ    |
+-------------+     +-------------+     +-------------+
                           |
                           v
                    +-------------+
                    |   FICHIER   |
                    |   FCLIENT   |
                    +-------------+
```

### Résolution

**Étape 1 : Définition de la transaction**

```
CEDA DEFINE TRANSACTION(MAJO) GROUP(CLIGROUP) PROGRAM(PRGMAJ)
```

| Paramètre | Valeur | Description |
|-----------|--------|-------------|
| TRANSACTION | MAJO | Code transaction (4 caractères max) |
| GROUP | CLIGROUP | Groupe de ressources du projet |
| PROGRAM | PRGMAJ | Programme COBOL à exécuter |

**Étape 2 : Installation de la transaction**

```
CEDA INSTALL TRANSACTION(MAJO) GROUP(CLIGROUP)
```

> **Bonne pratique** : Installer uniquement la ressource ajoutée (`CEDA INSTALL TRANSACTION`) plutôt que tout le groupe (`CEDA INSTALL GROUP`). Réinstaller le groupe complet peut causer des erreurs si certaines ressources (comme FCLIENT) sont déjà ouvertes.

### Vérification

```
CEDA VIEW TRANSACTION(MAJO) GROUP(CLIGROUP)
```
Résultat attendu : Affichage de la définition avec PROGRAM(PRGMAJ)

```
CEMT INQ TRAN(MAJO)
```
Résultat attendu : `Tra(MAJO) Pro(PRGMAJ) Ena`

### Test de la transaction

**Test sans debugger :**

```
MAJO
```

Comportement attendu :
1. Écran de saisie du numéro de compte (NUMCPT saisissable)
2. Saisir un numéro existant (ex: 000001) et ENTER
3. Affichage des données du client (NUMCPT protégé/grisé)
4. Modifier les champs souhaités (ex: changer l'adresse)
5. ENTER pour valider → Message "MISE A JOUR EFFECTUEE"
6. L'écran revient en phase 1 pour un nouveau client
7. PF3 pour quitter

**Test avec CEDF** (voir Partie 1, Exercice 5 pour la navigation CEDF) :

```
CEDF
MAJO
```

Points d'arrêt observés pour une mise à jour complète :

| Étape | Commande CICS | RESP attendu | Phase |
|-------|---------------|--------------|-------|
| 1 | SEND MAP | NORMAL | 1 - Écran recherche |
| 2 | RETURN TRANSID | - | Fin phase 1 |
| 3 | RECEIVE MAP | NORMAL | 2 - Réception numéro |
| 4 | READ FILE | NORMAL | 2 - Vérification existence |
| 5 | SEND MAP | NORMAL | 2 - Affichage client |
| 6 | RETURN TRANSID | - | Fin phase 2 |
| 7 | RECEIVE MAP | NORMAL | 3 - Réception modifications |
| 8 | READ FILE | NORMAL | 3 - Relecture données |
| 9 | READ UPDATE | NORMAL | 3 - Verrouillage |
| 10 | REWRITE | NORMAL | 3 - Mise à jour |
| 11 | SEND MAP | NORMAL | 3 - Message succès |
| 12 | RETURN TRANSID | - | Retour phase 1 |

> **Note** : Si le client n'existe pas, l'étape 4 retourne NOTFND et le programme affiche un message d'erreur sans passer à la phase 2.

### Ressources du groupe CLIGROUP après exercice 11

| Type | Nom | Description | Défini dans |
|------|-----|-------------|-------------|
| FILE | FCLIENT | Fichier VSAM clients | Exercice 1 |
| MAPSET | CLIAFF | Écran affichage | Exercice 4 |
| MAPSET | CLIAJT | Écran ajout | Exercice 8 |
| MAPSET | CLIMAJ | Écran mise à jour | Exercice 9 |
| PROGRAM | PRGCLIA | Programme affichage | Exercice 4 |
| PROGRAM | PRGAJT | Programme ajout | Exercice 8 |
| PROGRAM | PRGMAJ | Programme mise à jour | Exercice 10 |
| TRANSACTION | AFFI | Transaction affichage | Exercice 4 |
| TRANSACTION | AJOU | Transaction ajout | Exercice 8 |
| TRANSACTION | MAJO | Transaction mise à jour | Exercice 11 |

### Captures d'écran

<!--
Suggestions de captures d'écran pour cet exercice :

1. pt2ex11-1 : CEDA DEFINE TRANSACTION(MAJO) - écran de définition
2. pt2ex11-2 : CEDA INSTALL TRANSACTION(MAJO) - message INSTALL SUCCESSFUL
3. pt2ex11-3 : CEMT INQ TRAN(MAJO) - vérification statut Ena
4. pt2ex11-4 : Test CEDF - point d'arrêt sur READ UPDATE
5. pt2ex11-5 : Test CEDF - point d'arrêt sur REWRITE avec RESP NORMAL
6. pt2ex11-6 : Test fonctionnel - écran avec message "MISE A JOUR EFFECTUEE"
7. pt2ex11-7 : Vérification avec AFFI - les données modifiées sont visibles
-->

---

[< Partie 2a : Ajout](03-partie-2a-ajout.md) | [Retour au sommaire](00-introduction.md) | [Partie 2c : Suppression >](05-partie-2c-suppression.md)
