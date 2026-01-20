# Conclusion et Annexes

[< Partie 3 : Avancées](06-partie-3-avancees.md) | [Retour à l'introduction](00-introduction.md)

---

## Bilan du projet

### Synthèse chiffrée

| Catégorie | Quantité | Détails |
|-----------|----------|---------|
| **Programmes COBOL-CICS** | 7 | PRGCLIA, PRGAJT, PRGMAJ, PRGSUP, PRGDELG, PRGLGEN, PRGSTAT |
| **MAPs BMS** | 7 | CLIAFF, CLIAJT, CLIMAJ, CLISUP, CLIDEL, CLILIST, CLISTAT |
| **Transactions CICS** | 7 | AFFI, AJOU, MAJO, SUPP, DELG, LGEN, STAT |
| **Fichiers VSAM** | 2 | FCLIENT (KSDS), PCLIENT (PATH via AIX) |
| **JCL** | 17 | Définition VSAM, assemblage BMS, compilation COBOL |
| **Exercices réalisés** | 19 | Répartis en 4 parties thématiques |
| **Captures d'écran** | 160+ | Documentation complète de chaque étape |

**Commandes CICS maîtrisées :**

| Opération | Commandes |
|-----------|-----------|
| Lecture | READ, READ UPDATE |
| Écriture | WRITE, REWRITE, DELETE |
| Navigation | STARTBR, READNEXT, ENDBR |
| Écrans | SEND MAP, RECEIVE MAP |
| Contrôle | RETURN, RETURN TRANSID |

### Difficultés rencontrées et solutions

#### Définition VSAM et chargement (Partie 1)

| Problème | Cause | Solution |
|----------|-------|----------|
| Erreur VSAM 108 au chargement | Longueur incorrecte des enregistrements | Le DD * du JCL lit en LRECL=80 par défaut. Définir RECORDSIZE(80 80) et utiliser un FILLER de 16 octets |
| Volume non spécifié (TK4-) | Paramètre manquant | Ajouter VOLUMES(FDDBAS) dans la définition du cluster VSAM |
| Fichier VSAM vide après REPRO | LRECL incompatible | Passer à 80 octets pour tous les enregistrements |

#### Programmation COBOL-CICS (Parties 2 et 3)

| Problème | Cause | Solution | Programme(s) |
|----------|-------|----------|--------------|
| Écrasement données saisies | MODE=INOUT partage zones I/O (suffixes I et O) | Sauvegarder dans WS-SAISIE immédiatement après RECEIVE MAP, avant tout MOVE LOW-VALUES | PRGAJT |
| PERFORM sans THRU (GO TO bug) | GO TO vers paragraphe-FIN sort de la plage du PERFORM | Utiliser `PERFORM ... THRU paragraphe-FIN` pour inclure le paragraphe de sortie | PRGAJT, PRGLGEN, PRGSTAT |
| Message erreur non visible | SEND MAP sans ERASE ne rafraîchit pas l'écran | Ajouter `ERASE` au SEND MAP d'erreur | PRGAJT |
| LINKAGE SECTION manquante | DFHCOMMAREA non déclarée | Ajouter LINKAGE SECTION avec DFHCOMMAREA pour accéder aux données du RETURN précédent | PRGMAJ |
| Données effacées après MAJ partielle | Champs non modifiés ont longueur = 0 | Fusionner avec les données actuelles : si longueur > 0, utiliser la saisie ; sinon, conserver l'existant | PRGMAJ |
| Deadlock DELETE pendant browse | DELETE demande verrou exclusif pendant STARTBR actif | Collecter les clés en table (max 100), ENDBR, puis DELETE en boucle | PRGDELG |
| COMMAREA non réinitialisée | Contexte précédent conservé si aucun résultat | Réinitialiser WS-COMMAREA quand aucun client trouvé | PRGLGEN |
| NUMVAL non supporté | `FUNCTION NUMVAL` incompatible avec MOVE sur IBM Enterprise COBOL | Utiliser `REDEFINES` pour réinterpréter la zone alphanumérique comme numérique | PRGSTAT |
| Lignes COBOL > 72 colonnes | Messages trop longs dans le source | Raccourcir les messages ou les découper sur plusieurs lignes | PRGSTAT |

#### Administration CICS

| Problème | Cause | Solution |
|----------|-------|----------|
| CEDA INSTALL GROUP échoue | Fichier FCLIENT déjà ouvert | Installer uniquement la ressource spécifique : `CEDA INSTALL TRANSACTION(xxx)` |
| CEMT INQ MAPSET inexistant | Commande non disponible pour les mapsets | Utiliser `CEDA VIEW MAPSET(xxx)` à la place |
| Justification à droite des clés | Attribut NUM sur champ préfixe | Utiliser PIC X sans NUM pour les champs de clé partielle |

#### Assemblage BMS et JCL (Partie 3)

| Problème | Cause | Solution |
|----------|-------|----------|
| Erreurs assemblage BMS (CLILIST) | Structure MAP trop complexe | Simplifier la MAP en réduisant le nombre de champs ou en utilisant des noms plus courts |
| Format JCL incorrect | Paramètres mal positionnés dans ASMLIST/CMPLGEN | Corriger l'alignement et la syntaxe des cartes JCL |

### Compétences mises en œuvre

**VSAM et CICS :**

- Définition de fichiers VSAM KSDS (IDCAMS)
- Création d'AIX (Alternate Index) et PATH pour accès par clé alternative
- Intégration fichiers dans CICS (FCT - File Control Table)
- Commandes CICS : READ, WRITE, REWRITE, DELETE
- Navigation VSAM : STARTBR, READNEXT, ENDBR

**BMS et Écrans :**

- Conception d'écrans BMS (Basic Mapping Support)
- Gestion des attributs (ASKIP, UNPROT, BRT)
- Commandes SEND MAP, RECEIVE MAP

**Programmation COBOL-CICS :**

- Mode pseudo-conversationnel (RETURN TRANSID, COMMAREA)
- LINKAGE SECTION pour DFHCOMMAREA
- Validation et contrôle des données saisies
- Gestion des erreurs (RESP, DFHRESP)

**Administration :**

- Définition de transactions (CEDA DEFINE)
- Installation de ressources (CEDA INSTALL)
- Débogage avec CEDF

---

## Conclusion

Ce projet m'a permis de mettre en pratique l'ensemble des compétences acquises durant la formation POEI Mainframe COBOL pour le volet CICS. À travers les différentes parties du projet, j'ai pu :

- **Maîtriser VSAM sous CICS** : Définition de fichiers KSDS, création d'index alternatifs (AIX) avec PATH pour l'accès par clé secondaire, intégration dans la FCT (File Control Table), et gestion des opérations de lecture, écriture, mise à jour et suppression.

- **Développer des écrans BMS** : Conception de MAPs avec gestion des attributs (couleurs, protection), zones de saisie et d'affichage, messages d'erreur.

- **Programmer en COBOL-CICS** : Utilisation des commandes CICS (SEND/RECEIVE MAP, READ, WRITE, REWRITE, DELETE), gestion pseudo-conversationnelle avec RETURN TRANSID, et navigation VSAM avec STARTBR/READNEXT/ENDBR.

- **Administrer les transactions** : Définition via CEDA, installation de groupes, tests avec CEDF.

Le projet couvre un cas concret de gestion clientèle dans le secteur financier, avec **7 programmes COBOL-CICS**, **7 MAPs BMS**, **7 transactions** et **2 fichiers VSAM** (FCLIENT pour l'accès direct, PCLIENT via AIX/PATH pour les statistiques par région). Les principales difficultés rencontrées (gestion des attributs BMS, validation des données, navigation VSAM, fusion des modifications, deadlock lors de suppressions multiples, optimisation des accès via AIX) m'ont permis de développer une approche méthodique de résolution de problèmes.

Cette expérience constitue une base solide pour aborder des projets mainframe transactionnels en entreprise.

---

## Annexes

### A. Vue d'ensemble du projet

Ces captures montrent l'ensemble des ressources créées au cours du projet.

#### Ressources CICS - Groupe CLIGROUP

Le groupe CLIGROUP contient toutes les ressources définies pour l'application de gestion clientèle.

![CEDA DISPLAY GROUP - Partie 1](../captures/conclusion/1.PNG)

*Vue des ressources du groupe CLIGROUP : 2 fichiers (FCLIENT, PCLIENT), 7 mapsets BMS (CLIAFF à CLISTAT), et les premiers programmes COBOL.*

![CEDA DISPLAY GROUP - Partie 2](../captures/conclusion/2.PNG)

*Suite des ressources : les 7 programmes (PRGAJT à PRGSUP) et les 7 transactions (AFFI, AJOU, DELG, LGEN, MAJO, STAT, SUPP).*

#### Datasets sur z/OS

L'ensemble des fichiers du projet sont organisés dans l'espace de noms ROCHA.CICS.

![DSLIST - Datasets ROCHA.CICS](../captures/conclusion/3.PNG)

*Liste des 10 datasets du projet : CLIENT (VSAM KSDS), AIX (index alternatif), PATH, les composants VSAM (DATA, INDEX), et les bibliothèques (LINK, LOAD, SOURCE).*

#### Bibliothèque des sources - ROCHA.CICS.SOURCE

Cette bibliothèque contient tous les codes sources : BMS, COPY, JCL et programmes COBOL.

![ROCHA.CICS.SOURCE - Partie 1](../captures/conclusion/4.PNG)

*Membres sources : ASM* (JCL d'assemblage BMS), CLI* (sources BMS des mapsets), CMP* (JCL de compilation), ainsi que les tailles et dates de modification.*

![ROCHA.CICS.SOURCE - Partie 2](../captures/conclusion/5.PNG)

*Suite des membres : CMP* (compilations), DEFPATH (définition AIX/PATH), DEFVSAM (définition cluster), LOADVSAM (chargement données), et les 7 programmes PRG* (PRGAJT, PRGCLIA, PRGDELG, PRGLGEN, PRGMAJ, PRGSTAT, PRGSUP).*

#### Bibliothèque des modules - ROCHA.CICS.LOAD

Cette bibliothèque contient les programmes compilés (load modules) prêts à être exécutés.

![ROCHA.CICS.LOAD - Modules compilés](../captures/conclusion/6.PNG)

*14 modules load : 7 mapsets BMS compilés (CLI*) et 7 programmes COBOL-CICS compilés (PRG*). On note la taille de chaque module et le mode d'adressage (AMODE 24/31).*

#### Bibliothèque des linkedits BMS - ROCHA.CICS.LINK

Cette bibliothèque contient les copybooks générés lors de l'assemblage BMS.

![ROCHA.CICS.LINK - Copybooks BMS](../captures/conclusion/7.PNG)

*7 copybooks BMS (CLI*) générés par l'assembleur. Ces copybooks sont inclus dans les programmes COBOL via la COPY statement pour décrire la structure des écrans.*

---

### B. Référence des commandes CICS

#### Opérations sur enregistrements

| Commande | Usage | Prérequis |
|----------|-------|-----------|
| **READ** | Lecture directe par clé | Aucun |
| **READ UPDATE** | Lecture avec verrouillage | Pour REWRITE |
| **WRITE** | Ajout nouvel enregistrement | Le client ne doit PAS exister |
| **REWRITE** | Mise à jour enregistrement | READ UPDATE obligatoire dans même UOW |
| **DELETE** | Suppression enregistrement | Aucun |

#### Navigation VSAM (Browse)

| Commande | Usage |
|----------|-------|
| **STARTBR** | Positionner le curseur sur une clé (partielle) avec GTEQ |
| **READNEXT** | Lire l'enregistrement suivant |
| **ENDBR** | Terminer le parcours et libérer les ressources |

#### Écrans BMS

| Commande | Usage |
|----------|-------|
| **SEND MAP** | Afficher un écran à l'utilisateur |
| **RECEIVE MAP** | Recevoir les données saisies |
| **RETURN TRANSID** | Retour pseudo-conversationnel |
| **RETURN** | Fin de transaction |

---

### C. Liste des programmes COBOL-CICS

| Programme | Transaction | Commandes | Fichier | Description |
|-----------|-------------|-----------|---------|-------------|
| PRGCLIA | AFFI | READ | FCLIENT | Affichage d'un client |
| PRGAJT | AJOU | WRITE | FCLIENT | Ajout d'un nouveau client |
| PRGMAJ | MAJO | READ UPDATE, REWRITE | FCLIENT | Mise à jour d'un client |
| PRGSUP | SUPP | READ, DELETE | FCLIENT | Suppression d'un client (avec affichage) |
| PRGDELG | DELG | STARTBR, READNEXT, DELETE | FCLIENT | Suppression générique par préfixe |
| PRGLGEN | LGEN | STARTBR, READNEXT | FCLIENT | Liste générique paginée (10 clients/page) |
| PRGSTAT | STAT | STARTBR, READNEXT | PCLIENT | Statistiques par région via AIX/PATH |

---

### D. Liste des MAPs BMS

| Mapset | Map | Programme | Description |
|--------|-----|-----------|-------------|
| CLIAFF | MAPAFF | PRGCLIA | Écran d'affichage client |
| CLIAJT | MAPAJT | PRGAJT | Écran d'ajout client |
| CLIMAJ | MAPMAJ | PRGMAJ | Écran de mise à jour |
| CLISUP | MAPSUP | PRGSUP | Écran de suppression |
| CLIDEL | MAPDEL | PRGDELG | Écran de suppression générique |
| CLILIST | MAPLGEN | PRGLGEN | Écran de liste générique paginée |
| CLISTAT | MAPSTAT | PRGSTAT | Écran de statistiques par région |

---

### E. Liste des transactions CICS

| Code | Programme | Description |
|------|-----------|-------------|
| AFFI | PRGCLIA | Affichage client |
| AJOU | PRGAJT | Ajout client |
| MAJO | PRGMAJ | Mise à jour client |
| SUPP | PRGSUP | Suppression client |
| DELG | PRGDELG | Suppression générique |
| LGEN | PRGLGEN | Liste générique paginée |
| STAT | PRGSTAT | Statistiques par région |

---

### F. Liste des fichiers VSAM

| Nom CICS | Dataset | Type | Description |
|----------|---------|------|-------------|
| FCLIENT | ROCHA.CICS.CLIENT | KSDS | Fichier de base (clé primaire : NUMCPT) |
| PCLIENT | ROCHA.CICS.CLIENT.PATH | PATH | Accès via AIX sur CODREG |

**Architecture VSAM :**

```
ROCHA.CICS.CLIENT (KSDS)          ← Fichier de base
    │
    └── ROCHA.CICS.CLIENT.AIX     ← Index alternatif sur CODREG
           │
           └── ROCHA.CICS.CLIENT.PATH  ← PATH pour accès CICS
```

---

### G. Liste des JCL

| JCL | Description | Exercice |
|-----|-------------|----------|
| DEFVSAM.jcl | Définition du cluster VSAM KSDS | Ex 1 |
| LOADVSAM.jcl | Chargement des données initiales | Ex 1 |
| ASMCLIA.jcl | Assemblage MAP CLIAFF | Ex 3 |
| CMPCLIA.jcl | Compilation PRGCLIA | Ex 5 |
| ASMAJT.jcl | Assemblage MAP CLIAJT | Ex 7 |
| CMPAJT.jcl | Compilation PRGAJT | Ex 8 |
| ASMMAJ.jcl | Assemblage MAP CLIMAJ | Ex 10 |
| CMPMAJ.jcl | Compilation PRGMAJ | Ex 11 |
| ASMSUP.jcl | Assemblage MAP CLISUP | Ex 13 |
| CMPSUP.jcl | Compilation PRGSUP | Ex 14 |
| ASMDEL.jcl | Assemblage MAP CLIDEL | Ex 17 |
| CMPDELG.jcl | Compilation PRGDELG | Ex 17 |
| ASMLIST.jcl | Assemblage MAP CLILIST | Ex 18 |
| CMPLGEN.jcl | Compilation PRGLGEN | Ex 18 |
| DEFPATH.jcl | Définition AIX et PATH sur CODREG | Ex 19 |
| ASMSTAT.jcl | Assemblage MAP CLISTAT | Ex 19 |
| CMPSTAT.jcl | Compilation PRGSTAT | Ex 19 |

---

### H. Structure du fichier CLIENT (80 octets)

| Position | Champ | Type | Longueur | Description |
|----------|-------|------|----------|-------------|
| 01-06 | NUMCPT | NUM | 6 | Numéro compte (clé) |
| 07-08 | CODREG | NUM | 2 | Code région (01-04) |
| 09-10 | NATCPT | NUM | 2 | Nature compte |
| 11-20 | NOM | ALPHA | 10 | Nom client |
| 21-30 | PRENOM | ALPHA | 10 | Prénom client |
| 31-38 | DATNAISS | NUM | 8 | Date naissance (AAAAMMJJ) |
| 39 | SEXE | ALPHA | 1 | Sexe (M/F) |
| 40-41 | ACTPRO | NUM | 2 | Activité professionnelle |
| 42 | SITSO | ALPHA | 1 | Situation sociale (C/M/D/V) |
| 43-52 | ADRESSE | ALPHA | 10 | Adresse |
| 53-62 | SOLDE | NUM | 10 | Solde |
| 63-64 | POSITION | ALPHA | 2 | Position (DB/CR) |
| 65-80 | FILLER | - | 16 | Réserve |

---

### I. Messages d'erreur standards

| Message | Contexte |
|---------|----------|
| ENREGISTREMENT EN DOUBLE | Ajout d'un client existant |
| ZONE NUMERIQUE, RESAISIR CE CHAMP | Champ numérique invalide |
| REGION INEXISTANTE, SAISIR CODE REGION | Code région invalide |
| CLIENT INEXISTANT | Recherche sans résultat |
| SUPPRESSION EFFECTUEE | Confirmation suppression |
| MISE A JOUR EFFECTUEE | Confirmation mise à jour |

---

*Rapport réalisé par Josué ROCHA - Formation POEI Mainframe COBOL - M2i Formation, Strasbourg - Janvier 2026*

---

[< Partie 3 : Avancées](06-partie-3-avancees.md) | [Retour à l'introduction](00-introduction.md)
