# Conclusion et Annexes

[< Partie 3 : Avancees](06-partie-3-avancees.md) | [Retour a l'introduction](00-introduction.md)

---

## Bilan du projet

### Difficultes rencontrees et solutions

| Probleme | Cause | Solution |
|----------|-------|----------|
| Erreur VSAM 108 au chargement | Longueur incorrecte des enregistrements | Le DD * du JCL lit en LRECL=80 par defaut. Definir RECORDSIZE(80 80) et utiliser un FILLER de 16 octets |
| Volume non specifie (TK4-) | Parametre manquant | Ajouter VOLUMES(FDDBAS) dans la definition du cluster VSAM |
| Fichier VSAM vide apres REPRO | LRECL incompatible | Passer a 80 octets pour tous les enregistrements |
| Donnees effacees apres mise a jour | Bug fusion des modifications | Ajouter clause ELSE pour preserver les champs non modifies dans le paragraphe 4050-FUSIONNER-MODIFICATIONS |

### Competences mises en oeuvre

**VSAM et CICS :**
- Definition de fichiers VSAM KSDS (IDCAMS)
- Integration fichiers dans CICS (FCT - File Control Table)
- Commandes CICS : READ, WRITE, REWRITE, DELETE
- Navigation VSAM : STARTBR, READNEXT, ENDBR

**BMS et Ecrans :**
- Conception d'ecrans BMS (Basic Mapping Support)
- Gestion des attributs (ASKIP, UNPROT, BRT)
- Commandes SEND MAP, RECEIVE MAP

**Programmation COBOL-CICS :**
- Mode pseudo-conversationnel (RETURN TRANSID, COMMAREA)
- LINKAGE SECTION pour DFHCOMMAREA
- Validation et controle des donnees saisies
- Gestion des erreurs (RESP, DFHRESP)

**Administration :**
- Definition de transactions (CEDA DEFINE)
- Installation de ressources (CEDA INSTALL)
- Debogage avec CEDF

---

## Reference des Commandes CICS

### Operations sur enregistrements

| Commande | Usage | Prerequis |
|----------|-------|-----------|
| **READ** | Lecture directe par cle | Aucun |
| **READ UPDATE** | Lecture avec verrouillage | Pour REWRITE |
| **WRITE** | Ajout nouvel enregistrement | Le client ne doit PAS exister |
| **REWRITE** | Mise a jour enregistrement | READ UPDATE obligatoire dans meme UOW |
| **DELETE** | Suppression enregistrement | Aucun |

### Navigation VSAM (Browse)

| Commande | Usage |
|----------|-------|
| **STARTBR** | Positionner le curseur sur une cle (partielle) |
| **READNEXT** | Lire l'enregistrement suivant |
| **ENDBR** | Terminer le parcours et liberer les ressources |

### Ecrans BMS

| Commande | Usage |
|----------|-------|
| **SEND MAP** | Afficher un ecran a l'utilisateur |
| **RECEIVE MAP** | Recevoir les donnees saisies |
| **RETURN TRANSID** | Retour pseudo-conversationnel |
| **RETURN** | Fin de transaction |

---

## Annexes

### Liste des programmes COBOL-CICS

| Programme | Transaction | Commande | Description |
|-----------|-------------|----------|-------------|
| PRGCLIA | AFFI | READ | Affichage d'un client |
| PRGAJT | AJOU | WRITE | Ajout d'un nouveau client |
| PRGMAJ | MAJO | REWRITE | Mise a jour d'un client |
| PRGSUP | SUPP | DELETE | Suppression d'un client |
| PRGSUL | SULE | READ+DELETE | Suppression avec lecture prealable |
| PRGSDEL | SDEL | STARTBR+DELETE | Suppression par code generique |
| PRGLGEN | LGEN | READNEXT | Liste par code generique |
| PRGSTAT | STAT | STARTBR+READNEXT | Statistiques par region |

### Liste des MAPs BMS

| Mapset | Map | Programme | Description |
|--------|-----|-----------|-------------|
| CLIAFF | MAPAFF | PRGCLIA | Ecran d'affichage client |
| CLIAJT | MAPAJT | PRGAJT | Ecran d'ajout client |
| CLIMAJ | MAPMAJ | PRGMAJ | Ecran de mise a jour |
| CLISUP | MAPSUP | PRGSUP | Ecran de suppression |
| CLISTAT | MAPSTAT | PRGSTAT | Ecran de statistiques |

### Liste des transactions CICS

| Code | Programme | Description |
|------|-----------|-------------|
| AFFI | PRGCLIA | Affichage client |
| AJOU | PRGAJT | Ajout client |
| MAJO | PRGMAJ | Mise a jour client |
| SUPP | PRGSUP | Suppression client |
| SULE | PRGSUL | Suppression avec lecture |
| SDEL | PRGSDEL | Suppression generique |
| LGEN | PRGLGEN | Liste generique |
| STAT | PRGSTAT | Statistiques region |

### Structure du fichier CLIENT (80 octets)

| Position | Champ | Type | Longueur | Description |
|----------|-------|------|----------|-------------|
| 01-06 | NUMCPT | NUM | 6 | Numero compte (cle) |
| 07-08 | CODREG | NUM | 2 | Code region (01-04) |
| 09-10 | NATCPT | NUM | 2 | Nature compte |
| 11-20 | NOM | ALPHA | 10 | Nom client |
| 21-30 | PRENOM | ALPHA | 10 | Prenom client |
| 31-38 | DATNAISS | NUM | 8 | Date naissance (AAAAMMJJ) |
| 39 | SEXE | ALPHA | 1 | Sexe (M/F) |
| 40-41 | ACTPRO | NUM | 2 | Activite professionnelle |
| 42 | SITSO | ALPHA | 1 | Situation sociale (C/M/D/V) |
| 43-52 | ADRESSE | ALPHA | 10 | Adresse |
| 53-62 | SOLDE | NUM | 10 | Solde |
| 63-64 | POSITION | ALPHA | 2 | Position (DB/CR) |
| 65-80 | FILLER | - | 16 | Reserve |

### Messages d'erreur standards

| Message | Contexte |
|---------|----------|
| ENREGISTREMENT EN DOUBLE | Ajout d'un client existant |
| ZONE NUMERIQUE, RESAISIR CE CHAMP | Champ numerique invalide |
| REGION INEXISTANTE, SAISIR CODE REGION | Code region invalide |
| CLIENT INEXISTANT | Recherche sans resultat |
| SUPPRESSION EFFECTUEE | Confirmation suppression |
| MISE A JOUR EFFECTUEE | Confirmation mise a jour |

---

## Conclusion

Ce projet m'a permis de mettre en pratique l'ensemble des competences acquises durant la formation POEI Mainframe COBOL pour le volet CICS. A travers les trois parties du projet, j'ai pu :

- **Maitriser VSAM sous CICS** : Definition de fichiers KSDS, integration dans la FCT (File Control Table), et gestion des operations de lecture, ecriture, mise a jour et suppression.

- **Developper des ecrans BMS** : Conception de MAPs avec gestion des attributs (couleurs, protection), zones de saisie et d'affichage, messages d'erreur.

- **Programmer en COBOL-CICS** : Utilisation des commandes CICS (SEND/RECEIVE MAP, READ, WRITE, REWRITE, DELETE), gestion pseudo-conversationnelle avec RETURN TRANSID, et navigation VSAM avec STARTBR/READNEXT/ENDBR.

- **Administrer les transactions** : Definition via CEDA, installation de groupes, tests avec CEDF.

Le projet couvre un cas concret de gestion clientele dans le secteur financier, avec **8 programmes COBOL-CICS**, **5 MAPs BMS** et **8 transactions**. Les principales difficultes rencontrees (gestion des attributs BMS, validation des donnees, navigation VSAM, fusion des modifications) m'ont permis de developper une approche methodique de resolution de problemes.

Cette experience constitue une base solide pour aborder des projets mainframe transactionnels en entreprise.

---

*Rapport realise par Josue ROCHA - Formation POEI Mainframe COBOL - M2i Formation, Strasbourg - Janvier 2026*

---

[< Partie 3 : Avancees](06-partie-3-avancees.md) | [Retour a l'introduction](00-introduction.md)
