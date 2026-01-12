# Exercice 2 : Creation de la MAP BMS pour affichage

## Objectif

Creer la MAP conformement a la structure du Data Set CLIENT permettant l'affichage des donnees. Prevoir le controle des donnees redondantes et une zone de message de 60 caracteres pour afficher les informations necessaires en cas d'erreur ou de saisie correcte.

## Structure de la MAP

### Mapset et Map

| Element | Nom | Description |
|---------|-----|-------------|
| MAPSET | CLIAFF | Nom du mapset (8 car max) |
| MAP | MAPAFF | Nom de la map dans le mapset |

### Zones definies

| Zone | Longueur | Type | Description |
|------|----------|------|-------------|
| NUMCPT | 6 | UNPROT,NUM,IC | Numero compte (saisie, curseur initial) |
| CODREG | 2 | ASKIP,BRT | Code region (affichage) |
| LIBREG | 15 | ASKIP,BRT | Libelle region |
| NATCPT | 2 | ASKIP,BRT | Nature compte |
| LIBNAT | 15 | ASKIP,BRT | Libelle nature |
| NOM | 10 | ASKIP,BRT | Nom client |
| PRENOM | 10 | ASKIP,BRT | Prenom client |
| DATNA | 10 | ASKIP,BRT | Date naissance (formatee) |
| SEXE | 1 | ASKIP,BRT | Sexe (M/F) |
| LIBSEX | 8 | ASKIP,BRT | Libelle sexe |
| ACTPRO | 2 | ASKIP,BRT | Code activite professionnelle |
| SITSO | 1 | ASKIP,BRT | Situation sociale |
| LIBSIT | 12 | ASKIP,BRT | Libelle situation |
| ADRESSE | 10 | ASKIP,BRT | Adresse |
| SOLDE | 12 | ASKIP,BRT | Solde (formate) |
| POSIT | 2 | ASKIP,BRT | Position (DB/CR) |
| LIBPOS | 10 | ASKIP,BRT | Libelle position |
| MSG | 60 | ASKIP,BRT | Zone message |

### Attributs BMS utilises

| Attribut | Signification |
|----------|---------------|
| ASKIP | Champ non modifiable (affichage seul) |
| UNPROT | Champ modifiable (saisie) |
| NUM | Champ numerique uniquement |
| BRT | Affichage en surbrillance |
| IC | Initial Cursor (curseur positionne ici) |

### Options du DFHMSD

```
TYPE=&SYSPARM    : MAP ou DSECT selon parametre
MODE=INOUT       : Utilisation en entree et sortie
LANG=COBOL       : Generation copybook COBOL
STORAGE=AUTO     : Allocation automatique
CTRL=(FREEKB,FRSET) : Clavier debloque, MDT remis a zero
TIOAPFX=YES      : Reserve 12 octets pour prefixe TIOA
```

## Assemblage de la MAP

### Etape 1 : Generer la MAP physique

```jcl
//ASMMAP   EXEC PGM=DFHASM
//SYSIN    DD DSN=ROCHA.CICS.SOURCE(CLIAFF),DISP=SHR
//SYSPRINT DD SYSOUT=*
//SYSLIN   DD DSN=ROCHA.CICS.LINK(CLIAFF),DISP=SHR
```

Avec `SYSPARM=MAP` pour generer le module physique.

### Etape 2 : Generer le copybook DSECT

```jcl
//ASMDSCT  EXEC PGM=DFHASM
//SYSIN    DD DSN=ROCHA.CICS.SOURCE(CLIAFF),DISP=SHR
//SYSPRINT DD DSN=ROCHA.CICS.SOURCE(CLICOPY),DISP=SHR
```

Avec `SYSPARM=DSECT` pour generer le copybook COBOL.

## Copybook genere (structure)

Le copybook genere contiendra pour chaque champ :
- `xxxL` : Longueur du champ (PIC S9(4) COMP)
- `xxxF` : Flag d'attribut
- `xxxA` : Attribut (si DSATTS specifie)
- `xxxI` : Valeur en entree
- `xxxO` : Valeur en sortie

Exemple pour NUMCPT :
```cobol
05 NUMCPTL    PIC S9(4) COMP.
05 NUMCPTF    PIC X.
05 NUMCPTI    PIC X(6).
05 NUMCPTO    REDEFINES NUMCPTI PIC X(6).
```

## Apercu de l'ecran

```
                         *** AFFICHAGE CLIENT ***
--------------------------------------------------------------------------------

  NUMERO COMPTE : ______

  CODE REGION   : __                    _______________
  NATURE COMPTE : __                    _______________
  NOM           : __________
  PRENOM        : __________
  DATE NAISSANCE: __________
  SEXE          : _         ________
  ACTIVITE PRO  : __
  SITUATION SOC : _         ____________
  ADRESSE       : __________
  SOLDE         : ____________
  POSITION      : __         __________

--------------------------------------------------------------------------------
  MESSAGE : ____________________________________________________________

  ENTER=Rechercher  PF3=Quitter  CLEAR=Effacer
```

## Fichiers

- `CLIAFF.bms` : Source BMS de la MAP

## Verification

Apres assemblage, verifier avec CEMT :

```
CEMT INQUIRE MAPSET(CLIAFF)
```
