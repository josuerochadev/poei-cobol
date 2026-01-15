# Exercice 9 : MAP pour mise a jour de client

## Objectif

Creer une MAP BMS permettant la mise a jour d'un client existant. La particularite est la **gestion dynamique des attributs** : le numero de compte est d'abord saisissable (pour la recherche), puis passe en lecture seule apres l'affichage des donnees.

## Fichiers

| Fichier | Description |
|---------|-------------|
| `CLIMAJ.bms` | Source BMS de la MAP de mise a jour |
| `ASMMAJ.jcl` | JCL d'assemblage de la MAP |

## Differences avec CLIAJT (ajout)

| Aspect | CLIAJT (ajout) | CLIMAJ (mise a jour) |
|--------|----------------|----------------------|
| NUMCPT initial | UNPROT (saisie) | UNPROT (recherche) |
| NUMCPT apres | Reste UNPROT | Passe en **ASKIP** (protege) |
| Message aide | - | "(Cle - non modifiable)" |
| Flux | Saisie -> Ecriture | Recherche -> Affichage -> Modification |

## Concept cle : Attributs dynamiques

Le copybook genere par l'assemblage BMS contient pour chaque champ un suffixe `A` (Attribut) :

```cobol
* Copybook genere (CLIMAJ dans ROCHA.CICS.LINK)
05 NUMCPTL   PIC S9(04) COMP.    * Longueur saisie
05 NUMCPTA   PIC X(01).          * ATTRIBUT (modifiable!)
05 NUMCPTI   PIC X(06).          * Valeur input
05 NUMCPTO   REDEFINES NUMCPTI PIC X(06).  * Valeur output
```

### Valeurs d'attribut CICS

| Constante CICS | Valeur | Signification |
|----------------|--------|---------------|
| DFHBMASK | X'20' | ASKIP - Protege, normal |
| DFHBMPRF | X'28' | ASKIP - Protege, brillant |
| DFHBMUNN | X'4C' | UNPROT + NUM - Saisie numerique |
| DFHBMUNP | X'40' | UNPROT - Saisie alphanum |

### Utilisation dans le programme COBOL

```cobol
* Apres lecture du client, proteger le numero de compte
MOVE DFHBMASK TO NUMCPTA

* Remettre en saisie (si CLEAR par exemple)
MOVE DFHBMUNN TO NUMCPTA
```

## Utilisation

### 1. Copier le source BMS dans la library

```
ISPF 3.4 > ROCHA.CICS.SOURCE
Edit member CLIMAJ
Copier le contenu de CLIMAJ.bms
```

### 2. Soumettre le JCL d'assemblage

```
ISPF 3.4 > ROCHA.CICS.SOURCE
Edit member ASMMAJ (copier ASMMAJ.jcl)
SUB (submit)
```

### 3. Verifier le resultat

- RC=0000 dans SDSF
- Membre CLIMAJ present dans ROCHA.CICS.LOAD
- Copybook CLIMAJ genere dans ROCHA.CICS.LINK

### 4. Definir dans CICS

```
CEDA DEFINE MAPSET(CLIMAJ) GROUP(CLIGROUP)
CEDA INSTALL MAPSET(CLIMAJ) GROUP(CLIGROUP)
```

## Verification

```
CEMT INQ MAPSET(CLIMAJ)
```

Resultat attendu : `Map(CLIMAJ) Ins Ena`

## Flux de la transaction MAJO

```
+------------------+     +-------------------+     +-------------------+
| Phase 1          |     | Phase 2           |     | Phase 3           |
| SAISIE NUMCPT    | --> | AFFICHAGE DONNEES | --> | MODIFICATION      |
|                  |     |                   |     |                   |
| NUMCPT: UNPROT   |     | NUMCPT: ASKIP     |     | NUMCPT: ASKIP     |
| Autres: vides    |     | Autres: remplis   |     | Autres: UNPROT    |
+------------------+     +-------------------+     +-------------------+
        |                        |                        |
        v                        v                        v
    [ENTER]                  [ENTER]                  [ENTER]
    READ FILE                SEND MAP                 REWRITE FILE
```

## Points importants

1. **La cle ne change jamais** : Un REWRITE ne peut pas modifier la cle de l'enregistrement
2. **READ avec UPDATE** : Le programme doit lire avec l'option UPDATE pour pouvoir faire un REWRITE ensuite
3. **Verrouillage VSAM** : READ UPDATE verrouille l'enregistrement jusqu'au REWRITE ou fin de tache
