# Exercice 6 : MAP pour ajout de client

## Objectif

Creer une MAP BMS permettant la saisie de tous les champs pour ajouter un nouveau client.

## Fichiers

| Fichier | Description |
|---------|-------------|
| `CLIAJT.bms` | Source BMS de la MAP d'ajout |

## Differences avec CLIAFF (affichage)

| Aspect | CLIAFF | CLIAJT |
|--------|--------|--------|
| Champs donnees | ASKIP (affichage) | UNPROT (saisie) |
| Libelles calcules | Oui (region, sexe...) | Non |
| Aide contextuelle | Non | Oui (formats attendus) |

## Assemblage

```jcl
//ASSEM    EXEC DFHMAPS,INDEX='DFH510.CICS',
//          MAPLIB='ROCHA.CICS.LOAD',
//          DSCTLIB='ROCHA.CICS.LINK',
//          MAPNAME='CLIAJT',RMODE=24
//SYSUT1   DD DSN=ROCHA.CICS.SOURCE(CLIAJT),DISP=SHR
```

## Definition CICS

```
CEDA DEFINE MAPSET(CLIAJT) GROUP(CLIGROUP)
CEDA INSTALL MAPSET(CLIAJT) GROUP(CLIGROUP)
```
