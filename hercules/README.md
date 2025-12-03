# Hercules - Environnement Mainframe

Ce dossier contient les éléments spécifiques à l'émulateur Hercules (z/OS).

## Structure

```
hercules/
├── jcl/        # JCL pour compilation et exécution
├── proclib/    # Procédures cataloguées
└── data/       # Fichiers de données (VSAM, séquentiels)
```

## JCL disponibles

| JCL | Description |
|-----|-------------|
| `COMPILE.jcl` | Compilation d'un programme COBOL |
| `RUNPGM.jcl` | Exécution d'un programme compilé |
| `COMPRUN.jcl` | Compilation + exécution en une étape |
| `DEFVSAM.jcl` | Définition de fichiers VSAM (IDCAMS) |

## Utilisation

### 1. Compiler un programme

```jcl
//COMPILE  EXEC PROC=COBCOMP,PGM=C08RRDS
```

### 2. Exécuter un programme

```jcl
//RUN      EXEC PGM=C08RRDS
//STEPLIB  DD DSN=USER.LOADLIB,DISP=SHR
//SYSOUT   DD SYSOUT=*
```

### 3. Créer un fichier VSAM

```jcl
//STEP01   EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  DELETE cluster.name PURGE
  DEFINE CLUSTER (NAME(cluster.name) ...)
/*
```

## Adaptation GnuCOBOL → Hercules

| GnuCOBOL | Hercules/z/OS |
|----------|---------------|
| `ASSIGN TO 'fichier.dat'` | `ASSIGN TO DD-DDNAME` |
| `cobc -x prog.cbl` | JCL COMPILE |
| `./prog` | JCL EXEC PGM= |

## Notes

- Les sources COBOL sont dans `../exercices/chapitre-XX/`
- Adapter les `ASSIGN TO` si nécessaire pour Hercules
- Les PROGRAM-ID doivent respecter la limite de 8 caractères sur z/OS
