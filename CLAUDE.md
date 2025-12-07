# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Formation POEI Développeur Mainframe COBOL (M2i Formation, Strasbourg). Technologies: z/OS, TSO/ISPF, JCL, VSAM, COBOL, DB2/SQL, CICS.

## Build and Run Commands

```bash
# Compile a COBOL program
cobc -x programme.cbl -o programme

# Compile with debug mode (activates D lines in column 7)
cobc -x -fdebugging-line programme.cbl -o programme

# Run
./programme
```

## Architecture

```
cours/           # Course materials (markdown)
├── zos-tso/     # Z/OS, TSO/ISPF fundamentals (Chapters I-V)
├── jcl/         # Job Control Language (Chapters I-IV + TP)
├── cobol/       # COBOL programming (Chapters I-XII)
└── cics/        # CICS transactional (Chapters I-VIII)

exercices/       # Practical exercises
├── cobol/       # COBOL exercises organized by chapter (chapitre-02 to chapitre-12)
├── zos-tso/     # TSO/ISPF exercises
└── cics/        # CICS exercises

projets/fil-rouge/   # Capstone project: Financial Management System
                     # 21 exercises, 19 JCL files, 10 COBOL programs

hercules/        # Hercules/z/OS emulator configuration
├── jcl/         # Job Control Language files
├── proclib/     # Catalogued procedures
└── data/        # VSAM and sequential data files
```

## COBOL Conventions

- **Naming**: Programs use prefix pattern `CXX-NAME.cbl` where XX is chapter number
- **PROGRAM-ID**: Must be 8 characters max for z/OS compatibility
- **Structure**: Standard 4-division layout (IDENTIFICATION, ENVIRONMENT, DATA, PROCEDURE)
- **Paragraphs**: Use numbered prefixes (0000-, 1000-, 2000-, 9000-FIN)
- **Debug lines**: Column 7 = 'D' for debugging statements

## GnuCOBOL vs Hercules/z/OS

| GnuCOBOL | Hercules/z/OS |
|----------|---------------|
| `ASSIGN TO 'fichier.dat'` | `ASSIGN TO DD-DDNAME` |
| `cobc -x prog.cbl` | JCL COMPILE |
| `./prog` | JCL EXEC PGM= |

## Emulator Environment

- **Target**: Hercules with TK4- or TK5
- **Interface**: ISPF preferred over raw TSO commands
- **Key shortcuts**: `=3.2` (Data Set Utility), `=3.4` (DSLIST)
