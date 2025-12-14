# QCM 01 - Presentation Generale CICS

## Questions de la formatrice (Chapitre I)

### Question 1
Quelle est la signification de CICS ?

<details>
<summary>Reponse</summary>

**Customer Information Control System**

CICS est un moniteur transactionnel (Transaction Processing Monitor) developpe par IBM pour les environnements mainframe.
</details>

---

### Question 2
CICS permet-il de suivre l'execution des travaux quotidienne et periodique de l'entreprise ?

<details>
<summary>Reponse</summary>

**Non.**

CICS est un moniteur **transactionnel** (OLTP - Online Transaction Processing), pas un ordonnanceur de travaux batch. Pour le suivi des travaux quotidiens et periodiques, on utilise des ordonnanceurs comme TWS (Tivoli Workload Scheduler) ou CA7.

CICS gere les transactions interactives en temps reel declenchees par les utilisateurs.
</details>

---

### Question 3
Citer trois differences entre un travail BATCH et un travail Transactionnel.

<details>
<summary>Reponse</summary>

| Critere | BATCH | Transactionnel (CICS) |
|---------|-------|----------------------|
| **Declenchement** | Planifie (nuit, weekend) | A la demande de l'utilisateur |
| **Interaction** | Aucune (execution autonome) | Dialogue en temps reel |
| **Volume** | Traitement de masse | Transactions unitaires |
| **Duree** | Longue (heures) | Tres courte (secondes) |
| **Ressources** | Monopolise les ressources | Partage des ressources |
| **Reprise** | Restart depuis checkpoint | Pas de reprise (rejouer) |
</details>

---

### Question 4
Quelles sont les significations des termes suivants ?

#### a) Transaction
<details>
<summary>Reponse</summary>

Une **transaction** est une unite de travail logique et atomique declenchee par un utilisateur via un code de 4 caracteres (ex: CRED, INQ1). Elle respecte les proprietes ACID (Atomicite, Coherence, Isolation, Durabilite).
</details>

#### b) Tache
<details>
<summary>Reponse</summary>

Une **tache** (Task) est l'instance d'execution d'une transaction. Plusieurs taches peuvent executer la meme transaction simultanement pour differents utilisateurs.
</details>

#### c) Moniteur
<details>
<summary>Reponse</summary>

Un **moniteur transactionnel** (TP Monitor) est un logiciel qui gere l'execution des transactions en ligne. CICS assure la gestion des ressources, la securite, le multi-utilisateur et l'integrite des donnees.
</details>

#### d) Terminal 3270
<details>
<summary>Reponse</summary>

Le **terminal 3270** est le terminal standard IBM pour les mainframes. C'est un terminal "block mode" qui envoie les donnees en bloc (ecran complet) plutot que caractere par caractere. Il utilise des touches AID (PF1-24, ENTER, CLEAR).
</details>

#### e) LUW (Logical Unit of Work)
<details>
<summary>Reponse</summary>

Une **LUW** (Logical Unit of Work) est une unite logique de travail qui regroupe un ensemble d'operations devant etre validees ou annulees ensemble. Elle se termine par un SYNCPOINT (commit) ou un SYNCPOINT ROLLBACK.
</details>

#### f) Application
<details>
<summary>Reponse</summary>

Une **application CICS** est un ensemble de transactions liees fonctionnellement qui forment un systeme complet (ex: application bancaire, gestion de stock). Elle comprend des programmes, des ecrans (maps), des fichiers et des definitions de ressources.
</details>

---

## Questions supplementaires

### Question 5
Quel est le role principal de CICS ?

- [ ] a) Compiler des programmes COBOL
- [ ] b) Gerer des fichiers VSAM
- [ ] c) Executer des transactions en ligne de maniere fiable
- [ ] d) Ordonnancer des travaux batch

<details>
<summary>Reponse</summary>

**c) Executer des transactions en ligne de maniere fiable**

CICS est un moniteur transactionnel OLTP (Online Transaction Processing) qui permet d'executer des milliers de transactions simultanees de maniere fiable.
</details>

---

### Question 6
Que signifie OLTP ?

- [ ] a) Online Transaction Processing
- [ ] b) Offline Transaction Protocol
- [ ] c) Online Transfer Program
- [ ] d) Operational Logging Transaction Process

<details>
<summary>Reponse</summary>

**a) Online Transaction Processing**

OLTP designe le traitement transactionnel en ligne, caracterise par des transactions courtes, nombreuses et en temps reel.
</details>

---

### Question 7
Combien de caracteres possede un code transaction CICS ?

- [ ] a) 2 caracteres
- [ ] b) 4 caracteres
- [ ] c) 8 caracteres
- [ ] d) Variable

<details>
<summary>Reponse</summary>

**b) 4 caracteres**

Un code transaction CICS est compose de 1 a 4 caracteres alphanumeriques (ex: CEDA, CEDF, CEMT, CRED).
</details>

---

### Question 8
Quelles sont les proprietes ACID d'une transaction ?

<details>
<summary>Reponse</summary>

- **A**tomicite : La transaction est executee entierement ou pas du tout
- **C**oherence : La base de donnees reste dans un etat coherent avant et apres
- **I**solation : Les transactions concurrentes n'interferent pas entre elles
- **D**urabilite : Une fois validee, la transaction persiste meme en cas de panne
</details>

---

### Question 9
Quelle commande permet de valider definitivement les modifications d'une transaction ?

- [ ] a) COMMIT
- [ ] b) SYNCPOINT
- [ ] c) VALIDATE
- [ ] d) SAVE

<details>
<summary>Reponse</summary>

**b) SYNCPOINT**

En CICS, la commande `EXEC CICS SYNCPOINT` valide les modifications. `EXEC CICS SYNCPOINT ROLLBACK` les annule.
</details>

---

### Question 10
Dans quel mode fonctionne generalement une application CICS interactive ?

- [ ] a) Mode conversationnel
- [ ] b) Mode pseudo-conversationnel
- [ ] c) Mode batch
- [ ] d) Mode asynchrone

<details>
<summary>Reponse</summary>

**b) Mode pseudo-conversationnel**

Le mode pseudo-conversationnel libere les ressources entre chaque interaction utilisateur, permettant de supporter de nombreux utilisateurs simultanement. Le programme se termine apres chaque affichage et reprend a la reception de la reponse.
</details>

---

## Resume

| Concept | Definition |
|---------|------------|
| CICS | Customer Information Control System - Moniteur transactionnel IBM |
| Transaction | Unite de travail atomique (code de 4 car.) |
| Tache | Instance d'execution d'une transaction |
| LUW | Logical Unit of Work - Unite logique de travail |
| ACID | Atomicite, Coherence, Isolation, Durabilite |
| SYNCPOINT | Validation des modifications |
| Pseudo-conversationnel | Mode liberant les ressources entre interactions |

---
*Formation CICS - M2i Formation*
