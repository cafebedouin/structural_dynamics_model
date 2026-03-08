## STAGE 0: CONSTRAINT SCOPING & EXTRACTION

### PHASE A: EXTRACTION (What the text actually contains)

**A1. Anchor Inventory**

```
entities:
  - id: walton
    name: Robert Walton
    type: person
    role: arctic explorer, narrator of letters
  - id: victor
    name: Victor Frankenstein
    type: person
    role: scientist, creator, primary narrator
  - id: creature
    name: The Creature
    type: person
    role: the created being, narrator
  - id: elizabeth
    name: Elizabeth Lavenza
    type: person
    role: adopted sister/cousin of Victor
  - id: clerval
    name: Henry Clerval
    type: person
    role: friend of Victor
  - id: justine
    name: Justine Moritz
    type: person
    role: servant in the Frankenstein household
  - id: alphonse
    name: Alphonse Frankenstein
    type: person
    role: father of Victor
  - id: delacey_family
    name: The De Lacey Family
    type: institution
    role: cottagers observed by the Creature
  - id: scientific_ambition
    name: Scientific Ambition
    type: concept
    role: a driving motivation for discovery and glory
  - id: social_prejudice
    name: Social Prejudice
    type: concept
    role: a societal force of judgment based on appearance and status

constraints:
  - id: c_ambition
    description: A personal drive for glory and discovery compels individuals to sacrifice their well-being, relationships, and safety.
    agents: [self, societal values]
    targets: [walton, victor]
  - id: c_creation
    description: A creator is bound by an inescapable duty to the being they bring into existence.
    agents: [victor, the act of creation]
    targets: [victor, creature]
  - id: c_appearance
    description: A being's physical form determines their social acceptance or rejection, overriding their inner nature or actions.
    agents: [society, delacey_family, random individuals]
    targets: [creature, justine]
  - id: c_reputation
    description: The need to maintain social standing and avoid shame prevents an individual from confessing to transgressive acts.
    agents: [victor]
    targets: [victor, justine, william, elizabeth]
  - id: c_revenge
    description: A cycle of inflicted harm and retaliation locks two parties into a mutually destructive pursuit.
    agents: [victor, creature]
    targets: [victor, creature, clerval, elizabeth]
  - id: c_gender
    description: An individual's role and agency are strictly defined by their sex, with men pursuing public knowledge and women providing domestic comfort.
    agents: [social norms]
    targets: [elizabeth, justine, safie, caroline, victor, clerval]

tensions:
  - id: t_knowledge_happiness
    poles: [scientific_ambition, familial duty]
    nature: The pursuit of forbidden knowledge is structurally opposed to the maintenance of domestic peace and happiness.
  - id: t_creator_creation
    poles: [victor, creature]
    nature: The creator and the created are locked in a struggle for recognition, duty, and survival, where one's freedom requires the other's subjugation.
  - id: t_appearance_reality
    poles: [creature's form, creature's initial nature]
    nature: An entity's benevolent inner self is in constant conflict with a monstrous exterior that dictates its reception by the world.

absences:
  - id: a_ethical_oversight
    description: There is no institutional or peer-based ethical framework to govern or provide oversight for scientific pursuits.
  - id: a_female_agency
    description: Female characters lack independent agency, their lives and fates being almost entirely determined by the actions and decisions of male characters.
  - id: a_restorative_justice
    description: The legal and social systems offer no mechanism for truth or reconciliation, only condemnation based on circumstantial evidence and prejudice.
```

**A2. Source Domain**

```
primary_domain: kinship
secondary_domains: [identity, governance, labor, ethics]
disciplinary_lenses: [gothic horror, romanticism, social criticism, philosophy of science]
```

---

### PHASE B: DECOMPOSITION (Constraint properties)

**B1. Base Properties**

| id | ε (extractiveness) | Supp (suppression) | Coord (coordination) | Asymmetric |
| :--- | :--- | :--- | :--- | :--- |
| c_ambition | 0.80 | 0.20 | false | true |
| c_creation | 0.90 | 0.80 | false | true |
| c_appearance | 0.85 | 0.90 | true | true |
| c_reputation | 0.90 | 0.95 | true | true |
| c_revenge | 1.00 | 0.10 | false | true |

**B2. Index Calculations**

(Calculations for selected constraints are in the final manifest. All candidates were analyzed.)

**B3. Constraint Graph**

- `c_ambition`: `downstream_of`: none; `feeds_into`: `c_creation`
- `c_creation`: `downstream_of`: `c_ambition`; `feeds_into`: `c_appearance`, `c_reputation`, `c_revenge`
- `c_appearance`: `downstream_of`: `c_creation`; `feeds_into`: `c_revenge`
- `c_reputation`: `downstream_of`: `c_creation`; `feeds_into`: `c_revenge`
- `c_revenge`: `downstream_of`: `c_creation`, `c_appearance`, `c_reputation`; `feeds_into`: none

**B4. Dark Matter Probes**

```
Probe 1 — Operational Medium:
  The vast distances and slow communication technologies (letters, sea travel) are treated as background. This creates information lag that prevents timely intervention and heightens the isolation of the characters, turning personal crises into irreversible tragedies.

Probe 2 — Absence Inventory:
  For c_creation, the conspicuous absence is any concept of a creator's responsibility *before* the act of creation. Victor's entire focus is on the technical challenge, with no thought to the ethical or social consequences, highlighting a void in his scientific education.

Probe 3 — Beneficiary Scan:
  Who benefits from c_appearance? The "normal" members of society benefit by maintaining a stable, comfortable in-group, achieved by violently expelling any being that challenges their definition of human. This reinforces the idea that social cohesion is maintained through exclusion.
```

---

### PHASE C: SELECTION (Three-axis budget)

**C1. Centrality Scoring**

| Constraint | Inbound Edges | Outbound Edges | Type Weight (Avg) | Centrality Score |
| :--- | :--- | :--- | :--- | :--- |
| c_creation | 1 | 3 | 3 (Tangled Rope) | 7 |
| c_revenge | 