## STAGE 0: CONSTRAINT EXTRACTION

**Role:** The Analyst
**Model:** Gemini (strong meta-commentary about process; self-diagnoses extraction gaps; treats violations as diagnostic objects rather than hiding them)
**Input:** Source material
**Output:** Structured constraint map with indexed classifications

### 0.1 Narrative Sources

For stories, novels, films, plays — use UKE_Narrative v1.2, Stage 0 directly. Extract constraints, estimate ε/Supp/Coord/Asymmetric, determine indices (P,T,E,S), calculate χ, classify types per character.

### 0.2 Non-Narrative Sources (Hyperstition Engine Method)

For policy documents, organizational structures, economic systems, cultural phenomena — the Hyperstition Engine's triadic lens provides an alternative:

```
THE ARCHITECT (Ontology):
  What are the hard constraints? Finite resources, structural walls, immovable terrain.
  → These map to Mountains and base properties (ε, Supp)

THE THEOLOGIAN (Rationalization):
  How does the system justify its own flaws?
  → These map to theater ratio, error manifestations, and the gap between
    claimed type and experienced type

THE WEAVER (Autonomous Extension):
  Where does this system go without intervention? What's the drift vector?
  → These map to transformation rules, attractor, and Omega variables
```

The triadic output must be translated into standard constraint notation (C[I], ε, χ, types) before Stage 1.

**Model note:** Different models will produce different triadic analyses of the same material. Gemini emphasizes caloric cost and automation (Technocrat tribe). Grok emphasizes entropy and aesthetic singularity (Humanist tribe). Claude and Copilot tend to resist or deconstruct (Critic tribe). Match the model to the source material's character.

### 0.3 Pre-Formalized Sources

If input is already a Prolog constraint story or equivalent formal specification, Stage 0 may be skipped. Proceed directly to Stage 1.

### Stage 0 Tension Prompt (optional)
*"What constraint in the source text is too weird to classify cleanly? What happens if you treat that weirdness as the system's seed crystal?"*

---

STAGE 0 OUTPUT FORMAT:
For each constraint identified:
  C[n]: [name]
    ε (estimated): [value] — Evidence: [what in the text supports this]
    Supp (estimated): [value] — Evidence: [enforcement visible in text]
    Coord: [true/false] — [why]
    Asymmetric: [true/false] — [why]
    
    Per character/index:
      [Character] — I=(P,T,E,S) → χ=[calc] → Type=[classification]
    
    Indexical variance: [which characters see different types]
    UCZ candidates: [anything that resists clean classification]

---
