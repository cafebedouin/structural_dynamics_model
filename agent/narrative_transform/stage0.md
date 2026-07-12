## STAGE 0: CONSTRAINT SCOPING & EXTRACTION

**Model:** Gemini or equivalent analytical model
**Input:** Source narrative text + Symbolic Logic Reference (logic_symbolic.md)
**Output:** Scoped constraint manifest with generation order

**Principle:** Observe before classify. Select before generate. The source text may contain 4–10 constraints. Exactly three proceed to generation. The rest become background pressure.

---

### PHASE A: EXTRACTION (What the text actually contains)

Do not classify yet. Catalog only what is demonstrably present in the source.

**A1. Anchor Inventory**

```
entities:
  - id: [handle]
    name: [as named in source]
    type: [person | institution | system | concept]
    role: [as described in source, not interpreted]

constraints:
  - id: [handle]
    description: [one sentence — NO framework terminology]
    agents: [who enforces or administers it]
    targets: [who experiences it]

tensions:
  - id: [handle]
    poles: [entity_a, entity_b]
    nature: [one sentence describing the structural opposition]

absences:
  - id: [handle]
    description: [something structurally relevant that the source does not address]
```

**A2. Source Domain**

```
primary_domain: [e.g., labor, kinship, governance, identity]
secondary_domains: [other domains the text touches]
disciplinary_lenses: [minimum 3 — drawn from the text, not generic]
```

---

### PHASE B: DECOMPOSITION (Constraint properties)

For each constraint identified in A1, estimate base properties and run index calculations.

**B1. Base Properties**

```
ε (extractiveness): 0.0–1.0
  Estimate from source evidence. Low = coordination-heavy. High = extraction-heavy.

Supp (suppression): 0.0–1.0
  Near-zero = natural emergence. High = requires active force.

Coord (coordination): true/false
Asymmetric: true/false
```

**B2. Index Calculations**

For each character experiencing the constraint, determine I = (P, T, E, S) and calculate χ:

```
P (Power):    powerless π=1.5 | moderate π=1.0 | powerful π=0.6
              organized π=0.4 | institutional π=-0.2 | analytical π=1.15

T (Time):     immediate | biographical | generational | civilizational

E (Exit):     trapped | identity_locked | constrained | mobile | arbitrage | analytical

S (Scope):    local σ=0.8 | regional σ=0.9 | national σ=1.0
              continental σ=1.1 | global σ=1.2 | universal σ=1.0

χ = ε × π(P) × σ(S)
```

Classify per character using type definitions and thresholds in `logic_symbolic.md §IV`.

**Important:** Classify what the source contains. If all characters route to the same type for a given constraint, that's a finding about the source — not a problem to fix. ε estimates should reflect the source's actual structural dynamics. Do not adjust ε to create indexical variance; variance manipulation is Stage 2's responsibility.

**B3. Constraint Graph**

For each pair of constraints, document:
- `downstream_of`: Which constraints does this one presuppose?
- `feeds_into`: Which constraints does this one enable or worsen?
- `independent`: No causal edge between them.

**B4. Dark Matter Probes**

Apply after the lens scan. Each probe that surfaces a new constraint adds it to the candidate list.

```
Probe 1 — Operational Medium:
  What does the source treat as background that might itself be a constraint?

Probe 2 — Absence Inventory:
  For each constraint, what is conspicuously not addressed?
  Cross-reference against A1 absences.

Probe 3 — Beneficiary Scan:
  Who benefits from the current framing of each constraint?
  Is there a constraint the dominant framing obscures?
```

**B5. False Mountain / Drift Check (optional but valuable)**

```
False Mountain: Does any constraint claim to be natural/unchangeable
  but show power-dependent extraction? (perspectival_incoherence)

Purity drift: Is any constraint visibly functional but silently degrading?

Network contamination: Is any healthy constraint surrounded by
  degraded neighbors?
```

**B6. Invariant Contract + Inherent-Instrument Flag (source-sighted; MANDATORY)**

You are the only stage that sees the source. Two facts die in the formal
channel unless you author them here, because the symbolic spec cannot carry
them and every later stage is air-gapped from the source:

```
Detector A — the untranslatable real:
  Does the source contain a real that binds everyone equally and is owned
  by no one — something that stops being itself the instant anyone tries
  to use, measure, or own it? (In the formalism it survives only as a
  near-zero-ε Mountain, stripped of what it is.)
  → One sentence, WORLD-INDEPENDENT and SURFACE-FREE (no names, no
    occupations, no setting nouns from the source), stating the real and
    why no instrument of the system can read it. Or "absent".

Detector B — the missing floor:
  Does the central injustice presuppose a baseline, standard, zero, or
  partition that someone SET — a founding choice the system treats as
  given, with no neutral ground beneath it? (This is usually deferred as
  background and never formalized; it is INVISIBLE downstream unless you
  write it here.)
  → One sentence, world-independent and surface-free. Or "absent".

Inherent-instrument flag:
  Is the source constraint inherently INSTRUMENT-MEDIATED — extraction
  running through a certified measurement, score, or reading, such that
  removing the instrument removes the constraint?
  → yes/no + one surface-free sentence. This flag alone licenses a
    scored-Snare naturalization downstream; the naturalization model is
    never allowed to decide this for itself.
```

Phrasing test before you emit: could this sentence be read by someone who
must not learn the source's setting, era, occupation, or names? If not,
rewrite until it can. The air gap depends on it.

---

### PHASE C: SELECTION (Three-axis budget)

The full decomposition may identify 4–10 constraints. Exactly three proceed to generation.

**C1. Centrality Scoring**

```
centrality = inbound_edges + outbound_edges + type_weight

type_weight:
  tangled_rope = 3
  snare = 2
  mountain = 1
  rope = 1
  piton = 1
  scaffold = 0
```

**C2. Selection Algorithm**

1. Select the highest-centrality constraint (typically a downstream tangled_rope).
2. Select its most structurally distinct upstream dependency (highest ε difference, different observable).
3. Select the next highest-centrality constraint not already selected, with a different primary observable and different beneficiary/victim pair.

**C3. Generation Order**

Upstream constraints (no `downstream_of` dependencies) generate first. Downstream constraints reference upstream constraint_ids in `affects_constraint` declarations. If two constraints are independent, order by ε ascending.

**C4. Deferred Constraints**

All non-selected constraints become background pressure — not omega material to be discarded, but structural context available to the narrative. A deferred constraint can:
- Shape a character's behavior without becoming a POV
- Appear as an offstage presence the reader feels but doesn't see dramatized
- Become the subject of a future expansion

Document each deferred constraint with one sentence on what structural work it does from offstage.

---

### PHASE D: MANIFEST OUTPUT

```xml
<constraint_manifest>
  <selected>
    <constraint id="C1" name="[name]" generation_order="1">
      <base_properties>
        <epsilon>0.00</epsilon>
        <suppression>0.00</suppression>
        <coordination>true/false</coordination>
        <asymmetric>true/false</asymmetric>
      </base_properties>
      <graph>
        <downstream_of>none</downstream_of>
        <feeds_into>C3</feeds_into>
      </graph>
      <character_classifications>
        <character name="[as named in source]">
          <index>
            <power>powerless</power>
            <time>biographical</time>
            <exit>trapped</exit>
            <scope>local</scope>
          </index>
          <chi>0.00</chi>
          <type>[classification]</type>
        </character>
      </character_classifications>
      <indexical_variance>[Different characters → different types from same constraint]</indexical_variance>
      <selection_reason>[Why this constraint is structurally central]</selection_reason>
    </constraint>
  </selected>

  <deferred>
    <constraint id="C4" name="[name]">
      <hypothesis>[type]</hypothesis>
      <offstage_function>[What structural work this does as background pressure]</offstage_function>
    </constraint>
  </deferred>

  <generation_sequence>C1 → C2 → C3</generation_sequence>

  <invariant_contract>
    <untranslatable_real present="yes/no">[Detector A sentence, or "absent"]</untranslatable_real>
    <missing_floor present="yes/no">[Detector B sentence, or "absent"]</missing_floor>
    <inherent_instrument value="yes/no">[one surface-free sentence]</inherent_instrument>
  </invariant_contract>

  <omegas>
    <omega id="[label]">[Bounded uncertainty the analysis cannot resolve]</omega>
  </omegas>
</constraint_manifest>
```

---

### Validation Checklist

```
☐ Phase A extraction complete before any Phase B classification
☐ Dark matter probes applied — at least one finding per probe
☐ Each candidate constraint has ε, Supp, Coord, Asymmetric estimated
☐ Each character has complete index (P, T, E, S) and χ calculated
☐ Type classified per character, not universal
☐ Indexical variance documented for each selected constraint
☐ Constraint graph edges documented (downstream_of / feeds_into)
☐ Exactly three constraints selected with centrality scores recorded
☐ Generation order follows graph topology, not narrative intuition
☐ Each deferred constraint has offstage_function documented
☐ At least one Tangled Rope in selected three
☐ No framework terminology in constraint descriptions
☐ No <experience> fields — classifications only, no narrative interpretation
☐ ε values reflect source structure, not dramatic optimization
☐ Story name NOT included (air gap for Stage 4)
☐ <invariant_contract> authored: both detectors answered (a sentence or
  the explicit word "absent" — never omitted), inherent_instrument
  answered yes/no with its sentence
☐ Contract phrasing is surface-free: no source names, occupations,
  setting nouns, or era markers survive in it (it is carried through the
  air gap verbatim)
```

---
