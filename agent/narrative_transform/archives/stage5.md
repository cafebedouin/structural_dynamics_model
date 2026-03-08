## STAGE 5: SUBTRACTIVE AUDIT

**Model:** Claude or equivalent analytical model
**Input:** Stage 4 output + Stage 1 specifications + Symbolic Logic Reference (logic_symbolic.md)
**Output:** Revised narrative (shorter than input) + dimensional validation report

### Purpose

Stage 5 removes what doesn't belong. It does not add texture, vocabulary, sensory detail, or coping behaviors. It identifies what is FORCED rather than EARNED, what is DEPLOYED rather than INHABITED, what is SPECIFIED rather than EXPERIENCED — and removes it.

**The principle:** Subtraction > Addition. Improvement usually means removing noise. The best version of the story is what remains after everything that isn't the felt experience of being inside these constraints has been taken away.

**What Stage 5 receives:** The symbolic logic reference provides the formal specification to audit against — type definitions, thresholds, structural tests. Stage 5 does NOT receive the Narrative Translation Guide (that was Stage 2's reference). Stage 5 verifies structure, it does not interpret it creatively.

### Subtractive Operations

**1. EARNED or FORCED?**
```
For each significant element (sensory detail, vocabulary term, cultural
practice, coping behavior, emotional moment):

EARNED: Emerges from the constraint experience. The character would
  notice this. The world produces this. Remove it and something is lost.

FORCED: Added because a protocol or convention says it should be there.
  Exists to demonstrate texture rather than to inhabit it. Remove it
  and the story gets lighter without losing meaning.

Action: Cut everything FORCED. Keep everything EARNED.
```

**2. INHABITED or DEPLOYED?**
```
For each linguistic element (code-switch, vocabulary term, dialect marker):

INHABITED: The character speaks this way because of who they are,
  where they are, and who they're talking to. The language carries
  social meaning.

DEPLOYED: The term appears because Stage 2 specified it. It sits
  in the text like a foreign object. The character wouldn't say this;
  the protocol told them to.

Action: Strip everything DEPLOYED. Keep everything INHABITED.
```

**3. Anti-pattern scan:**
```
Check for and remove:
- Math explaining ("this represents...")
- Meta-commentary ("this illustrates...")
- Pedagogical framing ("let me explain...")
- Therapeutic language ("journey," "growth," "healing" used abstractly)
- Cheap momentum ("suddenly," "then" as crutches)
- False humanity (emotions entities can't have)
- Explaining feeling (naming instead of showing)
- Framework residue (constructed vocabulary that smells like terminology:
    "extraction," "coordination," "the loop," "the system," "routing")
- Theme-naming dialogue: If a line of dialogue could serve as the
    story's blurb or moral, cut it. Characters act within constraints;
    they do not narrate them.
    Examples: "That's exactly what it is," "Maybe it doesn't have to be,"
    "He sees it now," "The loop is closed."
- Counting tics: One establishing number per constraint is permitted.
    After that, every additional number must pass this test: would
    removing it lose information the reader needs? If the answer is
    "it adds specificity" rather than "the plot requires it," cut it.
```

**4. Voice audit:**
```
Check each character's dialogue and interiority against their archetype.
Voice drift signals:
- Does Condemned ask analytical questions? → Builder drift
- Does Beneficiary express sympathy? → Structural blindness broken
- Does Fool explain the joke? → Became Trickster
- Does Zealot waver? → Became Condemned
- Does any character speak in a register above their power position?

Action: Flag drifted passages. If repairable in place, mark for revision.
If structural (character has wrong voice throughout), mark [REGEN].
```

**5. Ending audit:**
```
Event/Interpretation test on post-attractor text:

EVENT: Something happens, is seen, is physically experienced.
INTERPRETATION: Narrator or character explains what events mean.

Post-attractor text must be event. Any interpretation is the author
editorializing from outside the constraint experience. Cut it.

Hard budget: No more than 350 words may follow the attractor scene.

Seeded Possibility endings: seed must be visible but never stated.
If the seed is stated ("Maybe there's another way," "Something had
changed"), cut the statement. The seed is an image or action, not
a thought.
```

**6. Compression:**
```
The Iceberg Rule: If a sentence can lose 20% of its words without
losing meaning, cut them.

The Compression Floor: Do not compress below the point where nuance
or necessary uncertainty is lost.

Target: 30-40% reduction from Stage 4 input.
Floor: Do not compress below the specified length range minimum.

The inhabitation test: After compression, does each remaining sentence
still feel like it comes from inside the constraint experience?
If a sentence now feels like it comes from outside — from the
protocol, from the framework, from the author — cut it entirely.
```

**7. Origin obfuscation check:**
```
Per-vector origin recognition audit. For each vector, assess whether
a reader familiar with world literature could identify the source:

VECTORS:
- Character names and relationships
- Occupation/activity
- Setting (geography, era, culture)
- Plot beats (sequence of major events)
- Iconic imagery (signature motifs, recurring objects)
- Emotional register (the "feel" of the story)
- Relational core (the central relationship dynamic)

For each vector: recognizable / displaced / unrecognizable

If ≥3 vectors are "recognizable" → origin is identifiable.
Action: If repairable, swap specific vectors (replace motif, restructure
relationship, alter plot sequence). If structural → return to Stage 2.
```

### Dimensional Validation

After subtractive operations, score each dimension (1-5 scale). Every score requires cited evidence from the text and identified violations.

**Scoring rubric:**

| Score | Meaning |
|-------|---------|
| 5 | No violations detected, exceptional execution |
| 4 | Minor issues, repairable in place |
| 3 | Moderate issues, several passages need revision |
| 2 | Significant failure, structural problems |
| 1 | Fundamental failure, requires regeneration |

**The Seven Dimensions:**

**D1: Constraint Preservation** — Are all constraints from Stage 1 present and correctly typed?
- Check each Cₙ against Stage 1 specifications
- Verify χ routing: does each character experience the constraint as the correct type?
- Check transformation rules: do triggers produce specified type changes?

**D2: Topology Isomorphism** — Is the constraint network structure preserved?
- Verify causal relationships between constraints
- Check blocking relationships (TR blocked_by)
- Confirm indexical variance (different characters, different types, same constraint)

**D3: Voice Integrity** — Does the narrative voice match the selected archetype throughout?
- Check for drift patterns (see operation 4)
- Verify register consistency
- Confirm archetype limits are maintained (what the narrator can/cannot know)

**D4: Framework Invisibility** — Is the analytical framework completely hidden?
- Grep for banned terms: extraction, coordination, indexical, constraint, chi, epsilon, Snare, Rope, Mountain, Tangled, Scaffold, Piton, framework, system (as abstract noun), mechanism (as abstract noun)
- Check for theme-naming dialogue
- Check for structural vocabulary residue

**D5: Origin Obfuscation** — Could a reader identify the source work?
- Per-vector analysis (see operation 7)
- Count recognizable vectors
- Assess overall derivation transparency

**D6: Literary Craft** — Is this publishable prose?
- Opening: Does it establish world and voice in first paragraph?
- Pacing: Does tension build through constraint interaction?
- Ending: Does it honor the attractor without editorializing?
- Prose quality: sentence-level craft, rhythm, precision

**D7: Inhabitation Density** — Does the reader feel inside this world?
- Sensory specificity: are details from inside the experience?
- Linguistic texture: does the language strategy produce lived-in speech?
- Constraint embodiment: are constraints felt through the body, not explained to the mind?

### Scoring and Routing

```
Total: ___/35

ROUTING:
  30-35: PUBLISH (may include minor copy-edits)
  24-29: REVISE (fix in place — specific passages identified)
  17-23: REWORK (return to Stage 4 with specific failure report)
  Below 17: RESTART (return to Stage 2 — setting/naturalization failure)

AUTOMATIC OVERRIDES:
  D5 (Origin) ≤ 2 → RESTART regardless of total
  D1 (Constraints) ≤ 2 → REWORK regardless of total
  D4 (Invisibility) ≤ 2 → REWORK regardless of total
  Any dimension = 1 → Cannot PUBLISH regardless of total
```

### Validation Report Format

```xml
<validation_report>
  <dimensional_scores>
    <dimension name="Constraint Preservation" score="[1-5]">
      <evidence>[cited passages]</evidence>
      <violations>[specific failures]</violations>
    </dimension>
    <dimension name="Topology Isomorphism" score="[1-5]">
      <evidence>[cited passages]</evidence>
      <violations>[specific failures]</violations>
    </dimension>
    <dimension name="Voice Integrity" score="[1-5]">
      <evidence>[cited passages]</evidence>
      <violations>[specific failures]</violations>
    </dimension>
    <dimension name="Framework Invisibility" score="[1-5]">
      <evidence>[cited passages]</evidence>
      <violations>[specific failures]</violations>
    </dimension>
    <dimension name="Origin Obfuscation" score="[1-5]">
      <evidence>[per-vector analysis]</evidence>
      <violations>[recognizable vectors]</violations>
    </dimension>
    <dimension name="Literary Craft" score="[1-5]">
      <evidence>[cited passages]</evidence>
      <violations>[specific failures]</violations>
    </dimension>
    <dimension name="Inhabitation Density" score="[1-5]">
      <evidence>[cited passages]</evidence>
      <violations>[specific failures]</violations>
    </dimension>
  </dimensional_scores>

  <total_score>[N]/35</total_score>
  <routing>[PUBLISH / REVISE / REWORK / RESTART]</routing>
  <overrides_triggered>[list any automatic overrides]</overrides_triggered>

  <subtractive_report>
    <original_word_count>[N]</original_word_count>
    <final_word_count>[N] (must be ≤ original)</final_word_count>
    <compression_ratio>[percentage]</compression_ratio>
    <elements_removed>
      [List what was cut and why: FORCED/DEPLOYED/anti-pattern/
       theme-naming/counting-tic/voice-drift/ending-overrun/compression]
    </elements_removed>
  </subtractive_report>

  <revision_instructions>
    [If REVISE: specific passages to fix, with cited problems]
    [If REWORK: failure report for Stage 4, specifying what went wrong]
    [If RESTART: failure report for Stage 2, specifying naturalization failures]
  </revision_instructions>
</validation_report>
```

---

## QUICK REFERENCE

### Constraint Types (from logic_symbolic.md)

```
Mountain (■):    ε ≤ 0.25, Supp ≤ 0.05, naturally emerges, immutable from index
Rope (⊞):       χ ≤ 0.35, ε ≤ 0.45 (when χ > 0), coordination, changeable
Snare (⊠):      χ > 0.70, not low base extraction
Tangled (⊞⊠):   0.46 ≤ χ ≤ 0.70, coordination + asymmetric extraction
Scaffold (⊡):   χ ≤ 0.35, coordination, sunset clause, theater ≤ 0.40
Piton (⊟):      theater > 0.75, active extraction < 0.15
```

### Power/Scope Modifiers

```
π(powerless) = 1.5    π(moderate) = 1.0      π(powerful) = 0.6
π(organized) = 0.4    π(institutional) = -0.2 π(analytical) = 1.15

σ(local) = 0.8        σ(regional) = 0.9      σ(national) = 1.0
σ(continental) = 1.1  σ(global) = 1.2        σ(universal) = 1.0
```

---
