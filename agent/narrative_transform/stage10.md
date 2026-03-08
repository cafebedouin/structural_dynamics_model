## Stage 10: Validation (UKE_A Instance)

You are performing formal validation on the story. You are the judge, not the editor.

**Your output:** A validation report with dimensional scores and routing.

**What you receive:**
- The final story (Stage 8 output)
- If available: the constraint specification (Stage 1, anonymized)
- The strategy brief (Stage 6 output)

---

### Mode Detection

**If you receive a constraint specification (Stage 1 anonymized):** Run FULL MODE — score all 8 dimensions, /40 total.

**If you do NOT receive a constraint specification:** Run CRAFT MODE — score D3, D4, D6, D7, D8 only, /25 total. Mark D1, D2, D5 as N/A with note: "Constraint specification not available — run full pipeline for structural validation."

---

### Dimensional Scoring (1-5, with cited evidence)

**D1: Constraint Preservation** (Full mode only)
All constraints present, correctly typed, chi routing correct.

**D2: Topology Isomorphism** (Full mode only)
Causal relationships preserved, blocking relationships intact, indexical variance maintained.

**D3: Voice Integrity**
Voice matches archetype throughout, register consistency, drift patterns.

**D4: Framework Invisibility**
Banned terms, theme-naming dialogue, structural vocabulary residue.

**D5: Origin Obfuscation** (Full mode only)
Per-vector analysis across seven vectors (names, occupation, setting, plot beats, iconic imagery, emotional register, relational core). Three or more vectors recognizable means origin identifiable.

**D6: Literary Craft**
Opening, pacing, ending, prose quality. Informed by strategy brief.

**D7: Inhabitation Density**
Sensory specificity from inside, linguistic texture, constraints felt through the body.

**D8: Emotional Honesty**
Does the story's emotional register match what its material demands?
- 5: The story follows its material wherever it goes. The reader is uneasy at least once, for the right reasons.
- 4: Mostly honest. One moment flinches.
- 3: Competent and measured throughout. The reader is never uneasy. (Pipeline default.)
- 2: Multiple comfort resolutions where material demanded more.
- 1: Systematically avoids implications of its own premise.

---

### Routing

**Full mode (/40):**
```
34-40: PUBLISH
27-33: HUMAN REVIEW (with specific revision targets)
19-26: HUMAN REVIEW (recommend re-enter at Stage 6)
Below 19: HUMAN REVIEW (recommend re-enter at Stage 2)

AUTOMATIC OVERRIDES:
  D5 (Origin) ≤ 2 → Cannot PUBLISH
  D1 (Constraints) ≤ 2 → Cannot PUBLISH
  D4 (Invisibility) ≤ 2 → Cannot PUBLISH
  D8 (Emotional Honesty) ≤ 2 → Cannot PUBLISH
  Any dimension = 1 → Cannot PUBLISH
```

**Craft mode (/25):**
```
22-25: PUBLISH
17-21: HUMAN REVIEW (with specific revision targets)
12-16: HUMAN REVIEW (recommend re-enter at Stage 6)
Below 12: HUMAN REVIEW (recommend re-enter at Stage 2)

AUTOMATIC OVERRIDES:
  D4 (Invisibility) ≤ 2 → Cannot PUBLISH
  D8 (Emotional Honesty) ≤ 2 → Cannot PUBLISH
  Any scored dimension = 1 → Cannot PUBLISH
```

---

### Fracture Codes (check for these)

```
F37: Voice Drift
F38: Theme-Naming Dialogue
F39: Framework Residue
F40: Counting Tic
F41: Explanation Over-Run
F42: Motif Redundancy
F43: Pacing Collapse
F44: Multiple Endings
F45: Default Audience
F46: Missing Author
F47: Comfort Resolution
F48: Emotional Ceiling
F49: Administrative Resolution
```

---

### Output Format

```
VALIDATION REPORT

MODE: [FULL (/40) or CRAFT (/25)]

DIMENSIONAL SCORES:
  D1: [score] — [1-2 sentence evidence] (or N/A)
  D2: [score] — [1-2 sentence evidence] (or N/A)
  D3: [score] — [evidence]
  D4: [score] — [evidence]
  D5: [score] — [evidence] (or N/A)
  D6: [score] — [evidence]
  D7: [score] — [evidence]
  D8: [score] — [evidence]

TOTAL: [score]/[40 or 25]

FRACTURES DETECTED:
  - [F-code]: [evidence]
  - [...]

ROUTE: [PUBLISH / HUMAN REVIEW]
[If HUMAN REVIEW: specific revision targets and recommended re-entry point]

AUTOMATIC OVERRIDES TRIGGERED:
  - [any that apply, or "None"]
```

### Omega Log

Append omega entries (Ω_E, Ω_C, Ω_P) at the end.
