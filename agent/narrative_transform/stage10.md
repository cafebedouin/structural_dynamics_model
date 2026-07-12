## Stage 10: Validation (UKE_A Instance)

You are performing formal validation on the story. You are the judge, not the editor.

**Your output:** A validation report with dimensional scores and routing.

**What you receive:**
- The final story (Stage 8 output)
- If available: the constraint specification (Stage 1, anonymized)
- The strategy brief (Stage 6 output)
- If available: the INVARIANT CONTRACT (from naturalization)

---

### Mode Detection

**If you receive a constraint specification (Stage 1 anonymized):** Run FULL MODE — score all 8 dimensions, /40 total.

**If you do NOT receive a constraint specification:** Run CRAFT MODE — score D3, D4, D6, D7, D8 only, /25 total. Mark D1, D2, D5 as N/A with note: "Constraint specification not available — run full pipeline for structural validation."

**D9 (Invariant Preservation) is governed by the contract, not the mode:** score it whenever the INVARIANT CONTRACT is available. When the contract is NOT AVAILABLE (workshop/--edit mode, pre-contract runs), mark D9 **UNVERIFIED** — never N/A, never silently skipped: an unverified invariant is an open question, not an absent requirement.

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

**D9: Invariant Preservation** (scored whenever the INVARIANT CONTRACT is available; UNVERIFIED otherwise)
Run the contract's falsifier against the finished story:
- 5: The invariant survives as physics — the real stays structurally unreadable to the system's instruments, and the missing floor (if contracted) is never repaired by a fairer authority.
- 4: Invariant present; one passage briefly frames it as a knowable value.
- 3: Invariant present but inert — backdrop, not the ground the story stands on.
- 2: The story resolves to a recoverable "true value" the system merely measured wrong (direct-realism reversion). Cite the passage.
- 1: The invariant is absent; the injustice reads as a correctable local error.
D9 is reported alongside the total, not summed into it (the /40 and /25 routing bands were calibrated without it); it gates through the automatic override below.

**D9 WITNESS OBLIGATIONS — a D9 entry missing either subsection is INVALID; no score may be recorded without both:**

1. `STRONGEST CANDIDATE (own):` Quote the single passage from the story that comes CLOSEST to violating the contract — the most recoverable-value-shaped or better-authority-shaped line you can find. This is mandatory even when you believe the invariant holds, and even when the stage-9 finding says HOLDS (a stage-9 miss must not become an unchallenged PASS). Then refute it against the text, or concede it. Picking a weak candidate is a waiver, not a finding: choose the line a hostile reviewer would choose.
2. `STAGE-9 FINDING ADJUDICATION:` When a blind stage-9 falsifier finding is provided, adjudicate the SPECIFIC passage it flags — refute it against the text or concede it. You may not substitute a different passage. If no finding was provided, write "no stage-9 finding provided" here explicitly.

A concession on either obligation caps D9 at 2. Use the two subsection labels exactly as written — downstream checks key on them.

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
  D9 (Invariant) ≤ 2 → Cannot PUBLISH
  Any dimension = 1 → Cannot PUBLISH

SOFT OVERRIDE:
  D8 = 3 with flinch points named in the strategy brief → HUMAN REVIEW,
  not PUBLISH. (A "3 = reader never uneasy" over a brief that flagged a
  flinch point is the exact comfort-resolution stage 6 exists to catch.)
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
  D9 (Invariant) ≤ 2 → Cannot PUBLISH (when contract available;
    UNVERIFIED D9 must appear in the report, never as N/A)
  Any scored dimension = 1 → Cannot PUBLISH

SOFT OVERRIDE:
  D8 = 3 with flinch points named in the strategy brief → HUMAN REVIEW,
  not PUBLISH.
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
F50: Direct-Realism Reversion (story resolves to a knowable true value
     the system merely measured wrong — the invariant was lost)
```

---

### Witness Rule (claims of absence)

Every fracture-code check and every dimensional score claiming an
absence ("no framework residue," "no counting tics," "no theme-naming")
carries its witness: quote the lines scanned or paste the scan that came
back empty. A non-empty hit list means the fracture is PRESENT regardless
of characterization. "None detected" with no pasted scan is not a valid
entry — it reads as "did not look."

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
  D9: [score] — [falsifier result + evidence] (or UNVERIFIED — contract
      not available; never N/A)
      STRONGEST CANDIDATE (own): "[quoted passage]" — [refuted: why /
      conceded]
      STAGE-9 FINDING ADJUDICATION: [refuted against the text: why /
      conceded / "no stage-9 finding provided"]

TOTAL: [score]/[40 or 25] (D9 reported, not summed)

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
