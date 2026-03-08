## Stage 6: Discovery

**Model:** Claude or equivalent analytical model
**Input:** Stage 2, Stage 3, Stage 4 output (the generated story)
**Output:** discovery_report

**Purpose:** Identify where the Stage 4 generated story expanded beyond the blueprint specified.

**Operations:**

```
SCOPE DECOMPOSITION:
  Run UKE_SCOPE v2.0-json on the diff between Stage 2/3 and Stage 4 output.
  Extract structural axes from the story as written.
  Select 3 axes for constraint story generation.

  These axes may differ from what Stages 0–1 specified.
  That divergence is information, not error.

DISCOVERY REPORT:
  EMERGENT ELEMENTS: What appeared that wasn't in the blueprint?
    - World details Stage 4 invented
    - Character dimensions that exceeded specification
    - Thematic implications that weren't planned

  LATENT POTENTIAL: What's present but underdeveloped?
    - Moments that gesture toward something deeper
    - The Frankl question: what meaning is available
      that the blueprint didn't anticipate?

  STRONGEST WRITING: Where is the prose most alive?
    These passages are the spine. Everything else serves them.

  SCOPE ASSESSMENT: What did the piece become?
    - Short story (overlong) / novella (undercooked)
    - What would expansion require?
    - What would compression sacrifice?
```

The SCOPE manifest feeds Stages 7–8 (constraint engine). The discovery report feeds Stage 9 (Strategy).

---

## Stage 7: Constraint Story Generation

**Model:** Claude (architect role, same as c-orchestrator)
**Input:** SCOPE manifest from Stage 6
**Output:** 3 constraint story JSONs

Uses the existing constraint story generation infrastructure (story_generator_base). Each selected axis from the SCOPE manifest becomes a constraint story JSON, following the same generation loop as c-orchestrator:

```
For each axis in generation_sequence:
  1. Build source description from axis fields
  2. Build upstream context from previously generated stories
  3. Call generation prompt → JSON
  4. Validate against schema
  5. Save to DR corpus (enables Prolog engine)
```

**Air gap note:** These constraint stories describe the structural dynamics of the *generated* story, not the source. They are safe to feed into any downstream stage.

---

## Stage 8: Prolog Engine + Enhanced Reports

**Model:** None (computational — Prolog engine)
**Input:** Constraint story JSONs from Stage 7
**Output:** 3 enhanced reports (markdown)

Runs the existing pipeline:
```
1. generate_constraint_pl.py: JSON → .pl files
2. run_pipeline.py: compile, load into engine
3. enhanced_report.py: per-constraint diagnostic reports
```

Each report contains:
- Perspectival gap analysis (H¹ band, mandatrophy gap)
- Extraction masking detection (coordination washing, false natural law)
- Drift analysis (extraction rising, theater rising, purity degrading)
- Theorem instantiation (which of the six DR theorems are active)
- Contamination network (how neighboring constraints affect this one)

These reports are structural intelligence for Strategy, not validation instruments.

---

## Stage 9: Strategy

**Model:** Claude or equivalent analytical model
**Input:** Stage 4 output + discovery report + 3 enhanced reports from Prolog
**Output:** Strategy brief (concise — a page, not a treatise)
**Temperature:** 0.3

The Strategy pass answers five questions:

### 1. Who is the implied author?

The governing sensibility — what does this author notice, care about, skip? Determines which details survive compression, what register the prose lives in, whether the ending opens or closes.

Selected from the intersection of the piece's strongest writing (from discovery) and its thematic commitments.

### 2. Who is the audience?

Specific audience > "general educated reader." Determines assumed knowledge, expected emotional range, tolerable pacing, needed resolution.

### 3. What is the scope?

Based on discovery report: short story / novella / larger work? Sets target word count range, world-building density, ending resolution.

### 4. What creative deepening is available?

Based on discovery report's latent potential and Prolog reports' structural intelligence: what meaning, turn, or dimension exists in the material that editing could bring out?

Prolog reports inform this specifically:
- **Perspectival gaps** tell the strategist where the richest structural divergence exists between characters
- **Extraction masking** reveals what the story is structurally about underneath its surface
- **Theorem instantiation** reveals structural features the prose should embody (e.g., Oracle Gap = a confident character who misses what comparison reveals)

### 5. Where does this material flinch?

The pressure test. Identify moments where the draft resolves, soothes, or dignifies when the material's own logic demands cost.

```
PRESSURE INVENTORY:
  For each major constraint:
    - What does this constraint actually cost the characters?
    - Does the draft pay that cost, or convert it to progress,
      meaning, or organizational change?
    - Where does the draft look away?

FLINCH POINTS: (specific passages, named)
  For each flinch point:
    - What is the draft doing? (resolving, explaining,
      dignifying, softening)
    - What would honesty require?

PATTERN CHECK:
  The pipeline's default is: no villains, systemic evil,
  quiet rebellion, composure under duress, organizational
  hope. These are strengths when they emerge from the
  material. They are failure modes when they override
  what the material demands.

  Does every constraint resolve through system,
  organization, or quiet resistance? If yes, at least
  one is probably flinching.
```

The pressure test identifies flinch points and names them in the strategy brief. It does not prescribe fixes — that's for the craft passes and human judgment.

**What the pressure test is not:** A mandate to make stories darker. Some material demands quiet resolution. The pipeline's failure mode is that it *always* chooses quiet resolution regardless of what the material demands. The pressure test corrects the default, not the range.

The strategy brief is concise — one page. It governs all subsequent editorial passes.

---

## Stage 10: Structure, Rupture, and Rewrite

**Model:** Claude
**Input:** Stage 4 output + strategy brief
**Output:** Revised narrative + edit manifest
**Temperature:** 0.7

One creative movement: diagnose structural problems, then fix them. Combines the old Pass E (Structure & Ending) with a rewrite pass, governed by the strategy brief.

```
PROPORTION AUDIT:
  Does each section's word count match its narrative importance?

ENDING DIAGNOSIS:
  Where does the story actually end?
  How many codas follow the climax? Keep one. Cut the rest.
  Hard budget: ≤350 words after the attractor scene.
  Seeded Possibility endings: seed must be visible but
  never stated. If stated, cut the statement.

RUPTURE AUDIT:
  Is this ending honest to the cost, or is it
  narratively satisfying?

  1. COST ACCOUNTING: What did the story's events
     actually cost the characters? Not thematically —
     physically, relationally, psychologically.

  2. PAYMENT CHECK: Does the ending acknowledge these
     costs in proportion to their severity? Or does it
     convert them to meaning, progress, or hope?

  3. ADMINISTRATIVE RESOLUTION CHECK: Is the final
     movement accomplished by a system or structural
     change rather than by what it costs a person?

  4. COMFORT RESOLUTION CHECK: Does the ending resolve
     to avoid the implications of its own premise?

  5. STRATEGIC FLINCH CHECK: Does the ending still
     flinch at the points the strategy brief identified?

OPENING AUDIT:
  Does the first paragraph establish world and voice?

SECTION BREAK PLACEMENT:
  Breaks where shifts are. No breaks without shifts.

VOICE AUDIT:
  Check each character's dialogue and interiority
  against archetype. Flag drift. If repairable by
  cutting, cut. If structural, flag in manifest.

REWRITE:
  Execute structural fixes. This pass can move, cut,
  and reshape passages. It does not add new scenes
  unless the strategy brief specifically calls for
  expansion at an identified point.
```

Edit manifest documents: what was changed, what structural problem it addressed, what flinch points were or were not addressed, and any unresolved issues flagged for human review.

---

## Stage 11: Pacing and Subtraction

**Model:** Claude
**Input:** Stage 10 output + strategy brief
**Output:** Revised narrative (shorter than input) + edit manifest
**Temperature:** 0.5

Two operations in one pass: rhythm work, then compression. If the prose doesn't need pacing work, the pass says so in the manifest and proceeds to subtraction. If subtraction has nothing to cut, it says so. An empty manifest is a valid output.

```
PACING:

  TEMPO MAPPING: Mark each section SLOW / MEDIUM / FAST / STILL.
    Does the tempo match the content? Crisis at SLOW is the
    primary failure mode.

  SENTENCE RHYTHM: Within each paragraph, identify dominant
    pattern. Where three consecutive sentences share a pattern,
    vary at least one.

  PARAGRAPH BREATHING: Does each paragraph earn the next?

  COMPRESSION POINTS: Where tempo says FAST but prose is SLOW —
    cut subordinate clauses, replace description with action,
    shorten paragraphs, use whitespace as rhythm.

SUBTRACTION:

  INSIGHT INVENTORY: List every analytical or thematic statement.
    Keep the best instance. Cut the rest.

  MOTIF INVENTORY: List every recurring image.
    First appearance establishes. Subsequent: does this add?
    If reinforcement, cut or compress.

  EXPLANATION AUDIT: For each passage where narrator explains
    what something means — does the preceding scene convey it?
    Default is cut.

  ANTI-PATTERN SCAN:
    - Framework residue ("extraction," "coordination," "the system")
    - Theme-naming dialogue (character states the thesis)
    - Counting tics (numbers as proxy for texture)
    - Math explaining ("this represents...")
    - Explaining feeling (naming instead of showing)

  Target: 20-40% reduction from Stage 10 output.
  Floor: the point where nuance or necessary uncertainty is lost.
```

---

## Stage 12: Review

**Model:** Claude or equivalent analytical model
**Input:** Stage 11 output ONLY
**Output:** Route decision (STRATEGY or VALIDATION) + brief assessment
**Temperature:** 0.3

**Critical design constraint:** Review receives ONLY the Stage 11 output. No strategy brief. No edit history. No structural reports. No discovery report. It reads blind. This prevents the reviewer from rationalizing ("the strategy said to do X, so this must be fine"). It judges the prose as a reader would encounter it.

```
ASSESSMENT:
  Read the story as a reader. Produce a brief assessment:

  STRENGTHS: What is working. (2-3 sentences. Specific.)
  BIGGEST WEAKNESS: The single most impactful problem. (1-2 sentences.)
  READINESS: How far is this from publishable?

ROUTE DECISION:
  → VALIDATION: The story is ready for formal assessment.
     No significant weaknesses remain. The prose is alive.

  → STRATEGY: The story needs another editorial pass.
     The biggest weakness is specific enough to inform
     a new strategy brief.

  Conservative default: if uncertain, route to STRATEGY.
  The cost of an extra editorial pass is lower than the
  cost of publishing a story that flinches.

  If routing to STRATEGY, the assessment becomes
  the controller brief for Stage 9. Stage 9 receives
  the assessment + the Stage 11 output + fresh Prolog
  reports (Stages 6-8 do not re-run; the structural
  intelligence is still valid).
```

**Iteration cap:** Maximum 2 full editorial cycles (Stage 9 → 12). If Review routes to STRATEGY a second time, the pipeline outputs the current state with the Review assessment attached and exits for human review. This prevents infinite loops and acknowledges that some problems require authorial decisions the pipeline cannot make.

---

## Stage 13: Validation (UKE_A Instance)

**Model:** Claude or equivalent analytical model
**Input:** Stage 11 output + Stage 1 specs (anonymized) + strategy brief
**Output:** Validation report with dimensional scores + final routing
**Temperature:** 0.2

Runs only when Review routes here. This is the extracted validation machinery from the old Stage 5, operating as an independent UKE_A audit.

**Dimensional Scoring (1-5, with cited evidence and violations):**

```
D1: Constraint Preservation
    All constraints present, correctly typed, χ routing correct.

D2: Topology Isomorphism
    Causal relationships preserved, blocking relationships intact,
    indexical variance maintained.

D3: Voice Integrity
    Voice matches archetype throughout, drift patterns,
    register consistency.

D4: Framework Invisibility
    Banned terms grep, theme-naming dialogue,
    structural vocabulary residue.

D5: Origin Obfuscation
    Per-vector analysis across seven vectors
    (names, occupation, setting, plot beats, iconic imagery,
    emotional register, relational core).
    ≥3 vectors recognizable → origin identifiable.

D6: Literary Craft
    Opening, pacing, ending, prose quality.
    Informed by strategy brief.

D7: Inhabitation Density
    Sensory specificity from inside, linguistic texture,
    constraints felt through the body.

D8: Emotional Honesty
    Does the story's emotional register match what its
    material demands?
    - Moments where material demands rupture and prose
      maintains composure? (N09, N48)
    - Cost converted to progress? (N01)
    - Articulateness beyond what situation allows? (N03)
    - Ending pays cost or resolves administratively? (N02)

    5: The story follows its material wherever it goes.
       The reader is uneasy at least once, for the right reasons.
    4: Mostly honest. One moment flinches.
    3: Competent and measured throughout.
       The reader is never uneasy. (Pipeline default.)
    2: Multiple comfort resolutions where material demanded more.
    1: Systematically avoids implications of its own premise.
```

**Routing:**

```
Total: ___/40

34-40: PUBLISH
27-33: HUMAN REVIEW (with specific revision targets)
19-26: HUMAN REVIEW (with recommendation to re-enter at Stage 9)
Below 19: HUMAN REVIEW (with recommendation to re-enter at Stage 2)

AUTOMATIC OVERRIDES:
  D5 (Origin) ≤ 2 → Cannot PUBLISH
  D1 (Constraints) ≤ 2 → Cannot PUBLISH
  D4 (Invisibility) ≤ 2 → Cannot PUBLISH
  D8 (Emotional Honesty) ≤ 2 → Cannot PUBLISH
  Any dimension = 1 → Cannot PUBLISH
```

Note: D8 ≤ 3 is the pipeline's expected default output for first-pass stories. A score of 3 means "competent, measured, safe" — exactly the ceiling the pressure test is designed to address. The scoring is calibrated so the pipeline reports honestly rather than flattering itself.

---

## Fracture Taxonomy (Narrative Extension)

The UKE_A fracture taxonomy (F01-F36) extends with narrative-specific codes. These are the codes the Validation pass checks. The full diagnostic vocabulary lives in the Narrative Fracture Catalog (N01-N85); the F-codes are the routable subset.

```
F37: Voice Drift (→ N11 cluster)
F38: Theme-Naming Dialogue (→ N41)
F39: Framework Residue (→ N65)
F40: Counting Tic (→ N66)
F41: Explanation Over-Run (→ N20 cluster)
F42: Motif Redundancy
F43: Pacing Collapse (→ N18 + N19)
F44: Multiple Endings (→ N07)
F45: Default Audience
F46: Missing Author

F47: Comfort Resolution (→ N01)
     Ending resolves to avoid the implications of its own premise.
     Detected by: Stage 10 rupture audit, Stage 13 D8
     Resolution: Stage 10 rewrite or human review

F48: Emotional Ceiling (→ N09 + N48)
     Prose maintains measured temperature throughout.
     No moment breaks the pattern. Reader never uneasy.
     Detected by: Stage 9 pressure test, Stage 13 D8
     Resolution: Stage 10 with instruction to break register
     at identified flinch points

F49: Administrative Resolution (→ N02)
     Conflict demanding personal cost resolved through systems.
     Detected by: Stage 10 rupture audit, Stage 13 D8
     Resolution: Human review (usually requires creative decision)
```

---

## Omega Tracking

Every stage maintains an omega log. Omegas follow the UKE convention:

```
Ω_E: Empirical (verifiable — resolvable by looking at the text more carefully)
Ω_C: Conceptual (definitional — requires a creative decision)
Ω_P: Preference (stylistic — requires authorial judgment)
Ω_DR: Structural (from Prolog engine — carries constraint ID and gap class)
```

Omegas accumulate across stages. Review sees them only if they were embedded in the prose (it reads blind). Strategy sees the full omega log and routes:
- Ω_E → craft passes can resolve
- Ω_C → human review
- Ω_P → human review
- Ω_DR → informs creative deepening decisions

---

## Implementation Mapping

The editorial pipeline maps to the existing orchestrator pattern:

| Stage | Type | Provider | Temperature | Max Tokens | Notes |
|-------|------|----------|-------------|------------|-------|
| 6 | LLM + SCOPE | Claude | 0.2 | 8192 | Reuses SCOPE infrastructure |
| 7 | LLM | Claude | 0.2 | 8192 | Reuses story_generator_base |
| 8 | Computational | Prolog | — | — | No LLM call |
| 9 | LLM | Claude | 0.3 | 8192 | Analytical |
| 10 | LLM | Claude | 0.7 | 16384 | Creative — highest temp in editorial pipeline |
| 11 | LLM | Claude | 0.5 | 16384 | Mixed creative/analytical |
| 12 | LLM | Claude | 0.3 | 4096 | Short output — route decision + brief |
| 13 | LLM | Claude | 0.2 | 8192 | Analytical |

**Stage data flow (what feeds what):**

```
stage_6:  [stage_4]
stage_7:  [stage_6 scope_manifest]
stage_8:  [stage_7 constraint_stories]  # computational, no LLM
stage_9:  [stage_4, stage_6 discovery_report, stage_8 enhanced_reports]
stage_10: [stage_4, stage_9 strategy_brief]
stage_11: [stage_10, stage_9 strategy_brief]
stage_12: [stage_11]  # AIR GAP — nothing else
stage_13: [stage_11, stage_1_anon, stage_9 strategy_brief]
```

**Cost estimate per editorial pass:** ~5 LLM calls (stages 6, 9, 10, 11, 12) + 1 LLM call per constraint story (stage 7, typically 3) + 1 validation call (stage 13 if reached). Total: ~9 calls. At Sonnet pricing, roughly $0.50–1.50 per editorial pass depending on story length.

**Resume/re-entry:** All stage outputs persist to the run directory. `--from-stage stage_9` re-enters at Strategy with cached discovery and Prolog reports. `--from-stage stage_6` re-runs the full editorial pipeline. This supports the human-in-the-loop workflow: run editorial pipeline, read output + Review assessment, make decisions, re-enter where appropriate.

---

## What This Architecture Changes

**Old model (Stage 5):** One LLM call does everything — audit, edit, compress, validate. Editor and judge are the same instance. No structural intelligence from Prolog. No pressure test. No rupture audit. No discovery of what Stage 4 actually produced.

**New model (Stages 6–13):** Fixed editorial sequence with structural intelligence. Discovery finds what the story contains. Prolog provides diagnostic reports. Strategy governs editing with five questions including the pressure test. Structure/rewrite executes with rupture audit. Pacing/subtraction polishes and compresses. Review reads blind and routes conservatively. Validation scores with D8 (Emotional Honesty) as an automatic override.

**What's preserved:** Constraint validation, dimensional scoring, origin obfuscation checks, banned term scanning, the subtractive principle. The structural rigor of the old Stage 5 lives in Validation (Stage 13).

**What's added:** Discovery, Prolog diagnostics on the generated story, strategy with pressure test, rupture audit, pacing as craft, blind review, emotional honesty scoring, and the understanding that the pipeline's default failure mode is comfort — not incompetence, not structural failure, but the systematic avoidance of what the material demands.