# UKE_Narrative: Post-Generation Architecture (v0.3)

## Lineage

This architecture descends from MCK v1.6 through the UKE protocol suite. The validation pass is a UKE_A instance (independent verification with fracture taxonomy and omega routing). The craft passes adapt UKE_E's verification-then-logic-then-flow sequence for narrative prose. The Protocol Framing Guide's central insight — these are routing mechanisms, not truth measurements — is the architectural principle throughout.

v0.3 replaces v0.2's controller-based decision loop with a fixed editorial sequence. The loop asked an LLM to make routing decisions (skip to validation vs. run another pass) that systematically favor early exit. A fixed sequence guarantees every story gets the full treatment. The one routing decision that remains — Review routes to Strategy or Validation — is binary and conservative: any significant concern sends the story back.

## Overview

The generation pipeline (Stages 0–4) produces the story. A Python infrastructure step runs the constraint engine on the diff between the blueprint (Stages 2/3) and the generated story (Stage 4). The editorial pipeline (Stages 5–10) then executes: discover what the story contains using the Prolog reports, make strategic decisions, execute editorial passes, review blind, validate.

**Three separations:**
1. **Discovery and strategy** are separated from **craft** (decide before you cut)
2. **Craft** is separated from **validation** (the editor and the judge are never the same instance)
3. **Review** is separated from **everything upstream** (Review sees only the final prose — no strategy brief, no edit history, no structural reports. It reads blind.)

```
Generation Pipeline (existing)
  Stage 0: Constraint extraction (Gemini)
  Stage 1: Formalization (Claude)
  Stage 2: Naturalization (Claude)
  Stage 3: Editorial decisions (Claude)
  Stage 4: Story generation (Claude, air gap)

  [Python: Constraint engine]
    Input: Stages 2/3 (blueprint) + Stage 4 (story)
    Operations: Diff analysis → constraint story generation → Prolog → enhanced reports
    Output: Enhanced reports (markdown)
    Not a stage. No LLM narrative call. Infrastructure.

Editorial Pipeline (new, replaces old Stage 5)
  Stage 5:  Discovery (using Prolog enhanced reports)
  Stage 6:  Strategy
  Stage 7:  Structure, rupture, and rewrite
  Stage 8:  Pacing and subtraction
  Stage 9:  Review → routes to Stage 6 or Stage 10
  Stage 10: Validation
```

---

## Entry Points

**Full pipeline run:** Stages 0–4 execute, constraint engine runs, then Stages 5–10 follow.

**Editorial-only run:** Point at a saved run directory and enter at Stage 5.
```
python3 uke_narrative_orchestrator.py --resume outputs/run/ --from-stage stage_5
```
This loads cached Stage 4 output, runs the constraint engine if reports don't exist yet, then runs the editorial pipeline. Can be invoked repeatedly for multiple editorial passes with human review between iterations.

**Strategy re-entry:** After reading the Review assessment, re-enter at Stage 6 for a second editorial cycle.
```
python3 uke_narrative_orchestrator.py --resume outputs/run/ --from-stage stage_6
```
Constraint engine reports from the first cycle are still valid and reused.

---

## Constraint Engine (Python Infrastructure Step)

**Not a stage.** Runs between Stage 4 and Stage 5 as a Python infrastructure step. No LLM narrative call — uses the existing c-orchestrator pattern (SCOPE → constraint story generation → Prolog → enhanced reports).

### Purpose

Identify what Stage 4 did with the blueprint. The constraint stories are built on the diff between what Stages 2/3 specified and what Stage 4 produced: axes that emerged, shifted, expanded, or got invented during generation.

### Input

- Stage 2 output (naturalization / world design)
- Stage 3 output (editorial decisions / blueprint)
- Stage 4 output (generated story)

### Operations

```
1. DIFF ANALYSIS: Compare Stages 2/3 specification against Stage 4 output.
   Identify structural axes on the differences:
   - Constraints Stage 4 enacted that weren't in the blueprint
   - Constraints from the blueprint that Stage 4 shifted or expanded
   - Dynamics that emerged from constraint interaction during generation
   - Power relationships that Stage 4 made more or less extreme

2. SCOPE: Run UKE_SCOPE on the diff analysis.
   Select 3 axes for constraint story generation.
   These axes describe what's structurally interesting about
   what Stage 4 *did*, not what it was *told* to do.

3. CONSTRAINT STORY GENERATION: Per-axis, using story_generator_base.
   Same generation loop as c-orchestrator.

4. PROLOG ENGINE: JSON → .pl → run_pipeline → enhanced_report.py
   Produces per-constraint diagnostic reports.
```

### Output

3 enhanced reports (markdown) containing:
- Perspectival gap analysis
- Extraction masking detection
- Drift analysis
- Theorem instantiation
- Contamination network

These reports feed Stage 5 (Discovery) as structural intelligence about what the story actually enacts. They do not feed the story directly.

### Why Not Feed Stage 4?

The pre-generation engine (previously between Stages 1–2) produced reports like Boltzmann compliance tables, orbit signatures, and coupling scores — structural diagnostics written for the analytical layer, not for narrative generation. Translating them into generation context ("DRIFT ANALYSIS shows how constraints tighten — use for pacing") required too much bridging work. The post-generation engine serves a different purpose: it tells the *editor* what the *story* is doing, not the *author* what to write.

---

## Stage 5: Discovery

**Model:** Claude or equivalent analytical model
**Input:** Stage 4 output + enhanced reports from constraint engine
**Output:** Discovery report
**Temperature:** 0.3

**Purpose:** Identify what the generated story contains, informed by Prolog's structural analysis of what it enacts.

Discovery does NOT receive the blueprint (Stages 2/3). It does not compare the story against specification — the diff work already happened in the constraint engine step. Discovery reads the story and the Prolog reports and identifies what's there.

```
EMERGENT ELEMENTS:
  What does the story contain? What structural dynamics
  does it enact?

  The enhanced reports identify structural axes the story
  embodies. For each:
  - How does this axis manifest in the prose?
  - Which characters carry which side of the perspectival gap?
  - Where is the axis most alive in the writing?
  - Where is it present but underdeveloped?

LATENT POTENTIAL:
  What's present but not yet fully exploited?
  - Moments that gesture toward something deeper
  - The Frankl question: what meaning is available
    in the constraint that the story hasn't yet found?
  - World elements that imply a larger context

  The Prolog reports inform this specifically:
  - Extraction masking reveals what the story is
    structurally about underneath its surface
  - Perspectival gaps show where characters experience
    the same constraint differently — the richest
    dramatic territory
  - Theorem instantiation reveals structural features
    the prose could embody more fully (e.g., Oracle Gap
    means a confident character who misses what
    cross-position comparison reveals)

STRONGEST WRITING:
  Where is the prose most alive?
  These passages are the spine. Everything else serves them.

SCOPE ASSESSMENT:
  What did the piece become?
  - Short story (overlong) / novella (undercooked)
  - What would expansion require?
  - What would compression sacrifice?
```

---

## Stage 6: Strategy

**Model:** Claude or equivalent analytical model
**Input:** Stage 4 output + discovery report
**Output:** Strategy brief (concise — a page, not a treatise)
**Temperature:** 0.3

On second and subsequent editorial cycles (when Review routes back here), Strategy receives the Review assessment in place of the discovery report. The discovery report from the first cycle is still available in the run directory if needed.

The Strategy pass answers five questions:

### 1. Who is the implied author?

The governing sensibility — what does this author notice, care about, skip? Determines which details survive compression, what register the prose lives in, whether the ending opens or closes.

Selected from the intersection of the piece's strongest writing (from discovery) and its thematic commitments.

### 2. Who is the audience?

Specific audience > "general educated reader." Determines assumed knowledge, expected emotional range, tolerable pacing, needed resolution.

### 3. What is the scope?

Based on discovery report: short story / novella / larger work? Sets target word count range, world-building density, ending resolution.

### 4. What creative deepening is available?

Based on discovery report's latent potential: what meaning, turn, or dimension exists in the material that editing could bring out?

### 5. Where does this material flinch?

The pressure test. Identify moments where the draft resolves, soothes, or dignifies when the material's own logic demands cost.

```
PRESSURE INVENTORY:
  For each major constraint in the story:
    - What does this constraint actually cost the
      characters who live inside it?
    - Does the draft pay that cost, or does it convert
      cost into progress, meaning, or organizational change?
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

The pressure test identifies flinch points and names them in the strategy brief. It does not prescribe fixes.

**What this is not:** A mandate to make stories darker. Some material demands quiet resolution. The pipeline's failure mode is that it *always* chooses quiet resolution regardless of what the material demands. The pressure test corrects the default, not the range.

---

## Stage 7: Structure, Rupture, and Rewrite

**Model:** Claude
**Input:** Stage 4 output + strategy brief
**Output:** Revised narrative + edit manifest
**Temperature:** 0.7

One creative movement: diagnose structural problems, then fix them, governed by the strategy brief.

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

Edit manifest documents: what was changed, what structural problem it addressed, what flinch points were or were not addressed, and any unresolved issues.

---

## Stage 8: Pacing and Subtraction

**Model:** Claude
**Input:** Stage 7 output + strategy brief
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

  Target: 20-40% reduction from Stage 7 output.
  Floor: the point where nuance or necessary uncertainty is lost.
```

**Post-stage-8 numeric density gate (R6/R7, 2026-07-11; threshold recalibrated
2026-07-12):** the orchestrator runs a deterministic numeric-register meter
(`_numeric_inventory`) over the stage-8 story, writes the sidecar
`numeric_inventory_stage_8.json`, and — above `NUMERIC_DENSITY_THRESHOLD`
(10.0/1000 words) — issues one targeted revision call, then flags OPEN for the
operator (fail-visible, never a silent loop; the gate never auto-rejects).

**Standing caveat (OQ-215 close, 2026-07-12): density-green ≠ invariant-alive.**
Density measures counting only; invariant survival is adjudicated by blind
stage-9 + operator read. 0.0 is not evidence the invariant held. The meter is a
proxy for numeric register, narrower than the defect (unearned counting) — a
gauge-owning POV can read high and be earned (the rift3 false-positive class),
and an invariant can die on a story the meter scores 0.0.

**Post-stage-8 theme-density gate (OQ-214, 2026-07-13):** the analogue of the
numeric meter for theme-naming / explanation over-run. `_theme_inventory`
deterministically extracts every candidate in six kinds — **density-bearing:**
`anaphora` (consecutive sentence-initial repetition) and `causal_chain`
(repeated because/therefore formulas); **adjudication-only:** `refrain`,
`aphorism`, `resonant_closer`, `word_arithmetic` — injects the complete list
into stages 7/8 for per-instance adjudication, writes
`theme_inventory_stage_8.json`, and above `THEME_DENSITY_THRESHOLD` (8.0/1000
words, **density-bearing kinds ONLY**) issues one targeted revision call over
the flagged density-bearing lines, then flags OPEN.

**INVARIANT — this gate MUST NOT auto-reject; it escalates OPEN only.** Why theme
differs from counting: a digit does not correlate with merit, so the counting
gate can arm; theme-repetition DOES — the same surface is lazy theme-naming AND
earned craft (rift3's institutional creed, the empty-pan's refused ledger-math).
So only the two low-merit-correlation kinds move the gate, and even they escalate
to the operator rather than rejecting. **Kill condition:** promoting any
merit-correlated kind (refrain/aphorism/closer/word_arithmetic) to the density
gate "for determinism" turns the meter into a craft-suppressor — revert.
**Calibration finding — a NULL, and the null is the deliverable (2026-07-13,
`audits/2026-07-13_oq214_theme_meter/`):** the density-bearing kinds do NOT
separate the OQ-218 defect from its v0.2 fix (runs 1&2 identical anaphora/causal
counts; the fix lived entirely in refrain, which cannot gate; earned rift3=5.12
outscores two of three defects). The gateable axis is orthogonal to the defect,
so the threshold sits above everything good and essentially never fires.

**General law (the counting↔theme mirror): a defect is mechanically gateable IFF
it has a merit-INDEPENDENT signature.** Counting had one (a digit means the same
in a defect and a masterpiece → `_numeric_inventory` gates); theme-naming does
not (its surface IS the surface earned prose uses on purpose → this meter
collapses to a candidate list under adjudication). WHAT SHIPPED: the self-
certifiable "Theme-naming: none found" is closed — the adjudication layer gets a
high-recall list it must address per instance. WHAT DID NOT: explanation over-run
is not metered and cannot be by this approach; the register problem stays
reader-held (the Q2 double-No), unreachable by regex. Do not let the meter's
existence imply the register problem moved.

**Consequence (ruled 2026-07-13, OQ-214).** The mechanization boundary runs
along merit-correlation, and counting was provisionally the LAST pipeline defect
with a merit-independent signature (falsifier: a future defect that IS
mechanically separable → the meter approach revives for it). So the assisted
posture is **permanent architecture, by the structure of the problem** — not a
maturity stage: above the line is instrumented (counting/invariant/break-address,
R1–R14), below is definitionally reader-held. "Improve the pipeline" now means
improve the READERS in the loop, not build more meters. The repo-wide test of
whether this partition generalizes is OQ-221.

---

## Stage 9: Review

**Model:** Claude or equivalent analytical model
**Input:** Stage 8 output ONLY
**Output:** Route decision (STRATEGY or VALIDATION) + brief assessment
**Temperature:** 0.3

**Critical design constraint:** Review receives ONLY the Stage 8 output. No strategy brief. No edit history. No discovery report. No Prolog reports. It reads blind. This prevents the reviewer from rationalizing. It judges the prose as a reader would encounter it.

```
ASSESSMENT:
  Read the story as a reader. Produce a brief assessment:

  STRENGTHS: What is working. (2-3 sentences. Specific.)
  BIGGEST WEAKNESS: The single most impactful problem.
  READINESS: How far is this from publishable?

ROUTE DECISION:
  → VALIDATION (Stage 10): The story is ready for formal
    assessment. No significant weaknesses remain.

  → STRATEGY (Stage 6): The story needs another editorial pass.
    The biggest weakness is specific enough to inform
    a new strategy brief.

  Conservative default: if uncertain, route to STRATEGY.
```

If routing to STRATEGY, the Review assessment becomes the input for Stage 6's second cycle. Discovery (Stage 5) and the constraint engine do not re-run — the structural intelligence from the first cycle is still valid. Stage 6 receives the Review assessment + Stage 8 output.

**Iteration cap:** Maximum 2 full editorial cycles (Stage 6 → 9). If Review routes to STRATEGY a second time, the pipeline outputs the current state with the Review assessment attached and exits for human review.

---

## Stage 10: Validation (UKE_A Instance)

**Model:** Claude or equivalent analytical model
**Input:** Stage 8 output + Stage 1 specs (anonymized) + strategy brief
**Output:** Validation report with dimensional scores
**Temperature:** 0.2

Runs only when Review routes here.

**Dimensional Scoring (1-5, with cited evidence):**

```
D1: Constraint Preservation
    All constraints present, correctly typed, χ routing correct.

D2: Topology Isomorphism
    Causal relationships preserved, blocking intact,
    indexical variance maintained.

D3: Voice Integrity
    Voice matches archetype throughout, register consistency.

D4: Framework Invisibility
    Banned terms, theme-naming dialogue, structural vocabulary.

D5: Origin Obfuscation
    Per-vector analysis across seven vectors.
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

    5: Follows its material wherever it goes.
       Reader uneasy at least once, for the right reasons.
    4: Mostly honest. One moment flinches.
    3: Competent and measured throughout.
       Reader never uneasy. (Pipeline default.)
    2: Multiple comfort resolutions where material demanded more.
    1: Systematically avoids implications of its own premise.
```

**Routing:**

```
Total: ___/40

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

D8 ≤ 3 is the pipeline's expected default for first-pass stories. A score of 3 means "competent, measured, safe" — exactly the ceiling the pressure test addresses.

---

## Fracture Taxonomy (Narrative Extension)

The UKE_A fracture taxonomy (F01-F36) extends with narrative-specific codes. These are the codes Validation checks. The full diagnostic vocabulary lives in the Narrative Fracture Catalog (N01-N85); the F-codes are the routable subset.

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
     Ending resolves to avoid implications of its own premise.
     Detected by: Stage 7 rupture audit, Stage 10 D8

F48: Emotional Ceiling (→ N09 + N48)
     Prose maintains measured temperature throughout.
     No moment breaks the pattern.
     Detected by: Stage 6 pressure test, Stage 10 D8

F49: Administrative Resolution (→ N02)
     Conflict demanding personal cost resolved through systems.
     Detected by: Stage 7 rupture audit, Stage 10 D8
```

---

## Omega Tracking

Every stage maintains an omega log following UKE convention:

```
Ω_E: Empirical (resolvable by looking at the text more carefully)
Ω_C: Conceptual (requires a creative decision)
Ω_P: Preference (requires authorial judgment)
Ω_DR: Structural (from Prolog engine — carries constraint ID and gap class)
```

Review sees omegas only if embedded in the prose (it reads blind). Strategy sees the full omega log.

---

## Implementation Mapping

| Stage | Type | Temperature | Max Tokens | Input |
|-------|------|-------------|------------|-------|
| [engine] | Python/Prolog | — | — | stages 2, 3, 4 |
| 5 | LLM | 0.3 | 8192 | stage 4 + engine reports |
| 6 | LLM | 0.3 | 8192 | stage 4 + discovery report |
| 7 | LLM | 0.7 | 16384 | stage 4 + strategy brief |
| 8 | LLM | 0.5 | 16384 | stage 7 + strategy brief |
| 9 | LLM | 0.3 | 4096 | stage 8 ONLY |
| 10 | LLM | 0.2 | 8192 | stage 8 + stage 1 anon + strategy brief |

**Cost per editorial pass:** ~5 LLM calls (stages 5–9) or 6 with validation (stage 10). At Sonnet pricing, roughly $0.50–1.50 per pass depending on story length. Constraint engine step adds ~3 LLM calls for story generation + Prolog compute.

**Resume/re-entry:** All stage outputs persist. `--from-stage stage_5` re-runs the full editorial pipeline. `--from-stage stage_6` re-enters at Strategy with cached discovery and Prolog reports.

---

## Operating Posture: Assisted-by-Design (operator ruling, 2026-07-12)

The pipeline is an **assisted instrument, not an autonomous author**. The sharpening judgment —
does this story say something a competent model wouldn't? — is the product's core value and is
operator-held *by necessity*: stage-10 D9 scored the OQ-215 arm-2 negative control 5/5, so the
only discriminating witnesses are the blind stage-9 falsifier and the operator read. Autonomous
operation is structurally foreclosed, not deferred. Full ruling:
`docs/design/design_discipline.md` §11.

**`--edit FILE` is the first-class assisted/audit-only mode:** stages 5–10 run on a human-written
(or externally drafted) story — discovery, strategy, rewrite, pacing, blind review, validation —
with no generation stages. Use it to put an operator draft through the editorial and audit layers
without the pipeline authoring anything upstream.

**Division of labor (matches commit `168bc222`):** the pipeline authors the break's **ADDRESS**
(stage 0 writes the break contract — original_break / prior_status / target_prior — carried to
stages 2/9/10); **executing the violation** — the improvement judgment itself — belongs to the
UKE_STORY protocol (`agent/uke_story_v0.2.md`) under the operator's read. The pipeline names
where the break lives; it does not certify that the break landed.

---

## What This Architecture Changes

**Old model (monolithic Stage 5):** One LLM call does everything. No structural intelligence from Prolog. No pressure test. No rupture audit. No discovery. Editor and judge are the same instance.

**New model (Stages 5–10):** Fixed editorial sequence with structural intelligence. Constraint engine analyzes the diff between blueprint and generated story. Discovery finds what the story contains. Strategy governs editing with five questions including the pressure test. Structure/rewrite executes with rupture audit. Pacing/subtraction compresses. Review reads blind. Validation scores with D8 (Emotional Honesty) as automatic override.

**What's preserved:** Constraint validation, dimensional scoring, origin obfuscation checks, banned term scanning, the subtractive principle.

**What's added:** Post-generation constraint engine (diff-based), discovery, strategy with pressure test, rupture audit, pacing as craft, blind review, emotional honesty scoring, and the recognition that the pipeline's default failure mode is comfort — not incompetence, but the systematic avoidance of what the material demands.
