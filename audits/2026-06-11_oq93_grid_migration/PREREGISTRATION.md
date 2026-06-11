# Preregistration — OQ-93 grid migration (stages A→D + coverage half-step + shim retirement)
# + riders OQ-102(a)(b) + terminal OQ-101 ledger

Drafted 2026-06-11, worktree `oq93-grid-migration` at base `a4297632`. Plan source:
`~/.claude/plans/review-oq-93-and-come-mellow-mountain.md` (operator-approved). This document
pre-registers witnesses and halt branches BEFORE any write pass. Per
`feedback_audit_plan_discipline`: expectations below are HYPOTHESES except where marked
REGRESSION-PIN (already-witnessed values whose substrate is pinned); the pass criterion is
always the produced output, never the prediction; a wrongly-specified criterion discovered
mid-run is itself halt-and-escalate, never inline-amended.

## Rulings in force (executed, not re-litigated)

- OQ-93 ruling (b) 2026-06-10: KEEP-AND-MIGRATE gradient/κ/pattern; imputation/injection killed
  permanently (authored-or-absent); named consumer = masking/naturalization verdict family.
- OQ-93 ruling (a) 2026-06-10: `structural_coercive_intent` excluded — retire-or-redesign
  sub-fork deferred (Phase 8 files it as a new OQ).
- Partial grids: consumer-named-levels REPLACES any global fraction threshold (operator
  CONFIRMED 2026-06-10 — recorded here, not re-asked).
- Riders confirmed this session: OQ-102(a) per-time-point `basis` provenance and OQ-102(b)
  drift severity-vs-confidence join ride the stages as SEPARATE commits inside each stage.
- OQ-101 ledger is terminal, after Phase-5 acceptance witnesses pass.

## Stage-D absence-semantics pin (ruling-(b) fidelity reconciliation)

The OQ-93 entry's battery item 4 says the naturalization verdict needs "structural AND
individual — its two needles — present, else OPEN." **Pinned reading: the grid-derived SIGNAL
goes OPEN** — when named levels are missing, the grid contributes nothing to the consuming
verdict, which falls back to its existing (static-proxy) evidence; absence never blocks or
flips the verdict itself. This is the only reading consistent with (i) the same entry's
"consumed positively … never blocks on absence" framing of ruling (b) and (ii) not flipping
the entire grid-absent live corpus to OPEN on wiring day. The κ-track's OWN verdicts
(pattern_analysis pattern, intent lower verdicts, SECTION 6 κ) DO go OPEN on missing named
levels — they are grid-native and have no non-grid evidence to fall back to.
**If the operator intended the literal verdict-level reading (consumer verdicts themselves go
OPEN), that is an operator call — escalate, do not absorb.**

## Stage-2 negative battery (schema/compiler guards; each case must be SHOWN to bite)

1. **Out-of-interval time points** — a grid point with `time_point` ∉ {t0, tn} is REJECTED
   (compiler referential-integrity check; schema where Draft7 can express it). Positive
   control: a constructed story with a t=5 point on a [0,10] interval fails loud naming the
   point.
2. **Out-of-enum levels/metrics** — schema enum rejection. Positive control: `level: "cosmic"`
   and `metric: "vibes"` each rejected by the validator.
3. **Duplicate slot authorship** — two grid points with identical (metric, level, time_point)
   REJECTED LOUD by the compiler on every generation path (`--no-validate` does NOT bypass).
   This is the contract that licenses the `once/1` slot-cap in
   `pattern_analysis:compute_completeness` (defense-in-depth note at pattern_analysis.pl:46-55);
   the queued **constructed-duplicate positive control** runs here: a JSON authoring the same
   slot twice must be rejected on BOTH the validate and --no-validate paths.
4. **Partial grids** — no fraction threshold. Consumer-named-levels (operator-CONFIRMED):
   reads carry coverage; each consumer states its required levels; any missing → that
   consumer's grid signal is OPEN.

## Coverage-carrying read — two-sided witness (the unchanged-plus-healed pair)

Substrate pin: the five probe stories at
`audits/2026-06-10_oq93_grid_viability_probe/stories/` (32/32 authored grids), run via
`scenario_manager:load_and_run` with shim OFF, at the worktree commit of the Phase-3 change.

- **Healed side:** the constructed 8/32 one-level grid (structural only, both endpoints),
  which TODAY yields `G_sys=0.2160, completeness=0.25, pattern=increasing_coercion`
  (witnessed 2026-06-10, OQ-93 entry), must yield pattern OPEN (missing named levels) after
  the change. The 0.2160 pre-change value is re-witnessed in this audit before the change
  lands (staleness rung: re-run, don't cite).
- **Unchanged side (REGRESSION-PIN, FINDINGS.md 2026-06-10):** the five probe stories keep
  exact values — G_sys +0.5880 / −0.5880 / 0.0000 / +0.1560 / +0.9800; patterns
  increasing/decreasing/stable/increasing/increasing; κ 0.80/0.20/0.50/0.49/1.00 — tolerance
  ±0.001 as pinned by the probe prereg.
- **Corpus side:** full validation suite exit 0, warning gate clean; live corpus is
  grid-absent, so every gradient/pattern/intent read reports OPEN (honest absence), with NO
  `stable`-shaped or `0.0`-shaped output from an empty read anywhere ([INTENT] line included).
  HYPOTHESIS: the only suite-output deltas vs current are absence-shaped lines replacing
  `stable (Confidence: low)`-shaped lines; any OTHER delta is halt-and-inspect.

## Fire-on-migration witnesses (OQ-93 entry, 2026-06-11 addendum — must FIRE or record why not)

i.  Kappa `[CONDITIONAL: grid authored A/T]` tag (`report_generator.pl` kappa block):
    construct a partially-authored grid (authored < total with κ still computable at Tn —
    e.g. full Tn row for ≥1 level, missing elsewhere) and paste the tag firing.
ii. Moderate→yellow verdict cap (`diagnostic_summary:verdict_join/3`): corpus-content-gated
    (needs a correction-grade signature on a base-GREEN constraint). Witness it firing if the
    grid batch produces such a constraint; otherwise RECORD WHY NOT (which constraints carry
    correction-grade signatures and their base verdicts). If it fires: re-run
    `audits/2026-06-11_oq98_verdict_join/histogram_gate.pl`.

## OQ-102(a) basis rider — firing chain (landed-as-code is NOT done)

Nothing live authors `basis`, so the rider owes a constructed fixture: a story with
`basis: projected` on a measurement, witnessed through (1) compiler emission →
`narrative_ontology:measurement_basis(MID, projected)`, (2) the per-bucket
`measurement_provenance` count showing a nonzero `projected` bucket, (3) at Phase 7, the
ledger's drift line rendering the provenance. OQ-102 closes only with this chain pasted plus
rider (b)'s read-site diff.

## Prevalence sequencing (structure, not a number)

1. Dedicated grid-batch variant prompt (NOT the live prompt) generates a small batch — the
   κ-plausibility-audit sample. OQ-70 example discipline: the one-shot example must not seed
   copyable constants (archived example at `prompts/archives/prompt_deterministic.md:182-191`
   is reference, not template).
2. The κ plausibility audit runs against the criterion below; NO consumer reads generated
   grids before the gate passes and the operator rules tolerance.
3. Only after the operator ruling: live prompt flips to opt-in-by-story-focus (the
   suppression_requirement/OQ-46 precedent). Never all-stories.

## κ plausibility criterion (PRE-WRITTEN; tolerance + N are the operator's, asked at prereg time)

Per generated grid story, machine-checkable counterfeit indicators:
- **C-echo (template echo):** grid values duplicate the prompt example's value table verbatim,
  or all 32 values are a single constant.
- **C-flat (level degeneracy):** all four levels carry identical per-metric trajectories
  (max pairwise level difference < 0.05 across all metric×time slots) — the level axis is the
  track's unique product; a batch that never differentiates levels authored no information.
- **C-dir (direction incongruence):** sign(G_sys) contradicts the story's own authored drift
  direction where one exists (any authored scalar measurement series with |Δ| ≥ 0.1; stories
  with no scalar series are exempt from C-dir, counted separately).
- **C-range:** any value outside [0,1] or any slot count ≠ 32 (schema should make this
  unreachable — its firing is a battery failure, halt).
**Tolerance + N: OPERATOR-RULED 2026-06-11 (typed ruling at prereg time, not plan-default).
Split by what the indicator measures — the gate answers two questions, not one:**

- **Per-story (consumer safety, fail-closed, NO tolerance):** any story failing any indicator
  is excluded from consumer reads, always; report k/N with N stated, never "clean."
- **C-echo (batch-level, prompt property): ZERO tolerance — one echo HALTS the batch.**
  Template echo means the one-shot example seeds copying (the OQ-70 failure this gate exists
  to catch); excluding the story and passing the batch would certify a prompt witnessed
  teaching the defect. There is no honest fraction of "the prompt teaches counterfeits."
- **C-flat / C-dir (batch-level, prompt quality): batch ESCALATES at ≥ 2/10 failing.**
  Level degeneracy and direction incongruence can be idiosyncratic one-story failures;
  per-story exclusion already secures consumer safety, so the batch fraction measures only
  prompt quality — ≥20% idiosyncratic failure is a prompt problem, not noise.
- **N = 10.** The binding detection target is C-echo, which is systemic by nature when
  present, so N=10 detects it with high power; N=5 of generated (not hand-authored) stories
  is a weak read even on a systemic defect; N=30's extra resolution only matters near the
  2/10 line.
- **Flip provision: a passed N=10 batch is NECESSARY for the live-prompt flip, not
  automatically sufficient** — the flip ruling may demand a supplemental batch first; the
  operator keeps N=30-equivalent confidence as an option without pre-paying it.

## Halt-and-escalate branches (pre-registered)

- Any two-sided control falsifying its expectation (probe values drift; 8/32 stays
  increasing_coercion; duplicate-slot story compiles; basis fixture emits nothing).
- Stage-A additivity sweep producing ANY new validation failure on existing `json/`.
- Stage-B old-vs-new compile producing ANY nonzero diff on the existing corpus outside the
  rider commits' derived expectations.
- κ batch failing the operator-ruled tolerance.
- C-range firing on a schema-validated story (battery failure).
- Shim-retirement suite diff ≠ 0 vs current shim-off default behavior.
- A pre-registered criterion discovered to be wrongly specified (escalate the criterion, do
  not amend inline).

## Expected diffs (derived before write, per `feedback_derive_diff_before_run`)

- Stage A: `pipeline_output.json` manifest unchanged; all existing `json/` validate
  identically pre/post (no story authors `coercion_grid` or `basis`); pre-existing invalids
  fail identically.
- Stage B: compiled `.pl` byte-identical over the full existing corpus (emission conditional
  on field presence). Pilot grid story (constructed, audit-local) emits interval + 32
  `measurement/5` facts queryable in swipl.
- Phase 3: suite-output delta = absence-shaped lines only (hypothesis above); probe stories
  pinned; 8/32 flips to OPEN.
- Phase 5: live corpus classifications UNCHANGED (grid-absent corpus → grid signal
  contributes nothing anywhere); the divergent probe story's consumer fires; flat-structural
  genuine-mountain probe stays silent.
- Phase 6: 0-diff vs current shim-off behavior; `grep -ri grid_shim` residue = comments/
  docs/audits only (positive control: grep finds this file).
