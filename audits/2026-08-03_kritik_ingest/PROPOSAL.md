# Kritik ingestion — comparative dry-run probe (PROPOSAL, pre-registered)

Date: 2026-08-03. Executor: Claude (Fable 5), local session. Plan:
`~/.claude/plans/temporal-soaring-cherny.md` (final, merged). This file is written BEFORE
any orchestrator run; the predicted reading lists below are derived from the SOURCE files'
block headings only — no manifest existed when they were written.

## Pre-registered question

Does SCOPE kernel-detection recover coherent structure from an arsenal-format document
(a debate-camp card file: dozens of attributed published voices under an authored
editorial tag/heading layer)? Central unknown: does SCOPE read the tag layer as claims,
the card layer, both, or neither?

## Design version executing (deviations from the plan as drafted, all pre-run)

1. **Preferred replicate EXECUTES.** `Capitalism K Aff And Neg - Northwestern 2026` could
   not be fetched from this environment (DNS SERVFAIL on openev.debatecoaches.org,
   witnessed 2026-08-03); the operator downloaded the .docx manually and it was
   pandoc-converted here (same recipe: `pandoc -f docx -t gfm --wrap=none`). So the
   same-camp near-replicate pair (Cap K NW + Biopower NW) executes — under (e),
   **divergence indicts the format**, the stronger design version.
2. **Size-class caveat.** The plan assumed the replicates were "same size class." They are
   not: Biopower NW .md = 280K, Cap K NW .md = 1.1M (~4×). Same camp, same format, same
   Aff-and-Neg arsenal structure (parallel #/##/### skeleton, verified below), but a
   size-driven divergence cannot be fully excluded; recorded as a scoped residue on any
   (e) verdict.
3. **Uniform `--skip-search` on ALL runs (control included).** Measured code fact
   (`_ingest_decision`, c-orchestrator.py:1053): with `--skip-search` only the decompose
   cap (~975k tok) is consulted; with research ON the ~187.9k research cap binds and
   Cap K NW (~1.1MB ≈ well over the cap) would trip the LOSSY brief — and a brief-derived
   manifest is not comparable to whole-doc manifests. Uniform flags are required for
   comparability, so all four runs go `--skip-search`. Consequences (from the plan's own
   contingency, adopted here as design):
   - the single-voice caveat applies to AT Fiat K UNGROUNDED (research grounding was
     load-bearing for it; it never happens);
   - Phase 3's "the frozen manifest already carries the dry-run's grounding" clause is
     STRUCK; Phase 3 `--skip-search` stands on the mechanical rationale alone (research
     feeds only the bypassed decompose step).
4. **Conversion normalization.** This machine's pandoc emits `<span class="underline">`
   for docx underline; the 15 pre-existing conversions carry no such spans. All 3,182
   underline spans were stripped from the fresh Cap K NW conversion (sed, verified 0
   remaining) so every specimen sits on the same emphasis-blind footing.

## Phase-0 conversion-fidelity verdict + emphasis ruling

Witnessed on `Biopower K Aff And Neg - Northwestern 2026`: the .docx carries 1,152
`w:highlight w:val="yellow"` runs (read-in-round layer) and stratified font sizes
(`w:sz` 16 ≈ minimized 8pt ×1120 runs vs 24 = 12pt ×513); the .md contains ZERO
highlight/span markers. **The read/unread distinction is flattened.** Nuance beyond the
plan's prediction: **bold survives** in the .md — a partial emphasis channel (the
bold/underlined card-emphasis layer) persists; highlight (read-in-round) and
minimization (font size) do not.

**Operator ruling (2026-08-03): branch (A) — emphasis-blind, chosen and recorded.**
Consequence, binding on Phase 2: outcome (d) is CONDITIONAL — claimable only as a
property of *emphasis-blind ingestion of the format*, never of the format itself. The
named discriminator follow-up is: a small extractor pulling `w:highlight`/`w:sz` from
`document.xml`, emitting emphasis markers, re-convert the three specimens, re-run
Phase 1 emphasis-aware. Rationale (from the plan): the tag layer is a few dozen words
per block vs hundreds in the card bodies — emphasis-blind ingestion mechanically favors
(d) by an order of magnitude in token volume, so (d) and the conversion loss are the
same confound.

## Specimen set + control

| File | Size (.md) | Role |
|---|---|---|
| `k_files/AT Fiat K - Michigan 2026 BCFP.md` | 68K | Meta-layer; single-voice behavior check. **NO evidential weight on the arsenal question** (per (c)). |
| `k_files/Capitalism K Aff And Neg - Northwestern 2026.md` | 1.1M | Replicate 1 (preferred; operator-fetched). |
| `k_files/Biopower K Aff And Neg - Northwestern 2026.md` | 280K | Replicate 2. |
| `agent/analysis/originals/emotives.md` | 12K | **Fresh control** — hand-authored; the witnessed Sonnet-5 full-run source (2026-07-22). "Coherent/incoherent" at Phase 2 means relative to THIS run's manifest. |

Fallback `Necropolitics K - JDI 2026.md` (252K) is NOT in the executing set.

## Exact commands (serial, in this order)

```bash
cd /home/scott/bin/structural_dynamics_model
# control (Phase 0 step 4)
python3 agent/c-orchestrator.py --dry-run --skip-search "agent/analysis/originals/emotives.md"
# Phase 1 specimens
python3 agent/c-orchestrator.py --dry-run --skip-search "agent/analysis/originals/k_files/AT Fiat K - Michigan 2026 BCFP.md"
python3 agent/c-orchestrator.py --dry-run --skip-search "agent/analysis/originals/k_files/Capitalism K Aff And Neg - Northwestern 2026.md"
python3 agent/c-orchestrator.py --dry-run --skip-search "agent/analysis/originals/k_files/Biopower K Aff And Neg - Northwestern 2026.md"
```

No `--auto-bypass-refusal` (refusal STOP is designed behavior). Corpus must remain
untouched (verify: `git status` clean on `prolog/testsets/`, no new `json/*.json`, no
`*_brief.md` beside sources).

## Pinned denominators and predicted reading lists

**Denominator rule (fixed):** top-level block headings only = the `##` sections under
each `#` side for the two arsenals; for AT Fiat K (which has no side/section structure —
a flat answers file, itself consistent with the documented single-voice format) the six
flat `###` headings. Extraction command: `grep -n '^#\{1,3\} ' <file>` (witnessed
2026-08-03; inventories below are its verbatim yield at the pinned level).

**Scoring definitions:** per manifest reading — **hit** = subject+stance match a
predicted reading below; **idiom** = tag (phrasing echoes the editorial tag/heading
layer) vs card (phrasing echoes cited authors/positions absent from the tags).
**precision** = hits / manifest readings. **recall** = predicted readings surfaced / N.

### Cap K NW — N = 10

Neg side:
1. **1NC (core K):** capitalism makes ill health structurally inevitable; health reform
   within capitalism cannot resolve it — critique stance.
2. **Framework (Neg):** evaluate the 1AC's epistemology/method, not simulated policy
   outcomes.
3. **Link/Perm (Neg):** the aff's care/NHI/reform entrenches capital; permutation
   (do both) fails/is co-opted.
4. **Impact (Neg):** capitalism → slow death, social determinants of ill health, serial
   policy failure, fascism, unsustainability.
5. **Alt:** movements/refusal — rejection of capitalism; cap is not inevitable.

Aff side:
6. **Framework (Aff):** policy simulation/fiat frame good; AT ideological-bias and
   epistemology-first.
7. **Link/Perm (Aff):** perm — single payer solves within capitalism; reform compatible.
8. **Sustainability:** capitalism is sustainable — living standards, decoupling,
   innovation, K-curve.
9. **Impact---Other:** cap good — poverty reduction, deterrence.
10. **Alt (Aff):** the alternative fails / transition offense.

### Biopower NW — N = 9

Neg side:
1. **1NC (core K):** universal health care as biopolitical governmentality — regularizing
   population health extends/centralizes state power.
2. **Framework (Neg):** micro-sovereign / epistemology-first framing; AT utilitarianism.
3. **Link:** coverage, crisis, health insurance, prisons, surveillance, universality as
   biopower links.
4. **Impact (Neg):** biopower makes massacre/war in the name of life possible.
5. **Alt:** counter-conduct / fugitivity / interrogation as resistance.

Aff side:
6. **Framework (Aff):** policy-evaluation frame against epistemology-first.
7. **Link/Perm (Aff):** perm do both; no link + link turn.
8. **Impact (Aff):** AT necropolitics; biopower wrong / non-unique / no impact; AT war.
9. **Alt (Aff):** counter-conduct fails / circular / alt turn.

### AT Fiat K — N = 6 (meta-layer question ONLY; no arsenal weight)

1. small-scale student-led change works
2. cards about healthcare
3. ethical localism turn
4. pretending is ethical
5. at: distancing / it makes us mean
6. nobody thinks a policy is getting passed

## Pre-registered outcomes (decision rule, verbatim from the plan)

- (a) **Mush** — operationalized: manifest readings pairwise indistinguishable by
  subject+stance, OR fewer than 2 distinct readings survive the §3 independence test, OR
  0 hits AND no identifiable cited-author positions (tethered to *neither* layer) — each
  judged relative to the fresh control's distinctness → card files defeat the format;
  route future kritik work through hand-authored source docs.
- (b) **Community-layer jackpot** — precision ≥ 1/2 AND recall ≥ 1/3 AND hits
  majority tag-idiom → card files are pre-labeled reading corpora; proceed to meta-layer
  full runs.
- (d) **Source-literature capture** — coherent (control-comparable), majority card-idiom,
  recall < 1/3 → the pipeline reads through the arsenal to its sources. **Conditional on
  the Phase-0 fidelity verdict:** emphasis WAS lost (branch A executing), so (d) may be
  claimed only as a property of emphasis-blind ingestion, never of the format; the
  extractor + re-run is the named discriminator.
- (f) **Partial recovery (residual class)** — coherent, but mixed idiom or sub-(b)
  thresholds (e.g. precision ≥ 1/2 with recall < 1/3 and mixed idiom, or precision in
  [1/4, 1/2)) → graduate ONE meta-layer file with the caveat attached; do not expand the
  K-file corpus on this evidence.
- (e) **Replicate divergence** — the two multi-voice files land in different outcome
  classes → format-unstable; neither (a) nor (b) may be claimed; more replicates before
  any full run graduates. Design version executing: **Cap K NW (same camp+format as
  Biopower NW) — divergence indicts the format**, with the size-class residue of
  Deviation 2 attached.
- (c) **AT Fiat K flat-routes** — recorded as consistent with the documented single-voice
  tripwire AND with arsenal failure; the design cannot distinguish these, so the
  observation carries no weight on the arsenal question (stated plainly in the writeup).

Hypotheses with interpretations, not expected results.

## Standing constraints

- OQ-230 (open): source→cid recorded manually in this audit dir at Phase 3.
- OQ-258 (open, no generation hold): ε-referent unusually live for kritik material;
  observe and record per authored ε at Phase 3; never cite ε cross-leg.
- Operator rulings: full runs land in live `testsets/`; no worktree; commit-as-you-go.
- Phase 2 is an OPERATOR GATE: no writes past it without the operator's go.
