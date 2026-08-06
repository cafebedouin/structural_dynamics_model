# OQ-264 — k-redraw variance floor of pooled idiom SHARE (PROPOSAL, pre-registered)

Date: 2026-08-06. Executor: Claude (Fable 5), local session. Plan:
`~/.claude/plans/reviw-the-kritik-series-robust-quiche.md` (rev 2, operator review folded
in; plan approval covers Phases A–B, which spend nothing). This file is committed BEFORE
any idiom scoring. Origin: OQ-264 (ISSUES.md), minted from the OQ-259 item-1 Arm-0 gate
firing (`audits/2026-08-05_oq259_emphasis_discriminator/ARM0_HALT_REPORT.md`, operator
ruling 2026-08-05: "the Arm-0 result IS the finding").

**Question.** What is the k=3 variance floor of the pooled idiom-SHARE observable on
same-input decompose redraws of ONE file (Biopower NW), and does that floor leave the
observable usable as an audit instrument (vs resolving OQ-264 as a standard-only
verdict)?

## 1. Inputs (all free; zero API calls in Phases A–B)

Six manifests, three per file, same input bytes per file (md5s re-verified 2026-08-06
against the `1bd57a84` baselines: Biopower `722602a7…`, Cap `18f726ab…`):

| Key | Manifest | Gate role |
|---|---|---|
| biopower/base | `audits/2026-08-03_kritik_ingest/biopower_k_nhi_debate_2026_20260803_102652` | gate draw |
| biopower/r1 | `audits/2026-08-05_oq259_emphasis_discriminator/biopower_healthcare_kernel_2026_20260805_144612` | gate draw |
| biopower/r2 | `audits/2026-08-05_oq259_emphasis_discriminator/biopower_nhi_debate_2026_20260805_144823` | gate draw |
| capk/base | `audits/2026-08-03_kritik_ingest/capitalism_k_ndi2026_20260803_102445` | contrast only |
| capk/r1 | `audits/2026-08-05_oq259_emphasis_discriminator/capitalism_k_debate_2026_20260805_145017` | contrast only |
| capk/r2 | `audits/2026-08-05_oq259_emphasis_discriminator/capitalism_kritik_ndi2026_20260805_145128` | contrast only; ZERO-KERNEL |

**The gate is computed on the Biopower triple only** (the operator-shaped "one file
first" path). The Cap triple is the churn-extreme CONTRAST and feeds no gate. Cap K NW
is out of scope for per-reading measurement per the OQ-264 entry.

## 2. Observable (exact) and denominator formula

Per draw d: **share(d) = TAG(d) / D(d)**.

- **Unit population (denominator D):** kernel readings
  (`commitment_system_recognition.readings`) UNION selected axes (`axes[].selected ==
  true`) whose `claim_id` is not among the kernel `reading_id`s. Deferred axes EXCLUDED.
- **TAG(d):** units whose judged idiom class is `tag` or `tag-leaning` (`mixed` and
  `card` count as non-TAG) — unchanged from SCORING.md / PROPOSAL_ADDENDUM.md §3.
- **Zero-kernel handling (pre-registered; cannot be decided post hoc):** a draw with no
  contested kernel (capk/r2: `commitment_system_recognition` absent) uses the
  selected-axes FALLBACK population — a *different unit population* — so it does NOT
  enter any share range; it is reported as its own categorical outcome
  (**kernel-minting churn**) with its fallback share shown as contrast only.
- Idiom class assignment is a JUDGED call (declared, not disguised as mechanical); the
  scoring rubric is §3, and the scorer-variance instruments are §4/§7.

**Mechanical denominator control (executed pre-commit as formula verification; the
plan's stated Phase-A step):** the formula yields D = 6/4/6 (Biopower) and 6/4/3 (Cap),
matching the plan's predicted denominators AND SCORING.md's baseline denominators (6
and 6) — ALL PASS, full output in `CALIBRATION.txt`. Control semantics per §7: a
mismatch would have triggered the fix-formula-and-recommit path, not a halt.

## 3. Rubric: classes, anchors, holdout, blinding

**Classes** (per reading, judged against the source file's tag/heading layer —
`TAG_INVENTORY.txt`, the mechanical `grep -n '^#\{1,3\} '` yield of both baseline .md
files):

- **tag** — phrasing and grounding echo the block-heading layer directly (block names
  near-verbatim; the authority IS the file's own block dispute).
- **tag-leaning** — scaffolded on a tag/block position, extended with card authors.
- **mixed** — tag names and card-layer frames in comparable measure.
- **card** — phrasing echoes cited authors/positions the tag layer never names
  (read-through).

**Anchors (6 of SCORING.md's 12 recorded calls, quoted as worked examples):**

| Reading | File | Call | SCORING.md rationale |
|---|---|---|---|
| accumulation_reading | Cap | tag-leaning | echoes 1NC tag ("built-in output… not a policy failure") + card authors (Das, Waitzkin) |
| market_exchange_reading | Cap | mixed | decoupling/innovation/K-curve = tag names; Hayek/knowledge-problem frame = card layer |
| neoliberal_regime_reading | Cap | card | reframes the perm as neoliberalism-vs-capitalism-as-such — a literature distinction, not a tag |
| counter_conduct_reading | Bio | tag | Alt blocks name Counter-Conduct verbatim |
| reformist_iatrogenic_reading | Bio | card | Illich frame is card-layer; no tag names iatrogenesis |
| coalition_governmentality_reading | Bio | card | Alt-fails stance via Ilott archival reading |

**Holdout (the other 6, named here but their calls NEVER quoted in this file):**
`world_system_reading`, `growth_process_reading`, `framework_competitiveness` (Cap);
`totalizing_biopolitical_reading`, `empirical_falsification_reading`,
`post_political_totalization_critique_reading` (Bio). They are re-scored blind inside
the pool; agreement with SCORING.md is the holdout reliability item. Class-coverage
note: only one `mixed` call exists among the 12, and it is an anchor — the holdout set
has no mixed representative (declared).

**Blinding — one pooled cross-file packet.** All denominator units from all six
manifests in a single shuffled pool; labels seeded from the packet content hash; a
seeded subset of entries silently DUPLICATED (§4); planted controls (§7) mixed in.
Calls are committed FIRST (`calls.json`), the label→draw mapping SECOND
(`mapping.json`, deterministic and regenerable, not read by the scorer until the calls
commit exists in git).

**Declared blind limitations (all structural, none repairable in a single-agent
session):**

- **File identity is unmaskable** — idiom is judged against a specific file's tag
  inventory, so content reveals the file; the blind covers DRAW identity within file.
- **The pre-registration is partially leaked** (plan's declared confound):
  PROPOSAL_ADDENDUM.md §3 already records baseline TAG counts (Cap 2/6, Bio 3/6) *and
  names the TAG-side readings*; SCORING.md records all 12 baseline calls, and this
  session's executor read both to operationalize the observable. **The blind covers
  only the redraws; every baseline re-score is contaminated and labeled as such.**
  Holdout "reliability" is therefore a consistency-under-contamination upper bound,
  secondary evidence; the DUPLICATE pairs (which include redraw entries with no prior
  recorded calls) are the primary scorer-variance instrument.
- Executor and scorer are the same agent (as in the OQ-259 audits, stated); duplicates
  are byte-identical and may be recognized as repeats — duplicate agreement measures
  within-pass call stability, not scorer independence.

## 4. Scorer-variance and mechanical-comparator instruments

The design measures Var(generator) + Var(scorer); these instruments keep a FAIL from
being unattributable between them:

- **Seeded duplicates:** 6 pool entries silently duplicated (seeded from packet hash).
  Disagreement on a duplicate pair is PURE scorer variance, measured directly.
  TAG-side duplicate agreement also feeds the gate modifier (§5).
- **Holdout reliability:** §3, secondary (contaminated).
- **Mechanical comparator observables (no judgment):** D itself, selected-axis count,
  deferred-axis count, contested-kernel presence — per draw, with ranges. If a
  mechanical observable's range matches/exceeds the share range, variance localizes to
  the GENERATOR; if much smaller, the judged layer is the noise source.

## 5. Gate calibration (numbers set AFTER the lattice print + quantization simulation)

Witness: `CALIBRATION.txt` (committed with this file). Key facts from it:

- **Share lattice:** D=6 draws step by 1/6 ≈ 0.167; the D=4 draw steps by 0.25. The
  achievable RANGE lattice over the triple is k/12, k = 0..12.
- **Bands (decimals, expressed on that lattice):** PASS = share range ≤ **0.25**
  (explainable by quantization + at most one class flip); FAIL = share range ≥ **0.50**
  (requires ≥ 2 units of movement beyond any single slip); INDETERMINATE between
  (achievable values 1/3 and 5/12). Reachable bands shown in the lattice print.
- **Quantization simulation** (null: share perfectly stable at s*, scorer makes exactly
  one class flip, uniform over the 16 Biopower units): P(FAIL) = 0 at every s* on a
  1/24 grid — FAIL is unreachable under the stable null (max P(FAIL) = 0.0000).
- **Sensitivity statistic:** per observed configuration, the minimum number of
  single-class flips changing the raw band verdict (BFS over unit flips; per-boundary
  table in the compute output).
- **RECALIBRATION (pre-registered path taken; the plan's own clause "if
  FAIL/indeterminate dominates under the stable null, recalibrate thresholds in Phase A
  — never write miscalibration up as a finding").** The plan-as-drafted modifier
  ("sensitivity = 1 → indeterminate regardless of band") is REJECTED by the
  simulation: under every stable null + one flip it yields P(INDET) = 1.000, and even
  under a PERFECT scorer it yields INDET at every stable non-representable s* (e.g.
  s* = 0.6 → TAG [4,2,4] → sensitivity 1) — the rev1 columns in CALIBRATION.txt
  witness this. An instrument that cannot pass when the world is stable is
  miscalibrated. **Recalibrated modifier (final):**
  - FAIL with sensitivity 1 → downgraded to INDETERMINATE, always (one scorer slip must
    never manufacture a failure verdict).
  - PASS with sensitivity 1 → stands ONLY if the duplicate-measured scorer variance is
    zero at TAG-side (all duplicate pairs agree on TAG vs non-TAG); otherwise
    downgraded to INDETERMINATE (a measured error process makes the boundary crossing
    live, not hypothetical).
  - Sensitivity ≥ 2 verdicts stand.
  Under this rule the zero-error stable null PASSes at every s* and one-error
  P(FAIL) = 0 everywhere (CALIBRATION.txt "recal" columns) — calibrated.
- **Component rule (pass branch):** stability of `share` with instability of `TAG` and
  `D` does NOT satisfy the pass branch. Operationalized D-aware (so that honest
  proportionality under different D is not blocked): with s_pooled = ΣTAG/ΣD, the pass
  branch additionally requires max_d |TAG(d) − s_pooled·D(d)| ≤ 1.0. TAG range and D
  range are reported separately in all cases and both feed the verdict text: D churn
  (mechanically 2 units here, a 33% unit-population swing at fixed input) is itself a
  generator-instability finding that any pass must carry — a pass asserts
  SHARE-stability only, never manifest stability.

## 6. Decision gate (Hypothesis framing — interpretations, not expected results)

Computed on the Biopower triple only, from the committed blinded calls:

- **PASS band** (final verdict PASS after §5 modifiers) → the pooled observable is a
  candidate instrument; Phase C sizes k against the observed range. **k-monotonicity
  clause:** observed range is monotonically non-decreasing in k, so ANY k=3 pass is
  PROVISIONAL by construction; Phase C pooling can retract it, and the retraction path
  is pre-registered (Phase C reports the range at full pooled k AND as the mean range
  over all 3-draw subsets; a Phase-B pass retracted at higher k is reported as
  retraction, never averaged away).
- **FAIL band** → the pooled repair fails at Phase 0; OQ-264 resolves as a
  **standard/verdict** (no per-reading or pooled-share claim citable without k-redraw
  error bars), with the scorer-vs-generator decomposition (§4) attributing the
  failure; OQ-259 items 2–3 re-scope — operator checkpoint before closure.
- **INDETERMINATE** → Phase C's first job is more Biopower draws only.
- **Cap triple:** churn-extreme contrast only; feeds no gate. capk/r2 is reported as
  the categorical kernel-minting-churn outcome (§2).
- **Operator checkpoint after Phase B in every branch** — gate outcome + variance
  attribution → Phase C go/shape, or standard-only closure. Phase C (spend) runs ONLY
  on operator go.

## 7. Control semantics (pre-registered)

- **Judged planted-manifest control — HALTs on failure.** Two synthetic entries
  (`planted_control.manifest.json`, authored in Phase A): `plant-tag` (pure block-layer
  idiom; its vocabulary confirmed PRESENT in the Bio tag inventory, 3 heading hits) and
  `plant-card` (Federici/Harvey/Luxemburg enclosure position; vocabulary confirmed
  ABSENT from both tag inventories — 0 hits, with positive controls Counter-Conduct ×4
  / Sustainability ×12 proving the probe finds what exists). HALT rule at the
  observable's altitude: `plant-tag` scored non-TAG, or `plant-card` scored TAG, halts
  the audit before any share computation. Exact-class agreement (`tag`/`card`) is
  reported descriptively. Single-agent limitation declared in §3.
- **Mechanical denominator control — does NOT halt on first mismatch.** Likely cause of
  a mismatch is a wrong formula hypothesis about 2026-08-03 practice: pre-registered
  path is fix formula → re-commit with a note → re-run; only an unresolvable mismatch
  halts. (Executed: ALL PASS on the first run; no fix path needed — §2.)

## 8. Declared confounds

- **Cross-day drift vs model version:** checked (free). All six manifests carry
  identical provenance (`scope_model: claude-sonnet-5`, prompt commit `d179423d`,
  schema commit `43ee9613`), and no finer build string exists anywhere in the manifests
  or the four run logs (the alias is the only model identifier, 16 log occurrences).
  So: no evidence of a version change, but the alias cannot pin the server-side build —
  a silent build change between 08-03 and 08-05 is not excludable from local records.
  Declared as a scoped residue on any drift attribution: observed spread =
  stochasticity + unexcluded server-side drift, jointly.
- **Age gap:** baseline draws are 2 days older than redraws (persisted_at 08-03 vs
  08-05); with the version check above, this cannot be decomposed further from local
  data.
- **Var(generator) + Var(scorer):** the design measures their sum; §4's instruments
  exist to keep a FAIL attributable.
- **Leak:** §3's declared blind limitations (ADDENDUM §3 counts + SCORING.md calls +
  same-agent executor/scorer).

## 9. Phase B execution list (no spend; witnesses named)

1. `python3 python/audits/oq264_idiom_share.py packet` → `packet.md` + `mapping.json`
   (mapping unread). Witnesses: entry counts, packet sha256, shuffle seed.
2. Commit `packet.md` (not `mapping.json`).
3. Blinded idiom pass over `packet.md` against `TAG_INVENTORY.txt` → `calls.json`
   (one class + one-line justification per label). Commit.
4. Commit `mapping.json`. Write `holdout_expected.json` from SCORING.md (only now).
5. `python3 python/audits/oq264_idiom_share.py compute --holdout holdout_expected.json`
   — planted HALT check first; then duplicates, shares, ranges (TAG, D, share
   separately), component residuals, sensitivity, final gate verdict, mechanical
   comparators, holdout agreement.
6. `PHASE0_REPORT.md` with every §4–§6 quantity + the categorical capk/r2 outcome +
   drift-vs-version attribution + leak caveat. Commit.
7. STOP at the operator checkpoint (§6).
