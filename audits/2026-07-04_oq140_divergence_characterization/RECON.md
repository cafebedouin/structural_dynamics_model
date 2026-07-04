# OQ-140 RECON — characterize the `author_engine_divergence` population

**Execution date:** 2026-07-04 · **Code state:** HEAD `7762b2c0` · **Mode:** read-only
(reads live `outputs/*.json`; no engine edits; regenerates only gitignored `outputs/`).

Re-witnessed this session against live artifacts (`outputs/routing_sink.json` +
`outputs/pipeline_output.json`, both mtime 2026-07-04 10:31). Every number below is
reproduced by `python3 python/audits/oq140_divergence_extract.py recon` (raw in
`extract.log`; JSON in `recon_summary.json`). The plan's rev-3 recon counts (277/512 on
96/128) **reproduce exactly** at this rev.

**Regime caveat (Σ-witnessed, not a memorized count):** all counts are relative to the
128-constraint live corpus at HEAD `7762b2c0`. The `routing_sink.json` manifest carries
`n_constraints/n_seats/n_records/per_seat_invariant_holds` but **NOT** `code_commit` or
`pipeline_run_at` (unlike the pipeline_output manifest convention) — so same-run coherence
is witnessed by the run (control 5), never by the file. `per_seat_invariant_holds = True`
(512 = 128×4).

## 1. Population (address counts)

| address | count |
|---|---|
| **author_engine_divergence** | **277** |
| no_route | 95 |
| engine_exit_table_review | 58 |
| engine_abstained | 46 |
| both_silent | 36 |
| *total* | *512* |

Divergence is **277 records on 96 constraints — the majority address**, exactly as the OQ
frames it (the OQ's "148/288" is stale pre-corpus-growth; superseded).

## 2. The titular "per diagnostic mode" axis is near-silent — and TWO more axes join it

`provenance.mismatch` over the 277: **273 `none` / 4 `type_3_snare_as_rope-severe`**. The
mismatch tap fires structurally (`dr_claim_mismatch/4`, `drl_core.pl:635-662`) only on
authored mountain/rope or theater_ratio≥0.70; it is a 4-record footnote, not an axis.

**Extends the plan (new this recon):** two further per-record axes are *also* constant
across all 277 —

- `author_mode` = **`seat_blind_claim` × 277 (100%)**
- `detector` = **`nl_absent` × 277 (100%)**

So the entire divergence population is *one* authoring mode (a single seat-blind claim per
story) against a *seat-varying* engine, with no NL detector involvement. Three of the OQ's
proposed cut-axes (diagnostic mode, author mode, detector) are degenerate on this
population; the live structure is carried by **granularity class** and **type-pair**.

## 3. Granularity class — the primary cut (record-level Σ)

Computed from the **engine field only** (independence note); membership from the **address
field**. Σ-checked at record level.

| class | definition | constraints | records |
|---|---|---|---|
| **G-A** | uniform engine orbit (all 4 seats same type) | 14 | 56 |
| **G-B** | non-uniform orbit, author matches ≥1 seat | 50 | 122 |
| **G-C** | non-uniform orbit, author matches no seat | 32 | 99 |

56 + 122 + 99 = **277** ✓ (Σ-check == n_divergence: True). G-A's 56 = 14×4 is definitional
(uniform orbit + seat-blind author ⟹ all four seats diverge) — arithmetic, not a
measurement.

## 4. Pair × G-class crosstab — pairs live in disjoint classes

| pair | total | G-A | G-B | G-C |
|---|---|---|---|---|
| tangled_rope→snare | 111 | 0 | 63 | 48 |
| rope→scaffold | 61 | 36 | 0 | 25 |
| tangled_rope→rope | 49 | 0 | 30 | 19 |
| snare→rope | 11 | 0 | 11 | 0 |
| scaffold→piton | 8 | 8 | 0 | 0 |
| snare→tangled_rope | 7 | 0 | 7 | 0 |
| *(tail: 9 cells)* | *30* | | | |

Tail cells (30 records, unnamed by the plan's candidate kinds): tangled_rope→naturalized 7,
scaffold→tangled_rope 4, rope→snare 4 (all G-A), piton→rope 4 (all G-A), rope→piton 4 (all
G-A), rope→tangled_rope 3, scaffold→snare 2, snare→naturalized 1, scaffold→rope 1. **Note
the 3 clean G-A tail cells (rope→snare, piton→rope, rope→piton — 12 records) the plan did
not enumerate; they belong to the confound-free population and need disposition (esc. 4).**

## 5. Confound quantity (committed number — feeds the reframe gate)

The reframe gate asks: does the seat-blind→seat-varying setup cross type-pair boundaries
*mechanically*, regardless of authored content? Witnessed:

- **ε is uniform across seats in 96/96 divergence constraints** (0 non-uniform). Within a
  constraint every seat shares one ε; provenance 89 `none_authored` / 7 `authored` — even
  the 7 authored are seat-uniform. ⟹ **100% of the per-seat engine-orbit variation is
  driven by per-seat `d` (cognitive displacement) at fixed ε.**
- **institutional has the lowest `d` in 92/96** constraints ⟹ institutional is the
  systematically-displaced seat; it **diverges on 93/96** (the most of any seat: institutional
  93, moderate 72, analytical 64, powerless 48).

**Committed partition:**

| | records | share |
|---|---|---|
| **confound-FREE** (G-A, uniform orbit — engine agrees across seats, disagrees with author) | 56 | **20.2%** |
| **confound-EXPOSED** (G-B+G-C, d-fractured orbit) | 221 | **79.8%** |

G-A is confound-free *by construction*: a uniform orbit means the d-mechanic did **not**
fracture the type, so the author↔engine disagreement there is a genuine content
disagreement. G-B/G-C are `confound-until-shown`: their divergence *could* be pure
d-band-crossing. This is the number the operator rules against threshold **X** at the
reframe gate (esc. 2). The evidence settles the share; only X is the seat.

## 6. D-ladder — axis-D (signature involvement) is POWERED

**(i) Code-read at HEAD:** live unconditional overwrite clauses in
`resolve_modal_signature_conflict` confirmed at `signature_detection.pl:938`
(`coupling_invariant_rope→rope`), `:953` (`coordination_scaffold→rope`), `:978-995`
(constructed_*). So raw≠final is *feasible*.

**(ii) Natural positive control (`d_ladder_control.log`):** the corpus already exhibits
**49 baseline seats where `metric_based_type_indexed` (raw) ≠ `dr_type` (final)** — live
signature-overwrite seats (e.g. `architectural_pattern_validity` raw=`scaffold` →
final=`rope` on all 4 seats). This proves the raw≠final scan **can see what it reports**
without needing a planted overlay. ⟹ if a Phase-2 corpus scan of the *divergence*
population finds zero signature involvement, that is a **genuine absence** (scan powered),
and axis D drops with "unpowered on the divergence population at this regime" honestly. The
mismatch-axis silence (§2: 273/277 `none`) already suggests signatures barely touch the
divergence population — axis D is likely to drop, but the drop is now positive-controlled.

## 7. Answerability summary

- **Answerable now (JSON join):** G-class, pair×class, seat-shape, ε/d confound, silent-axis
  census. All in `recon_summary.json`.
- **Phase 2 (post-ratification, needs independent Prolog sourcing):** membership.tsv with
  author sourced by `narrative_ontology:constraint_claim/2` + engine by `dr_type/3`
  (control 6); the stratified hand-read; the planted-mountain / planted-ε-band controls.
- **Ω_C (what KIND each divergence is):** *not* settled by recon — it needs the hand-read,
  and only over the confound-free / confound-shown population. Recon has narrowed the live
  Ω_C surface to G-A kinds + G-B-cells-that-survive-the-confound.

## 8. Recon's steer to the reframe gate (evidence, not ruling)

Recon did **not** rule the reframe. It commits: confound-exposed share = **79.8%**; the
three "diagnostic-mode/author-mode/detector" axes the OQ proposed are degenerate; the live
Ω_C surface is smaller than the OQ assumed. Whether 79.8% clears the operator's X (reframe:
close on corrected primary cut + silence findings + residual OQ; vs full: hand-read the
confound-free G-A cells + confound-shown G-B cells) is escalation 2.
