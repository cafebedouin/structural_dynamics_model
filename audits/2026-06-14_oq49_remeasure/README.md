# OQ-49 re-measure — signature-override prevalence on the live corpora (post-reset, post-OQ-70)

**Date:** 2026-06-14 · **Branch:** `oq49-remeasure` · **Type:** read-only audit (no engine/corpus write)
**Plan:** `~/.claude/plans/review-oq-49-in-issues-md-twinkly-mochi.md`

## Question

OQ-49 asked whether the signature-override layer is *laundering* (metric read right, override
corrupts it → remove the clause) or *load-bearing* (metric read wrong, override corrects → keep).
Its headline was measured on `testsets_3000` (the archived `original_v6` chimera corpus):
**1730 effective overrides, 1730 confident-overwrites, 0 unknown-fills**, dominated by
`false_natural_law → tangled_rope` = **1661 (96%)**. It sat OPEN awaiting an (a)/(b) ruling.

The ruling was un-answerable as posed: the substrate is gone twice over — (1) `testsets_3000` is a
dead corpus (reset 2026-06-05); (2) the FNL driver was deleted (OQ-70, commit `72ec2cdd`, removed
`claimed_natural`'s bait source — the one that read any single authored mountain *perspective* as a
story-level *claim*, 827/1106 pre-reset firings). So we **re-measure on live** and disposition
under OQ-74's seat frame; we do **not** rule clauses or touch the engine.

## Method

`python/audits/oq49_override_remeasure.py` (read-only). One swipl process per corpus, corpus
overlaid with `asserta`. Per reading at the default analytical context:
`MT = drl_core:metric_based_type_indexed/3` (override suppressed), `FT = dr_type/3` (override
active), `Sig = constraint_signature/2`. Override *effective* iff `MT ≠ FT`. Every FNL firing
tagged source-1 (`constraint_claim(_,mountain)`) vs source-2 (`natural_law_signature` profile).

**Positive controls (per process, all passed):** `PC_CLAUSE878` — the FNL→TR override clause
(`signature_detection.pl:878`) computes a change; `PC_SOURCE1` — source-1 `claimed_natural` path
reachable (synthetic, retracted); `PC_LIVECHANGE` — N of MT≠FT registered (17 / 168 / 181). A
collapsed result is only a finding because the probe demonstrably *can* fire.

## Results

Load counts match the manifest: **57 / 960 / 960** (`corpus_constraint/1`).

| corpus | readings | FNL firings | FNL src split | FNL override-effective | total override-effective | confident / unknown-fill |
|---|---|---|---|---|---|---|
| testsets | 57 | 1 | source1:1 | **0** | 17 | 14 / 3 |
| testsets_haiku | 960 | 13 | source1:13 | **6** | 168 | 98 / 70 |
| testsets_flash | 960 | 8 | source1:8 | **8** | 181 | 167 / 14 |

**The collapse witness is structural, not numeric: every FNL firing across all three corpora tags
source-1.** Zero source-2, zero unaccounted (kill condition = any FNL firing tagged neither — not
triggered). The 827 bait-driven firings are gone *by construction*: the bait path is unreachable,
so FNL fires only via source-1 (explicit `constraint_claim(_,mountain)`) or source-2
(`natural_law_signature` profile). The raw "1661 → ≤8" drop is size-confounded (3380 vs 960) and is
**color, not the witness.**

**FNL is no longer the override layer's dominant effect.** The live override layer is led by
`false_ci_rope → tangled_rope` (scaffold→TR: 5 / 52 / 78) and `coupling_invariant_rope → rope`
(scaffold→rope: 3 / 18 / 34) — distinct signatures, not the OQ-49 FNL focus. The
`false_natural_law → tangled_rope` line is now 0 / 6 / 8 effective.

**Inert clauses (working-as-designed on live):** the `natural_law → mountain` clause
(`:867`) fires 0 times — `natural_law` appears in no signature distribution. The
`unknown, false_natural_law → unknown` clause (`:877`) fires 0 — every live FNL firing has a
definite metric (snare/scaffold), never unknown.

**Residual = FNL snare→TR overwrites (the pre-reset dominant case), now 0 / 4 / 4.**
`fnl_survivor_coordination.txt`: **8/8 of these carry both coordination AND asymmetric extraction**
(coord+asym). Under the pre-reset OQ-49 partition — coord=0 = clean laundering candidate;
coord+asym = override-supplies-omitted-structure (correction-leaning) — the **clean-laundering
coord=0 pure-extraction subset is EMPTY on live.** The human-stakes escalation the (a)/(b) ruling
was reserved for **dissolves to zero** on these corpora. The 8 correction-leaning readings are
two-seat signal (metric snare-clause is coordination-blind; FNL→TR supplies the omitted
coordination-awareness) — handed to **OQ-74**, not ruled here.

## Twin paired check (model-invariance + generator-convention signal)

`testsets_haiku` and `testsets_flash` are exact base-name mirrors (960/960), two models writing the
same story ids — a matched paired design.

- **Model-invariance (real second witness):** the FNL collapse holds on **both** twins — every FNL
  firing source-1, override-effective single digits. Robust to generator idiom.
- **Generator-convention signal (analogue, NOT a witness of the seat frame):** per-id
  override-effective sets diverge sharply — **81 shared, 87 haiku-only, 100 flash-only**
  (`eff_haiku.ids` / `eff_flash.ids`). This is two models authoring the same id differently
  (OQ-26/OQ-78-analog), *not* metric-vs-signature divergence within one reading. Logged as
  generator-convention signal; it does **not** stand in as evidence for the metric/signature seat
  split.

## Disposition (→ ISSUES.md split-close)

1. **Obsolete-prevalence half — CLOSE** on source-attribution (every live FNL firing source-1; the
   827 bait firings gone by construction). Raw count is color.
2. **Inert clauses — CLOSE** working-as-designed (`:867` natural_law→mountain 0 firings; `:877`
   FNL-unknown-fill 0).
3. **Residual — disposition, NOT ruled.** coord=0 clean-laundering subset empty; the 8 coord+asym
   FNL snare→TR overwrites are two-seat signal → **OQ-74**. Escalation dissolves to zero here.
4. **Hand-off.** Any witness-not-verdict engine change is OQ-74's gated pass, not OQ-49's. No clause
   removed or edited.

**Kill condition (not triggered):** FNL→TR did not stay dominant/confident; it collapsed. No engine
write performed.

## Artifacts

- `probe_output.txt` — full per-corpus probe report (the table above, raw).
- `probe_stderr.txt` — engine load chatter.
- `rows_{testsets,testsets_haiku,testsets_flash}.tsv` — raw per-reading rows
  (`ROW id MT FT Sig eff fnl_source`).
- `eff_haiku.ids` / `eff_flash.ids` — per-id override-effective sets (paired diff source).
- `fnl_survivor_coordination.txt` — coord/asym check of the 8 FNL snare→TR survivors.
- probe: `python/audits/oq49_override_remeasure.py`.
