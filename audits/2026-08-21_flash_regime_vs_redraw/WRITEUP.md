# flash regime-vs-redraw — on gemini-2.5-flash, thinking-on mostly ADDS DRAW VARIANCE; the replicated regime shift is a doubled red-verdict rate, and the regime contrast sits only a few points below the thinking-on redraw floor

**Executed:** 2026-08-21
**OQ:** none — operator-directed spend (2026-08-21 chat: "do whatever additional draws make sense") to build the within-model controls the five-leg comparison lacked; feeds OQ-78/OQ-281 (family/rail reading) and the AGENTS.md "dispositional, not capability" ruling, which rests on the thinking-regime confound this measures.
**Verdict:** On one model (gemini-2.5-flash) over the same 1005 seeds at one prompt/schema commit (685ed7cf), a thinking-off redraw agrees with itself at h1 85% / verdict 89% / signature 93% / authored ε 81%, a thinking-on redraw at 64 / 71 / 77 / 40%, and the off-vs-on contrast at 59 / 65 / 72 / 34% — so the regime's main effect is instability, not a directional move; the one marginal shift that replicates in BOTH thinking draws is red-verdict rate 6.5–6.8% → 13.4–14.3% (band-3 share 41–42% → 44–45%). Scope: one model, one budget (8192), k=2 per regime; the June `testsets_flash` leg is a different prompt/schema commit (2e9dff2f/22843cdf) AND possibly a different model snapshot, so flash-vs-flash2 (72–74% h1) measures schema-change + redraw, not redraw.
**Substrate:** `outputs/pipeline_output.{flash,flash2,flash3,flash_think,flash_think2}.json` via `classify_corpus(...,'gemini-2.5-flash')` — n 960 / 944 / 958 / 988 / 992, n_unclassified 0 each; code_commit f0ef08a (flash, flash2, flash_think) and bbce40f (flash3, flash_think2), code_dirty True (in-flight untracked legs). Engine-coherent: `git diff --name-only f0ef08a HEAD -- prolog/ | grep -E '^prolog/[^/]+\.pl$'` is empty, and the same grep over `c8baf01b~1..c8baf01b` lists `prolog/probe_harness.pl` (positive control).
**Fired:** live — the first read (3 legs, before the redraw floors existed) was headlined "thinking-on doubles the red rate with H¹ unchanged"; the floors flipped it: the thinking-on legs disagree with EACH OTHER almost as much as with the thinking-off leg (64% vs 59% h1), so the consumer-visible claim changed from "regime effect" to "regime-induced variance + a small replicated marginal shift".
**Evidence map:**
- `paired_agreement.py` — the read: per-leg marginals + pairwise same-seed agreement on h1_band, verdict_join, signature, purity_band, authored claimed_type and ε; join key = filename id (GAP-35).
- `paired_agreement_3leg_*.txt` — the pre-floor 3-leg output (flash, flash2, flash_think); the superseded headline.
- `paired_agreement_5leg.txt` — the 5-leg output this verdict cites.

## What was built (all gemini-2.5-flash, seed pool `prolog/kernels/rebuild_2026-06-13/never_generated_seeds.json`, driver `agent/run_no_scope_gemini.py --leg-suffix … [--thinking-budget 8192]`)

| leg | regime | n | passes | residue |
|---|---|---|---|---|
| testsets_flash2 | budget 0 | 944 | 851 → 924 → 944 | seed-persistent `'stakeholders' is a required property` |
| testsets_flash3 | budget 0 | 958 | 896 → 943 → 958 | same |
| testsets_flash_think | budget 8192 | 988 | 856 → 974 → 988 | same |
| testsets_flash_think2 | budget 8192 | 992 | 893 → 981 → 992 | same |

Cost (batch halves of the driver's interactive figure): ≈ $17 / $17 / $15 / $17.

## Three incidental findings, each witnessed in the run logs

1. **[CORRECTED 2026-08-22 — see KNOWN_STATE 2026-08-22: the field was present; `story_repair.py` deleted it. The figures below are repair artifacts, not omission rates.]** ~~Flash omits the schema-required `stakeholders` field in roughly a third of draws~~ under the 685ed7cf schema (the gate landed `becd0f87`, 2026-06-19, AFTER the June Flash leg). Sonnet (1001/1005) and kimi (1005/1005) passed the same schema. A Gemini-specific prompt-robustness defect; the legs top out at 94–99% because of it.
2. **Attempt 2 of both first passes errored 100% at the batch layer** (272/272, 451/451) and attempt 3 recovered ~40%; the driver recorded no error text. `run_no_scope_gemini` now prints `batch row error: …` (`ef897ae9`); no recurrence in the four reruns since.
3. **Thinking-on Flash is not just noisier — its authored ε is nearly a coin-flip across draws** (40% exact agreement vs 81% thinking-off), which bears directly on the ε-rail reading (OQ-78): a rail measured on a thinking-on leg is measured against a much wider per-story spread.

## What it does NOT license
- Nothing about other models: k=2 per regime on one model. The stealth (`ox-alpha`, reasoning mandatory) and kimi-k2.6 legs are thinking-on legs with NO off arm; the kimi-off arm is constructible (k2.6 accepts `thinking:{type:disabled}`, INVESTIGATIONS 2026-08-21) and is the next regime pair.
- Nothing about capability (OQ-228 / GAP-25 stand).
- flash vs flash2/3 is NOT a redraw floor — it is redraw + schema change (+ possible model-snapshot drift); cite flash2 vs flash3 for the floor.

## Addendum 2026-08-22 — |Δε|, not just exact agreement (operator pushback: "the whole thing rests on ε")

`epsilon_distance.py` → `epsilon_distance_2026-08-22.txt`. Engine ε bands: `tangled_rope` floor
0.30, `rope` ceiling 0.45, `snare` floor 0.46, `piton` floor 0.10 — adjacent bands ~0.15 apart.

| pair | median |Δε| | p75 | p90 | share ≥0.10 | share ≥0.20 |
|---|---|---|---|---|---|
| flash2 vs flash3 (thinking-off floor) | 0.00 | 0.00 | 0.05 | 6% | 2% |
| flash_think vs flash_think2 (thinking-on floor) | 0.03 | 0.10 | 0.20 | 22% | 10% |
| flash2 vs flash_think (regime) | 0.05 | 0.13 | 0.23 | 34% | 15% |
| stealth vs nemotron (cross-model, for scale) | 0.08 | 0.17 | 0.32 | 43% | 21% |

**Correction to this WRITEUP's own verdict language.** "Mostly adds draw variance" described the
*shape* (no directional move) and under-stated the *size*: thinking-on moves the engine's primary
input by a band-crossing amount (≥0.10) on about a fifth of seeds, and by ≥0.20 on a tenth, where
a thinking-off redraw pins ε to the hundredth on 94% of seeds. Since χ = ε·f(d)·σ(S), this is
instability in the one authored quantity every downstream classification multiplies through —
not a side effect. Fired: live still (the verdict sharpened against the author).
