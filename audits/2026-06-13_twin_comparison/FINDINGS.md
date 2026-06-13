# Twin-model comparison — FINDINGS (2026-06-13)

Adjudication of `RESULTS.md` against the pre-registered H1/H2 rules. Both twins
classified at commit `8126231`, 960 stories each, schema_version 2, full id
intersection n=960 (twins are name-identical by construction). Permutations N=1000,
seed 20260613. The classification driver's provenance fingerprint confirmed
haiku→`claude-haiku-4-5-20251001`, flash→`gemini-2.5-flash` (no swap).

## H1 — structural type is model-invariant (per-field; NO aggregate claim)

**All 7 structural fields HOLD**: each field's Wilson-95% lower bound exceeds its own
permute band (95th pct). The same constraint-id is typed the same across haiku and
flash markedly more often than chance re-pairing would produce — structural
classification carries a model-invariant signal, field by field.

This is **not** an aggregate "the engine is model-invariant" claim. The six type
fields are non-independent (`verdict_join` folds the perspectives via
`compute_verdict/4`), so verdict-agreement and perspective-agreement are the same
signal reported at two altitudes, never two confirmations.

**Seat gradient (a hypothesis the shape generates, not a settled measurement):**

| field | rate | margin over band95 |
|---|---|---|
| `verdict` | 0.749 | +0.081 |
| `claimed_type` | 0.721 | +0.336 |
| `signature` | 0.722 | +0.219 |
| `persp:institutional` | 0.672 | **+0.032 (narrowest)** |
| `persp:analytical` | 0.610 | +0.189 |
| `persp:moderate` | 0.577 | +0.201 |
| `persp:powerless` | 0.397 | **+0.058 (lowest rate)** |

The **powerless seat is the most model-sensitive** (rate 0.397 — the two models
disagree on the powerless reading more often than they agree). The **institutional
seat has the highest raw agreement (0.672) but the narrowest margin** over chance
(its chance band is also high, 0.610) — institutional typing is the least
*discriminating* invariance even though it looks the most agreed. Both are worth a
seated follow-up; neither is a closed result on one witness (under-claim per the
synthesis-side discipline: one twin pair earns "separable here," not "orthogonal
everywhere").

**A model-characteristic signature lean (hypothesis):** the recurring `signature`
disparity is `constructed_high_extraction` (haiku) ↔ `false_ci_rope` (flash), in both
directions across many ids. The two models systematically foreground different
structural codings of the same substrate. Per OQ-70 this is STRUCTURAL-coding
disagreement, **not** a detection claim — signature prevalence is bait-confounded.

## H2 — continuous drift

**Pre-registered literal rule (H2 holds iff observed mean|Δ| > band95): NOT met for
any of the 5 continuous fields.** There is no anti-dispersion — true haiku/flash pairs
are not MORE different than random re-pairings.

**The opposite tail fired for all 5 fields: observed mean|Δ| < band5.** True pairs are
*more similar* than chance re-pairing — the continuous values (`theater_ratio`, the four
perspective χ) **track the constraint, not the generating model.** This is the natural
invariance reading, pre-registered as the reported alternative tail. Committing to it
(a kill condition existed and was run — the permutation band — so this is COMMIT, not
hedge): the continuous surface is model-invariant in the paired-similarity sense.

`chi:institutional` is the weakest continuous invariance (obs 0.0746 vs band5 0.0793 —
just inside the tail, band 0.0793–0.0802 is very tight): the institutional χ is close to
chance, echoing the institutional seat's narrow structural margin above.

## What this is and is not

- **Is:** evidence that, for this twin pair at this commit, engine classification of
  independently model-generated stories of the same seeds is largely shared, with a
  seat-localized model-sensitivity (powerless seat) and a model-characteristic signature
  lean. The product is the SHAPE.
- **Is not:** a re-measurement. A haiku draw and a flash draw of one seed are two
  different draws on two seats (CLAUDE.md determinism-frontier ruling); a per-field
  disparity can be the engine correctly typing two genuinely different authored
  situations. Disparity counts are substrate for hypotheses, not error counts.
- **Headline verdict** is `verdict_join.verdict` only (OQ-98). **Signature** agreement
  is structural coding, not detection (OQ-70). Paired claims are over the matched
  intersection only; runs are serial (OQ-77).

## Reproduce

```
# both twins, serial, one commit:
python3 -c "import sys; sys.path.insert(0,'python'); from run_pipeline import classify_corpus; \
  classify_corpus('testsets_haiku','pipeline_output.haiku.json','claude-haiku-4-5'); \
  classify_corpus('testsets_flash','pipeline_output.flash.json','gemini-2.5-flash')"
python3 python/audits/twin_comparison.py \
  --twin haiku=outputs/pipeline_output.haiku.json \
  --twin flash=outputs/pipeline_output.flash.json \
  --essay mixed=outputs/pipeline_output.json --permute 1000
```
