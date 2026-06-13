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

## H2 — continuous drift (disposition: FALSIFIED-for-drift; tail exploratory)

**This disposition is a discretionary call and is surfaced as such.** The pre-registered
test produced a result the committed rule did not name a confirmatory disposition for, so
how to report the tail was a judgment, not a mechanical read. An earlier draft of this
file folded it in as "invariance fired for all 5 fields" — that let a *failed* test wear
the credibility of a passed one (5/5 reads like clean convergence). The disposition below
is what `PRE_REGISTRATION.md` actually entitles.

The pre-registered statistic tested a **single direction**: observed paired-Δ spread
*exceeds* the permuted-Δ band ⇒ drift. That test **FAILED for all 5 fields** (observed did
not exceed the band). The only other named disposition is OPEN (statistic not stood up).
Per the committed rule, the licensed disposition is **H2-drift FALSIFIED**, with the
residual marked OPEN — **not** "invariance confirmed."

Observed Δ fell *below* the band for all 5 fields, which is consistent with
continuous-metric invariance. But the lower tail was pre-registered only to be **reported**
(`PRE_REGISTRATION.md` H2: "RESULTS.md reports observed against BOTH tails and states which
fired") — it carries **no pre-committed falsifier**. It is therefore an **exploratory
finding requiring its own registered test on a fresh corpus** before it counts as a result.
It is **not** a second confirmation alongside H1.

**[EDGE] The invariance tail may not be independent of H1.** `perspective_chi` feeds the
structural classification (the dual-threshold χ AND ε path), so type is partly a *function*
of χ. If structural type is invariant (H1 holds), the χ values that determine it are
mechanically dragged toward non-random paired-similarity — the H2 lower tail could be
**partly entailed by H1 rather than separate evidence about models.** The uniform 5/5 is
exactly the pattern a near-tautology against a loose threshold would produce. Before the
invariance tail counts as evidence *about models*, the H1-entailed portion must be carved
out (e.g. a registered test that residualizes χ against type, or conditions on
H1-disagreeing ids). Until then it is reported, not claimed.

`chi:institutional` is the field closest to chance (obs 0.0746 vs a very tight band
0.0793–0.0802) — i.e. its below-tail margin is the slimmest, echoing the institutional
seat's narrow structural margin in H1. (Descriptive only; same exploratory caveat.)

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

## Validity confirmations (substrate-witnessed 2026-06-13)

- **Part A is irrelevant to this comparison — WITNESSED, not inferred.** Census of every
  authored `cs_axiom_status` across both twin corpora: `holdable` ×3885, `overridden` ×55,
  nothing else. Positive control: the same grep pattern *does* flag `foreclosed`/`banana`
  when seeded into a test file, so the empty out-of-enum result is a real absence, not a
  blind grep. The Fix A repair path therefore could not have altered either built corpus.
- **The tree moving under the run did not contaminate the classify path.** `34481f4b`
  (the concurrent OQ-122 commit that landed mid-session, and an ancestor of the classify
  commit `8126231`) touched only `ISSUES.md` and an audit directory — **zero classify-path
  files** (corpus_loader / json_report / diagnostic_summary / cs_axiom_engine / drl_core /
  config). Both twins were classified at `8126231` with HEAD not moving between the two
  runs, so model-difference is not aliased onto code-difference.
- **Inertness diff normalization was field-surgical.** Exactly four named manifest keys
  were popped (`pipeline_run_at`, `code_commit`, `code_commit_short`, `code_dirty`); no
  block/range normalization, so the control was not blinded to data adjacent to the commit
  field.

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
