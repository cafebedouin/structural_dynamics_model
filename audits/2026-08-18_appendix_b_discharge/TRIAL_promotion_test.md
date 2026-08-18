# The §8.2 promotion-test trial — result, and the instrument incident inside it

**Executed:** 2026-08-18 · **OQ:** OQ-309 · **Prereg:** `PREREGISTRATION.md`, md5
`5f5785baf1c5f0d3786ec29e0a67d3f8`, frozen before any call.
**Slot filled:** `amnesiac_institution_v0_6.md:1635-1637`.

## Result

Two draws per arm (see *The incident* below for why there are two, which was not the design).

| draw | arm | goal as emitted | score |
|---|---|---|---|
| 1 | WITHOUT | `assertz(config:param(corpus_path,'testsets_flash')), …` | **MISTAKE-PREDICTED** |
| 1 | WITH | `retractall(config:param(corpus_path,_)), asserta(…), …` | CORRECT |
| 2 | WITHOUT | `asserta(config:param(corpus_path,'testsets_flash'))` | CORRECT |
| 2 | WITH | `asserta(config:param(corpus_path,'testsets_flash')), …` | CORRECT |

**The licensed statement.** *The promotion test is empirically runnable.* Handing a fresh instance
the files a tripwire names, with the tripwire withheld, produced the tripwire's predicted silent
mistake on **1 of 2** draws; with the tripwire supplied, on **0 of 2**. The rubric discriminates:
draw 1's two arms differ by exactly the tripwire paragraph and scored differently.

**What it is not.** n = 2 per arm is an **existence witness for the mechanism**, not a rate, and
certainly not an effect size. §8.2's declared gaps are unaffected: the missing **stakes term** —
the test weights frequency, not severity — is untouched by this and stays a design gap.

## The sharpest thing in the raw text, and it is not the score

Draw 1's WITHOUT arm **emitted the defective goal and then explained why it was defective**, in the
next paragraph:

> *"…if `config.pl` also defines `param(corpus_path, 'testsets')` as a static fact loaded via
> `use_module`, you'd need `asserta` to make the new one clause tried by first-solution semantics,
> or better, retract the old one explicitly with `retractall(config:param(corpus_path, _))` before
> asserting the new value, to avoid relying on clause-order accidents."*

The knowledge was present and did not reach the artifact. That is a **different** failure from the
one the tripwire is written against. The tripwire's model of the mistake is *the worker does not
know about clause order*; what happened here is *the worker knows, hedges in prose, and ships the
wrong goal anyway*. A promotion test scored on outcome catches both; a promotion test scored on
whether the instance "seems to know" would have scored this a pass. **Outcome-scoring is
load-bearing, and this trial is the reason to say so.**

## Prompt-suppression check (OQ-292)

The rubric requires an ABSENT/CORRECT score to be provisional until the prompt is checked for
wording that could produce the right answer without the instance reasoning to it. Run against the
verbatim WITHOUT prompt (`evidence/promotion_test_prompt_without.txt`):

```
asserta          hits=0
first clause     hits=0
first solution   hits=0
clause order     hits=0
ignored          hits=0
retractall       hits=2   <- both in corpus_loader.pl source, on loader_directory/1 and
                             corpus_constraint/1; neither concerns corpus_path
silently         hits=3   <- all three in corpus_loader.pl comments about failing closed
                             on a 0-file glob; none concerns clause order
```

The tripwire's own vocabulary is absent from the WITHOUT prompt. The two incidental `retractall`
occurrences are a real exposure and are declared: the technique is visible in the source even
though the reason to use it here is not.

## THE INCIDENT — the harness destroyed the data the harness exists to protect

**This was not the design. Draw 2 exists because of an error, and the error is the most instructive
thing this trial produced.**

The driver persists every raw response to disk **before parsing**, precisely so that a scorer defect
can be repaired by re-reading the artifact rather than by re-running the spend. That rule paid off
immediately: reading draw 1's WITHOUT response showed the scorer was matching `asserta`/`retractall`
anywhere in the **response** while the frozen rubric scores the **goal**, so the arm's prose caveat
inverted its own score (reported CORRECT; the goal was the predicted mistake).

The repair was attempted by **importing the driver module** to reuse its scorer. The module ended
with a bare `sys.exit(main())`, so the import **executed main()**, made two fresh API calls, and
**overwrote both persisted responses** with new draws. The harness built to re-read the raw datum
destroyed it.

**What was lost and how it was recovered.** Draw 1's two response files are gone from disk. Their
text had been printed verbatim to the session transcript before the overwrite and is restored from
there into `promotion_test_response_*_draw1.txt`. **Those two files are transcript-sourced
reconstructions, not file-persisted artifacts**, and are labelled as such wherever they are cited.
Draw 2's files are genuinely file-persisted.

**Three things are true at once and all three are recorded:**
1. The persist-before-parse rule was correct and was honoured by the driver.
2. It was defeated by the *re-scoring* step, which was not covered by it — the rule protects the
   write path and says nothing about who may import the writer.
3. The accident produced a *better-bounded* result (n=2 per arm instead of n=1), and that is
   **not** a justification. An accident laundered into a design is exactly the move this paper
   catalogues. It is reported as an accident.

**The fix, in the same change:** an entry-point guard on the driver (`if __name__ == "__main__"`),
and a separate `frame/score_only.py` that duplicates the rubric regexes rather than importing them
— a duplicate that cannot spend beats a shared import that can. Re-scoring is now structurally
incapable of making a call.

**Where this belongs in the taxonomy.** It is the third introduced-instrument defect in this pass
(after the line-wrapped absence probe and the no-op cadence filter), and the only one that destroyed
data. In `build_discipline.md`'s terms it is *An introduced instrument is itself a claim* with the
sharpest available instance: the instrument violated the specific rule it was written to honour, at
the one step that rule did not name.

## Evidence map

| artifact | what it is |
|---|---|
| `PREREGISTRATION.md` | frozen design, rubric, pre-committed readings |
| `frame/promotion_test_trial.py` | driver (now entry-point guarded; AMENDMENT 1 declared in source) |
| `frame/score_only.py` | non-spending re-scorer, with the arm-separation control |
| `evidence/promotion_test_prompt_without.txt` / `_with.txt` | verbatim prompts |
| `evidence/promotion_test_response_*_draw1.txt` | **transcript-restored**, not file-persisted |
| `evidence/promotion_test_response_*_draw2.txt` | file-persisted |
| `evidence/promotion_test_scores.txt` | scoring run under the amended rubric |
