# PREREGISTRATION — the §8.2 promotion-test trial

**Frozen:** 2026-08-18, before any model call. **OQ:** OQ-309.
**Slot filled:** `amnesiac_institution_v0_6.md:1635-1637` — §8.2's *"a cheap improvement available
now is to run it empirically — hand a fresh instance the files without the entry and see whether the
predicted silent mistake occurs."* This is the **one** `[UNWITNESSED]` slot this pass fills. The
`:1893` standing-gate catch series is a DIFFERENT slot (§10.4, cited there as RQ-c and reachable
from RQ4's method) and is **not** discharged here.

## What is being tested, and what is not

**Tested:** whether the promotion test is *empirically runnable* — whether one can hand a fresh
instance the files a promoted tripwire names, withhold the tripwire, and observe whether the
predicted silent mistake occurs. **n = 1. This is an existence witness, not a rate.**

**Not tested:** the promotion test's missing **stakes term**. §8.2 declares two gaps — the test
weights frequency rather than severity, and the test is evaluated by operator judgment rather than
by simulation. This trial addresses only the second. The stakes term stays a declared design gap
and its `[UNWITNESSED]` status is unaffected.

## The tripwire under test

CLAUDE.md → *Corpus Loading*: **overlay `config:param(corpus_path, …)` with `asserta`, never plain
`assertz`.** `config.pl:489` defines `param(corpus_path, testsets)` as the FIRST clause and the
loader takes the first solution, so a plain `assertz` appends *after* the default and is silently
ignored — the default leg loads, and the count looks successful. Witnessed 2026-06-13: a
twin-corpus overlay loaded 44 stories instead of 960 with no error.

This tripwire is chosen because its predicted mistake is **crisp, silent, and checkable from the
output text alone**: the wrong goal is a one-token difference from the right one, and the failure
produces a plausible number rather than an error.

## Arms

**Arm WITHOUT (the trial).** A fresh instance receives: the task, the full text of
`prolog/corpus_loader.pl`, and the `corpus_path`-relevant excerpt of `prolog/config.pl` **including
line 489's default clause**. It receives **no** CLAUDE.md, no KNOWN_STATE.md, no
`swipl_load_path_and_probe_gotchas.md`, and no sentence anywhere in the prompt that mentions
`asserta`, clause order, first-solution semantics, or the 2026-06-13 incident. The files are the
substrate; the tripwire is withheld. This is exactly §8.2's *"the files without the entry."*

**Arm WITH (the control side).** Identical prompt plus the CLAUDE.md tripwire paragraph verbatim.
Run second. Its purpose is the DECLINE side: if the WITH arm also makes the mistake, the trial
witnesses nothing about the tripwire — it witnesses that the task is hard. A one-armed trial here
would be exactly the one-sided control §7.3 refuses.

## Scoring — fixed before the run, applied to the response text

The instance is asked to emit the exact Prolog goal it would run. The goal is scored on how it
installs the overlay:

| score | condition | reading |
|---|---|---|
| **MISTAKE-PREDICTED** | uses `assertz`/`assert` on `config:param(corpus_path, _)` with no preceding `retractall` and no `asserta` | the predicted silent mistake occurred |
| **CORRECT** | uses `asserta`, or `retractall(config:param(corpus_path,_))` before asserting | the mistake did not occur |
| **MISTAKE-OTHER** | any other defect (wrong module qualifier, edits `config.pl` on disk, never loads the corpus, uses a path that cannot resolve) | a different mistake — recorded, not scored as the prediction |
| **NO-GOAL** | no runnable goal emitted | unscoreable; the trial is void and is re-run with the task made concrete |

**Two-sided by construction:** MISTAKE-PREDICTED and CORRECT are both reachable from the same
prompt, and the WITH arm supplies the decline side at the arm level.

## Pre-committed readings

- **WITHOUT = MISTAKE-PREDICTED and WITH = CORRECT** → the promotion test is empirically runnable
  and this tripwire's predicted mistake reproduced. The strongest available n=1 result.
- **WITHOUT = CORRECT** → the predicted mistake did not occur on this draw. This does **not**
  refute the tripwire (n=1, and the model may have read `config.pl:489` carefully). It is reported
  as a decline, and per **OQ-292** the prompt is inspected for wording that could have prompted the
  correct answer without the instance reasoning to it.
- **Both arms MISTAKE-PREDICTED** → the tripwire text does not prevent the mistake at the point of
  use. That is a finding about the tripwire, and it is reportable.
- **WITHOUT = MISTAKE-OTHER** → recorded verbatim; the predicted mistake is scored ABSENT, and the
  ABSENT score is provisional under OQ-292 until the prompt is checked for suppression.

## Sampling and reproducibility

Model `claude-sonnet-5` via `agent/llm_call.py` `call()` (the canonical path; Sonnet 5 rejects
non-default `temperature` and runs adaptive thinking when the field is omitted, so
`sampling_overrides` pins thinking off). **Generation is stochastic and not pinnable** — no seed,
no temperature. Therefore the Appendix B row this produces regenerates the **TRIAL**, not the
**VALUE**: a re-run may legitimately land on a different score without falsifying the dated
existence witness. Every other row in Appendix B is the opposite — re-running reproduces the number.
That asymmetry is stated in the row itself so a future divergent re-run reads as expected behaviour
rather than as manifest failure.

Prompt text is saved verbatim to `evidence/promotion_test_prompt_without.txt` and
`evidence/promotion_test_prompt_with.txt`; raw responses to `evidence/promotion_test_response_*.txt`
**before** any parsing or scoring (gate the output, not only the input).

## Cost

Two calls. No fixtures, no sweep.
