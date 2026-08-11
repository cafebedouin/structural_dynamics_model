# PREREGISTRATION — threshold-comparability probe for the escape stratum

**Written:** 2026-08-11, by the escape extractor, **before either party has seen any result.**
**Authority:** operator ruling 2026-08-11 — §H.2 confirmation WITHHELD on the current evidence;
the escape row is not scored until this probe runs.
**Scope note:** this file pre-registers ONE instrument. It is not the arc prereg, which is the
other instance's (`PREREGISTRATION.md` at the audit root, unwritten as of this file). Placed inside
`packets/escape_units/` to avoid forking that name.

## What is under test — and what is not

**Under test:** whether the escape extractor and the primary-convention holder apply the boundary
rule's REPORTS clause at the same threshold.

**NOT under test:** whether any of the six candidates is a real hit. This probe cannot confirm one.
Confirmation is §H.2 — the operator reading the source directory — and remains per candidate.

The reason the probe is aimed at the threshold rather than at the incidents: judging only the six,
each taken alone, invites a yes on each one. Mixing in units already extracted under the primary
conventions calibrates the judge, because the primaries' verdicts say what the judge's threshold is
before their candidate verdicts say anything about the candidates.

## The judgement set — 2 + 2, blind

Four units, presented in one pass, labels stripped, identical instructions, judged
**extract / no-extract**. The judge is not told which are which, how many of each there are, or
that two strata exist.

### Escape side — drawn now, executed, not re-rollable

Stratified by `incident_location`, because placement is the confound most likely to masquerade as
threshold: an incident reported in a self-audit subsection may read softer than one that is the
document's subject, and an unstratified draw could return two of the same kind and confound the two.
The six split 3 `subject` / 3 `self_audit_subsection`. One drawn from each:

```python
import random
subject    = ["02_authoring_closure_fabricated_defaults", "05_oq46_close", "06_oq98_verdict_join"]
self_audit = ["01_spectral_laplacian", "04_signature_liveness_crosscorpus", "07_oq197_source_h1_crosstab"]
r = random.Random(20260811)
a = r.choice(sorted(subject))      # -> 02_authoring_closure_fabricated_defaults
b = r.choice(sorted(self_audit))   # -> 01_spectral_laplacian
```

**Drawn: `02_authoring_closure_fabricated_defaults` (subject) and `01_spectral_laplacian`
(self_audit_subsection).** Executed at write time and pasted here so the draw cannot be re-rolled
after anyone dislikes a verdict.

### Primary side — drawn by the party who can see it, NOT by me

I must not select these; I have not seen the primary sample and must not during this probe. Rule
for whoever assembles the packet:

1. Both must be units that WERE extracted under the primary conventions — known-positives. A
   primary NO-UNIT in this slot destroys the calibration arm.
2. Match the two escape draws on `incident_location` where the primary stratum contains that value
   (`subject` and `self_audit_subsection`). If a value is unavailable, substitute the nearest
   available and **declare the mismatch in the result** — an undeclared substitution voids the pass.
3. Draw with a stated seed, recorded before the judge sees anything.

## Pre-committed outcome semantics — fixed now, not after

| result | reading | consequence |
|---|---|---|
| both primaries extract **and** both candidates extract | thresholds agree on this evidence | the six proceed individually to §H.2. This licenses scoring the row, not any candidate. |
| both primaries extract **and** ≥1 candidate no-extract | thresholds differ; the strata are not comparable | **the escape row is VOID, not positive** (operator ruling) |
| ≥1 primary no-extract | the judge is not applying the primary conventions — instrument failure | discard **all four** verdicts, including the candidates'. A candidate verdict from a pass whose calibration arm failed is not evidence in either direction. Re-run with a different holder. |
| abstention, refusal, or any answer not of the form extract/no-extract | inconclusive | void |
| recognition flagged on the unseen fifth item (Amendment 3) | the recognition instrument reports false positives | **arm VOID regardless of all four verdicts** — this row overrides every row above it |

**Fireability check** (a kill condition that cannot fire is hedging in a falsifier's costume — each
row needs a possible world): *agree* — a judge who extracts all four; *differ* — a judge who
extracts both primaries and declines a candidate as "a caveat, not an instance"; *instrument
failure* — a judge who declines a primary. All three are constructible. No row is definitionally
unreachable.

## Declared limits — read these before quoting any outcome

- **n = 4 verdicts.** This detects a gross threshold mismatch. It estimates no rate, and no
  miss-rate sentence may be written from it.
- **Asymmetry is deliberate and one-directional.** A single candidate disagreement voids the row.
  That makes the probe conservative toward VOID and never toward POSITIVE — so a void is cheap and
  an agreement is not. Do not read a void as evidence that the keyword proxy is fine; it is
  evidence about comparability only.
- **Agreement on two does not transfer to the other four.** Each of the six still needs §H.2.
- **Stratification controls placement only.** Length, domain familiarity, and whether the incident
  is the document's headline are uncontrolled and could each move a verdict.
- **Independence is partial.** The judge holds the conventions under test — a party judging its own
  threshold. Declared, not repaired; I can see no way to repair it without a third convention holder
  who does not exist.
- **If the judge is a model instance, one pass is a draw, not a measurement.** Pre-committed
  asymmetry: a **VOID** verdict stands at k=1 (conservative direction); an **AGREE** verdict must be
  re-fired at least once, with a different presentation order, before it licenses scoring.

## Execution conditions

- **No model call may be made without the operator's explicit spend-go.** The escape extractor will
  not make it. If the judge is a live instance, this probe is a spend and waits at the same gate as
  the rest of the arc.
- Packet assembly is the other instance's. I do not see the primary units at any point, including
  during this probe.
- Result lands as a dated section appended to this file, with the primary-side seed, the presentation
  order, any declared location mismatch, and all four verdicts recorded together.

## Amendment 1 (2026-08-11, BEFORE any run) — the judge does not have to be a model

Operator question: is a live instance required because the conventions are tacit, or are they
written? **They are written**, so a model judge is not required on the knowledge axis.

- The boundary rule was fixed in writing before extraction (`verdict_grammar_amendment.md` §H.1,
  "Boundary rule — fixed NOW, before the remaining units are extracted"), and my own handoff carries
  it verbatim along with the selection rule (fullest described mechanism, alternatives recorded) and
  the redaction rule. A judge needs the written clause and four short units; nothing tacit is in
  play. I established this from section headers and my own handoff — I did not read, and did not
  need to read, any primary unit.
- **The binding axis is recognition, not knowledge, and it is the one that argues for care.** An
  operator judge removes model variance, the spend, and the same-family confound outright. It adds
  one risk a fresh instance does not carry: the operator has read the primary sample and may
  **recognize** the two primaries under stripped labels — at which point the calibration arm is
  circular, because the primaries come back extract for being known primaries rather than for
  clearing a threshold. A fresh instance is genuinely blind to them, and pays for it in variance
  and in judging its own family.

**Recommendation: the operator judges, under three conditions.** All three are pre-committed here,
before any item is assembled.

1. **The other instance assembles and holds the key.** It draws the primary side, strips labels,
   randomizes presentation order with a stated seed, and does not tell the judge the composition —
   not the assignment, not the 2+2 split, not that two strata exist.
2. **A recognition flag per item, recorded WITH the verdict and BEFORE the key is revealed:** *did
   you recognize this item's provenance?* Pre-commit: **any item flagged recognized voids the
   calibration arm**, and that pass falls back to a fresh instance, at which point spend-go applies.
   This is a self-report where no witness is available, and it is admissible only because it errs
   safe — it can void a pass, never validate one.
3. **Judge before any §H.2 confirmation of the two drawn candidates.** Blindness protects the
   confirmation as well as the probe: while blind, the judge cannot know which two items are escape
   candidates, so this pass cannot anchor the later confirmation of those same two. Reveal the key
   only after all four verdicts and all four recognition flags are written down.

If any condition cannot hold, the fallback is a fresh instance plus spend-go, and the same-family
confound is **declared, not repaired**.

**Accepted by the operator 2026-08-11, all three conditions as written.** Condition 3 is recorded at
the operator's instruction as a design contribution rather than a side effect: judging while blind
to composition is what prevents this probe from anchoring the later §H.2 confirmation of the same
two candidates, closing a circularity the confirmation would otherwise have walked into.

## Amendment 3 (operator, 2026-08-11, still before any run) — the recognition flag gets its own control

The recognition flag is a self-report from the party it constrains — the same structure as the
extractor's declared-slip problem, where the fix was to make the assurance *checkable* rather than
to trust it. So:

**A fifth item is added, drawn from NEITHER stratum: a unit the judge demonstrably has not seen.**
Pre-committed: **if the judge flags recognition on it, the recognition reporting is miscalibrated
and the arm VOIDS regardless of what was said about the other four.** One extra item converts an
assurance into a check.

Assembler requirements, so "demonstrably unseen" is a fact rather than a hope:

1. **Freshly extracted after this ruling**, from a directory in neither the primary sample nor the
   escape sample, by a party that is not the judge. Freshness is what makes unseen-ness
   demonstrable: the item did not exist when the judge last read anything.
2. **Never shown to the judge before the pass**, and its identity held with the key.
3. **In distribution with the other four** — same format, same redaction, comparable length. An item
   that visibly stands out gets flagged as *unfamiliar* rather than *recognized*, and a null
   recognition on an obvious outlier witnesses nothing.
4. **Its extract/no-extract verdict is NOT scored.** It comes from neither stratum, so it carries no
   threshold information, and it must not enter the 2+2 arithmetic. It gates the arm; it is not
   evidence in it.

**The exact limit of what this closes, stated so nobody overreads it.** The fifth item makes
**false recognition** checkable — flagging something never seen. It does **not** make **silent
recognition** checkable — failing to flag something that was in fact recognized, which is the
direction that would actually make the calibration arm circular. The control closes one channel and
the confound relocates to the other.

**The completing arm, offered for a ruling and not assumed:** a **sixth** item that the judge
demonstrably HAS seen recently and would be expected to recognize, with the pre-commitment that
failing to flag it means the recognition instrument under-reports and the arm voids on that ground.
That is the two-sided version. Cost: one more item, and the judge knowing that recognition is under
test in both directions (unavoidable — they designed it; what stays hidden is *which* item is which,
not *that* controls exist). **Not adopted unless the operator rules it in; the five-item design
stands as pre-registered either way.**

## Queue for the assembler (the other instance)

1. **Primary-side draw** — two extracted primary units, matched on `incident_location` to the escape
   draws where available, mismatch declared if not, seed recorded before anything is shown.
2. **Fifth item** — the unseen recognition control, per Amendment 3's four requirements.
3. **Assembly** — strip labels, randomize order with a stated seed, hold the key, tell the judge
   nothing about composition.
4. **Frame census across all 22 escape directories** — `frame_audit_prose_census.py` in this
   directory, run against the full escape sample. Not extrapolated from the extractor's slice, which
   drew 2 of the 4 unseeable directories at p = 0.031.

## Amendment 2 (2026-08-11, same turn) — what the frame audit changed under this probe

`FRAME_AUDIT_prose_visibility.md` (this directory) removes 4 unseeable directories from the frame,
two of which are in my sample and are exactly my two NO-UNITs. **In-frame, the escape result is 6
candidates from 6 directories, not 6 from 8.** This does not change the probe's design, its draw, or
its outcome semantics. It changes the stakes: there is no longer a NO-UNIT in my slice that could be
read as the threshold behaving conservatively somewhere.

## Result

*(empty — nothing has been run. Do not fill this in from a pass whose calibration arm failed, or
from a pass in which any item was flagged recognized.)*
