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

## Amendment 2 (2026-08-11, same turn) — what the frame audit changed under this probe

`FRAME_AUDIT_prose_visibility.md` (this directory) removes 4 unseeable directories from the frame,
two of which are in my sample and are exactly my two NO-UNITs. **In-frame, the escape result is 6
candidates from 6 directories, not 6 from 8.** This does not change the probe's design, its draw, or
its outcome semantics. It changes the stakes: there is no longer a NO-UNIT in my slice that could be
read as the threshold behaving conservatively somewhere.

## Result

*(empty — nothing has been run. Do not fill this in from a pass whose calibration arm failed, or
from a pass in which any item was flagged recognized.)*
