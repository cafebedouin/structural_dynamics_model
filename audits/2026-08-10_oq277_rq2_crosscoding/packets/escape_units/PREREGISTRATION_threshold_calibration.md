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

### RETIRED (operator, 2026-08-11, at assembly, before any judging) — **not satisfied**

**Amendment 1 is RETIRED.** The operator is not the judge; the judge is a fresh instance with no
history of this arc. Amendment 1's entire content was *the operator judges, under three conditions*,
and with the operator out of the seat there is nothing left for the three conditions to govern.

This is the amendment's own declared fallback taken, and its declared cost comes with it:
**the same-family confound now applies to the calibration arm, and it is DECLARED, NOT REPAIRED.**
A fresh instance judging these units is a party from the same family as the party whose threshold is
under test. Amendment 1 priced this exactly ("a fresh instance is genuinely blind to them, and pays
for it in variance and in judging its own family") and no repair is available: the third convention
holder it would take does not exist. **Any outcome of this probe — AGREE or VOID — is read under
that confound.** Retirement is not satisfaction: nothing here shows the confound is absent or small.

Condition 3's substance survives on its own footing and is NOT retired with the rest: the judge is
still blind to composition and still judges before any §H.2 confirmation of the two drawn
candidates, which is what stops this pass anchoring that confirmation. It now holds by construction
(a fresh instance cannot know the composition) rather than by the operator's undertaking.

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

### RETIRED (operator, 2026-08-11, at assembly, before any judging) — **not satisfied**

**Amendment 3 is RETIRED, and the fifth item with it, together with the recognition flag it
controlled.** Ground: **the judge no longer has history.** Recognition was a risk only because the
operator had read the primary sample; a fresh instance has read none of it, so recognition is
impossible by construction and the control is **vacuous rather than merely expensive**. Retired on
that ground — not on cost, and not because the control was ever satisfied.

The assembler separately found the fifth item unbuildable as specified: extracting a fresh unit means
reading source prose, every tool result lands in the operator's terminal, so requirement 2 ("never
shown to the judge") was unachievable without a spend-go'd blind subagent. Recorded as the *weaker*
reason. The governing one is vacuity.

**What retirement does NOT close.** Amendment 3 existed to make *false* recognition checkable, and
its own text already declared that *silent* recognition — the direction that would actually make the
arm circular — stayed open. Both channels are now out of scope rather than closed: with no flags
collected, neither is measured. The sixth-item completing arm is moot and is not re-offered.

**Consequently there are no recognition flags in this pass**, and the fifth row of the pre-committed
outcome table ("recognition flagged on the unseen fifth item → arm VOID") **cannot fire**. It must
not be read as having passed: it is a row with no instrument behind it.

## Amendment 4 (extractor, 2026-08-11, before any run) — what the judge sees, pinned

Found while writing the assembler's instructions: the design above says *what is judged* and never
says *what is shown*, which leaves the assembler to decide it — and the choice changes the result.
Pinned now rather than at assembly time.

> **Amended 2026-08-11 at assembly:** "five items" below now reads **four**, Amendment 3 having been
> retired above. Nothing else in Amendment 4 changes — the pin on *what is shown* is what governed
> the assembly, and it is the reason §H.1 shipped redacted (see the assembly record).

**Shown, for every one of the ~~five~~ four items, identically:** the four coder-facing fields only —
`symptom`, `mechanism_as_described`, `detection_path`, `consequence` — verbatim, in that order,
under a neutral item number.

**Not shown, for any item:** `extraction_notes` (its `boundary_rule_applied` argues the case for
extraction and would supply the verdict it is meant to test), `metadata` (`incident_location` is one
of the two strata's matching keys), `source_dir`, `extractor`, `role`, `files_read`, filenames, and
field ordering or formatting that differs between items.

**The question put to the judge, fixed wording:** *does this meet the boundary rule's REPORTS
clause — extract or no-extract?* The written clause (§H.1) is supplied with the packet; nothing else
is.

Rationale, in one line each: showing the extractor's reasoning would test whether the judge finds
that reasoning persuasive rather than whether they share the threshold; showing `incident_location`
would hand over the stratification key; and any formatting difference between strata is a label the
stripping was supposed to remove.

## Queue for the assembler (the other instance)

1. **Primary-side draw** — two extracted primary units, matched on `incident_location` to the escape
   draws where available, mismatch declared if not, seed recorded before anything is shown.
2. ~~**Fifth item** — the unseen recognition control, per Amendment 3's four requirements.~~
   **STRUCK 2026-08-11 (operator), not merely superseded.** Amendment 3 is retired above; this line
   was stale text the retirement failed to strike, and the assembler surfaced the contradiction
   rather than picking a side. Struck explicitly here because a stale instruction sitting beside a
   live one is two copies parsing — Build Discipline Pattern 2, the same shape the §1 edit already
   corrected once this arc. **Do not build a fifth item.**
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

## Assembly record (assembler, 2026-08-11) — written before the packet was shown to anyone

Two operator clarifications were taken during assembly. Both sit inside this prereg's existing
grammar, so neither is an amendment: the retirements above, and the §H.1 redaction below.

### Queue 1 — primary-side draw

Seed **`20260811`**, method identical to the escape-side draw (`random.Random(SEED)`, sorted pool per
`incident_location`, `subject` first then `self_audit_subsection`), executed by
`packets/judging/draw_primary.py` and recorded before anything was assembled.

- **Location match: EXACT. Nothing to declare.** Both escape-side `incident_location` values are
  present in the primary stratum (`subject` n=17, `self_audit_subsection` n=5 across 26 units); the
  script fails closed rather than substituting, so no undeclared substitution is possible.
- **Rule 1 (known-positives) checked, not assumed:** all 26 pool members assert non-empty on all four
  coder-facing fields. There is no NO-UNIT in the pool that a draw could have landed on.

### Queue 3 — assembly, and the §H.1 collision found while doing it

Presentation order seed **`20260811`** (`random.Random(SEED).shuffle`) over a canonical
`(stratum, path)` sort. Packet, key, and verdict template are separate files; the key was committed
with the packet and before the packet was shown to any judge, both md5s recorded in `audit_log.md`.

**§H.1 could not ship fully verbatim.** Its text adjudicates one specifically named directory as a
UNIT — and that directory is **one of the two primary draws** — and it separately defines the
`incident_location` values, which Amendment 4 forbids showing. Verbatim shipping would have handed
the judge one calibration item's verdict plus the stratification key.

**Operator ruling: redact and declare** (2026-08-11). The boundary-rule blockquote and its "why this
line" rationale ship verbatim; the two offending paragraphs are omitted, **each marked in place with
its reason and the total count stated**, never silently shortened, with path and commit `4360fcdc` so
the full text is recoverable. The rejected alternative was re-drawing the primary side to dodge the
collision — that would have traded a declared redaction for an undeclared selection effect and broken
the no-re-roll rule. The packet also tells the judge that if the rule cannot be applied without the
omitted paragraph, they should say so rather than guess: that would be a finding about §H.1's
self-sufficiency, which Amendment 1 asserted when it argued a model judge was unnecessary.

**A residual label the stripping did not remove — declared, not repaired.** In the realized draw,
**total field length separates the two strata perfectly**: the two primaries total 1,420 and 1,529
characters, the two candidates 3,331 and 3,472, an 1,802-character gap with nothing between. Internal
formatting is uniform (no field in any item contains a newline) and a scan for lexical tells
(`escape`, `quarantin`, `candidate`, `no-unit`, `incident_location`, `self_audit`, …) returns **zero
hits in all four items**, so this is the only surviving discriminator. It cannot be repaired without
breaking Amendment 4's *verbatim* pin or re-rolling the draw. **Direction of the bias, stated because
it cuts the expensive way:** the candidates are the items supplying more described mechanism, and more
described mechanism makes `extract` easier to reach — so the residue pushes toward **AGREE**, the
outcome this probe treats as costly, not toward the cheap VOID. This upgrades the pre-registered limit
"length … uncontrolled" to *length is confounded with stratum in this specific draw*.

### RULING on the length tell (operator, 2026-08-11) — pre-registered BEFORE the pass

Derived from the licensing asymmetry already registered above; it adds no new asymmetry.

**What the tell actually gives the judge.** Two items at ~1,450 characters and two at ~3,400 lets a
judge partition the packet into two pairs **without reading a word**. It cannot tell which pair is
which — and it does not need to. Length correlates with described mechanism, described mechanism is
what the REPORTS clause turns on, so the longer pair reads as more clearly extractable. The
candidates are the longer pair. The tell therefore pushes toward candidates clearing the bar: the
**AGREE** direction, which is the outcome that opens the gate to §H.2.

**Why the existing control does not cover it.** The pre-registered re-fire — an AGREE owes a second
pass at a different presentation order — was designed against **order** effects. **A length tell
survives reordering intact.** Shipping without saying so would leave an AGREE looking better
controlled than it is.

**The ruling:**

| verdict | how the tell bears | licence |
|---|---|---|
| **VOID** | confound points the other way | **strengthened.** A void reached *despite* a confound pushing toward agreement is stronger evidence than the design anticipated. Stands at **k = 1**, as already written. |
| **AGREE** | confound points at it | **weakened. Does NOT license scoring at face value.** It ships as a weaker verdict than designed, and **the six candidates do NOT proceed individually to §H.2 on it alone.** |

If AGREE comes back, what additional evidence it would take is decided **then** — with the number in
hand and this limit already declared. That order is deliberate: the limit is pre-committed, the
remedy is not pre-guessed.

**Do not repair the tell.** Truncating or padding the fields would break Amendment 4's *verbatim*
pin, and that pin is load-bearing for a different reason than this confound is costly. The confound
is priced, not removed.

### Queue 4 — frame census

`frame_audit_prose_census.py` run at commit-time state; listing md5 `ecc91562…` **MATCH** against the
frozen manifest; two-sided control **GREEN**.

| population | seeable (≥1 `.md`, recursive) | unseeable (zero `.md`) |
|---|---|---|
| all 101 non-census dirs (the escape stratum's full frame) | 97 | 4 |
| the drawn escape sample | 6 | **2** |
| the drawn primary sample | 22 | **0** |

Partition check 97 + 4 = 101. Of the 97 seeable, 0 have `.md` totalling zero bytes.

**The instruction said "all 22 escape directories"; there is no such population, and this is a
correction rather than a completion.** The frozen frame draws **n_escape = 8** and **n_primary = 22**
(`frame/draw_sample.py`, `frame/sample.json`, seed `20260810`). The "22" is the primary draw. The
census over all 101 is a superset of any escape subset, so the number the frame audit wanted is
computed here rather than left open — and it is **2 of 8**, exactly the count the escape extractor
already reported from its own slice. There are no "other 14" escape directories to visit.

Two consequences for `FRAME_AUDIT_prose_visibility.md`, which assumed the larger population:

1. Its "the count must be done over all 22 drawn directories" resolves to **0 unseeable**, and
   **necessarily so**: the primary sample is drawn from the *incident-bearing* stratum, which is
   defined by a grep over `--include='*.md'`, so every member has an `.md` by construction. That
   arithmetic could not have come out any other way — a consistency check, not a discrimination
   check, and it licenses no reassurance.
2. The n it actually loosens is therefore **n=8, not n=22**. The 12.7% figure is §H.2's *primary-side
   false-positive* bound and is untouched by unseeable directories. The escape side's own licence is
   the ~31% recall bound at n=8, and with 2 of 8 out of frame the effective n is **6**, giving
   1 − 0.05^(1/6) ≈ **39.3%**. That is a **looser** bound, i.e. a null is *less* reassuring — the
   one-way direction the frame audit already declared. Moot while six candidates are proposed.

## Result — pass 1, 2026-08-11

Judge: a fresh instance, no history of this arc, blind to composition. Packet md5
`d72b28b4a99c4d755338784981d74e64`; verdicts committed at `32228a4a` **before** `KEY.md` was opened.
Primary-side seed `20260811`; presentation-order seed `20260811`; **location mismatch: none**.

| item | stratum | verdict |
|---|---|---|
| 1 | primary (known-positive) | extract |
| 2 | escape (candidate) | extract |
| 3 | primary (known-positive) | extract |
| 4 | escape (candidate) | extract |

Presentation order alternated `primary, escape, primary, escape` — so the length tell separated the
packet into `{1,3}` short and `{2,4}` long, exactly along the stratum boundary.

**Table row reached: row 1 — both primaries extract and both candidates extract → AGREE.** No
abstention, no refusal, every answer inside the grammar. The recognition row could not fire
(Amendment 3 retired). So the row is scored **AGREE**, and then immediately braked twice, both brakes
pre-registered before the pass:

1. **The model-judge clause.** One pass is a draw, not a measurement; an AGREE owes a re-fire at a
   different presentation order before it licenses scoring. **Not yet run.**
2. **The length-tell ruling.** AGREE was the direction the confound pointed. It does not license
   scoring at face value, and **the six candidates do not proceed individually to §H.2 on it alone.**
   Reordering does not cure this one, so brake 1 cannot discharge brake 2.

### The finding that matters more than the row — the calibration arm did not discriminate

**All four came back `extract`, and a response of all-`extract` passes the calibration arm by
construction.** The arm fails only if a primary returns `no-extract`. A judge with a permissive
threshold — one that extracts anything placed in front of it — passes the arm *and* extracts the
candidates, producing exactly this result. So this pass **cannot distinguish "the judge shares the
primary threshold" from "the judge extracts everything."** The AGREE is consistent with both, and the
data contain no evidence separating them.

This is not the design failing to anticipate a possible world: the prereg's fireability check named
the discriminating world ("a judge who extracts both primaries and declines a candidate as *a caveat,
not an instance*") and it was genuinely constructible. It is the **realized** response carrying no
discriminating information. The distinction matters — the falsifier was fireable, it just did not fire,
and that is a weaker result than a fired falsifier, not an equivalent one.

**Why it cannot be repaired from existing material — and this is structural.** The fix would be a
**known-negative**: an item the primary conventions ruled NO-UNIT, which a threshold-sharing judge
must decline. **There is none. §M records k = 0 across the full n=22 primary sample** — zero
operator-confirmed NO-UNIT directories. The calibration arm is therefore **one-sided by construction**,
and no re-draw, re-order, or re-fire can make it two-sided, because the material does not exist. A
two-sided arm would require minting a known-negative, which is a new spend and a new ruling.

**Same shape as §L.2, and the origin is again the population rather than the code.** A check whose
pass is guaranteed by the state of the sample it draws from is a definition restated. §L.2's vacuity
came from the frame's sampling rule; this one comes from the primary sample's realized k = 0. Nothing
was coded wrong in either. **Recorded as the arc's sixth instance**, on the operator's running count.

### What this pass does and does not license

- **Does:** the escape row is not VOID. Nothing here shows the two thresholds differ.
- **Does not:** license scoring the escape row, license any of the six candidates, or support any
  sentence about the keyword proxy's completeness or miss rate. It does not even establish that the
  judge applied a threshold, as against extracting by default.
- **Unchanged:** each of the six still needs §H.2 individually, and §H.2 is the operator reading the
  source directory. This probe was never able to confirm a candidate and did not.

**Open, with its graduation step named:** an AGREE that discriminates would need a packet containing at
least one item a threshold-sharing judge must decline. Until such an item exists, re-firing this packet
at a new order tests order sensitivity only — worth doing, but it cannot convert this AGREE into
evidence of threshold agreement. That decision is the operator's, with the number now in hand.

### One small item, under-claimed deliberately

Item 3 is the directory §H.1's redacted paragraph adjudicates as a UNIT. The blind judge, never shown
that paragraph, returned `extract` — the same verdict. **This is worth almost nothing as
corroboration:** the judge returned `extract` on all four, so agreement with §H.1 here is what a
judge extracting everything would produce anyway. Recorded so a later reader does not find the
coincidence and promote it.
