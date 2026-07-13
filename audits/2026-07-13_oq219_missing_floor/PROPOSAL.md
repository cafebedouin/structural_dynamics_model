# OQ-219 — PROPOSAL (pre-registration; authored before any graded run)

**Date:** 2026-07-13. **Spend-go:** NOT yet granted. This pre-registration is the
$0-spend deliverable; it is committed **before** the Step-0 triage (≈$0.03/source) and
the two graded runs, all of which are HELD for the operator's spend-go
(`blocked_on_human operator-spend-go`, OQ-219 `ISSUES.md`). The `#6` free-read gate is
already discharged (positive prior, recorded in the OQ-219 entry / the review plan); the
actual spend-go remains the operator's seat.

**Entry:** OQ-219 (`ISSUES.md`, "R14 missing-floor / Detector-B load-bearing probe").
**Sibling template:** OQ-218 `audits/2026-07-12_oq218_scored_snare/PROPOSAL.md`.
**Discipline carried verbatim:** adjudicate **per leg; do not average** (OQ-218); improver
≠ blind reader; arms = fresh instances, ≥2 model families, identical-escape payloads,
grep-adjudicated claims; pipeline stages stay on production defaults (Sonnet-5), no
`--<stage>-model` flags (avoids the R12 confound).

---

## Question

R14's floor-contract — the Detector-B "missing floor" invariant that Stage 0 authors into
`<invariant_contract><missing_floor>` and Stage 2/9 (R13/R14) thread downstream — is confirmed
**authored** (OQ-215 arm 3: floor authored 5/5) but never confirmed **load-bearing in output**.
Two questions:

1. **Does the pipeline-produced seed dramatize the missing floor?** (floor-specific, not merely
   the co-present Detector-A grain)
2. **If not, can UKE_STORY v0.2 dramatize a contract-only invariant** (the §4 repair pass)?

**Declared stance: test, not confirm.** A floor authored in the contract but only decorative in
the text is **NOT load-bearing**, and this proposal pre-registers the cells under which we say so.

---

## Sources + certification

### Leg B — the load-bearing result (pure Detector-B). *Positive control for the naming probe.*

**Selected by Step-0 Stage-0 dry-run triage (≈$0.03/source), on spend-go.** Selection criterion,
pre-registered: certify a source whose Stage-0 `<invariant_contract>` returns
`missing_floor present="yes"` **AND** `untranslatable_real present="no"`/weak. **Prefer a
corpus-drawn source (#5).** If none certifies pure and leg B must be authored, author it **blind
to the load-bearing criteria** — or hand the authoring to the operator — and pre-register before
grading, so leg B tests the **pipeline**, not a floor-heavy source composed to pass (authored-to-
order leg B is circular: it would test that a floor-heavy *source* yields a floor-heavy *seed*,
not that the pipeline *surfaces* floors). Prose pre-screen (a HYPOTHESIS, not the engine's
verdict): `rotation_seven.md` and `faint_blue.md` are both **NOT purer** than leg A (rotation_seven
= dual-grain Detector-A-dominant; faint_blue = wrong grain, a Snare); reserve them only if
certification surprises the pre-screen. **Fill-in on triage:** `leg B source = __________`;
`Stage-0 cert = __________` (paste the `<invariant_contract>` as evidence).

**Why leg B is read as the load-bearing result and is the positive control (operator sharpening
#2):** if the naming probe (cold arms / blind stage-9) structurally names whatever grain is
*emotionally dominant*, then a null on the dual-grain leg A is a fact about the probe, not about
the story — a clean read byte-identical to a read that never looked. A pure-Detector-B leg B is
what proves the naming probe *can* flag a floor at all. **Leg B carries the load-bearing verdict;
leg A is the weaker supplement.**

### Leg A — the co-presence supplement (dual-grain; already staged + certified, no new spend).

`the_datum_stone` (`agent/narrative_transform/originals/the_datum_stone.md`; certified Stage 0:
`agent/narrative_transform/uke/the_datum_stone_1783889757/stage_0_output.md`, commit `434ec74d`,
dry-run witnessed; run dir holds only `source_story.txt` + `stage_0_output.md`, so
`--resume --from-stage stage_1` is provenance-continuous with the certified contract, no fresh
stochastic re-draw). The `--from-stage` ordering is driven only by leg A being **already paid
for**, not by leg A answering better.

**Leg A is dual-grain (the key [EDGE], the exact confound OQ-219 exists to avoid).** Its Stage-0
contract authors BOTH:
- `missing_floor present="yes"` — Detector-B, **structurally primary**: C1 "The Naturalized Datum"
  (the crown datum / the Stone), `generation_order=1`, `downstream_of=none`, `feeds_into=C2`,
  centrality-5 foundational — the arbitrary cut line every water-right hangs *from*.
- `untranslatable_real present="yes"` — Detector-A, **emotionally dominant**: "the walking of the
  water," carried by the *deferred* C4 "The Unwritten Turns." The source itself subordinates the
  floor grievance to it: *"the walking of the water did not survive the correction — that is the
  loss I actually mean this account to carry, the others being only money"*
  (`originals/the_datum_stone.md:15`).

So the load-bearing read on leg A must **discriminate the floor's narrative work from the
untranslatable-real's**, or the verdict is uninterpretable. **The discrimination IS the
measurement.**

### The discrimination rule (pre-registered — which grain a passage names)

- **Detector-B / missing floor (the target):** *there is no neutral zero-point; the founding
  choice was a hand, not a bottom; the correction only relocates the line.* Textual signatures in
  the source: "Not toward anything. From it." / "if you ask what the line itself hangs from … the
  honest answer … is: eleven weeks at the upper mill, a brass level, and a man's hand." / "There
  is no right place." / "There is no bottom. There is a hand, and a mark cut over the hand." A
  passage naming *this* is a floor-hit.
- **Detector-A / untranslatable real (the confound):** *a mutual obligation that binds all
  equally because it belongs to no one, destroyed by codification.* Textual signatures: "The
  walking held only because it was no one's." / "a bought turn is a different thing than a turn." /
  "These are not the same knowledge." A passage naming *only this* is **NOT** a floor-hit
  (Detector-A naming ≠ floor load-bearing).

---

## Pre-registered "load-bearing" (verbatim, sharpened for dual-grain)

"Load-bearing" is pre-registered as a **three-valued AND** (residue A — the null-is-a-fact-about-
the-probe rule applies to the *erasure* instrument too, not only the naming one):

**Conjunct 1 — Floor-specific naming.** The floor is named as **constitutive**. The *independent*
witness is the **cold arm** (no contract in the payload); the pipeline's **stage-9** corroborates
but is **un-*re*-prompted, not independent** (the contract is threaded upstream via R13/R14 — stage
9 is un-re-prompted, not unprompted; the naming witness must say so or it overstates independence).
Either witness must name the *missing floor / arbitrary datum* as constitutive — "there is no
neutral zero; the correction only relocates the hand" — **NOT** the walking-of-the-water. **Naming
only the walking = floor NOT load-bearing.**

**Conjunct 2 — Capability-specific erasure (operator sharpening #4).** Deleting the floor's-
arbitrariness element must remove the **floor-naming capability specifically** — a reader can no
longer recover "the zero was chosen / no neutral ground" — **not merely weaken the prose**
(uke_story_v0.2.md:104 §4: "name the specific capability lost … No nameable lost capability →
decorative"). On a dual-grain source the floor-erasure will *likely* weaken via the walking's prose
context, not via floor-loss — counterfeiting a witness. **Pass/fail therefore requires pasting the
deleted span to show it is walking-free.** If the span carries walking material, the erasure is
**confounded → INCONCLUSIVE, never a fail** (residue A: a blocked conjunct must NOT default the AND
to NOT-load-bearing). When one conjunct is blocked, name which one carries: cold-arm naming can
carry a **provisional** load-bearing read, or the verdict is held **OPEN and priced** — a confounded
erasure is **never scored as a negative**.

**Falsifier (Detector-B loss, `stage2.md:63`):** if the seed frames the injustice as a **local
error a better authority could correct**, the floor invariant was lost. (Leg A's source stages this
falsifier explicitly and refuses it: "We had asked whether the line was in the right place. There
is no right place." — the finished seed must not collapse to legible-mismeasured-value.)

---

## Pre-registered outcomes (observation → consequence)

**Claim-scope discipline (operator sharpening #1): the general claim does NOT ride on this run.**
A clean load-bearing leg establishes **EXISTENCE** ("the floor CAN carry a story"), NOT generality
("R14's floor does narrative work"). The READOUT **prices the general claim by reference class**
(needs more clean sources) in-body; n=1 must not read as the general claim. The old framing —
"R14 graduates from authored-5/5 to does-narrative-work" — is retired here.

| # | Observation | Consequence |
|---|-------------|-------------|
| 1 | **Leg B (pure) load-bearing** — cold-arm names floor + capability-specific erasure passes | **EXISTENCE established: the floor CAN carry a story.** General "R14 does narrative work" stays OPEN pending more clean sources. |
| 2 | **BOTH legs load-bearing** | **Strongest n=2 signal available:** the floor carries **ALONE** (pure B) **AND survives co-presence** with a dominant Detector-A grain (A). Existence firm; generality still priced by reference class, not proven. |
| 3 | **Naming passes but erasure INCONCLUSIVE** (no walking-free floor-only span isolable — the likely case on leg A; residue A) | **INCONCLUSIVE, not fail.** Cold-arm naming carries a *provisional* load-bearing read; verdict held OPEN and priced. **Do NOT score the confounded erasure as a negative** (that is the #2 error one level down). |
| 4 | **Leg A load-bearing but leg B NOT** (pre-registered now, #3 — do not leave to post-hoc ruling) | **Most diagnostic result: the floor is PARASITIC on the Detector-A grain** — load-bearing only in its presence, not on its own. R14's floor is a co-rider, not an independent carrier. |
| 5 | **Neither leg load-bearing as a seed, but v0.2 repair dramatizes it** | **Missing-floor is a v0.2 *repair* question, not a pipeline-generation claim** (the OQ's stated alternative). |
| 6 | **Neither, even under repair** | **Floor-contract decorative even under repair;** log as a v0.2 §-level gap. |
| 7 | **Cold arms name only the dominant grain on BOTH legs (incl. pure B)** | The naming probe cannot flag a floor; the null is a fact about the probe → **probe invalid, re-instrument** (the #2 failure the positive control exists to catch). |

**Adjudicate per leg; state any split explicitly; never average.**

---

## Adjudication design

- **Blind arms (the INDEPENDENT naming witness + the positive control).** Per leg, build
  `blind_arm_payload_<leg>.md` (seed story, titles/provenance stripped, **NO contract**, arm
  questions incl. "which real does the story turn on — name it"; the identical-escape clause "these
  may be identical — say so"). Run **≥2 model-family arms** (mirror OQ-218: Sonnet + Gemini-2.5-pro),
  one output file each; **grep-adjudicate every arm factual claim before filing**. Verbatim arm
  questions adapted from v0.2 §6 / OQ-218 (Q1 violated-expectation, Q2 inimitable sentence, Q4
  discrimination), **adapted to ask which real each reader names**. `AB_KEY_<leg>.md` for any A/B
  contrast (never shown to a blind arm). **Leg B is the positive control:** if its cold arms cannot
  name the floor, the probe is invalid and any leg-A null is uninterpretable.
- **Blind-arm ineligibility:** the operator authored the pre-registration; the improving/executing
  instance has seen the contract. Neither may sit an arm.
- **Un-re-prompted stage-9 corroborator (weaker).** Pull the pipeline's `stage_9_output.md`: does it
  affirm the *missing-floor* invariant holds (`stage9.md:34-51`)? Grep-witness the exact passage and
  label it **un-re-prompted** (contract threaded), **not independent**.
- **Contaminated-expert audit (operator §1a).** Full-context read of the seed(s) [+ improved]
  against this pre-registration; NOT the blind instrument.
- **Operator ruling.** How the two legs combine and whether R14 graduates vs. reassigns to a v0.2
  repair question is the operator's ruling, recorded verbatim in the entry (D9/OQ-218 precedent).

---

## Deliverables

`audits/2026-07-13_oq219_missing_floor/`:
- `PROPOSAL.md` (this file).
- Per leg: the graded seed (from `stage_9_output.md` / stage-8 story), `blind_arm_payload_<leg>.md`,
  `blind_arm_<model>_<leg>.md` (≥2), `AB_KEY_<leg>.md`, `ERASURE_<leg>.md` (with the pasted
  walking-free deleted span).
- On a not-load-bearing branch only: `MANIFEST_<leg>.md` (v0.2 §7, `blind_read: PENDING`) + the v0.2
  `--edit` improved story; re-run erasure + blind arms on it.
- `OPERATOR_LEG_BUNDLE.md` (seeds [+ improved], full context) for the operator's §1a audit.
- `READOUT.md` (evidence-first, per-leg, per Audit Methodology).

---

## Cost estimate

Leg-A Stage 0 already spent. Leg-B Stage-0 triage ≈ $0.03/source. Each graded run (resume, stages
1–10, Sonnet-5) ≈ one full narrative run — the bulk of spend, ×2 legs. Step-6 v0.2 `--edit` (stages
5–10) runs only on a not-load-bearing branch. Blind arms ≈ a few cold calls per model family per leg
(cf. OQ-218: ~4 Sonnet calls/leg). Order-of-magnitude ≈ two OQ-218 stages. Exact figure recorded here
at spend time.

---

## Verification (of the run, when it happens)

- This PROPOSAL committed **before** the graded runs (git-witness: the PROPOSAL commit precedes each
  run dir's `stage_1` mtime).
- Each graded run witnessed by: exit 0, **OQ-216 guard green** (Stage-2 SECTION 0 extracted; R13
  threading live), `stage_9_output.md` present, run manifest stamped.
- Load-bearing verdict rests on the **cold-arm** naming (independent) + **capability-specific
  erasure** + operator read; stage-9 naming is the weaker un-re-prompted corroborator; the **density
  number is NEVER the witness** (0.0 is not evidence the invariant held).
- Every "names the floor" claim carries a grep-witnessed passage and is labelled **cold-arm vs
  un-re-prompted stage-9**; every erasure claim pastes the deleted (walking-free) span
  (paste-or-untag).
- **Positive-control hygiene:** leg B (pure Detector-B) is the naming probe's positive control.
- **Claim-scope (#1):** a clean leg = floor-carrying **existence**, not R14 generality; the READOUT
  prices generality by reference class. The A-yes/B-no cell (#4) is pre-named "floor parasitic on
  the Detector-A grain."
- **Three-valued AND (residue A):** a blocked/confounded erasure is scored **INCONCLUSIVE → OPEN**,
  never a negative; the READOUT states which conjunct carried when the other was blocked. No outcome
  silently defaults to NOT-load-bearing on an instrument block.

---

# ADDENDUM — Datum Stone run: routing table + pre-committed v0.3 spec (operator ruling, 2026-07-13)

Written **before** the Datum Stone graded run, per the operator's directive ("routing table into the
pre-registration first"). Follows the v0.2-repair **pilot** on "The Margins" (READOUT_v02repair.md):
n=1 seed, split 1/2 cold arms (Gemini recovered the floor; Sonnet read grain). **The pilot cannot
assign the fragility's cause** — three candidates it cannot separate: the **protocol** (floor entered
as a break-rider, no §1 structural home), the **seed** (dual-grain by construction, dominant grain
absorbs the floor), the **reader** (presuppositional prior — recovering a floor is "asking a reader
to see water"). Don't design at a fork the next run resolves.

## The isolating instrument: The Datum Stone (floor-PRIMARY)

The Margins was the **hardest case, run first by accident**: grain-dominant, floor contract-only — we
asked v0.2 to *add* a floor to a story whose grain owns the reader. The Datum Stone **inverts the
dominance**: floor primary (C1 foundational), grain (the walking-of-the-water) present but subordinate
in the contract. If floor-recovery **tracks dominance** (clean on Datum Stone, fragile on Margins), the
fault was never the protocol — it is dual-grain competition, and the fix is **seed-side**. If the floor
**collapses cold even at floor-primary**, the §1 socket is genuinely missing → v0.3.

## Routing table (pre-committed — the run lands on a DECISION, not a design meeting)

- **(a) Clean cold floor-recovery on Datum Stone** → **NO v0.3.** Add a **Stage-2 dominance-ordering
  clause**: when the contract marks the floor primary, subordinate the other real on-screen. This is
  machinery v0.2 already owns one level down — the **two-reals subordination** clause (§1a: "two reals
  may coexist only if the story subordinates one on-screen"), applied to floor-vs-grain. File the
  floor's fragility on **dual-grain** seeds as a known seam.
- **(b) Collapse at floor-primary (cold arms miss the floor even when it is the primary invariant)** →
  **v0.3 executes** (spec pre-committed below).
- **(c) Floor ABSENT from the generated seed entirely** → the R14 thread isn't reaching generation →
  **pipeline-side (stages 2/3)**, NOT v0.3 at all.

## Pre-committed v0.3 spec (executes ONLY on outcome b; budget-legal, ships shorter)

- **One new term: "the floor."** Passes Parfit easily (far shorter than "a zero-point the system's
  honest operation presupposes and cannot audit"). Concept budget 4 → 5 with the offset below.
- **§1a gains a two-kinds clause with PAIRED falsifiers:** the **grain's** falsifier is *a better
  instrument*; the **floor's** is *a fairer authority*. Floor-indifference = the fairer authority
  arrives and **re-founds the same zero** (the correction relocates the line; there is no right place).
- **One deformation operator (§2), the Mendoza move generalized:** *let the instrument be perfectly
  honest, then ask it to weigh its own founding.*
- **Ships-shorter offset (self-application):** §5 (the ensemble) is **untested across five passes and
  compressible** (its own Ω_E2 amputation condition) — cut/compress it to pay for the new term.

## Taxonomy ruling (STANDING — settled by the pilot, regardless of a/b/c outcome)

**The floor is the grain's structural SIBLING, not a break-species.** Different **mechanism**
(presupposition vs unreadability — the mountain isn't unreadable, it's *un-weighed by the choice that
set the zero*); different **falsifier class** (fairer authority vs better instrument); different
**dramatic carrier** (the grain lives in a character's *knowing*; the floor lives in the instrument's
*honest operation* — which is why the seed's honest scale was the right stage for it). **The break-rider
was NOT a hack:** violating the neutrality prior *is* the floor's reader-facing form, so §1b was the
correct vehicle; what is missing is only the §1 **structural home**. **Legibility handicap (permanent):**
the neutrality prior is *presuppositional*, so **floor-recovery runs BELOW grain-recovery in blind arms
by nature** — not a craft failure. Do not build a concept because the floor is the framework's darling
([EDGE], operator): localize before building (the counting-ban-at-five-sites lesson). If Datum Stone
says seed-side, the floor does not get a term, however much the theory loves it.

## Label-delta leg (added to the load-bearing bar)

Record **BOTH** recoveries: **primed** (stage-9 reads *with* the contract) vs **cold** (§6 arms read
blind). The delta is the measurement. The honest **load-bearing bar** becomes: **primed reader names
the floor + erasure weakens it + cold recovery MEASURED-AND-REPORTED (not required)** — because a
presuppositional prior should not be *required* of a cold reader to count as load-bearing.

## Datum Stone §6 design (this run)

- **Arms: THREE cold** (given the pilot's observed 50/50), payload = the generated seed only (no
  contract), floor-vs-grain discriminator; grep-adjudicated. (Two-plus-a-human would double as the
  gold arm this arc owes — deferred; no human arm available this session, so three machine arms.)
- **Primed leg:** the pipeline's stage-9 (contract-threaded) floor-affirmation, grep-witnessed and
  labelled **primed/un-re-prompted**, reported beside the cold recovery as the label-delta.
- **Filing:** The Margins goes under OQ-219 as its **pilot** (cf. the ergodicity pilot under OQ-220),
  existence-at-n=1, **split preserved** (both arms filed, never averaged). Verdict of record is the
  operator's (v0.2 Ω_C2); the machine arms are the assisted instrument; the OQ-218 human-read gate
  still gates publication.
