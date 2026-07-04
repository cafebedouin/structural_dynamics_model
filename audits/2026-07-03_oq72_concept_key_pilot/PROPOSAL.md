# OQ-72 concept-key pilot — PROPOSAL (pre-registration)

Status: **R1 RATIFIED 2026-07-04** (all four asks; C1 carries a diagnostic rider — see
`R1_RULING.md`, same dir). Written 2026-07-03, BEFORE any vocabulary draft or assignment
run. Substrate witnesses: `RECON.md` (same dir, re-witnessed this session). Plan of
record: `~/.claude/plans/present-a-plan-for-humble-haven.md`.

## What this pilot claims and does not claim

The mechanical key does NOT move the axiom axis to "discovered" footing. It relocates the
ruling from per-pair hand-authoring to vocabulary granularity and swaps a silent seat for
an explicit **ratified** one (operator = author of record). Closure claim, if earned, is
scoped: **"mechanism demonstrated"** on this mixed haiku/live pilot substrate — not "the
axiom axis carries an auditable ontology on the corpus OQ-75b will cite" (that is the
separate live scale-up spend-go).

## Pilot roster (10 kernels; witnessed in RECON.md §§2–3)

Live leg (`testsets/`) — the contradiction discriminating case (twins carry ZERO
`cs_axiom_contradiction` facts; only these can exercise the criterion's own prediction):

| kernel | readings | contradiction facts |
|---|---|---|
| digital_money_legitimacy | 3 | 2 |
| moral_causation_locus | 3 | 2 |
| visual_evidentiary_authority | 4 | 6 |

Haiku leg (`testsets_haiku/`) — pair-rich breadth, spanning the reading-count distribution
(3×1r, 49×2r, 259×3r, 18×4r, 2×5r across 328 multi-reading kernels):

| kernel | readings |
|---|---|
| marriage_authority_kernel | 5 |
| vatican_ii_doctrinal_authority | 4 |
| software_source_status | 4 |
| ai_governance_legitimacy | 3 |
| animal_moral_status | 3 |
| tordesillas_demarcation_kernel | 2 |
| wto_treaty_framework | 2 |

Roster note: haiku candidates were chosen to AVOID kernel-ids that also occur on the live
leg (`speech_protection_kernel`, `udhr_article_3` were eligible by reading-count but
collide; see RECON.md §6) — cheap removal of a cross-leg confound, no loss of coverage.
Legs are loaded and swept separately (one `asserta` corpus_path overlay at a time, serial).

## Mechanics (pre-registered)

- **Vocabulary draft (Phase 2, THE BIASABLE STEP):** per-kernel controlled vocabulary of
  subject/functional-slot concepts (atom + one-line definition), drafted from the kernel
  question/topic and the axiom-NAME population — never from pairing structure. Concept
  atoms are kernel-scoped, namespaced `<kernel_id>__<slot>` to make accidental cross-kernel
  equivalence claims syntactically impossible. R2 ratification is where the operator's seat
  is exercised — this pipeline is NOT sold as unbiasable end-to-end; the unbiasable step is
  only the assignment below, and the ruling relocates to vocabulary granularity.
- **Assignment (Phase 3, the unbiasable step):** per-reading, one reading at a time; the
  assigner sees ONLY that reading's own `.pl` file + the ratified kernel vocabulary; never
  sibling readings' axioms, never contradiction facts, never pairing structure. Output row:
  axiom name → concept | `no_slot`, one-line rationale. Inline (session model, zero API
  spend).
- **Alignment stays the existing join:** `axiom_aligned(concept, A, B) :-
  axiom_concept(A, C), axiom_concept(B, C).` (`axiom_diff.pl:72`); unmapped names stay
  unique `unmapped(Name)` vantages (blind, never silently merged, `:82-83`).

## Criterion (committed, with falsifier)

Same subject / functional slot: two axioms get the same concept iff they occupy the same
subject-slot of the kernel's question — so **contradiction pairs SHOULD align** (opposed
poles of one subject, differing in grounding/status, the westphalia disparity shape).

## Controls (each pre-registered with PASS/FAIL semantics; gate is TWO-SIDED)

**C1 — Within-kernel positive control (proposer PASS carrier; under-merge side).**
Named live cross-reading contradiction pairs (witnessed cross-reading in RECON.md §4):
1. `state_monopoly_on_legitimate_issuance` ↔ `consensus_suffices_for_legitimacy`
   (digital_money_legitimacy: sovereign_cbdc_reading vs crypto_permissionless_reading)
2. `situational_primacy_over_disposition` ↔ `character_cross_situational_stability`
   (moral_causation_locus: situational_reading vs dispositional_reading)
3. `indexical_traces_recoverable` ↔ `verification_impossibility_at_scale`
   (visual_evidentiary_authority: indexical_realism vs epistemic_collapse)

Checked at Phase 3: blind assignment must independently land both members of a pair on the
same concept atom. **PASS = ≥2/3 pairs same-concept. 1/3 = MARGINAL (operator ruling at R3
whether to proceed or revisit granularity). 0/3 = FAIL → kill-condition leg (b).** A
proposer that maps everything to `no_slot`/unique concepts fails HERE, not silently.

**R1 diagnostic rider (ratified 2026-07-04):** the count is the TRIGGER, not the verdict.
At ANY miss, classify the cause before kill/pass fires — the two causes have opposite
responses: (i) **granularity artifact** (vocabulary drawn too fine; both members occupy
one subject-slot the vocab split) → re-vocab at R2 with the criterion intact — even 0/3
from a globally-too-fine vocab is an R2 rerun, not a criterion death; (ii) **subject
mismatch** (the `cs_axiom_contradiction` fact opposes on an axis other than shared
subject) → the same-subject criterion is leaking — THAT is the criterion revisit. The
classification, not the count, decides re-vocab vs criterion revisit; a 2/3 pass with a
subject-mismatch miss still surfaces the mismatch to the operator rather than waving it
through.

**C2 — Contradiction-specific PASS (the criterion's own executable falsifier).**
Checked at the Phase-5 sweep, on the 3 live kernels only: ≥1 `cs_axiom_contradiction` pair
lands on a shared concept atom and surfaces in `axiom_diff` as same-concept agreement or
disparity (the westphalia shape). **FAIL (zero pairs aligned) = the CRITERION failed, not
coverage — stop, write up, OQ stays open with granularity revisit. Not paperable with
haiku conversions.** This is the one prediction the committed criterion makes that
`exact_name` cannot already deliver.

**C3 — Non-degeneracy floor (under-merge gate, pooled).**
≥1 blind→(agree/disparity) vantage conversion in **≥⅔ of pilot kernels (≥7 of 10)** at the
Phase-5 both-keys sweep. Pooled floor alone is NOT sufficient (satisfiable entirely by
pair-rich haiku kernels) — C2 must pass independently.

**C4 — Negative control (over-merge gate).**
Lexical adversary (`python/audits/oq72_lexical_adversary.py`, token-overlap clusterer over
pilot axiom names — the OQ-64 morphology trap repurposed as ADVERSARY, never proposer)
generates merge candidates; at R3 the operator marks the truly-distinct pairs among them;
the proposer must have labeled every marked-distinct pair DIFFERENTLY. **The adversary gets
its own positive control:** a planted lexically-close synthetic pair must be merged by the
adversary, else the adversary is invalid and the negative control is VOID (halt, fix
adversary, re-run — an empty candidate list is "didn't look," not "nothing there").
**FAIL = any marked-distinct pair shares a concept label** (counts into the false-merge
rate and trips kill leg (a) if the negative control is silent).

**C5 — Join/regression control.**
Existing westphalia fixture tests (`prolog/tests/test_axiom_diff.pl`, cross-kernel pair
`westphalia_sovereignty` vs `westphalian_sovereignty`) stay green — proves the diff join
consumes the `axiom_concept/2` seat and surfaces the grounding inversion. Does NOT
exercise the proposer and is not claimed to.

**C6 — Ratification-gate control.**
A planted unratified row fed to the baker (`python/axiom_concept_bake.py`) must be
REFUSED (fail-closed, refusal output pasted). Checked at Phase 4.

## Pre-registered bars (operator ratifies or adjusts at R1)

- **False-merge bar: ≤10%** — fraction of proposed non-`no_slot` assignment rows rejected
  at R3 ratification *as over-merges* (rejection because the row would merge distinct
  subjects; edits for atom naming taste do not count). Denominator: all proposed
  non-`no_slot` rows across the pilot.
- **Non-degeneracy floor:** as C3 (≥1 conversion in ≥7/10 kernels).

## Kill condition (two legs; either → stop, write up, OQ-72 stays open)

- **(a) over-merge:** false-merge rate > bar, OR negative control silent (a marked-distinct
  pair shares a label / adversary's planted-pair control fails). Checked at R3.
- **(b) under-merge:** C1 at 0/3, OR pooled floor (C3) unmet, OR zero contradiction pairs
  aligned on the live kernels (C2). C1 checked at R3; C3+C2 at the Phase-5 sweep. Closure
  is not claimable until BOTH legs have passed.

## R1 asks (the operator's seat — this proposal blocks here)

1. Ratify/adjust the 10-kernel roster (3 live + 7 haiku above).
2. Ratify/adjust the false-merge bar (proposed ≤10%) and non-degeneracy floor (proposed
   ≥7/10 kernels).
3. Ratify C1's PASS grading (≥2/3 pass / 1/3 marginal / 0/3 kill).
4. Confirm the mixed haiku/live pilot substrate is acceptable for a
   "mechanism-demonstrated" closure (live-corpus coverage remains the scale-up spend-go).
