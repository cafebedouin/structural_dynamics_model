# R2 ruling (operator adoption, 2026-07-04)

**All ten kernel vocabularies RATIFIED as drafted** (`vocabulary_draft.md`, unedited).
Constitution of the seat: the operator adopted the reviewed recommendation ("Agree with
Claude Web suggestions, in the main. Incorporate what makes sense to you") — the seven
haiku vocabularies ratified on coverage grounds (haiku leg carries zero contradiction
facts, so granularity there is pure coverage: over-merge caught by the C4 adversary,
under-merge reads blind via `no_slot` and shows at the floor), the three live
vocabularies ratified after per-pair containment checks. The operator is author of
record by adoption; the analysis below is the recommendation's witness, not the seat.

## The R2 corollary of the C1 rider (ratified as working rule)

For a contradiction-bearing kernel, vocabulary granularity at R2 *predetermines* whether
C1 is passable at R3: slots that split a tested pair make C1 unpassable by construction —
a manufactured granularity artifact, to be fixed HERE, not diagnosed at R3 (where the
≥2/3 tolerance band could hide it as one tolerable "miss"). The asymmetry that keeps this
from being coarsen-to-green: one slot makes C1 *passable*, not *pass* — the blind
assigner must still independently land each pole on the slot rather than `no_slot`.
Coarsen only where subject-merits say one subject.

## Pairing witness — ALL contradiction pairs on the three live kernels

Full fact paste (this session, deduped from symmetric duplicates; 10 facts = 5 pairs):

```
cs_axiom_contradiction(state_monopoly_on_legitimate_issuance, consensus_suffices_for_legitimacy).   % + converse
cs_axiom_contradiction(indexical_traces_recoverable, verification_impossibility_at_scale).          % + converse
cs_axiom_contradiction(indexical_traces_recoverable, consensus_primacy_over_indexicality).          % + converse
cs_axiom_contradiction(verification_impossibility_at_scale, cryptographic_provenance_as_truth_warrant). % + converse
cs_axiom_contradiction(character_cross_situational_stability, situational_primacy_over_disposition).    % + converse
```

Slot containment by name→definition read (drafting input was names; NOT a witnessed
assignment — Phase 3 decides assignments blind):

| kernel | pair | slot read | contained? |
|---|---|---|---|
| digital_money_legitimacy | state_monopoly ↔ consensus_suffices | both → `issuance_legitimacy_basis` ("state authority, distributed consensus" named in def) | YES (C1 pair) |
| moral_causation_locus | character_cross_situational_stability ↔ situational_primacy | both → `causation_locus` (def spans both poles; causation axis, not accountability) | YES (C1 pair) |
| visual_evidentiary_authority | indexical_traces ↔ verification_impossibility | both → `verification_feasibility` ("recoverable traces", "scale impossibility" named in def) | YES (C1 pair) |
| visual_evidentiary_authority | indexical_traces ↔ consensus_primacy_over_indexicality | `verification_feasibility` vs `truth_warrant_source` | **NO — spans slots** |
| visual_evidentiary_authority | verification_impossibility ↔ cryptographic_provenance | `verification_feasibility` vs `truth_warrant_source` | **NO — spans slots** |

Resolved per-call:
- **Call 1 (digital_money):** `private_innovation_within_regulatory_perimeter` is NOT a
  member of any contradiction pair → ordinary coverage; broad-def-let-assigner-rule
  stands, C4 adversary as over-merge backstop. Ratified as drafted.
- **Call 2 (moral_causation):** one-slot `causation_locus` confirmed against the pair
  (pair sits on the causation axis, not `accountability_intervention_locus`). Ratified.
- **Call 3 (visual_evidentiary, the unflagged third):** the pre-named falsifier fired in
  the fine direction for the C1 pair (single-slots under `verification_feasibility`); the
  3-way split stands. Ratified.

## Pre-registered finding: two by-construction split pairs (visual_evidentiary)

The Phase-5 watch is CLOSED by witness: the kernel's 6 facts are 3 DISTINCT pairs, not
directional duplicates. Two of the three span `verification_feasibility` ↔
`truth_warrant_source` under the ratified vocabulary and therefore CANNOT align at any
assignment. Pre-registered interpretation, written before the sweep:

1. No gate is foreclosed: C1 tests only the three named pairs (all contained); C2
   requires ≥1 aligned pair on the live kernels (satisfiable by each kernel's contained
   pair). Kill legs unchanged.
2. A Phase-5 zero on these two pairs is BY-CONSTRUCTION, never proposer failure — do not
   count them against C2/C3 or read them as misses.
3. The vocabulary is NOT coarsened to make them alignable: subject-merits keep
   verification-feasibility distinct from truth-warrant-source, and merging to green the
   pairs would be exactly the rigging the R2 corollary's asymmetry guards against.
4. What the split pairs ARE: evidence that `cs_axiom_contradiction` records opposition
   that is not always same-subject (a feasibility claim can contradict a warrant claim
   that presupposes feasibility). This scopes the criterion's gloss "contradiction pairs
   = opposed poles of ONE subject" — carried to the WRITEUP as a criterion-scope
   observation and to the Phase-6 close (the scale-up recipe should not assume
   contradiction⟹same-subject universally). Operator may overrule with a merge at R3;
   default is as ratified.

## Incorporated non-blocking notes (logged so they aren't relitigated)

- **Namespacing precision:** `<kernel_id>__<slot>` atoms make cross-kernel concept
  EQUIVALENCE impossible, but the `axiom_concept/2` KEY is the bare axiom name — a name
  recurring on two legs gets one mapping wherever it occurs. Separate guard: the RECON §6
  bespoke-name census runs at R3 (expected 0 cross-occurrences). Both guards needed;
  neither redundant.
- **software_source_status__empirical_effects** slices by claim-kind rather than subject
  — different principle than its siblings; haiku leg, no C1 stake; assigner + adversary
  carry it.
- **tordesillas_demarcation_kernel** (2 readings, 4 axioms, 3 slots) is near-degenerate
  and a probable ~0-conversion floor non-contributor; a zero there is not proposer
  failure (floor tolerance 7/10 absorbs it).

**Next:** Phase 3 — blind per-reading assignment (fresh subagents, one reading file + its
ratified kernel vocabulary each), lexical adversary with planted-pair control, then R3.
