# Sibling-surface sweep — does any engine surface carry the VIEWING POSITION'S OWN contingency?

**Run:** 2026-08-25, HEAD `25f92b3f4`. Precondition for the `feature_access` self-position OQ.

**Criterion (reachability, not idiom):** does any surface carry a value about *the mutability of
the observer's own position* — not the mutability of the constraint observed — to any read site?

**No planted decoy.** The negative is stated at LIVENESS grade: what was searched, with what
terms, and a positive control showing the procedure finds a real neighbour.

## 1. The four named surfaces — every mutability-valued slot is about the CONSTRAINT

| surface | mutability-valued slot | what the value is ABOUT |
|---|---|---|
| `observer_accessible/3` (`constraint_indexing.pl:872-891`) | `PerceivedMutability`, from `effective_immutability_for_context/2` (`:888`) | the **constraint**, as perceived from the position |
| `seat_perceived_vs_real/4` (`stakeholder_seats.pl:160-172`) | `Perceived ∈ {immutable, changeable}`, from `effective_immutability(T,E,mountain)` on the seat's own `(T,E)` | the **constraint**, from that seat |
| `trigger_epistemic_trap/3` (`abductive_triggers.pl:876-916`) | restricted vs full `dr_type` | the **constraint's type**, two ways |
| `extraction_blindness` (`report_generator.pl:368-372`) | `De > Df` over reading power levels | the **constraint's type** disagreeing across seats |

All four key ON observer coordinates `(T, E)` and return a judgement ABOUT the constraint. None
returns a value about whether the viewing position is itself contingent, revisable, or
maintained. The nearest thing in `feature_access/3` is `cross_context` — seeing the constraint
from other positions — which is still the constraint.

## 2. Term sweep (engine: `prolog/`, `python/`; `--include=*.pl --include=*.py`)

| term | files | |
|---|---:|---|
| `own_position`, `self_position`, `observer_contingency`, `seat_contingency`, `own_contingency`, `position_mutability`, `observer_mutability`, `seat_mutability`, `own_revisability`, `position_revisability`, `viewing_position` | **0** each | — |
| `position_contingency` | 2 | **authored story content, not engine** — `omega_variable(chartered_party_position_contingency, conceptual, …)` in `testsets_stealth2/july_charter_…`, and `ideological_opposition_contingency` in `archives/datasets/kernel_v1/cold_war_geopolitical_constraint.pl` |
| `observer_position_` | 4 | **authored story content + one doc reference** — `omega_variable(observer_position_bias, …)` (`archives/…/colombia_rural_vote_buying.pl`), `observer_position_inversion` (`testsets_kimi/equal_protection_commitment__remedial_reading.pl`), `constraint_vindicates(…, observer_position_correlation)` (`testsets/partition_choice_reading.pl`), and `linter.py:869` citing `docs/observer_position_same_level_actors.md` |

**The concept exists in the corpus as authored CONTENT** — a story may declare "whether this
seat's own position is contingent" as an `omega_variable/3` — **and nowhere as an engine surface
that computes or routes it.** Authored omegas are inert with respect to classification.

## 3. Positive control — the same procedure, on terms known to exist

| term | files |
|---|---:|
| `effective_immutability` | 96 |
| `effective_immutability_for_context` | 29 |
| `PerceivedMutability` | 1 |

The procedure finds the near-miss surface — an observer-keyed, mutability-valued predicate read
into a live classification path — in 96 files. **A zero above is therefore a zero, not a
didn't-dispatch.**

## 4. Grade and falsifier

**Liveness grade.** This licenses: *no surface under these names carries the viewing position's
own contingency to a read site.* It does NOT license *the engine cannot express it* — a surface
under a name none of these terms reaches would be missed. **A later surface discovery reopens the
question**, and that is the OQ's standing falsifier rather than a caveat on it.
