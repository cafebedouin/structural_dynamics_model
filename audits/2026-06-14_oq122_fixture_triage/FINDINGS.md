# OQ-122 fixture-migration triage — RE-MEASURED ON LIVE; the 36-fixture blocker is STALE

**Date:** 2026-06-14 · **Branch:** `oq122-oq50-oq74` · **Type:** read-only measurement (no fixture
edit, no engine write, gate NOT merged) · **Plan:** `review-oq-122-oq-50-and-expressive-whistle.md` item D

## What item D asked for, and why it changed

The plan asked for a per-fixture triage of the **36 unit-test fixtures** the FSM victim-gate (branch
`oq122-fsm-victim-gate`, `ab1e9b26`) breaks — `test_agent_beneficiary` ×31 + `test_contradiction_signatures`
×5 — sorting each into **add-a-victim** vs **flip-the-expectation**. That "36" was measured 2026-06-13,
**before** the live corpus drifted. Re-measured on the current live corpus (HEAD `da0e88e2`), the premise
does not hold: **the gate introduces zero new test failures, and the fixtures it was said to break are
already failing for a corpus-drift reason that has nothing to do with the gate.** A 36-row add-victim/
flip-expectation table would be fiction. The honest artifact is this re-measurement.

## Method

`run_tests` over each suite transitively loads every plunit unit in the loaded corpus (the testset-embedded
`*_tests` units) plus the hand-authored fixture unit. Captured the **deduplicated failing-test id set** on
HEAD (no gate) and with the gate's single-file diff applied (`git checkout oq122-fsm-victim-gate --
prolog/signature_detection.pl`, reverted after), and diffed the sets. Also measured the gate's actual effect
on `false_summit_mountain/2` over the live corpus, and the presence of the fixtures' referenced constraints.
Evidence files in this directory: `{baseline,gate}_agent_beneficiary_failures.txt`,
`{baseline,gate}_contradiction_sig_failures.txt`.

## Result 1 — the gate breaks NOTHING new on the live corpus

| suite | baseline (HEAD) unique failures | gate unique failures | gate-introduced delta |
|---|---|---|---|
| `test_agent_beneficiary` | 20 | 20 | **∅ (identical sets)** |
| `test_contradiction_signatures` | 5 | 5 | **∅ (identical sets)** |

(`comm -3` of the two id-sets is empty in both suites. The "31" in the gate commit is the raw
per-instance count; it dedups to 20. Both numbers are pre-existing, not gate-caused.)

## Result 2 — the fixtures' referenced constraints are GONE from the post-reset corpus

- **0 of 11** `fsm_agent_mountains` (`test_agent_beneficiary.pl:81-93`) are present in the live corpus.
- `maxwell_demon_impossibility` (the suite's flagship positive control) is **absent**.
- The 4 hand-authored `agent_beneficiary` unit failures on baseline
  (`agent_beneficiary_view_dispatch`, `maxwell_certifies_natural_law`,
  `maxwell_mountain_at_all_canonical_contexts`, `nlwb_287_inertness_direct`) all fail because their
  subject constraints were deleted at the **2026-06-05 corpus reset** — corpus drift, not the gate.
- The other 16 baseline failures are testset-embedded `mountain_threshold_validation` /
  `nl_profile_validation` checks in 8 live testsets (animal_status, demographic_resource,
  neutron_star, etc.) — pre-existing corpus-health failures, also gate-independent.

## Result 3 — the gate's ACTUAL live effect is a clean 2→0 on the physics false-positives

- Baseline `false_summit_mountain` distinct firings on live = **2**:
  `actinide_replenishment_mechanism_flat_control`, `radiative_levitation_stratification` — both
  `constraint_victim`-empty (`vic0`). These are exactly OQ-122's known physics false-positive footprint.
- With the gate: **0** firings. Both vic=0 physics cases are exempted, which is the gate's intended
  semantics (no victim ⇒ no payer ⇒ nothing to conceal).
- Zero test regressions accompany that 2→0 (Result 1).

## Disposition (evidence only — the merge/hold call remains the operator's)

1. **The "36-fixture migration" merge-blocker (OQ-122 blocker (i)) is STALE.** It cannot be triaged
   add-victim-vs-flip because on the live corpus the gate breaks none of them; they are already broken by
   corpus drift. Triage is moot until/unless the agent-beneficiary fixtures are rebuilt against the
   post-reset corpus (a separate fixture-health task — the fixtures reference a dead corpus regardless of
   the gate).
2. **On the live corpus the gate is a clean, zero-regression 2→0** on the two physics false-positives.
   The fixture-cost half of the plan's hold rationale ("invisible payoff vs real 36-fixture cost") no
   longer applies to the current corpus.
3. **The hold/bundle ruling still stands on its OTHER leg.** Whether exempting FSM actually moves those
   2 constraints' headline verdict to GREEN is the **physics-RED / power-scaling Ω_C** — their RED is
   overdetermined by `type_1`-over-CIR power-scaling (`drl_core.pl:605-613`), minted here as **OQ-128**.
   FSM-exemption alone may not clear their cap. So bundling the gate with OQ-128 (not standalone) remains
   the recorded disposition; this re-measurement corrects the *stated cost*, not the bundle decision.
4. **Operator re-rule available (not taken here):** with the fixture cost shown stale and the live effect
   clean, the operator may wish to revisit "hold" — but that is an output-changing engine merge and a
   human call; recorded as an OQ-122 open item, not actioned.
