# Coverage map — the 20 abstaining constraints (OPEN-B)

Live corpus `prolog/testsets/` (57 constraints), commit at audit time. Verdict source:
the rewired `report_generator:detect_gap_pattern/2` (canonical seat path). An abstain row
mints nothing; the abstention set **is** backward-compat made visible (no-seat set =
no-gap-possible set). Per the plan, **visibility ≠ adjudication** — the deliberate-vs-hole
call on each row needs the PROSE, which this map does not read. The `stk`/`sixq` columns are
the empirical authored-field facts; the **Call** column is the starting hypothesis, not a
ruling. Definitive adjudication is OQ-129 OPEN-B.

Vacuous-signal note: a naive `grep persp` over the json matches schema boilerplate in every
file (it returned True for all 20) — it is NOT a hole signal and was dropped. The real signals
are: `stakeholders[]` count, `six_questions` count, and json presence.

| Constraint | stk | sixq | json | Call (hypothesis — OPEN) |
|---|---|---|---|---|
| actinide_replenishment_mechanism_contradictions | — | — | NO_JSON | pl-only `*_contradictions` entry; likely deliberate (physics) — VERIFY |
| performance_legitimacy_contradictions | — | — | NO_JSON | pl-only `*_contradictions` entry — VERIFY |
| catastrophe_memory_kernel__boundary_maintenance_reading | 0 | 8 | yes | **HOLE candidate** — perspectival content authored in `six_questions`, not compiled to `stakeholders[]` |
| animal_status_kernel__property_reading | 0 | 0 | yes | HOLE? (plan-named example) — needs prose read |
| basic_law_interpretive_authority__parliamentary_sovereignty_reading | 0 | 0 | yes | unknown — empty both authored surfaces |
| bitcoin_whitepaper_purpose__nakamoto_oracle_opacity | 0 | 0 | yes | unknown |
| border_control_legitimacy__freedom_of_movement_primary | 0 | 0 | yes | unknown |
| divine_legitimacy_substrate__folk_syncretistic_reading | 0 | 0 | yes | unknown |
| equal_protection_kernel__colorblind_reading | 0 | 0 | yes | unknown |
| fourteenth_amendment_equal_protection__formal_equality_reading | 0 | 0 | yes | unknown |
| jewish_self_determination__indigenous_return_reading | 0 | 0 | yes | unknown |
| lausanne_minority_protections__guarantor_reading | 0 | 0 | yes | unknown |
| maat_order_principle__reciprocity_reading | 0 | 0 | yes | unknown |
| marriage_authority__judicial_harmonization_reading | 0 | 0 | yes | unknown |
| nicene_creed_authority__liturgical_habituation_reading | 0 | 0 | yes | unknown |
| secession_legitimacy_boundary__constitutional_impossibility_reading | 0 | 0 | yes | unknown |
| shinbutsu_ontological_commitment__incoherence_reading | 0 | 0 | yes | unknown |
| speech_protection_kernel__absolutist_reading | 0 | 0 | yes | unknown |
| zero_mathematical_status__placeholder_reading | 0 | 0 | yes | unknown |

19 no-seat abstainers above. Plus 1 **seated-but-all-unknown** abstainer (OPEN-C):

| Constraint | stk | sixq | note |
|---|---|---|---|
| livelihood_security_reading | 8 | 8 | 8 authored seats, ALL compute `unknown` under the canonical seat path. Serializes as `gaps:null` (didn't-look), NOT `[]` — the Pattern-6 coverage bit. OPEN-C: missing-metric hole vs genuinely untyped. NB: under the plan's inline `dr_type/3` path it read 6 unknown + 2 rope (still no_gap); the seat path types all 8 unknown. |

Pattern-6 coverage tally at serialization (`pipeline_output.json`, per_constraint=57):
**gaps null=20 (abstain) · []=17 (no-gap) · populated=20 (gap)**.
