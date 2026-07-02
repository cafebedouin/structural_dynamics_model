# OQ-137 classification table — reading layer vs the typed-absence convention (§5)

Evidence: `sweep_results.txt` (live corpus n=119, 661 seats, 89 UIDs, 83 drift-UIDs, 45
kernels; probe controls fired) + per-predicate code reads (line refs at git `2b66dedc`).
Classes: **T** = total_on_domain (registered + suite-guarded) / **P** = partial_by_design
(registered with reason) / **D** = silently-failing-defect (fixed this audit) / **O** = out of
scope (not an aggregatable reading; recorded here only).

## Family A — stakeholder_seats (14 exports)

| predicate | domain | sweep | class | evidence / reason |
|---|---|---|---|---|
| q6_crosscheck/3 | constraint | 119 one | **T** (already registered) | OQ-121 template; suite-proven |
| extraction_state/2 | constraint | (census Σ) | **T** (already registered) | OQ-121 totalization |
| consensus_provenance/2 | constraint | 119 one | **T** (already registered) | OQ-121 totalization |
| seat_perceived_vs_real/4 | seat | 661 one | **T** (already registered) | untyped token on underivable type |
| in_contention/3 | — | — | **P** (already registered) | relation between seats |
| stakeholder_context/3 | seat | 661 one | **T** (registered this audit) | deterministic projection of the authored seat fact; suite also guards duplicate seat authoring |
| derive_directionality_for_stakeholder/3 | seat | 661 one | **T** (registered) | override → role-d → canonical-power fallback chain; total on authored seats |
| dr_type_for_stakeholder/3 | seat | 661 one | **P** | raw computation, CAN fail by design (stakeholder_seats.pl:145 doc); its totalized wrapper is seat_perceived_vs_real/4 (`Computed = untyped`). Registering T would make a legitimate untypeable seat a gate failure |
| chi_for_stakeholder/3 | seat | 661 one | **P** | same shape as dr_type_for_stakeholder; no totalized wrapper exists yet — noted as the first candidate if an aggregate ever consumes per-seat χ directly |
| power_witness_map/2 | constraint | 119 one | **T** (registered) | findall construction — total by shape (OQ-108) |
| power_witness_count/3 | constraint×power | — | **P** | per-atom expansion of power_witness_map/2 (6 solutions per C); the map is the registered exactly-one surface |
| extraction_reading/2 | constraint | 0 fires | **P** (registered) | fires exactly on extraction_fired (contract, stakeholder_seats.pl:368); total surface = extraction_state/2 |
| stakeholder_d_override/3 | — | — | **O** | dynamic probe input, not a reading |
| extractive_type/1 | — | — | **O** | type-atom membership table |

## Family B — signature_detection (16 exports)

| predicate | domain | sweep | class | evidence / reason |
|---|---|---|---|---|
| constraint_signature/2 | constraint | 119 one | **T** (already registered) | honest-abstain `unknown` fallback (:136) |
| signature_confidence/3 | (C, its signature) | 119 one | **T** (registered, domain `constraint_signature_pair`) | generic fallback clause (:579) covers every signature incl. unknown |
| explain_signature/3 | (C, its signature) | **9 zero** / 110 one | **D → fixed → T** (registered, `constraint_signature_pair`) | NO clause for `unknown` → fails on exactly the 9 unknown-signature constraints (all `*_contradictions`); consumer report_generator.pl:567–571 chains it unguarded inside `catch(forall(...)) ; true` (:98–103), so ONE failure silently TRUNCATED the whole [STRUCTURAL SIGNATURE ANALYSIS] section at that constraint (Pattern 6 at the composition). Fix: honest-abstain `unknown` clause + per-constraint OPEN marker at the read site; the registered pair-domain entry makes any future explanation-less signature a suite failure |
| false_natural_law/2 | constraint | 1 fire | **P** (registered) | detection verdict; total surface = constraint_signature/2 |
| false_summit_mountain/2 | constraint | 3 fires | **P** (registered) | detection verdict |
| coupling_invariant_rope/2 | constraint | 17 fire (16 multi ≤3) | **P** (registered) | one solution per coupling witness — consumers must once/1 it (constraint_signature does, :121) |
| false_ci_rope/2 | constraint | 32 fires | **P** (registered) | detection verdict |
| structural_purity/2 | constraint | 119 one | **T** (registered) | inconclusive-guard + contaminated(Failures) catch shape (:1177–1200); bound-probe fix 2026-06-03 made it dispatch correctly |
| has_viable_alternatives/2 | constraint | 119 one | **T** (registered) | catch-all `unknown` clause (:272); range {true, unknown} — `false` builder-unreachable (OQ-113, documented) |
| has_metric_perspectival_variance/1 | constraint | 89 fire | **P** (registered) | boolean; consumers (diagnostic_summary:319,:497) treat failure as false via catch-guards. CAVEAT recorded: failure collapses no-authored-perspective-metrics with authored-equal-metrics |
| level_gradient_divergence/2 | constraint | 0 fires | **P** (registered) | OQ-93 Stage D detection; consumer report_generator.pl:383 catches and degrades to the grid-less question (documented ruling) — wired, currently dark on this corpus |
| get_constraint_profile/2 | constraint | 119 one | **O** | internal profile constructor (classification input), not an aggregate-consumed reading; witnessed total anyway |
| integrate_signature_with_modal/3, resolve_modal_signature_conflict/3 | — | — | **O** | engine classification path (OQ-138 territory) |
| signature_grade/2, signature_severity/2 | — | — | **O** | signature-atom tables (OQ-98) |

## Family C — cs_* modules

| predicate | domain | sweep | class | evidence / reason |
|---|---|---|---|---|
| cs_pattern/3 | constraint | 119 one | **T** (registered) | explicit `no_pattern_match` + `cs_fields_absent` fallbacks (:102,:113) — the cs-layer total surface |
| cs_has_fields/1 | constraint | 89 fire | **P** (registered) | domain gate; the 30 non-CS stories fail correctly (their absence is typed by cs_pattern's cs_fields_absent) |
| cs_verdict/2 | constraint | 71 zero / 44 one / 4 multi | **P** (registered) | enumerates FIRED verdicts (several can co-fire); absence = none fired; the didn't-look case is carried by cs_pattern/3, not this predicate |
| cs_naturalized_mountain/1 | constraint | 0 fires | **P** (registered) | detection; zero-firing on this corpus (known dark-signature family) |
| cs_authority_masking/3 | constraint | 70 fire | **P** (registered) | detection with evidence args |
| cs_cover_story_active/2 | constraint | 5 fires | **P** (registered) | detection |
| cs_displaced_beneficiary/1 | constraint | 0 fires | **P** (registered) | detection; dark on this corpus |
| cs_grounding_mismatch/3 | constraint | 70 fire | **P** (registered) | detection |
| cs_has_axioms/1 | story-UID | 0 fires at constraint key | **P** (registered) + doc fix | `cs_axiom/3` facts are UID-keyed; the "+C" doc comment reads as constraint-name and the predicate NEVER fires at that key (witnessed). No consumers exist yet — the wrong-key trap is the DOC, fixed to say UID (OQ-57-class prevention, no behavior change) |
| cs_axiom_inconsistent/2 | story-UID | 0 fires at constraint key | **P** (registered) + doc fix | same key ambiguity, same fix; fires require authored cs_axiom_contradiction/2 pairs |
| cs_axiom_foreclosed/2 | story-UID | 7 one / 9 multi | **P** (registered) | detection; one solution per foreclosed atom |
| cs_drift_unacknowledged/2 | story-UID | 57 fire | **P** (registered) | detection |
| cs_drift_trajectory/3 | drift-UID (cs_drift_state authored) | 0 zero / 75 one / **8 multi** | **D → fixed → T** (registered, domain `drift_story`) | attractor table rows overlap: `(stable,minor,_)` matched BOTH `(stable,_,_)` and `(_,minor,_)` → duplicate `stable_pattern` (all 8 multis witnessed benign-identical), and LATENT conflict `(revival_pressure,minor,_)` → {stable_pattern, revival} order-dependent. Fix: row-disjointness guards preserving first-solution semantics for every combination (before/after full-table enumeration diff in fix commit) |
| cs_kernel_coverage/2 | kernel | 45 one | **T** (registered, domain `kernel`) | count construction |
| cs_kernel_obstruction_status/2 | kernel | 45 one | **T** (registered) | typed status incl. `singleton` |
| cs_reading_trifurcation/3 | kernel | 33 zero / 12 one | **P** (registered) | documented: FAILS on singleton kernels — "not contested, no verdict" (cs_trifurcation.pl:47); the typed surface for the singleton case is cs_kernel_obstruction_status/2 |
| cs_kernel_divergence/4 | kernel | 34 zero / 11 multi (≤224) | **P** (registered) | relational enumeration of divergence pairs |
| cs_readings_for_kernel/2, compare_kernel_readings/3, ctx_reading_verdict/2, pair_reading_agreement/7, divergence_pattern_list/4, cs_kernel_obstruction/4, stance predicates, report/export preds | — | — | **O** | lookups, pair-level relations, report drivers — not per-key verdict readings (recorded; the kernel-level verdict surfaces above are the registered ones) |

## Family D — report_generator read sites

| site | shape | class | disposition |
|---|---|---|---|
| :98–103 signature section | `catch(forall(...), E, print) ; true` — one member failure truncates the section silently | **D → fixed** | per-constraint `->  ;` OPEN marker (OQ-99 convention: print the absence, keep iterating) |
| :447–453 extraction_reading_line | `-> ... ; true` | **P** | documented deliberate silence ("absence is silence, never a fabricated value"); total surface extraction_state/2 |
| :462–468 r5_zombie_crosscheck_line | reads total q6_crosscheck; skips only q6_unmeasured explicitly | **P** | typed absence already |
| :285 catch(dr_type…, fail) | seat enumeration skips unknown/null with explicit filter | **P** | filter is explicit, not absorbing |
| :383 catch(level_gradient_divergence…, fail) | fallback question WITHOUT grid detail | **P** | graceful degradation, semantics unchanged (documented) |
| json_report.pl:360 gap_status default `undetermined(no_seats)` | typed default | **P** | typed, not fabricated-plausible |

## Defect count

2 silently-failing defects found and fixed (explain_signature/unknown + section truncation, as
one composed defect; cs_drift_trajectory row overlap), 1 doc-key trap fixed (cs_axiom UID
comments), 0 fixes touch the dr_type path (pipeline diff witnessed in the fix commits).
