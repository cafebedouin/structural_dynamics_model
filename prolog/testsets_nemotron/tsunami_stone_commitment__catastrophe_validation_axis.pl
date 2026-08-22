% ============================================================================
% CONSTRAINT STORY: tsunami_stone_commitment__catastrophe_validation_axis
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_tsunami_stone_commitment__catastrophe_validation_axis, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Constraint Identity Rule (DP-001: ε-Invariance) ---
% Each constraint story must have a single, stable base extractiveness (ε).
% If changing the observable used to evaluate this constraint would change ε,
% you are looking at two distinct constraints. Write separate .pl files for
% each, link them with affects_constraint/2, and document the relationship
% in both files' narrative context sections.
%
% The context tuple is CLOSED at arity 4: (P, T, E, S).
% Do not add measurement_basis, beneficiary/victim, or any other arguments.
% Linter Rule 23 enforces context/4.
%
% See: epsilon_invariance_principle.md

% --- Namespace Hooks (Required for loading) ---
:- multifile
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:theater_ratio/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:has_sunset_clause/1,
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: tsunami_stone_commitment__catastrophe_validation_axis
 *   human_readable: 2011 Tsunami as Binary Validation Test for Stone Inscription Commitments
 *   domain: disaster_anthropology/commitment_system/institutional_memory
 *
 * SUMMARY:
 *   The 2011 Tohoku tsunami provided a catastrophic empirical test for stone
 *   inscriptions (tsunami stones) placed along Japan's northeastern coast
 *   marking historical tsunami inundation lines and warning against building
 *   below them. Communities that heeded these inscriptions — notably Aneyoshi
 *   village, which built above the stone's warning line — suffered zero
 *   fatalities despite total structural destruction. Communities that built
 *   below the line suffered catastrophic losses. This reading treats the 2011
 *   event as a binary validation test: the commitment system (stone
 *   inscriptions as binding ancestral directives) either passes (communities
 *   survive) or fails (communities perish) based on a single physical event.
 *   The constraint is the tsunami itself as an epistemic adjudicator — a
 *   Mountain because the tsunami's physical reality and its differential
 *   impact across compliance/non-compliance groups would persist regardless
 *   of human interpretation, enforcement, or institutional maintenance.
 *
 * KEY AGENTS:
 *   - compliant_communities: Primary beneficiary (powerless/identity_locked) — survived by following stone guidance
 *   - non_compliant_communities: Primary victim (powerless/identity_locked) — perished by ignoring stone guidance
 *   - stone_inscriptions: Physical constraint (universal scope) — the commitment markers being tested
 *   - disaster_anthropologists: Analytical observer (analytical/analytical) — interpret the validation outcome
 *   - municipal_governments: Institutional actor (organized/national) — administer rebuilding and memory policy
 *   - ancestral_lineage_holders: Agenda setter (powerless/identity_locked) — transmitted the commitment across generations
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tsunami_stone_commitment__catastrophe_validation_axis, 0.05).
domain_priors:suppression_score(tsunami_stone_commitment__catastrophe_validation_axis, 0.02).
domain_priors:theater_ratio(tsunami_stone_commitment__catastrophe_validation_axis, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tsunami_stone_commitment__catastrophe_validation_axis, extractiveness, 0.05).
narrative_ontology:constraint_metric(tsunami_stone_commitment__catastrophe_validation_axis, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(tsunami_stone_commitment__catastrophe_validation_axis, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(tsunami_stone_commitment__catastrophe_validation_axis, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(tsunami_stone_commitment__catastrophe_validation_axis, resistance, 0.03).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tsunami_stone_commitment__catastrophe_validation_axis, mountain).
narrative_ontology:human_readable(tsunami_stone_commitment__catastrophe_validation_axis, "2011 Tsunami as Binary Validation Test for Stone Inscription Commitments").
narrative_ontology:topic_domain(tsunami_stone_commitment__catastrophe_validation_axis, "disaster_anthropology/commitment_system/institutional_memory").

domain_priors:emerges_naturally(tsunami_stone_commitment__catastrophe_validation_axis).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tsunami_stone_commitment__catastrophe_validation_axis, '856eae1d-ba0b-4589-8ea2-5f384093c742').
narrative_ontology:cs_kernel_codification('856eae1d-ba0b-4589-8ea2-5f384093c742', distributed).
narrative_ontology:cs_authority_grounding('856eae1d-ba0b-4589-8ea2-5f384093c742', lineage).
narrative_ontology:cs_interpretation_layer_present('856eae1d-ba0b-4589-8ea2-5f384093c742').
narrative_ontology:cs_reading_relation('856eae1d-ba0b-4589-8ea2-5f384093c742', tsunami_stone_commitment__behavioral_competence_reading, influences).
narrative_ontology:cs_reading_relation('856eae1d-ba0b-4589-8ea2-5f384093c742', tsunami_stone_commitment__commemorative_husk_reading, influences).
narrative_ontology:cs_axiom('856eae1d-ba0b-4589-8ea2-5f384093c742', foundational, catastrophe_provides_binary_epistemic_test).
narrative_ontology:cs_axiom_status(catastrophe_provides_binary_epistemic_test, holdable).
narrative_ontology:cs_axiom_grounding('856eae1d-ba0b-4589-8ea2-5f384093c742', catastrophe_provides_binary_epistemic_test, empirically_contingent).
narrative_ontology:cs_axiom('856eae1d-ba0b-4589-8ea2-5f384093c742', foundational, survival_outcome_validates_commitment_system).
narrative_ontology:cs_axiom_status(survival_outcome_validates_commitment_system, holdable).
narrative_ontology:cs_axiom_grounding('856eae1d-ba0b-4589-8ea2-5f384093c742', survival_outcome_validates_commitment_system, empirically_contingent).
narrative_ontology:cs_reference_frame('856eae1d-ba0b-4589-8ea2-5f384093c742', pre_2011_commitment_unvalidated).
narrative_ontology:cs_drift_state('856eae1d-ba0b-4589-8ea2-5f384093c742', post_2011_validation, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('856eae1d-ba0b-4589-8ea2-5f384093c742', '').
narrative_ontology:cs_kernel_id(tsunami_stone_commitment__catastrophe_validation_axis, tsunami_stone_commitment).

% --- Structural relationships ---
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(tsunami_stone_commitment__catastrophe_validation_axis, compliant_communities).
narrative_ontology:constraint_victim(tsunami_stone_commitment__catastrophe_validation_axis, non_compliant_communities).
narrative_ontology:constraint_vindicates(tsunami_stone_commitment__catastrophe_validation_axis, empirical_validation_of_ancestral_knowledge).
narrative_ontology:constraint_vindicates(tsunami_stone_commitment__catastrophe_validation_axis, commitment_system_survives_catastrophe_test).
narrative_ontology:constraint_vindicates(tsunami_stone_commitment__catastrophe_validation_axis, intergenerational_memory_has_operational_value).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Communities (e.g., Aneyoshi) that maintained ancestral building restrictions above the stone marker lines. Their identity is fused with the commitment — the stones mark not just safe ground but the boundary of communal continuity. Exit would mean abandoning the ancestral contract that defines them. The 2011 tsunami validated their commitment: zero fatalities despite total property destruction.
narrative_ontology:constraint_stakeholder(tsunami_stone_commitment__catastrophe_validation_axis, compliant_communities, beneficiary,
    powerless, generational, identity_locked, local).

% Communities that built below the stone markers, either through population pressure, economic necessity, or institutional forgetting. They paid the ultimate cost in 2011 — catastrophic fatality rates. Their identity was also fused with place, but the commitment had been weakened or overridden by modern planning, economic development, or administrative rezoning. The tsunami falsified their arrangement.
narrative_ontology:constraint_stakeholder(tsunami_stone_commitment__catastrophe_validation_axis, non_compliant_communities, payer,
    powerless, generational, identity_locked, local).

% The physical stone markers themselves — not agents but the constraint objects being tested. They exist as carved inscriptions on bedrock, marking historical inundation lines with warnings like 'Do not build below this point.' Their 'situation' is to endure and be legible. The 2011 tsunami tested their guidance against physical reality. They neither benefit nor pay — they are the measuring stick.
narrative_ontology:constraint_stakeholder(tsunami_stone_commitment__catastrophe_validation_axis, stone_inscriptions, observer,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(tsunami_stone_commitment__catastrophe_validation_axis, stone_inscriptions).

% Researchers who study the stone inscriptions, the 2011 outcomes, and the commitment system dynamics. They gain epistemic authority and research funding from the validation narrative. Their exit is analytical — they can change frameworks, but their careers are built on this kernel. They are the primary producers of the catastrophe_validation_axis reading.
narrative_ontology:constraint_stakeholder(tsunami_stone_commitment__catastrophe_validation_axis, disaster_anthropologists, observer,
    analytical, biographical, analytical, global).

% Local and prefectural administrations managing post-2011 reconstruction, memorialization, and disaster planning. They administer whether stone guidance is incorporated into rebuilding zoning, whether stones are preserved as heritage, and how the validation narrative shapes policy. They benefit from the legitimization of ancestral knowledge (UNESCO bids, central government funding) but bear costs of maintaining the commitment infrastructure. Their exit is constrained by electoral cycles and bureaucratic mandate.
narrative_ontology:constraint_stakeholder(tsunami_stone_commitment__catastrophe_validation_axis, municipal_governments, agenda_setter,
    organized, biographical, constrained, national).

% Families and community elders who transmitted the stone commitments across generations — maintaining the oral injunctions, enforcing building norms, preserving the stones themselves. Their authority derives from the lineage of transmission. The 2011 validation vindicated their stewardship. They are identity-locked: abandoning the commitment would dissolve their role. They gain epistemic vindication but no material extraction.
narrative_ontology:constraint_stakeholder(tsunami_stone_commitment__catastrophe_validation_axis, ancestral_lineage_holders, agenda_setter,
    powerless, generational, identity_locked, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The stone inscriptions coordinate intergenerational risk knowledge — they transmit tsunami inundation data across centuries without writing systems, bureaucracy, or digital media. They solve the problem: how does a community remember a 1000-year event?
% TRANSFER_FUNCTION: The 2011 tsunami transferred survival from compliant communities to non-compliant ones — a binary outcome where the constraint (tsunami physics) enforced the commitment's terms. No agent transfers resources; the physical event distributes consequences according to compliance.
% ABSENT_VOICES: The dead of non-compliant communities — they would object to the characterization of their demise as 'validation.' Also absent: communities that *would have* complied but were displaced, urbanized, or administratively overridden before 2011. Their absence is structural — the commitment system failed to reach them, not that they rejected it.
% DISAPPEARANCE_RATIONALE: If the 2011 tsunami had not occurred (or had occurred at a different magnitude/timing), the stone commitments would remain untested hypotheses. The validation reading would not exist. The behavioral_competence_reading and commemorative_husk_reading would lack their decisive empirical anchor. The commitment system's epistemic status would remain contested without the binary test.
% FOUNDING_PROBLEM: How to transmit tsunami risk knowledge across generations that exceed human lifespan, without relying on state infrastructure, literacy, or continuous institutional maintenance.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is corroborated by the stones' own inscriptions (which explicitly reference ancestral warnings), by the geological record of recurring tsunamis (869 Jogan, 1611 Keicho, 1896 Meiji-Sanriku, 1933 Showa-Sanriku), and by disaster engineering literature on the limits of structural defenses (seawalls failed in 2011). No beneficiary of the validation narrative disputes that the founding problem was real — the dispute is whether the stones *solved* it or merely *marked* it.
narrative_ontology:disappearance_verdict(tsunami_stone_commitment__catastrophe_validation_axis, world_rearranges).
narrative_ontology:founding_problem_status(tsunami_stone_commitment__catastrophe_validation_axis, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(tsunami_stone_commitment__catastrophe_validation_axis, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(tsunami_stone_commitment__catastrophe_validation_axis, 'none', 1).
narrative_ontology:epsilon_provenance(tsunami_stone_commitment__catastrophe_validation_axis, 0.05, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(tsunami_stone_commitment__catastrophe_validation_axis_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(tsunami_stone_commitment__catastrophe_validation_axis, ExtMetricName, E),
    domain_priors:suppression_score(tsunami_stone_commitment__catastrophe_validation_axis, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(tsunami_stone_commitment__catastrophe_validation_axis),
    narrative_ontology:constraint_metric(tsunami_stone_commitment__catastrophe_validation_axis, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(tsunami_stone_commitment__catastrophe_validation_axis, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(tsunami_stone_commitment__catastrophe_validation_axis_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is near-zero (0.05) because the tsunami as physical event extracts nothing — it simply *is*. The validation is a property of the physical interaction between water and land, not a transfer between agents. Suppression is negligible (0.02) because no agent enforces the tsunami; the constraint is the wave itself. Theater ratio is low (0.08) because the validation event was genuine, not performative — though post-hoc commemoration may inflate this over time (captured in the rising theater trajectory). Accessibility collapse is extreme (0.92) because once the tsunami occurred, the validation is irreversible — no alternative interpretation of the physical outcome can restore the lives lost in non-compliant zones. Resistance is near-zero (0.03) because the physical constraint meets no resistance — it simply destroys what is in its path. The claimed Mountain type is structurally honest: the tsunami as validation device is a natural law operating on a human commitment system.
 *
 * DIRECTIONALITY LOGIC:
 *   The tsunami as physical event has no directionality — it is the adjudication mechanism itself. Directionality emerges only in the *readings* of the event. This reading (catastrophe_validation_axis) treats compliant communities as beneficiaries of the validation (their commitment was vindicated) and non-compliant communities as victims (their non-compliance was falsified). But this is a reading-level assignment, not a property of the tsunami. The engine will derive directionality from the stakeholder declarations: compliant communities get low d (beneficiary), non-compliant get high d (victim), anthropologists get d≈0.5 (analytical), municipal governments get d≈0.3 (institutional actors managing aftermath).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint does not suffer mandatrophy — it is not a mandate that outlived its function. The tsunami was a one-time empirical test. The *reading* of that test as validation may serve ongoing institutional functions (legitimizing ancestral knowledge, justifying heritage preservation, funding disaster anthropology), but the constraint itself (the 2011 tsunami as binary test) is a historical event with fixed properties. Mandatrophy would apply to the *institutionalization* of the validation narrative, not to the validation event itself.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_instantiation,
    'Does this constraint instantiate a genuine Mountain (physical catastrophe as epistemic adjudicator) or does it serve a constructed reading of the tsunami_stone_commitment kernel?',
    'Compare survival outcomes across communities with and without stone inscriptions controlling for topography, evacuation infrastructure, and warning system access. If the stone-inscription communities show statistically significant survival advantage attributable to the inscriptions'' guidance, the validation is empirical; if not, the validation reading is a constructed narrative.',
    'If constructed, this reading''s claimed Mountain status is a false summit — the constraint would be a post-hoc legitimization narrative (piton or scaffold) rather than a physical constraint. The engine''s false_summit_mountain signature would reclassify via beneficiary detection.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_instantiation, empirical, 'Whether the 2011 tsunami actually validated the stone commitments or whether the validation narrative was constructed post-hoc').

omega_variable(
    beneficiary_structure_ambiguity,
    'Who benefits from the tsunami validation reading — communities preserving ancestral knowledge, institutional actors seeking legitimization, or disaster anthropology as a discipline?',
    'Trace resource flows: research funding, heritage designation, tourism revenue, policy influence. Map which actors gain material or epistemic authority from the validation narrative.',
    'If identifiable beneficiaries exist (municipalities gaining UNESCO status, researchers building careers, agencies justifying budgets), the Mountain claim carries FSM risk — the constraint may be a false summit serving those beneficiaries.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_structure_ambiguity, empirical, 'Whether the validation reading has identifiable beneficiaries that would trigger false summit detection').

omega_variable(
    binary_validation_epistemology,
    'Is a single catastrophic event a legitimate binary test for a commitment system''s validity, or does this reading impose a falsificationist epistemology on a system that operates on different grounds?',
    'Analyze the commitment system''s own criteria for validity — does it treat catastrophic survival as necessary/sufficient proof, or does it operate on continuity of practice, transmission fidelity, or ancestral authority?',
    'If the system''s internal logic does not recognize binary catastrophe testing, this reading imposes an external epistemic frame. The constraint would then be a reading artifact, not a structural feature of the commitment system itself.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(binary_validation_epistemology, conceptual, 'Whether binary catastrophe validation is internal to the commitment system or externally imposed').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tsunami_stone_commitment__catastrophe_validation_axis, 2011, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tsun_tr_t2011, tsunami_stone_commitment__catastrophe_validation_axis, theater_ratio, 2011, 0.05).
narrative_ontology:measurement(tsun_tr_t2013, tsunami_stone_commitment__catastrophe_validation_axis, theater_ratio, 2013, 0.06).
narrative_ontology:measurement(tsun_tr_t2016, tsunami_stone_commitment__catastrophe_validation_axis, theater_ratio, 2016, 0.07).
narrative_ontology:measurement(tsun_tr_t2019, tsunami_stone_commitment__catastrophe_validation_axis, theater_ratio, 2019, 0.07).
narrative_ontology:measurement(tsun_tr_t2022, tsunami_stone_commitment__catastrophe_validation_axis, theater_ratio, 2022, 0.08).
narrative_ontology:measurement(tsun_tr_t2026, tsunami_stone_commitment__catastrophe_validation_axis, theater_ratio, 2026, 0.08).

% Extraction over time
narrative_ontology:measurement(tsun_be_t2011, tsunami_stone_commitment__catastrophe_validation_axis, base_extractiveness, 2011, 0.02).
narrative_ontology:measurement(tsun_be_t2013, tsunami_stone_commitment__catastrophe_validation_axis, base_extractiveness, 2013, 0.03).
narrative_ontology:measurement(tsun_be_t2016, tsunami_stone_commitment__catastrophe_validation_axis, base_extractiveness, 2016, 0.04).
narrative_ontology:measurement(tsun_be_t2019, tsunami_stone_commitment__catastrophe_validation_axis, base_extractiveness, 2019, 0.04).
narrative_ontology:measurement(tsun_be_t2022, tsunami_stone_commitment__catastrophe_validation_axis, base_extractiveness, 2022, 0.05).
narrative_ontology:measurement(tsun_be_t2026, tsunami_stone_commitment__catastrophe_validation_axis, base_extractiveness, 2026, 0.05).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(tsunami_stone_commitment__catastrophe_validation_axis, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tsunami_stone_commitment__catastrophe_validation_axis, information_standard).
narrative_ontology:boltzmann_floor_override(tsunami_stone_commitment__catastrophe_validation_axis, 0.02).
narrative_ontology:affects_constraint(tsunami_stone_commitment__catastrophe_validation_axis, tsunami_stone_commitment__behavioral_competence_reading).
narrative_ontology:affects_constraint(tsunami_stone_commitment__catastrophe_validation_axis, tsunami_stone_commitment__commemorative_husk_reading).

% DUAL FORMULATION NOTE:
% This reading (catastrophe_validation_axis) decomposes the tsunami_stone_commitment kernel by treating the 2011 tsunami as an independent epistemic adjudicator (Mountain) that validates or falsifies the commitment system. The behavioral_competence_reading treats the stones as live coordination mechanism (Rope/Tangled Rope). The commemorative_husk_reading treats the stones as degraded artifacts (Piton). The catastrophe validation reading provides the empirical anchor both sibling readings must account for — the behavioral reading claims the validation proves the system works; the commemorative reading must explain away the validation correlation. This reading's ε (0.05) is structurally distinct from the behavioral reading's ε (moderate, active enforcement costs) and the commemorative reading's ε (low, but with high theater).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(tsunami_stone_commitment__catastrophe_validation_axis, powerless, 0.15).
constraint_indexing:directionality_override(tsunami_stone_commitment__catastrophe_validation_axis, analytical, 0.5).
constraint_indexing:directionality_override(tsunami_stone_commitment__catastrophe_validation_axis, organized, 0.3).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
