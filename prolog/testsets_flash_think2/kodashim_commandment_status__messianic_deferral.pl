% ============================================================================
% CONSTRAINT STORY: kodashim_commandment_status__messianic_deferral
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kodashim_commandment_status__messianic_deferral, []).

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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: kodashim_commandment_status__messianic_deferral
 *   human_readable: Kodashim Commandment Status: Messianic Deferral Reading
 *   domain: religious_studies/halakhic_theory/commitment_system_analysis
 *
 * SUMMARY:
 *   This constraint describes the 'messianic deferral' reading of the
 *   Kodashim (Temple sacrifice) commandments within Jewish law. It posits
 *   that these commandments, while currently impossible to perform due to the
 *   absence of the Temple, are not obsolete but merely suspended. Active
 *   study of these laws is mandated to maintain readiness for their future
 *   restoration in the messianic era. This reading functions as a 'scaffold'
 *   because it provides temporary support for the continuity of a core
 *   religious obligation, justified by a future, transitional state (the
 *   messianic era) rather than the current steady state.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kodashim_commandment_status__messianic_deferral, 0.65).
domain_priors:suppression_score(kodashim_commandment_status__messianic_deferral, 0.88).
domain_priors:theater_ratio(kodashim_commandment_status__messianic_deferral, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kodashim_commandment_status__messianic_deferral, extractiveness, 0.65).
narrative_ontology:constraint_metric(kodashim_commandment_status__messianic_deferral, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(kodashim_commandment_status__messianic_deferral, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kodashim_commandment_status__messianic_deferral, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(kodashim_commandment_status__messianic_deferral, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kodashim_commandment_status__messianic_deferral, scaffold).
narrative_ontology:human_readable(kodashim_commandment_status__messianic_deferral, "Kodashim Commandment Status: Messianic Deferral Reading").
narrative_ontology:topic_domain(kodashim_commandment_status__messianic_deferral, "religious_studies/halakhic_theory/commitment_system_analysis").

domain_priors:requires_active_enforcement(kodashim_commandment_status__messianic_deferral).
narrative_ontology:has_sunset_clause(kodashim_commandment_status__messianic_deferral).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kodashim_commandment_status__messianic_deferral, '3957a317-71e8-464b-89e5-69d93a74a910').
narrative_ontology:cs_kernel_codification('3957a317-71e8-464b-89e5-69d93a74a910', fixed_text).
narrative_ontology:cs_authority_grounding('3957a317-71e8-464b-89e5-69d93a74a910', lineage).
narrative_ontology:cs_interpretation_layer_present('3957a317-71e8-464b-89e5-69d93a74a910').
narrative_ontology:cs_reading_relation('3957a317-71e8-464b-89e5-69d93a74a910', kodashim_commandment_status__performance_only, forecloses).
narrative_ontology:cs_reading_relation('3957a317-71e8-464b-89e5-69d93a74a910', kodashim_commandment_status__study_as_performance, influences).
narrative_ontology:cs_axiom('3957a317-71e8-464b-89e5-69d93a74a910', foundational, messianic_era_restoration_of_temple_service).
narrative_ontology:cs_axiom_status(messianic_era_restoration_of_temple_service, holdable).
narrative_ontology:cs_axiom_grounding('3957a317-71e8-464b-89e5-69d93a74a910', messianic_era_restoration_of_temple_service, theological).
narrative_ontology:cs_axiom('3957a317-71e8-464b-89e5-69d93a74a910', foundational, halakhic_continuity_through_study_as_readiness).
narrative_ontology:cs_axiom_status(halakhic_continuity_through_study_as_readiness, holdable).
narrative_ontology:cs_axiom_grounding('3957a317-71e8-464b-89e5-69d93a74a910', halakhic_continuity_through_study_as_readiness, conventional).
narrative_ontology:cs_reference_frame('3957a317-71e8-464b-89e5-69d93a74a910', post_temple_destruction_halakha).
narrative_ontology:cs_drift_state('3957a317-71e8-464b-89e5-69d93a74a910', contemporary_halakhic_discourse, gap(stable, minor, true)).
narrative_ontology:cs_created_at('3957a317-71e8-464b-89e5-69d93a74a910', '').
narrative_ontology:cs_kernel_id(kodashim_commandment_status__messianic_deferral, kodashim_commandment_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kodashim_commandment_status__messianic_deferral, halakhic_authorities).
narrative_ontology:constraint_beneficiary(kodashim_commandment_status__messianic_deferral, religious_scholars).
narrative_ontology:constraint_beneficiary(kodashim_commandment_status__messianic_deferral, future_generations).
narrative_ontology:constraint_victim(kodashim_commandment_status__messianic_deferral, devout_adherents).
narrative_ontology:constraint_victim(kodashim_commandment_status__messianic_deferral, present_generation_needs).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interpret and transmit the halakha (Jewish law), including the status of Temple-dependent commandments. They enforce the norm of study as preparation for future restoration and manage the community's theological understanding of deferral. They benefit from the continuity of their interpretive authority.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__messianic_deferral, halakhic_authorities, agenda_setter,
    institutional, generational, identity_locked, global).

% Dedicate their lives to studying the intricate laws of Kodashim (Temple sacrifices), maintaining the intellectual tradition and ensuring readiness. Their work is central to the constraint's operation and their professional identity is fused with this ongoing study. They benefit from the elevated status of their intellectual pursuit.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__messianic_deferral, religious_scholars, beneficiary,
    organized, biographical, identity_locked, global).

% Adhere to the halakhic rulings, engaging in study and prayer in anticipation of the Temple's rebuilding. They bear the opportunity cost of deferred spiritual fulfillment and the subordination of present-day material needs to a future, abstract goal. Their identity is deeply tied to the messianic hope.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__messianic_deferral, devout_adherents, payer,
    moderate, biographical, identity_locked, global).

% Represents the collective material and spiritual needs of the current generation that might be prioritized differently if the focus were not on future restoration. This 'agent' bears the diffuse cost of resources and attention diverted to abstract future readiness.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__messianic_deferral, present_generation_needs, payer,
    powerless, immediate, trapped, global).
narrative_ontology:stakeholder_non_agent(kodashim_commandment_status__messianic_deferral, present_generation_needs).

% Will inherit a preserved and understood tradition, ready for implementation upon the Temple's rebuilding. They are the ultimate beneficiaries of the present generation's commitment to study and deferral, receiving the continuity of the halakhic chain.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__messianic_deferral, future_generations, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(kodashim_commandment_status__messianic_deferral, future_generations).

% View the focus on future Temple service as anachronistic or a diversion from contemporary social and ethical concerns. Their perspective is outside the halakhic framework and their arguments are not considered within the internal discourse of the constraint.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__messianic_deferral, secular_critics, excluded,
    analytical, biographical, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(kodashim_commandment_status__messianic_deferral, halakhic_authorities).
narrative_ontology:fixing_cost_class(kodashim_commandment_status__messianic_deferral, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the Jewish community's spiritual and intellectual life around the shared anticipation of the messianic era and the restoration of Temple service, ensuring the continuity of halakhic knowledge and readiness for its future implementation.
% TRANSFER_FUNCTION: Transfers significant intellectual and spiritual resources (time, scholarly effort, communal focus) from addressing immediate present-day needs to maintaining readiness for a future, divinely ordained event. It also transfers the burden of deferred fulfillment to the devout adherents.
% ABSENT_VOICES: Secular critics and those who prioritize immediate social justice or material welfare over abstract future religious readiness are largely excluded from the discourse. They would argue for a re-evaluation of priorities and the allocation of resources to present-day challenges.
% DISAPPEARANCE_RATIONALE: If this constraint vanished, the entire framework of messianic anticipation and the continuity of Temple-related halakha would collapse. The spiritual and intellectual life of the community would be profoundly reoriented, potentially leading to a crisis of identity and purpose regarding the future of Jewish practice.
% FOUNDING_PROBLEM: The destruction of the Second Temple left a void in Jewish religious life, rendering many commandments (Kodashim) impossible to perform. The problem was how to maintain the integrity, relevance, and knowledge of these laws without the physical means to observe them, ensuring their eventual restoration.
% FOUNDING_PROBLEM_CORROBORATION: Historical rabbinic texts (Talmud, Maimonides) and centuries of theological consensus attest to the ongoing nature of the Temple's absence and the messianic anticipation. This is corroborated by the continued existence of the Temple Mount in Jerusalem and the absence of Temple service, which are observable facts outside the immediate benefiting parties.
narrative_ontology:disappearance_verdict(kodashim_commandment_status__messianic_deferral, world_rearranges).
narrative_ontology:founding_problem_status(kodashim_commandment_status__messianic_deferral, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kodashim_commandment_status__messianic_deferral, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(kodashim_commandment_status__messianic_deferral, 'none', 1).
narrative_ontology:epsilon_provenance(kodashim_commandment_status__messianic_deferral, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kodashim_commandment_status__messianic_deferral_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(kodashim_commandment_status__messianic_deferral, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(kodashim_commandment_status__messianic_deferral_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.65) due to the significant opportunity cost: resources (time, intellectual effort, communal focus) are directed towards abstract future readiness rather than immediate needs or alternative forms of spiritual expression. Suppression is high (0.88) because the physical impossibility of performing the commandments (absence of the Temple) is an absolute barrier, and the halakhic framework actively suppresses alternative interpretations that would declare the laws obsolete or allow for symbolic substitutes. Theater ratio is low (0.10) because the study is considered a genuine and necessary act of preparation, not a mere performance or empty ritual. Accessibility collapse is very high (0.95) as actual performance is impossible, and resistance is low (0.15) because this reading is a widely accepted and deeply ingrained theological position within the tradition.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of halakhic authorities and scholars, this constraint is a vital mechanism for preserving tradition and ensuring future continuity. From the perspective of devout adherents, it involves a significant personal and communal sacrifice, though one accepted as part of their faith. Secular critics, if included, would likely view it as an extractive diversion of resources from present-day concerns, but their voices are structurally excluded from the internal discourse.
 *
 * DIRECTIONALITY LOGIC:
 *   Halakhic authorities and religious scholars are beneficiaries; they maintain their interpretive authority and professional identity through the ongoing study and transmission of these laws. Devout adherents and 'present generation needs' (as a conceptual victim group) are payers, bearing the opportunity cost of deferred fulfillment and the subordination of immediate concerns to a distant future. Future generations are also beneficiaries, as they will inherit a preserved and understood tradition. The 'identity_locked' exit option for adherents and scholars reflects the deep fusion of their self-concept with this religious framework.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    opportunity_cost_quantification,
    'What is the precise economic and social opportunity cost of dedicating significant communal resources to the study of Kodashim laws, rather than to other communal needs?',
    'Detailed economic and sociological studies comparing resource allocation in communities with differing theological stances on messianic anticipation and halakhic priorities.',
    'A higher quantified opportunity cost would strengthen the ''extraction'' component of the constraint, potentially shifting its classification towards a Tangled Rope if the coordination function is deemed less central than the cost borne.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(opportunity_cost_quantification, empirical, 'Quantifying the real-world costs of deferred fulfillment and resource allocation.').

omega_variable(
    readiness_vs_fulfillment_ambiguity,
    'Is ''maintaining readiness'' for future Temple service truly a fulfillment of the commandment''s spirit, or a distinct, albeit related, religious obligation?',
    'Theological debate and re-interpretation within the halakhic framework, potentially leading to a re-categorization of study''s role (e.g., as an independent commandment rather than a substitute for Kodashim).',
    'If study is deemed a distinct obligation, the ''scaffold'' function (temporary support for transition) might weaken, as the direct link between present study and future performance becomes less central, potentially reclassifying it as a Piton if the original function is seen as atrophied.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(readiness_vs_fulfillment_ambiguity, conceptual, 'Theological distinction between preparatory study and actual commandment fulfillment.').

omega_variable(
    messianic_era_contingency,
    'Is the ''sunset clause'' (messianic era) a sufficiently concrete and universally accepted temporal boundary to justify a ''scaffold'' classification, given its theological and indeterminate nature?',
    'Analysis of other ''scaffold'' constraints with indeterminate or theological sunset clauses, and their classification outcomes. This is a meta-classification question for the framework itself.',
    'If the messianic era is deemed too indeterminate, the ''scaffold'' classification might be challenged, potentially reclassifying it as a Tangled Rope or even a Snare if the deferral is seen as primarily serving the interests of the halakhic authorities without a clear, actionable end-state.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(messianic_era_contingency, conceptual, 'Theological vs. empirical nature of the ''sunset clause'' for scaffold classification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kodashim_commandment_status__messianic_deferral, 100, 200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(koda_tr_t100, kodashim_commandment_status__messianic_deferral, theater_ratio, 100, 0.1).
narrative_ontology:measurement(koda_tr_t120, kodashim_commandment_status__messianic_deferral, theater_ratio, 120, 0.1).
narrative_ontology:measurement(koda_tr_t140, kodashim_commandment_status__messianic_deferral, theater_ratio, 140, 0.1).
narrative_ontology:measurement(koda_tr_t160, kodashim_commandment_status__messianic_deferral, theater_ratio, 160, 0.1).
narrative_ontology:measurement(koda_tr_t180, kodashim_commandment_status__messianic_deferral, theater_ratio, 180, 0.1).
narrative_ontology:measurement(koda_tr_t200, kodashim_commandment_status__messianic_deferral, theater_ratio, 200, 0.1).

% Extraction over time
narrative_ontology:measurement(koda_be_t100, kodashim_commandment_status__messianic_deferral, base_extractiveness, 100, 0.6).
narrative_ontology:measurement(koda_be_t120, kodashim_commandment_status__messianic_deferral, base_extractiveness, 120, 0.62).
narrative_ontology:measurement(koda_be_t140, kodashim_commandment_status__messianic_deferral, base_extractiveness, 140, 0.63).
narrative_ontology:measurement(koda_be_t160, kodashim_commandment_status__messianic_deferral, base_extractiveness, 160, 0.64).
narrative_ontology:measurement(koda_be_t180, kodashim_commandment_status__messianic_deferral, base_extractiveness, 180, 0.65).
narrative_ontology:measurement(koda_be_t200, kodashim_commandment_status__messianic_deferral, base_extractiveness, 200, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(koda_su_t100, kodashim_commandment_status__messianic_deferral, suppression_requirement, 100, 0.85).
narrative_ontology:measurement(koda_su_t120, kodashim_commandment_status__messianic_deferral, suppression_requirement, 120, 0.86).
narrative_ontology:measurement(koda_su_t140, kodashim_commandment_status__messianic_deferral, suppression_requirement, 140, 0.87).
narrative_ontology:measurement(koda_su_t160, kodashim_commandment_status__messianic_deferral, suppression_requirement, 160, 0.88).
narrative_ontology:measurement(koda_su_t180, kodashim_commandment_status__messianic_deferral, suppression_requirement, 180, 0.88).
narrative_ontology:measurement(koda_su_t200, kodashim_commandment_status__messianic_deferral, suppression_requirement, 200, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kodashim_commandment_status__messianic_deferral, identity_coordination).
narrative_ontology:affects_constraint(kodashim_commandment_status__messianic_deferral, kodashim_commandment_status__performance_only).
narrative_ontology:affects_constraint(kodashim_commandment_status__messianic_deferral, kodashim_commandment_status__study_as_performance).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'kodashim_commandment_status' kernel. This 'messianic_deferral' reading emphasizes study as preparation for future restoration, distinct from readings that see the commandment as merely suspended or study as equivalent to performance.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
