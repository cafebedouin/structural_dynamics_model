% ============================================================================
% CONSTRAINT STORY: secession_legitimacy_boundary__constitutional_impossibility_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_secession_legitimacy_boundary__constitutional_impossibility_reading, []).

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
 *   constraint_id: secession_legitimacy_boundary__constitutional_impossibility_reading
 *   human_readable: Constitutional Impossibility of Unilateral Secession
 *   domain: political_economy/federalism/constitutional_law
 *
 * SUMMARY:
 *   This constraint story instantiates the 'constitutional impossibility'
 *   reading of the secession legitimacy boundary kernel. From this
 *   perspective, unilateral secession is categorically impermissible, and the
 *   federal constitution is the supreme and unchangeable arbiter of state
 *   integrity. Any claims of 'extraction' by separatist movements are deemed
 *   illegitimate, as the federal government's operation is seen as the
 *   legitimate exercise of its constitutional mandate. The constraint is
 *   claimed as a Mountain due to its asserted unchangeable nature, but the
 *   presence of beneficiaries will trigger False Summit Mountain detection.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(secession_legitimacy_boundary__constitutional_impossibility_reading, 0.15).
domain_priors:suppression_score(secession_legitimacy_boundary__constitutional_impossibility_reading, 0.88).
domain_priors:theater_ratio(secession_legitimacy_boundary__constitutional_impossibility_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(secession_legitimacy_boundary__constitutional_impossibility_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__constitutional_impossibility_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__constitutional_impossibility_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(secession_legitimacy_boundary__constitutional_impossibility_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__constitutional_impossibility_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(secession_legitimacy_boundary__constitutional_impossibility_reading, mountain).
narrative_ontology:human_readable(secession_legitimacy_boundary__constitutional_impossibility_reading, "Constitutional Impossibility of Unilateral Secession").
narrative_ontology:topic_domain(secession_legitimacy_boundary__constitutional_impossibility_reading, "political_economy/federalism/constitutional_law").

domain_priors:requires_active_enforcement(secession_legitimacy_boundary__constitutional_impossibility_reading).
domain_priors:emerges_naturally(secession_legitimacy_boundary__constitutional_impossibility_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(secession_legitimacy_boundary__constitutional_impossibility_reading, '1e7de404-5fbc-4c12-a820-01692859326f').
narrative_ontology:cs_kernel_codification('1e7de404-5fbc-4c12-a820-01692859326f', fixed_text).
narrative_ontology:cs_authority_grounding('1e7de404-5fbc-4c12-a820-01692859326f', lineage).
narrative_ontology:cs_interpretation_layer_present('1e7de404-5fbc-4c12-a820-01692859326f').
narrative_ontology:cs_reading_relation('1e7de404-5fbc-4c12-a820-01692859326f', secession_legitimacy_boundary__popular_sovereignty_reading, forecloses).
narrative_ontology:cs_reading_relation('1e7de404-5fbc-4c12-a820-01692859326f', secession_legitimacy_boundary__grievance_threshold_reading, forecloses).
narrative_ontology:cs_reading_relation('1e7de404-5fbc-4c12-a820-01692859326f', secession_legitimacy_boundary__treaty_primacy_reading, forecloses).
narrative_ontology:cs_axiom('1e7de404-5fbc-4c12-a820-01692859326f', foundational, federal_union_perpetual).
narrative_ontology:cs_axiom_status(federal_union_perpetual, holdable).
narrative_ontology:cs_axiom_grounding('1e7de404-5fbc-4c12-a820-01692859326f', federal_union_perpetual, deontological).
narrative_ontology:cs_axiom('1e7de404-5fbc-4c12-a820-01692859326f', foundational, constitutional_text_supreme).
narrative_ontology:cs_axiom_status(constitutional_text_supreme, holdable).
narrative_ontology:cs_axiom_grounding('1e7de404-5fbc-4c12-a820-01692859326f', constitutional_text_supreme, conventional).
narrative_ontology:cs_reference_frame('1e7de404-5fbc-4c12-a820-01692859326f', original_constitutional_compact).
narrative_ontology:cs_drift_state('1e7de404-5fbc-4c12-a820-01692859326f', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('1e7de404-5fbc-4c12-a820-01692859326f', '').
narrative_ontology:cs_kernel_id(secession_legitimacy_boundary__constitutional_impossibility_reading, secession_legitimacy_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(secession_legitimacy_boundary__constitutional_impossibility_reading, federal_government).
narrative_ontology:constraint_beneficiary(secession_legitimacy_boundary__constitutional_impossibility_reading, unionist_citizens).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__constitutional_impossibility_reading, separatist_movements).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The ultimate authority that defines and enforces the constitutional limits on secession. Benefits from the stability and territorial integrity of the union, and the continued collection of taxes and resources from all constituent territories. Views any unilateral secession attempt as an illegal act to be suppressed.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__constitutional_impossibility_reading, federal_government, agenda_setter,
    institutional, civilizational, arbitrage, national).

% Citizens who identify with the federal union and benefit from its stability, common market, and shared identity. They view the constitutional prohibition on unilateral secession as a fundamental guarantee of their national identity and collective future. They would resist any attempt to break up the union.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__constitutional_impossibility_reading, unionist_citizens, beneficiary,
    organized, generational, mobile, national).

% Groups advocating for the unilateral secession of a constituent territory. From this reading's perspective, they bear the cost of being denied a legitimate path to unilateral independence, facing legal and political suppression from the federal government. Their claims of 'extraction' are deemed illegitimate by this reading.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__constitutional_impossibility_reading, separatist_movements, payer,
    organized, biographical, constrained, regional).

% Legal experts who interpret the constitution as unequivocally prohibiting unilateral secession, emphasizing the permanence of the union and the supremacy of federal law. They provide the intellectual grounding for the federal government's position.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__constitutional_impossibility_reading, constitutional_scholars_impossibility_reading, observer,
    analytical, civilizational, analytical, universal).

% International organizations and legal scholars who observe and comment on secession disputes, often balancing principles of self-determination with territorial integrity. While they may offer opinions, this reading asserts their authority does not supersede national constitutional law.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__constitutional_impossibility_reading, international_law_bodies, observer,
    institutional, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To maintain the territorial integrity, legal continuity, and political stability of the federal state by establishing a clear, singular, and legitimate process for any change to its composition, thereby preventing internal fragmentation and ensuring collective governance.
% TRANSFER_FUNCTION: Legitimizes the federal government's claim to jurisdiction, resources, and authority over all constituent territories, preventing any transfer of these to unilaterally seceding entities. It transfers the burden of proof and legal legitimacy onto any party seeking to alter the union's composition.
% ABSENT_VOICES: Proponents of popular sovereignty, grievance-based secession, and indigenous treaty primacy are present in public discourse but their claims are deemed constitutionally illegitimate by this reading, effectively excluding their arguments from the legitimate constitutional framework for secession.
% DISAPPEARANCE_RATIONALE: If the constitutional prohibition on unilateral secession vanished, the legal and political framework for national unity would collapse. This would immediately legitimize separatist claims, leading to widespread challenges to federal authority, potential territorial fragmentation, and a complete reorganization of the state's political and economic landscape.
% FOUNDING_PROBLEM: The founding problem was to establish a perpetual union, preventing internal dissolution through unilateral action by constituent parts, and ensuring long-term stability, collective security, and shared economic prosperity for all members.
% FOUNDING_PROBLEM_CORROBORATION: The federal government, federal courts, and unionist political parties consistently attest that the threat of internal dissolution and fragmentation remains a live concern, citing historical precedents and ongoing separatist movements. Constitutional scholars supporting this reading also corroborate the original intent of the framers to create an indissoluble union.
narrative_ontology:disappearance_verdict(secession_legitimacy_boundary__constitutional_impossibility_reading, world_rearranges).
narrative_ontology:founding_problem_status(secession_legitimacy_boundary__constitutional_impossibility_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(secession_legitimacy_boundary__constitutional_impossibility_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(secession_legitimacy_boundary__constitutional_impossibility_reading, 'none', 1).
narrative_ontology:epsilon_provenance(secession_legitimacy_boundary__constitutional_impossibility_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(secession_legitimacy_boundary__constitutional_impossibility_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(secession_legitimacy_boundary__constitutional_impossibility_reading, ExtMetricName, E),
    domain_priors:suppression_score(secession_legitimacy_boundary__constitutional_impossibility_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(secession_legitimacy_boundary__constitutional_impossibility_reading),
    narrative_ontology:constraint_metric(secession_legitimacy_boundary__constitutional_impossibility_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(secession_legitimacy_boundary__constitutional_impossibility_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(secession_legitimacy_boundary__constitutional_impossibility_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness is low (0.15) because, from this reading's perspective, the federal government's collection of resources and exercise of authority over all territories is a legitimate constitutional function, not extraction. Suppression is very high (0.88) because the federal authority would actively and forcefully resist any unilateral secession attempt, using all legal and political means. Theater ratio is low (0.10) as the enforcement of this constitutional principle is genuine and functional, not performative. Accessibility collapse is high (0.92) because this reading asserts no legitimate alternative path to secession outside of a constitutional amendment process, which is extremely difficult to achieve. Resistance is high (0.70) due to the persistent existence of separatist movements.
 *
 * PERSPECTIVAL GAP:
 *   While this reading asserts low extraction and no victims, other readings (e.g., popular sovereignty, grievance threshold) would compute very high extraction from the federal government and identify separatist populations as clear victims. The engine's per-seat classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The federal government and unionist citizens are clear beneficiaries, gaining from the stability and integrity of the union. Separatist movements are payers, bearing the costs of their claims being deemed illegitimate and facing active suppression. From this reading's perspective, there are no 'victims' because the federal government's actions are constitutionally legitimate.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    secession_legitimacy_kernel_reading,
    'Is this constraint a genuine constitutional limit, or a constructed interpretation that benefits identifiable federal and unionist actors?',
    'Analysis of historical constitutional debates, judicial interpretations over time, and comparative constitutional law to assess the degree of interpretive flexibility and political contestation surrounding the ''perpetual union'' doctrine.',
    'If primarily a constructed interpretation, the constraint''s effective extractiveness (χ) would be higher for separatist groups, and its classification would shift from Mountain to a more extractive type (e.g., Tangled Rope or Snare) for those seats.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(secession_legitimacy_kernel_reading, conceptual, 'Ambiguity between constitutional principle and political construction.').

omega_variable(
    constitutional_immutability_vs_evolution,
    'Is the constitutional text''s prohibition on unilateral secession truly immutable, or is it subject to evolving interpretations and societal norms regarding self-determination?',
    'Examination of judicial precedents, legislative history, and public discourse over long time horizons to identify shifts in the understanding of constitutional permanence versus the right to self-determination. Comparative analysis with other federal states'' constitutional evolutions.',
    'If the constitutional interpretation is found to be more fluid, the ''accessibility_collapse'' metric would decrease, and the ''suppression'' might be re-evaluated as less absolute, potentially shifting the constraint''s classification for affected seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constitutional_immutability_vs_evolution, empirical, 'Whether constitutional text is fixed or evolves.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(secession_legitimacy_boundary__constitutional_impossibility_reading, 1900, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sece_tr_t1900, secession_legitimacy_boundary__constitutional_impossibility_reading, theater_ratio, 1900, 0.08).
narrative_ontology:measurement(sece_tr_t1925, secession_legitimacy_boundary__constitutional_impossibility_reading, theater_ratio, 1925, 0.09).
narrative_ontology:measurement(sece_tr_t1950, secession_legitimacy_boundary__constitutional_impossibility_reading, theater_ratio, 1950, 0.09).
narrative_ontology:measurement(sece_tr_t1975, secession_legitimacy_boundary__constitutional_impossibility_reading, theater_ratio, 1975, 0.1).
narrative_ontology:measurement(sece_tr_t2000, secession_legitimacy_boundary__constitutional_impossibility_reading, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(sece_tr_t2024, secession_legitimacy_boundary__constitutional_impossibility_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(sece_be_t1900, secession_legitimacy_boundary__constitutional_impossibility_reading, base_extractiveness, 1900, 0.12).
narrative_ontology:measurement(sece_be_t1925, secession_legitimacy_boundary__constitutional_impossibility_reading, base_extractiveness, 1925, 0.13).
narrative_ontology:measurement(sece_be_t1950, secession_legitimacy_boundary__constitutional_impossibility_reading, base_extractiveness, 1950, 0.14).
narrative_ontology:measurement(sece_be_t1975, secession_legitimacy_boundary__constitutional_impossibility_reading, base_extractiveness, 1975, 0.15).
narrative_ontology:measurement(sece_be_t2000, secession_legitimacy_boundary__constitutional_impossibility_reading, base_extractiveness, 2000, 0.15).
narrative_ontology:measurement(sece_be_t2024, secession_legitimacy_boundary__constitutional_impossibility_reading, base_extractiveness, 2024, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(sece_su_t1900, secession_legitimacy_boundary__constitutional_impossibility_reading, suppression_requirement, 1900, 0.8).
narrative_ontology:measurement(sece_su_t1925, secession_legitimacy_boundary__constitutional_impossibility_reading, suppression_requirement, 1925, 0.82).
narrative_ontology:measurement(sece_su_t1950, secession_legitimacy_boundary__constitutional_impossibility_reading, suppression_requirement, 1950, 0.85).
narrative_ontology:measurement(sece_su_t1975, secession_legitimacy_boundary__constitutional_impossibility_reading, suppression_requirement, 1975, 0.87).
narrative_ontology:measurement(sece_su_t2000, secession_legitimacy_boundary__constitutional_impossibility_reading, suppression_requirement, 2000, 0.88).
narrative_ontology:measurement(sece_su_t2024, secession_legitimacy_boundary__constitutional_impossibility_reading, suppression_requirement, 2024, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(secession_legitimacy_boundary__constitutional_impossibility_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__constitutional_impossibility_reading, secession_legitimacy_boundary__popular_sovereignty_reading).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__constitutional_impossibility_reading, secession_legitimacy_boundary__grievance_threshold_reading).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__constitutional_impossibility_reading, secession_legitimacy_boundary__treaty_primacy_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of several readings of the 'secession_legitimacy_boundary' kernel, each representing a distinct structural claim about the conditions under which secession is legitimate.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
