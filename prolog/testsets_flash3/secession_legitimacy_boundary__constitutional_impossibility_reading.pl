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
 *   domain: political_economy/federalism/resource_politics
 *
 * SUMMARY:
 *   This constraint represents the 'constitutional impossibility' reading of
 *   secession, where unilateral exit from a federal union is deemed
 *   illegitimate and legally impossible without a constitutional amendment.
 *   It is presented as a fundamental, almost natural, feature of the federal
 *   system. The federal government and remaining provinces are beneficiaries
 *   of this stability, while secessionist movements are the primary payers,
 *   facing legal and political suppression. This is one reading of the
 *   'secession_legitimacy_boundary' kernel.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(secession_legitimacy_boundary__constitutional_impossibility_reading, 0.15).
domain_priors:suppression_score(secession_legitimacy_boundary__constitutional_impossibility_reading, 0.75).
domain_priors:theater_ratio(secession_legitimacy_boundary__constitutional_impossibility_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(secession_legitimacy_boundary__constitutional_impossibility_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__constitutional_impossibility_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__constitutional_impossibility_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(secession_legitimacy_boundary__constitutional_impossibility_reading, accessibility_collapse, 0.85).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__constitutional_impossibility_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(secession_legitimacy_boundary__constitutional_impossibility_reading, mountain).
narrative_ontology:human_readable(secession_legitimacy_boundary__constitutional_impossibility_reading, "Constitutional Impossibility of Unilateral Secession").
narrative_ontology:topic_domain(secession_legitimacy_boundary__constitutional_impossibility_reading, "political_economy/federalism/resource_politics").

domain_priors:requires_active_enforcement(secession_legitimacy_boundary__constitutional_impossibility_reading).
domain_priors:emerges_naturally(secession_legitimacy_boundary__constitutional_impossibility_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(secession_legitimacy_boundary__constitutional_impossibility_reading, '20989686-3b3a-4122-8150-5e468f448630').
narrative_ontology:cs_kernel_codification('20989686-3b3a-4122-8150-5e468f448630', fixed_text).
narrative_ontology:cs_authority_grounding('20989686-3b3a-4122-8150-5e468f448630', lineage).
narrative_ontology:cs_interpretation_layer_present('20989686-3b3a-4122-8150-5e468f448630').
narrative_ontology:cs_reading_relation('20989686-3b3a-4122-8150-5e468f448630', secession_legitimacy_boundary__popular_sovereignty_reading, forecloses).
narrative_ontology:cs_reading_relation('20989686-3b3a-4122-8150-5e468f448630', secession_legitimacy_boundary__grievance_threshold_reading, forecloses).
narrative_ontology:cs_reading_relation('20989686-3b3a-4122-8150-5e468f448630', secession_legitimacy_boundary__treaty_primacy_reading, coexists_with).
narrative_ontology:cs_axiom('20989686-3b3a-4122-8150-5e468f448630', foundational, federal_union_indivisible).
narrative_ontology:cs_axiom_status(federal_union_indivisible, holdable).
narrative_ontology:cs_axiom_grounding('20989686-3b3a-4122-8150-5e468f448630', federal_union_indivisible, deontological).
narrative_ontology:cs_axiom('20989686-3b3a-4122-8150-5e468f448630', foundational, constitutional_text_supreme).
narrative_ontology:cs_axiom_status(constitutional_text_supreme, holdable).
narrative_ontology:cs_axiom_grounding('20989686-3b3a-4122-8150-5e468f448630', constitutional_text_supreme, conventional).
narrative_ontology:cs_reference_frame('20989686-3b3a-4122-8150-5e468f448630', original_constitutional_compact).
narrative_ontology:cs_drift_state('20989686-3b3a-4122-8150-5e468f448630', contemporary_secessionist_challenges, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('20989686-3b3a-4122-8150-5e468f448630', '').
narrative_ontology:cs_kernel_id(secession_legitimacy_boundary__constitutional_impossibility_reading, secession_legitimacy_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(secession_legitimacy_boundary__constitutional_impossibility_reading, federal_government).
narrative_ontology:constraint_beneficiary(secession_legitimacy_boundary__constitutional_impossibility_reading, remaining_provinces).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__constitutional_impossibility_reading, secessionist_movements).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Upholds the constitutional order, asserting its indivisibility and the illegitimacy of unilateral secession. Benefits from the stability and territorial integrity this reading provides, and from continued access to all national resources.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__constitutional_impossibility_reading, federal_government, agenda_setter,
    institutional, generational, analytical, national).

% Seek to establish independent statehood but are legally and constitutionally barred from unilateral action. Their efforts are deemed illegitimate by the federal authority, facing legal challenges and potential federal intervention. They bear the cost of non-recognition and the suppression of their political aspirations.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__constitutional_impossibility_reading, secessionist_movements, payer,
    organized, generational, trapped, regional).

% Benefit from the stability of the federal union and the continued sharing of resources and responsibilities. They support the constitutional impossibility reading as it protects their own territorial integrity and prevents a cascade of separatist movements.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__constitutional_impossibility_reading, remaining_provinces, beneficiary,
    institutional, generational, constrained, national).

% Analyze the legal precedents and historical interpretations of the constitution regarding secession. Their work often reinforces the federal government's position, contributing to the intellectual legitimacy of the 'constitutional impossibility' reading.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__constitutional_impossibility_reading, constitutional_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a clear, stable framework for national unity and territorial integrity, preventing fragmentation and ensuring consistent governance across the federal entity.
% TRANSFER_FUNCTION: Transfers ultimate sovereignty and control over territory and resources from sub-national entities to the federal union, in exchange for legal and political stability.
% ABSENT_VOICES: Advocates for popular sovereignty and self-determination, who would argue that a people's will can supersede constitutional text, are excluded from the federal legal discourse. Indigenous nations asserting pre-existing treaty rights are also marginalized by this federal-centric reading.
% DISAPPEARANCE_RATIONALE: If the constitutional impossibility of unilateral secession vanished, the federal state would immediately face multiple, potentially violent, challenges to its territorial integrity. Secessionist movements would declare independence, leading to widespread political instability, border disputes, and a complete reordering of national and international relations.
% FOUNDING_PROBLEM: The original problem was to establish a durable, indivisible union of states, preventing internal dissolution and ensuring collective security and economic prosperity.
% FOUNDING_PROBLEM_CORROBORATION: The federal government and most remaining provinces attest that the problem of national unity and potential fragmentation remains live. International legal scholars and historians corroborate the historical imperative for union in the founding era, and the ongoing challenges to state integrity globally.
narrative_ontology:disappearance_verdict(secession_legitimacy_boundary__constitutional_impossibility_reading, world_rearranges).
narrative_ontology:founding_problem_status(secession_legitimacy_boundary__constitutional_impossibility_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(secession_legitimacy_boundary__constitutional_impossibility_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
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
 *   The extractiveness is low (0.15) because this reading frames the federal union as a legitimate, mutually beneficial arrangement, not an extractive one. Any 'extraction' from sub-national entities is seen as a necessary cost of a functioning union. Suppression is high (0.75) because the federal authority actively suppresses any attempts at unilateral secession through legal and political means, and potentially force. Accessibility collapse is high (0.85) as this reading asserts no legitimate alternative to remaining in the union, short of a constitutional amendment. Resistance is moderate (0.3) reflecting ongoing, but legally delegitimized, secessionist movements. Theater ratio is low (0.1) as the federal government genuinely believes in and acts upon this constitutional principle.
 *
 * PERSPECTIVAL GAP:
 *   From the federal government's perspective, this is a foundational principle, almost a Mountain. From the secessionist movements' perspective, it is a Snare, trapping them in an unwanted union. The engine will compute this divergence based on the declared structural relationships and metrics.
 *
 * DIRECTIONALITY LOGIC:
 *   The federal government and remaining provinces are clear beneficiaries (d near 0.0) as they gain stability, territorial integrity, and resource access. Secessionist movements are targets (d near 1.0) as they bear the full cost of non-recognition and suppression. Constitutional scholars, while observers, often reinforce the federal position, contributing to the constraint's legitimacy.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_legal_construct,
    'Is the constitutional impermissibility of unilateral secession a ''natural law'' of federalism, or a legal construct maintained by the dominant power?',
    'Comparative analysis of federal systems globally, particularly those with successful or attempted secessions, to identify common structural features versus context-specific legal interpretations.',
    'If a natural law, the constraint is a genuine Mountain. If a legal construct, it is a Tangled Rope or Snare, maintained for the benefit of the federal center, and the claimed ''emerges_naturally'' is a false summit.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_legal_construct, conceptual, 'Ambiguity between inherent federal structure and power-maintained legal interpretation.').

omega_variable(
    legitimacy_of_extraction_claim,
    'Is the ''extraction'' claimed by secessionist movements a legitimate grievance, or an invalid claim within the constitutional framework?',
    'Independent economic and political analysis of resource flows and governance benefits/costs, assessed against a neutral standard of equity, rather than solely constitutional legality.',
    'If extraction is found to be legitimate, the ''constitutional impossibility'' reading''s low extractiveness score is misleading, and the constraint operates as a Snare from the secessionist seat. If extraction is invalid, the Mountain classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_of_extraction_claim, empirical, 'Whether the federal union''s resource allocation is genuinely equitable or constitutes extraction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(secession_legitimacy_boundary__constitutional_impossibility_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sece_tr_t0, secession_legitimacy_boundary__constitutional_impossibility_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(sece_tr_t10, secession_legitimacy_boundary__constitutional_impossibility_reading, theater_ratio, 10, 0.07).
narrative_ontology:measurement(sece_tr_t20, secession_legitimacy_boundary__constitutional_impossibility_reading, theater_ratio, 20, 0.08).
narrative_ontology:measurement(sece_tr_t30, secession_legitimacy_boundary__constitutional_impossibility_reading, theater_ratio, 30, 0.09).
narrative_ontology:measurement(sece_tr_t40, secession_legitimacy_boundary__constitutional_impossibility_reading, theater_ratio, 40, 0.1).
narrative_ontology:measurement(sece_tr_t50, secession_legitimacy_boundary__constitutional_impossibility_reading, theater_ratio, 50, 0.1).

% Extraction over time
narrative_ontology:measurement(sece_be_t0, secession_legitimacy_boundary__constitutional_impossibility_reading, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(sece_be_t10, secession_legitimacy_boundary__constitutional_impossibility_reading, base_extractiveness, 10, 0.12).
narrative_ontology:measurement(sece_be_t20, secession_legitimacy_boundary__constitutional_impossibility_reading, base_extractiveness, 20, 0.13).
narrative_ontology:measurement(sece_be_t30, secession_legitimacy_boundary__constitutional_impossibility_reading, base_extractiveness, 30, 0.14).
narrative_ontology:measurement(sece_be_t40, secession_legitimacy_boundary__constitutional_impossibility_reading, base_extractiveness, 40, 0.15).
narrative_ontology:measurement(sece_be_t50, secession_legitimacy_boundary__constitutional_impossibility_reading, base_extractiveness, 50, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(sece_su_t0, secession_legitimacy_boundary__constitutional_impossibility_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(sece_su_t10, secession_legitimacy_boundary__constitutional_impossibility_reading, suppression_requirement, 10, 0.65).
narrative_ontology:measurement(sece_su_t20, secession_legitimacy_boundary__constitutional_impossibility_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(sece_su_t30, secession_legitimacy_boundary__constitutional_impossibility_reading, suppression_requirement, 30, 0.72).
narrative_ontology:measurement(sece_su_t40, secession_legitimacy_boundary__constitutional_impossibility_reading, suppression_requirement, 40, 0.74).
narrative_ontology:measurement(sece_su_t50, secession_legitimacy_boundary__constitutional_impossibility_reading, suppression_requirement, 50, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(secession_legitimacy_boundary__constitutional_impossibility_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__constitutional_impossibility_reading, secession_legitimacy_boundary__popular_sovereignty_reading).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__constitutional_impossibility_reading, secession_legitimacy_boundary__grievance_threshold_reading).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__constitutional_impossibility_reading, secession_legitimacy_boundary__treaty_primacy_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of four readings of the 'secession_legitimacy_boundary' kernel. This 'constitutional impossibility' reading asserts federal authority and constitutional text as supreme, influencing (and often foreclosing) other readings that prioritize popular will, grievance, or treaty rights.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
