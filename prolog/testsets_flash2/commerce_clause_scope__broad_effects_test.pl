% ============================================================================
% CONSTRAINT STORY: commerce_clause_scope__broad_effects_test
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_commerce_clause_scope__broad_effects_test, []).

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
 *   constraint_id: commerce_clause_scope__broad_effects_test
 *   human_readable: Commerce Clause Scope: Broad Effects Test (Aggregation Doctrine)
 *   domain: constitutional_law/federalism
 *
 * SUMMARY:
 *   This constraint represents the 'broad effects test' reading of the U.S.
 *   Constitution's Commerce Clause, which holds that federal power extends to
 *   any economic activity that substantially affects interstate commerce,
 *   even if purely intrastate, when aggregated. This interpretation,
 *   solidified during the New Deal era and expanded through the Civil Rights
 *   era, grants the federal government extensive regulatory authority. This
 *   story focuses on the structural implications of this expansive reading,
 *   particularly the transfer of power from states to the federal government.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(commerce_clause_scope__broad_effects_test, 0.85).
domain_priors:suppression_score(commerce_clause_scope__broad_effects_test, 0.75).
domain_priors:theater_ratio(commerce_clause_scope__broad_effects_test, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(commerce_clause_scope__broad_effects_test, extractiveness, 0.85).
narrative_ontology:constraint_metric(commerce_clause_scope__broad_effects_test, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(commerce_clause_scope__broad_effects_test, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(commerce_clause_scope__broad_effects_test, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(commerce_clause_scope__broad_effects_test, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(commerce_clause_scope__broad_effects_test, tangled_rope).
narrative_ontology:human_readable(commerce_clause_scope__broad_effects_test, "Commerce Clause Scope: Broad Effects Test (Aggregation Doctrine)").
narrative_ontology:topic_domain(commerce_clause_scope__broad_effects_test, "constitutional_law/federalism").

domain_priors:requires_active_enforcement(commerce_clause_scope__broad_effects_test).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(commerce_clause_scope__broad_effects_test, 'aec2c1c6-90b6-47a6-9e14-b2f0a35605bf').
narrative_ontology:cs_kernel_codification('aec2c1c6-90b6-47a6-9e14-b2f0a35605bf', fixed_text).
narrative_ontology:cs_authority_grounding('aec2c1c6-90b6-47a6-9e14-b2f0a35605bf', lineage).
narrative_ontology:cs_interpretation_layer_present('aec2c1c6-90b6-47a6-9e14-b2f0a35605bf').
narrative_ontology:cs_reading_relation('aec2c1c6-90b6-47a6-9e14-b2f0a35605bf', commerce_clause_scope__narrow_originalist, influences).
narrative_ontology:cs_reading_relation('aec2c1c6-90b6-47a6-9e14-b2f0a35605bf', commerce_clause_scope__intermediate_channels, influences).
narrative_ontology:cs_axiom('aec2c1c6-90b6-47a6-9e14-b2f0a35605bf', foundational, aggregate_effects_doctrine).
narrative_ontology:cs_axiom_status(aggregate_effects_doctrine, holdable).
narrative_ontology:cs_axiom_grounding('aec2c1c6-90b6-47a6-9e14-b2f0a35605bf', aggregate_effects_doctrine, conventional).
narrative_ontology:cs_axiom('aec2c1c6-90b6-47a6-9e14-b2f0a35605bf', foundational, federal_supremacy_in_economic_policy).
narrative_ontology:cs_axiom_status(federal_supremacy_in_economic_policy, holdable).
narrative_ontology:cs_axiom_grounding('aec2c1c6-90b6-47a6-9e14-b2f0a35605bf', federal_supremacy_in_economic_policy, conventional).
narrative_ontology:cs_reference_frame('aec2c1c6-90b6-47a6-9e14-b2f0a35605bf', new_deal_era_expansive_power).
narrative_ontology:cs_drift_state('aec2c1c6-90b6-47a6-9e14-b2f0a35605bf', contemporary_post_lopez_morrison, gap(stable, minor, true)).
narrative_ontology:cs_created_at('aec2c1c6-90b6-47a6-9e14-b2f0a35605bf', '').
narrative_ontology:cs_kernel_id(commerce_clause_scope__broad_effects_test, commerce_clause_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(commerce_clause_scope__broad_effects_test, federal_regulators).
narrative_ontology:constraint_beneficiary(commerce_clause_scope__broad_effects_test, national_interest_groups).
narrative_ontology:constraint_beneficiary(commerce_clause_scope__broad_effects_test, civil_rights_enforcement).
narrative_ontology:constraint_victim(commerce_clause_scope__broad_effects_test, state_sovereignty).
narrative_ontology:constraint_victim(commerce_clause_scope__broad_effects_test, local_economic_autonomy).
narrative_ontology:constraint_victim(commerce_clause_scope__broad_effects_test, intrastate_businesses).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interpret and apply the Commerce Clause to justify federal legislation across a vast array of economic and even non-economic activities, provided there's an aggregate effect on interstate commerce. They benefit from expanded jurisdiction and policy reach.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__broad_effects_test, federal_regulators, agenda_setter,
    institutional, generational, constrained, national).

% Advocate for federal solutions to national problems, leveraging the broad interpretation of the Commerce Clause to achieve uniform policy outcomes that might be difficult to secure at the state level. They benefit from the ability to bypass state-level resistance.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__broad_effects_test, national_interest_groups, beneficiary,
    organized, biographical, mobile, national).

% Relies heavily on the broad Commerce Clause interpretation to justify federal anti-discrimination laws, arguing that discrimination in local businesses affects interstate commerce. They benefit from the expansive reach of federal power to enforce civil rights.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__broad_effects_test, civil_rights_enforcement, beneficiary,
    institutional, generational, constrained, national).

% Experiences a significant reduction in its traditional police powers and regulatory authority over intrastate matters, as federal power expands to cover activities previously considered purely local. Bears the cost of diminished autonomy.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__broad_effects_test, state_sovereignty, payer,
    institutional, generational, constrained, national).

% Local businesses and communities face federal regulation even for activities that are purely intrastate, losing the ability to tailor economic policy to local conditions. Bears the cost of centralized control.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__broad_effects_test, local_economic_autonomy, payer,
    moderate, biographical, constrained, local).

% Small businesses operating entirely within a single state find themselves subject to federal laws and regulations due to the aggregation doctrine, increasing compliance costs and limiting their operational freedom. They have minimal recourse.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__broad_effects_test, intrastate_businesses, payer,
    powerless, immediate, trapped, local).

% Argue that this broad interpretation fundamentally distorts the original meaning and intent of the Commerce Clause, undermining the federalist structure. Their arguments are often marginalized in judicial and legislative discourse.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__broad_effects_test, constitutional_scholars_originalist, excluded,
    analytical, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aims to coordinate national economic policy, prevent states from undermining federal objectives through local actions, and ensure a uniform economic playing field across states, particularly for issues like civil rights.
% TRANSFER_FUNCTION: Transfers regulatory authority and policy-making power from state and local governments to the federal government, enabling federal agencies to control a wide range of economic activities.
% ABSENT_VOICES: Advocates for a more limited federal government and greater state autonomy, particularly those aligned with originalist or textualist interpretations of the Constitution, are often excluded from the dominant legal and political discourse that upholds this broad interpretation.
% DISAPPEARANCE_RATIONALE: If the broad effects test vanished, federal regulatory power would contract dramatically, leading to a massive shift of authority back to the states. Many federal laws (e.g., environmental, labor, civil rights) would lose their constitutional basis, forcing a complete re-evaluation of federal-state relations and potentially creating a patchwork of conflicting state laws.
% FOUNDING_PROBLEM: The Articles of Confederation failed due to states erecting trade barriers and undermining national economic unity. The Commerce Clause was designed to empower the federal government to create a single national market.
% FOUNDING_PROBLEM_CORROBORATION: Federal agencies and national interest groups attest that the problem of economic fragmentation and the need for national solutions remains live. State governments and some constitutional scholars argue that the original problem has been over-solved, and the current interpretation creates new problems of federal overreach; legislative history and judicial dissents provide corroboration for the over-reach reading.
narrative_ontology:disappearance_verdict(commerce_clause_scope__broad_effects_test, world_rearranges).
narrative_ontology:founding_problem_status(commerce_clause_scope__broad_effects_test, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(commerce_clause_scope__broad_effects_test, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(commerce_clause_scope__broad_effects_test, 'none', 1).
narrative_ontology:epsilon_provenance(commerce_clause_scope__broad_effects_test, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(commerce_clause_scope__broad_effects_test_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(commerce_clause_scope__broad_effects_test, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(commerce_clause_scope__broad_effects_test_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.85) because this reading allows the federal government to claim regulatory authority over virtually all economic activity, significantly diminishing state autonomy. Suppression (0.75) is also high, as states and local entities have limited legal avenues to resist federal preemption once an aggregate effect is established. Theater ratio is low (0.1) because the federal government actively exercises this power; it's not merely performative. The metrics reflect the peak of federal power under this interpretation, with slight fluctuations due to cases like Lopez and Morrison, which introduced minor limiting principles but did not fundamentally alter the aggregation doctrine's scope.
 *
 * PERSPECTIVAL GAP:
 *   Federal actors perceive this as a necessary coordination mechanism for a modern national economy, ensuring uniformity and addressing collective action problems. State and local actors, however, experience it as a significant extraction of their traditional powers, leading to a loss of local control and policy diversity. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Federal regulators, national interest groups, and civil rights enforcement are clear beneficiaries, gaining expanded power and reach. State sovereignty, local economic autonomy, and intrastate businesses are the primary victims, losing regulatory control and facing federal mandates. Constitutional scholars advocating for a narrow interpretation are excluded, as their arguments are largely rejected by the prevailing legal framework.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    economic_vs_non_economic_activity,
    'What constitutes ''economic activity'' for the purpose of aggregation, and how robust are the judicial limits on regulating ''non-economic'' activity?',
    'Future Supreme Court rulings clarifying the boundaries of ''economic activity'' and the necessity of a jurisdictional element for non-economic activities.',
    'A stricter definition of ''economic activity'' or more robust limits on non-economic regulation would reduce federal extractiveness and suppression, potentially shifting the classification towards a more balanced ''tangled_rope'' or even ''rope'' for some applications. A looser definition would further entrench federal power.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(economic_vs_non_economic_activity, conceptual, 'Ambiguity in defining the scope of ''economic activity'' under the Commerce Clause.').

omega_variable(
    state_sovereignty_erosion_threshold,
    'At what point does the cumulative erosion of state sovereignty via federal preemption become a fundamental alteration of the federal system, rather than a necessary adjustment?',
    'A constitutional convention or a series of state-led challenges that fundamentally re-negotiate the balance of power, or a clear judicial articulation of an ''outer limit'' to federal power that is not merely rhetorical.',
    'If a threshold is crossed, the constraint could be reclassified as a ''snare'' from the perspective of state sovereignty, as the coordination function becomes entirely subsumed by extraction. If it''s deemed a necessary adjustment, the ''tangled_rope'' classification holds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(state_sovereignty_erosion_threshold, preference, 'The normative threshold for federal overreach versus necessary national coordination.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(commerce_clause_scope__broad_effects_test, 1937, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comm_tr_t1937, commerce_clause_scope__broad_effects_test, theater_ratio, 1937, 0.05).
narrative_ontology:measurement(comm_tr_t1964, commerce_clause_scope__broad_effects_test, theater_ratio, 1964, 0.08).
narrative_ontology:measurement(comm_tr_t1995, commerce_clause_scope__broad_effects_test, theater_ratio, 1995, 0.12).
narrative_ontology:measurement(comm_tr_t2012, commerce_clause_scope__broad_effects_test, theater_ratio, 2012, 0.09).
narrative_ontology:measurement(comm_tr_t2024, commerce_clause_scope__broad_effects_test, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(comm_be_t1937, commerce_clause_scope__broad_effects_test, base_extractiveness, 1937, 0.6).
narrative_ontology:measurement(comm_be_t1964, commerce_clause_scope__broad_effects_test, base_extractiveness, 1964, 0.8).
narrative_ontology:measurement(comm_be_t1995, commerce_clause_scope__broad_effects_test, base_extractiveness, 1995, 0.75).
narrative_ontology:measurement(comm_be_t2012, commerce_clause_scope__broad_effects_test, base_extractiveness, 2012, 0.82).
narrative_ontology:measurement(comm_be_t2024, commerce_clause_scope__broad_effects_test, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(comm_su_t1937, commerce_clause_scope__broad_effects_test, suppression_requirement, 1937, 0.5).
narrative_ontology:measurement(comm_su_t1964, commerce_clause_scope__broad_effects_test, suppression_requirement, 1964, 0.7).
narrative_ontology:measurement(comm_su_t1995, commerce_clause_scope__broad_effects_test, suppression_requirement, 1995, 0.65).
narrative_ontology:measurement(comm_su_t2012, commerce_clause_scope__broad_effects_test, suppression_requirement, 2012, 0.72).
narrative_ontology:measurement(comm_su_t2024, commerce_clause_scope__broad_effects_test, suppression_requirement, 2024, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(commerce_clause_scope__broad_effects_test, enforcement_mechanism).
narrative_ontology:affects_constraint(commerce_clause_scope__broad_effects_test, commerce_clause_scope__intermediate_channels).
narrative_ontology:affects_constraint(commerce_clause_scope__broad_effects_test, commerce_clause_scope__narrow_originalist).
narrative_ontology:affects_constraint(commerce_clause_scope__broad_effects_test, federal_environmental_regulation).
narrative_ontology:affects_constraint(commerce_clause_scope__broad_effects_test, federal_labor_standards).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'commerce_clause_scope' kernel. Its expansive interpretation of federal power directly influences the viability and scope of other readings, particularly the 'intermediate_channels' and 'narrow_originalist' interpretations, by setting a high bar for limiting federal authority.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
