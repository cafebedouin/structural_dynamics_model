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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: commerce_clause_scope__broad_effects_test
 *   human_readable: Commerce Clause: Broad Effects Test
 *   domain: constitutional_law/federalism
 *
 * SUMMARY:
 *   This constraint represents the 'broad effects test' reading of the
 *   Commerce Clause, which holds that federal power extends to any economic
 *   activity that substantially affects interstate commerce in the aggregate,
 *   and that 'regulate' includes prohibition and comprehensive control. This
 *   interpretation, solidified during the New Deal era, vastly expanded
 *   federal power, allowing it to reach intrastate activities with cumulative
 *   national economic impact. It is a reading of the 'commerce_clause_scope'
 *   kernel.
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
narrative_ontology:constraint_metric(commerce_clause_scope__broad_effects_test, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(commerce_clause_scope__broad_effects_test, tangled_rope).
narrative_ontology:human_readable(commerce_clause_scope__broad_effects_test, "Commerce Clause: Broad Effects Test").
narrative_ontology:topic_domain(commerce_clause_scope__broad_effects_test, "constitutional_law/federalism").

domain_priors:requires_active_enforcement(commerce_clause_scope__broad_effects_test).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(commerce_clause_scope__broad_effects_test, 'dce9189c-4d52-4a36-82b3-7c360ffcd41b').
narrative_ontology:cs_kernel_codification('dce9189c-4d52-4a36-82b3-7c360ffcd41b', fixed_text).
narrative_ontology:cs_authority_grounding('dce9189c-4d52-4a36-82b3-7c360ffcd41b', lineage).
narrative_ontology:cs_interpretation_layer_present('dce9189c-4d52-4a36-82b3-7c360ffcd41b').
narrative_ontology:cs_reading_relation('dce9189c-4d52-4a36-82b3-7c360ffcd41b', commerce_clause_scope__narrow_originalist, coexists_with).
narrative_ontology:cs_reading_relation('dce9189c-4d52-4a36-82b3-7c360ffcd41b', commerce_clause_scope__intermediate_channels, coexists_with).
narrative_ontology:cs_axiom('dce9189c-4d52-4a36-82b3-7c360ffcd41b', foundational, aggregate_effects_doctrine).
narrative_ontology:cs_axiom_status(aggregate_effects_doctrine, holdable).
narrative_ontology:cs_axiom_grounding('dce9189c-4d52-4a36-82b3-7c360ffcd41b', aggregate_effects_doctrine, conventional).
narrative_ontology:cs_axiom('dce9189c-4d52-4a36-82b3-7c360ffcd41b', foundational, regulate_includes_prohibition).
narrative_ontology:cs_axiom_status(regulate_includes_prohibition, holdable).
narrative_ontology:cs_axiom_grounding('dce9189c-4d52-4a36-82b3-7c360ffcd41b', regulate_includes_prohibition, conventional).
narrative_ontology:cs_reference_frame('dce9189c-4d52-4a36-82b3-7c360ffcd41b', new_deal_constitutionalism).
narrative_ontology:cs_drift_state('dce9189c-4d52-4a36-82b3-7c360ffcd41b', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('dce9189c-4d52-4a36-82b3-7c360ffcd41b', '').
narrative_ontology:cs_kernel_id(commerce_clause_scope__broad_effects_test, commerce_clause_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(commerce_clause_scope__broad_effects_test, federal_regulators).
narrative_ontology:constraint_beneficiary(commerce_clause_scope__broad_effects_test, national_interest_groups).
narrative_ontology:constraint_beneficiary(commerce_clause_scope__broad_effects_test, civil_rights_enforcement).
narrative_ontology:constraint_victim(commerce_clause_scope__broad_effects_test, state_governments).
narrative_ontology:constraint_victim(commerce_clause_scope__broad_effects_test, local_businesses).
narrative_ontology:constraint_victim(commerce_clause_scope__broad_effects_test, individual_economic_actors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interpret and apply the broad effects test to justify federal legislation across a vast range of economic and even non-economic activities, expanding their jurisdictional reach and policy influence. They benefit from the flexibility and power this interpretation grants.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__broad_effects_test, federal_regulators, agenda_setter,
    institutional, generational, arbitrage, national).

% Advocate for federal legislation to achieve uniform policy goals (e.g., environmental protection, labor standards) that might otherwise be blocked by state-level resistance. This reading provides a powerful tool for achieving their objectives.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__broad_effects_test, national_interest_groups, beneficiary,
    organized, biographical, mobile, national).

% Relies on the broad effects test to justify federal anti-discrimination laws, particularly in areas like public accommodations, by linking local discriminatory practices to aggregate effects on interstate travel and commerce. This reading is crucial for their mandate.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__broad_effects_test, civil_rights_enforcement, beneficiary,
    institutional, generational, constrained, national).

% Experience a significant erosion of their traditional police powers and regulatory autonomy as federal authority expands into areas previously considered purely intrastate. They bear the cost of diminished sovereignty and reduced capacity for state-level experimentation.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__broad_effects_test, state_governments, payer,
    institutional, generational, constrained, national).

% Face federal regulation even for activities that appear purely local, due to the aggregation doctrine. This can lead to increased compliance costs and reduced flexibility, as they must adhere to national standards rather than local ones.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__broad_effects_test, local_businesses, payer,
    moderate, immediate, constrained, local).

% Are subject to federal mandates and prohibitions on activities that might seem entirely personal or local, such as growing certain crops for personal consumption, if those activities are deemed to have a cumulative effect on the national economy. Their autonomy is significantly curtailed.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__broad_effects_test, individual_economic_actors, payer,
    powerless, biographical, trapped, local).

% Argue that the broad effects test fundamentally distorts the original meaning of the Commerce Clause, but their interpretations are largely marginalized in contemporary jurisprudence, especially in areas where federal power is well-established.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__broad_effects_test, narrow_originalist_scholars, excluded,
    analytical, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a legal basis for national coordination on economic and social issues, preventing a 'race to the bottom' among states and enabling uniform policy where aggregate effects are significant, such as environmental protection or civil rights.
% TRANSFER_FUNCTION: Transfers regulatory authority and policy-making power from state and local governments to the federal government, along with the associated benefits of centralized control and the costs of diminished local autonomy.
% ABSENT_VOICES: Advocates for a more limited federal government, including states' rights proponents and strict originalists, are largely excluded from the dominant legal discourse that upholds the broad effects test. They would argue for a return to a more constrained federal role.
% DISAPPEARANCE_RATIONALE: If the broad effects test vanished overnight, decades of federal legislation (e.g., civil rights laws, environmental regulations, healthcare mandates) would immediately lose their constitutional basis, leading to a massive shift of power back to the states and a chaotic reorganization of national policy.
% FOUNDING_PROBLEM: The Articles of Confederation demonstrated the need for a stronger national government to regulate interstate commerce, prevent trade wars among states, and ensure a unified national economy.
% FOUNDING_PROBLEM_CORROBORATION: Historians and constitutional scholars widely corroborate the founding problem of fragmented economic governance under the Articles. The debate is not whether a national commerce power is needed, but its appropriate scope. Federal regulators and national interest groups attest the problem is still live, requiring broad federal power to address modern challenges; state governments contest the extent of this necessity.
narrative_ontology:disappearance_verdict(commerce_clause_scope__broad_effects_test, world_rearranges).
narrative_ontology:founding_problem_status(commerce_clause_scope__broad_effects_test, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(commerce_clause_scope__broad_effects_test, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(commerce_clause_scope__broad_effects_test, 'none', 1).

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
 *   Extractiveness is high (0.85) because this reading allows the federal government to subsume vast areas of state regulatory authority, effectively extracting sovereignty from states and local entities. Suppression is also high (0.75) as it actively suppresses state experimentation and local economic autonomy through federal preemption and mandates. Theater ratio is low (0.1) because the federal government genuinely exercises this power; it's not merely performative. The slight dip in extractiveness and suppression around 1995 reflects Supreme Court cases (Lopez, Morrison) that attempted to rein in Commerce Clause power, but the overall trend has been towards expansion.
 *
 * PERSPECTIVAL GAP:
 *   Federal actors perceive this as a necessary and legitimate coordination mechanism for a complex national economy, ensuring stability and addressing collective action problems. State and local actors, however, experience it as an extractive encroachment on their traditional spheres of authority, leading to a loss of local control and responsiveness. The engine's per-seat classification should reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Federal regulators and national interest groups are clear beneficiaries, gaining immense power and a tool for uniform policy. Civil rights enforcement also benefits significantly. State governments, local businesses, and individual economic actors are the primary victims, experiencing a loss of autonomy and increased federal oversight. The directionality for states is high (target) due to the direct erosion of their sovereign powers.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    economic_vs_non_economic_activity,
    'What constitutes ''economic activity'' for the purpose of aggregation, and can non-economic activity ever be aggregated?',
    'Further Supreme Court clarification or legislative action defining the boundaries of ''economic activity'' and the applicability of aggregation to non-economic spheres.',
    'A narrower definition of ''economic activity'' would reduce federal power, potentially reclassifying some federal regulations as overreach. A broader definition would further entrench the current expansive federal authority.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(economic_vs_non_economic_activity, conceptual, 'Ambiguity in the definition of ''economic activity'' and its aggregation.').

omega_variable(
    state_sovereignty_erosion_threshold,
    'At what point does the cumulative effect of federal regulation under the Commerce Clause so diminish state sovereignty that it fundamentally alters the federal structure?',
    'Constitutional amendment or a clear, sustained shift in judicial philosophy that re-establishes a more robust sphere of exclusive state power.',
    'If a threshold is identified and crossed, the constraint would be reclassified as a Snare on state sovereignty; if no such threshold is acknowledged, the current Tangled Rope classification holds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(state_sovereignty_erosion_threshold, preference, 'The point at which federal power becomes an existential threat to state sovereignty.').


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
narrative_ontology:measurement(comm_tr_t1995, commerce_clause_scope__broad_effects_test, theater_ratio, 1995, 0.15).
narrative_ontology:measurement(comm_tr_t2012, commerce_clause_scope__broad_effects_test, theater_ratio, 2012, 0.12).
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
narrative_ontology:affects_constraint(commerce_clause_scope__broad_effects_test, federal_civil_rights_legislation).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'commerce_clause_scope' kernel. Its expansive interpretation directly influences the scope and legitimacy of federal environmental and civil rights legislation, and stands in tension with more limited interpretations of the Commerce Clause.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
