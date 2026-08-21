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
 *   human_readable: Commerce Clause Scope: Broad Effects Test
 *   domain: constitutional_law/federalism
 *
 * SUMMARY:
 *   This constraint story describes the 'broad effects test' reading of the
 *   Commerce Clause, which holds that federal power extends to any economic
 *   activity that substantially affects interstate commerce in the aggregate,
 *   including intrastate activities with cumulative national economic impact.
 *   This reading emerged prominently during the New Deal era and expanded
 *   significantly through the mid-20th century, providing a constitutional
 *   basis for extensive federal regulation. It is one of several competing
 *   interpretations of the Commerce Clause's scope.
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
narrative_ontology:constraint_metric(commerce_clause_scope__broad_effects_test, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(commerce_clause_scope__broad_effects_test, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(commerce_clause_scope__broad_effects_test, tangled_rope).
narrative_ontology:human_readable(commerce_clause_scope__broad_effects_test, "Commerce Clause Scope: Broad Effects Test").
narrative_ontology:topic_domain(commerce_clause_scope__broad_effects_test, "constitutional_law/federalism").

domain_priors:requires_active_enforcement(commerce_clause_scope__broad_effects_test).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(commerce_clause_scope__broad_effects_test, '8ebf34da-e7e7-4140-a12c-8db91673d876').
narrative_ontology:cs_kernel_codification('8ebf34da-e7e7-4140-a12c-8db91673d876', fixed_text).
narrative_ontology:cs_authority_grounding('8ebf34da-e7e7-4140-a12c-8db91673d876', lineage).
narrative_ontology:cs_interpretation_layer_present('8ebf34da-e7e7-4140-a12c-8db91673d876').
narrative_ontology:cs_reading_relation('8ebf34da-e7e7-4140-a12c-8db91673d876', commerce_clause_scope__intermediate_channels, coexists_with).
narrative_ontology:cs_reading_relation('8ebf34da-e7e7-4140-a12c-8db91673d876', commerce_clause_scope__narrow_originalist, coexists_with).
narrative_ontology:cs_axiom('8ebf34da-e7e7-4140-a12c-8db91673d876', foundational, aggregate_effects_doctrine).
narrative_ontology:cs_axiom_status(aggregate_effects_doctrine, holdable).
narrative_ontology:cs_axiom_grounding('8ebf34da-e7e7-4140-a12c-8db91673d876', aggregate_effects_doctrine, conventional).
narrative_ontology:cs_axiom('8ebf34da-e7e7-4140-a12c-8db91673d876', foundational, federal_supremacy_in_economic_regulation).
narrative_ontology:cs_axiom_status(federal_supremacy_in_economic_regulation, holdable).
narrative_ontology:cs_axiom_grounding('8ebf34da-e7e7-4140-a12c-8db91673d876', federal_supremacy_in_economic_regulation, conventional).
narrative_ontology:cs_reference_frame('8ebf34da-e7e7-4140-a12c-8db91673d876', new_deal_era_expansive_power).
narrative_ontology:cs_drift_state('8ebf34da-e7e7-4140-a12c-8db91673d876', contemporary_jurisprudence, gap(stable, minor, true)).
narrative_ontology:cs_created_at('8ebf34da-e7e7-4140-a12c-8db91673d876', '').
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

% Interpret and apply the broad effects test to justify federal legislation across a wide range of economic and social issues. They benefit from expanded jurisdiction and the ability to implement uniform national policies.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__broad_effects_test, federal_regulators, agenda_setter,
    institutional, generational, constrained, national).

% Advocate for federal legislation on issues ranging from environmental protection to labor standards, leveraging the broad interpretation of the Commerce Clause to achieve national policy goals that might be difficult to pass at the state level.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__broad_effects_test, national_interest_groups, beneficiary,
    organized, biographical, mobile, national).

% Relies heavily on the broad effects test to justify federal anti-discrimination laws, arguing that discrimination in local businesses or public accommodations cumulatively affects interstate commerce. Benefits from a powerful tool for national enforcement of civil rights.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__broad_effects_test, civil_rights_enforcement, beneficiary,
    institutional, generational, constrained, national).

% Experience a reduction in their sovereign police powers and legislative autonomy as federal authority expands into areas traditionally reserved for states. They bear the cost of preemption and reduced ability to experiment with local solutions.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__broad_effects_test, state_governments, payer,
    institutional, generational, constrained, national).

% Subject to federal regulations that may not be tailored to local conditions, increasing compliance costs and potentially stifling local economic activity. Their ability to operate under purely local rules is diminished.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__broad_effects_test, local_businesses, payer,
    moderate, biographical, constrained, local).

% Their seemingly purely intrastate activities (e.g., growing food for personal consumption) can be aggregated and brought under federal regulatory power, limiting individual autonomy and economic freedom at the local level.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__broad_effects_test, individual_economic_actors, payer,
    powerless, immediate, trapped, local).

% Argue that the broad effects test fundamentally distorts the original meaning of the Commerce Clause, but their interpretive framework is largely marginalized in contemporary jurisprudence regarding this reading.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__broad_effects_test, constitutional_originalists, excluded,
    analytical, civilizational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a legal basis for uniform national regulation of economic activity, preventing a 'race to the bottom' among states and facilitating a single national market. It allows for federal responses to national economic crises or social problems with economic dimensions.
% TRANSFER_FUNCTION: Transfers regulatory authority and policy-making power from state and local governments to the federal government, enabling the federal government to control a vast array of economic activities and associated social policies.
% ABSENT_VOICES: Advocates for a more limited federal government and greater state autonomy, particularly constitutional originalists and proponents of strict federalism, are largely excluded from the interpretive process that sustains this broad reading. They would argue for a return to a more constrained understanding of federal power.
% DISAPPEARANCE_RATIONALE: If the broad effects test vanished overnight, a vast body of federal legislation (e.g., environmental laws, labor laws, civil rights acts) would lose its constitutional basis, leading to a massive shift of regulatory power back to the states and a chaotic restructuring of national policy.
% FOUNDING_PROBLEM: The original Articles of Confederation failed to provide a strong central government capable of regulating interstate commerce, leading to economic balkanization and interstate trade disputes. The Constitution aimed to create a more unified national economy.
% FOUNDING_PROBLEM_CORROBORATION: Federal regulators and national interest groups attest that the problem of fragmented economic regulation and the need for national solutions remains live. State governments and originalist scholars contest that the current interpretation overshoots the original problem, creating new issues of federal overreach.
narrative_ontology:disappearance_verdict(commerce_clause_scope__broad_effects_test, world_rearranges).
narrative_ontology:founding_problem_status(commerce_clause_scope__broad_effects_test, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(commerce_clause_scope__broad_effects_test, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
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
 *   The extractiveness is high (0.85) because this reading allows the federal government to claim regulatory authority over virtually all economic activity, significantly diminishing state sovereignty. Suppression is also high (0.75) as it actively overrides state legislative power and limits alternatives for state-level economic policy. The claimed type is 'tangled_rope' because it provides a genuine coordination function (national economic unity) but also involves significant asymmetric extraction of power from states to the federal government, requiring active enforcement through judicial review and federal preemption.
 *
 * PERSPECTIVAL GAP:
 *   Federal regulators and national interest groups perceive this as a necessary and beneficial coordination mechanism for a modern national economy, enabling effective governance. State governments and local economic actors, however, experience it as an extractive mechanism that centralizes power and diminishes local autonomy. The engine's per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Federal regulators, national interest groups, and civil rights enforcement are beneficiaries (low d) as they gain expanded power and policy tools. State governments, local businesses, and individual economic actors are targets (high d) as they bear the costs of reduced autonomy and increased federal oversight. Constitutional originalists are excluded, as their interpretive framework is not the one currently driving the constraint's operation.
 *
 * MANDATROPHY ANALYSIS:
 *   The broad effects test prevents mislabeling coordination as pure extraction by acknowledging the genuine collective action problem of a fragmented national economy. However, the high extractiveness and suppression metrics, coupled with the 'contested' status of the founding problem, suggest a risk of mandatrophy where the coordination function becomes a cover for continued power centralization beyond its original justification. The omegas address the ongoing debate about the appropriate balance between national coordination and state autonomy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    federalism_balance_ambiguity,
    'Is the current scope of federal power under the broad effects test an appropriate balance for a modern national economy, or does it unduly infringe on state sovereignty?',
    'Ongoing judicial review, legislative action to redefine federal-state boundaries, or a constitutional amendment clarifying the Commerce Clause. Public opinion shifts on federalism.',
    'If deemed an undue infringement, it could lead to judicial narrowing of the Commerce Clause, reclassifying it closer to a Snare for states. If affirmed as appropriate, its Tangled Rope classification would be reinforced, with the coordination aspect emphasized.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(federalism_balance_ambiguity, preference, 'Debate over the normative balance of federal and state power.').

omega_variable(
    economic_vs_non_economic_activity,
    'What constitutes ''economic activity'' for the purpose of aggregation under the Commerce Clause, and how far can federal power extend to ''non-economic'' activity with attenuated effects?',
    'Future Supreme Court rulings clarifying the distinction between economic and non-economic activity, and the permissible causal chain for ''substantial effects''.',
    'A stricter definition of ''economic activity'' would reduce the constraint''s scope and extractiveness, potentially shifting it towards a more balanced Rope or even a Scaffold if temporary. A broad definition maintains its current extractive nature.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(economic_vs_non_economic_activity, conceptual, 'Ambiguity in defining the scope of ''economic activity'' subject to federal regulation.').

omega_variable(
    original_intent_vs_living_constitution,
    'Should the Commerce Clause be interpreted according to its original public meaning, or as a ''living'' document adaptable to modern economic realities?',
    'Dominance of a particular interpretive methodology within the judiciary and legal scholarship. This is a foundational conceptual debate.',
    'If originalism gains dominance, this reading would be foreclosed, and the Commerce Clause would be reclassified as a much narrower constraint (e.g., a Mountain or Rope for facilitating trade). If the living constitution view prevails, this reading''s legitimacy is reinforced.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(original_intent_vs_living_constitution, conceptual, 'The fundamental interpretive conflict underlying Commerce Clause jurisprudence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(commerce_clause_scope__broad_effects_test, 1937, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

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
narrative_ontology:affects_constraint(commerce_clause_scope__broad_effects_test, federal_environmental_regulation).
narrative_ontology:affects_constraint(commerce_clause_scope__broad_effects_test, federal_labor_laws).
narrative_ontology:affects_constraint(commerce_clause_scope__broad_effects_test, federal_civil_rights_legislation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
