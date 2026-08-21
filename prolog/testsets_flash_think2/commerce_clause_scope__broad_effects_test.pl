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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: commerce_clause_scope__broad_effects_test
 *   human_readable: Commerce Clause Broad Effects Test
 *   domain: constitutional_law/federalism
 *
 * SUMMARY:
 *   This constraint represents the 'broad effects test' reading of the U.S.
 *   Constitution's Commerce Clause, which holds that federal power extends to
 *   any economic activity that substantially affects interstate commerce in
 *   the aggregate, including the power to prohibit and comprehensively
 *   control such activity. This interpretation, solidified in cases like
 *   Wickard v. Filburn (1942), grants the federal government expansive
 *   authority over intrastate activities with cumulative national economic
 *   impact. It is a reading that prioritizes national uniformity and federal
 *   problem-solving over state autonomy.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(commerce_clause_scope__broad_effects_test, 0.78).
domain_priors:suppression_score(commerce_clause_scope__broad_effects_test, 0.85).
domain_priors:theater_ratio(commerce_clause_scope__broad_effects_test, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(commerce_clause_scope__broad_effects_test, extractiveness, 0.78).
narrative_ontology:constraint_metric(commerce_clause_scope__broad_effects_test, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(commerce_clause_scope__broad_effects_test, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(commerce_clause_scope__broad_effects_test, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(commerce_clause_scope__broad_effects_test, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(commerce_clause_scope__broad_effects_test, tangled_rope).
narrative_ontology:human_readable(commerce_clause_scope__broad_effects_test, "Commerce Clause Broad Effects Test").
narrative_ontology:topic_domain(commerce_clause_scope__broad_effects_test, "constitutional_law/federalism").

domain_priors:requires_active_enforcement(commerce_clause_scope__broad_effects_test).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(commerce_clause_scope__broad_effects_test, '5a08c4b1-49c2-4d7a-9fa7-a7f9fd2374a1').
narrative_ontology:cs_kernel_codification('5a08c4b1-49c2-4d7a-9fa7-a7f9fd2374a1', fixed_text).
narrative_ontology:cs_authority_grounding('5a08c4b1-49c2-4d7a-9fa7-a7f9fd2374a1', lineage).
narrative_ontology:cs_interpretation_layer_present('5a08c4b1-49c2-4d7a-9fa7-a7f9fd2374a1').
narrative_ontology:cs_reading_relation('5a08c4b1-49c2-4d7a-9fa7-a7f9fd2374a1', commerce_clause_scope__narrow_originalist, forecloses).
narrative_ontology:cs_reading_relation('5a08c4b1-49c2-4d7a-9fa7-a7f9fd2374a1', commerce_clause_scope__intermediate_channels, influences).
narrative_ontology:cs_axiom('5a08c4b1-49c2-4d7a-9fa7-a7f9fd2374a1', foundational, aggregate_effects_doctrine).
narrative_ontology:cs_axiom_status(aggregate_effects_doctrine, holdable).
narrative_ontology:cs_axiom_grounding('5a08c4b1-49c2-4d7a-9fa7-a7f9fd2374a1', aggregate_effects_doctrine, empirically_contingent).
narrative_ontology:cs_axiom('5a08c4b1-49c2-4d7a-9fa7-a7f9fd2374a1', foundational, federal_power_to_prohibit_and_control).
narrative_ontology:cs_axiom_status(federal_power_to_prohibit_and_control, holdable).
narrative_ontology:cs_axiom_grounding('5a08c4b1-49c2-4d7a-9fa7-a7f9fd2374a1', federal_power_to_prohibit_and_control, conventional).
narrative_ontology:cs_reference_frame('5a08c4b1-49c2-4d7a-9fa7-a7f9fd2374a1', new_deal_era_national_economy).
narrative_ontology:cs_drift_state('5a08c4b1-49c2-4d7a-9fa7-a7f9fd2374a1', contemporary_political_polarization, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('5a08c4b1-49c2-4d7a-9fa7-a7f9fd2374a1', '').
narrative_ontology:cs_kernel_id(commerce_clause_scope__broad_effects_test, commerce_clause_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(commerce_clause_scope__broad_effects_test, federal_regulators).
narrative_ontology:constraint_beneficiary(commerce_clause_scope__broad_effects_test, national_interest_groups).
narrative_ontology:constraint_victim(commerce_clause_scope__broad_effects_test, state_governments).
narrative_ontology:constraint_victim(commerce_clause_scope__broad_effects_test, local_economic_actors).
narrative_ontology:constraint_vindicates(commerce_clause_scope__broad_effects_test, national_supremacy_doctrine).
narrative_ontology:constraint_vindicates(commerce_clause_scope__broad_effects_test, implied_powers_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers and enforces federal laws based on the broad interpretation of the Commerce Clause, extending federal reach into areas traditionally regulated by states. Benefits from expanded jurisdiction and policy uniformity.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__broad_effects_test, federal_regulators, agenda_setter,
    institutional, generational, arbitrage, national).

% Advocates for federal solutions to national problems (e.g., civil rights, environmental protection, labor standards), benefiting from the broad interpretation that allows uniform national policy without state-by-state variation.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__broad_effects_test, national_interest_groups, beneficiary,
    organized, biographical, mobile, national).

% Bears the cost of federal preemption and reduced autonomy over intrastate economic activities. Their ability to experiment with local solutions or tailor policies to state-specific needs is constrained by federal power.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__broad_effects_test, state_governments, payer,
    institutional, generational, constrained, national).

% Engages in intrastate economic activities that, when aggregated, are deemed to affect interstate commerce. They face federal regulation and mandates, even if their individual activity is purely local, limiting their economic freedom.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__broad_effects_test, local_economic_actors, payer,
    moderate, biographical, constrained, local).

% The ultimate arbiter of the Commerce Clause's scope, its interpretations define the boundaries of federal power. Its decisions shape the constraint's application and can either expand or contract federal authority.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__broad_effects_test, supreme_court, agenda_setter,
    institutional, civilizational, analytical, national).

% Analyzes the historical development and contemporary application of the Commerce Clause, debating its constitutional fidelity and practical implications for federalism. Provides critical commentary but does not directly enforce or pay.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__broad_effects_test, constitutional_scholars, observer,
    analytical, generational, analytical, universal).

% Advocates for a more limited interpretation of the Commerce Clause, arguing that the broad effects test exceeds the original intent of the framers. Their arguments are often marginalized in the prevailing legal framework.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__broad_effects_test, narrow_originalist_advocates, excluded,
    organized, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(commerce_clause_scope__broad_effects_test, federal_regulators).
narrative_ontology:fixing_cost_class(commerce_clause_scope__broad_effects_test, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables the federal government to coordinate a national economy, prevent states from undermining federal policy through local protectionism, and address national problems that require uniform regulatory solutions.
% TRANSFER_FUNCTION: Transfers significant regulatory authority and policy-making power from state and local governments to the federal government, centralizing control over economic activity.
% ABSENT_VOICES: Advocates for a narrow, originalist interpretation of the Commerce Clause and proponents of robust state sovereignty are often excluded from the dominant legal discourse that upholds the broad effects test. They would argue for a return to more limited federal power.
% DISAPPEARANCE_RATIONALE: If the broad effects test vanished overnight, numerous federal laws (e.g., civil rights, environmental, labor, criminal statutes) would lose their constitutional basis, leading to a massive regulatory vacuum, economic fragmentation, and a fundamental restructuring of federal-state relations.
% FOUNDING_PROBLEM: The economic balkanization and inability to address national economic problems under the Articles of Confederation, necessitating a federal power to regulate a unified national market.
% FOUNDING_PROBLEM_CORROBORATION: Federal agencies and national interest groups argue the founding problem of national economic coordination remains live, requiring broad federal power. State governments and some constitutional scholars contend the original problem is largely solved, and the current interpretation constitutes federal overreach; this is supported by historical analysis and arguments for state autonomy from outside the benefiting federal parties.
narrative_ontology:disappearance_verdict(commerce_clause_scope__broad_effects_test, world_rearranges).
narrative_ontology:founding_problem_status(commerce_clause_scope__broad_effects_test, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(commerce_clause_scope__broad_effects_test, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(commerce_clause_scope__broad_effects_test, 'none', 1).
narrative_ontology:epsilon_provenance(commerce_clause_scope__broad_effects_test, 0.78, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is high (0.78) because this interpretation allows the federal government to claim regulatory authority over vast swathes of economic activity, often preempting state law and imposing federal mandates, thereby extracting policy-making power and resources from states. Suppression is very high (0.85) as it actively limits state and local governments' ability to regulate their own economies independently, backed by the full force of federal law and judicial precedent. Theater ratio is low (0.10) because the constraint is actively and functionally applied, with little performative maintenance; its effects are real and pervasive. Accessibility collapse is high (0.75) as it significantly reduces the viable alternatives for states to regulate economic activity without federal interference. Resistance is moderate (0.60) reflecting ongoing legal and political challenges to this interpretation, particularly from states' rights advocates and some conservative legal scholars.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of federal regulators and national interest groups, this interpretation is a necessary coordination mechanism for a complex national economy, preventing a 'race to the bottom' and ensuring uniform standards. From the perspective of state governments and local economic actors, it is an extractive mechanism that centralizes power, erodes federalism, and suppresses local autonomy. The engine's per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Federal regulators and national interest groups are clear beneficiaries, gaining expanded power and the ability to implement uniform policies. State governments and local economic actors are the primary targets, bearing the costs of federal preemption and reduced self-governance. The Supreme Court acts as an agenda-setter, interpreting and shaping the constraint's boundaries. Constitutional scholars observe and analyze, while narrow originalist advocates are structurally excluded from the dominant interpretive framework.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate, to prevent economic balkanization, is contested. While the original problem of a fragmented national economy has been largely addressed, the broad effects test continues to expand federal power beyond what some argue is necessary for coordination, suggesting a potential for mandatrophy where the coordination function is now a cover for extraction of state sovereignty. The 'contested' status of the founding problem reflects this ongoing debate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scope_of_economic_activity_ambiguity,
    'What constitutes ''economic activity'' for the purpose of aggregation under the Commerce Clause, and how far can this definition extend to non-commercial activities with indirect economic effects?',
    'Further Supreme Court rulings clarifying the distinction between economic and non-economic activity, or legislative action defining the scope of federal regulatory power.',
    'A narrower definition would reduce federal extractiveness and suppression on states; a broader definition would further entrench federal power.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scope_of_economic_activity_ambiguity, conceptual, 'Ambiguity in defining ''economic activity'' for Commerce Clause aggregation.').

omega_variable(
    federalism_balance_ambiguity,
    'Is the current balance of federal and state power under the broad effects test a necessary coordination for a modern national economy, or an undue infringement on state sovereignty?',
    'Empirical studies on the efficiency and democratic accountability of centralized vs. decentralized regulation, or a shift in societal values regarding federalism.',
    'If deemed undue infringement, it would support reclassification towards a Snare for states; if necessary coordination, it would reinforce the Tangled Rope classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(federalism_balance_ambiguity, preference, 'Whether the federalism balance is optimal or extractive.').

omega_variable(
    reading_identity_commerce_clause_scope,
    'This constraint is one specific reading of the ''commerce_clause_scope'' kernel. What would change structurally if a sibling reading were adopted?',
    'Analysis of judicial decisions or legislative actions that explicitly adopt a different interpretive framework for the Commerce Clause.',
    'Adoption of the ''narrow_originalist'' reading would drastically reduce federal extractiveness and suppression, shifting the constraint towards a Rope or even Mountain for states. Adoption of the ''intermediate_channels'' reading would introduce more limiting principles, reducing extractiveness and suppression to a lesser degree.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_identity_commerce_clause_scope, conceptual, 'This constraint is the ''broad_effects_test'' reading of the Commerce Clause scope kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(commerce_clause_scope__broad_effects_test, 1942, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comm_tr_t1942, commerce_clause_scope__broad_effects_test, theater_ratio, 1942, 0.05).
narrative_ontology:measurement(comm_tr_t1960, commerce_clause_scope__broad_effects_test, theater_ratio, 1960, 0.08).
narrative_ontology:measurement(comm_tr_t1980, commerce_clause_scope__broad_effects_test, theater_ratio, 1980, 0.1).
narrative_ontology:measurement(comm_tr_t2000, commerce_clause_scope__broad_effects_test, theater_ratio, 2000, 0.12).
narrative_ontology:measurement(comm_tr_t2010, commerce_clause_scope__broad_effects_test, theater_ratio, 2010, 0.11).
narrative_ontology:measurement(comm_tr_t2024, commerce_clause_scope__broad_effects_test, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(comm_be_t1942, commerce_clause_scope__broad_effects_test, base_extractiveness, 1942, 0.6).
narrative_ontology:measurement(comm_be_t1960, commerce_clause_scope__broad_effects_test, base_extractiveness, 1960, 0.7).
narrative_ontology:measurement(comm_be_t1980, commerce_clause_scope__broad_effects_test, base_extractiveness, 1980, 0.75).
narrative_ontology:measurement(comm_be_t2000, commerce_clause_scope__broad_effects_test, base_extractiveness, 2000, 0.72).
narrative_ontology:measurement(comm_be_t2010, commerce_clause_scope__broad_effects_test, base_extractiveness, 2010, 0.76).
narrative_ontology:measurement(comm_be_t2024, commerce_clause_scope__broad_effects_test, base_extractiveness, 2024, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(comm_su_t1942, commerce_clause_scope__broad_effects_test, suppression_requirement, 1942, 0.7).
narrative_ontology:measurement(comm_su_t1960, commerce_clause_scope__broad_effects_test, suppression_requirement, 1960, 0.8).
narrative_ontology:measurement(comm_su_t1980, commerce_clause_scope__broad_effects_test, suppression_requirement, 1980, 0.85).
narrative_ontology:measurement(comm_su_t2000, commerce_clause_scope__broad_effects_test, suppression_requirement, 2000, 0.82).
narrative_ontology:measurement(comm_su_t2010, commerce_clause_scope__broad_effects_test, suppression_requirement, 2010, 0.84).
narrative_ontology:measurement(comm_su_t2024, commerce_clause_scope__broad_effects_test, suppression_requirement, 2024, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(commerce_clause_scope__broad_effects_test, enforcement_mechanism).
narrative_ontology:affects_constraint(commerce_clause_scope__broad_effects_test, federal_environmental_regulations).
narrative_ontology:affects_constraint(commerce_clause_scope__broad_effects_test, federal_civil_rights_legislation).
narrative_ontology:affects_constraint(commerce_clause_scope__broad_effects_test, state_police_powers).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the Commerce Clause scope kernel, each with different structural properties and classifications. This 'broad_effects_test' reading emphasizes aggregate economic impact and federal supremacy.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
