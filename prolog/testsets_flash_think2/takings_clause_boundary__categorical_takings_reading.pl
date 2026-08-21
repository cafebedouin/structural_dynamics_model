% ============================================================================
% CONSTRAINT STORY: takings_clause_boundary__categorical_takings_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_takings_clause_boundary__categorical_takings_reading, []).

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
 *   constraint_id: takings_clause_boundary__categorical_takings_reading
 *   human_readable: Categorical Takings Rule with Penn Central Balancing
 *   domain: constitutional_law/property_rights/regulatory_theory
 *
 * SUMMARY:
 *   This constraint represents the 'categorical takings' reading of the Fifth
 *   Amendment's Takings Clause, as established by Supreme Court jurisprudence
 *   (e.g., Loretto, Lucas, Penn Central). It holds that permanent physical
 *   occupations and regulations that eliminate all economically beneficial
 *   use of property are 'per se' takings requiring compensation. All other
 *   regulations are evaluated under the more flexible, multi-factor Penn
 *   Central balancing test. This reading attempts to provide bright-line
 *   rules for extreme cases while preserving regulatory flexibility for the
 *   vast majority of government actions.
 *
 * KEY AGENTS:
 *   - government_regulators: Agenda setter (institutional/constrained)
 *   - property_owners: Payer (powerful/constrained)
 *   - developers: Payer (organized/constrained)
 *   - public_interest_advocates: Beneficiary (organized/mobile)
 *   - courts: Agenda setter (institutional/analytical)
 *   - legal_scholars: Observer (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(takings_clause_boundary__categorical_takings_reading, 0.68).
domain_priors:suppression_score(takings_clause_boundary__categorical_takings_reading, 0.75).
domain_priors:theater_ratio(takings_clause_boundary__categorical_takings_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(takings_clause_boundary__categorical_takings_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(takings_clause_boundary__categorical_takings_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(takings_clause_boundary__categorical_takings_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(takings_clause_boundary__categorical_takings_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(takings_clause_boundary__categorical_takings_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(takings_clause_boundary__categorical_takings_reading, tangled_rope).
narrative_ontology:human_readable(takings_clause_boundary__categorical_takings_reading, "Categorical Takings Rule with Penn Central Balancing").
narrative_ontology:topic_domain(takings_clause_boundary__categorical_takings_reading, "constitutional_law/property_rights/regulatory_theory").

domain_priors:requires_active_enforcement(takings_clause_boundary__categorical_takings_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(takings_clause_boundary__categorical_takings_reading, 'fa1e52df-60c2-43e6-a2ee-d797030d0d8f').
narrative_ontology:cs_kernel_codification('fa1e52df-60c2-43e6-a2ee-d797030d0d8f', fixed_text).
narrative_ontology:cs_authority_grounding('fa1e52df-60c2-43e6-a2ee-d797030d0d8f', lineage).
narrative_ontology:cs_interpretation_layer_present('fa1e52df-60c2-43e6-a2ee-d797030d0d8f').
narrative_ontology:cs_reading_relation('fa1e52df-60c2-43e6-a2ee-d797030d0d8f', takings_clause_boundary__physical_appropriation_reading, forecloses).
narrative_ontology:cs_reading_relation('fa1e52df-60c2-43e6-a2ee-d797030d0d8f', takings_clause_boundary__regulatory_takings_reading, influences).
narrative_ontology:cs_axiom('fa1e52df-60c2-43e6-a2ee-d797030d0d8f', foundational, private_property_fundamental_right).
narrative_ontology:cs_axiom_status(private_property_fundamental_right, holdable).
narrative_ontology:cs_axiom_grounding('fa1e52df-60c2-43e6-a2ee-d797030d0d8f', private_property_fundamental_right, deontological).
narrative_ontology:cs_axiom('fa1e52df-60c2-43e6-a2ee-d797030d0d8f', foundational, regulatory_power_essential_for_public_welfare).
narrative_ontology:cs_axiom_status(regulatory_power_essential_for_public_welfare, holdable).
narrative_ontology:cs_axiom_grounding('fa1e52df-60c2-43e6-a2ee-d797030d0d8f', regulatory_power_essential_for_public_welfare, instrumental).
narrative_ontology:cs_reference_frame('fa1e52df-60c2-43e6-a2ee-d797030d0d8f', penn_central_balancing_framework).
narrative_ontology:cs_drift_state('fa1e52df-60c2-43e6-a2ee-d797030d0d8f', contemporary_judicial_review, gap(stable, minor, true)).
narrative_ontology:cs_created_at('fa1e52df-60c2-43e6-a2ee-d797030d0d8f', '').
narrative_ontology:cs_kernel_id(takings_clause_boundary__categorical_takings_reading, takings_clause_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(takings_clause_boundary__categorical_takings_reading, government_regulators).
narrative_ontology:constraint_beneficiary(takings_clause_boundary__categorical_takings_reading, public_interest_advocates).
narrative_ontology:constraint_victim(takings_clause_boundary__categorical_takings_reading, property_owners).
narrative_ontology:constraint_victim(takings_clause_boundary__categorical_takings_reading, developers).
narrative_ontology:constraint_vindicates(takings_clause_boundary__categorical_takings_reading, regulatory_flexibility_doctrine).
narrative_ontology:constraint_vindicates(takings_clause_boundary__categorical_takings_reading, economic_substantive_due_process_limits).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Implement regulations for public welfare, environmental protection, and zoning. They benefit from the flexibility of the Penn Central test, which often allows regulations without compensation, and from the clear boundaries of the per se rules, which prevent frivolous claims.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__categorical_takings_reading, government_regulators, agenda_setter,
    institutional, generational, constrained, national).

% Bear the costs of regulations that diminish property value but do not fall into the per se categories. They gain predictability for extreme cases but face significant uncertainty and litigation costs when challenging regulations under Penn Central.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__categorical_takings_reading, property_owners, payer,
    powerful, biographical, constrained, local).

% Are subject to regulations that impact development potential and profitability. They navigate the legal framework to assess risk and potential compensation claims, often absorbing costs or passing them to consumers.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__categorical_takings_reading, developers, payer,
    organized, biographical, constrained, regional).

% Advocate for regulations that protect the environment, historical sites, or public access. They benefit from a legal framework that generally permits such regulations without requiring government compensation, enabling broader public welfare initiatives.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__categorical_takings_reading, public_interest_advocates, beneficiary,
    organized, generational, mobile, national).

% Interpret and apply the Takings Clause framework, adjudicating disputes between property owners and government. Their decisions shape the boundaries of what constitutes a taking and how compensation is determined.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__categorical_takings_reading, courts, agenda_setter,
    institutional, civilizational, analytical, national).

% Analyze the coherence, fairness, and economic impact of the Takings Clause jurisprudence. They provide critical commentary and propose alternative frameworks, influencing future legal development.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__categorical_takings_reading, legal_scholars, observer,
    analytical, generational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(takings_clause_boundary__categorical_takings_reading, government_regulators).
narrative_ontology:fixing_cost_class(takings_clause_boundary__categorical_takings_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the balance between private property rights and the government's police power to regulate for public welfare, providing clear rules for extreme cases and a flexible test for others.
% TRANSFER_FUNCTION: Transfers the cost of regulations that diminish property value (but are not per se takings) from the public/government to individual property owners, while ensuring compensation for permanent physical occupations or total value eliminations.
% ABSENT_VOICES: Advocates for absolute property rights (who would reject any uncompensated regulation) and proponents of unlimited government regulatory power (who would reject any compensation requirement) are structurally marginalized by this balancing framework.
% DISAPPEARANCE_RATIONALE: Without this framework, the fundamental tension between private property and public regulation would lead to legal chaos. Property owners would face unpredictable government actions, and governments would be paralyzed by potential compensation claims, fundamentally reorganizing land use, development, and public infrastructure.
% FOUNDING_PROBLEM: To define the constitutional boundary between legitimate government regulation and an unconstitutional 'taking' of private property that requires just compensation.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars, property rights organizations, and government agencies consistently acknowledge the ongoing challenge of balancing these interests, though they dispute the optimal application of the framework. Supreme Court cases continue to refine its edges.
narrative_ontology:disappearance_verdict(takings_clause_boundary__categorical_takings_reading, world_rearranges).
narrative_ontology:founding_problem_status(takings_clause_boundary__categorical_takings_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(takings_clause_boundary__categorical_takings_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(takings_clause_boundary__categorical_takings_reading, 'none', 1).
narrative_ontology:epsilon_provenance(takings_clause_boundary__categorical_takings_reading, 0.68, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(takings_clause_boundary__categorical_takings_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(takings_clause_boundary__categorical_takings_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(takings_clause_boundary__categorical_takings_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The `extractiveness` is moderately high (0.68) because the Penn Central test, while balancing, often allows significant diminution of property value without compensation, effectively transferring costs to property owners. `suppression` is high (0.75) as the legal framework actively suppresses claims that do not meet the high bar of per se rules or the complex Penn Central factors, making successful challenges difficult. `theater_ratio` is low (0.15) because the legal system is genuinely functional, though some arguments may be performative. `accessibility_collapse` is high (0.88) as there are no alternatives to this constitutional framework for resolving takings claims. `resistance` is moderate (0.55) due to ongoing litigation and advocacy by property rights groups, but the framework itself is generally accepted as the governing law.
 *
 * PERSPECTIVAL GAP:
 *   Government regulators and public interest advocates perceive this framework as a necessary and largely fair balance, enabling public welfare. Property owners and developers, however, often experience the Penn Central test as highly extractive and unpredictable, viewing the per se rules as insufficient protection against regulatory burdens. The courts, as agenda setters, aim for a consistent application of the law, but their decisions are often seen as favoring one side or the other.
 *
 * DIRECTIONALITY LOGIC:
 *   Government regulators and public interest advocates are beneficiaries, as the framework largely upholds the state's power to regulate without compensation. Property owners and developers are payers, bearing the costs of regulations that fall outside the narrow per se categories. Courts are agenda setters, interpreting and enforcing the constraint, while legal scholars act as observers.
 *
 * MANDATROPHY ANALYSIS:
 *   This framework prevents mislabeling legitimate regulation as pure extraction by providing a balancing test, while also preventing the state from taking property without compensation in extreme cases. The 'live' status of the founding problem and the 'world_rearranges' disappearance verdict indicate that the constraint's mandate is still active and essential, preventing mandatrophy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    total_value_elimination_ambiguity,
    'What constitutes ''total value elimination'' in practice, and how is it measured, especially for partial interests or future development rights?',
    'Further judicial clarification on the precise metrics and scope of ''total value elimination,'' potentially through legislative guidance or economic valuation standards.',
    'If ''total value elimination'' is interpreted narrowly, fewer regulations will trigger per se compensation, increasing effective extraction. If interpreted broadly, more regulations will require compensation, reducing extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(total_value_elimination_ambiguity, empirical, 'Ambiguity in defining and measuring ''total value elimination'' for per se takings.').

omega_variable(
    penn_central_factors_subjectivity,
    'How consistently and predictably are the Penn Central factors (economic impact, interference with investment-backed expectations, character of government action) applied across different courts and jurisdictions?',
    'Empirical study of judicial decisions applying Penn Central, quantitative analysis of outcomes, and development of more standardized guidelines for factor evaluation.',
    'Greater consistency would increase predictability for property owners and regulators, potentially reducing litigation costs. High subjectivity increases uncertainty and perceived extraction for property owners.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(penn_central_factors_subjectivity, conceptual, 'Subjectivity and variability in applying the Penn Central balancing test.').

omega_variable(
    predictability_vs_flexibility_balance,
    'What is the optimal balance between providing clear, predictable rules for property owners and maintaining sufficient flexibility for government to regulate for evolving public needs?',
    'This is a preference question, resolvable through legislative policy choices or ongoing societal consensus shifts, rather than empirical data or conceptual clarification.',
    'A shift towards more bright-line rules would reduce regulatory flexibility but increase property owner certainty. A shift towards greater flexibility would increase regulatory power but reduce property owner certainty.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(predictability_vs_flexibility_balance, preference, 'The inherent tension between predictability for property owners and regulatory flexibility for government.').

omega_variable(
    kernel_reading_identity,
    'Is this constraint a genuine, distinct reading of the Takings Clause, or merely a specific application of a broader principle?',
    'Analysis of the logical coherence and distinct foundational axioms of this reading compared to its siblings, and whether it can be held without internal contradiction alongside other readings.',
    'If it is a distinct reading, its classification stands as an independent structural claim. If it is merely an application, its classification might be subsumed under a more foundational reading, or its distinctiveness might be re-evaluated as a ''sub-constraint'' within a larger framework.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'This constraint is one reading of the ''takings_clause_boundary'' kernel, specifically the ''categorical_takings_reading''.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(takings_clause_boundary__categorical_takings_reading, 1978, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(taki_tr_t1978, takings_clause_boundary__categorical_takings_reading, theater_ratio, 1978, 0.1).
narrative_ontology:measurement(taki_tr_t1988, takings_clause_boundary__categorical_takings_reading, theater_ratio, 1988, 0.12).
narrative_ontology:measurement(taki_tr_t1998, takings_clause_boundary__categorical_takings_reading, theater_ratio, 1998, 0.13).
narrative_ontology:measurement(taki_tr_t2008, takings_clause_boundary__categorical_takings_reading, theater_ratio, 2008, 0.14).
narrative_ontology:measurement(taki_tr_t2018, takings_clause_boundary__categorical_takings_reading, theater_ratio, 2018, 0.15).
narrative_ontology:measurement(taki_tr_t2023, takings_clause_boundary__categorical_takings_reading, theater_ratio, 2023, 0.15).

% Extraction over time
narrative_ontology:measurement(taki_be_t1978, takings_clause_boundary__categorical_takings_reading, base_extractiveness, 1978, 0.6).
narrative_ontology:measurement(taki_be_t1988, takings_clause_boundary__categorical_takings_reading, base_extractiveness, 1988, 0.63).
narrative_ontology:measurement(taki_be_t1998, takings_clause_boundary__categorical_takings_reading, base_extractiveness, 1998, 0.65).
narrative_ontology:measurement(taki_be_t2008, takings_clause_boundary__categorical_takings_reading, base_extractiveness, 2008, 0.67).
narrative_ontology:measurement(taki_be_t2018, takings_clause_boundary__categorical_takings_reading, base_extractiveness, 2018, 0.68).
narrative_ontology:measurement(taki_be_t2023, takings_clause_boundary__categorical_takings_reading, base_extractiveness, 2023, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(taki_su_t1978, takings_clause_boundary__categorical_takings_reading, suppression_requirement, 1978, 0.7).
narrative_ontology:measurement(taki_su_t1988, takings_clause_boundary__categorical_takings_reading, suppression_requirement, 1988, 0.72).
narrative_ontology:measurement(taki_su_t1998, takings_clause_boundary__categorical_takings_reading, suppression_requirement, 1998, 0.73).
narrative_ontology:measurement(taki_su_t2008, takings_clause_boundary__categorical_takings_reading, suppression_requirement, 2008, 0.74).
narrative_ontology:measurement(taki_su_t2018, takings_clause_boundary__categorical_takings_reading, suppression_requirement, 2018, 0.75).
narrative_ontology:measurement(taki_su_t2023, takings_clause_boundary__categorical_takings_reading, suppression_requirement, 2023, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(takings_clause_boundary__categorical_takings_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(takings_clause_boundary__categorical_takings_reading, environmental_regulations).
narrative_ontology:affects_constraint(takings_clause_boundary__categorical_takings_reading, zoning_laws).
narrative_ontology:affects_constraint(takings_clause_boundary__categorical_takings_reading, historic_preservation_ordinances).

% DUAL FORMULATION NOTE:
% This constraint is part of the 'takings_clause_boundary' constraint family, which decomposes into multiple readings of the Fifth Amendment's Takings Clause. This specific reading provides categorical rules for per se takings and a balancing test for others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
