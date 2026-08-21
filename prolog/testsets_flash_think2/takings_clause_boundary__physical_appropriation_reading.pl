% ============================================================================
% CONSTRAINT STORY: takings_clause_boundary__physical_appropriation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_takings_clause_boundary__physical_appropriation_reading, []).

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
 *   constraint_id: takings_clause_boundary__physical_appropriation_reading
 *   human_readable: Takings Clause Boundary: Physical Appropriation Reading
 *   domain: constitutional_law/property_rights/regulatory_theory
 *
 * SUMMARY:
 *   This constraint story instantiates the 'physical appropriation' reading
 *   of the Takings Clause, which holds that only direct physical seizures or
 *   permanent physical occupations of private property by the government
 *   trigger the constitutional requirement for just compensation. Under this
 *   reading, regulations that merely diminish property value, no matter how
 *   severely, do not constitute a 'taking' unless they result in a physical
 *   invasion. This interpretation grants broad power to government regulators
 *   while placing significant uncompensated burdens on property owners.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(takings_clause_boundary__physical_appropriation_reading, 0.7).
domain_priors:suppression_score(takings_clause_boundary__physical_appropriation_reading, 0.8).
domain_priors:theater_ratio(takings_clause_boundary__physical_appropriation_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(takings_clause_boundary__physical_appropriation_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(takings_clause_boundary__physical_appropriation_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(takings_clause_boundary__physical_appropriation_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(takings_clause_boundary__physical_appropriation_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(takings_clause_boundary__physical_appropriation_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(takings_clause_boundary__physical_appropriation_reading, tangled_rope).
narrative_ontology:human_readable(takings_clause_boundary__physical_appropriation_reading, "Takings Clause Boundary: Physical Appropriation Reading").
narrative_ontology:topic_domain(takings_clause_boundary__physical_appropriation_reading, "constitutional_law/property_rights/regulatory_theory").

domain_priors:requires_active_enforcement(takings_clause_boundary__physical_appropriation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(takings_clause_boundary__physical_appropriation_reading, 'e8af3310-28dd-4251-9be4-6aa479e6c4f3').
narrative_ontology:cs_kernel_codification('e8af3310-28dd-4251-9be4-6aa479e6c4f3', fixed_text).
narrative_ontology:cs_authority_grounding('e8af3310-28dd-4251-9be4-6aa479e6c4f3', lineage).
narrative_ontology:cs_interpretation_layer_present('e8af3310-28dd-4251-9be4-6aa479e6c4f3').
narrative_ontology:cs_reading_relation('e8af3310-28dd-4251-9be4-6aa479e6c4f3', takings_clause_boundary__regulatory_takings_reading, coexists_with).
narrative_ontology:cs_reading_relation('e8af3310-28dd-4251-9be4-6aa479e6c4f3', takings_clause_boundary__categorical_takings_reading, coexists_with).
narrative_ontology:cs_axiom('e8af3310-28dd-4251-9be4-6aa479e6c4f3', foundational, direct_physical_invasion_is_the_threshold).
narrative_ontology:cs_axiom_status(direct_physical_invasion_is_the_threshold, holdable).
narrative_ontology:cs_axiom_grounding('e8af3310-28dd-4251-9be4-6aa479e6c4f3', direct_physical_invasion_is_the_threshold, conventional).
narrative_ontology:cs_axiom('e8af3310-28dd-4251-9be4-6aa479e6c4f3', foundational, economic_impact_alone_is_not_a_taking).
narrative_ontology:cs_axiom_status(economic_impact_alone_is_not_a_taking, holdable).
narrative_ontology:cs_axiom_grounding('e8af3310-28dd-4251-9be4-6aa479e6c4f3', economic_impact_alone_is_not_a_taking, conventional).
narrative_ontology:cs_reference_frame('e8af3310-28dd-4251-9be4-6aa479e6c4f3', narrow_historical_interpretation_of_takings).
narrative_ontology:cs_drift_state('e8af3310-28dd-4251-9be4-6aa479e6c4f3', contemporary_judicial_discourse, gap(stable, minor, true)).
narrative_ontology:cs_created_at('e8af3310-28dd-4251-9be4-6aa479e6c4f3', '').
narrative_ontology:cs_kernel_id(takings_clause_boundary__physical_appropriation_reading, takings_clause_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(takings_clause_boundary__physical_appropriation_reading, government_regulators).
narrative_ontology:constraint_beneficiary(takings_clause_boundary__physical_appropriation_reading, public_interest_advocates).
narrative_ontology:constraint_victim(takings_clause_boundary__physical_appropriation_reading, property_owners_suffering_regulatory_losses).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(takings_clause_boundary__physical_appropriation_reading, developers_and_investors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These agents (e.g., environmental protection agencies, zoning boards) benefit from the broad power to regulate land use and property without triggering compensation, allowing them to pursue public welfare goals more freely. They enforce regulations that may diminish property value without physical seizure.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__physical_appropriation_reading, government_regulators, agenda_setter,
    institutional, generational, constrained, national).

% These property owners bear the economic costs of regulations that significantly diminish the value or use of their property, but do not involve a direct physical seizure or permanent occupation. Their only recourse is to challenge the regulation itself, not to claim compensation under the Takings Clause.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__physical_appropriation_reading, property_owners_suffering_regulatory_losses, payer,
    organized, biographical, constrained, local).

% Groups advocating for environmental protection, historic preservation, or public access to resources benefit from this reading, as it empowers the government to enact regulations serving the public good without the fiscal burden of widespread compensation claims.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__physical_appropriation_reading, public_interest_advocates, beneficiary,
    organized, generational, mobile, national).

% The judiciary interprets and enforces the Takings Clause, defining the boundary between legitimate regulation and compensable taking. This reading reflects a judicial philosophy that prioritizes governmental police power over individual economic loss in the absence of physical appropriation.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__physical_appropriation_reading, courts, agenda_setter,
    institutional, civilizational, analytical, national).

% These actors must factor in the risk of regulatory changes that can significantly impact their property investments without compensation. While they can adapt by choosing different projects or locations, they still bear the uncompensated losses when regulations are applied to their existing holdings.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__physical_appropriation_reading, developers_and_investors, payer,
    powerful, immediate, mobile, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(takings_clause_boundary__physical_appropriation_reading, government_regulators).
narrative_ontology:fixing_cost_class(takings_clause_boundary__physical_appropriation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the government's exercise of its police power to regulate for public welfare (e.g., health, safety, environment) with the constitutional protection of private property rights, by narrowly defining when compensation is required.
% TRANSFER_FUNCTION: Transfers the economic burden of regulatory impacts that do not involve physical appropriation from the public (via government compensation) to individual property owners.
% ABSENT_VOICES: Property owners who experience severe economic diminution from regulations without physical invasion would object, arguing that their property has been 'taken' in all but name, and that the current standard leaves them without adequate remedy.
% DISAPPEARANCE_RATIONALE: If this narrow interpretation vanished, every regulation significantly impacting property value could potentially trigger a compensation claim, leading to a massive increase in litigation, paralyzing government's ability to regulate, and potentially bankrupting public treasuries. The balance of power between the state and private property would fundamentally shift.
% FOUNDING_PROBLEM: The Fifth Amendment's Takings Clause was established to prevent the government from arbitrarily seizing private property for public use without just compensation, balancing the needs of the public with the rights of individuals.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars, historical records of constitutional debates, and ongoing Supreme Court cases consistently demonstrate the enduring tension and live nature of balancing private property rights against public welfare, confirming the problem's persistence from outside the benefiting parties.
narrative_ontology:disappearance_verdict(takings_clause_boundary__physical_appropriation_reading, world_rearranges).
narrative_ontology:founding_problem_status(takings_clause_boundary__physical_appropriation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(takings_clause_boundary__physical_appropriation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(takings_clause_boundary__physical_appropriation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(takings_clause_boundary__physical_appropriation_reading, 0.7, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(takings_clause_boundary__physical_appropriation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(takings_clause_boundary__physical_appropriation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(takings_clause_boundary__physical_appropriation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.70) is high because this reading allows the government to impose substantial economic costs on property owners through regulation without compensation. Suppression (0.80) is also high, as property owners are legally barred from seeking compensation for non-physical regulatory impacts, effectively suppressing their claims. The theater ratio (0.10) is low, as this is a fundamental legal principle actively applied by courts, not a performative or atrophied function. Accessibility collapse (0.75) is high for regulatory takings claims, as the legal path to compensation is largely closed off. Resistance (0.60) is moderate, reflecting ongoing litigation and academic debate from property rights advocates seeking a broader interpretation.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of government regulators and public interest advocates, this reading is a necessary coordination mechanism that enables effective governance for the common good. From the perspective of property owners suffering regulatory losses, it is a highly extractive mechanism that forces them to bear disproportionate costs for public benefit without just compensation. The courts, as agenda-setters, mediate this fundamental tension.
 *
 * DIRECTIONALITY LOGIC:
 *   Government regulators and public interest advocates are clear beneficiaries, as this reading expands their capacity to achieve policy goals without fiscal penalty. Property owners, particularly those whose land is heavily regulated, are the primary victims, bearing the uncompensated costs. Courts, while enforcing the constraint, also benefit from a clearer, albeit narrower, rule for adjudication.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents the 'mandatrophy' of government's police power by limiting the financial burden of compensation. If every regulation that 'went too far' economically required compensation, the government's ability to address public problems (e.g., environmental degradation, urban planning) would atrophy due to prohibitive costs. By narrowly defining 'taking,' the constraint ensures the regulatory mandate remains viable, albeit at the cost of property owners' uncompensated losses.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint accurately identified as the ''physical appropriation'' reading of the Takings Clause boundary?',
    'Analysis of judicial opinions and legal scholarship to confirm the distinct interpretive framework and its consistent application.',
    'If misidentified, the analysis of its structural relations to other readings (e.g., regulatory takings) would be flawed, leading to incorrect classification of the broader kernel contest.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Confirms the specific reading of the Takings Clause being analyzed.').

omega_variable(
    scope_of_physical_occupation,
    'What constitutes a ''permanent physical occupation'' for the purpose of triggering compensation, particularly in cases of minimal or non-invasive government action?',
    'Further judicial clarification or legislative definition of the threshold for ''physical occupation,'' especially concerning new technologies or indirect impacts.',
    'A broader definition of ''physical occupation'' would expand the victim set and increase the constraint''s effective extractiveness on the government, potentially shifting its classification towards a more balanced ''rope'' or even ''scaffold'' if temporary. A narrower definition would reinforce its current extractive nature.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scope_of_physical_occupation, empirical, 'Ambiguity in defining ''physical occupation'' for compensation.').

omega_variable(
    regulatory_burden_threshold,
    'At what point does a regulatory burden, absent physical appropriation, become so severe that it functionally equates to a taking, even under this narrow reading?',
    'Empirical studies of property value diminution under various regulations, combined with judicial re-evaluation of the ''nuisance exception'' or ''background principles of property law'' that might justify uncompensated burdens.',
    'If a de facto threshold for regulatory burden is acknowledged, it would introduce a ''regulatory takings'' element into this reading, increasing its complexity and potentially shifting its classification towards a ''tangled_rope'' with a more balanced extraction profile, or even a ''snare'' if the threshold is set impossibly high.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(regulatory_burden_threshold, conceptual, 'The conceptual boundary of ''too far'' for regulatory burdens without physical taking.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(takings_clause_boundary__physical_appropriation_reading, 1978, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(taki_tr_t1978, takings_clause_boundary__physical_appropriation_reading, theater_ratio, 1978, 0.1).
narrative_ontology:measurement(taki_tr_t1988, takings_clause_boundary__physical_appropriation_reading, theater_ratio, 1988, 0.1).
narrative_ontology:measurement(taki_tr_t1998, takings_clause_boundary__physical_appropriation_reading, theater_ratio, 1998, 0.1).
narrative_ontology:measurement(taki_tr_t2008, takings_clause_boundary__physical_appropriation_reading, theater_ratio, 2008, 0.1).
narrative_ontology:measurement(taki_tr_t2018, takings_clause_boundary__physical_appropriation_reading, theater_ratio, 2018, 0.1).
narrative_ontology:measurement(taki_tr_t2023, takings_clause_boundary__physical_appropriation_reading, theater_ratio, 2023, 0.1).

% Extraction over time
narrative_ontology:measurement(taki_be_t1978, takings_clause_boundary__physical_appropriation_reading, base_extractiveness, 1978, 0.65).
narrative_ontology:measurement(taki_be_t1988, takings_clause_boundary__physical_appropriation_reading, base_extractiveness, 1988, 0.68).
narrative_ontology:measurement(taki_be_t1998, takings_clause_boundary__physical_appropriation_reading, base_extractiveness, 1998, 0.7).
narrative_ontology:measurement(taki_be_t2008, takings_clause_boundary__physical_appropriation_reading, base_extractiveness, 2008, 0.69).
narrative_ontology:measurement(taki_be_t2018, takings_clause_boundary__physical_appropriation_reading, base_extractiveness, 2018, 0.7).
narrative_ontology:measurement(taki_be_t2023, takings_clause_boundary__physical_appropriation_reading, base_extractiveness, 2023, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(taki_su_t1978, takings_clause_boundary__physical_appropriation_reading, suppression_requirement, 1978, 0.75).
narrative_ontology:measurement(taki_su_t1988, takings_clause_boundary__physical_appropriation_reading, suppression_requirement, 1988, 0.78).
narrative_ontology:measurement(taki_su_t1998, takings_clause_boundary__physical_appropriation_reading, suppression_requirement, 1998, 0.8).
narrative_ontology:measurement(taki_su_t2008, takings_clause_boundary__physical_appropriation_reading, suppression_requirement, 2008, 0.79).
narrative_ontology:measurement(taki_su_t2018, takings_clause_boundary__physical_appropriation_reading, suppression_requirement, 2018, 0.8).
narrative_ontology:measurement(taki_su_t2023, takings_clause_boundary__physical_appropriation_reading, suppression_requirement, 2023, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(takings_clause_boundary__physical_appropriation_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(takings_clause_boundary__physical_appropriation_reading, regulatory_takings_reading).
narrative_ontology:affects_constraint(takings_clause_boundary__physical_appropriation_reading, categorical_takings_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the 'takings_clause_boundary' kernel. Each reading defines the compensation requirement differently, leading to different extraction profiles and stakeholder impacts. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
