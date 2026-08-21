% ============================================================================
% CONSTRAINT STORY: takings_clause_boundary__regulatory_takings_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_takings_clause_boundary__regulatory_takings_reading, []).

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
 *   constraint_id: takings_clause_boundary__regulatory_takings_reading
 *   human_readable: Regulatory Takings Doctrine (Penn Central Reading)
 *   domain: constitutional_law/property_rights/regulatory_theory
 *
 * SUMMARY:
 *   This constraint represents the 'regulatory takings' reading of the Fifth
 *   Amendment's Takings Clause, primarily articulated through the Penn
 *   Central Transportation Co. v. City of New York (1978) decision. It holds
 *   that regulations, even without physical appropriation, can constitute a
 *   taking if they 'go too far' in diminishing a property's economic value,
 *   requiring compensation. This reading expanded the scope of takings beyond
 *   direct physical seizures, introducing an ad hoc, multi-factor balancing
 *   test. The claimed type is 'tangled_rope' because it genuinely coordinates
 *   the balance between private rights and public good, but also involves
 *   significant, asymmetric extraction from public regulatory capacity.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(takings_clause_boundary__regulatory_takings_reading, 0.65).
domain_priors:suppression_score(takings_clause_boundary__regulatory_takings_reading, 0.45).
domain_priors:theater_ratio(takings_clause_boundary__regulatory_takings_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(takings_clause_boundary__regulatory_takings_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(takings_clause_boundary__regulatory_takings_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(takings_clause_boundary__regulatory_takings_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(takings_clause_boundary__regulatory_takings_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(takings_clause_boundary__regulatory_takings_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(takings_clause_boundary__regulatory_takings_reading, tangled_rope).
narrative_ontology:human_readable(takings_clause_boundary__regulatory_takings_reading, "Regulatory Takings Doctrine (Penn Central Reading)").
narrative_ontology:topic_domain(takings_clause_boundary__regulatory_takings_reading, "constitutional_law/property_rights/regulatory_theory").

domain_priors:requires_active_enforcement(takings_clause_boundary__regulatory_takings_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(takings_clause_boundary__regulatory_takings_reading, '4b173c11-d418-4176-887a-cfadfe92fa1a').
narrative_ontology:cs_kernel_codification('4b173c11-d418-4176-887a-cfadfe92fa1a', fixed_text).
narrative_ontology:cs_authority_grounding('4b173c11-d418-4176-887a-cfadfe92fa1a', lineage).
narrative_ontology:cs_interpretation_layer_present('4b173c11-d418-4176-887a-cfadfe92fa1a').
narrative_ontology:cs_reading_relation('4b173c11-d418-4176-887a-cfadfe92fa1a', takings_clause_boundary__physical_appropriation_reading, influences).
narrative_ontology:cs_reading_relation('4b173c11-d418-4176-887a-cfadfe92fa1a', takings_clause_boundary__categorical_takings_reading, coexists_with).
narrative_ontology:cs_axiom('4b173c11-d418-4176-887a-cfadfe92fa1a', foundational, economic_value_diminution_is_a_taking).
narrative_ontology:cs_axiom_status(economic_value_diminution_is_a_taking, holdable).
narrative_ontology:cs_axiom_grounding('4b173c11-d418-4176-887a-cfadfe92fa1a', economic_value_diminution_is_a_taking, conventional).
narrative_ontology:cs_axiom('4b173c11-d418-4176-887a-cfadfe92fa1a', foundational, ad_hoc_balancing_is_appropriate).
narrative_ontology:cs_axiom_status(ad_hoc_balancing_is_appropriate, holdable).
narrative_ontology:cs_axiom_grounding('4b173c11-d418-4176-887a-cfadfe92fa1a', ad_hoc_balancing_is_appropriate, conventional).
narrative_ontology:cs_reference_frame('4b173c11-d418-4176-887a-cfadfe92fa1a', penn_central_balancing_framework).
narrative_ontology:cs_drift_state('4b173c11-d418-4176-887a-cfadfe92fa1a', contemporary_jurisprudence, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('4b173c11-d418-4176-887a-cfadfe92fa1a', '').
narrative_ontology:cs_kernel_id(takings_clause_boundary__regulatory_takings_reading, takings_clause_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(takings_clause_boundary__regulatory_takings_reading, property_owners).
narrative_ontology:constraint_beneficiary(takings_clause_boundary__regulatory_takings_reading, developers).
narrative_ontology:constraint_victim(takings_clause_boundary__regulatory_takings_reading, local_governments).
narrative_ontology:constraint_victim(takings_clause_boundary__regulatory_takings_reading, environmental_regulators).
narrative_ontology:constraint_victim(takings_clause_boundary__regulatory_takings_reading, public_interest_advocates).
narrative_ontology:constraint_vindicates(takings_clause_boundary__regulatory_takings_reading, economic_liberty_doctrine).
narrative_ontology:constraint_vindicates(takings_clause_boundary__regulatory_takings_reading, private_property_rights).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from the potential for compensation when regulations severely diminish their property's economic value, even without physical occupation. This provides a check on government power but introduces uncertainty into regulatory planning.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__regulatory_takings_reading, property_owners, beneficiary,
    powerful, biographical, constrained, local).

% Utilize the doctrine to challenge land-use restrictions or environmental regulations that significantly impact their project's profitability, potentially securing compensation or forcing regulatory changes. Their leverage is tied to the economic impact of the regulation.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__regulatory_takings_reading, developers, beneficiary,
    organized, biographical, constrained, regional).

% Bear the cost of potential compensation payments or the chilling effect on public-interest regulations due to the risk of takings claims. They must navigate the ad hoc Penn Central balancing test, leading to unpredictable outcomes and increased litigation costs.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__regulatory_takings_reading, local_governments, payer,
    institutional, generational, constrained, local).

% Face challenges to regulations designed to protect natural resources or public health, as these can be framed as diminishing economic value. The doctrine forces them to consider compensation costs, potentially weakening environmental protections.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__regulatory_takings_reading, environmental_regulators, payer,
    institutional, generational, constrained, national).

% Advocate for regulations that serve collective goods (e.g., historic preservation, open space) but find their efforts hampered by the threat of takings claims. They bear the cost of diminished regulatory capacity and increased legal battles.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__regulatory_takings_reading, public_interest_advocates, payer,
    moderate, generational, constrained, national).

% Interprets and applies the Takings Clause, setting the legal standards for regulatory takings. Its decisions shape the balance between private property rights and public regulatory power, often through complex, fact-specific balancing tests.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__regulatory_takings_reading, supreme_court, agenda_setter,
    institutional, civilizational, analytical, national).

% Analyze the evolving jurisprudence of regulatory takings, critiquing its coherence, predictability, and impact on both property rights and public welfare. They provide the intellectual framework for future legal arguments.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__regulatory_takings_reading, legal_scholars, observer,
    analytical, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a mechanism for adjudicating disputes between private property rights and public regulatory power, ensuring that individuals are not forced to bear disproportionate public burdens without compensation.
% TRANSFER_FUNCTION: Potentially transfers financial compensation from government entities (taxpayers) to property owners when regulations are deemed to 'go too far' in diminishing economic value.
% ABSENT_VOICES: Future generations, who would benefit from robust environmental and land-use regulations, are not directly represented in the balancing test, which tends to prioritize present economic value over long-term public goods.
% DISAPPEARANCE_RATIONALE: If the regulatory takings doctrine vanished, governments would have significantly more freedom to regulate land use and economic activity without fear of compensation claims. This would likely lead to more aggressive environmental protection, zoning, and public health regulations, fundamentally altering the balance of power between the state and private property owners.
% FOUNDING_PROBLEM: To prevent government from effectively confiscating private property through regulation without paying for it, ensuring that the costs of public benefits are borne by the public as a whole, not just individual property owners.
% FOUNDING_PROBLEM_CORROBORATION: Property rights advocates and many legal scholars attest that the problem of regulatory overreach remains live, requiring judicial oversight. Government entities and some public interest groups argue the doctrine itself creates problems by chilling necessary regulation, but acknowledge the underlying tension between private rights and public good is ongoing.
narrative_ontology:disappearance_verdict(takings_clause_boundary__regulatory_takings_reading, world_rearranges).
narrative_ontology:founding_problem_status(takings_clause_boundary__regulatory_takings_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(takings_clause_boundary__regulatory_takings_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(takings_clause_boundary__regulatory_takings_reading, 'none', 1).
narrative_ontology:epsilon_provenance(takings_clause_boundary__regulatory_takings_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(takings_clause_boundary__regulatory_takings_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(takings_clause_boundary__regulatory_takings_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(takings_clause_boundary__regulatory_takings_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.65) because the doctrine imposes significant costs on public regulatory bodies, either through direct compensation or by chilling beneficial regulations. Suppression (0.45) is moderate, reflecting the legal and financial barriers it places on government action, but not a complete collapse of regulatory alternatives. Theater ratio (0.20) is low, as the legal process is genuinely adversarial, though some arguments may be performative. The increasing extractiveness and suppression over time reflect the expansion of property rights jurisprudence and the increasing litigation burden on governments.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of property owners, this is a vital 'rope' protecting fundamental rights against government overreach. From the perspective of regulators and public interest advocates, it can function as a 'snare' that extracts public resources and suppresses necessary collective action, forcing the public to pay for benefits that should be uncompensated. The engine's classification will reflect this divergence based on the structural positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Property owners and developers are beneficiaries (low directionality) as the doctrine provides them a legal avenue for compensation and leverage against regulations. Local governments and environmental regulators are targets (high directionality) as they bear the costs and constraints imposed by the doctrine. Public interest advocates are also targets, as their goals are often undermined. The Supreme Court, as the agenda-setter, shapes the doctrine's application.
 *
 * MANDATROPHY ANALYSIS:
 *   The doctrine's mandate to prevent uncompensated confiscation remains live. However, the 'going too far' standard introduces significant ambiguity, leading to high litigation costs and a chilling effect on regulation that may exceed the original intent of preventing outright confiscation. The ad hoc nature of the Penn Central test means its application is often contested, preventing a clear resolution of whether its function has atrophied or merely shifted.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    too_far_definition_ambiguity,
    'What constitutes ''going too far'' in diminishing economic value, and is this standard consistently applied across cases?',
    'Empirical analysis of judicial decisions to identify consistent patterns in the application of the Penn Central factors, or legislative clarification of the threshold for compensation.',
    'A clearer, more consistent definition would reduce litigation costs and regulatory uncertainty, potentially lowering the doctrine''s extractiveness. Persistent ambiguity maintains the current high transaction costs.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(too_far_definition_ambiguity, empirical, 'Ambiguity in the ''too far'' standard of regulatory takings.').

omega_variable(
    public_burden_distribution,
    'Is the regulatory takings doctrine effectively distributing the costs of public benefits across the public, or is it disproportionately benefiting a subset of property owners at the expense of broader public goods?',
    'Economic analysis of compensation awards versus the societal benefits of challenged regulations, and a normative assessment of who should bear the costs of public welfare improvements.',
    'If costs are disproportionately borne by the public for private gain, the doctrine''s ''coordination'' function is undermined, pushing it closer to a ''snare''. If costs are genuinely distributed, its ''rope'' aspects are strengthened.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(public_burden_distribution, preference, 'Whether the doctrine achieves its goal of fair burden distribution.').

omega_variable(
    chilling_effect_quantification,
    'To what extent does the threat of regulatory takings claims actually deter or weaken beneficial public regulations (the ''chilling effect'')?',
    'Comparative studies of regulatory outcomes in jurisdictions with different takings jurisprudence, or surveys of government planners and regulators regarding their decision-making processes.',
    'A strong, quantifiable chilling effect would increase the measured suppression and extractiveness, highlighting the doctrine''s cost to public welfare. A weak effect would suggest the doctrine is less impactful than claimed by its critics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(chilling_effect_quantification, empirical, 'Quantification of the chilling effect on public regulation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(takings_clause_boundary__regulatory_takings_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(taki_tr_t0, takings_clause_boundary__regulatory_takings_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(taki_tr_t10, takings_clause_boundary__regulatory_takings_reading, theater_ratio, 10, 0.12).
narrative_ontology:measurement(taki_tr_t20, takings_clause_boundary__regulatory_takings_reading, theater_ratio, 20, 0.15).
narrative_ontology:measurement(taki_tr_t30, takings_clause_boundary__regulatory_takings_reading, theater_ratio, 30, 0.18).
narrative_ontology:measurement(taki_tr_t40, takings_clause_boundary__regulatory_takings_reading, theater_ratio, 40, 0.2).

% Extraction over time
narrative_ontology:measurement(taki_be_t0, takings_clause_boundary__regulatory_takings_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(taki_be_t10, takings_clause_boundary__regulatory_takings_reading, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(taki_be_t20, takings_clause_boundary__regulatory_takings_reading, base_extractiveness, 20, 0.6).
narrative_ontology:measurement(taki_be_t30, takings_clause_boundary__regulatory_takings_reading, base_extractiveness, 30, 0.63).
narrative_ontology:measurement(taki_be_t40, takings_clause_boundary__regulatory_takings_reading, base_extractiveness, 40, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(taki_su_t0, takings_clause_boundary__regulatory_takings_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(taki_su_t10, takings_clause_boundary__regulatory_takings_reading, suppression_requirement, 10, 0.38).
narrative_ontology:measurement(taki_su_t20, takings_clause_boundary__regulatory_takings_reading, suppression_requirement, 20, 0.4).
narrative_ontology:measurement(taki_su_t30, takings_clause_boundary__regulatory_takings_reading, suppression_requirement, 30, 0.43).
narrative_ontology:measurement(taki_su_t40, takings_clause_boundary__regulatory_takings_reading, suppression_requirement, 40, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
