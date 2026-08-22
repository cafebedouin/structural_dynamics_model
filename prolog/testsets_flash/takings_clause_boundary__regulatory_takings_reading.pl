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
 *   constraint_id: takings_clause_boundary__regulatory_takings_reading
 *   human_readable: Regulatory Takings Doctrine (Penn Central Reading)
 *   domain: constitutional_law/property_rights/regulatory_theory
 *
 * SUMMARY:
 *   This constraint describes the 'regulatory takings' doctrine, primarily
 *   articulated in Penn Central Transportation Co. v. City of New York
 *   (1978), which holds that regulations that 'go too far' in diminishing
 *   property value can constitute a taking requiring compensation, even
 *   without physical appropriation. This is one reading of the broader
 *   Takings Clause kernel, emphasizing economic impact over physical
 *   invasion. The doctrine introduces an ad hoc balancing test, creating
 *   uncertainty for regulators but offering property owners a mechanism to
 *   challenge value-diminishing regulations.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(takings_clause_boundary__regulatory_takings_reading, 0.65).
domain_priors:suppression_score(takings_clause_boundary__regulatory_takings_reading, 0.4).
domain_priors:theater_ratio(takings_clause_boundary__regulatory_takings_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(takings_clause_boundary__regulatory_takings_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(takings_clause_boundary__regulatory_takings_reading, suppression_requirement, 0.4).
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
narrative_ontology:cs_story_uid(takings_clause_boundary__regulatory_takings_reading, 'b3b3903a-cb06-43f9-bdde-65b569250e08').
narrative_ontology:cs_kernel_codification('b3b3903a-cb06-43f9-bdde-65b569250e08', fixed_text).
narrative_ontology:cs_authority_grounding('b3b3903a-cb06-43f9-bdde-65b569250e08', lineage).
narrative_ontology:cs_interpretation_layer_present('b3b3903a-cb06-43f9-bdde-65b569250e08').
narrative_ontology:cs_reading_relation('b3b3903a-cb06-43f9-bdde-65b569250e08', takings_clause_boundary__physical_appropriation_reading, coexists_with).
narrative_ontology:cs_reading_relation('b3b3903a-cb06-43f9-bdde-65b569250e08', takings_clause_boundary__categorical_takings_reading, coexists_with).
narrative_ontology:cs_axiom('b3b3903a-cb06-43f9-bdde-65b569250e08', foundational, economic_value_diminution_is_a_taking).
narrative_ontology:cs_axiom_status(economic_value_diminution_is_a_taking, holdable).
narrative_ontology:cs_axiom_grounding('b3b3903a-cb06-43f9-bdde-65b569250e08', economic_value_diminution_is_a_taking, deontological).
narrative_ontology:cs_axiom('b3b3903a-cb06-43f9-bdde-65b569250e08', secondary, ad_hoc_balancing_is_appropriate).
narrative_ontology:cs_axiom_status(ad_hoc_balancing_is_appropriate, holdable).
narrative_ontology:cs_axiom_grounding('b3b3903a-cb06-43f9-bdde-65b569250e08', ad_hoc_balancing_is_appropriate, conventional).
narrative_ontology:cs_reference_frame('b3b3903a-cb06-43f9-bdde-65b569250e08', penn_central_balancing_framework).
narrative_ontology:cs_drift_state('b3b3903a-cb06-43f9-bdde-65b569250e08', contemporary, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('b3b3903a-cb06-43f9-bdde-65b569250e08', '').
narrative_ontology:cs_kernel_id(takings_clause_boundary__regulatory_takings_reading, takings_clause_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(takings_clause_boundary__regulatory_takings_reading, property_owners).
narrative_ontology:constraint_beneficiary(takings_clause_boundary__regulatory_takings_reading, real_estate_developers).
narrative_ontology:constraint_victim(takings_clause_boundary__regulatory_takings_reading, local_governments).
narrative_ontology:constraint_victim(takings_clause_boundary__regulatory_takings_reading, environmental_regulators).
narrative_ontology:constraint_victim(takings_clause_boundary__regulatory_takings_reading, public_interest_advocates).
narrative_ontology:constraint_vindicates(takings_clause_boundary__regulatory_takings_reading, economic_liberty_doctrine).
narrative_ontology:constraint_vindicates(takings_clause_boundary__regulatory_takings_reading, limited_government_intervention).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Seek compensation when regulations significantly diminish their property's economic value, even without physical occupation. They benefit from the doctrine's protection against overreaching government action, but face high litigation costs.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__regulatory_takings_reading, property_owners, beneficiary,
    powerful, biographical, constrained, local).

% Benefit from the regulatory takings doctrine by using it to challenge land-use restrictions that reduce development potential, thereby reducing their costs or increasing potential profits. They have resources to litigate.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__regulatory_takings_reading, real_estate_developers, beneficiary,
    organized, biographical, mobile, regional).

% Bear the cost of potential compensation payments or the chilling effect on public-interest regulations due to fear of takings claims. They must balance public welfare with property rights, often facing legal challenges.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__regulatory_takings_reading, local_governments, payer,
    institutional, generational, constrained, local).

% Face legal challenges when implementing regulations to protect natural resources or public health, as these may be deemed regulatory takings. The doctrine constrains their ability to act decisively.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__regulatory_takings_reading, environmental_regulators, payer,
    institutional, generational, constrained, national).

% Advocate for regulations that serve collective goods (e.g., environmental protection, historic preservation) but find their efforts hampered by the threat of takings claims, which can make such regulations politically and financially unfeasible.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__regulatory_takings_reading, public_interest_advocates, payer,
    moderate, generational, constrained, national).

% Interprets the Takings Clause and applies the Penn Central balancing test, shaping the boundaries of regulatory power and property rights. Its decisions define the constraint and its enforcement.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__regulatory_takings_reading, supreme_court, agenda_setter,
    institutional, civilizational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a legal framework for adjudicating disputes between private property rights and government regulatory power, ensuring that the costs of public benefits are not unfairly borne by individual property owners.
% TRANSFER_FUNCTION: Potentially transfers financial compensation from government entities (and thus taxpayers) to private property owners when regulations are deemed to 'go too far' in diminishing economic value.
% ABSENT_VOICES: Future generations, who would benefit from robust environmental and land-use regulations but whose interests are often discounted in present-day economic value calculations, are structurally absent from the immediate legal calculus.
% DISAPPEARANCE_RATIONALE: If the regulatory takings doctrine vanished, governments would have significantly more freedom to regulate land use and economic activity without fear of compensation claims. This would likely lead to more aggressive environmental protection, zoning, and public health regulations, fundamentally altering the balance between private property and public welfare.
% FOUNDING_PROBLEM: To prevent government from imposing burdens on private property that, while not a direct physical seizure, effectively destroy its value, forcing individuals to bear public costs alone.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars and property rights advocates attest that the problem of regulatory overreach remains live, citing ongoing cases where regulations severely impact property values. Public interest groups and some legal academics, however, argue that the doctrine itself creates a chilling effect on necessary regulation, suggesting the 'problem' is often a pretext for resisting public good.
narrative_ontology:disappearance_verdict(takings_clause_boundary__regulatory_takings_reading, world_rearranges).
narrative_ontology:founding_problem_status(takings_clause_boundary__regulatory_takings_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(takings_clause_boundary__regulatory_takings_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_gemini+stakeholder_backfill', 'agent/example_platform_commission.json',
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
 *   The extractiveness (0.65) is substantial because the doctrine shifts potential costs from private property owners to the public purse, and its application is often unpredictable, creating a 'regulatory chill.' Suppression (0.40) is moderate; while it constrains government action, it doesn't entirely suppress regulation, but rather channels it through a complex legal process. Theater ratio (0.20) is low, as the legal process is genuinely adversarial, though some arguments may be performative. The claimed type is 'tangled_rope' because it genuinely coordinates the balance between public and private interests, but with clear asymmetric extraction from public entities to private owners, requiring active judicial enforcement.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of property owners, the doctrine is a necessary 'rope' protecting fundamental rights. From the perspective of regulators and public interest advocates, it often functions as a 'snare' that extracts public resources and stifles essential public welfare initiatives. The engine's classification will reflect this divergence based on the declared structural relationships and metrics.
 *
 * DIRECTIONALITY LOGIC:
 *   Property owners and developers are beneficiaries, as the doctrine protects their economic interests and provides a pathway for compensation. Local governments, environmental regulators, and public interest advocates are payers, as they bear the costs of compensation or the chilling effect on public-interest regulations. The Supreme Court acts as the agenda-setter, defining and enforcing the boundaries of the doctrine.
 *
 * MANDATROPHY ANALYSIS:
 *   The doctrine's mandate to prevent unfair burdens on individuals remains live, but its application has arguably drifted to prioritize private economic interests over collective public goods, leading to a contest over whether its original coordination function is still primary or if it has become primarily extractive. The 'contested' status of the founding problem reflects this tension.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    penn_central_balancing_test_objectivity,
    'Is the Penn Central ad hoc balancing test applied objectively, or does it reflect judicial policy preferences?',
    'Empirical analysis of judicial decisions over time, correlating outcomes with the political leanings of judges or prevailing economic ideologies.',
    'If subjective, the doctrine''s extractiveness is more arbitrary and less predictable, potentially increasing its ''snare'' characteristics for regulators. If objective, it reinforces its ''tangled_rope'' function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(penn_central_balancing_test_objectivity, empirical, 'Objectivity of the Penn Central balancing test.').

omega_variable(
    chilling_effect_quantification,
    'To what extent does the threat of regulatory takings claims actually deter beneficial public-interest regulations?',
    'Comparative studies of regulatory activity in jurisdictions with and without strong regulatory takings doctrines, or surveys of local government officials and regulators.',
    'A strong chilling effect would increase the effective suppression of the constraint, pushing it closer to a ''snare'' for public welfare. A weak effect would suggest the doctrine primarily functions as a legitimate check on government power.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(chilling_effect_quantification, empirical, 'Quantification of the ''chilling effect'' on regulation.').

omega_variable(
    regulatory_takings_vs_physical_appropriation,
    'Is the conceptual distinction between regulatory takings and physical appropriations sufficiently clear, or does the ''goes too far'' standard blur the line in practice?',
    'Analysis of dissenting opinions and legal commentary on takings cases, focusing on arguments about the coherence of the distinction.',
    'If the line is consistently blurred, it suggests a conceptual instability in the kernel, potentially leading to inconsistent application and increased perceived extraction for regulators. If clear, it reinforces the distinct structural claims of each reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(regulatory_takings_vs_physical_appropriation, conceptual, 'Clarity of the distinction between regulatory and physical takings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(takings_clause_boundary__regulatory_takings_reading, 1978, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(taki_tr_t1978, takings_clause_boundary__regulatory_takings_reading, theater_ratio, 1978, 0.1).
narrative_ontology:measurement(taki_tr_t1990, takings_clause_boundary__regulatory_takings_reading, theater_ratio, 1990, 0.15).
narrative_ontology:measurement(taki_tr_t2000, takings_clause_boundary__regulatory_takings_reading, theater_ratio, 2000, 0.18).
narrative_ontology:measurement(taki_tr_t2010, takings_clause_boundary__regulatory_takings_reading, theater_ratio, 2010, 0.19).
narrative_ontology:measurement(taki_tr_t2024, takings_clause_boundary__regulatory_takings_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(taki_be_t1978, takings_clause_boundary__regulatory_takings_reading, base_extractiveness, 1978, 0.5).
narrative_ontology:measurement(taki_be_t1990, takings_clause_boundary__regulatory_takings_reading, base_extractiveness, 1990, 0.58).
narrative_ontology:measurement(taki_be_t2000, takings_clause_boundary__regulatory_takings_reading, base_extractiveness, 2000, 0.62).
narrative_ontology:measurement(taki_be_t2010, takings_clause_boundary__regulatory_takings_reading, base_extractiveness, 2010, 0.64).
narrative_ontology:measurement(taki_be_t2024, takings_clause_boundary__regulatory_takings_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(taki_su_t1978, takings_clause_boundary__regulatory_takings_reading, suppression_requirement, 1978, 0.3).
narrative_ontology:measurement(taki_su_t1990, takings_clause_boundary__regulatory_takings_reading, suppression_requirement, 1990, 0.35).
narrative_ontology:measurement(taki_su_t2000, takings_clause_boundary__regulatory_takings_reading, suppression_requirement, 2000, 0.38).
narrative_ontology:measurement(taki_su_t2010, takings_clause_boundary__regulatory_takings_reading, suppression_requirement, 2010, 0.39).
narrative_ontology:measurement(taki_su_t2024, takings_clause_boundary__regulatory_takings_reading, suppression_requirement, 2024, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(takings_clause_boundary__regulatory_takings_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(takings_clause_boundary__regulatory_takings_reading, environmental_protection_regulations).
narrative_ontology:affects_constraint(takings_clause_boundary__regulatory_takings_reading, zoning_laws).
narrative_ontology:affects_constraint(takings_clause_boundary__regulatory_takings_reading, historic_preservation_ordinances).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
