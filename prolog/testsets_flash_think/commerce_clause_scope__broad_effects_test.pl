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
 *   human_readable: Commerce Clause Broad Effects Test
 *   domain: constitutional_law/federalism
 *
 * SUMMARY:
 *   This constraint describes the 'broad effects test' reading of the
 *   Commerce Clause, which holds that federal power extends to any economic
 *   activity that substantially affects interstate commerce in the aggregate,
 *   and that 'regulate' includes prohibition and comprehensive control. This
 *   interpretation, solidified during the New Deal era, dramatically expanded
 *   federal power, allowing Congress to regulate a vast array of intrastate
 *   activities. It is one reading of the broader 'commerce_clause_scope'
 *   kernel.
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
narrative_ontology:constraint_metric(commerce_clause_scope__broad_effects_test, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(commerce_clause_scope__broad_effects_test, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(commerce_clause_scope__broad_effects_test, tangled_rope).
narrative_ontology:human_readable(commerce_clause_scope__broad_effects_test, "Commerce Clause Broad Effects Test").
narrative_ontology:topic_domain(commerce_clause_scope__broad_effects_test, "constitutional_law/federalism").

domain_priors:requires_active_enforcement(commerce_clause_scope__broad_effects_test).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(commerce_clause_scope__broad_effects_test, '033536af-3ae8-4cee-b88f-cf8fae8b4289').
narrative_ontology:cs_kernel_codification('033536af-3ae8-4cee-b88f-cf8fae8b4289', fixed_text).
narrative_ontology:cs_authority_grounding('033536af-3ae8-4cee-b88f-cf8fae8b4289', lineage).
narrative_ontology:cs_interpretation_layer_present('033536af-3ae8-4cee-b88f-cf8fae8b4289').
narrative_ontology:cs_reading_relation('033536af-3ae8-4cee-b88f-cf8fae8b4289', commerce_clause_scope__narrow_originalist, forecloses).
narrative_ontology:cs_reading_relation('033536af-3ae8-4cee-b88f-cf8fae8b4289', commerce_clause_scope__intermediate_channels, coexists_with).
narrative_ontology:cs_axiom('033536af-3ae8-4cee-b88f-cf8fae8b4289', foundational, economic_interdependence_requires_national_regulation).
narrative_ontology:cs_axiom_status(economic_interdependence_requires_national_regulation, holdable).
narrative_ontology:cs_axiom_grounding('033536af-3ae8-4cee-b88f-cf8fae8b4289', economic_interdependence_requires_national_regulation, empirically_contingent).
narrative_ontology:cs_axiom('033536af-3ae8-4cee-b88f-cf8fae8b4289', foundational, congressional_power_is_plenary_within_enumerated_spheres).
narrative_ontology:cs_axiom_status(congressional_power_is_plenary_within_enumerated_spheres, holdable).
narrative_ontology:cs_axiom_grounding('033536af-3ae8-4cee-b88f-cf8fae8b4289', congressional_power_is_plenary_within_enumerated_spheres, deontological).
narrative_ontology:cs_reference_frame('033536af-3ae8-4cee-b88f-cf8fae8b4289', new_deal_era_expansion).
narrative_ontology:cs_drift_state('033536af-3ae8-4cee-b88f-cf8fae8b4289', contemporary_judicial_review, gap(stable, minor, true)).
narrative_ontology:cs_created_at('033536af-3ae8-4cee-b88f-cf8fae8b4289', '').
narrative_ontology:cs_kernel_id(commerce_clause_scope__broad_effects_test, commerce_clause_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(commerce_clause_scope__broad_effects_test, federal_regulators).
narrative_ontology:constraint_beneficiary(commerce_clause_scope__broad_effects_test, national_interest_groups).
narrative_ontology:constraint_beneficiary(commerce_clause_scope__broad_effects_test, civil_rights_enforcement).
narrative_ontology:constraint_victim(commerce_clause_scope__broad_effects_test, state_governments).
narrative_ontology:constraint_victim(commerce_clause_scope__broad_effects_test, local_economic_actors).
narrative_ontology:constraint_victim(commerce_clause_scope__broad_effects_test, federalism_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Agencies and departments that implement federal laws justified by the Commerce Clause. They benefit from broad regulatory authority, enabling uniform national policies and addressing issues that cross state lines. Their power is directly expanded by this interpretation.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__broad_effects_test, federal_regulators, agenda_setter,
    institutional, generational, arbitrage, national).

% Advocacy groups seeking national solutions to social, economic, or environmental problems. They benefit from the federal government's ability to enact comprehensive legislation that might otherwise be blocked by state-level resistance or fragmentation.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__broad_effects_test, national_interest_groups, beneficiary,
    organized, biographical, mobile, national).

% Organizations and government bodies focused on civil rights. This interpretation provided a crucial basis for federal civil rights legislation, allowing the regulation of private discrimination based on its aggregate effect on interstate commerce.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__broad_effects_test, civil_rights_enforcement, beneficiary,
    organized, biographical, mobile, national).

% State legislatures and executive branches that find their traditional police powers (health, safety, welfare) increasingly preempted or constrained by federal legislation justified under the broad Commerce Clause. They bear the cost of reduced autonomy and policy experimentation.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__broad_effects_test, state_governments, payer,
    institutional, generational, constrained, national).

% Small businesses and local industries that may face federal regulations designed for national markets, which can be disproportionately burdensome or ill-suited to local conditions. Their ability to operate under purely local rules is suppressed.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__broad_effects_test, local_economic_actors, payer,
    moderate, biographical, constrained, local).

% Legal scholars, political groups, and citizens who argue for a more limited federal government and greater state sovereignty. They bear the cost of seeing their preferred constitutional structure eroded by expansive federal power.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__broad_effects_test, federalism_advocates, payer,
    organized, generational, constrained, national).

% The ultimate arbiter of the Commerce Clause's meaning. While it has occasionally reined in the broadest interpretations, its jurisprudence largely established and maintains the broad effects test, setting the boundaries of federal power.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__broad_effects_test, supreme_court, agenda_setter,
    institutional, civilizational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables the federal government to address national economic problems, ensure uniform commercial standards, and prevent states from erecting barriers to interstate trade, thereby fostering a single national market.
% TRANSFER_FUNCTION: Transfers regulatory authority and policy-making power from state and local governments to the federal government, allowing national interests to supersede local preferences in economic matters.
% ABSENT_VOICES: Local communities seeking to implement unique economic development strategies or social policies that might be deemed to 'substantially affect interstate commerce' and thus fall under federal purview. Their ability to experiment is suppressed.
% DISAPPEARANCE_RATIONALE: If this interpretation vanished, much of modern federal legislation (e.g., environmental protection, labor laws, civil rights) would lose its constitutional basis. States would regain significant regulatory authority, leading to a fragmented and potentially chaotic national economy, and a dramatic shift in the balance of power.
% FOUNDING_PROBLEM: The economic balkanization and interstate trade disputes under the Articles of Confederation, where states imposed tariffs and barriers, hindering national economic development and stability.
% FOUNDING_PROBLEM_CORROBORATION: Historians and federalists cite the economic chaos of the post-Revolutionary War period as evidence of the original problem. Contemporary federal agencies and national interest groups argue that modern economic interdependence still requires broad federal power. States' rights advocates and some legal scholars argue the original problem is long solved, and the current interpretation constitutes federal overreach; their arguments are supported by independent legal analysis and historical critiques of federal expansion.
narrative_ontology:disappearance_verdict(commerce_clause_scope__broad_effects_test, world_rearranges).
narrative_ontology:founding_problem_status(commerce_clause_scope__broad_effects_test, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(commerce_clause_scope__broad_effects_test, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
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
 *   Extractiveness is high (0.78) because this interpretation allows the federal government to claim regulatory authority over nearly all economic activity, significantly reducing state autonomy. Suppression is very high (0.85) as it actively suppresses state-level policy divergence and local economic experimentation, backed by federal judicial and enforcement power. Theater ratio is low (0.1) because the power is actively and functionally exercised, not merely maintained for show; federal agencies and courts consistently apply this broad interpretation.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of federal beneficiaries, this interpretation is a necessary coordination mechanism for a modern, integrated economy, solving collective action problems and ensuring national welfare. From the perspective of state and local payers, it represents an extractive overreach that undermines federalism and local self-governance. The engine's classification will reflect this structural asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   Federal regulators, national interest groups, and civil rights enforcement are clear beneficiaries, as this interpretation grants them the authority and means to pursue national policy goals. State governments, local economic actors, and federalism advocates are the primary targets, bearing the costs of diminished sovereignty, increased regulatory burden, and the erosion of a decentralized constitutional structure. The Supreme Court, as the ultimate arbiter, acts as an agenda-setter, defining and maintaining the scope of this power.
 *
 * MANDATROPHY ANALYSIS:
 *   The broad effects test prevents mislabeling essential national coordination (e.g., environmental protection, financial regulation) as pure extraction. However, its expansive nature means that the coordination function can become a cover for federal power accumulation, extracting regulatory authority from states even when local solutions might be more appropriate. The 'contested' status of the founding problem highlights this tension: is it still solving the original problem, or has it become an engine of centralized power beyond its original mandate?
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    substantial_effect_threshold,
    'What constitutes a ''substantial effect'' on interstate commerce, and is this threshold consistently applied or subject to judicial discretion?',
    'Empirical analysis of Supreme Court and lower court rulings over time, identifying patterns in what activities are deemed ''substantial'' versus ''insubstantial'' and whether these patterns correlate with political or ideological shifts.',
    'If the threshold is inconsistent or politically driven, the constraint''s effective extractiveness and suppression are higher, as its application becomes unpredictable and potentially arbitrary, further eroding state autonomy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(substantial_effect_threshold, empirical, 'Ambiguity in defining ''substantial effect'' on interstate commerce.').

omega_variable(
    federalism_balance_preference,
    'What is the optimal balance between national uniformity and state-level policy experimentation in economic regulation?',
    'This is a preference-based question, resolvable only through political deliberation, legislative action, or constitutional amendment reflecting societal values regarding federalism.',
    'A preference for greater state autonomy would reclassify aspects of this constraint as more extractive; a preference for national uniformity would emphasize its coordination function.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(federalism_balance_preference, preference, 'The normative preference for federal vs. state regulatory authority.').

omega_variable(
    economic_vs_non_economic_activity,
    'How clearly can ''economic activity'' be distinguished from ''non-economic activity'' for the purpose of aggregation, and does this distinction provide a meaningful limit on federal power?',
    'Further judicial clarification or legislative definitions that provide clear, consistent criteria for distinguishing economic from non-economic activity, and empirical observation of whether these distinctions effectively constrain federal regulatory scope.',
    'If the distinction is blurred or easily circumvented, the constraint''s scope is effectively universal, increasing extractiveness and suppression by removing a theoretical limit on federal power.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(economic_vs_non_economic_activity, conceptual, 'The conceptual boundary between economic and non-economic activity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(commerce_clause_scope__broad_effects_test, 1937, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comm_tr_t1937, commerce_clause_scope__broad_effects_test, theater_ratio, 1937, 0.05).
narrative_ontology:measurement(comm_tr_t1950, commerce_clause_scope__broad_effects_test, theater_ratio, 1950, 0.08).
narrative_ontology:measurement(comm_tr_t1970, commerce_clause_scope__broad_effects_test, theater_ratio, 1970, 0.1).
narrative_ontology:measurement(comm_tr_t1990, commerce_clause_scope__broad_effects_test, theater_ratio, 1990, 0.1).
narrative_ontology:measurement(comm_tr_t2010, commerce_clause_scope__broad_effects_test, theater_ratio, 2010, 0.1).
narrative_ontology:measurement(comm_tr_t2023, commerce_clause_scope__broad_effects_test, theater_ratio, 2023, 0.1).

% Extraction over time
narrative_ontology:measurement(comm_be_t1937, commerce_clause_scope__broad_effects_test, base_extractiveness, 1937, 0.65).
narrative_ontology:measurement(comm_be_t1950, commerce_clause_scope__broad_effects_test, base_extractiveness, 1950, 0.7).
narrative_ontology:measurement(comm_be_t1970, commerce_clause_scope__broad_effects_test, base_extractiveness, 1970, 0.75).
narrative_ontology:measurement(comm_be_t1990, commerce_clause_scope__broad_effects_test, base_extractiveness, 1990, 0.78).
narrative_ontology:measurement(comm_be_t2010, commerce_clause_scope__broad_effects_test, base_extractiveness, 2010, 0.78).
narrative_ontology:measurement(comm_be_t2023, commerce_clause_scope__broad_effects_test, base_extractiveness, 2023, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(comm_su_t1937, commerce_clause_scope__broad_effects_test, suppression_requirement, 1937, 0.7).
narrative_ontology:measurement(comm_su_t1950, commerce_clause_scope__broad_effects_test, suppression_requirement, 1950, 0.78).
narrative_ontology:measurement(comm_su_t1970, commerce_clause_scope__broad_effects_test, suppression_requirement, 1970, 0.83).
narrative_ontology:measurement(comm_su_t1990, commerce_clause_scope__broad_effects_test, suppression_requirement, 1990, 0.85).
narrative_ontology:measurement(comm_su_t2010, commerce_clause_scope__broad_effects_test, suppression_requirement, 2010, 0.85).
narrative_ontology:measurement(comm_su_t2023, commerce_clause_scope__broad_effects_test, suppression_requirement, 2023, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(commerce_clause_scope__broad_effects_test, enforcement_mechanism).
narrative_ontology:affects_constraint(commerce_clause_scope__broad_effects_test, spending_clause_conditions).
narrative_ontology:affects_constraint(commerce_clause_scope__broad_effects_test, supremacy_clause_preemption).
narrative_ontology:affects_constraint(commerce_clause_scope__broad_effects_test, federal_environmental_regulation).
narrative_ontology:affects_constraint(commerce_clause_scope__broad_effects_test, federal_labor_laws).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'commerce_clause_scope' kernel, alongside 'narrow_originalist' and 'intermediate_channels' readings. Each reading presents a distinct structural claim about federal power.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
