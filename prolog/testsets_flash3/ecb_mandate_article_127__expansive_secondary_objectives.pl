% ============================================================================
% CONSTRAINT STORY: ecb_mandate_article_127__expansive_secondary_objectives
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ecb_mandate_article_127__expansive_secondary_objectives, []).

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
 *   constraint_id: ecb_mandate_article_127__expansive_secondary_objectives
 *   human_readable: ECB Mandate Article 127: Expansive Secondary Objectives Reading
 *   domain: monetary_policy/constitutional_law/eu_institutional_governance
 *
 * SUMMARY:
 *   This constraint represents an 'expansive secondary objectives' reading of
 *   the ECB's Article 127 mandate, which permits the ECB to give operational
 *   weight to employment and growth, provided price stability is not
 *   threatened. The 'without prejudice' clause is interpreted as authorizing
 *   discretionary balancing. This reading expands the beneficiary set to
 *   include workers and debtors, and allows for distributional considerations
 *   in monetary policy. It is a contested interpretation, standing in
 *   contrast to more orthodox views.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ecb_mandate_article_127__expansive_secondary_objectives, 0.35).
domain_priors:suppression_score(ecb_mandate_article_127__expansive_secondary_objectives, 0.45).
domain_priors:theater_ratio(ecb_mandate_article_127__expansive_secondary_objectives, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ecb_mandate_article_127__expansive_secondary_objectives, extractiveness, 0.35).
narrative_ontology:constraint_metric(ecb_mandate_article_127__expansive_secondary_objectives, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(ecb_mandate_article_127__expansive_secondary_objectives, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ecb_mandate_article_127__expansive_secondary_objectives, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(ecb_mandate_article_127__expansive_secondary_objectives, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ecb_mandate_article_127__expansive_secondary_objectives, tangled_rope).
narrative_ontology:human_readable(ecb_mandate_article_127__expansive_secondary_objectives, "ECB Mandate Article 127: Expansive Secondary Objectives Reading").
narrative_ontology:topic_domain(ecb_mandate_article_127__expansive_secondary_objectives, "monetary_policy/constitutional_law/eu_institutional_governance").

domain_priors:requires_active_enforcement(ecb_mandate_article_127__expansive_secondary_objectives).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ecb_mandate_article_127__expansive_secondary_objectives, '10268c64-b2e5-43df-8e66-1e15aeb3650b').
narrative_ontology:cs_kernel_codification('10268c64-b2e5-43df-8e66-1e15aeb3650b', fixed_text).
narrative_ontology:cs_authority_grounding('10268c64-b2e5-43df-8e66-1e15aeb3650b', lineage).
narrative_ontology:cs_interpretation_layer_present('10268c64-b2e5-43df-8e66-1e15aeb3650b').
narrative_ontology:cs_reading_relation('10268c64-b2e5-43df-8e66-1e15aeb3650b', ecb_mandate_article_127__orthodox_price_stability, coexists_with).
narrative_ontology:cs_reading_relation('10268c64-b2e5-43df-8e66-1e15aeb3650b', ecb_mandate_article_127__climate_incorporation, coexists_with).
narrative_ontology:cs_axiom('10268c64-b2e5-43df-8e66-1e15aeb3650b', foundational, monetary_policy_supports_general_eu_policies).
narrative_ontology:cs_axiom_status(monetary_policy_supports_general_eu_policies, holdable).
narrative_ontology:cs_axiom_grounding('10268c64-b2e5-43df-8e66-1e15aeb3650b', monetary_policy_supports_general_eu_policies, conventional).
narrative_ontology:cs_axiom('10268c64-b2e5-43df-8e66-1e15aeb3650b', foundational, without_prejudice_clause_grants_discretion).
narrative_ontology:cs_axiom_status(without_prejudice_clause_grants_discretion, holdable).
narrative_ontology:cs_axiom_grounding('10268c64-b2e5-43df-8e66-1e15aeb3650b', without_prejudice_clause_grants_discretion, conventional).
narrative_ontology:cs_reference_frame('10268c64-b2e5-43df-8e66-1e15aeb3650b', balanced_economic_stewardship).
narrative_ontology:cs_drift_state('10268c64-b2e5-43df-8e66-1e15aeb3650b', contemporary_eu_economic_challenges, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('10268c64-b2e5-43df-8e66-1e15aeb3650b', '').
narrative_ontology:cs_kernel_id(ecb_mandate_article_127__expansive_secondary_objectives, ecb_mandate_article_127).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ecb_mandate_article_127__expansive_secondary_objectives, eu_member_states).
narrative_ontology:constraint_beneficiary(ecb_mandate_article_127__expansive_secondary_objectives, eu_citizens_workers_debtors).
narrative_ontology:constraint_victim(ecb_mandate_article_127__expansive_secondary_objectives, orthodox_economists).
narrative_ontology:constraint_victim(ecb_mandate_article_127__expansive_secondary_objectives, financial_market_speculators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets and implements the ECB mandate, balancing price stability with secondary objectives like employment and growth, especially when inflation is not threatened. This reading grants them discretion to consider broader societal impacts.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__expansive_secondary_objectives, ecb_governing_council, agenda_setter,
    institutional, generational, constrained, continental).

% Benefit from monetary policy that supports economic growth and employment, particularly during downturns, without being solely focused on inflation. This provides fiscal space and reduces social friction.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__expansive_secondary_objectives, eu_member_states, beneficiary,
    institutional, generational, constrained, continental).

% Benefit from policies that prioritize employment and stable economic conditions, potentially leading to lower unemployment and more favorable borrowing terms. They are less directly impacted by strict inflation targeting.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__expansive_secondary_objectives, eu_citizens_workers_debtors, beneficiary,
    organized, biographical, constrained, continental).

% Bear the cost of a less predictable monetary policy framework, which they argue could lead to inflation overshoots or moral hazard. Their intellectual framework is challenged by this expansive interpretation.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__expansive_secondary_objectives, orthodox_economists, payer,
    powerful, biographical, constrained, global).

% May face increased uncertainty or reduced arbitrage opportunities if the ECB's policy becomes less predictable due to broader objectives. They prefer clear, singular mandates for monetary policy.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__expansive_secondary_objectives, financial_market_speculators, payer,
    powerful, immediate, mobile, global).

% Provides legal counsel on the interpretation of the mandate, navigating the 'without prejudice' clause and its implications for policy decisions. Their analysis supports the legal defensibility of this expansive reading.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__expansive_secondary_objectives, ecb_legal_department, observer,
    institutional, generational, analytical, continental).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates monetary policy to support the general economic policies of the EU, including employment and growth, while maintaining price stability. This allows for a more holistic response to economic challenges across member states.
% TRANSFER_FUNCTION: Transfers policy discretion from a narrow price stability focus to a broader set of economic objectives, potentially shifting benefits towards employment and growth at the expense of strict inflation adherence.
% ABSENT_VOICES: Advocates for a purely rules-based, inflation-targeting central bank, who argue that discretionary balancing introduces political interference and reduces central bank independence, are often marginalized in this discourse.
% DISAPPEARANCE_RATIONALE: If this expansive reading vanished, the ECB would likely revert to a stricter, more orthodox interpretation of its mandate, potentially leading to less support for employment and growth, and a more rigid response to economic crises. This would force EU member states to rely more heavily on fiscal policy, fundamentally altering the EU's economic governance.
% FOUNDING_PROBLEM: The original ECB mandate aimed to ensure price stability while acknowledging the need to support general economic policies, reflecting a compromise between different economic philosophies within the EU.
% FOUNDING_PROBLEM_CORROBORATION: EU treaties and founding documents corroborate the dual nature of the mandate. Legal scholars and political scientists, independent of the ECB, attest that the tension between price stability and secondary objectives remains a live issue, requiring ongoing interpretation.
narrative_ontology:disappearance_verdict(ecb_mandate_article_127__expansive_secondary_objectives, world_rearranges).
narrative_ontology:founding_problem_status(ecb_mandate_article_127__expansive_secondary_objectives, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ecb_mandate_article_127__expansive_secondary_objectives, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(ecb_mandate_article_127__expansive_secondary_objectives, 'none', 1).
narrative_ontology:epsilon_provenance(ecb_mandate_article_127__expansive_secondary_objectives, 0.35, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ecb_mandate_article_127__expansive_secondary_objectives_tests).
:- end_tests(ecb_mandate_article_127__expansive_secondary_objectives_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.35) is moderate, reflecting the costs borne by those who prefer a singular focus on price stability (e.g., orthodox economists, some financial market actors) due to increased policy discretion and potential for 'mission creep'. Suppression (0.45) is also moderate, as this reading actively pushes back against purely orthodox interpretations and requires enforcement of a broader policy scope. Theater ratio (0.15) is low, as the ECB genuinely attempts to balance these objectives, though the degree of 'balancing' is often debated. The metrics reflect the ongoing contestation and the active effort to maintain this interpretation.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of beneficiaries (EU member states, workers/debtors), this reading is a necessary adaptation to complex economic realities, allowing for a more humane and effective monetary policy. From the perspective of victims (orthodox economists, speculators), it represents an overreach that compromises the ECB's independence and risks long-term stability. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The ECB Governing Council, as the agenda-setter, benefits from increased policy flexibility. EU member states and citizens (workers/debtors) are beneficiaries, as their economic well-being is explicitly considered. Orthodox economists and financial market speculators are victims, as their preferred policy framework is challenged. The legal department acts as an observer, providing the interpretive framework.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading actively prevents mandatrophy by re-interpreting the mandate to remain relevant to contemporary economic challenges beyond mere price stability. It avoids the piton trap by ensuring the mandate's function remains live and responsive, rather than becoming a theatrical performance of an outdated goal. The contestation itself is a sign of a live, rather than atrophied, mandate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    discretionary_balancing_scope,
    'What are the precise operational limits of the ''discretionary balancing'' permitted by the ''without prejudice'' clause, and how are they determined?',
    'Further legal rulings from the European Court of Justice or explicit legislative clarification from the European Parliament and Council, defining the boundaries of secondary objective pursuit.',
    'A narrower definition would push the constraint closer to the ''orthodox price stability'' reading, increasing extractiveness for beneficiaries of this reading. A broader definition would solidify this expansive reading, potentially increasing resistance from orthodox camps.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(discretionary_balancing_scope, conceptual, 'Ambiguity regarding the extent of ECB''s discretion in pursuing secondary objectives.').

omega_variable(
    secondary_objective_effectiveness,
    'To what extent do ECB policies, when pursuing secondary objectives under this reading, genuinely contribute to employment and growth outcomes, and what are the unintended side effects?',
    'Longitudinal empirical studies by independent economic research institutions, evaluating the causal impact of specific ECB policy decisions on employment and growth metrics, controlling for other factors.',
    'Strong evidence of positive impact would strengthen the legitimacy of this reading, potentially reducing resistance. Evidence of limited impact or significant negative externalities would weaken its justification, pushing towards a more orthodox interpretation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(secondary_objective_effectiveness, empirical, 'Empirical effectiveness of ECB''s secondary objective pursuit.').

omega_variable(
    mandate_political_legitimacy,
    'Does this expansive reading of the mandate enhance or detract from the ECB''s political legitimacy among EU citizens and institutions, given its unelected status?',
    'Public opinion surveys across EU member states, analysis of parliamentary debates, and expert legal commentary on the democratic accountability implications of central bank mandate interpretation.',
    'If legitimacy is enhanced, the constraint''s stability is improved. If legitimacy is eroded, it could lead to political pressure for mandate reform or increased resistance to ECB policies, potentially pushing the constraint towards a more ''contested'' status.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandate_political_legitimacy, preference, 'Political legitimacy of an expansive ECB mandate interpretation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ecb_mandate_article_127__expansive_secondary_objectives, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ecb__tr_t0, ecb_mandate_article_127__expansive_secondary_objectives, theater_ratio, 0, 0.1).
narrative_ontology:measurement(ecb__tr_t5, ecb_mandate_article_127__expansive_secondary_objectives, theater_ratio, 5, 0.12).
narrative_ontology:measurement(ecb__tr_t10, ecb_mandate_article_127__expansive_secondary_objectives, theater_ratio, 10, 0.14).
narrative_ontology:measurement(ecb__tr_t15, ecb_mandate_article_127__expansive_secondary_objectives, theater_ratio, 15, 0.15).
narrative_ontology:measurement(ecb__tr_t20, ecb_mandate_article_127__expansive_secondary_objectives, theater_ratio, 20, 0.15).

% Extraction over time
narrative_ontology:measurement(ecb__be_t0, ecb_mandate_article_127__expansive_secondary_objectives, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(ecb__be_t5, ecb_mandate_article_127__expansive_secondary_objectives, base_extractiveness, 5, 0.3).
narrative_ontology:measurement(ecb__be_t10, ecb_mandate_article_127__expansive_secondary_objectives, base_extractiveness, 10, 0.33).
narrative_ontology:measurement(ecb__be_t15, ecb_mandate_article_127__expansive_secondary_objectives, base_extractiveness, 15, 0.34).
narrative_ontology:measurement(ecb__be_t20, ecb_mandate_article_127__expansive_secondary_objectives, base_extractiveness, 20, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(ecb__su_t0, ecb_mandate_article_127__expansive_secondary_objectives, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(ecb__su_t5, ecb_mandate_article_127__expansive_secondary_objectives, suppression_requirement, 5, 0.4).
narrative_ontology:measurement(ecb__su_t10, ecb_mandate_article_127__expansive_secondary_objectives, suppression_requirement, 10, 0.43).
narrative_ontology:measurement(ecb__su_t15, ecb_mandate_article_127__expansive_secondary_objectives, suppression_requirement, 15, 0.44).
narrative_ontology:measurement(ecb__su_t20, ecb_mandate_article_127__expansive_secondary_objectives, suppression_requirement, 20, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ecb_mandate_article_127__expansive_secondary_objectives, enforcement_mechanism).
narrative_ontology:affects_constraint(ecb_mandate_article_127__expansive_secondary_objectives, ecb_mandate_article_127__orthodox_price_stability).
narrative_ontology:affects_constraint(ecb_mandate_article_127__expansive_secondary_objectives, ecb_mandate_article_127__climate_incorporation).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the ECB's Article 127 mandate, focusing on expansive secondary objectives. It is part of a family of interpretations that includes 'orthodox price stability' and 'climate incorporation', each representing a distinct structural claim about the mandate's operation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
