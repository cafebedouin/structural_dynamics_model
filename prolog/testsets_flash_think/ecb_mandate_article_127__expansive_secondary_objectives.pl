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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   human_readable: ECB Mandate: Expansive Secondary Objectives Reading
 *   domain: monetary_policy/constitutional_law/eu_governance
 *
 * SUMMARY:
 *   This constraint represents the 'expansive secondary objectives' reading
 *   of Article 127 of the Treaty on the Functioning of the European Union
 *   (TFEU), which governs the ECB's mandate. This reading emphasizes that
 *   while price stability is the primary objective, the ECB is permitted to
 *   give operational weight to supporting the general economic policies of
 *   the Union, including employment and sustainable growth, provided price
 *   stability is not threatened. The 'without prejudice' clause is
 *   interpreted as authorizing discretionary balancing. This reading has
 *   gained prominence, particularly during periods of economic crisis,
 *   leading to policies that aim to achieve broader economic stabilization.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ecb_mandate_article_127__expansive_secondary_objectives, 0.72).
domain_priors:suppression_score(ecb_mandate_article_127__expansive_secondary_objectives, 0.77).
domain_priors:theater_ratio(ecb_mandate_article_127__expansive_secondary_objectives, 0.27).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ecb_mandate_article_127__expansive_secondary_objectives, extractiveness, 0.72).
narrative_ontology:constraint_metric(ecb_mandate_article_127__expansive_secondary_objectives, suppression_requirement, 0.77).
narrative_ontology:constraint_metric(ecb_mandate_article_127__expansive_secondary_objectives, theater_ratio, 0.27).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ecb_mandate_article_127__expansive_secondary_objectives, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(ecb_mandate_article_127__expansive_secondary_objectives, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ecb_mandate_article_127__expansive_secondary_objectives, tangled_rope).
narrative_ontology:human_readable(ecb_mandate_article_127__expansive_secondary_objectives, "ECB Mandate: Expansive Secondary Objectives Reading").
narrative_ontology:topic_domain(ecb_mandate_article_127__expansive_secondary_objectives, "monetary_policy/constitutional_law/eu_governance").

domain_priors:requires_active_enforcement(ecb_mandate_article_127__expansive_secondary_objectives).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ecb_mandate_article_127__expansive_secondary_objectives, 'aa1e9d38-83f2-4270-af1a-0cb594f20054').
narrative_ontology:cs_kernel_codification('aa1e9d38-83f2-4270-af1a-0cb594f20054', fixed_text).
narrative_ontology:cs_authority_grounding('aa1e9d38-83f2-4270-af1a-0cb594f20054', lineage).
narrative_ontology:cs_interpretation_layer_present('aa1e9d38-83f2-4270-af1a-0cb594f20054').
narrative_ontology:cs_reading_relation('aa1e9d38-83f2-4270-af1a-0cb594f20054', ecb_mandate_article_127__orthodox_price_stability, coexists_with).
narrative_ontology:cs_reading_relation('aa1e9d38-83f2-4270-af1a-0cb594f20054', ecb_mandate_article_127__climate_incorporation, influences).
narrative_ontology:cs_axiom('aa1e9d38-83f2-4270-af1a-0cb594f20054', foundational, holistic_economic_stabilization).
narrative_ontology:cs_axiom_status(holistic_economic_stabilization, holdable).
narrative_ontology:cs_axiom_grounding('aa1e9d38-83f2-4270-af1a-0cb594f20054', holistic_economic_stabilization, instrumental).
narrative_ontology:cs_axiom('aa1e9d38-83f2-4270-af1a-0cb594f20054', foundational, discretionary_balancing_authority).
narrative_ontology:cs_axiom_status(discretionary_balancing_authority, holdable).
narrative_ontology:cs_axiom_grounding('aa1e9d38-83f2-4270-af1a-0cb594f20054', discretionary_balancing_authority, conventional).
narrative_ontology:cs_reference_frame('aa1e9d38-83f2-4270-af1a-0cb594f20054', post_lisbon_treaty_framework).
narrative_ontology:cs_drift_state('aa1e9d38-83f2-4270-af1a-0cb594f20054', contemporary_eu_crises, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('aa1e9d38-83f2-4270-af1a-0cb594f20054', '').
narrative_ontology:cs_kernel_id(ecb_mandate_article_127__expansive_secondary_objectives, ecb_mandate_article_127).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ecb_mandate_article_127__expansive_secondary_objectives, eu_member_states).
narrative_ontology:constraint_beneficiary(ecb_mandate_article_127__expansive_secondary_objectives, workers).
narrative_ontology:constraint_beneficiary(ecb_mandate_article_127__expansive_secondary_objectives, debtors).
narrative_ontology:constraint_victim(ecb_mandate_article_127__expansive_secondary_objectives, savers).
narrative_ontology:constraint_victim(ecb_mandate_article_127__expansive_secondary_objectives, fiscally_conservative_member_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(ecb_mandate_article_127__expansive_secondary_objectives, eu_member_states).
narrative_ontology:constraint_vindicates(ecb_mandate_article_127__expansive_secondary_objectives, economic_stabilization_doctrine).
narrative_ontology:constraint_vindicates(ecb_mandate_article_127__expansive_secondary_objectives, social_market_economy_principles).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets and implements the ECB's mandate, exercising discretion under the 'without prejudice' clause to balance price stability with support for broader EU economic policies. Benefits from the flexibility this interpretation provides in crisis management.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__expansive_secondary_objectives, ecb_governing_council, agenda_setter,
    institutional, generational, constrained, global).

% Benefit from ECB policies that support employment and economic growth, especially during downturns. However, some may bear costs through potential inflation or perceived fiscal burden, depending on their national economic priorities.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__expansive_secondary_objectives, eu_member_states, beneficiary,
    institutional, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(ecb_mandate_article_127__expansive_secondary_objectives, eu_member_states, payer).

% Benefit from monetary policies that prioritize employment and economic growth, leading to job creation and better labor market conditions.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__expansive_secondary_objectives, workers, beneficiary,
    powerless, biographical, constrained, national).

% Benefit from policies that may keep interest rates lower or allow for moderate inflation, easing the real burden of debt repayment.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__expansive_secondary_objectives, debtors, beneficiary,
    powerless, biographical, constrained, national).

% Bear the cost of policies that might lead to lower real returns on savings or higher inflation, eroding their purchasing power.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__expansive_secondary_objectives, savers, payer,
    powerless, biographical, constrained, national).

% May perceive expansive monetary policies as undermining fiscal discipline, creating moral hazard, or leading to undesirable long-term economic imbalances, bearing political and economic costs from these outcomes.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__expansive_secondary_objectives, fiscally_conservative_member_states, payer,
    institutional, generational, constrained, continental).

% Constantly react to ECB policy signals and decisions, influencing bond yields, currency values, and investment flows across the Eurozone. Their interpretation of the mandate's application affects market stability.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__expansive_secondary_objectives, financial_markets, observer,
    powerful, immediate, mobile, global).

% Provides democratic oversight and scrutinizes ECB decisions, engaging in debates about the interpretation and application of the mandate, but possesses limited direct power to alter monetary policy.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__expansive_secondary_objectives, eu_parliament, observer,
    institutional, biographical, analytical, continental).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To provide a stable economic environment for the Eurozone by balancing price stability with support for general economic policies of the Union, including employment and sustainable growth, within the 'without prejudice' clause of Article 127 TFEU.
% TRANSFER_FUNCTION: Transfers economic stability benefits (employment, growth) to member states, workers, and debtors, potentially at the cost of real returns for savers or increased fiscal pressure for fiscally conservative states, through discretionary monetary policy decisions.
% ABSENT_VOICES: Advocates for a strict, singular focus on price stability, who argue that secondary objectives dilute the mandate and create moral hazard, are often marginalized in policy debates when the expansive interpretation is dominant.
% DISAPPEARANCE_RATIONALE: If the ECB's mandate to balance objectives vanished overnight, Eurozone monetary policy would either default to an exclusive price stability focus (with severe economic consequences for employment/growth) or fragment into national policies, leading to economic chaos and potential dissolution of the Eurozone. The current balancing act, however contested, holds the system together.
% FOUNDING_PROBLEM: The need for a unified monetary policy for the Eurozone that could ensure price stability while also contributing to broader economic goals, avoiding the pitfalls of national monetary policies and ensuring the stability of the single currency.
% FOUNDING_PROBLEM_CORROBORATION: The ECB itself, many EU member states, and a significant portion of economic academia attest that the need for a balanced approach to monetary policy, especially during crises, remains live. Critics (e.g., some fiscally conservative economists and politicians) argue the founding problem has shifted, and the expansive interpretation now serves to justify overreach.
narrative_ontology:disappearance_verdict(ecb_mandate_article_127__expansive_secondary_objectives, world_rearranges).
narrative_ontology:founding_problem_status(ecb_mandate_article_127__expansive_secondary_objectives, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ecb_mandate_article_127__expansive_secondary_objectives, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(ecb_mandate_article_127__expansive_secondary_objectives, 'none', 1).
narrative_ontology:epsilon_provenance(ecb_mandate_article_127__expansive_secondary_objectives, 0.72, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ecb_mandate_article_127__expansive_secondary_objectives_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ecb_mandate_article_127__expansive_secondary_objectives, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ecb_mandate_article_127__expansive_secondary_objectives_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.72) is high because the discretionary balancing inherent in this reading allows the ECB to pursue policies that may benefit some groups (e.g., debtors, workers) at the expense of others (e.g., savers, fiscally conservative states). Suppression (0.77) is also high, as the ECB's institutional authority and the broad interpretation of its mandate effectively suppress alternative policy approaches or challenges from those who prefer a stricter focus on price stability. The theater ratio (0.27) is moderate, reflecting that while the ECB's actions are genuinely aimed at economic stability, there is also a performative aspect in justifying the balancing act and managing expectations across diverse member states. The increasing trend in extractiveness and suppression over the interval reflects the growing assertion of this expansive interpretation, particularly in response to successive crises.
 *
 * PERSPECTIVAL GAP:
 *   The ECB Governing Council, operating from this expansive reading, perceives its actions as necessary and legitimate coordination for overall Eurozone stability. However, from the perspective of savers or fiscally conservative member states, the same policies may be experienced as extractive, imposing costs without their direct consent or in contradiction to their preferred economic principles. The engine's per-seat classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The ECB Governing Council, EU member states (collectively), workers, and debtors are structural beneficiaries, as policies under this reading aim to support employment, growth, and ease debt burdens. Savers and fiscally conservative member states are victims, bearing the costs of potentially lower real returns or perceived policy overreach. The 'without prejudice' clause grants the ECB significant discretion, amplifying its beneficiary position and the target position of those who bear the costs.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification as a Tangled Rope prevents mislabeling the constraint as a pure Rope (which would ignore the asymmetric extraction from savers and fiscally conservative states) or a pure Snare (which would ignore the genuine coordination function of supporting employment and growth for the broader EU economy). It highlights that the constraint serves a real coordination purpose but does so through a structure that enables significant, actively enforced, and asymmetric extraction, particularly when the discretionary balancing leans heavily towards certain objectives.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ambiguity_without_prejudice,
    'How broadly can the ''without prejudice'' clause be interpreted to allow for discretionary balancing of objectives without undermining the primary price stability mandate?',
    'Legal rulings from the European Court of Justice or a formal re-negotiation of the EU treaties clarifying the hierarchy and scope of objectives.',
    'A narrow interpretation would shift the constraint towards the ''orthodox_price_stability'' reading, reducing extractiveness and suppression related to secondary objectives. A broad interpretation would solidify the expansive reading, potentially increasing extractiveness for some groups.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(ambiguity_without_prejudice, conceptual, 'Whether the ''without prejudice'' clause permits wide or narrow discretion.').

omega_variable(
    measurement_of_threat,
    'What constitutes ''price stability not threatened'' in practice, and how is this threshold objectively measured and communicated by the ECB?',
    'Development of clear, publicly agreed-upon quantitative indicators and a transparent decision-making framework for assessing threats to price stability, independent of political pressure.',
    'Lack of clarity allows for discretionary policy choices that can be perceived as extractive. Clearer metrics would reduce the scope for discretionary extraction and increase accountability, potentially lowering extractiveness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(measurement_of_threat, empirical, 'Clarity and objectivity of the ''price stability not threatened'' condition.').

omega_variable(
    distributional_impact_assessment,
    'Are the distributional consequences (e.g., for savers vs. debtors) of policies enacted under the expansive mandate systematically assessed and publicly reported?',
    'Mandatory, independent ex-ante and ex-post distributional impact assessments for all major monetary policy decisions, with findings integrated into policy debates.',
    'If negative distributional impacts are consistently found and ignored, it strengthens the argument for the constraint operating as a snare for certain groups. If impacts are balanced or mitigated, it supports the tangled_rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(distributional_impact_assessment, empirical, 'Systematic assessment of distributional impacts of expansive policies.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ecb_mandate_article_127__expansive_secondary_objectives, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ecb__tr_t0, ecb_mandate_article_127__expansive_secondary_objectives, theater_ratio, 0, 0.15).
narrative_ontology:measurement(ecb__tr_t6, ecb_mandate_article_127__expansive_secondary_objectives, theater_ratio, 6, 0.18).
narrative_ontology:measurement(ecb__tr_t12, ecb_mandate_article_127__expansive_secondary_objectives, theater_ratio, 12, 0.21).
narrative_ontology:measurement(ecb__tr_t18, ecb_mandate_article_127__expansive_secondary_objectives, theater_ratio, 18, 0.23).
narrative_ontology:measurement(ecb__tr_t24, ecb_mandate_article_127__expansive_secondary_objectives, theater_ratio, 24, 0.25).
narrative_ontology:measurement(ecb__tr_t30, ecb_mandate_article_127__expansive_secondary_objectives, theater_ratio, 30, 0.27).

% Extraction over time
narrative_ontology:measurement(ecb__be_t0, ecb_mandate_article_127__expansive_secondary_objectives, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(ecb__be_t6, ecb_mandate_article_127__expansive_secondary_objectives, base_extractiveness, 6, 0.6).
narrative_ontology:measurement(ecb__be_t12, ecb_mandate_article_127__expansive_secondary_objectives, base_extractiveness, 12, 0.65).
narrative_ontology:measurement(ecb__be_t18, ecb_mandate_article_127__expansive_secondary_objectives, base_extractiveness, 18, 0.68).
narrative_ontology:measurement(ecb__be_t24, ecb_mandate_article_127__expansive_secondary_objectives, base_extractiveness, 24, 0.7).
narrative_ontology:measurement(ecb__be_t30, ecb_mandate_article_127__expansive_secondary_objectives, base_extractiveness, 30, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(ecb__su_t0, ecb_mandate_article_127__expansive_secondary_objectives, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(ecb__su_t6, ecb_mandate_article_127__expansive_secondary_objectives, suppression_requirement, 6, 0.65).
narrative_ontology:measurement(ecb__su_t12, ecb_mandate_article_127__expansive_secondary_objectives, suppression_requirement, 12, 0.7).
narrative_ontology:measurement(ecb__su_t18, ecb_mandate_article_127__expansive_secondary_objectives, suppression_requirement, 18, 0.73).
narrative_ontology:measurement(ecb__su_t24, ecb_mandate_article_127__expansive_secondary_objectives, suppression_requirement, 24, 0.75).
narrative_ontology:measurement(ecb__su_t30, ecb_mandate_article_127__expansive_secondary_objectives, suppression_requirement, 30, 0.77).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ecb_mandate_article_127__expansive_secondary_objectives, enforcement_mechanism).
narrative_ontology:affects_constraint(ecb_mandate_article_127__expansive_secondary_objectives, eu_fiscal_rules).
narrative_ontology:affects_constraint(ecb_mandate_article_127__expansive_secondary_objectives, ecb_asset_purchase_programs).
narrative_ontology:affects_constraint(ecb_mandate_article_127__expansive_secondary_objectives, eu_green_deal_financing).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
