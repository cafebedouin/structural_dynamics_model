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
 *   constraint_id: ecb_mandate_article_127__expansive_secondary_objectives
 *   human_readable: ECB Mandate: Expansive Secondary Objectives Reading
 *   domain: monetary_policy/constitutional_law/eu_institutional_governance
 *
 * SUMMARY:
 *   This constraint represents the 'expansive secondary objectives' reading
 *   of Article 127 of the Treaty on the Functioning of the European Union
 *   (TFEU), which governs the ECB's mandate. This reading emphasizes that
 *   while price stability is the primary objective, the 'without prejudice'
 *   clause allows the ECB to give operational weight to other EU objectives,
 *   such as employment and growth, provided price stability is not
 *   threatened. This interpretation has gained prominence, particularly in
 *   response to economic crises, leading to a more active and discretionary
 *   role for the ECB in supporting broader economic goals.
 *
 * KEY AGENTS:
 *   - ecb_governing_council: Agenda setter (institutional/constrained) — interprets and implements the mandate.
 *   - eu_member_states: Beneficiary (institutional/constrained) — benefit from growth/employment support.
 *   - workers: Beneficiary (organized/constrained) — benefit from employment focus.
 *   - debtors: Beneficiary (moderate/constrained) — benefit from lower interest rates.
 *   - savers: Payer (powerless/constrained) — bear costs of lower returns/inflation.
 *   - creditors: Payer (powerful/mobile) — bear costs of lower returns/inflation.
 *   - orthodox_economists: Excluded (analytical/analytical) — advocate for stricter mandate.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ecb_mandate_article_127__expansive_secondary_objectives, 0.6).
domain_priors:suppression_score(ecb_mandate_article_127__expansive_secondary_objectives, 0.55).
domain_priors:theater_ratio(ecb_mandate_article_127__expansive_secondary_objectives, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ecb_mandate_article_127__expansive_secondary_objectives, extractiveness, 0.6).
narrative_ontology:constraint_metric(ecb_mandate_article_127__expansive_secondary_objectives, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(ecb_mandate_article_127__expansive_secondary_objectives, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ecb_mandate_article_127__expansive_secondary_objectives, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(ecb_mandate_article_127__expansive_secondary_objectives, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ecb_mandate_article_127__expansive_secondary_objectives, tangled_rope).
narrative_ontology:human_readable(ecb_mandate_article_127__expansive_secondary_objectives, "ECB Mandate: Expansive Secondary Objectives Reading").
narrative_ontology:topic_domain(ecb_mandate_article_127__expansive_secondary_objectives, "monetary_policy/constitutional_law/eu_institutional_governance").

domain_priors:requires_active_enforcement(ecb_mandate_article_127__expansive_secondary_objectives).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ecb_mandate_article_127__expansive_secondary_objectives, '245e2d15-5143-43ba-b073-68ae23aec812').
narrative_ontology:cs_kernel_codification('245e2d15-5143-43ba-b073-68ae23aec812', fixed_text).
narrative_ontology:cs_authority_grounding('245e2d15-5143-43ba-b073-68ae23aec812', lineage).
narrative_ontology:cs_interpretation_layer_present('245e2d15-5143-43ba-b073-68ae23aec812').
narrative_ontology:cs_reading_relation('245e2d15-5143-43ba-b073-68ae23aec812', ecb_mandate_article_127__orthodox_price_stability, coexists_with).
narrative_ontology:cs_reading_relation('245e2d15-5143-43ba-b073-68ae23aec812', ecb_mandate_article_127__climate_incorporation, influences).
narrative_ontology:cs_axiom('245e2d15-5143-43ba-b073-68ae23aec812', foundational, holistic_mandate_interpretation).
narrative_ontology:cs_axiom_status(holistic_mandate_interpretation, holdable).
narrative_ontology:cs_axiom_grounding('245e2d15-5143-43ba-b073-68ae23aec812', holistic_mandate_interpretation, conventional).
narrative_ontology:cs_axiom('245e2d15-5143-43ba-b073-68ae23aec812', foundational, discretionary_balancing_legitimate).
narrative_ontology:cs_axiom_status(discretionary_balancing_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('245e2d15-5143-43ba-b073-68ae23aec812', discretionary_balancing_legitimate, conventional).
narrative_ontology:cs_reference_frame('245e2d15-5143-43ba-b073-68ae23aec812', post_lisbon_treaty_interpretation).
narrative_ontology:cs_drift_state('245e2d15-5143-43ba-b073-68ae23aec812', contemporary_economic_crises_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('245e2d15-5143-43ba-b073-68ae23aec812', '').
narrative_ontology:cs_kernel_id(ecb_mandate_article_127__expansive_secondary_objectives, ecb_mandate_article_127).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ecb_mandate_article_127__expansive_secondary_objectives, eu_member_states).
narrative_ontology:constraint_beneficiary(ecb_mandate_article_127__expansive_secondary_objectives, workers).
narrative_ontology:constraint_beneficiary(ecb_mandate_article_127__expansive_secondary_objectives, debtors).
narrative_ontology:constraint_victim(ecb_mandate_article_127__expansive_secondary_objectives, savers).
narrative_ontology:constraint_victim(ecb_mandate_article_127__expansive_secondary_objectives, creditors).
narrative_ontology:constraint_victim(ecb_mandate_article_127__expansive_secondary_objectives, orthodox_economists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets and implements the ECB's mandate, giving operational weight to employment and growth objectives when price stability is not threatened. This involves discretionary balancing and policy choices that affect various economic actors across the Eurozone.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__expansive_secondary_objectives, ecb_governing_council, agenda_setter,
    institutional, generational, constrained, continental).

% Benefit from monetary policies that support economic growth and employment, especially during downturns, as this eases fiscal pressures and social unrest. They are constrained by the single currency and the ECB's independence.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__expansive_secondary_objectives, eu_member_states, beneficiary,
    institutional, generational, constrained, continental).

% Benefit from policies that prioritize employment and wage growth, potentially leading to lower unemployment rates and improved living standards. Their influence is primarily through national political processes.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__expansive_secondary_objectives, workers, beneficiary,
    organized, biographical, constrained, continental).

% Benefit from lower interest rates and potentially higher inflation, which reduces the real burden of their debts. Their position is largely passive, reacting to policy decisions.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__expansive_secondary_objectives, debtors, beneficiary,
    moderate, immediate, constrained, continental).

% Bear the costs of lower interest rates and potential inflation, which erodes the real value of their savings. Their options for mitigating these effects are limited within the Eurozone.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__expansive_secondary_objectives, savers, payer,
    powerless, biographical, constrained, continental).

% Bear the costs of lower interest rates and potential inflation, which reduces the real return on their investments. They have some mobility to seek higher returns but are still significantly affected by ECB policy.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__expansive_secondary_objectives, creditors, payer,
    powerful, biographical, mobile, continental).

% Advocate for a strict interpretation of the ECB's mandate, prioritizing price stability above all else. Their preferred policy approach is not fully adopted under this expansive reading, leading to their exclusion from direct influence on policy implementation.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__expansive_secondary_objectives, orthodox_economists, excluded,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ecb_mandate_article_127__expansive_secondary_objectives, diffuse).
narrative_ontology:fixing_cost_class(ecb_mandate_article_127__expansive_secondary_objectives, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates monetary policy across the Eurozone to achieve both price stability and support for broader economic objectives like employment and growth, balancing these goals to maintain overall economic welfare.
% TRANSFER_FUNCTION: Transfers economic benefits (e.g., lower borrowing costs, employment support) to member states, workers, and debtors, while transferring costs (e.g., eroded savings value, lower investment returns) to savers and creditors, through discretionary policy choices.
% ABSENT_VOICES: Strict monetarists and those advocating for a singular focus on price stability are effectively sidelined; they would argue for less discretion and a clearer, narrower mandate, but their views are subordinated to the expansive interpretation.
% DISAPPEARANCE_RATIONALE: If this expansive interpretation vanished, the ECB would likely revert to a stricter price stability focus, leading to different interest rate policies, potentially higher unemployment, and significant shifts in economic outcomes across the Eurozone, fundamentally altering the economic landscape.
% FOUNDING_PROBLEM: The original problem was to establish a credible, independent central bank for the Eurozone, primarily focused on maintaining price stability, but also acknowledging broader economic goals as secondary objectives.
% FOUNDING_PROBLEM_CORROBORATION: The ECB itself, many EU member states, and a significant portion of academic economists attest that the need for a central bank to balance these objectives remains live, especially in times of economic crisis. Critics (e.g., orthodox economists) contest the *degree* of balancing, but not the existence of the underlying tension.
narrative_ontology:disappearance_verdict(ecb_mandate_article_127__expansive_secondary_objectives, world_rearranges).
narrative_ontology:founding_problem_status(ecb_mandate_article_127__expansive_secondary_objectives, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ecb_mandate_article_127__expansive_secondary_objectives, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(ecb_mandate_article_127__expansive_secondary_objectives, 'none', 1).
narrative_ontology:epsilon_provenance(ecb_mandate_article_127__expansive_secondary_objectives, 0.6, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness (0.60) is moderate-high because the discretionary balancing, while aiming for overall welfare, inevitably creates winners and losers, with some parties bearing costs for the benefit of others. Suppression (0.55) is moderate as alternative policy paths (e.g., strict price stability) are not entirely foreclosed but are subordinated to this interpretation. The theater ratio (0.20) is low to moderate, reflecting that the balancing act is a genuine, active function, though institutional communication always has a performative element. Accessibility collapse (0.45) is moderate, as other policy options are harder to pursue but not impossible. Resistance (0.40) is moderate, coming from those who prefer a narrower mandate.
 *
 * PERSPECTIVAL GAP:
 *   The ECB Governing Council views this interpretation as a necessary and legitimate exercise of its mandate to serve the broader EU interest. Beneficiaries like workers and debtors see it as a vital support mechanism. However, payers like savers and creditors, along with orthodox economists, perceive it as an overreach that dilutes the primary mandate and imposes unfair costs, leading to significant divergence in how the constraint is experienced.
 *
 * DIRECTIONALITY LOGIC:
 *   The ECB Governing Council, as the agenda setter, benefits from the flexibility this reading provides, allowing it to respond to diverse economic pressures (d near 0.15). EU member states, workers, and debtors are beneficiaries, receiving support for growth and employment (d near 0.0-0.3). Savers and creditors are targets, bearing the costs of lower returns and potential inflation (d near 0.7-0.9). Orthodox economists are excluded, as their preferred policy is not fully implemented (d near 1.0 for their policy preference).
 *
 * MANDATROPHY ANALYSIS:
 *   This reading actively prevents mandatrophy by adapting the ECB's role to evolving economic realities and political pressures, ensuring the mandate remains relevant. However, the contestation around the 'without prejudice' clause highlights the risk of mission creep, where the original coordination function (price stability) could be diluted by an accumulation of secondary objectives, potentially turning a Tangled Rope into a Snare if the extraction from those preferring strict stability becomes too high without clear justification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    discretionary_scope_of_without_prejudice_clause,
    'How much operational discretion does the ''without prejudice'' clause truly grant the ECB in balancing secondary objectives, and what are its legal limits?',
    'Further legal interpretations by the European Court of Justice, or explicit legislative clarification from EU institutions.',
    'If the discretion is found to be narrower, the ECB''s ability to pursue expansive secondary objectives would be curtailed, potentially shifting the constraint towards a more ''orthodox price stability'' reading with lower extractiveness from savers/creditors. If broader, it solidifies the current reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(discretionary_scope_of_without_prejudice_clause, conceptual, 'Ambiguity regarding the legal scope of the ECB''s discretionary powers under Article 127.').

omega_variable(
    measurement_of_price_stability_not_threatened,
    'How is the condition ''price stability not threatened'' objectively defined and measured, and is this definition consistently applied?',
    'Independent audit of the ECB''s internal economic models and decision-making criteria, or a public, transparent framework for assessing threats to price stability.',
    'If the definition is found to be overly flexible or inconsistently applied, it could indicate a higher degree of extraction from those who prioritize strict price stability, pushing the constraint closer to a Snare. A clear, consistent definition would reinforce its Tangled Rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(measurement_of_price_stability_not_threatened, empirical, 'Ambiguity in the operational definition of the primary mandate''s non-threat condition.').

omega_variable(
    trade_offs_between_objectives,
    'Are the benefits to employment and growth from this expansive interpretation genuinely outweighing the costs to price stability and savers, or is it a zero-sum game with distributional consequences?',
    'Comprehensive, independent economic impact assessments comparing outcomes under different policy regimes, including counterfactual analysis.',
    'If the trade-offs are found to be consistently negative for price stability and savers, it would strengthen the argument that the constraint is primarily extractive (Snare). If positive, it would reinforce the coordination aspect (Tangled Rope or even Rope).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(trade_offs_between_objectives, empirical, 'Uncertainty about the net welfare effects of balancing multiple objectives.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ecb_mandate_article_127__expansive_secondary_objectives, 2009, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ecb__tr_t2009, ecb_mandate_article_127__expansive_secondary_objectives, theater_ratio, 2009, 0.1).
narrative_ontology:measurement(ecb__tr_t2012, ecb_mandate_article_127__expansive_secondary_objectives, theater_ratio, 2012, 0.15).
narrative_ontology:measurement(ecb__tr_t2015, ecb_mandate_article_127__expansive_secondary_objectives, theater_ratio, 2015, 0.2).
narrative_ontology:measurement(ecb__tr_t2018, ecb_mandate_article_127__expansive_secondary_objectives, theater_ratio, 2018, 0.22).
narrative_ontology:measurement(ecb__tr_t2021, ecb_mandate_article_127__expansive_secondary_objectives, theater_ratio, 2021, 0.25).
narrative_ontology:measurement(ecb__tr_t2024, ecb_mandate_article_127__expansive_secondary_objectives, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(ecb__be_t2009, ecb_mandate_article_127__expansive_secondary_objectives, base_extractiveness, 2009, 0.45).
narrative_ontology:measurement(ecb__be_t2012, ecb_mandate_article_127__expansive_secondary_objectives, base_extractiveness, 2012, 0.5).
narrative_ontology:measurement(ecb__be_t2015, ecb_mandate_article_127__expansive_secondary_objectives, base_extractiveness, 2015, 0.55).
narrative_ontology:measurement(ecb__be_t2018, ecb_mandate_article_127__expansive_secondary_objectives, base_extractiveness, 2018, 0.58).
narrative_ontology:measurement(ecb__be_t2021, ecb_mandate_article_127__expansive_secondary_objectives, base_extractiveness, 2021, 0.62).
narrative_ontology:measurement(ecb__be_t2024, ecb_mandate_article_127__expansive_secondary_objectives, base_extractiveness, 2024, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(ecb__su_t2009, ecb_mandate_article_127__expansive_secondary_objectives, suppression_requirement, 2009, 0.4).
narrative_ontology:measurement(ecb__su_t2012, ecb_mandate_article_127__expansive_secondary_objectives, suppression_requirement, 2012, 0.45).
narrative_ontology:measurement(ecb__su_t2015, ecb_mandate_article_127__expansive_secondary_objectives, suppression_requirement, 2015, 0.5).
narrative_ontology:measurement(ecb__su_t2018, ecb_mandate_article_127__expansive_secondary_objectives, suppression_requirement, 2018, 0.53).
narrative_ontology:measurement(ecb__su_t2021, ecb_mandate_article_127__expansive_secondary_objectives, suppression_requirement, 2021, 0.58).
narrative_ontology:measurement(ecb__su_t2024, ecb_mandate_article_127__expansive_secondary_objectives, suppression_requirement, 2024, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ecb_mandate_article_127__expansive_secondary_objectives, enforcement_mechanism).
narrative_ontology:affects_constraint(ecb_mandate_article_127__expansive_secondary_objectives, eu_fiscal_rules).
narrative_ontology:affects_constraint(ecb_mandate_article_127__expansive_secondary_objectives, eu_banking_union).
narrative_ontology:affects_constraint(ecb_mandate_article_127__expansive_secondary_objectives, ecb_mandate_article_127__orthodox_price_stability).
narrative_ontology:affects_constraint(ecb_mandate_article_127__expansive_secondary_objectives, ecb_mandate_article_127__climate_incorporation).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the ECB's Article 127 mandate, each with different structural properties and stakeholder impacts. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
