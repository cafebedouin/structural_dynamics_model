% ============================================================================
% CONSTRAINT STORY: ecb_mandate_article_127__orthodox_price_stability
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ecb_mandate_article_127__orthodox_price_stability, []).

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
 *   constraint_id: ecb_mandate_article_127__orthodox_price_stability
 *   human_readable: ECB Mandate: Orthodox Price Stability Reading
 *   domain: monetary_policy/constitutional_law/eu_governance
 *
 * SUMMARY:
 *   This constraint represents the orthodox reading of the ECB's mandate
 *   under Article 127 of the TFEU, which asserts an exclusive focus on
 *   maintaining price stability (defined as 2% inflation) and subordinates
 *   all other objectives (such as employment, growth, or environmental
 *   protection) to this primary goal, rendering them non-operational. This
 *   reading is contested by those advocating for a broader interpretation of
 *   the mandate.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ecb_mandate_article_127__orthodox_price_stability, 0.7).
domain_priors:suppression_score(ecb_mandate_article_127__orthodox_price_stability, 0.8).
domain_priors:theater_ratio(ecb_mandate_article_127__orthodox_price_stability, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ecb_mandate_article_127__orthodox_price_stability, extractiveness, 0.7).
narrative_ontology:constraint_metric(ecb_mandate_article_127__orthodox_price_stability, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(ecb_mandate_article_127__orthodox_price_stability, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ecb_mandate_article_127__orthodox_price_stability, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(ecb_mandate_article_127__orthodox_price_stability, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ecb_mandate_article_127__orthodox_price_stability, tangled_rope).
narrative_ontology:human_readable(ecb_mandate_article_127__orthodox_price_stability, "ECB Mandate: Orthodox Price Stability Reading").
narrative_ontology:topic_domain(ecb_mandate_article_127__orthodox_price_stability, "monetary_policy/constitutional_law/eu_governance").

domain_priors:requires_active_enforcement(ecb_mandate_article_127__orthodox_price_stability).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ecb_mandate_article_127__orthodox_price_stability, 'a55e1faf-a760-48e1-8f7e-fb74130bcddd').
narrative_ontology:cs_kernel_codification('a55e1faf-a760-48e1-8f7e-fb74130bcddd', fixed_text).
narrative_ontology:cs_authority_grounding('a55e1faf-a760-48e1-8f7e-fb74130bcddd', lineage).
narrative_ontology:cs_interpretation_layer_present('a55e1faf-a760-48e1-8f7e-fb74130bcddd').
narrative_ontology:cs_reading_relation('a55e1faf-a760-48e1-8f7e-fb74130bcddd', ecb_mandate_article_127__expansive_secondary_objectives, forecloses).
narrative_ontology:cs_reading_relation('a55e1faf-a760-48e1-8f7e-fb74130bcddd', ecb_mandate_article_127__climate_incorporation, forecloses).
narrative_ontology:cs_axiom('a55e1faf-a760-48e1-8f7e-fb74130bcddd', foundational, price_stability_is_sole_primary_objective).
narrative_ontology:cs_axiom_status(price_stability_is_sole_primary_objective, holdable).
narrative_ontology:cs_axiom_grounding('a55e1faf-a760-48e1-8f7e-fb74130bcddd', price_stability_is_sole_primary_objective, conventional).
narrative_ontology:cs_axiom('a55e1faf-a760-48e1-8f7e-fb74130bcddd', foundational, secondary_objectives_are_subordinate_and_non_operational).
narrative_ontology:cs_axiom_status(secondary_objectives_are_subordinate_and_non_operational, holdable).
narrative_ontology:cs_axiom_grounding('a55e1faf-a760-48e1-8f7e-fb74130bcddd', secondary_objectives_are_subordinate_and_non_operational, conventional).
narrative_ontology:cs_reference_frame('a55e1faf-a760-48e1-8f7e-fb74130bcddd', single_mandate_focus).
narrative_ontology:cs_drift_state('a55e1faf-a760-48e1-8f7e-fb74130bcddd', contemporary_mandate_debate, gap(stable, minor, true)).
narrative_ontology:cs_created_at('a55e1faf-a760-48e1-8f7e-fb74130bcddd', '').
narrative_ontology:cs_kernel_id(ecb_mandate_article_127__orthodox_price_stability, ecb_mandate_article_127).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ecb_mandate_article_127__orthodox_price_stability, savers).
narrative_ontology:constraint_beneficiary(ecb_mandate_article_127__orthodox_price_stability, creditors).
narrative_ontology:constraint_beneficiary(ecb_mandate_article_127__orthodox_price_stability, financial_markets).
narrative_ontology:constraint_beneficiary(ecb_mandate_article_127__orthodox_price_stability, orthodox_economists).
narrative_ontology:constraint_victim(ecb_mandate_article_127__orthodox_price_stability, unemployed_citizens).
narrative_ontology:constraint_victim(ecb_mandate_article_127__orthodox_price_stability, indebted_states).
narrative_ontology:constraint_victim(ecb_mandate_article_127__orthodox_price_stability, climate_vulnerable_sectors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets and enforces the ECB's mandate, prioritizing price stability as its exclusive primary objective. Actively resists calls for mandate expansion or reinterpretation to include other objectives operationally.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__orthodox_price_stability, ecb_governing_council, agenda_setter,
    institutional, generational, constrained, continental).

% Benefit from stable prices that preserve the purchasing power of their savings. Their financial assets are less eroded by inflation, aligning with the orthodox interpretation of the mandate.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__orthodox_price_stability, savers, beneficiary,
    moderate, biographical, mobile, global).

% Benefit from a low inflation environment, which ensures the real value of their loans and investments is maintained or increased. They are strong proponents of the orthodox price stability focus.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__orthodox_price_stability, creditors, beneficiary,
    powerful, biographical, arbitrage, global).

% Value the predictability and stability provided by a clear, singular focus on price stability, which reduces uncertainty and facilitates long-term investment planning. They generally support the orthodox interpretation.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__orthodox_price_stability, financial_markets, beneficiary,
    institutional, immediate, arbitrage, global).

% Their intellectual framework and policy recommendations are validated and implemented by the ECB's adherence to the orthodox price stability mandate. They provide theoretical justification for the current approach.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__orthodox_price_stability, orthodox_economists, beneficiary,
    analytical, biographical, analytical, global).

% Bear the costs of potentially higher unemployment or slower economic growth if monetary policy is exclusively focused on inflation control and does not actively support employment objectives. Their voices are often marginalized in policy debates.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__orthodox_price_stability, unemployed_citizens, payer,
    powerless, immediate, trapped, national).

% Face greater fiscal pressure and reduced flexibility if monetary policy does not consider growth or debt sustainability, especially during crises. They advocate for a more expansive interpretation of the mandate.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__orthodox_price_stability, indebted_states, payer,
    organized, generational, constrained, national).

% Suffer from the externalization of climate risks, as monetary policy does not actively integrate climate considerations into its operations, potentially hindering green transition efforts and exacerbating climate-related financial instability.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__orthodox_price_stability, climate_vulnerable_sectors, payer,
    organized, generational, constrained, regional).

% Has democratic oversight but limited direct operational influence over the ECB's mandate interpretation. While it can debate and pass resolutions, the ECB maintains its independence in operationalizing its mandate.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__orthodox_price_stability, eu_parliament, excluded,
    institutional, generational, analytical, continental).

% Their alternative economic theories and policy recommendations, which often advocate for broader monetary policy objectives (e.g., employment, climate action), are systematically excluded from the operational framework of the ECB under this orthodox reading.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__orthodox_price_stability, heterodox_economists, excluded,
    analytical, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a clear, singular objective for monetary policy across the Eurozone, aiming to coordinate expectations around price stability and maintain the currency's value.
% TRANSFER_FUNCTION: Transfers economic stability and asset value preservation to savers, creditors, and financial markets, while transferring the costs of foregone employment, growth, and climate action to other sectors and citizens.
% ABSENT_VOICES: Labor unions, climate activists, and heterodox economists advocating for a broader mandate that includes employment, growth, or climate objectives are structurally excluded from the operational decision-making process, despite their significant societal impact.
% DISAPPEARANCE_RATIONALE: If the exclusive focus on the 2% inflation target and the subordination of other objectives vanished, the ECB's policy framework would lose its anchor, leading to significant uncertainty, potential inflation volatility, and a scramble to define new operational priorities, fundamentally reorganizing Eurozone monetary policy.
% FOUNDING_PROBLEM: The historical experience of hyperinflation in Europe and the need for a credible, independent central bank to ensure monetary stability and facilitate economic integration across diverse member states.
% FOUNDING_PROBLEM_CORROBORATION: The ECB Governing Council, financial institutions, and orthodox economists attest that the founding problem of inflation risk remains live. However, labor unions, some member states, and heterodox economists argue that the problem is largely contained and that the exclusive focus now serves to suppress other legitimate policy goals, making its 'live' status contested.
narrative_ontology:disappearance_verdict(ecb_mandate_article_127__orthodox_price_stability, world_rearranges).
narrative_ontology:founding_problem_status(ecb_mandate_article_127__orthodox_price_stability, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ecb_mandate_article_127__orthodox_price_stability, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(ecb_mandate_article_127__orthodox_price_stability, 'none', 1).
narrative_ontology:epsilon_provenance(ecb_mandate_article_127__orthodox_price_stability, 0.7, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ecb_mandate_article_127__orthodox_price_stability_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ecb_mandate_article_127__orthodox_price_stability, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ecb_mandate_article_127__orthodox_price_stability_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.7) is high because the exclusive focus on price stability imposes costs on other societal objectives, such as employment and growth, which are not actively pursued by monetary policy. Suppression (0.8) is high due to the active institutional resistance to any reinterpretation or expansion of the mandate, effectively suppressing alternative policy approaches. The theater ratio (0.1) is low because, from this reading's perspective, the ECB genuinely and consistently pursues its stated primary objective without significant performative deviation. Accessibility collapse (0.6) is moderate as alternative interpretations exist conceptually but are institutionally difficult to implement. Resistance (0.7) is high due to ongoing political and academic pressure for mandate reform.
 *
 * PERSPECTIVAL GAP:
 *   The ECB Governing Council and its beneficiaries (savers, creditors, orthodox economists) perceive this constraint as a necessary and effective coordination mechanism for economic stability. Conversely, victims (unemployed citizens, indebted states, climate-vulnerable sectors) experience it as an extractive force that prioritizes certain interests over others, leading to significant societal costs. The engine will compute these divergent classifications from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   The ECB Governing Council, savers, creditors, and financial markets are beneficiaries, as the constraint's operation directly aligns with their interests in price stability and asset preservation. Orthodox economists also benefit from the validation of their intellectual framework. Unemployed citizens, indebted states, and climate-vulnerable sectors are victims, bearing the costs of a monetary policy that does not actively address their concerns. The EU Parliament and heterodox economists are excluded, as their perspectives are not operationally integrated into the mandate's interpretation.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mandate_interpretive_choice,
    'Is the exclusive focus on price stability a structural requirement of the TFEU Article 127, or an interpretive choice by the ECB Governing Council?',
    'A ruling by the European Court of Justice on the precise scope and hierarchy of Article 127''s objectives, or a formal amendment to the TFEU clarifying the mandate.',
    'If it''s an interpretive choice, the constraint''s suppression and extractiveness are more clearly attributable to institutional agency rather than legal necessity, potentially reclassifying it closer to a Snare. If it''s a structural requirement, the constraint''s Mountain-like qualities (from a legal perspective) would be reinforced.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(mandate_interpretive_choice, conceptual, 'Ambiguity between legal necessity and institutional interpretation of the ECB mandate.').

omega_variable(
    cost_of_externalized_objectives,
    'What are the quantifiable economic and social costs of externalizing objectives like employment, growth, and climate action from the ECB''s operational mandate?',
    'Comprehensive, independent economic and social impact assessments comparing outcomes under the current mandate with counterfactual scenarios where secondary objectives are given operational weight.',
    'Higher quantifiable costs would increase the measured extractiveness of the constraint, strengthening its classification as a Tangled Rope or Snare. Lower costs would support the orthodox view that the trade-offs are minimal or justified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cost_of_externalized_objectives, empirical, 'Quantification of costs imposed by the narrow mandate focus.').

omega_variable(
    price_stability_definition_flexibility,
    'Is the 2% inflation target an empirically derived optimal target, or a conventional policy choice that could be adjusted without compromising price stability?',
    'Academic consensus shifts based on new macroeconomic research regarding optimal inflation targets, or a formal review by the ECB itself leading to a revised target.',
    'If the 2% target is found to be a flexible policy choice, it would highlight the agency in setting the constraint''s parameters, potentially reducing its perceived Mountain-like qualities and increasing its classification as a constructed constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(price_stability_definition_flexibility, empirical, 'Flexibility of the 2% inflation target.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ecb_mandate_article_127__orthodox_price_stability, 2000, 2030).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ecb__tr_t2000, ecb_mandate_article_127__orthodox_price_stability, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(ecb__tr_t2005, ecb_mandate_article_127__orthodox_price_stability, theater_ratio, 2005, 0.1).
narrative_ontology:measurement(ecb__tr_t2010, ecb_mandate_article_127__orthodox_price_stability, theater_ratio, 2010, 0.1).
narrative_ontology:measurement(ecb__tr_t2015, ecb_mandate_article_127__orthodox_price_stability, theater_ratio, 2015, 0.1).
narrative_ontology:measurement(ecb__tr_t2020, ecb_mandate_article_127__orthodox_price_stability, theater_ratio, 2020, 0.1).
narrative_ontology:measurement(ecb__tr_t2025, ecb_mandate_article_127__orthodox_price_stability, theater_ratio, 2025, 0.1).
narrative_ontology:measurement(ecb__tr_t2030, ecb_mandate_article_127__orthodox_price_stability, theater_ratio, 2030, 0.1).

% Extraction over time
narrative_ontology:measurement(ecb__be_t2000, ecb_mandate_article_127__orthodox_price_stability, base_extractiveness, 2000, 0.55).
narrative_ontology:measurement(ecb__be_t2005, ecb_mandate_article_127__orthodox_price_stability, base_extractiveness, 2005, 0.6).
narrative_ontology:measurement(ecb__be_t2010, ecb_mandate_article_127__orthodox_price_stability, base_extractiveness, 2010, 0.65).
narrative_ontology:measurement(ecb__be_t2015, ecb_mandate_article_127__orthodox_price_stability, base_extractiveness, 2015, 0.68).
narrative_ontology:measurement(ecb__be_t2020, ecb_mandate_article_127__orthodox_price_stability, base_extractiveness, 2020, 0.7).
narrative_ontology:measurement(ecb__be_t2025, ecb_mandate_article_127__orthodox_price_stability, base_extractiveness, 2025, 0.7).
narrative_ontology:measurement(ecb__be_t2030, ecb_mandate_article_127__orthodox_price_stability, base_extractiveness, 2030, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(ecb__su_t2000, ecb_mandate_article_127__orthodox_price_stability, suppression_requirement, 2000, 0.65).
narrative_ontology:measurement(ecb__su_t2005, ecb_mandate_article_127__orthodox_price_stability, suppression_requirement, 2005, 0.7).
narrative_ontology:measurement(ecb__su_t2010, ecb_mandate_article_127__orthodox_price_stability, suppression_requirement, 2010, 0.75).
narrative_ontology:measurement(ecb__su_t2015, ecb_mandate_article_127__orthodox_price_stability, suppression_requirement, 2015, 0.78).
narrative_ontology:measurement(ecb__su_t2020, ecb_mandate_article_127__orthodox_price_stability, suppression_requirement, 2020, 0.8).
narrative_ontology:measurement(ecb__su_t2025, ecb_mandate_article_127__orthodox_price_stability, suppression_requirement, 2025, 0.8).
narrative_ontology:measurement(ecb__su_t2030, ecb_mandate_article_127__orthodox_price_stability, suppression_requirement, 2030, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ecb_mandate_article_127__orthodox_price_stability, enforcement_mechanism).
narrative_ontology:affects_constraint(ecb_mandate_article_127__orthodox_price_stability, ecb_mandate_article_127__expansive_secondary_objectives).
narrative_ontology:affects_constraint(ecb_mandate_article_127__orthodox_price_stability, ecb_mandate_article_127__climate_incorporation).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the ECB's Article 127 mandate. This 'orthodox_price_stability' reading emphasizes an exclusive focus on price stability, subordinating other objectives. It is linked to the 'expansive_secondary_objectives' and 'climate_incorporation' readings, which propose broader interpretations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
