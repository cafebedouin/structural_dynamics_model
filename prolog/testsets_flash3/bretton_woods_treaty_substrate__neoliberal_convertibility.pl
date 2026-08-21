% ============================================================================
% CONSTRAINT STORY: bretton_woods_treaty_substrate__neoliberal_convertibility
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_bretton_woods_treaty_substrate__neoliberal_convertibility, []).

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
 *   constraint_id: bretton_woods_treaty_substrate__neoliberal_convertibility
 *   human_readable: Bretton Woods: Neoliberal Convertibility Reading
 *   domain: international_political_economy/monetary_history/institutional_design
 *
 * SUMMARY:
 *   This constraint represents the 'neoliberal convertibility' reading of the
 *   Bretton Woods treaty substrate, which interprets the original agreement
 *   as primarily enabling free capital markets by constraining government
 *   intervention. This reading emerged and gained dominance after the
 *   collapse of the fixed exchange rate system in the early 1970s, shifting
 *   the focus from 'embedded liberalism' (protecting domestic policy space)
 *   to financial liberalization. The constraint is classified as a Tangled
 *   Rope because it provides a coordination function for international
 *   finance while extracting policy autonomy from national governments,
 *   requiring active enforcement by international institutions.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bretton_woods_treaty_substrate__neoliberal_convertibility, 0.78).
domain_priors:suppression_score(bretton_woods_treaty_substrate__neoliberal_convertibility, 0.85).
domain_priors:theater_ratio(bretton_woods_treaty_substrate__neoliberal_convertibility, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__neoliberal_convertibility, extractiveness, 0.78).
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__neoliberal_convertibility, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__neoliberal_convertibility, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__neoliberal_convertibility, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__neoliberal_convertibility, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bretton_woods_treaty_substrate__neoliberal_convertibility, tangled_rope).
narrative_ontology:human_readable(bretton_woods_treaty_substrate__neoliberal_convertibility, "Bretton Woods: Neoliberal Convertibility Reading").
narrative_ontology:topic_domain(bretton_woods_treaty_substrate__neoliberal_convertibility, "international_political_economy/monetary_history/institutional_design").

domain_priors:requires_active_enforcement(bretton_woods_treaty_substrate__neoliberal_convertibility).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bretton_woods_treaty_substrate__neoliberal_convertibility, '2140e21c-7c5d-4ec1-9027-eb174ee6176d').
narrative_ontology:cs_kernel_codification('2140e21c-7c5d-4ec1-9027-eb174ee6176d', formalized).
narrative_ontology:cs_authority_grounding('2140e21c-7c5d-4ec1-9027-eb174ee6176d', extraction).
narrative_ontology:cs_interpretation_layer_present('2140e21c-7c5d-4ec1-9027-eb174ee6176d').
narrative_ontology:cs_reading_relation('2140e21c-7c5d-4ec1-9027-eb174ee6176d', bretton_woods_treaty_substrate__keynesian_embedded_liberalism, influences).
narrative_ontology:cs_reading_relation('2140e21c-7c5d-4ec1-9027-eb174ee6176d', bretton_woods_treaty_substrate__sovereignty_defense, influences).
narrative_ontology:cs_axiom('2140e21c-7c5d-4ec1-9027-eb174ee6176d', foundational, capital_mobility_as_efficiency_driver).
narrative_ontology:cs_axiom_status(capital_mobility_as_efficiency_driver, holdable).
narrative_ontology:cs_axiom_grounding('2140e21c-7c5d-4ec1-9027-eb174ee6176d', capital_mobility_as_efficiency_driver, empirically_contingent).
narrative_ontology:cs_axiom('2140e21c-7c5d-4ec1-9027-eb174ee6176d', foundational, government_intervention_as_market_distortion).
narrative_ontology:cs_axiom_status(government_intervention_as_market_distortion, holdable).
narrative_ontology:cs_axiom_grounding('2140e21c-7c5d-4ec1-9027-eb174ee6176d', government_intervention_as_market_distortion, empirically_contingent).
narrative_ontology:cs_reference_frame('2140e21c-7c5d-4ec1-9027-eb174ee6176d', post_bretton_woods_liberalization).
narrative_ontology:cs_drift_state('2140e21c-7c5d-4ec1-9027-eb174ee6176d', contemporary_global_finance, gap(stable, minor, true)).
narrative_ontology:cs_created_at('2140e21c-7c5d-4ec1-9027-eb174ee6176d', '').
narrative_ontology:cs_kernel_id(bretton_woods_treaty_substrate__neoliberal_convertibility, bretton_woods_treaty_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bretton_woods_treaty_substrate__neoliberal_convertibility, international_financial_institutions).
narrative_ontology:constraint_beneficiary(bretton_woods_treaty_substrate__neoliberal_convertibility, global_capital_markets).
narrative_ontology:constraint_victim(bretton_woods_treaty_substrate__neoliberal_convertibility, national_governments_policy_autonomy).
narrative_ontology:constraint_victim(bretton_woods_treaty_substrate__neoliberal_convertibility, developing_economies).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the rules of the international monetary system, promoting capital mobility and convertibility. Benefits from the stability and growth of global financial markets, which it helps to regulate and expand. Its mandate is interpreted to prioritize open capital flows.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__neoliberal_convertibility, international_financial_institutions, agenda_setter,
    institutional, generational, arbitrage, global).

% Thrives on the free movement of capital across borders, enabled by currency convertibility and reduced government intervention. Benefits from increased investment opportunities and reduced transaction costs, leading to higher profits and influence.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__neoliberal_convertibility, global_capital_markets, beneficiary,
    institutional, generational, arbitrage, global).

% Bears the cost of reduced ability to implement independent monetary and fiscal policies, especially those involving capital controls or non-market interventions. Must align domestic policies with international financial stability requirements, limiting sovereign choices.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__neoliberal_convertibility, national_governments_policy_autonomy, payer,
    powerful, biographical, constrained, national).

% Are particularly vulnerable to capital flight and external financial shocks due to open capital accounts. Their ability to use capital controls for development or crisis management is severely constrained, often leading to economic instability and dependence on international lenders.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__neoliberal_convertibility, developing_economies, payer,
    moderate, generational, trapped, regional).

% Would advocate for policies that prioritize full employment and social welfare over capital mobility, often requiring capital controls. Their voice is marginalized in a system that prioritizes financial liberalization, leading to downward pressure on wages and social protections.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__neoliberal_convertibility, domestic_labor_movements, excluded,
    organized, biographical, constrained, national).

% Analyze the benefits of free capital markets and convertibility, often providing intellectual justification for the constraint. Their research supports policies that further liberalize financial flows and reduce government intervention.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__neoliberal_convertibility, economic_liberal_academics, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a framework for currency convertibility and stable exchange rates, facilitating international trade and investment by reducing currency risk and transaction costs for global capital.
% TRANSFER_FUNCTION: Transfers policy autonomy from national governments to the international financial system, enabling global capital markets to operate with fewer restrictions and extract higher returns.
% ABSENT_VOICES: Advocates for national economic sovereignty and capital controls (e.g., some developing country leaders, heterodox economists, labor unions) are systematically excluded from the core decision-making bodies that interpret and enforce the Bretton Woods legacy, as their positions are deemed contrary to the system's foundational principles.
% DISAPPEARANCE_RATIONALE: If the neoliberal interpretation of Bretton Woods vanished, national governments would regain significant policy space, potentially reintroducing capital controls. Global capital markets would face increased friction and fragmentation, leading to a re-localization of finance and a re-evaluation of international economic governance.
% FOUNDING_PROBLEM: The interwar period was characterized by competitive devaluations, trade wars, and capital flight, leading to economic instability and a collapse of international trade.
% FOUNDING_PROBLEM_CORROBORATION: International financial institutions and economic liberal academics argue that the problem of instability persists, necessitating continued adherence to open capital markets. Critics, including some national governments and development economists, argue that the original problem has evolved, and the current interpretation exacerbates new forms of instability, particularly for developing economies. Independent historical analysis supports the shift in interpretation over time.
narrative_ontology:disappearance_verdict(bretton_woods_treaty_substrate__neoliberal_convertibility, world_rearranges).
narrative_ontology:founding_problem_status(bretton_woods_treaty_substrate__neoliberal_convertibility, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bretton_woods_treaty_substrate__neoliberal_convertibility, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(bretton_woods_treaty_substrate__neoliberal_convertibility, 'none', 1).
narrative_ontology:epsilon_provenance(bretton_woods_treaty_substrate__neoliberal_convertibility, 0.78, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(bretton_woods_treaty_substrate__neoliberal_convertibility_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(bretton_woods_treaty_substrate__neoliberal_convertibility, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(bretton_woods_treaty_substrate__neoliberal_convertibility_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.78) because the benefits of capital mobility accrue disproportionately to international finance, while the costs (e.g., vulnerability to financial crises, loss of policy tools) are borne by national governments and developing economies. Suppression (0.85) is also high, reflecting the strong institutional pressure and conditionalities imposed by international financial institutions to maintain open capital accounts. Theater ratio is low (0.15) as the enforcement mechanisms are genuinely functional in promoting capital mobility, with little performative overhead. The measurement series track the increasing extractiveness and suppression as this reading gained dominance post-1971.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of international finance, this reading of Bretton Woods is a necessary coordination mechanism for global prosperity. From the perspective of national governments, particularly in developing economies, it is an extractive force that limits their sovereign capacity to manage their own economies. The engine's per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   International financial institutions and global capital markets are clear beneficiaries (d near 0.0), as the constraint directly enables their operations and profit. National governments' policy autonomy and developing economies are targets (d near 1.0), as they bear the costs of restricted policy tools and increased vulnerability. Domestic labor movements are excluded, as their policy preferences (e.g., capital controls) are incompatible with this reading's core tenets.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    original_intent_vs_evolved_interpretation,
    'To what extent does the ''neoliberal convertibility'' reading align with the original intent of the Bretton Woods founders, versus representing an evolved interpretation driven by changing economic conditions and power dynamics?',
    'Historical analysis of primary source documents, diplomatic correspondence, and economic policy debates from the 1940s-1970s, comparing stated goals with the outcomes of later interpretations.',
    'If largely an evolved interpretation, it strengthens the argument that the constraint is a constructed Snare or Tangled Rope rather than an inevitable outcome of the original agreement. If closely aligned with original intent, it would lend more credence to the coordination function, though not necessarily reduce extractiveness.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(original_intent_vs_evolved_interpretation, conceptual, 'Ambiguity regarding the historical fidelity of the neoliberal convertibility reading to the original Bretton Woods intent.').

omega_variable(
    capital_mobility_vs_stability_tradeoff,
    'Is the observed level of capital mobility and convertibility genuinely optimal for global economic stability, or does it introduce systemic risks that outweigh its benefits?',
    'Empirical studies comparing economic stability and growth outcomes in countries with varying degrees of capital account openness, particularly during financial crises. Analysis of the frequency and severity of financial crises in the era of high capital mobility.',
    'If high capital mobility is found to be a net destabilizing force, it would undermine the coordination justification for this reading, pushing its classification closer to a Snare. If it demonstrably enhances stability, it would reinforce the coordination aspect.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capital_mobility_vs_stability_tradeoff, empirical, 'The empirical tradeoff between free capital markets and global economic stability.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of national policy autonomy structural (e.g., IMF conditionalities, market pressure) or internalized (e.g., national policymakers adopting neoliberal ideology)?',
    'Post-exit suppression trajectory: if national policymakers continue to avoid capital controls even when external pressures are reduced, reclassify as partially internalized. Analysis of policy discourse and elite education trends.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — national policymakers carry the suppression with them after external pressures ease. This would make the constraint more resilient to external challenges.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for national policy autonomy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bretton_woods_treaty_substrate__neoliberal_convertibility, 1971, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bret_tr_t1971, bretton_woods_treaty_substrate__neoliberal_convertibility, theater_ratio, 1971, 0.2).
narrative_ontology:measurement(bret_tr_t1985, bretton_woods_treaty_substrate__neoliberal_convertibility, theater_ratio, 1985, 0.18).
narrative_ontology:measurement(bret_tr_t1995, bretton_woods_treaty_substrate__neoliberal_convertibility, theater_ratio, 1995, 0.15).
narrative_ontology:measurement(bret_tr_t2008, bretton_woods_treaty_substrate__neoliberal_convertibility, theater_ratio, 2008, 0.12).
narrative_ontology:measurement(bret_tr_t2015, bretton_woods_treaty_substrate__neoliberal_convertibility, theater_ratio, 2015, 0.15).
narrative_ontology:measurement(bret_tr_t2024, bretton_woods_treaty_substrate__neoliberal_convertibility, theater_ratio, 2024, 0.15).

% Extraction over time
narrative_ontology:measurement(bret_be_t1971, bretton_woods_treaty_substrate__neoliberal_convertibility, base_extractiveness, 1971, 0.6).
narrative_ontology:measurement(bret_be_t1985, bretton_woods_treaty_substrate__neoliberal_convertibility, base_extractiveness, 1985, 0.7).
narrative_ontology:measurement(bret_be_t1995, bretton_woods_treaty_substrate__neoliberal_convertibility, base_extractiveness, 1995, 0.75).
narrative_ontology:measurement(bret_be_t2008, bretton_woods_treaty_substrate__neoliberal_convertibility, base_extractiveness, 2008, 0.8).
narrative_ontology:measurement(bret_be_t2015, bretton_woods_treaty_substrate__neoliberal_convertibility, base_extractiveness, 2015, 0.78).
narrative_ontology:measurement(bret_be_t2024, bretton_woods_treaty_substrate__neoliberal_convertibility, base_extractiveness, 2024, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(bret_su_t1971, bretton_woods_treaty_substrate__neoliberal_convertibility, suppression_requirement, 1971, 0.7).
narrative_ontology:measurement(bret_su_t1985, bretton_woods_treaty_substrate__neoliberal_convertibility, suppression_requirement, 1985, 0.8).
narrative_ontology:measurement(bret_su_t1995, bretton_woods_treaty_substrate__neoliberal_convertibility, suppression_requirement, 1995, 0.85).
narrative_ontology:measurement(bret_su_t2008, bretton_woods_treaty_substrate__neoliberal_convertibility, suppression_requirement, 2008, 0.88).
narrative_ontology:measurement(bret_su_t2015, bretton_woods_treaty_substrate__neoliberal_convertibility, suppression_requirement, 2015, 0.85).
narrative_ontology:measurement(bret_su_t2024, bretton_woods_treaty_substrate__neoliberal_convertibility, suppression_requirement, 2024, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bretton_woods_treaty_substrate__neoliberal_convertibility, global_infrastructure).
narrative_ontology:affects_constraint(bretton_woods_treaty_substrate__neoliberal_convertibility, keynesian_embedded_liberalism).
narrative_ontology:affects_constraint(bretton_woods_treaty_substrate__neoliberal_convertibility, sovereignty_defense).
narrative_ontology:affects_constraint(bretton_woods_treaty_substrate__neoliberal_convertibility, global_trade_agreements).
narrative_ontology:affects_constraint(bretton_woods_treaty_substrate__neoliberal_convertibility, developing_country_debt_conditionalities).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the Bretton Woods treaty substrate. The 'neoliberal convertibility' reading emphasizes free capital markets and constrains government intervention. It contrasts with the 'keynesian embedded liberalism' reading (which prioritizes domestic policy space) and the 'sovereignty defense' reading (which emphasizes national monetary autonomy).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
