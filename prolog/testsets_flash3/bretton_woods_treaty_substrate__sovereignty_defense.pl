% ============================================================================
% CONSTRAINT STORY: bretton_woods_treaty_substrate__sovereignty_defense
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_bretton_woods_treaty_substrate__sovereignty_defense, []).

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
 *   constraint_id: bretton_woods_treaty_substrate__sovereignty_defense
 *   human_readable: Bretton Woods Treaty Substrate: Sovereignty Defense Reading
 *   domain: international_political_economy/monetary_history/institutional_design
 *
 * SUMMARY:
 *   This constraint story represents the 'sovereignty defense' reading of the
 *   Bretton Woods treaty substrate. It focuses on how the system, while
 *   ostensibly promoting stability, imposed external monetary discipline on
 *   non-reserve currency states, thereby limiting their national monetary
 *   sovereignty and creating an 'exorbitant privilege' for the United States.
 *   The gold anchor, initially conceived as a stabilizer, became a mechanism
 *   for this asymmetric extraction, eventually resembling a snare for many
 *   nations.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bretton_woods_treaty_substrate__sovereignty_defense, 0.65).
domain_priors:suppression_score(bretton_woods_treaty_substrate__sovereignty_defense, 0.75).
domain_priors:theater_ratio(bretton_woods_treaty_substrate__sovereignty_defense, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__sovereignty_defense, extractiveness, 0.65).
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__sovereignty_defense, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__sovereignty_defense, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__sovereignty_defense, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__sovereignty_defense, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bretton_woods_treaty_substrate__sovereignty_defense, tangled_rope).
narrative_ontology:human_readable(bretton_woods_treaty_substrate__sovereignty_defense, "Bretton Woods Treaty Substrate: Sovereignty Defense Reading").
narrative_ontology:topic_domain(bretton_woods_treaty_substrate__sovereignty_defense, "international_political_economy/monetary_history/institutional_design").

domain_priors:requires_active_enforcement(bretton_woods_treaty_substrate__sovereignty_defense).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bretton_woods_treaty_substrate__sovereignty_defense, '2b058760-e335-4e43-96bd-8dc60e772780').
narrative_ontology:cs_kernel_codification('2b058760-e335-4e43-96bd-8dc60e772780', formalized).
narrative_ontology:cs_authority_grounding('2b058760-e335-4e43-96bd-8dc60e772780', extraction).
narrative_ontology:cs_interpretation_layer_present('2b058760-e335-4e43-96bd-8dc60e772780').
narrative_ontology:cs_reading_relation('2b058760-e335-4e43-96bd-8dc60e772780', bretton_woods_treaty_substrate__keynesian_embedded_liberalism, coexists_with).
narrative_ontology:cs_reading_relation('2b058760-e335-4e43-96bd-8dc60e772780', bretton_woods_treaty_substrate__neoliberal_convertibility, coexists_with).
narrative_ontology:cs_axiom('2b058760-e335-4e43-96bd-8dc60e772780', foundational, national_monetary_sovereignty_is_paramount).
narrative_ontology:cs_axiom_status(national_monetary_sovereignty_is_paramount, holdable).
narrative_ontology:cs_axiom_grounding('2b058760-e335-4e43-96bd-8dc60e772780', national_monetary_sovereignty_is_paramount, deontological).
narrative_ontology:cs_axiom('2b058760-e335-4e43-96bd-8dc60e772780', foundational, dollar_hegemony_is_a_structural_extraction).
narrative_ontology:cs_axiom_status(dollar_hegemony_is_a_structural_extraction, holdable).
narrative_ontology:cs_axiom_grounding('2b058760-e335-4e43-96bd-8dc60e772780', dollar_hegemony_is_a_structural_extraction, empirically_contingent).
narrative_ontology:cs_reference_frame('2b058760-e335-4e43-96bd-8dc60e772780', post_war_monetary_order).
narrative_ontology:cs_drift_state('2b058760-e335-4e43-96bd-8dc60e772780', contemporary_global_finance, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('2b058760-e335-4e43-96bd-8dc60e772780', '').
narrative_ontology:cs_kernel_id(bretton_woods_treaty_substrate__sovereignty_defense, bretton_woods_treaty_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bretton_woods_treaty_substrate__sovereignty_defense, united_states).
narrative_ontology:constraint_beneficiary(bretton_woods_treaty_substrate__sovereignty_defense, reserve_currency_issuers).
narrative_ontology:constraint_victim(bretton_woods_treaty_substrate__sovereignty_defense, non_reserve_currency_states).
narrative_ontology:constraint_victim(bretton_woods_treaty_substrate__sovereignty_defense, developing_nations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% As the issuer of the primary reserve currency, the U.S. benefits from 'exorbitant privilege,' allowing it to run current account deficits without facing the same external monetary discipline as other nations. It sets the terms of the system and enforces its stability.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__sovereignty_defense, united_states, agenda_setter,
    institutional, generational, arbitrage, global).

% These states are compelled to maintain fixed exchange rates against the dollar, requiring them to accumulate dollar reserves and subject their domestic monetary policy to external constraints. Their ability to pursue independent economic policy is limited by the need to defend their currency peg.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__sovereignty_defense, non_reserve_currency_states, payer,
    moderate, biographical, constrained, national).

% Often lacking sufficient dollar reserves, these nations are particularly vulnerable to external shocks and capital flight. The fixed exchange rate system, while offering some stability, also limits their policy options for development and crisis response, making them net payers into the system's stability.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__sovereignty_defense, developing_nations, payer,
    powerless, generational, trapped, national).

% Administers the rules of the Bretton Woods system, providing loans to states facing balance of payments difficulties, often with conditions that reinforce external monetary discipline. It acts as an enforcer of the system's stability, which disproportionately affects non-reserve currency states.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__sovereignty_defense, international_monetary_fund, agenda_setter,
    institutional, generational, constrained, global).

% Beyond the U.S., other nations whose currencies are used as reserves (though secondary to the dollar) also benefit from greater flexibility in managing their external accounts and less pressure to conform to strict monetary discipline.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__sovereignty_defense, reserve_currency_issuers, beneficiary,
    institutional, generational, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a system of fixed exchange rates, anchored to the U.S. dollar, which is convertible to gold, aiming to prevent competitive devaluations and promote international trade stability.
% TRANSFER_FUNCTION: Transfers monetary policy autonomy and economic flexibility from non-reserve currency states to the United States and other reserve currency issuers, in exchange for a degree of exchange rate stability.
% ABSENT_VOICES: Advocates for a truly multilateral reserve system or a global currency, who would argue against the dollar's hegemonic role and the associated 'exorbitant privilege,' are structurally excluded from the core design and ongoing administration of the system.
% DISAPPEARANCE_RATIONALE: If the Bretton Woods system (as interpreted by this reading) vanished overnight, the global monetary order would undergo a fundamental reorganization. Non-reserve currency states would gain monetary autonomy but face increased exchange rate volatility, while the U.S. would lose its 'exorbitant privilege,' leading to significant shifts in global financial flows and power dynamics.
% FOUNDING_PROBLEM: The interwar period was characterized by competitive devaluations, protectionism, and unstable exchange rates, which hindered international trade and contributed to economic crises.
% FOUNDING_PROBLEM_CORROBORATION: Historians and economists generally agree on the problem of interwar monetary instability. However, this reading emphasizes that the solution created new asymmetries, which are still debated by critical international political economy scholars outside the direct beneficiaries of the system.
narrative_ontology:disappearance_verdict(bretton_woods_treaty_substrate__sovereignty_defense, world_rearranges).
narrative_ontology:founding_problem_status(bretton_woods_treaty_substrate__sovereignty_defense, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bretton_woods_treaty_substrate__sovereignty_defense, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(bretton_woods_treaty_substrate__sovereignty_defense, 'none', 1).
narrative_ontology:epsilon_provenance(bretton_woods_treaty_substrate__sovereignty_defense, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(bretton_woods_treaty_substrate__sovereignty_defense_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(bretton_woods_treaty_substrate__sovereignty_defense, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(bretton_woods_treaty_substrate__sovereignty_defense_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) reflects the cost to non-reserve currency states of maintaining fixed exchange rates and accumulating dollar reserves, which limited their policy space. Suppression (0.75) is high due to the IMF's enforcement mechanisms and the lack of viable alternatives for participation in the global economy. Theater ratio (0.20) is low, as the system's functions were largely real, but the narrative of universal benefit increasingly masked the asymmetric costs. The claimed type is 'tangled_rope' because it provided a coordination function (exchange rate stability) but with significant asymmetric extraction.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the U.S., Bretton Woods was a successful coordination mechanism for global stability. From the perspective of non-reserve currency states, it was a system that constrained their sovereignty and extracted resources. This reading highlights that divergence, showing how the same structure could be a 'rope' for some and a 'snare' for others.
 *
 * DIRECTIONALITY LOGIC:
 *   The United States and other reserve currency issuers are clear beneficiaries, enjoying greater monetary flexibility and seigniorage benefits. Non-reserve currency states, particularly developing nations, are victims, bearing the costs of external discipline and limited policy autonomy. The IMF acts as an agenda-setter and enforcer, maintaining the system that benefits the core powers.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling the system as pure coordination by highlighting the structural asymmetries and the active enforcement required to maintain them. It shows how the initial coordination mandate (preventing competitive devaluations) became intertwined with a mechanism for transferring monetary sovereignty, leading to a 'tangled rope' classification rather than a 'rope' or 'snare' (which would imply no coordination function).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    exorbitant_privilege_quantification,
    'What is the precise economic value of the ''exorbitant privilege'' accrued by the United States under Bretton Woods, and how does it compare to the costs borne by non-reserve currency states?',
    'Detailed econometric studies comparing counterfactual scenarios (e.g., a truly multilateral reserve system) with the actual Bretton Woods outcomes, accounting for trade, investment, and monetary policy impacts.',
    'A clear quantification would strengthen the claim of asymmetric extraction and could inform reparations or new institutional designs. If the privilege is found to be negligible, it would weaken this reading''s core claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exorbitant_privilege_quantification, empirical, 'Quantifying the asymmetric benefits and costs of the Bretton Woods system.').

omega_variable(
    gold_anchor_function_ambiguity,
    'Was the gold anchor primarily a genuine stabilizer for global monetary value, or did it function as a structural mechanism to enforce dollar hegemony and external discipline?',
    'Historical analysis of policy debates and economic outcomes, focusing on instances where the gold convertibility constraint was used to pressure non-U.S. states versus instances where it genuinely stabilized the system for all.',
    'If primarily a stabilizer, the ''snare'' aspect of the gold anchor is overstated. If primarily a disciplinary tool, it reinforces the ''tangled rope'' classification and the extraction narrative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gold_anchor_function_ambiguity, conceptual, 'Ambiguity of the gold anchor''s primary function within Bretton Woods.').

omega_variable(
    kernel_reading_sovereignty_defense,
    'This constraint is one reading of the ''bretton_woods_treaty_substrate'' kernel. What structural elements would change if a sibling reading, such as ''keynesian_embedded_liberalism'' or ''neoliberal_convertibility'', were adopted?',
    'Comparative analysis of the core axioms and beneficiary/victim sets across the different readings, identifying specific points of divergence in the interpretation of the treaty''s intent and effects.',
    'Adopting the ''keynesian_embedded_liberalism'' reading would shift the focus to capital controls as a coordination mechanism, potentially reducing the perceived extraction from national policy space. Adopting the ''neoliberal_convertibility'' reading would emphasize the role of free capital markets, potentially reclassifying capital controls as a ''snare'' for investors.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_sovereignty_defense, conceptual, 'This constraint is the ''sovereignty_defense'' reading of the Bretton Woods kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bretton_woods_treaty_substrate__sovereignty_defense, 1944, 1971).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bret_tr_t1944, bretton_woods_treaty_substrate__sovereignty_defense, theater_ratio, 1944, 0.1).
narrative_ontology:measurement(bret_tr_t1950, bretton_woods_treaty_substrate__sovereignty_defense, theater_ratio, 1950, 0.12).
narrative_ontology:measurement(bret_tr_t1958, bretton_woods_treaty_substrate__sovereignty_defense, theater_ratio, 1958, 0.15).
narrative_ontology:measurement(bret_tr_t1965, bretton_woods_treaty_substrate__sovereignty_defense, theater_ratio, 1965, 0.18).
narrative_ontology:measurement(bret_tr_t1971, bretton_woods_treaty_substrate__sovereignty_defense, theater_ratio, 1971, 0.2).

% Extraction over time
narrative_ontology:measurement(bret_be_t1944, bretton_woods_treaty_substrate__sovereignty_defense, base_extractiveness, 1944, 0.5).
narrative_ontology:measurement(bret_be_t1950, bretton_woods_treaty_substrate__sovereignty_defense, base_extractiveness, 1950, 0.55).
narrative_ontology:measurement(bret_be_t1958, bretton_woods_treaty_substrate__sovereignty_defense, base_extractiveness, 1958, 0.6).
narrative_ontology:measurement(bret_be_t1965, bretton_woods_treaty_substrate__sovereignty_defense, base_extractiveness, 1965, 0.63).
narrative_ontology:measurement(bret_be_t1971, bretton_woods_treaty_substrate__sovereignty_defense, base_extractiveness, 1971, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(bret_su_t1944, bretton_woods_treaty_substrate__sovereignty_defense, suppression_requirement, 1944, 0.6).
narrative_ontology:measurement(bret_su_t1950, bretton_woods_treaty_substrate__sovereignty_defense, suppression_requirement, 1950, 0.65).
narrative_ontology:measurement(bret_su_t1958, bretton_woods_treaty_substrate__sovereignty_defense, suppression_requirement, 1958, 0.7).
narrative_ontology:measurement(bret_su_t1965, bretton_woods_treaty_substrate__sovereignty_defense, suppression_requirement, 1965, 0.73).
narrative_ontology:measurement(bret_su_t1971, bretton_woods_treaty_substrate__sovereignty_defense, suppression_requirement, 1971, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bretton_woods_treaty_substrate__sovereignty_defense, global_infrastructure).
narrative_ontology:affects_constraint(bretton_woods_treaty_substrate__sovereignty_defense, keynesian_embedded_liberalism).
narrative_ontology:affects_constraint(bretton_woods_treaty_substrate__sovereignty_defense, neoliberal_convertibility).

% DUAL FORMULATION NOTE:
% This constraint is the 'sovereignty_defense' reading of the Bretton Woods treaty substrate. It highlights the asymmetric power dynamics and the transfer of monetary sovereignty, contrasting with the 'keynesian_embedded_liberalism' reading (which emphasizes domestic policy space protection) and the 'neoliberal_convertibility' reading (which focuses on free capital markets).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
