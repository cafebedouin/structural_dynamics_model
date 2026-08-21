% ============================================================================
% CONSTRAINT STORY: bretton_woods_treaty_substrate__keynesian_embedded_liberalism
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_bretton_woods_treaty_substrate__keynesian_embedded_liberalism, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: bretton_woods_treaty_substrate__keynesian_embedded_liberalism
 *   human_readable: Bretton Woods: Keynesian Embedded Liberalism Reading (Capital Controls for Policy Space)
 *   domain: international_political_economy/monetary_history/institutional_design
 *
 * SUMMARY:
 *   This constraint story represents the 'Keynesian Embedded Liberalism'
 *   reading of the Bretton Woods system, where the primary function was to
 *   constrain international capital flows to safeguard domestic policy
 *   autonomy for full employment and welfare states. It views capital
 *   controls as legitimate tools for national economic management, rather
 *   than infringements on market freedom. The system is classified as a
 *   Tangled Rope because it provided genuine coordination for national
 *   governments and domestic labor markets, but simultaneously extracted from
 *   international finance capital through restrictions on mobility, requiring
 *   active enforcement.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, 0.45).
domain_priors:suppression_score(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, 0.6).
domain_priors:theater_ratio(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, extractiveness, 0.45).
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, tangled_rope).
narrative_ontology:human_readable(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, "Bretton Woods: Keynesian Embedded Liberalism Reading (Capital Controls for Policy Space)").
narrative_ontology:topic_domain(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, "international_political_economy/monetary_history/institutional_design").

domain_priors:requires_active_enforcement(bretton_woods_treaty_substrate__keynesian_embedded_liberalism).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, '87fbcbb0-0374-4285-b4e9-37768f3a7017').
narrative_ontology:cs_kernel_codification('87fbcbb0-0374-4285-b4e9-37768f3a7017', formalized).
narrative_ontology:cs_authority_grounding('87fbcbb0-0374-4285-b4e9-37768f3a7017', lineage).
narrative_ontology:cs_interpretation_layer_present('87fbcbb0-0374-4285-b4e9-37768f3a7017').
narrative_ontology:cs_reading_relation('87fbcbb0-0374-4285-b4e9-37768f3a7017', bretton_woods_treaty_substrate__neoliberal_convertibility, forecloses).
narrative_ontology:cs_reading_relation('87fbcbb0-0374-4285-b4e9-37768f3a7017', bretton_woods_treaty_substrate__sovereignty_defense, coexists_with).
narrative_ontology:cs_axiom('87fbcbb0-0374-4285-b4e9-37768f3a7017', foundational, capital_controls_legitimate_policy_tool).
narrative_ontology:cs_axiom_status(capital_controls_legitimate_policy_tool, holdable).
narrative_ontology:cs_axiom_grounding('87fbcbb0-0374-4285-b4e9-37768f3a7017', capital_controls_legitimate_policy_tool, conventional).
narrative_ontology:cs_axiom('87fbcbb0-0374-4285-b4e9-37768f3a7017', foundational, domestic_policy_space_priority).
narrative_ontology:cs_axiom_status(domestic_policy_space_priority, holdable).
narrative_ontology:cs_axiom_grounding('87fbcbb0-0374-4285-b4e9-37768f3a7017', domestic_policy_space_priority, deontological).
narrative_ontology:cs_reference_frame('87fbcbb0-0374-4285-b4e9-37768f3a7017', post_war_embedded_liberal_consensus).
narrative_ontology:cs_drift_state('87fbcbb0-0374-4285-b4e9-37768f3a7017', post_nixon_shock_era, gap(codification_collapse, severe, true)).
narrative_ontology:cs_created_at('87fbcbb0-0374-4285-b4e9-37768f3a7017', '').
narrative_ontology:cs_kernel_id(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, bretton_woods_treaty_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, national_governments).
narrative_ontology:constraint_beneficiary(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, domestic_labor_markets).
narrative_ontology:constraint_victim(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, international_finance_capital).
narrative_ontology:constraint_victim(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, speculative_investors).
narrative_ontology:constraint_vindicates(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, keynesian_macroeconomic_management).
narrative_ontology:constraint_vindicates(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, embedded_liberalism_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefited from the ability to implement independent monetary and fiscal policies, shielded from speculative capital flows. They actively enforced capital controls and managed exchange rates to prioritize domestic employment and welfare goals.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, national_governments, agenda_setter,
    institutional, generational, constrained, national).

% Benefited from stable domestic economic conditions and full employment policies, which were made possible by the insulation from international financial volatility. Their welfare was prioritized over capital mobility.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, domestic_labor_markets, beneficiary,
    organized, biographical, constrained, national).

% Subject to capital controls and fixed exchange rates, limiting their ability to move funds freely across borders for speculative gains. This reduced their profit opportunities and influence over national economic policies.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, international_finance_capital, payer,
    powerful, immediate, constrained, global).

% Directly constrained by regulations preventing rapid, large-scale cross-border capital movements. Their investment strategies were limited to long-term, productive capital flows rather than short-term arbitrage.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, speculative_investors, payer,
    moderate, immediate, trapped, global).

% Administered the rules of the Bretton Woods system, including overseeing exchange rate parities and providing short-term liquidity to countries facing balance of payments difficulties, thereby supporting the system of capital controls.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, international_monetary_fund, agenda_setter,
    institutional, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinated international monetary policy to prevent competitive devaluations and provide stability for trade, while allowing national governments to pursue domestic full employment policies without external financial constraints.
% TRANSFER_FUNCTION: Transferred the burden of adjustment from domestic economies (labor, industry) to international capital, by restricting capital mobility and speculative flows, thereby protecting national policy space.
% ABSENT_VOICES: Advocates for unrestricted capital mobility and financial liberalization were largely excluded from the initial design and early operation of the system, as their views were considered secondary to the goals of domestic stability and full employment.
% DISAPPEARANCE_RATIONALE: If the Bretton Woods system (as interpreted by embedded liberalism) had vanished overnight, national governments would have immediately faced renewed pressure from international capital, leading to a loss of domestic policy autonomy, increased financial volatility, and a shift towards prioritizing capital market demands over domestic welfare.
% FOUNDING_PROBLEM: The interwar period was characterized by competitive devaluations, beggar-thy-neighbor policies, and financial instability, which undermined global trade and contributed to economic depression.
% FOUNDING_PROBLEM_CORROBORATION: Historians and economists widely corroborate the existence and severity of the interwar problems. However, the 'dead' status is attested by neoliberal economists and policymakers who argue that the problems were solved by the subsequent liberalization of capital markets, while Keynesian scholars contend that the problems merely re-emerged in different forms after Bretton Woods' collapse.
narrative_ontology:disappearance_verdict(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, world_rearranges).
narrative_ontology:founding_problem_status(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, 'none', 1).
narrative_ontology:epsilon_provenance(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(bretton_woods_treaty_substrate__keynesian_embedded_liberalism_tests).
:- end_tests(bretton_woods_treaty_substrate__keynesian_embedded_liberalism_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.45) because while capital mobility was restricted, the system also facilitated stable trade and growth, from which capital indirectly benefited. Suppression is moderate (0.6) due to the active enforcement of capital controls by national governments and the IMF. Theater ratio is low (0.1) as the system's functions were largely genuine and effective during its operational period. The slight increase in extractiveness and suppression over time reflects the growing pressure from international capital and the increasing effort required to maintain the controls before the system's eventual collapse.
 *
 * PERSPECTIVAL GAP:
 *   National governments and domestic labor markets experienced this as a beneficial coordination mechanism, enabling policy space and stability. International finance capital and speculative investors, however, experienced it as an extractive constraint, limiting their profit opportunities and freedom of movement. The engine's per-seat classification would reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   National governments and domestic labor markets are beneficiaries (low d) as the constraint directly enabled their policy goals and welfare. International finance capital and speculative investors are victims (high d) as their activities were directly curtailed and profits reduced. The IMF, as an agenda-setter, facilitated the system's operation, aligning with the beneficiaries.
 *
 * MANDATROPHY ANALYSIS:
 *   The 'founding problem' of interwar instability was largely addressed by Bretton Woods. However, the 'founding_problem_status' is 'dead' from a neoliberal perspective, which views the system's constraints as obsolete. This divergence highlights how the classification prevents mislabeling: what was once a functional (though extractive) Tangled Rope from the Keynesian perspective became a 'dead' constraint from a later, dominant perspective, leading to its eventual dismantling. The system's persistence was tied to the political will of national governments to maintain capital controls, which eroded over time.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    capital_control_necessity,
    'To what extent were capital controls genuinely necessary for domestic policy autonomy, versus being an overreach that stifled efficient capital allocation?',
    'Comparative analysis of economic performance and policy space in countries with and without capital controls under similar external conditions, or counterfactual modeling of alternative international monetary systems.',
    'If capital controls were found to be largely unnecessary, the extractiveness from international finance would be re-evaluated as less justified, potentially shifting the classification towards a Snare. If highly necessary, the coordination function would be strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capital_control_necessity, empirical, 'Ambiguity regarding the functional necessity of capital controls for the stated coordination goals.').

omega_variable(
    reading_framing_impact,
    'Is this constraint primarily about ''embedded liberalism'' (protecting domestic policy space) or ''sovereignty defense'' (preserving national monetary autonomy)?',
    'Analysis of primary source documents and policy debates from the period, focusing on the explicit justifications and priorities articulated by key architects and policymakers of the Bretton Woods system.',
    'If the ''sovereignty_defense'' framing is found to be more dominant, the beneficiary structure might shift to emphasize national states'' self-interest over broader welfare goals, potentially altering the perceived coordination function and the balance of extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_framing_impact, conceptual, 'Ambiguity in the primary normative framing of the Bretton Woods system''s purpose.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, 1944, 1971).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bret_tr_t1944, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, theater_ratio, 1944, 0.05).
narrative_ontology:measurement(bret_tr_t1950, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, theater_ratio, 1950, 0.08).
narrative_ontology:measurement(bret_tr_t1958, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, theater_ratio, 1958, 0.1).
narrative_ontology:measurement(bret_tr_t1965, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, theater_ratio, 1965, 0.12).
narrative_ontology:measurement(bret_tr_t1971, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, theater_ratio, 1971, 0.1).

% Extraction over time
narrative_ontology:measurement(bret_be_t1944, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, base_extractiveness, 1944, 0.4).
narrative_ontology:measurement(bret_be_t1950, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, base_extractiveness, 1950, 0.42).
narrative_ontology:measurement(bret_be_t1958, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, base_extractiveness, 1958, 0.45).
narrative_ontology:measurement(bret_be_t1965, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, base_extractiveness, 1965, 0.48).
narrative_ontology:measurement(bret_be_t1971, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, base_extractiveness, 1971, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(bret_su_t1944, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, suppression_requirement, 1944, 0.55).
narrative_ontology:measurement(bret_su_t1950, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, suppression_requirement, 1950, 0.58).
narrative_ontology:measurement(bret_su_t1958, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, suppression_requirement, 1958, 0.6).
narrative_ontology:measurement(bret_su_t1965, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, suppression_requirement, 1965, 0.62).
narrative_ontology:measurement(bret_su_t1971, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, suppression_requirement, 1971, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, 0.1).
narrative_ontology:affects_constraint(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, neoliberal_convertibility).
narrative_ontology:affects_constraint(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, sovereignty_defense).
narrative_ontology:affects_constraint(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, post_bretton_woods_floating_exchange_rates).
narrative_ontology:affects_constraint(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, washington_consensus_fiscal_discipline).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the Bretton Woods treaty substrate. Its structural properties and classification differ significantly from the 'neoliberal_convertibility' and 'sovereignty_defense' readings, which emphasize different aspects of the original agreement and its subsequent evolution.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
