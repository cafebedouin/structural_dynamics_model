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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: bretton_woods_treaty_substrate__keynesian_embedded_liberalism
 *   human_readable: Bretton Woods: Keynesian Embedded Liberalism Reading
 *   domain: international_political_economy/monetary_history/institutional_design
 *
 * SUMMARY:
 *   This constraint represents the 'Keynesian Embedded Liberalism' reading of
 *   the Bretton Woods system, where international capital flows were
 *   constrained to allow national governments to pursue full employment and
 *   welfare state policies without external monetary discipline. Capital
 *   controls were a central feature, designed to protect domestic policy
 *   space from the volatility of global finance. This reading views these
 *   controls as a necessary coordination mechanism, albeit one that extracted
 *   from international financial actors.
 *
 * KEY AGENTS:
 *   - national_governments: Primary beneficiary (institutional/mobile) – gained policy autonomy.
 *   - international_finance_capital: Primary victim (powerful/constrained) – faced restrictions on movement.
 *   - speculative_investors: Victim (moderate/constrained) – limited arbitrage opportunities.
 *   - domestic_labor_markets: Beneficiary (organized/biographical) – protected from external shocks.
 *   - international_monetary_fund: Agenda setter (institutional/analytical) – administered the system.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, 0.4).
domain_priors:suppression_score(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, 0.6).
domain_priors:theater_ratio(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, extractiveness, 0.4).
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, tangled_rope).
narrative_ontology:human_readable(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, "Bretton Woods: Keynesian Embedded Liberalism Reading").
narrative_ontology:topic_domain(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, "international_political_economy/monetary_history/institutional_design").

domain_priors:requires_active_enforcement(bretton_woods_treaty_substrate__keynesian_embedded_liberalism).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, 'd742e924-c91f-4761-bfb8-57def5aef64e').
narrative_ontology:cs_kernel_codification('d742e924-c91f-4761-bfb8-57def5aef64e', formalized).
narrative_ontology:cs_authority_grounding('d742e924-c91f-4761-bfb8-57def5aef64e', lineage).
narrative_ontology:cs_interpretation_layer_present('d742e924-c91f-4761-bfb8-57def5aef64e').
narrative_ontology:cs_reading_relation('d742e924-c91f-4761-bfb8-57def5aef64e', bretton_woods_treaty_substrate__neoliberal_convertibility, forecloses).
narrative_ontology:cs_reading_relation('d742e924-c91f-4761-bfb8-57def5aef64e', bretton_woods_treaty_substrate__sovereignty_defense, coexists_with).
narrative_ontology:cs_axiom('d742e924-c91f-4761-bfb8-57def5aef64e', foundational, capital_controls_legitimate_for_policy_space).
narrative_ontology:cs_axiom_status(capital_controls_legitimate_for_policy_space, holdable).
narrative_ontology:cs_axiom_grounding('d742e924-c91f-4761-bfb8-57def5aef64e', capital_controls_legitimate_for_policy_space, conventional).
narrative_ontology:cs_axiom('d742e924-c91f-4761-bfb8-57def5aef64e', foundational, domestic_policy_autonomy_priority).
narrative_ontology:cs_axiom_status(domestic_policy_autonomy_priority, holdable).
narrative_ontology:cs_axiom_grounding('d742e924-c91f-4761-bfb8-57def5aef64e', domestic_policy_autonomy_priority, instrumental).
narrative_ontology:cs_reference_frame('d742e924-c91f-4761-bfb8-57def5aef64e', post_war_embedded_liberal_consensus).
narrative_ontology:cs_drift_state('d742e924-c91f-4761-bfb8-57def5aef64e', post_1971_collapse, gap(codification_collapse, severe, true)).
narrative_ontology:cs_created_at('d742e924-c91f-4761-bfb8-57def5aef64e', '').
narrative_ontology:cs_kernel_id(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, bretton_woods_treaty_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, national_governments).
narrative_ontology:constraint_beneficiary(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, domestic_labor_markets).
narrative_ontology:constraint_victim(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, international_finance_capital).
narrative_ontology:constraint_victim(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, speculative_investors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gained significant policy autonomy to manage domestic economies, pursue full employment, and build welfare states without being constrained by speculative capital flows or external monetary pressures. They actively supported and enforced capital controls.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, national_governments, beneficiary,
    institutional, generational, mobile, national).

% Faced restrictions on cross-border movement of funds, limiting arbitrage opportunities and the ability to pressure national governments through capital flight. They bore the direct costs of capital controls and sought ways to circumvent them.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, international_finance_capital, payer,
    powerful, biographical, constrained, global).

% Their ability to profit from short-term currency movements and interest rate differentials was significantly curtailed by capital controls. They were direct targets of the constraint's suppressive mechanisms.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, speculative_investors, payer,
    moderate, immediate, constrained, global).

% Benefited from stable domestic economic conditions, lower unemployment, and the ability of governments to implement counter-cyclical policies, shielded from external financial shocks. Their well-being was directly tied to the policy space created by the constraint.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, domestic_labor_markets, beneficiary,
    organized, generational, identity_locked, national).

% Administered the Bretton Woods system, including overseeing exchange rate parities and providing short-term liquidity. While a beneficiary of the system's stability, its primary role was to set and enforce the rules, including the legitimacy of capital controls for current account transactions.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, international_monetary_fund, agenda_setter,
    institutional, generational, analytical, global).

% Advocated for free capital mobility and minimal government intervention, viewing capital controls as economically inefficient and an infringement on individual liberty. Their views were largely marginalized during the Bretton Woods era but gained prominence later.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, neoliberal_economists, excluded,
    powerful, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To coordinate international monetary stability and provide national governments with policy autonomy by managing exchange rates and allowing capital controls, preventing a return to the competitive devaluations and capital flight of the interwar period.
% TRANSFER_FUNCTION: Transferred the ability to freely move capital across borders from international financial actors to national governments, in exchange for domestic policy space and stability. This involved a transfer of potential profits from speculative finance to domestic economic stability.
% ABSENT_VOICES: Neoliberal economists and advocates for unrestricted capital mobility were largely excluded from the core design and early operation of the Bretton Woods system. They would have argued against capital controls as inefficient and freedom-restricting, advocating for market-led allocation of capital.
% DISAPPEARANCE_RATIONALE: If the Bretton Woods system (under this reading) had never existed, national governments would have faced much greater external constraints on their domestic policy choices, leading to potentially more volatile economies and less robust welfare states. International finance would have operated with fewer restrictions, but global monetary stability would have been harder to achieve.
% FOUNDING_PROBLEM: The interwar period was characterized by competitive devaluations, protectionism, and volatile capital flows that undermined domestic economic stability and international trade, contributing to global depression and conflict.
% FOUNDING_PROBLEM_CORROBORATION: Historians of economic policy, political scientists studying international relations, and many contemporary economists (especially those advocating for 'macroprudential' capital controls) corroborate that the problems of financial instability and the need for policy space remain relevant, even if the specific Bretton Woods solutions are no longer in place. The IMF itself, while having shifted its stance, acknowledges the historical context.
narrative_ontology:disappearance_verdict(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, world_rearranges).
narrative_ontology:founding_problem_status(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, 'none', 1).

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
 *   The extractiveness (0.4) is moderate, reflecting the cost imposed on international capital, but balanced by the coordination benefits for national policy. Suppression (0.6) is significant, as capital controls required active enforcement to prevent circumvention. Theater ratio (0.1) is low, as the system was genuinely functional in its stated goals during its operational period. The metrics reflect the period of the Bretton Woods system's active operation (1944-1971) under this specific interpretation.
 *
 * PERSPECTIVAL GAP:
 *   National governments and domestic labor markets experienced this as a beneficial coordination mechanism, enabling stability and growth. International finance and speculative investors experienced it as a restrictive, extractive regime limiting their freedom and profit opportunities. The IMF, as agenda setter, viewed it as a necessary framework for global monetary stability.
 *
 * DIRECTIONALITY LOGIC:
 *   National governments and domestic labor markets are beneficiaries (d=0.0-0.2) as the constraint subsidized their policy autonomy and stability. International finance capital and speculative investors are victims (d=0.8-1.0) as the constraint directly restricted their operations and extracted from their potential gains. The IMF, while administering the system, also benefited from its stability and legitimacy (d=0.1-0.3).
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling the capital controls as pure extraction by acknowledging their genuine coordination function for domestic policy space. The 'mandate' was to balance international stability with national autonomy. The system's eventual collapse (post-1971) suggests a mandatrophy where the founding problem (unfettered capital flows disrupting domestic policy) was either no longer perceived as critical by powerful actors, or the costs of enforcement became too high relative to perceived benefits, leading to a shift towards the 'neoliberal_convertibility' reading.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_keynesian_embedded_liberalism,
    'Is this constraint a genuine coordination mechanism for domestic policy autonomy, or an extractive mechanism against international capital?',
    'Analysis of capital flow volatility and domestic policy effectiveness in periods with and without capital controls.',
    'If primarily coordination, the constraint is a Rope; if primarily extractive, it leans towards Snare. This reading emphasizes the coordination function for domestic policy space.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_keynesian_embedded_liberalism, conceptual, 'This constraint is the ''Keynesian Embedded Liberalism'' reading of the Bretton Woods kernel, emphasizing capital controls for domestic policy space.').

omega_variable(
    capital_control_legitimacy,
    'Are capital controls a legitimate tool for national economic management, or an illegitimate interference with free markets?',
    'International legal and economic consensus shifts over time, and the outcomes of national policy experiments.',
    'If capital controls are seen as illegitimate, the constraint''s suppression of international finance is reclassified as pure extraction; if legitimate, it is a necessary coordination cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capital_control_legitimacy, preference, 'Ambiguity regarding the normative legitimacy of capital controls.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, 1944, 1971).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bret_tr_t0, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, theater_ratio, 0, 0.05).
narrative_ontology:measurement(bret_tr_t10, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, theater_ratio, 10, 0.1).
narrative_ontology:measurement(bret_tr_t20, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, theater_ratio, 20, 0.12).
narrative_ontology:measurement(bret_tr_t30, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, theater_ratio, 30, 0.1).

% Extraction over time
narrative_ontology:measurement(bret_be_t0, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(bret_be_t10, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, base_extractiveness, 10, 0.4).
narrative_ontology:measurement(bret_be_t20, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, base_extractiveness, 20, 0.45).
narrative_ontology:measurement(bret_be_t30, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, base_extractiveness, 30, 0.4).

% Suppression requirement over time
narrative_ontology:measurement(bret_su_t0, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(bret_su_t10, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, suppression_requirement, 10, 0.6).
narrative_ontology:measurement(bret_su_t20, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, suppression_requirement, 20, 0.65).
narrative_ontology:measurement(bret_su_t30, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, suppression_requirement, 30, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, 0.1).
narrative_ontology:affects_constraint(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, bretton_woods_treaty_substrate__neoliberal_convertibility).
narrative_ontology:affects_constraint(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, bretton_woods_treaty_substrate__sovereignty_defense).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'Bretton Woods Treaty Substrate' kernel. Its structural delta (international finance as victim, national governments as beneficiaries, capital controls as legitimate) differs significantly from the 'neoliberal_convertibility' and 'sovereignty_defense' readings, which emphasize different beneficiaries, victims, and legitimacy claims regarding capital flows and government intervention.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
