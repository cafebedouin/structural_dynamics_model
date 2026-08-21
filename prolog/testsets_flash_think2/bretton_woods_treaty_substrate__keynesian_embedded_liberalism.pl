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
 *   constraint_id: bretton_woods_treaty_substrate__keynesian_embedded_liberalism
 *   human_readable: Bretton Woods: Keynesian Embedded Liberalism Reading
 *   domain: international_political_economy/monetary_history/institutional_design
 *
 * SUMMARY:
 *   This constraint story describes the Bretton Woods system from the
 *   'Keynesian Embedded Liberalism' reading, focusing on its role in
 *   constraining international capital to protect domestic policy space. It
 *   was a post-WWII institutional design aimed at preventing the economic
 *   instability of the interwar period. The system, characterized by fixed
 *   exchange rates and capital controls, allowed national governments to
 *   pursue full employment and welfare policies, effectively extracting from
 *   the mobility and profit-making potential of international finance.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, 0.7).
domain_priors:suppression_score(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, 0.8).
domain_priors:theater_ratio(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, extractiveness, 0.7).
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, tangled_rope).
narrative_ontology:human_readable(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, "Bretton Woods: Keynesian Embedded Liberalism Reading").
narrative_ontology:topic_domain(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, "international_political_economy/monetary_history/institutional_design").

domain_priors:requires_active_enforcement(bretton_woods_treaty_substrate__keynesian_embedded_liberalism).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, '4c822538-e45d-4a1c-9e4d-820ff8080539').
narrative_ontology:cs_kernel_codification('4c822538-e45d-4a1c-9e4d-820ff8080539', formalized).
narrative_ontology:cs_authority_grounding('4c822538-e45d-4a1c-9e4d-820ff8080539', lineage).
narrative_ontology:cs_interpretation_layer_present('4c822538-e45d-4a1c-9e4d-820ff8080539').
narrative_ontology:cs_reading_relation('4c822538-e45d-4a1c-9e4d-820ff8080539', bretton_woods_treaty_substrate__neoliberal_convertibility, forecloses).
narrative_ontology:cs_reading_relation('4c822538-e45d-4a1c-9e4d-820ff8080539', bretton_woods_treaty_substrate__sovereignty_defense, coexists_with).
narrative_ontology:cs_axiom('4c822538-e45d-4a1c-9e4d-820ff8080539', foundational, capital_controls_legitimate_policy_tool).
narrative_ontology:cs_axiom_status(capital_controls_legitimate_policy_tool, holdable).
narrative_ontology:cs_axiom_grounding('4c822538-e45d-4a1c-9e4d-820ff8080539', capital_controls_legitimate_policy_tool, conventional).
narrative_ontology:cs_axiom('4c822538-e45d-4a1c-9e4d-820ff8080539', foundational, domestic_policy_autonomy_priority).
narrative_ontology:cs_axiom_status(domestic_policy_autonomy_priority, holdable).
narrative_ontology:cs_axiom_grounding('4c822538-e45d-4a1c-9e4d-820ff8080539', domestic_policy_autonomy_priority, deontological).
narrative_ontology:cs_reference_frame('4c822538-e45d-4a1c-9e4d-820ff8080539', post_war_economic_reconstruction_consensus).
narrative_ontology:cs_drift_state('4c822538-e45d-4a1c-9e4d-820ff8080539', post_nixon_shock_era, gap(axiom_overriding, severe, true)).
narrative_ontology:cs_created_at('4c822538-e45d-4a1c-9e4d-820ff8080539', '').
narrative_ontology:cs_kernel_id(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, bretton_woods_treaty_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, national_governments).
narrative_ontology:constraint_beneficiary(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, domestic_industries).
narrative_ontology:constraint_beneficiary(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, labor_unions).
narrative_ontology:constraint_victim(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, international_capital).
narrative_ontology:constraint_victim(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, multinational_corporations).
narrative_ontology:constraint_vindicates(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, keynesian_macroeconomics).
narrative_ontology:constraint_vindicates(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, embedded_liberalism_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% As primary architects and enforcers of the Bretton Woods system, national governments gained significant policy autonomy to pursue full employment and welfare state objectives, protected from speculative capital flows and external monetary discipline. Their exit options were constrained by the need to maintain international monetary stability.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, national_governments, agenda_setter,
    institutional, generational, constrained, national).

% Comprising banks, investment funds, and wealthy individuals, international capital faced significant restrictions on cross-border movement through capital controls. This limited their ability to engage in arbitrage, speculation, and to seek higher returns globally, effectively extracting from their potential profits.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, international_capital, payer,
    powerful, biographical, constrained, global).

% Benefited from stable exchange rates and protection from disruptive capital flows, which fostered a predictable environment for long-term investment and growth within national borders. They were shielded from intense international competition and capital flight.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, domestic_industries, beneficiary,
    organized, biographical, mobile, national).

% Gained from the full employment policies and social welfare programs that national governments could pursue due to protected domestic policy space. Stable economic conditions and reduced external pressures supported wage growth and worker protections.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, labor_unions, beneficiary,
    organized, biographical, constrained, national).

% Experienced difficulties in optimizing global operations due to capital controls, which complicated the movement of profits, investments, and financing across national borders. This imposed additional costs and reduced their flexibility compared to a free capital regime.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, multinational_corporations, payer,
    powerful, biographical, constrained, global).

% Established to oversee the Bretton Woods system, the IMF monitored fixed exchange rates, provided short-term liquidity, and enforced compliance with the rules, including the legitimacy of capital controls. It acted as a central administrative body for the system.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, international_monetary_fund, agenda_setter,
    institutional, generational, analytical, global).

% Advocated for free capital mobility and market-driven exchange rates, arguing that capital controls distorted markets and hindered efficiency. Their views were largely marginalized during the Bretton Woods era but gained prominence later, influencing the system's eventual dismantling.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, neoliberal_economists, excluded,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, national_governments).
narrative_ontology:fixing_cost_class(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To establish a stable international monetary system with fixed but adjustable exchange rates, prevent competitive devaluations, and allow national governments to manage their domestic economies without being dictated by international capital flows.
% TRANSFER_FUNCTION: Transferred policy autonomy and economic stability to national governments and domestic economies, by restricting the mobility and speculative opportunities of international capital and multinational corporations.
% ABSENT_VOICES: Proponents of fully free capital markets and floating exchange rates, primarily neoliberal economists and financial interests, were largely excluded from the system's design and early operation. They would have argued for the efficiency gains of capital mobility.
% DISAPPEARANCE_RATIONALE: The collapse of the Bretton Woods system in the early 1970s led to a fundamental reorganization of international finance, including the shift to floating exchange rates and increased capital mobility. This profoundly altered national policy space, demonstrating the system's foundational role.
% FOUNDING_PROBLEM: The interwar period's economic instability, characterized by competitive currency devaluations, protectionism, and disruptive capital flight, which severely hampered national economic recovery and contributed to global depression.
% FOUNDING_PROBLEM_CORROBORATION: Historians of economic thought, international relations scholars, and contemporary policy documents from the post-WWII era corroborate the severity of the interwar problems. The subsequent shift to floating rates and capital liberalization indicates the original problem was either solved or superseded by new priorities, though new forms of instability emerged.
narrative_ontology:disappearance_verdict(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, world_rearranges).
narrative_ontology:founding_problem_status(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, 'none', 1).
narrative_ontology:epsilon_provenance(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, 0.7, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(bretton_woods_treaty_substrate__keynesian_embedded_liberalism_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(bretton_woods_treaty_substrate__keynesian_embedded_liberalism_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.7) reflects the significant limitations placed on international capital's ability to move freely and generate profits from arbitrage and speculation. Suppression (0.8) is high due to the active enforcement of capital controls by national governments and the IMF. The theater ratio is low (0.1) because the system was genuinely functional and achieved its stated goals for a significant period. Accessibility collapse (0.75) was high as alternatives for free capital movement were largely closed off. Resistance (0.55) was moderate, coming from financial interests and some economic schools.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of national governments and domestic constituencies, Bretton Woods was a successful coordination mechanism that enabled post-war reconstruction and social stability. From the perspective of international capital, it was a highly extractive system that limited their freedom and profit potential. The engine's per-seat classification will reflect this divergence based on the declared structural relationships.
 *
 * DIRECTIONALITY LOGIC:
 *   National governments, domestic industries, and labor unions were the primary beneficiaries, gaining policy autonomy and economic stability (low directionality). International capital and multinational corporations were the targets, bearing the costs of restricted mobility and reduced profit opportunities (high directionality). The IMF acted as an agenda-setter, administering the system.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    capital_control_efficacy_drivers,
    'Was the effectiveness of Bretton Woods'' capital controls primarily due to their institutional design, or to the unique post-WWII geopolitical and economic context (e.g., lack of alternative financial centers, US hegemony)?',
    'Comparative historical analysis with other periods of capital controls, examining the relative impact of institutional mechanisms versus external contextual factors on their persistence and efficacy.',
    'If context was dominant, the constraint''s structural robustness is lower than it appears, suggesting it was a ''scaffold'' for a specific era rather than a durable ''tangled_rope''. If design was dominant, its ''tangled_rope'' classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capital_control_efficacy_drivers, empirical, 'Factors driving the efficacy of capital controls under Bretton Woods.').

omega_variable(
    policy_space_quantification,
    'How precisely can ''domestic policy space'' be quantified and attributed solely to the Bretton Woods system, as opposed to other domestic political and economic factors?',
    'Development of robust econometric models that isolate the impact of international monetary regimes on national policy autonomy, controlling for domestic political economy variables.',
    'If policy space gains are less attributable to Bretton Woods, the ''beneficiary'' status of national governments is weaker, potentially shifting the constraint closer to a ''snare'' for international capital without a strong coordination counter-balance.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(policy_space_quantification, empirical, 'Measurement and attribution of domestic policy space gains.').

omega_variable(
    mandatrophy_of_controls_drivers,
    'Did the capital controls become obsolete as economies recovered and integrated, or were they primarily dismantled due to an ideological shift towards neoliberalism and financial liberalization?',
    'Analysis of policy debates and economic conditions leading up to the system''s collapse, distinguishing between functional obsolescence and political/ideological choices.',
    'If obsolescence was primary, the system''s later stages might be reclassified as a ''piton''. If ideological shift was primary, it reinforces the ''tangled_rope'' classification, highlighting the contest over its extractive nature.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandatrophy_of_controls_drivers, conceptual, 'Drivers of the dismantling of Bretton Woods capital controls.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, 0, 27).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bret_tr_t0, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, theater_ratio, 0, 0.05).
narrative_ontology:measurement(bret_tr_t5, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, theater_ratio, 5, 0.07).
narrative_ontology:measurement(bret_tr_t10, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, theater_ratio, 10, 0.08).
narrative_ontology:measurement(bret_tr_t15, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, theater_ratio, 15, 0.09).
narrative_ontology:measurement(bret_tr_t20, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, theater_ratio, 20, 0.1).
narrative_ontology:measurement(bret_tr_t27, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, theater_ratio, 27, 0.12).

% Extraction over time
narrative_ontology:measurement(bret_be_t0, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(bret_be_t5, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, base_extractiveness, 5, 0.6).
narrative_ontology:measurement(bret_be_t10, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, base_extractiveness, 10, 0.65).
narrative_ontology:measurement(bret_be_t15, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, base_extractiveness, 15, 0.68).
narrative_ontology:measurement(bret_be_t20, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, base_extractiveness, 20, 0.7).
narrative_ontology:measurement(bret_be_t27, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, base_extractiveness, 27, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(bret_su_t0, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(bret_su_t5, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, suppression_requirement, 5, 0.75).
narrative_ontology:measurement(bret_su_t10, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, suppression_requirement, 10, 0.78).
narrative_ontology:measurement(bret_su_t15, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, suppression_requirement, 15, 0.8).
narrative_ontology:measurement(bret_su_t20, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, suppression_requirement, 20, 0.82).
narrative_ontology:measurement(bret_su_t27, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, suppression_requirement, 27, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, enforcement_mechanism).
narrative_ontology:affects_constraint(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, post_bretton_woods_floating_exchange_rates).
narrative_ontology:affects_constraint(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, washington_consensus_fiscal_discipline).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'bretton_woods_treaty_substrate' kernel. This reading emphasizes the system's role in constraining international capital to protect domestic policy space, contrasting with neoliberal and sovereignty-focused interpretations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
