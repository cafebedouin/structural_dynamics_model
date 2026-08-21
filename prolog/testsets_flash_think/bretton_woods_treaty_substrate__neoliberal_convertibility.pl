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
 *   constraint_id: bretton_woods_treaty_substrate__neoliberal_convertibility
 *   human_readable: Bretton Woods Neoliberal Convertibility Regime
 *   domain: international_political_economy/monetary_history/institutional_design
 *
 * SUMMARY:
 *   This constraint story instantiates the 'neoliberal convertibility'
 *   reading of the Bretton Woods treaty substrate. From this perspective,
 *   Bretton Woods, and its subsequent evolution, primarily established
 *   constraints on government intervention to enable free capital markets.
 *   This reading emphasizes the benefits of capital mobility and market
 *   discipline, viewing national policy autonomy (especially capital
 *   controls) as an impediment to global efficiency. The classification as a
 *   Tangled Rope reflects the genuine coordination function for international
 *   finance, coupled with significant extraction of national policy space and
 *   domestic welfare.
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
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__neoliberal_convertibility, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__neoliberal_convertibility, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bretton_woods_treaty_substrate__neoliberal_convertibility, tangled_rope).
narrative_ontology:human_readable(bretton_woods_treaty_substrate__neoliberal_convertibility, "Bretton Woods Neoliberal Convertibility Regime").
narrative_ontology:topic_domain(bretton_woods_treaty_substrate__neoliberal_convertibility, "international_political_economy/monetary_history/institutional_design").

domain_priors:requires_active_enforcement(bretton_woods_treaty_substrate__neoliberal_convertibility).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bretton_woods_treaty_substrate__neoliberal_convertibility, '238b1e82-d7d4-41b9-acc9-af2eb3fe981e').
narrative_ontology:cs_kernel_codification('238b1e82-d7d4-41b9-acc9-af2eb3fe981e', formalized).
narrative_ontology:cs_authority_grounding('238b1e82-d7d4-41b9-acc9-af2eb3fe981e', extraction).
narrative_ontology:cs_interpretation_layer_present('238b1e82-d7d4-41b9-acc9-af2eb3fe981e').
narrative_ontology:cs_reading_relation('238b1e82-d7d4-41b9-acc9-af2eb3fe981e', bretton_woods_treaty_substrate__keynesian_embedded_liberalism, forecloses).
narrative_ontology:cs_reading_relation('238b1e82-d7d4-41b9-acc9-af2eb3fe981e', bretton_woods_treaty_substrate__sovereignty_defense, forecloses).
narrative_ontology:cs_axiom('238b1e82-d7d4-41b9-acc9-af2eb3fe981e', foundational, unrestricted_capital_flows_optimize_allocation).
narrative_ontology:cs_axiom_status(unrestricted_capital_flows_optimize_allocation, holdable).
narrative_ontology:cs_axiom_grounding('238b1e82-d7d4-41b9-acc9-af2eb3fe981e', unrestricted_capital_flows_optimize_allocation, empirically_contingent).
narrative_ontology:cs_axiom('238b1e82-d7d4-41b9-acc9-af2eb3fe981e', foundational, government_intervention_distorts_markets).
narrative_ontology:cs_axiom_status(government_intervention_distorts_markets, holdable).
narrative_ontology:cs_axiom_grounding('238b1e82-d7d4-41b9-acc9-af2eb3fe981e', government_intervention_distorts_markets, empirically_contingent).
narrative_ontology:cs_reference_frame('238b1e82-d7d4-41b9-acc9-af2eb3fe981e', free_capital_flow_discipline).
narrative_ontology:cs_drift_state('238b1e82-d7d4-41b9-acc9-af2eb3fe981e', contemporary_globalization_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('238b1e82-d7d4-41b9-acc9-af2eb3fe981e', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(bretton_woods_treaty_substrate__neoliberal_convertibility, bretton_woods_treaty_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bretton_woods_treaty_substrate__neoliberal_convertibility, international_financial_institutions).
narrative_ontology:constraint_beneficiary(bretton_woods_treaty_substrate__neoliberal_convertibility, multinational_corporations).
narrative_ontology:constraint_beneficiary(bretton_woods_treaty_substrate__neoliberal_convertibility, international_investors).
narrative_ontology:constraint_victim(bretton_woods_treaty_substrate__neoliberal_convertibility, national_governments_seeking_autonomy).
narrative_ontology:constraint_victim(bretton_woods_treaty_substrate__neoliberal_convertibility, domestic_labor_movements).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Institutions like the IMF and World Bank actively promoted and enforced policies of capital account liberalization and limited government intervention, framing them as essential for global financial stability and growth. They benefit from their central role in this system.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__neoliberal_convertibility, international_financial_institutions, agenda_setter,
    institutional, generational, arbitrage, global).

% Benefit significantly from the free movement of capital, allowing them to optimize production, investment, and tax strategies across borders without significant regulatory hurdles or capital controls.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__neoliberal_convertibility, multinational_corporations, beneficiary,
    powerful, biographical, arbitrage, global).

% Directly benefit from the ability to move capital freely across national borders, seeking the highest returns and diversifying risk, without facing restrictions on entry or exit.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__neoliberal_convertibility, international_investors, beneficiary,
    powerful, immediate, arbitrage, global).

% Bear the cost of reduced policy autonomy, particularly in managing exchange rates, interest rates, and capital flows to achieve domestic economic goals like full employment or industrial development. Deviating from convertibility norms incurs market penalties.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__neoliberal_convertibility, national_governments_seeking_autonomy, payer,
    institutional, biographical, constrained, national).

% Experience downward pressure on wages and working conditions as capital mobility allows corporations to seek lower labor costs globally, reducing their bargaining power and making national-level policy interventions less effective.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__neoliberal_convertibility, domestic_labor_movements, payer,
    organized, biographical, trapped, national).

% Their arguments for capital controls and active government intervention to manage demand and protect domestic policy space were largely sidelined or actively opposed by the dominant neoliberal consensus, effectively excluding them from policy-making influence.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__neoliberal_convertibility, keynesian_economists, excluded,
    analytical, generational, analytical, global).

% Provided the intellectual framework and justification for the neoliberal convertibility regime, advocating for free markets, deregulation, and limited government intervention. Their ideas became foundational to the system's operation and legitimacy.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__neoliberal_convertibility, neoliberal_economists, agenda_setter,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(bretton_woods_treaty_substrate__neoliberal_convertibility, international_financial_institutions).
narrative_ontology:fixing_cost_class(bretton_woods_treaty_substrate__neoliberal_convertibility, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To establish a stable and integrated international financial system that facilitates global trade and investment by promoting free capital movement and fixed (or managed) exchange rates, thereby reducing transaction costs and uncertainty for international economic actors.
% TRANSFER_FUNCTION: Transfers significant national policy autonomy (especially over capital controls, monetary policy, and fiscal space) from national governments to the discipline of international financial markets and institutions, benefiting international capital and multinational corporations.
% ABSENT_VOICES: Advocates for strong capital controls, proponents of national industrial policy, and those prioritizing domestic employment and social welfare over international financial stability and capital mobility. Their perspectives were marginalized as the neoliberal consensus solidified.
% DISAPPEARANCE_RATIONALE: If the principles of neoliberal convertibility and limited government intervention in capital markets vanished overnight, the global financial system would undergo a profound restructuring. Capital flows would be re-regulated, national policy autonomy would increase, and the power dynamics between states and markets would fundamentally shift, leading to a very different international economic order.
% FOUNDING_PROBLEM: The post-WWII international economic landscape was characterized by instability, competitive devaluations, and fragmented trade, necessitating a new framework for global monetary cooperation and reconstruction.
% FOUNDING_PROBLEM_CORROBORATION: International financial institutions and neoliberal economists argue that the underlying problems of financial instability and the need for market discipline remain live. Critics, including post-Keynesian and development economists, contend that the original problems were largely solved, and the neoliberal convertibility regime introduced new forms of instability and inequality; their arguments are supported by historical analysis and empirical studies from outside the benefiting parties.
narrative_ontology:disappearance_verdict(bretton_woods_treaty_substrate__neoliberal_convertibility, world_rearranges).
narrative_ontology:founding_problem_status(bretton_woods_treaty_substrate__neoliberal_convertibility, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bretton_woods_treaty_substrate__neoliberal_convertibility, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
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
 *   Base extractiveness is high (0.78) because the regime systematically limits national policy choices, imposing market discipline that often conflicts with domestic social goals. Suppression is very high (0.85) due to the powerful enforcement mechanisms of international financial institutions (e.g., IMF conditionality) and the punitive reactions of global capital markets to deviations. Theater ratio is low (0.15) because the enforcement of capital mobility and market discipline was very real and effective, not merely performative. Accessibility collapse is high (0.70) as alternative policy paths (like strong capital controls) were actively discouraged and made costly. Resistance is moderate (0.60) from some national governments and social movements, but often overcome by market forces and institutional pressure. The temporal measurements reflect the increasing liberalization of capital markets and the tightening of market discipline from 1970 to 2000.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of international finance and neoliberal economists, this regime is a highly effective Rope, coordinating global capital for mutual benefit. However, from the perspective of national governments seeking policy space or domestic labor, it operates as a Snare or Tangled Rope, extracting autonomy and welfare under the guise of efficiency. The engine's computation of per-seat classifications will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   International financial institutions, multinational corporations, and international investors are clear beneficiaries, gaining from reduced friction and increased opportunities in global capital markets. National governments seeking autonomy and domestic labor movements are the primary targets/payers, losing policy tools and facing increased competitive pressures. Neoliberal economists act as agenda-setters, providing the intellectual justification, while Keynesian economists are excluded, their policy prescriptions marginalized.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this as a Tangled Rope prevents mislabeling it as a pure Rope (which would ignore the significant extraction of national policy autonomy) or a pure Snare (which would ignore the genuine, albeit asymmetrically distributed, coordination function for international capital). It captures the hybrid nature where a coordination mechanism for one set of actors simultaneously imposes costs on others, requiring active enforcement to maintain.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    original_intent_vs_neoliberal_reading,
    'To what extent did the original architects of Bretton Woods intend the degree of capital mobility and limited government intervention that characterized the neoliberal convertibility regime, versus a more ''embedded liberal'' framework?',
    'Historical analysis of primary source documents, diplomatic correspondence, and early institutional mandates, comparing them with later interpretations and policy shifts.',
    'If the original intent was significantly different, it would highlight a substantial drift in the kernel''s interpretation, potentially reclassifying the ''neoliberal convertibility'' as a Snare or Piton that co-opted the original framework. If consistent, it would strengthen the claim of a continuous, albeit evolving, coordination function.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(original_intent_vs_neoliberal_reading, conceptual, 'Ambiguity regarding the historical fidelity of the neoliberal reading to the original Bretton Woods intent.').

omega_variable(
    capital_mobility_efficiency_empirical_validity,
    'Is the empirical claim that unrestricted capital flows consistently lead to optimal global resource allocation and economic growth robust, or do they frequently contribute to financial instability and crises?',
    'Longitudinal empirical studies comparing economic performance, financial stability, and inequality in countries with varying degrees of capital account openness, controlling for other factors.',
    'If the empirical claim of efficiency is substantially weakened by evidence of instability or inequality, it would undermine a foundational axiom of this reading, potentially reclassifying the constraint as more extractive (Snare) by exposing the coordination story as cover.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capital_mobility_efficiency_empirical_validity, empirical, 'Empirical validity of the core economic claims underpinning free capital markets.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bretton_woods_treaty_substrate__neoliberal_convertibility, 1970, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bret_tr_t1970, bretton_woods_treaty_substrate__neoliberal_convertibility, theater_ratio, 1970, 0.1).
narrative_ontology:measurement(bret_tr_t1975, bretton_woods_treaty_substrate__neoliberal_convertibility, theater_ratio, 1975, 0.12).
narrative_ontology:measurement(bret_tr_t1980, bretton_woods_treaty_substrate__neoliberal_convertibility, theater_ratio, 1980, 0.14).
narrative_ontology:measurement(bret_tr_t1985, bretton_woods_treaty_substrate__neoliberal_convertibility, theater_ratio, 1985, 0.15).
narrative_ontology:measurement(bret_tr_t1990, bretton_woods_treaty_substrate__neoliberal_convertibility, theater_ratio, 1990, 0.15).
narrative_ontology:measurement(bret_tr_t1995, bretton_woods_treaty_substrate__neoliberal_convertibility, theater_ratio, 1995, 0.15).
narrative_ontology:measurement(bret_tr_t2000, bretton_woods_treaty_substrate__neoliberal_convertibility, theater_ratio, 2000, 0.15).

% Extraction over time
narrative_ontology:measurement(bret_be_t1970, bretton_woods_treaty_substrate__neoliberal_convertibility, base_extractiveness, 1970, 0.65).
narrative_ontology:measurement(bret_be_t1975, bretton_woods_treaty_substrate__neoliberal_convertibility, base_extractiveness, 1975, 0.68).
narrative_ontology:measurement(bret_be_t1980, bretton_woods_treaty_substrate__neoliberal_convertibility, base_extractiveness, 1980, 0.72).
narrative_ontology:measurement(bret_be_t1985, bretton_woods_treaty_substrate__neoliberal_convertibility, base_extractiveness, 1985, 0.75).
narrative_ontology:measurement(bret_be_t1990, bretton_woods_treaty_substrate__neoliberal_convertibility, base_extractiveness, 1990, 0.77).
narrative_ontology:measurement(bret_be_t1995, bretton_woods_treaty_substrate__neoliberal_convertibility, base_extractiveness, 1995, 0.78).
narrative_ontology:measurement(bret_be_t2000, bretton_woods_treaty_substrate__neoliberal_convertibility, base_extractiveness, 2000, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(bret_su_t1970, bretton_woods_treaty_substrate__neoliberal_convertibility, suppression_requirement, 1970, 0.7).
narrative_ontology:measurement(bret_su_t1975, bretton_woods_treaty_substrate__neoliberal_convertibility, suppression_requirement, 1975, 0.75).
narrative_ontology:measurement(bret_su_t1980, bretton_woods_treaty_substrate__neoliberal_convertibility, suppression_requirement, 1980, 0.8).
narrative_ontology:measurement(bret_su_t1985, bretton_woods_treaty_substrate__neoliberal_convertibility, suppression_requirement, 1985, 0.83).
narrative_ontology:measurement(bret_su_t1990, bretton_woods_treaty_substrate__neoliberal_convertibility, suppression_requirement, 1990, 0.85).
narrative_ontology:measurement(bret_su_t1995, bretton_woods_treaty_substrate__neoliberal_convertibility, suppression_requirement, 1995, 0.85).
narrative_ontology:measurement(bret_su_t2000, bretton_woods_treaty_substrate__neoliberal_convertibility, suppression_requirement, 2000, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bretton_woods_treaty_substrate__neoliberal_convertibility, resource_allocation).
narrative_ontology:affects_constraint(bretton_woods_treaty_substrate__neoliberal_convertibility, bretton_woods_treaty_substrate__keynesian_embedded_liberalism).
narrative_ontology:affects_constraint(bretton_woods_treaty_substrate__neoliberal_convertibility, bretton_woods_treaty_substrate__sovereignty_defense).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the Bretton Woods treaty substrate kernel, focusing on the neoliberal interpretation of capital convertibility and limited government intervention. It is structurally distinct from the Keynesian embedded liberalism and sovereignty defense readings, which emphasize different aspects of the original framework.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
