% ============================================================================
% CONSTRAINT STORY: structural_adjustment_conditionalities__debtor_extraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-23
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_structural_adjustment_conditionalities__debtor_extraction_reading, []).

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
 *   constraint_id: structural_adjustment_conditionalities__debtor_extraction_reading
 *   human_readable: Structural Adjustment Conditionalities as Extractive Neo-Colonial Regime
 *   domain: international_political_economy/development_finance
 *
 * SUMMARY:
 *   This is the debtor_extraction_reading of the
 *   structural_adjustment_conditionalities kernel. It treats IMF and World
 *   Bank policy conditionalities not as transitional crisis-management
 *   devices but as a standing coercive extraction regime that dismantles
 *   post-colonial social contracts to secure creditor profit and corporate
 *   market access. The sibling readingsâcreditor_coordination_reading and
 *   hybrid_selectivity_readingâare separate constraints in the same family,
 *   linked by network edges. This reading authors high extractiveness and
 *   suppression because the referent is the standing arrangement under
 *   contest: the enforced policy package itself, assessed from the standpoint
 *   of the domestic populations who bear its costs. The metrics and the
 *   claimed type are authored independently; the engine will compute per-seat
 *   classifications from the structural data.
 *
 * KEY AGENTS:
 *   - imf_world_bank: Primary agenda-setter (institutional/global) â designs and enforces conditionality frameworks
 *   - private_creditor_banks: Primary beneficiary (institutional/global) â receives prioritized debt service flows protected by enforcement
 *   - transnational_corporations: Secondary beneficiary (powerful/global) â captures privatized assets and liberalized market access
 *   - debtor_state_executive: Administrative transmission agent (institutional/national, constrained exit) â implements creditor mandates under liquidity duress
 *   - domestic_populations: Primary target (powerless/national, trapped) â bears austerity costs and service dismantlement
 *   - civil_society_groups: Excluded voice (moderate/national, constrained) â opposed to austerity but kept outside negotiation rooms
 *   - post_colonial_analysts: Analytical observer (analytical/global) â documents continuity between colonial and contemporary extraction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(structural_adjustment_conditionalities__debtor_extraction_reading, 0.9).
domain_priors:suppression_score(structural_adjustment_conditionalities__debtor_extraction_reading, 0.85).
domain_priors:theater_ratio(structural_adjustment_conditionalities__debtor_extraction_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__debtor_extraction_reading, extractiveness, 0.9).
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__debtor_extraction_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__debtor_extraction_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__debtor_extraction_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(structural_adjustment_conditionalities__debtor_extraction_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(structural_adjustment_conditionalities__debtor_extraction_reading, snare).
narrative_ontology:human_readable(structural_adjustment_conditionalities__debtor_extraction_reading, "Structural Adjustment Conditionalities as Extractive Neo-Colonial Regime").
narrative_ontology:topic_domain(structural_adjustment_conditionalities__debtor_extraction_reading, "international_political_economy/development_finance").

domain_priors:requires_active_enforcement(structural_adjustment_conditionalities__debtor_extraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(structural_adjustment_conditionalities__debtor_extraction_reading, '2706b444-1c92-47f1-a4ba-8417c8a19014').
narrative_ontology:cs_kernel_codification('2706b444-1c92-47f1-a4ba-8417c8a19014', formalized).
narrative_ontology:cs_authority_grounding('2706b444-1c92-47f1-a4ba-8417c8a19014', extraction).
narrative_ontology:cs_interpretation_layer_present('2706b444-1c92-47f1-a4ba-8417c8a19014').
narrative_ontology:cs_reading_relation('2706b444-1c92-47f1-a4ba-8417c8a19014', structural_adjustment_conditionalities__creditor_coordination_reading, coexists_with).
narrative_ontology:cs_reading_relation('2706b444-1c92-47f1-a4ba-8417c8a19014', structural_adjustment_conditionalities__hybrid_selectivity_reading, coexists_with).
narrative_ontology:cs_axiom('2706b444-1c92-47f1-a4ba-8417c8a19014', foundational, popular_sovereignty_precludes_external_austerity_mandates).
narrative_ontology:cs_axiom_status(popular_sovereignty_precludes_external_austerity_mandates, holdable).
narrative_ontology:cs_axiom_grounding('2706b444-1c92-47f1-a4ba-8417c8a19014', popular_sovereignty_precludes_external_austerity_mandates, deontological).
narrative_ontology:cs_axiom('2706b444-1c92-47f1-a4ba-8417c8a19014', foundational, conditionalities_systemically_dismantle_social_contracts).
narrative_ontology:cs_axiom_status(conditionalities_systemically_dismantle_social_contracts, holdable).
narrative_ontology:cs_axiom_grounding('2706b444-1c92-47f1-a4ba-8417c8a19014', conditionalities_systemically_dismantle_social_contracts, empirically_contingent).
narrative_ontology:cs_reference_frame('2706b444-1c92-47f1-a4ba-8417c8a19014', creditor_supremacy_framework).
narrative_ontology:cs_drift_state('2706b444-1c92-47f1-a4ba-8417c8a19014', contemporary_debt_justice_movement_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('2706b444-1c92-47f1-a4ba-8417c8a19014', '').
narrative_ontology:cs_kernel_id(structural_adjustment_conditionalities__debtor_extraction_reading, structural_adjustment_conditionalities).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__debtor_extraction_reading, private_creditor_banks).
narrative_ontology:constraint_beneficiary(structural_adjustment_conditionalities__debtor_extraction_reading, transnational_corporations).
narrative_ontology:constraint_victim(structural_adjustment_conditionalities__debtor_extraction_reading, domestic_populations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers and enforces conditionalities through loan agreements, surveillance, and policy-based lending. Determines the macroeconomic policy benchmarks debtor states must meet to maintain access to concessional and non-concessional finance. Retains institutional influence regardless of individual program outcomes.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__debtor_extraction_reading, imf_world_bank, agenda_setter,
    institutional, generational, mobile, global).

% Hold sovereign debt instruments and receive prioritized debt-service streams under conditionalities that enforce austerity and foreign-exchange reserve accumulation. Benefit from IMF enforcement that reduces default risk and homogenizes debtor policy to protect creditor claims.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__debtor_extraction_reading, private_creditor_banks, beneficiary,
    institutional, generational, arbitrage, global).

% Gain access to privatized public assets, liberalized markets, and suppressed labor costs under conditionalities that mandate trade openness and deregulation. Extract rents from sectors opened by mandated reform.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__debtor_extraction_reading, transnational_corporations, beneficiary,
    powerful, generational, arbitrage, global).

% Formally signs letters of intent and implements austerity budgets, privatization schedules, and legal reforms required by creditors. Retains nominal sovereignty but operates under severe liquidity constraints and credit-rating pressure that make non-compliance structurally prohibitive. Acts as the domestic transmission mechanism for external conditionality.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__debtor_extraction_reading, debtor_state_executive, agenda_setter,
    institutional, biographical, constrained, national).

% Bear the costs of reduced public health, education, and subsidy expenditure; face user fees for previously public services; experience wage suppression and employment insecurity from liberalization mandates. No formal voice in conditionality design and limited ability to exit the state jurisdiction.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__debtor_extraction_reading, domestic_populations, payer,
    powerless, biographical, trapped, national).

% Represent popular opposition to austerity and privatization but are systematically excluded from creditor-debtor negotiations. Their policy preferences are treated as external to the technocratic conditionality process, and their domestic advocacy is constrained by the threat of capital flight or program suspension.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__debtor_extraction_reading, civil_society_groups, excluded,
    moderate, biographical, constrained, national).

% Document the historical continuity between colonial extraction mechanisms and contemporary debt conditionalities. Observe the asymmetry between creditor and debtor power, the collapse of policy alternatives under structural adjustment, and the distributional outcomes of enforced austerity.
narrative_ontology:constraint_stakeholder(structural_adjustment_conditionalities__debtor_extraction_reading, post_colonial_analysts, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(structural_adjustment_conditionalities__debtor_extraction_reading, private_creditor_banks).
narrative_ontology:fixing_cost_class(structural_adjustment_conditionalities__debtor_extraction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates cross-border capital repayment and investment risk management for transnational creditors by enforcing uniform fiscal discipline on sovereign debtors, overriding domestic policy autonomy.
% TRANSFER_FUNCTION: Moves public assets, social service expenditure, and policy autonomy from domestic populations and debtor states to transnational creditors and corporations, through mandated austerity, privatization, and market liberalization.
% ABSENT_VOICES: Domestic populations in debtor countries are structurally excluded from the rooms where conditionalities are designed; civil society and labor organizations that would reject austerity are sidelined in favor of finance ministries that speak for creditor interests.
% DISAPPEARANCE_RATIONALE: If conditionalities vanished overnight, debtor states could redirect fiscal resources toward domestic social contracts, sovereign debt would require genuine renegotiation rather than enforced compliance, transnational creditor repayment flows would be disrupted and repriced, and global capital markets would face a regime change in sovereign risk management.
% FOUNDING_PROBLEM: The 1980s sovereign debt crisis and the perceived need to restore balance-of-payments equilibrium and creditor confidence in heavily indebted developing countries.
% FOUNDING_PROBLEM_CORROBORATION: Academic critics, UNCTAD analyses, and debt-justice campaigns from outside the creditor institutions attest that the original liquidity crisis was resolved decades ago for many debtors, yet conditionalities persist and have expanded into permanent governance mechanisms. Creditor institutions self-assert that the problem remains live, but this claim lacks corroboration from independent parties outside the benefiting coalition.
narrative_ontology:disappearance_verdict(structural_adjustment_conditionalities__debtor_extraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(structural_adjustment_conditionalities__debtor_extraction_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(structural_adjustment_conditionalities__debtor_extraction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(structural_adjustment_conditionalities__debtor_extraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(structural_adjustment_conditionalities__debtor_extraction_reading, 0.9, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(structural_adjustment_conditionalities__debtor_extraction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(structural_adjustment_conditionalities__debtor_extraction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(structural_adjustment_conditionalities__debtor_extraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness is authored at 0.9 because conditionalities systematically redirect public resources to external creditors and corporate entrants, with negligible reciprocity to domestic populations. Suppression is 0.85 because the constraint persists only through active enforcementâcredit rationing, surveillance, and the threat of capital-market exclusionâand alternatives such as autonomous monetary policy or default are structurally collapsed. Theater ratio is 0.48: the rhetoric of 'poverty reduction' and 'country ownership' (PRSP era onward) layers performative legitimacy over unchanged extraction. Resistance is 0.62: substantial popular and scholarly opposition exists but is fragmented and systematically excluded from formal decision fora. Accessibility collapse is 0.78 because once a state enters the conditionality regime, alternative development paths are foreclosed by credit-rating and diplomatic pressure. The temporal series show extraction intensifying as conditionalities evolved from explicit structural adjustment to more diffuse but deeper 'partnership' governance, while theater increased to manage legitimacy and suppression hardened to contain resistance.
 *
 * PERSPECTIVAL GAP:
 *   The creditor-coordination seat and the domestic-population seat compute radically different constraint types from the same structural data. From the creditor-bank seat, conditionalities appear as a rope or tangled ropeâcoordination of repayment that prevents free-riding and default. From the domestic-population seat, the identical structure is a snare: coercion, absence of alternatives, and identifiable victimization. The engine derives this divergence from the beneficiary-victim declarations and the extreme exit asymmetry between global creditors and trapped domestic populations. The debtor state executive experiences the constraint as externally imposed agenda-setting rather than as either pure beneficiary or pure target, reflecting its role as a transmission belt.
 *
 * DIRECTIONALITY LOGIC:
 *   Private creditor banks and transnational corporations sit near the full-beneficiary end: the constraint subsidizes their risk and opens markets. Domestic populations sit near the full-target end: they bear the austerity and privatization costs with no offsetting benefit and no exit. The debtor state executive occupies an intermediate position with constrained exit: it formally administers the constraint and incurs sovereignty costs, but its compliance is driven by liquidity desperation rather than capture of the extraction. Civil society groups are excluded from the beneficiary-target axis altogether by structural exclusion from negotiations. The IMF, as agenda-setter, is not a direct financial beneficiary of the extraction in the same seat as private creditors, but it captures institutional authority and survival value from the regime's persistence.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâthe 1980s liquidity crisisâis dead for most debtor jurisdictions, yet the arrangement persists and has expanded in scope. This is a classic mandatrophy: the constraint has outlived the problem it was built to solve. The R5 genealogy interview records founding_problem_status: dead, disappearance_verdict: world_rearranges, indicating a zombie or capture pattern. The metrics support this: rising theater_ratio alongside rising extractiveness shows the constraint substituting legitimacy performance for crisis response. The classification prevents mislabeling by requiring active enforcement and victim identification for a snare, distinguishing it from a genuine transitional scaffold.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_divergence,
    'Is the divergence between this debtor extraction reading and the creditor_coordination_reading a difference in normative evaluation over the same structural facts, or do the readings disagree on the structural facts themselves (e.g., whether enforcement is coercive or voluntary)?',
    'Comparative case analysis of debtor-creditor bargaining protocols and the objective availability of alternative financing in the absence of IMF programs.',
    'If the divergence is purely normative, the constraints share structural referents and differ primarily in claimed_type; if structural facts differ, they are separate constraints with independently authored epsilon values.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_divergence, conceptual, 'Nature of disagreement between kernel readings').

omega_variable(
    debt_sustainability_as_construct,
    'Do current debt-sustainability thresholds represent an objective macroeconomic constraint requiring external discipline, or are they a constructed metric maintained by creditor-friendly accounting conventions and rating-agency methodologies?',
    'Historical counterfactual analysis comparing development outcomes under conditionality versus autonomous heterodox policies, and auditing the empirical foundations of IMF debt-sustainability analyses.',
    'If the thresholds are constructed, the constraint''s accessibility_collapse is higher than a natural scarcity reading would suggest, and the snare classification is strengthened; if objective, the constraint moves toward tangled rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(debt_sustainability_as_construct, empirical, 'Whether debt sustainability is objective or constructed').

omega_variable(
    suppression_as_structural_vs_discursive,
    'Is the suppression of alternatives primarily structural (enforced by credit-rationing and capital-flight threats) or discursive (enforced by the hegemony of neoclassical economic expertise that defines policy imagination)?',
    'Trace the policy options actually considered by debtor finance ministries during program negotiations, and measure the gap between technically feasible alternatives and the options admitted into discourse.',
    'If primarily discursive, the constraint''s effective suppression is higher than the structural measure suggests because exits exist but are cognitively foreclosed; if primarily structural, suppression is accurately measured by observable enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_as_structural_vs_discursive, conceptual, 'Structural versus discursive suppression mechanisms').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(structural_adjustment_conditionalities__debtor_extraction_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stru_tr_t0, structural_adjustment_conditionalities__debtor_extraction_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(stru_tr_t5, structural_adjustment_conditionalities__debtor_extraction_reading, theater_ratio, 5, 0.24).
narrative_ontology:measurement(stru_tr_t10, structural_adjustment_conditionalities__debtor_extraction_reading, theater_ratio, 10, 0.32).
narrative_ontology:measurement(stru_tr_t15, structural_adjustment_conditionalities__debtor_extraction_reading, theater_ratio, 15, 0.38).
narrative_ontology:measurement(stru_tr_t20, structural_adjustment_conditionalities__debtor_extraction_reading, theater_ratio, 20, 0.42).
narrative_ontology:measurement(stru_tr_t25, structural_adjustment_conditionalities__debtor_extraction_reading, theater_ratio, 25, 0.45).
narrative_ontology:measurement(stru_tr_t30, structural_adjustment_conditionalities__debtor_extraction_reading, theater_ratio, 30, 0.47).
narrative_ontology:measurement(stru_tr_t40, structural_adjustment_conditionalities__debtor_extraction_reading, theater_ratio, 40, 0.48).

% Extraction over time
narrative_ontology:measurement(stru_be_t0, structural_adjustment_conditionalities__debtor_extraction_reading, base_extractiveness, 0, 0.62).
narrative_ontology:measurement(stru_be_t5, structural_adjustment_conditionalities__debtor_extraction_reading, base_extractiveness, 5, 0.71).
narrative_ontology:measurement(stru_be_t10, structural_adjustment_conditionalities__debtor_extraction_reading, base_extractiveness, 10, 0.78).
narrative_ontology:measurement(stru_be_t15, structural_adjustment_conditionalities__debtor_extraction_reading, base_extractiveness, 15, 0.83).
narrative_ontology:measurement(stru_be_t20, structural_adjustment_conditionalities__debtor_extraction_reading, base_extractiveness, 20, 0.86).
narrative_ontology:measurement(stru_be_t25, structural_adjustment_conditionalities__debtor_extraction_reading, base_extractiveness, 25, 0.88).
narrative_ontology:measurement(stru_be_t30, structural_adjustment_conditionalities__debtor_extraction_reading, base_extractiveness, 30, 0.89).
narrative_ontology:measurement(stru_be_t40, structural_adjustment_conditionalities__debtor_extraction_reading, base_extractiveness, 40, 0.9).

% Suppression requirement over time
narrative_ontology:measurement(stru_su_t0, structural_adjustment_conditionalities__debtor_extraction_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(stru_su_t5, structural_adjustment_conditionalities__debtor_extraction_reading, suppression_requirement, 5, 0.65).
narrative_ontology:measurement(stru_su_t10, structural_adjustment_conditionalities__debtor_extraction_reading, suppression_requirement, 10, 0.71).
narrative_ontology:measurement(stru_su_t15, structural_adjustment_conditionalities__debtor_extraction_reading, suppression_requirement, 15, 0.76).
narrative_ontology:measurement(stru_su_t20, structural_adjustment_conditionalities__debtor_extraction_reading, suppression_requirement, 20, 0.79).
narrative_ontology:measurement(stru_su_t25, structural_adjustment_conditionalities__debtor_extraction_reading, suppression_requirement, 25, 0.82).
narrative_ontology:measurement(stru_su_t30, structural_adjustment_conditionalities__debtor_extraction_reading, suppression_requirement, 30, 0.84).
narrative_ontology:measurement(stru_su_t40, structural_adjustment_conditionalities__debtor_extraction_reading, suppression_requirement, 40, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(structural_adjustment_conditionalities__debtor_extraction_reading, creditor_coordination_reading).
narrative_ontology:affects_constraint(structural_adjustment_conditionalities__debtor_extraction_reading, hybrid_selectivity_reading).

% DUAL FORMULATION NOTE:
% The natural-language label 'structural adjustment conditionalities' conflates at least three structurally distinct constraints: a creditor-coordination mechanism (low epsilon, rope-like), a selective geopolitical discipline mechanism (variable epsilon, hybrid), and an extractive neo-colonial regime (high epsilon, snare). They form a constraint family linked by shared institutional history and competing empirical claims about the same policy instruments.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
