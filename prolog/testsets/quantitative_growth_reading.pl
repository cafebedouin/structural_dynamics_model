% ============================================================================
% CONSTRAINT STORY: quantitative_growth_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_quantitative_growth_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: quantitative_growth_reading
 *   human_readable: Quantitative GDP Growth as Performance Legitimacy
 *   domain: political_economy/development_planning/state_capitalism
 *
 * SUMMARY:
 *   A state-capitalist regime grounds its political legitimacy in maintaining
 *   a target GDP growth rate, treating quantitative expansion as the primary
 *   signal of successful governance. The constraint emerged during economic
 *   transition as a performance compact: the state would deliver rising
 *   material welfare measurable by aggregate growth. Over time, the growth
 *   imperative has locked in an investment-heavy, export-dependent model that
 *   extracts from household consumption and environmental commons to sustain
 *   the headline rate. This is the quantitative-growth reading of the
 *   performance-legitimacy kernel—one of four sibling readings that interpret
 *   the same legitimacy compact through different observables and beneficiary
 *   structures.
 *
 * KEY AGENTS:
 *   - central_planning_authority: Institutional agenda-setter (constrained/national) — sets targets, maintains legitimacy through growth demonstration
 *   - local_government_officials: Organized beneficiary (identity_locked/regional) — promoted on jurisdictional GDP, maximize investment regardless of quality
 *   - industrial_export_complex: Powerful beneficiary (mobile/global) — receives subsidized inputs to drive fixed investment and export volumes
 *   - household_consumption_sector: Organized payer (constrained/national) — income transferred to investment via suppressed rates, consumption share structurally low
 *   - environmental_commons: Non-agent payer (trapped/national) — degraded by growth pace and composition, costs externalized
 *   - domestic_reform_economists: Moderate excluded (constrained/national) — advocate rebalancing, structurally excluded from target-setting
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quantitative_growth_reading, 0.68).
domain_priors:suppression_score(quantitative_growth_reading, 0.71).
domain_priors:theater_ratio(quantitative_growth_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quantitative_growth_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(quantitative_growth_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(quantitative_growth_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quantitative_growth_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(quantitative_growth_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quantitative_growth_reading, tangled_rope).
narrative_ontology:human_readable(quantitative_growth_reading, "Quantitative GDP Growth as Performance Legitimacy").
narrative_ontology:topic_domain(quantitative_growth_reading, "political_economy/development_planning/state_capitalism").

domain_priors:requires_active_enforcement(quantitative_growth_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quantitative_growth_reading, '544fecfb-1f46-4dba-9d6a-29231f08dcac').
narrative_ontology:cs_kernel_codification('544fecfb-1f46-4dba-9d6a-29231f08dcac', formalized).
narrative_ontology:cs_authority_grounding('544fecfb-1f46-4dba-9d6a-29231f08dcac', practice).
narrative_ontology:cs_interpretation_layer_present('544fecfb-1f46-4dba-9d6a-29231f08dcac').
narrative_ontology:cs_reading_relation('544fecfb-1f46-4dba-9d6a-29231f08dcac', quantitative_growth_reading__qualitative_development_reading, influences).
narrative_ontology:cs_reading_relation('544fecfb-1f46-4dba-9d6a-29231f08dcac', quantitative_growth_reading__techno_nationalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('544fecfb-1f46-4dba-9d6a-29231f08dcac', quantitative_growth_reading__livelihood_security_reading, influences).
narrative_ontology:cs_axiom('544fecfb-1f46-4dba-9d6a-29231f08dcac', foundational, gdp_growth_as_legitimacy_signal).
narrative_ontology:cs_axiom_status(gdp_growth_as_legitimacy_signal, holdable).
narrative_ontology:cs_axiom_grounding('544fecfb-1f46-4dba-9d6a-29231f08dcac', gdp_growth_as_legitimacy_signal, conventional).
narrative_ontology:cs_axiom('544fecfb-1f46-4dba-9d6a-29231f08dcac', secondary, investment_primacy_over_consumption).
narrative_ontology:cs_axiom_status(investment_primacy_over_consumption, holdable).
narrative_ontology:cs_axiom_grounding('544fecfb-1f46-4dba-9d6a-29231f08dcac', investment_primacy_over_consumption, instrumental).
narrative_ontology:cs_reference_frame('544fecfb-1f46-4dba-9d6a-29231f08dcac', post_reform_growth_compact).
narrative_ontology:cs_drift_state('544fecfb-1f46-4dba-9d6a-29231f08dcac', contemporary_imbalance_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('544fecfb-1f46-4dba-9d6a-29231f08dcac', '').
narrative_ontology:cs_kernel_id(quantitative_growth_reading, performance_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quantitative_growth_reading, industrial_export_complex).
narrative_ontology:constraint_beneficiary(quantitative_growth_reading, local_government_officials).
narrative_ontology:constraint_beneficiary(quantitative_growth_reading, state_owned_enterprises).
narrative_ontology:constraint_victim(quantitative_growth_reading, household_consumption_sector).
narrative_ontology:constraint_victim(quantitative_growth_reading, environmental_commons).
narrative_ontology:constraint_victim(quantitative_growth_reading, labor_force_in_overcapacity_sectors).
narrative_ontology:constraint_vindicates(quantitative_growth_reading, gdp_as_development_proxy).
narrative_ontology:constraint_vindicates(quantitative_growth_reading, investment_led_growth_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets annual growth targets and evaluates provincial performance against them. Maintains legitimacy by demonstrating continuous expansion; any sustained slowdown threatens the performance compact. Must balance growth imperative against accumulating structural imbalances but is measured primarily on the headline rate.
narrative_ontology:constraint_stakeholder(quantitative_growth_reading, central_planning_authority, agenda_setter,
    institutional, generational, constrained, national).

% Promoted based on GDP growth in their jurisdiction. Maximize investment and industrial output to hit targets; debt accumulation and environmental costs are externalized or deferred. Career advancement depends on maintaining the growth rate regardless of quality or sustainability.
narrative_ontology:constraint_stakeholder(quantitative_growth_reading, local_government_officials, beneficiary,
    organized, biographical, identity_locked, regional).

% Receives subsidized credit, land, energy, and regulatory forbearance to drive export volumes and fixed-asset investment. Growth model treats their expansion as the primary engine; overcapacity and thin margins are tolerated as necessary costs of maintaining the aggregate rate.
narrative_ontology:constraint_stakeholder(quantitative_growth_reading, industrial_export_complex, beneficiary,
    powerful, biographical, mobile, global).

% Operate as countercyclical investment vehicles; expand capacity during slowdowns to stabilize the growth rate. Profitability is secondary to their stabilization function; losses are absorbed by the state banking system as the price of maintaining the quantitative target.
narrative_ontology:constraint_stakeholder(quantitative_growth_reading, state_owned_enterprises, beneficiary,
    institutional, generational, constrained, national).

% Bears the cost of the investment bias: suppressed interest rates transfer income from savers to borrowers, consumption share of GDP remains structurally low, and household welfare gains lag aggregate growth. The growth rate is maintained by deferring consumption in favor of fixed investment.
narrative_ontology:constraint_stakeholder(quantitative_growth_reading, household_consumption_sector, payer,
    organized, biographical, constrained, national).

% Degraded by the pace and composition of growth: air and water quality, soil health, and carbon emissions are externalized costs not reflected in the GDP measure. The growth imperative systematically discounts environmental limits.
narrative_ontology:constraint_stakeholder(quantitative_growth_reading, environmental_commons, payer,
    powerless, generational, trapped, national).
narrative_ontology:stakeholder_non_agent(quantitative_growth_reading, environmental_commons).

% Employed in industries sustained by policy support rather than market demand; face wage suppression, job insecurity, and delayed restructuring as capacity is maintained to preserve the growth rate. Exit options are limited by regional labor immobility and skill mismatch.
narrative_ontology:constraint_stakeholder(quantitative_growth_reading, labor_force_in_overcapacity_sectors, payer,
    moderate, biographical, constrained, regional).

% Monitor growth composition and sustainability; publish analyses questioning whether the quantitative rate reflects genuine development or accumulating imbalances. Provide alternative frameworks emphasizing consumption, productivity, and environmental accounts.
narrative_ontology:constraint_stakeholder(quantitative_growth_reading, international_development_institutions, observer,
    institutional, generational, analytical, global).

% Argue for rebalancing toward consumption, services, and total factor productivity; structurally excluded from the target-setting process because their prescriptions would lower the near-term growth rate. Their voice is heard in academic forums but not in the planning apparatus.
narrative_ontology:constraint_stakeholder(quantitative_growth_reading, domestic_reform_economists, excluded,
    moderate, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single quantitative metric for evaluating economic performance across provinces and time periods, enabling centralized resource allocation and official accountability within a unified planning framework.
% TRANSFER_FUNCTION: Transfers income from households to the investment sector via suppressed interest rates and credit allocation; transfers environmental and social costs from measured GDP to unmeasured externalities; concentrates career rewards on officials who maximize the growth rate.
% ABSENT_VOICES: Domestic reform economists advocating consumption-led rebalancing and environmental economists pricing ecological costs are structurally excluded from target-setting; their frameworks would lower the measured rate and are therefore inadmissible under the current legitimacy compact.
% DISAPPEARANCE_RATIONALE: If the growth-rate imperative vanished, the investment bias would unwind, credit would reallocate toward consumption and services, local officials would face different incentives, and the industrial-export complex would lose its policy support—the entire development model would reorganize around a different performance metric.
% FOUNDING_PROBLEM: Post-reform legitimacy required demonstrating that the new economic model could deliver rising living standards and employment after abandoning the command economy; a quantitative growth target provided a clear, comparable signal of success.
% FOUNDING_PROBLEM_CORROBORATION: The central authority attests the problem remains live, citing employment stability and poverty reduction. International development institutions and domestic reform economists attest the founding problem has shifted: absolute poverty is largely solved, and the constraint now persists as a political-economy lock-in rather than a development necessity. Independent economic analysis from outside the benefiting parties supports the shifted-function reading.
narrative_ontology:disappearance_verdict(quantitative_growth_reading, world_rearranges).
narrative_ontology:founding_problem_status(quantitative_growth_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quantitative_growth_reading, '046e0a40c34cddf4fff29b8c15f632dbdef31b7a',
    'c6d6880c39ec6bdfedde2a1d41cc00211f451559', '2026-06-11',
    'performance_legitimacy_kernel', 'agent/example_platform_commission.json',
    'claude-sonnet-4-20250514', 'temperature=1.0').
narrative_ontology:story_seed(quantitative_growth_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quantitative_growth_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(quantitative_growth_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(quantitative_growth_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is substantial (0.68 at interval end) because the growth-rate target is maintained by transferring income from households to the investment sector and externalizing environmental costs—the measured rate is decoupled from household welfare gains. Suppression is high (0.71) because sustaining the model requires suppressing alternative performance metrics, constraining capital outflows, and limiting policy debate around rebalancing. Theater ratio is moderate (0.42): the coordination function (a unified planning metric) is real, but a growing share of activity is devoted to hitting the quantitative target rather than improving underlying productivity or welfare. Accessibility collapse is moderate-low (0.48): alternative development models are visible and advocated by reform economists, but the political-economy lock-in makes them difficult to adopt. Resistance is substantial (0.58): households, environmental advocates, and reform economists contest the model, but the beneficiary coalition is entrenched. The measurement series shows extraction and suppression rising as imbalances accumulate and the model becomes harder to sustain without intensifying policy support.
 *
 * PERSPECTIVAL GAP:
 *   From the central authority's seat, the constraint is a necessary coordination mechanism that has delivered poverty reduction and employment stability—genuine development measurable by the aggregate rate. From the household and environmental seats, the same structure operates as enforced extraction: consumption is suppressed, environmental limits are ignored, and the growth rate is maintained at their expense. The engine computes this divergence from the structural data; the claimed type (tangled_rope) reflects the coordination function without adjudicating whether the extraction is justified.
 *
 * DIRECTIONALITY LOGIC:
 *   The central planning authority is the agenda-setter but sits near symmetric directionality: it benefits from the legitimacy the growth rate provides but also bears the cost of managing accumulating imbalances. Local officials and the industrial-export complex are clear beneficiaries (low d, negative or low χ): they collect career rewards and subsidized inputs directly. Households, environmental commons, and labor in overcapacity sectors are the targets (high d, high χ): they bear the income transfer, environmental degradation, and employment insecurity that sustain the rate. The identity-lock on local officials is professional: their career path is fused with the growth-maximization mandate, making exit from the incentive structure unthinkable within the current system.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint exhibits mandatrophy characteristics: the founding problem (demonstrating post-reform success) has largely been solved by absolute poverty reduction, but the growth-rate imperative persists because the beneficiary coalition (local officials, SOEs, industrial exporters) depends on it for career advancement and subsidized inputs. The mismatch between founding_problem_status (contested, with reform economists attesting it is solved) and disappearance_verdict (world_rearranges) flags the constraint as a candidate for capture/zombie classification—it would reorganize the political economy if removed, not because the original development problem remains unsolved, but because entrenched interests now depend on its persistence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    growth_quality_vs_quantity,
    'Does the measured GDP growth rate reflect genuine productivity gains and welfare improvements, or is it sustained by debt-financed investment and capacity expansion that will require future adjustment?',
    'Decomposition of growth into total factor productivity, capital deepening, and labor reallocation; comparison of household income growth to GDP growth; analysis of corporate profitability and debt-service capacity in the investment-driven sectors.',
    'If growth is quality-driven, the extraction measured here is the necessary cost of coordination; if it is quantity-driven (debt and capacity accumulation), the constraint is extracting from future consumption to maintain a current political signal, and the measured extraction understates the true transfer.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(growth_quality_vs_quantity, empirical, 'Whether the growth rate reflects productivity or deferred adjustment costs.').

omega_variable(
    legitimacy_compact_substitutability,
    'Is quantitative GDP growth the only observable that can sustain the performance legitimacy compact, or could the regime substitute a different metric (employment, consumption, innovation) without losing political stability?',
    'Natural experiment from a regime that shifts its primary performance metric and observes whether legitimacy holds; historical analysis of other developmental states that transitioned from growth-rate to quality-of-life metrics.',
    'If the compact is metric-substitutable, the quantitative-growth reading is one of several viable interpretations and the constraint''s persistence reflects beneficiary capture rather than structural necessity; if it is metric-specific, the constraint is closer to genuine coordination.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(legitimacy_compact_substitutability, conceptual, 'Whether the legitimacy compact requires this specific metric or is substitutable.').

omega_variable(
    sibling_reading_coexistence,
    'Can the quantitative-growth reading coexist with the qualitative-development or livelihood-security readings within a single policy framework, or do they foreclose each other?',
    'Policy analysis of whether rebalancing toward consumption (qualitative reading) or employment stability (livelihood reading) can be pursued without abandoning the growth-rate target; examination of whether the investment bias is structurally necessary to hit the quantitative target.',
    'If the readings coexist, the kernel supports multiple simultaneous interpretations and the quantitative reading is one emphasis among several; if they foreclose each other, adopting the quantitative reading rules out the alternatives and the constraint''s beneficiaries have locked in their preferred interpretation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_coexistence, conceptual, 'Whether this reading forecloses or coexists with sibling interpretations of the performance-legitimacy kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quantitative_growth_reading, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(quan_tr_t0, quantitative_growth_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(quan_tr_t7, quantitative_growth_reading, theater_ratio, 7, 0.27).
narrative_ontology:measurement(quan_tr_t14, quantitative_growth_reading, theater_ratio, 14, 0.32).
narrative_ontology:measurement(quan_tr_t21, quantitative_growth_reading, theater_ratio, 21, 0.36).
narrative_ontology:measurement(quan_tr_t28, quantitative_growth_reading, theater_ratio, 28, 0.39).
narrative_ontology:measurement(quan_tr_t35, quantitative_growth_reading, theater_ratio, 35, 0.42).

% Extraction over time
narrative_ontology:measurement(quan_be_t0, quantitative_growth_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(quan_be_t7, quantitative_growth_reading, base_extractiveness, 7, 0.54).
narrative_ontology:measurement(quan_be_t14, quantitative_growth_reading, base_extractiveness, 14, 0.59).
narrative_ontology:measurement(quan_be_t21, quantitative_growth_reading, base_extractiveness, 21, 0.63).
narrative_ontology:measurement(quan_be_t28, quantitative_growth_reading, base_extractiveness, 28, 0.66).
narrative_ontology:measurement(quan_be_t35, quantitative_growth_reading, base_extractiveness, 35, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(quan_su_t0, quantitative_growth_reading, suppression_requirement, 0, 0.52).
narrative_ontology:measurement(quan_su_t7, quantitative_growth_reading, suppression_requirement, 7, 0.58).
narrative_ontology:measurement(quan_su_t14, quantitative_growth_reading, suppression_requirement, 14, 0.63).
narrative_ontology:measurement(quan_su_t21, quantitative_growth_reading, suppression_requirement, 21, 0.67).
narrative_ontology:measurement(quan_su_t28, quantitative_growth_reading, suppression_requirement, 28, 0.69).
narrative_ontology:measurement(quan_su_t35, quantitative_growth_reading, suppression_requirement, 35, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(quantitative_growth_reading, qualitative_development_reading).
narrative_ontology:affects_constraint(quantitative_growth_reading, techno_nationalist_reading).
narrative_ontology:affects_constraint(quantitative_growth_reading, livelihood_security_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of four readings of the performance_legitimacy kernel. All four readings interpret the same state commitment (ground legitimacy in economic performance) but differ in their primary observable, beneficiary structure, and enforcement mechanisms. The quantitative_growth_reading (this story) treats GDP growth rate as the performance signal and benefits local officials and the industrial-export complex. The qualitative_development_reading treats human development indicators and consumption gains as the signal and benefits households and service sectors. The techno_nationalist_reading treats indigenous innovation and strategic-sector dominance as the signal and benefits national-security-linked industries. The livelihood_security_reading treats employment stability and social safety nets as the signal and benefits labor and social-insurance institutions. These are not perspectives on one constraint—they are separate constraints with different ε values, linked by network.affects_constraints because they compete for the same policy resources and legitimacy bandwidth.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
