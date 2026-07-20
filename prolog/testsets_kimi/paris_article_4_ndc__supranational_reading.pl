% ============================================================================
% CONSTRAINT STORY: paris_article_4_ndc__supranational_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_paris_article_4_ndc__supranational_reading, []).

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
 *   constraint_id: paris_article_4_ndc__supranational_reading
 *   human_readable: Paris Article 4 NDC â Supranational Binding Reading
 *   domain: international climate governance / treaty law / political economy
 *
 * SUMMARY:
 *   The Paris Agreement Article 4 Nationally Determined Contributions (NDCs)
 *   under the supranational reading constitute a binding international
 *   climate governance regime. States assume enforceable commitments to
 *   ratchet greenhouse-gas emissions toward net-zero, subject to
 *   transparency, global stocktake, and escalating reputational and financial
 *   sanctions for non-compliance. The regime institutionalizes wealth
 *   transfers from developed to developing nations and engineers the
 *   regulatory extinction of carbon-intensive industries. This reading treats
 *   the Paris text as creating genuine supranational obligation, not
 *   voluntary pledgership. It is one reading of the contested
 *   paris_article_4_ndc kernel; the sovereigntist and equity readings
 *   instantiate structurally distinct constraints from the same treaty text.
 *
 * KEY AGENTS:
 *   - developed_nations: Primary payer and transfer source (institutional power, constrained exit)
 *   - carbon_intensive_industries: Primary structural target facing regulatory extinction (powerful, trapped exit)
 *   - developing_nations: Primary beneficiary of climate finance and capacity-building (moderate power, constrained exit)
 *   - unfccc_bureaucracy: Agenda-setter administering accountability machinery (institutional power, constrained exit)
 *   - fossil_fuel_dependent_economies: Secondary payer facing sanctions and market exclusion (powerful, constrained exit)
 *   - renewable_energy_sector: Secondary beneficiary capturing policy space and market share (organized, mobile exit)
 *   - independent_climate_scientists: Analytical observer providing trajectory assessments (analytical, analytical exit)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(paris_article_4_ndc__supranational_reading, 0.76).
domain_priors:suppression_score(paris_article_4_ndc__supranational_reading, 0.71).
domain_priors:theater_ratio(paris_article_4_ndc__supranational_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(paris_article_4_ndc__supranational_reading, extractiveness, 0.76).
narrative_ontology:constraint_metric(paris_article_4_ndc__supranational_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(paris_article_4_ndc__supranational_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(paris_article_4_ndc__supranational_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(paris_article_4_ndc__supranational_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(paris_article_4_ndc__supranational_reading, tangled_rope).
narrative_ontology:human_readable(paris_article_4_ndc__supranational_reading, "Paris Article 4 NDC â Supranational Binding Reading").
narrative_ontology:topic_domain(paris_article_4_ndc__supranational_reading, "international climate governance / treaty law / political economy").

domain_priors:requires_active_enforcement(paris_article_4_ndc__supranational_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(paris_article_4_ndc__supranational_reading, 'fd460966-ca7f-40e2-9202-f2f237092d45').
narrative_ontology:cs_kernel_codification('fd460966-ca7f-40e2-9202-f2f237092d45', formalized).
narrative_ontology:cs_authority_grounding('fd460966-ca7f-40e2-9202-f2f237092d45', lineage).
narrative_ontology:cs_interpretation_layer_present('fd460966-ca7f-40e2-9202-f2f237092d45').
narrative_ontology:cs_reading_relation('fd460966-ca7f-40e2-9202-f2f237092d45', paris_article_4_ndc__sovereigntist_reading, forecloses).
narrative_ontology:cs_reading_relation('fd460966-ca7f-40e2-9202-f2f237092d45', paris_article_4_ndc__equity_reading, coexists_with).
narrative_ontology:cs_axiom('fd460966-ca7f-40e2-9202-f2f237092d45', foundational, state_accountability_supranational).
narrative_ontology:cs_axiom_status(state_accountability_supranational, holdable).
narrative_ontology:cs_axiom_grounding('fd460966-ca7f-40e2-9202-f2f237092d45', state_accountability_supranational, conventional).
narrative_ontology:cs_axiom('fd460966-ca7f-40e2-9202-f2f237092d45', foundational, ratchet_net_zero_mandatory).
narrative_ontology:cs_axiom_status(ratchet_net_zero_mandatory, holdable).
narrative_ontology:cs_axiom_grounding('fd460966-ca7f-40e2-9202-f2f237092d45', ratchet_net_zero_mandatory, conventional).
narrative_ontology:cs_reference_frame('fd460966-ca7f-40e2-9202-f2f237092d45', supranational_accountability_framework).
narrative_ontology:cs_drift_state('fd460966-ca7f-40e2-9202-f2f237092d45', contemporary_post_paris_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('fd460966-ca7f-40e2-9202-f2f237092d45', '').
narrative_ontology:cs_kernel_id(paris_article_4_ndc__supranational_reading, paris_article_4_ndc).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(paris_article_4_ndc__supranational_reading, developing_nations).
narrative_ontology:constraint_beneficiary(paris_article_4_ndc__supranational_reading, renewable_energy_sector).
narrative_ontology:constraint_beneficiary(paris_article_4_ndc__supranational_reading, small_island_states).
narrative_ontology:constraint_victim(paris_article_4_ndc__supranational_reading, developed_nations).
narrative_ontology:constraint_victim(paris_article_4_ndc__supranational_reading, carbon_intensive_industries).
narrative_ontology:constraint_victim(paris_article_4_ndc__supranational_reading, fossil_fuel_dependent_economies).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bear the primary costs of NDC compliance through domestic mitigation, reporting burdens, and institutionalized wealth transfers to developing nations. Exit from the Paris regime triggers diplomatic isolation, carbon-border adjustment exposure, and exclusion from green finance flows.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__supranational_reading, developed_nations, payer,
    institutional, generational, constrained, global).

% Receive climate finance, technology transfer, and capacity-building support through the supranational accountability framework. Their mitigation pledges are partially subsidized by developed-nation transfers, but they remain rule-takers in the stocktake and compliance architecture.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__supranational_reading, developing_nations, beneficiary,
    moderate, generational, constrained, global).

% Face regulatory extinction under ratcheting net-zero trajectories: structural closure of capital access, licensing, insurance, and social license. No viable sectoral exit exists because the constraint suppresses the underlying business model itself rather than merely taxing it.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__supranational_reading, carbon_intensive_industries, payer,
    powerful, biographical, trapped, global).

% States whose fiscal and energy systems depend on hydrocarbon rents face reputational sanctions, exclusion from climate finance, and carbon-border levies for non-compliance with binding trajectories. Economic diversification is constrained by sunk infrastructure and debt structures.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__supranational_reading, fossil_fuel_dependent_economies, payer,
    powerful, generational, constrained, national).

% Captures market share, subsidy regimes, and policy space liberated by the regulatory suppression of carbon-intensive competitors. Firms and capital can relocate across jurisdictions to exploit ratcheting demand for clean technology.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__supranational_reading, renewable_energy_sector, beneficiary,
    organized, biographical, mobile, global).

% Receive existential risk reduction from global mitigation ambition and adaptation finance. Geographically trapped by sea-level rise but institutionally empowered by the supranational frameworkâs formal commitment to loss and damage.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__supranational_reading, small_island_states, beneficiary,
    moderate, civilizational, trapped, global).

% Administers the transparency, reporting, and compliance machinery; convenes global stocktakes and ratcheting cycles. Derives institutional mandate, staffing, and budget from the regimeâs persistence and expansion.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__supranational_reading, unfccc_bureaucracy, agenda_setter,
    institutional, generational, constrained, global).

% Provide independent assessment of emission trajectories, carbon-budget adequacy, and policy effectiveness. Neither collect from nor pay into the constraint; their credibility depends on methodological independence from the UNFCCC policy cycle.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__supranational_reading, independent_climate_scientists, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the global commons problem of greenhouse-gas accumulation by creating a common accountability framework with ratcheting ambition cycles, transparent reporting, and a unified measurement standard that prevents free-riding on atmospheric stabilization.
% TRANSFER_FUNCTION: Moves financial and technological resources from developed nations to developing nations, and regulatory market share from carbon-intensive industries to renewable-energy sectors, underwritten by the threat of reputational and financial sanctions for non-compliance.
% ABSENT_VOICES: Future generations have no formal seat in the stocktake process; carbon-intensive industry voices are consulted in technical dialogues but structurally overridden in the core legal architecture. States that reject the Paris framework entirely are excluded from the compliance conversation but not from the physical atmosphere.
% DISAPPEARANCE_RATIONALE: If the supranational binding framework vanished overnight, climate-finance flows would collapse, carbon regulation would fragment into incompatible national regimes, the renewable sector would lose its policy subsidy backbone, and fossil-fuel-dependent economies would rapidly re-expand production â the global emissions trajectory would decouple from net-zero within one political cycle.
% FOUNDING_PROBLEM: The atmospheric commons is a global public good: no single state can stabilize the climate unilaterally, and unilateral mitigation is undermined by free-riding from non-acting states.
% FOUNDING_PROBLEM_CORROBORATION: The IPCC, an independent scientific body outside the beneficiary set, attests that the climate-stabilization problem remains live and under-addressed. Independent economic analyses from the IEA and OECD corroborate that current NDC trajectories are insufficient to meet temperature goals, supporting the claim that the founding problem persists. Some heterodox economists and fossil-fuel-exporting states contest the urgency, but their testimony is contested by the majority of external scientific and economic authorities.
narrative_ontology:disappearance_verdict(paris_article_4_ndc__supranational_reading, world_rearranges).
narrative_ontology:founding_problem_status(paris_article_4_ndc__supranational_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(paris_article_4_ndc__supranational_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(paris_article_4_ndc__supranational_reading, 'none', 1).
narrative_ontology:epsilon_provenance(paris_article_4_ndc__supranational_reading, 0.76, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(paris_article_4_ndc__supranational_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(paris_article_4_ndc__supranational_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(paris_article_4_ndc__supranational_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The supranational reading produces high extractiveness (0.76) because it structurally transfers wealth from North to South, suppresses entire industrial sectors through regulatory extinction, and imposes compliance and reporting costs on developed economies. Suppression (0.71) is high because the constraint depends on active exclusion of non-compliant states from finance and reputation, and on the suppression of carbon-intensive alternatives. Theater ratio (0.48) reflects significant performative complianceâelaborate reporting rituals and ambitious pledges with weak deliveryâalongside real binding force in the financial and regulatory architecture. Accessibility collapse (0.72) is high because the Paris framework collapsed the prior alternative of purely voluntary, non-accountable climate action. Resistance (0.78) is high due to organized resistance from fossil capital and recalcitrant states.
 *
 * PERSPECTIVAL GAP:
 *   The developed-nation seat experiences the constraint as extractive (wealth outflows, domestic industrial suppression), while the developing-nation seat experiences it as coordinative (resource inflows, existential risk reduction). The carbon-industry seat experiences pure extraction (regulatory extinction), while the renewable sector experiences subsidy and market expansion. The UNFCCC bureaucracy experiences a hybrid: it administers coordination but its institutional survival and budget depend on the constraint's persistence. These divergences are structurally derived from the same treaty text read through the supranational lens.
 *
 * DIRECTIONALITY LOGIC:
 *   Developing nations, small island states, and the renewable sector sit at the beneficiary end (low d): they receive transfers, policy space, and existential risk reduction. Developed nations, carbon-intensive industries, and fossil-fuel-dependent economies sit at the target end (high d): they pay transfers, face regulatory extinction, and bear sanctions. The UNFCCC bureaucracy sits near symmetric but slightly toward beneficiary: it does not extract personal rents, but its institutional budget and authority are subsidized by the regime. Independent scientists are analytical (neutral d).
 *
 * MANDATROPHY ANALYSIS:
 *   Without the coordination functionâsolving the global commons problem of atmospheric greenhouse-gas accumulationâthe wealth transfers and industrial suppression would read as pure extraction (snare). The presence of a genuine collective-action problem means the constraint must be classified as tangled_rope: it coordinates emission reductions and transparency while simultaneously extracting from specific industrial and national seats. The mandatrophy guard prevents reclassification to snare by noting the live founding problem (climate stabilization), even as the extraction is substantial and asymmetric.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    supranational_binding_reality,
    'Does the supranational reading describe a genuinely enforceable legal obligation, or a coordinative fiction sustained by diplomatic theater?',
    'Jurisprudential analysis of ICJ and ITLOS admissibility of Paris-based claims; empirical tracking of state behavioral change following compliance findings and public naming.',
    'If unenforceable in adjudicative practice, the high extraction is achieved through networked reputation rather than law, reclassifying the suppression mechanism as diplomatic stigma rather than institutional enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(supranational_binding_reality, conceptual, 'Whether the binding character is legal fact or interpretive projection').

omega_variable(
    regulatory_extinction_necessity,
    'Is the regulatory extinction of carbon-intensive industries a necessary cost of global coordination, or asymmetric extraction disguised as environmental necessity?',
    'Comparative analysis of decarbonization pathways: do technological substitution rates and grid-ready alternatives justify the pace of regulatory closure, or does the closure outrun viable substitutes?',
    'If closure outruns substitution, the industry seat experiences pure extraction; if synchronized, the cost is the genuine price of planetary-scale coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_extinction_necessity, empirical, 'Whether industrial suppression tracks coordination need or extraction').

omega_variable(
    north_south_transfer_equity,
    'Are North-South wealth transfers under the supranational reading reparative justice for historical emissions, or extractive redistribution that enriches developing elites without measurably solving the coordination problem?',
    'Traceability audits of climate-finance flows; correlation between transfer volumes and verified developing-nation emission-mitigation outcomes.',
    'If transfers fail to reduce global emissions, the coordination story is cover for redistribution; if correlated with mitigation, the extraction is the price of coordination.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(north_south_transfer_equity, empirical, 'Whether wealth transfers produce coordination or pure redistribution').

omega_variable(
    kernel_reading_operativity,
    'Does the supranational reading''s claim of binding obligation foreclose the sovereigntist reading in state practice, or do the two readings coexist as live interpretive options within the same Party submissions?',
    'Discourse analysis of state NDC communications and national ratification instruments: do states treat their NDCs as binding obligations or voluntary contributions in their own domestic legal orders?',
    'If states consistently treat NDCs as voluntary domestically, the supranational reading is an analytical projection rather than an operative constraint, and effective extraction is lower than the structural claim suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_operativity, conceptual, 'Whether the supranational reading is operative in state practice or remains contested').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(paris_article_4_ndc__supranational_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(paris_ndc_supra_tr_t0, paris_article_4_ndc__supranational_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(paris_ndc_supra_tr_t5, paris_article_4_ndc__supranational_reading, theater_ratio, 5, 0.27).
narrative_ontology:measurement(paris_ndc_supra_tr_t10, paris_article_4_ndc__supranational_reading, theater_ratio, 10, 0.32).
narrative_ontology:measurement(paris_ndc_supra_tr_t15, paris_article_4_ndc__supranational_reading, theater_ratio, 15, 0.38).
narrative_ontology:measurement(paris_ndc_supra_tr_t20, paris_article_4_ndc__supranational_reading, theater_ratio, 20, 0.42).
narrative_ontology:measurement(paris_ndc_supra_tr_t25, paris_article_4_ndc__supranational_reading, theater_ratio, 25, 0.45).
narrative_ontology:measurement(paris_ndc_supra_tr_t30, paris_article_4_ndc__supranational_reading, theater_ratio, 30, 0.48).

% Extraction over time
narrative_ontology:measurement(paris_ndc_supra_be_t0, paris_article_4_ndc__supranational_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(paris_ndc_supra_be_t5, paris_article_4_ndc__supranational_reading, base_extractiveness, 5, 0.58).
narrative_ontology:measurement(paris_ndc_supra_be_t10, paris_article_4_ndc__supranational_reading, base_extractiveness, 10, 0.63).
narrative_ontology:measurement(paris_ndc_supra_be_t15, paris_article_4_ndc__supranational_reading, base_extractiveness, 15, 0.67).
narrative_ontology:measurement(paris_ndc_supra_be_t20, paris_article_4_ndc__supranational_reading, base_extractiveness, 20, 0.71).
narrative_ontology:measurement(paris_ndc_supra_be_t25, paris_article_4_ndc__supranational_reading, base_extractiveness, 25, 0.74).
narrative_ontology:measurement(paris_ndc_supra_be_t30, paris_article_4_ndc__supranational_reading, base_extractiveness, 30, 0.76).

% Suppression requirement over time
narrative_ontology:measurement(paris_ndc_supra_su_t0, paris_article_4_ndc__supranational_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(paris_ndc_supra_su_t5, paris_article_4_ndc__supranational_reading, suppression_requirement, 5, 0.52).
narrative_ontology:measurement(paris_ndc_supra_su_t10, paris_article_4_ndc__supranational_reading, suppression_requirement, 10, 0.58).
narrative_ontology:measurement(paris_ndc_supra_su_t15, paris_article_4_ndc__supranational_reading, suppression_requirement, 15, 0.63).
narrative_ontology:measurement(paris_ndc_supra_su_t20, paris_article_4_ndc__supranational_reading, suppression_requirement, 20, 0.67).
narrative_ontology:measurement(paris_ndc_supra_su_t25, paris_article_4_ndc__supranational_reading, suppression_requirement, 25, 0.69).
narrative_ontology:measurement(paris_ndc_supra_su_t30, paris_article_4_ndc__supranational_reading, suppression_requirement, 30, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(paris_article_4_ndc__supranational_reading, global_infrastructure).
narrative_ontology:affects_constraint(paris_article_4_ndc__supranational_reading, paris_article_4_ndc__sovereigntist_reading).
narrative_ontology:affects_constraint(paris_article_4_ndc__supranational_reading, paris_article_4_ndc__equity_reading).

% DUAL FORMULATION NOTE:
% This constraint is the supranational reading of the paris_article_4_ndc kernel. The same treaty text supports the sovereigntist reading (voluntary pledges preserving national energy sovereignty) and the equity reading (CBDR-structured differentiation between developed and developing obligations). Each reading instantiates a distinct constraint with different epsilon, beneficiary/victim structure, and directionality. The supranational reading influences the equity reading's operating conditions and forecloses the sovereigntist reading's core premise of pure voluntariness.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
