% ============================================================================
% CONSTRAINT STORY: trips_agreement_interpretive_kernel__strong_exclusivity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_trips_agreement_interpretive_kernel__strong_exclusivity_reading, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: trips_agreement_interpretive_kernel__strong_exclusivity_reading
 *   human_readable: TRIPS Strong Exclusivity Reading: High Uniform Patent Protections with Narrow Flexibilities
 *   domain: international_trade_law/public_health/intellectual_property
 *
 * SUMMARY:
 *   This constraint instantiates the strong_exclusivity_reading of the
 *   trips_agreement_interpretive_kernel: the interpretation that the TRIPS
 *   text mandates high, uniform patent standards across all WTO members with
 *   only narrowly construed flexibilities, justified by the need to
 *   incentivize pharmaceutical innovation. Under this reading, patent holders
 *   and developed states are structural beneficiaries with enforcement
 *   mechanisms at their disposal, while low-income states and patients bear
 *   the costs of delayed generic access and high drug prices. The
 *   claim/metric independence principle is observed: the constraint claims a
 *   coordination function (innovation incentive) while the authored metrics
 *   describe substantially extractive, actively enforced operation with high
 *   suppression and moderate theater.
 *
 * KEY AGENTS:
 *   - Developed states (agenda_setter/institutional/global) â architect and enforce the regime
 *   - Pharmaceutical patent holders (beneficiary/powerful/global) â capture monopoly rents
 *   - Low-income states (payer/powerless/national) â constrained by trade retaliation threats
 *   - Patients in low-income countries (payer/powerless/national) â trapped by patent barriers
 *   - Generic drug manufacturers (excluded/organized/global) â barred from competing
 *   - Public health advocates (observer/organized/global) â analytical resistance without enforcement power
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(trips_agreement_interpretive_kernel__strong_exclusivity_reading, 0.78).
domain_priors:suppression_score(trips_agreement_interpretive_kernel__strong_exclusivity_reading, 0.82).
domain_priors:theater_ratio(trips_agreement_interpretive_kernel__strong_exclusivity_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__strong_exclusivity_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__strong_exclusivity_reading, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__strong_exclusivity_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__strong_exclusivity_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__strong_exclusivity_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(trips_agreement_interpretive_kernel__strong_exclusivity_reading, tangled_rope).
narrative_ontology:human_readable(trips_agreement_interpretive_kernel__strong_exclusivity_reading, "TRIPS Strong Exclusivity Reading: High Uniform Patent Protections with Narrow Flexibilities").
narrative_ontology:topic_domain(trips_agreement_interpretive_kernel__strong_exclusivity_reading, "international_trade_law/public_health/intellectual_property").

domain_priors:requires_active_enforcement(trips_agreement_interpretive_kernel__strong_exclusivity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(trips_agreement_interpretive_kernel__strong_exclusivity_reading, '4fcca3d7-79df-4c49-8b7b-6ae33dd670a9').
narrative_ontology:cs_kernel_codification('4fcca3d7-79df-4c49-8b7b-6ae33dd670a9', fixed_text).
narrative_ontology:cs_authority_grounding('4fcca3d7-79df-4c49-8b7b-6ae33dd670a9', lineage).
narrative_ontology:cs_interpretation_layer_present('4fcca3d7-79df-4c49-8b7b-6ae33dd670a9').
narrative_ontology:cs_reading_relation('4fcca3d7-79df-4c49-8b7b-6ae33dd670a9', trips_agreement_interpretive_kernel__public_health_flexibility_reading, coexists_with).
narrative_ontology:cs_reading_relation('4fcca3d7-79df-4c49-8b7b-6ae33dd670a9', trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, influences).
narrative_ontology:cs_axiom('4fcca3d7-79df-4c49-8b7b-6ae33dd670a9', foundational, uniform_patent_minimum_standard).
narrative_ontology:cs_axiom_status(uniform_patent_minimum_standard, holdable).
narrative_ontology:cs_axiom_grounding('4fcca3d7-79df-4c49-8b7b-6ae33dd670a9', uniform_patent_minimum_standard, conventional).
narrative_ontology:cs_axiom('4fcca3d7-79df-4c49-8b7b-6ae33dd670a9', foundational, innovation_incentive_exclusivity).
narrative_ontology:cs_axiom_status(innovation_incentive_exclusivity, holdable).
narrative_ontology:cs_axiom_grounding('4fcca3d7-79df-4c49-8b7b-6ae33dd670a9', innovation_incentive_exclusivity, instrumental).
narrative_ontology:cs_reference_frame('4fcca3d7-79df-4c49-8b7b-6ae33dd670a9', uniform_patent_exclusivity_framework).
narrative_ontology:cs_drift_state('4fcca3d7-79df-4c49-8b7b-6ae33dd670a9', post_doha_access_pressure, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('4fcca3d7-79df-4c49-8b7b-6ae33dd670a9', '').
narrative_ontology:cs_kernel_id(trips_agreement_interpretive_kernel__strong_exclusivity_reading, trips_agreement_interpretive_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(trips_agreement_interpretive_kernel__strong_exclusivity_reading, pharmaceutical_patent_holders).
narrative_ontology:constraint_beneficiary(trips_agreement_interpretive_kernel__strong_exclusivity_reading, developed_states).
narrative_ontology:constraint_victim(trips_agreement_interpretive_kernel__strong_exclusivity_reading, low_income_states).
narrative_ontology:constraint_victim(trips_agreement_interpretive_kernel__strong_exclusivity_reading, patients_in_low_income_countries).
narrative_ontology:constraint_vindicates(trips_agreement_interpretive_kernel__strong_exclusivity_reading, patent_harmonization_doctrine).
narrative_ontology:constraint_vindicates(trips_agreement_interpretive_kernel__strong_exclusivity_reading, innovation_incentive_theory).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Obtain and enforce uniform patent monopolies across all WTO member markets, extending exclusivity for 20 years and using TRIPS dispute mechanisms and bilateral pressure to block generic entry. They capture monopoly rents and set prices without competition in low-income markets.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__strong_exclusivity_reading, pharmaceutical_patent_holders, beneficiary,
    powerful, generational, arbitrage, global).

% Negotiated and administer the TRIPS framework through the WTO; benefit from domestic pharmaceutical export competitiveness and leverage the regime in bilateral trade agreements. They control dispute settlement appointments and can retaliate against members that use flexibilities broadly.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__strong_exclusivity_reading, developed_states, agenda_setter,
    institutional, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(trips_agreement_interpretive_kernel__strong_exclusivity_reading, developed_states, beneficiary).

% Must implement patent standards exceeding domestic industrial capacity and forego local generic production for patented medicines. Health ministries bear the budgetary and mortality costs of delayed access. Using compulsory licensing triggers trade retaliation and bilateral pressure, so flexibilities exist on paper but are constrained in practice.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__strong_exclusivity_reading, low_income_states, payer,
    powerless, generational, constrained, national).

% Face monopoly prices for patented essential medicines and lack affordable generic alternatives due to patent barriers and the narrow construction of compulsory licensing. Individual patients have no exit from the global patent regime and must pay prices set by distant patent holders or go without treatment.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__strong_exclusivity_reading, patients_in_low_income_countries, payer,
    powerless, biographical, trapped, national).

% Are barred from producing patented active pharmaceutical ingredients for export or domestic use in WTO markets. They are confined to pre-TRIPS processes, non-WTO markets, or post-expiry production. They would enter the market immediately if flexibilities were interpreted broadly.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__strong_exclusivity_reading, generic_drug_manufacturers, excluded,
    organized, biographical, constrained, global).

% Document access gaps, publish treatment-need analyses, and lobby for broader TRIPS flexibilities and alternative R&D models. They generate discursive and moral pressure but hold no enforcement power within the WTO dispute system.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__strong_exclusivity_reading, public_health_advocates, observer,
    organized, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(trips_agreement_interpretive_kernel__strong_exclusivity_reading, pharmaceutical_patent_holders).
narrative_ontology:fixing_cost_class(trips_agreement_interpretive_kernel__strong_exclusivity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Standardizes global patent protection to solve free-rider problems in pharmaceutical R&D, intending to ensure innovators can recoup investment across all national markets rather than losing price exclusivity to immediate generic competition.
% TRANSFER_FUNCTION: Moves monopoly rents, delayed generic entry, and higher medicine costs from patients and public health budgets in low-income countries to pharmaceutical patent holders and developed-state export industries, enforced through WTO dispute settlement and trade retaliation.
% ABSENT_VOICES: Patients in low-income countries and generic manufacturers are structurally excluded from WTO negotiations and dispute settlement proceedings; their interests appear only indirectly through public health advocates who lack formal voting or veto power in trade governance.
% DISAPPEARANCE_RATIONALE: If the strong exclusivity mandate vanished, generic manufacturers would enter markets for patented essential medicines, prices would fall, low-income health budgets would reallocate from monopoly rents to other care, and the global pharmaceutical R&D financing model would shift toward alternative incentive structures.
% FOUNDING_PROBLEM: Pharmaceutical innovation is vulnerable to immediate generic competition that erodes prices before R&D costs are recovered, leading to systematic underinvestment in new drug development.
% FOUNDING_PROBLEM_CORROBORATION: Pharmaceutical industry associations and developed-state trade ministries attest the problem remains live. Independent public health economists, the WHO, and MÃ©decins Sans FrontiÃ¨res attest that the problem is overstated relative to the extraction authorized, that public funding accounts for much early-stage research, and that alternative R&D financing mechanisms exist; the Doha Declaration on TRIPS and Public Health itself, adopted by the full WTO membership, corroborates that the founding problem is contested rather than settled.
narrative_ontology:disappearance_verdict(trips_agreement_interpretive_kernel__strong_exclusivity_reading, world_rearranges).
narrative_ontology:founding_problem_status(trips_agreement_interpretive_kernel__strong_exclusivity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(trips_agreement_interpretive_kernel__strong_exclusivity_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(trips_agreement_interpretive_kernel__strong_exclusivity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(trips_agreement_interpretive_kernel__strong_exclusivity_reading, 0.78, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(trips_agreement_interpretive_kernel__strong_exclusivity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(trips_agreement_interpretive_kernel__strong_exclusivity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(trips_agreement_interpretive_kernel__strong_exclusivity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.78) is high because the constraint moves substantial monopoly rents from low-income patients to patent holders under conditions where marginal R&D cost recovery is decoupled from price. Suppression (0.82) is higher because the regime persists only through active WTO dispute settlement, bilateral trade pressure, and the structural narrowing of compulsory licensing. Theater ratio (0.42) reflects a significant performative layer: the 'innovation incentive' narrative is maintained even as evidence grows that much R&D is publicly funded and that alternative incentive models exist. Accessibility collapse (0.72) is high because once a drug is patented under TRIPS, legal generic alternatives are barred until expiry or an exceptional compulsory license. Resistance (0.58) captures persistent but uneven opposition from Global South coalitions, access campaigns, and occasional compulsory licensing episodes.
 *
 * PERSPECTIVAL GAP:
 *   The developed-state seat and the patent-holder seat experience the constraint as necessary coordination protecting a fragile innovation pipeline; the low-income state and patient seats experience the same structure as enforced extraction that prioritizes rents over mortality. The engine computes this divergence from the structural data: beneficiaries with arbitrage-grade exit and global scope receive damped effective extraction, while powerless trapped payers at national scope experience amplified extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Pharmaceutical patent holders are full beneficiaries (d near 0.0): they collect the monopoly rents and can arbitrage jurisdictions. Developed states are near-beneficiaries with agenda-setting power (low d). Low-income states are targets (high d): they pay through constrained health budgets and suppressed policy sovereignty. Patients in low-income countries are full targets (d near 1.0): they are identity-locked to their national health systems and trapped in the global regime. Generic manufacturers are excluded rather than coordinated; their exclusion is the enforcement object.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â underinvestment in pharmaceuticals due to free-riding â may have been live in 1994, but the strong exclusivity reading resists flexibility expansion even where the innovation incentive is tenuous. The mandatrophy question is whether the constraint now persists because it solves the coordination problem or because it locks in a transfer function that benefits concentrated parties. The temporal measurements show rising extractiveness and theater over the interval, suggesting drift toward extraction, though not yet full mandatrophy resolution (the coordination function is not entirely dead).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    innovation_extraction_proportion,
    'Does the revenue extracted through strong patent exclusivity flow predominantly into marginal pharmaceutical innovation, or into rent capture, marketing, and shareholder returns?',
    'Comparative analysis of R&D expenditure ratios, public funding contributions to patented drugs, and economic studies of price elasticity versus innovation output.',
    'If the innovation return is low relative to extraction, the coordination story weakens and the constraint shifts toward snare classification; if high, the tangled_rope classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(innovation_extraction_proportion, empirical, 'Whether extraction funds innovation or rent capture').

omega_variable(
    compulsory_licensing_accessibility,
    'Are TRIPS flexibilities for compulsory licensing practically accessible to low-income states, or are they blocked by procedural complexity, legal uncertainty, and political retaliation?',
    'Systematic review of compulsory licensing petitions, grant rates, subsequent trade measures, and pharmaceutical-industry litigation patterns.',
    'If flexibilities are practically inaccessible, the suppression metric understates the true constraint on low-income states and effective extractiveness is higher than the structural measure suggests.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(compulsory_licensing_accessibility, empirical, 'Practical accessibility of TRIPS compulsory licensing').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is low-income state non-use of TRIPS flexibilities driven by external trade retaliation threats (structural suppression) or by internalized belief that strong IP protection is necessary to attract foreign investment (internalized suppression)?',
    'Post-crisis trajectory analysis: do states use flexibilities more freely after breaking the investment-fear frame (e.g., during pandemic emergencies) or does avoidance persist even when retaliation risk is temporarily low?',
    'If internalized, effective suppression is higher than the structural measure suggests because states carry the constraint with them even when external enforcement is relaxed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, conceptual, 'Structural vs internalized suppression in state behavior').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(trips_agreement_interpretive_kernel__strong_exclusivity_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(trips_strong_excl_tr_t0, trips_agreement_interpretive_kernel__strong_exclusivity_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(trips_strong_excl_tr_t5, trips_agreement_interpretive_kernel__strong_exclusivity_reading, theater_ratio, 5, 0.28).
narrative_ontology:measurement(trips_strong_excl_tr_t10, trips_agreement_interpretive_kernel__strong_exclusivity_reading, theater_ratio, 10, 0.32).
narrative_ontology:measurement(trips_strong_excl_tr_t15, trips_agreement_interpretive_kernel__strong_exclusivity_reading, theater_ratio, 15, 0.35).
narrative_ontology:measurement(trips_strong_excl_tr_t20, trips_agreement_interpretive_kernel__strong_exclusivity_reading, theater_ratio, 20, 0.38).
narrative_ontology:measurement(trips_strong_excl_tr_t25, trips_agreement_interpretive_kernel__strong_exclusivity_reading, theater_ratio, 25, 0.4).
narrative_ontology:measurement(trips_strong_excl_tr_t30, trips_agreement_interpretive_kernel__strong_exclusivity_reading, theater_ratio, 30, 0.42).

% Extraction over time
narrative_ontology:measurement(trips_strong_excl_be_t0, trips_agreement_interpretive_kernel__strong_exclusivity_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(trips_strong_excl_be_t5, trips_agreement_interpretive_kernel__strong_exclusivity_reading, base_extractiveness, 5, 0.6).
narrative_ontology:measurement(trips_strong_excl_be_t10, trips_agreement_interpretive_kernel__strong_exclusivity_reading, base_extractiveness, 10, 0.65).
narrative_ontology:measurement(trips_strong_excl_be_t15, trips_agreement_interpretive_kernel__strong_exclusivity_reading, base_extractiveness, 15, 0.7).
narrative_ontology:measurement(trips_strong_excl_be_t20, trips_agreement_interpretive_kernel__strong_exclusivity_reading, base_extractiveness, 20, 0.74).
narrative_ontology:measurement(trips_strong_excl_be_t25, trips_agreement_interpretive_kernel__strong_exclusivity_reading, base_extractiveness, 25, 0.77).
narrative_ontology:measurement(trips_strong_excl_be_t30, trips_agreement_interpretive_kernel__strong_exclusivity_reading, base_extractiveness, 30, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(trips_strong_excl_su_t0, trips_agreement_interpretive_kernel__strong_exclusivity_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(trips_strong_excl_su_t5, trips_agreement_interpretive_kernel__strong_exclusivity_reading, suppression_requirement, 5, 0.62).
narrative_ontology:measurement(trips_strong_excl_su_t10, trips_agreement_interpretive_kernel__strong_exclusivity_reading, suppression_requirement, 10, 0.68).
narrative_ontology:measurement(trips_strong_excl_su_t15, trips_agreement_interpretive_kernel__strong_exclusivity_reading, suppression_requirement, 15, 0.74).
narrative_ontology:measurement(trips_strong_excl_su_t20, trips_agreement_interpretive_kernel__strong_exclusivity_reading, suppression_requirement, 20, 0.78).
narrative_ontology:measurement(trips_strong_excl_su_t25, trips_agreement_interpretive_kernel__strong_exclusivity_reading, suppression_requirement, 25, 0.81).
narrative_ontology:measurement(trips_strong_excl_su_t30, trips_agreement_interpretive_kernel__strong_exclusivity_reading, suppression_requirement, 30, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(trips_agreement_interpretive_kernel__strong_exclusivity_reading, resource_allocation).
narrative_ontology:affects_constraint(trips_agreement_interpretive_kernel__strong_exclusivity_reading, public_health_flexibility_reading).
narrative_ontology:affects_constraint(trips_agreement_interpretive_kernel__strong_exclusivity_reading, dispute_settlement_interpretive_authority).

% DUAL FORMULATION NOTE:
% The TRIPS agreement kernel decomposes into at least three structurally distinct constraints: the strong exclusivity reading (high uniform patents, narrow flexibilities), the public health flexibility reading (broad flexibilities for access), and the dispute settlement interpretive authority (who decides between them). Each has different beneficiary/victim structures and different epsilon values.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
