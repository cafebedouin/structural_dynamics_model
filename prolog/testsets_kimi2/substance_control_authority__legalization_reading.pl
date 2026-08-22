% ============================================================================
% CONSTRAINT STORY: substance_control_authority__legalization_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_substance_control_authority__legalization_reading, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: substance_control_authority__legalization_reading
 *   human_readable: State-Regulated Drug Market Legalization
 *   domain: public_health_policy/criminal_justice/political_economy
 *
 * SUMMARY:
 *   This constraint story models the legalization reading of the
 *   substance_control_authority kernel: state power is used to create a legal
 *   commercial market for drugs with quality controls and taxed access,
 *   displacing criminal supply. Users exit the criminal-supply victim set and
 *   gain quality guarantees; black market operators become the target of
 *   enforcement. The state captures tax revenue and exercises regulatory
 *   discretion. It is claimed as tangled_rope because the same structure that
 *   coordinates safety and tax collection also extracts from excluded
 *   operators and concentrates market power in licensed vendors. This reading
 *   is one of three decomposed constraints (prohibition_reading,
 *   harm_reduction_reading) linked in a constraint family.
 *
 * KEY AGENTS:
 *   - state_regulator: Agenda-setter (institutional/arbitrage) â designs and enforces the regulatory framework, captures tax revenue
 *   - licensed_vendors: Primary beneficiary (powerful/constrained) â operate within the licensed regime, profit from reduced competition
 *   - drug_consumers: Mixed beneficiary-payer (moderate/constrained) â gain quality/safety, pay taxes and regulated prices
 *   - black_market_operators: Primary target/payer (organized/trapped) â bear enforcement costs and market exclusion
 *   - affected_communities: Secondary beneficiary-payer (organized/constrained) â gain crime reduction, bear use-volume externalities
 *   - public_health_authorities: Analytical observer (institutional/analytical) â monitor outcomes, advise on adjustments
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(substance_control_authority__legalization_reading, 0.55).
domain_priors:suppression_score(substance_control_authority__legalization_reading, 0.52).
domain_priors:theater_ratio(substance_control_authority__legalization_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(substance_control_authority__legalization_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(substance_control_authority__legalization_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(substance_control_authority__legalization_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(substance_control_authority__legalization_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(substance_control_authority__legalization_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(substance_control_authority__legalization_reading, tangled_rope).
narrative_ontology:human_readable(substance_control_authority__legalization_reading, "State-Regulated Drug Market Legalization").
narrative_ontology:topic_domain(substance_control_authority__legalization_reading, "public_health_policy/criminal_justice/political_economy").

domain_priors:requires_active_enforcement(substance_control_authority__legalization_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(substance_control_authority__legalization_reading, '2ea1712e-dc9e-401f-8d9b-57205fac2985').
narrative_ontology:cs_kernel_codification('2ea1712e-dc9e-401f-8d9b-57205fac2985', formalized).
narrative_ontology:cs_authority_grounding('2ea1712e-dc9e-401f-8d9b-57205fac2985', lineage).
narrative_ontology:cs_interpretation_layer_present('2ea1712e-dc9e-401f-8d9b-57205fac2985').
narrative_ontology:cs_reading_relation('2ea1712e-dc9e-401f-8d9b-57205fac2985', substance_control_authority__prohibition_reading, forecloses).
narrative_ontology:cs_reading_relation('2ea1712e-dc9e-401f-8d9b-57205fac2985', substance_control_authority__harm_reduction_reading, influences).
narrative_ontology:cs_axiom('2ea1712e-dc9e-401f-8d9b-57205fac2985', foundational, legal_commerce_superior_to_prohibition).
narrative_ontology:cs_axiom_status(legal_commerce_superior_to_prohibition, holdable).
narrative_ontology:cs_axiom_grounding('2ea1712e-dc9e-401f-8d9b-57205fac2985', legal_commerce_superior_to_prohibition, instrumental).
narrative_ontology:cs_axiom('2ea1712e-dc9e-401f-8d9b-57205fac2985', secondary, licensed_market_quality_guarantee).
narrative_ontology:cs_axiom_status(licensed_market_quality_guarantee, holdable).
narrative_ontology:cs_axiom_grounding('2ea1712e-dc9e-401f-8d9b-57205fac2985', licensed_market_quality_guarantee, empirically_contingent).
narrative_ontology:cs_reference_frame('2ea1712e-dc9e-401f-8d9b-57205fac2985', state_regulated_commerce).
narrative_ontology:cs_drift_state('2ea1712e-dc9e-401f-8d9b-57205fac2985', contemporary_post_legalization, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('2ea1712e-dc9e-401f-8d9b-57205fac2985', '').
narrative_ontology:cs_kernel_id(substance_control_authority__legalization_reading, substance_control_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(substance_control_authority__legalization_reading, licensed_vendors).
narrative_ontology:constraint_beneficiary(substance_control_authority__legalization_reading, drug_consumers).
narrative_ontology:constraint_beneficiary(substance_control_authority__legalization_reading, affected_communities).
narrative_ontology:constraint_victim(substance_control_authority__legalization_reading, black_market_operators).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(substance_control_authority__legalization_reading, drug_consumers).
narrative_ontology:constraint_victim(substance_control_authority__legalization_reading, affected_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Designs licensing regimes, product quality standards, taxation schedules, and enforcement priorities. Captures tax revenue and exercises discretion over market structure, including the number and location of licensed vendors.
narrative_ontology:constraint_stakeholder(substance_control_authority__legalization_reading, state_regulator, agenda_setter,
    institutional, generational, arbitrage, national).

% Operate legally under state-issued licenses, benefiting from reduced black-market competition and state-enforced barriers to entry. Pay taxes and compliance costs, but gain protected market access and profit margins.
narrative_ontology:constraint_stakeholder(substance_control_authority__legalization_reading, licensed_vendors, beneficiary,
    powerful, biographical, constrained, national).

% Access quality-controlled substances through licensed channels, avoiding criminalization and adulterated supply. Pay embedded taxes and regulated prices; dependent users face limited exit from the regulatory channel itself.
narrative_ontology:constraint_stakeholder(substance_control_authority__legalization_reading, drug_consumers, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(substance_control_authority__legalization_reading, drug_consumers, payer).

% Previously supplied the market; now targeted by law enforcement for operating outside the licensing regime. Face asset confiscation, prosecution, and exclusion from the formal economy, with high barriers to obtaining licenses.
narrative_ontology:constraint_stakeholder(substance_control_authority__legalization_reading, black_market_operators, payer,
    organized, immediate, trapped, national).

% Experience reduced drug-related violence and street-level disorder due to market formalization, but bear potential externalities from increased visibility of use, advertising, and concentration of retail outlets.
narrative_ontology:constraint_stakeholder(substance_control_authority__legalization_reading, affected_communities, beneficiary,
    organized, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(substance_control_authority__legalization_reading, affected_communities, payer).

% Monitor substance use trends, product potency, and health outcomes. Advise on regulatory adjustments but do not directly collect revenue or bear enforcement costs through the constraint.
narrative_ontology:constraint_stakeholder(substance_control_authority__legalization_reading, public_health_authorities, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a legal, regulated market for psychoactive substances in which product quality is verified, access is channeled through licensed outlets, and consumer safety is separated from criminal supply chains.
% TRANSFER_FUNCTION: Moves tax revenue and licensing fees from consumers and licensed vendors to the state, and transfers market share and economic surplus from unlicensed operators to licensed vendors under state protection.
% ABSENT_VOICES: Prohibitionist constituencies who view any legalization as moral hazard; unlicensed traditional or small-scale producers excluded by licensing capital requirements; future dependent users whose voices are not represented in initial policy design.
% DISAPPEARANCE_RATIONALE: If the regulatory framework vanished overnight, licensed vendors would lose legal protection and banking access, consumers would return to unregulated supply, black market operators would re-enter, and the state's revenue and enforcement apparatus would lose its primary leverage over the substance market.
% FOUNDING_PROBLEM: Criminal drug markets produce violence, unsafe products, and mass incarceration while failing to reduce availability; the state needed a mechanism to separate drug commerce from criminal organization and protect consumers from adulterated supply.
% FOUNDING_PROBLEM_CORROBORATION: Public health researchers and former law enforcement officials outside the tax-collection apparatus attest that unregulated supply caused measurable harms. However, prohibitionist advocates and some community organizations contest that the founding problem is best solved by legalization, citing potential use-volume increases.
narrative_ontology:disappearance_verdict(substance_control_authority__legalization_reading, world_rearranges).
narrative_ontology:founding_problem_status(substance_control_authority__legalization_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(substance_control_authority__legalization_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(substance_control_authority__legalization_reading, 'none', 1).
narrative_ontology:epsilon_provenance(substance_control_authority__legalization_reading, 0.55, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(substance_control_authority__legalization_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(substance_control_authority__legalization_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(substance_control_authority__legalization_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.55) reflects the tax and licensing transfer plus the market exclusion of unlicensed sellers. Suppression (0.52 at interval end) captures the persistent need to suppress the unlicensed market to maintain licensed-market viability and price levels. Theater ratio (0.28) acknowledges some performative compliance and revenue-protection enforcement, but the core quality-control function remains operational. Accessibility collapse (0.45) shows unlicensed alternatives are partially closed by enforcement but not eliminated. Resistance (0.40) reflects ongoing black-market adaptation and prohibitionist political opposition.
 *
 * PERSPECTIVAL GAP:
 *   Licensed vendors and consumers experience the constraint as coordination (safe supply, legal commerce), while black market operators experience it as extraction backed by state violence. The state sees policy implementation. The engine should compute divergent seat classifications from these structural positions without reconciling them to the authored claim.
 *
 * DIRECTIONALITY LOGIC:
 *   The state regulator sits near the beneficiary end as agenda-setter and tax collector. Licensed vendors are clear beneficiaries (d low). Consumers are mixed (d mid) because they receive safety benefits while paying tax-embedded prices. Black market operators are the clearest targets (d high) due to active enforcement and exclusion. Affected communities sit near symmetric, experiencing both crime-reduction benefits and use-volume costs.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâcriminal market violence and unsafe supplyâis partially solved, but the constraint persists as a revenue and regulatory apparatus with concentrated beneficiaries. It has not atrophied into a piton because the coordination function remains substantial, beneficiaries actively maintain it, and the cost of fixing (dismantling the market) is politically significant. The tension between public health coordination and revenue extraction creates drift risk captured in the temporal measurements.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legalization_kernel_reading_contest,
    'Does this constraint represent genuine coordination through market regulation, or is it primarily a revenue-extraction mechanism layered over a public health coordination story?',
    'Cross-jurisdictional comparison of tax-rate levels versus health outcomes; structural analysis of whether regulatory barriers exceed what quality control requires.',
    'If extraction dominates, classification shifts toward snare; if coordination dominates, it remains tangled_rope or shifts toward rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legalization_kernel_reading_contest, conceptual, 'Core ambiguity of the legalization reading within the substance control kernel.').

omega_variable(
    unlicensed_market_suppression_mechanism,
    'Is suppression of the unlicensed market necessary for consumer safety, or does it primarily protect licensed vendor profitability and state tax revenue?',
    'Natural experiments where enforcement intensity varies independently of product safety standards; measurement of adulteration rates in jurisdictions with lower suppression.',
    'If suppression is safety-necessary, extraction is lower; if it is rent-protection, extraction is higher and directionality toward black market operators is more severe.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(unlicensed_market_suppression_mechanism, empirical, 'Whether black-market enforcement serves safety or revenue protection.').

omega_variable(
    kernel_sibling_foreclosure,
    'Does the legalization reading''s core premise (state-regulated legal commerce) logically foreclose the prohibition reading''s premise (criminalization of use), or do they coexist as policy alternatives?',
    'Historical analysis of jurisdictions that have reversed legalization; examination of whether a single legal framework can simultaneously hold criminalization for some substances and legalization for others.',
    'If foreclosed, the kernel exhibits irreversibility; if coexisting, the constraint''s persistence depends on political contingencies rather than structural lock-in.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_sibling_foreclosure, conceptual, 'Logical relationship between legalization and prohibition readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(substance_control_authority__legalization_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(subs_tr_t0, substance_control_authority__legalization_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(subs_tr_t4, substance_control_authority__legalization_reading, theater_ratio, 4, 0.2).
narrative_ontology:measurement(subs_tr_t8, substance_control_authority__legalization_reading, theater_ratio, 8, 0.25).
narrative_ontology:measurement(subs_tr_t12, substance_control_authority__legalization_reading, theater_ratio, 12, 0.3).
narrative_ontology:measurement(subs_tr_t16, substance_control_authority__legalization_reading, theater_ratio, 16, 0.32).
narrative_ontology:measurement(subs_tr_t20, substance_control_authority__legalization_reading, theater_ratio, 20, 0.3).
narrative_ontology:measurement(subs_tr_t24, substance_control_authority__legalization_reading, theater_ratio, 24, 0.28).

% Extraction over time
narrative_ontology:measurement(subs_be_t0, substance_control_authority__legalization_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(subs_be_t4, substance_control_authority__legalization_reading, base_extractiveness, 4, 0.45).
narrative_ontology:measurement(subs_be_t8, substance_control_authority__legalization_reading, base_extractiveness, 8, 0.52).
narrative_ontology:measurement(subs_be_t12, substance_control_authority__legalization_reading, base_extractiveness, 12, 0.58).
narrative_ontology:measurement(subs_be_t16, substance_control_authority__legalization_reading, base_extractiveness, 16, 0.6).
narrative_ontology:measurement(subs_be_t20, substance_control_authority__legalization_reading, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(subs_be_t24, substance_control_authority__legalization_reading, base_extractiveness, 24, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(subs_su_t0, substance_control_authority__legalization_reading, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(subs_su_t4, substance_control_authority__legalization_reading, suppression_requirement, 4, 0.65).
narrative_ontology:measurement(subs_su_t8, substance_control_authority__legalization_reading, suppression_requirement, 8, 0.6).
narrative_ontology:measurement(subs_su_t12, substance_control_authority__legalization_reading, suppression_requirement, 12, 0.55).
narrative_ontology:measurement(subs_su_t16, substance_control_authority__legalization_reading, suppression_requirement, 16, 0.58).
narrative_ontology:measurement(subs_su_t20, substance_control_authority__legalization_reading, suppression_requirement, 20, 0.55).
narrative_ontology:measurement(subs_su_t24, substance_control_authority__legalization_reading, suppression_requirement, 24, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(substance_control_authority__legalization_reading, resource_allocation).
narrative_ontology:affects_constraint(substance_control_authority__legalization_reading, substance_control_authority__prohibition_reading).
narrative_ontology:affects_constraint(substance_control_authority__legalization_reading, substance_control_authority__harm_reduction_reading).

% DUAL FORMULATION NOTE:
% This constraint is the legalization reading of the substance_control_authority kernel, decomposed from the prohibition and harm_reduction readings due to structurally distinct epsilon values, beneficiary/victim structures, and coordination mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
