% ============================================================================
% CONSTRAINT STORY: digital_money_origin__first_held_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_digital_money_origin__first_held_reading, []).

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
 *   constraint_id: digital_money_origin__first_held_reading
 *   human_readable: Digital Money Emergence (First Held Reading)
 *   domain: monetary_history/technology_studies/institutional_economics
 *
 * SUMMARY:
 *   This constraint story analyzes the emergence of digital money through the
 *   lens of its 'first held' reading, focusing on the point when individuals
 *   began to practically use non-physical monetary instruments as reliable
 *   stores of value. This perspective emphasizes the role of individual
 *   adoption, technological infrastructure, and network effects in
 *   establishing digital money, rather than its conceptualization or formal
 *   regulatory recognition. The constraint itself is the set of conditions
 *   that made this practical holding possible and effective.
 *
 * KEY AGENTS:
 *   - digital_platform_operators: Primary beneficiary/agenda_setter (institutional/arbitrage) — benefits from widespread adoption, sets standards.
 *   - early_adopters: Primary beneficiary (moderate/mobile) — gains convenience, new capabilities.
 *   - individuals_without_infrastructure: Primary target (powerless/trapped) — bears cost of exclusion/inaccessibility.
 *   - traditional_financial_institutions: Secondary target (institutional/constrained) — bears cost of disruption, must adapt.
 *   - monetary_historians: Analytical observer (analytical/analytical) — analyzes the structural shift.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(digital_money_origin__first_held_reading, 0.35).
domain_priors:suppression_score(digital_money_origin__first_held_reading, 0.3).
domain_priors:theater_ratio(digital_money_origin__first_held_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(digital_money_origin__first_held_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(digital_money_origin__first_held_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(digital_money_origin__first_held_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(digital_money_origin__first_held_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(digital_money_origin__first_held_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(digital_money_origin__first_held_reading, rope).
narrative_ontology:human_readable(digital_money_origin__first_held_reading, "Digital Money Emergence (First Held Reading)").
narrative_ontology:topic_domain(digital_money_origin__first_held_reading, "monetary_history/technology_studies/institutional_economics").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(digital_money_origin__first_held_reading, '6ae812e8-bdc3-4ad3-817e-63109f8c13a3').
narrative_ontology:cs_kernel_codification('6ae812e8-bdc3-4ad3-817e-63109f8c13a3', implicit).
narrative_ontology:cs_authority_grounding('6ae812e8-bdc3-4ad3-817e-63109f8c13a3', practice).
narrative_ontology:cs_reading_relation('6ae812e8-bdc3-4ad3-817e-63109f8c13a3', digital_money_origin__became_thinkable_reading, influences).
narrative_ontology:cs_reading_relation('6ae812e8-bdc3-4ad3-817e-63109f8c13a3', digital_money_origin__regulatory_recognition_reading, coexists_with).
narrative_ontology:cs_axiom('6ae812e8-bdc3-4ad3-817e-63109f8c13a3', foundational, practical_utility_defines_money).
narrative_ontology:cs_axiom_status(practical_utility_defines_money, holdable).
narrative_ontology:cs_axiom_grounding('6ae812e8-bdc3-4ad3-817e-63109f8c13a3', practical_utility_defines_money, empirically_contingent).
narrative_ontology:cs_axiom('6ae812e8-bdc3-4ad3-817e-63109f8c13a3', secondary, individual_agency_in_monetary_evolution).
narrative_ontology:cs_axiom_status(individual_agency_in_monetary_evolution, holdable).
narrative_ontology:cs_axiom_grounding('6ae812e8-bdc3-4ad3-817e-63109f8c13a3', individual_agency_in_monetary_evolution, conventional).
narrative_ontology:cs_reference_frame('6ae812e8-bdc3-4ad3-817e-63109f8c13a3', individual_practical_acceptance).
narrative_ontology:cs_drift_state('6ae812e8-bdc3-4ad3-817e-63109f8c13a3', contemporary_digital_economy, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('6ae812e8-bdc3-4ad3-817e-63109f8c13a3', '').
narrative_ontology:cs_kernel_id(digital_money_origin__first_held_reading, digital_money_origin).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(digital_money_origin__first_held_reading, early_adopters).
narrative_ontology:constraint_beneficiary(digital_money_origin__first_held_reading, digital_platform_operators).
narrative_ontology:constraint_victim(digital_money_origin__first_held_reading, individuals_without_infrastructure).
narrative_ontology:constraint_victim(digital_money_origin__first_held_reading, traditional_financial_institutions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals who first adopted and practically used non-physical monetary instruments as reliable stores of value, gaining convenience, speed, and access to new digital economies. They could revert to physical cash but chose digital for its benefits.
narrative_ontology:constraint_stakeholder(digital_money_origin__first_held_reading, early_adopters, beneficiary,
    moderate, biographical, mobile, global).

% Entities (e.g., early payment processors, tech companies) that built and maintained the infrastructure enabling individuals to hold and transact with digital money. They benefited from network effects and transaction volumes, shaping the conditions of digital value holding.
narrative_ontology:constraint_stakeholder(digital_money_origin__first_held_reading, digital_platform_operators, agenda_setter,
    institutional, generational, arbitrage, global).

% Individuals lacking access to the necessary technology, internet connectivity, or financial literacy to participate in digital money systems. They bore the cost of exclusion from new economic opportunities and convenience, remaining reliant on traditional, often less efficient, monetary forms.
narrative_ontology:constraint_stakeholder(digital_money_origin__first_held_reading, individuals_without_infrastructure, payer,
    powerless, biographical, trapped, local).

% Banks and other legacy financial entities that initially faced disruption and competition from the emergence of digital money. They bore the cost of adapting their services, infrastructure, and business models to a new monetary landscape, or risked obsolescence.
narrative_ontology:constraint_stakeholder(digital_money_origin__first_held_reading, traditional_financial_institutions, payer,
    institutional, generational, constrained, national).

% Academics and researchers who study the evolution of monetary systems, analyzing the conditions, impacts, and societal shifts associated with the emergence of digital money from a detached, analytical perspective.
narrative_ontology:constraint_stakeholder(digital_money_origin__first_held_reading, monetary_historians, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enabled individuals to store and transfer value without physical instruments, facilitating faster, more convenient transactions, and new forms of commerce by establishing shared technical and social conventions for digital value representation.
% TRANSFER_FUNCTION: Transferred convenience, speed, and new economic capabilities to early adopters and digital platform operators, while imposing costs of exclusion, adaptation, or friction on individuals without infrastructure and traditional financial institutions.
% ABSENT_VOICES: Individuals without access to digital infrastructure or literacy were largely absent from the early discourse, and would have highlighted the digital divide and the unequal distribution of benefits and costs.
% DISAPPEARANCE_RATIONALE: If the ability for individuals to practically hold non-physical monetary instruments as stores of value vanished overnight, the global economy would fundamentally reorganize. E-commerce, global financial flows, and many modern services rely entirely on digital money, and their collapse would force a return to physical or highly localized exchange, with massive economic disruption.
% FOUNDING_PROBLEM: The limitations of physical cash for large-scale, rapid, or remote transactions, coupled with a desire for more efficient, secure, and convenient value transfer mechanisms.
% FOUNDING_PROBLEM_CORROBORATION: Technology developers, economists, and financial historians widely corroborate the ongoing limitations of purely physical monetary systems and the persistent demand for digital solutions, even as the specific forms of digital money evolve.
narrative_ontology:disappearance_verdict(digital_money_origin__first_held_reading, world_rearranges).
narrative_ontology:founding_problem_status(digital_money_origin__first_held_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(digital_money_origin__first_held_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(digital_money_origin__first_held_reading, 'none', 1).
narrative_ontology:epsilon_provenance(digital_money_origin__first_held_reading, 0.35, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(digital_money_origin__first_held_reading_tests).
:- end_tests(digital_money_origin__first_held_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Rope because its primary function is to coordinate a new, more efficient way of storing and transferring value. While it has costs (extractiveness 0.45 at end of interval) and creates barriers for some (suppression 0.40), these are largely due to the inherent friction of new technology adoption, network effects, and the cost of building infrastructure, rather than active coercion. The theater ratio remains low (0.10) as the practical holding of value is a functional, not performative, act. The metrics show a slight increase in extractiveness and suppression over time as digital systems became more entrenched and network effects solidified, creating higher barriers to entry for non-participants.
 *
 * PERSPECTIVAL GAP:
 *   The 'first held' reading emphasizes the bottom-up adoption and practical utility, which contrasts with the 'became thinkable' (conceptual) and 'regulatory recognition' (top-down institutional) readings. Each reading highlights different agents and different mechanisms of constraint, leading to distinct classifications and analyses.
 *
 * DIRECTIONALITY LOGIC:
 *   Digital platform operators and early adopters are clear beneficiaries, gaining new capabilities and economic advantages. Individuals without infrastructure are victims, bearing the cost of exclusion from these new systems. Traditional financial institutions are also victims, facing disruption and the imperative to adapt. The directionality reflects these structural positions, with beneficiaries experiencing low effective extraction (or subsidy) and victims experiencing higher effective extraction.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    extraction_source_ambiguity,
    'Is the measured extractiveness primarily from the inherent friction and cost of early digital systems, or from the network effects and platform control that emerged later?',
    'Detailed economic analysis comparing transaction costs and platform fees in early vs. mature digital money systems, accounting for technological advancements.',
    'If primarily from early friction, the extractiveness is a transient cost of innovation. If from network effects/platform control, it indicates a more persistent, potentially rent-seeking, structural feature.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_source_ambiguity, empirical, 'Distinguishing sources of extraction in digital money''s early phase.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression primarily structural (lack of infrastructure, technical barriers) or institutional (deliberate exclusion by early operators)?',
    'Comparative studies of digital money adoption in regions with varying infrastructure development and regulatory approaches to platform exclusivity.',
    'If structural, suppression is a challenge of development. If institutional, it points to active gatekeeping and potential anti-competitive practices.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. institutional suppression in digital money adoption.').

omega_variable(
    reading_origin_date_sensitivity,
    'How sensitive is the classification to the precise historical moment chosen for ''first held'' as a practical store of value?',
    'Sensitivity analysis by shifting the interval start date by +/- 5-10 years and re-evaluating base metrics and stakeholder positions.',
    'A high sensitivity would indicate that the ''first held'' concept is itself historically ambiguous, potentially requiring further decomposition or a more nuanced temporal model.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_origin_date_sensitivity, conceptual, 'Impact of ''first held'' definition on constraint classification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(digital_money_origin__first_held_reading, 1970, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(digi_tr_t1970, digital_money_origin__first_held_reading, theater_ratio, 1970, 0.1).
narrative_ontology:measurement(digi_tr_t1980, digital_money_origin__first_held_reading, theater_ratio, 1980, 0.1).
narrative_ontology:measurement(digi_tr_t1990, digital_money_origin__first_held_reading, theater_ratio, 1990, 0.1).
narrative_ontology:measurement(digi_tr_t2000, digital_money_origin__first_held_reading, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(digi_tr_t2010, digital_money_origin__first_held_reading, theater_ratio, 2010, 0.1).
narrative_ontology:measurement(digi_tr_t2020, digital_money_origin__first_held_reading, theater_ratio, 2020, 0.1).

% Extraction over time
narrative_ontology:measurement(digi_be_t1970, digital_money_origin__first_held_reading, base_extractiveness, 1970, 0.35).
narrative_ontology:measurement(digi_be_t1980, digital_money_origin__first_held_reading, base_extractiveness, 1980, 0.37).
narrative_ontology:measurement(digi_be_t1990, digital_money_origin__first_held_reading, base_extractiveness, 1990, 0.39).
narrative_ontology:measurement(digi_be_t2000, digital_money_origin__first_held_reading, base_extractiveness, 2000, 0.41).
narrative_ontology:measurement(digi_be_t2010, digital_money_origin__first_held_reading, base_extractiveness, 2010, 0.43).
narrative_ontology:measurement(digi_be_t2020, digital_money_origin__first_held_reading, base_extractiveness, 2020, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(digi_su_t1970, digital_money_origin__first_held_reading, suppression_requirement, 1970, 0.3).
narrative_ontology:measurement(digi_su_t1980, digital_money_origin__first_held_reading, suppression_requirement, 1980, 0.32).
narrative_ontology:measurement(digi_su_t1990, digital_money_origin__first_held_reading, suppression_requirement, 1990, 0.35).
narrative_ontology:measurement(digi_su_t2000, digital_money_origin__first_held_reading, suppression_requirement, 2000, 0.37).
narrative_ontology:measurement(digi_su_t2010, digital_money_origin__first_held_reading, suppression_requirement, 2010, 0.39).
narrative_ontology:measurement(digi_su_t2020, digital_money_origin__first_held_reading, suppression_requirement, 2020, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(digital_money_origin__first_held_reading, resource_allocation).
narrative_ontology:affects_constraint(digital_money_origin__first_held_reading, e_commerce_growth).
narrative_ontology:affects_constraint(digital_money_origin__first_held_reading, global_financial_flows).
narrative_ontology:affects_constraint(digital_money_origin__first_held_reading, digital_identity_systems).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
