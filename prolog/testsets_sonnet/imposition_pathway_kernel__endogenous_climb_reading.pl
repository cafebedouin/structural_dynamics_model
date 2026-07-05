% ============================================================================
% CONSTRAINT STORY: imposition_pathway_kernel__endogenous_climb_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_imposition_pathway_kernel__endogenous_climb_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: imposition_pathway_kernel__endogenous_climb_reading
 *   human_readable: Endogenous Climb Reading of Commitment Displacement (Meiji Calendar/Dress Reform)
 *   domain: historical_sociology/state_formation
 *
 * SUMMARY:
 *   This story instantiates the endogenous_climb_reading of the
 *   imposition_pathway_kernel, applied to the Meiji government's 1873
 *   calendar reform and associated dress conventions. On this reading, what
 *   appears in state historiography as a sudden top-down decree is actually
 *   the compressed, visible tip of a climb that began years earlier and
 *   invisibly in treaty-port commerce and military modernization. The
 *   decree's function was to ratify and generalize an already-underway fringe
 *   adoption, not to originate a new commitment from a standing start. Rising
 *   theater_ratio after 1873 reflects the state's decree performing a
 *   visibility function (dramatizing 'top-down' authorship) that exceeds its
 *   actual causal contribution to the underlying practice shift, which was
 *   already substantially climbed.
 *
 * KEY AGENTS:
 *   - treaty_port_merchants: primary beneficiary/originator of fringe climb (moderate/arbitrage)
 *   - military_modernizers: secondary fringe-climb originator (organized/constrained)
 *   - meiji_state_bureaucracy: agenda_setter/ratifier (institutional/mobile)
 *   - rural_agricultural_communities: payer, late/non-climb population (powerless/trapped)
 *   - traditionalist_court_officials: excluded objector (moderate/constrained)
 *   - historical_sociologists: analytical observer
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(imposition_pathway_kernel__endogenous_climb_reading, 0.28).
domain_priors:suppression_score(imposition_pathway_kernel__endogenous_climb_reading, 0.22).
domain_priors:theater_ratio(imposition_pathway_kernel__endogenous_climb_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(imposition_pathway_kernel__endogenous_climb_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(imposition_pathway_kernel__endogenous_climb_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(imposition_pathway_kernel__endogenous_climb_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(imposition_pathway_kernel__endogenous_climb_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(imposition_pathway_kernel__endogenous_climb_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(imposition_pathway_kernel__endogenous_climb_reading, rope).
narrative_ontology:human_readable(imposition_pathway_kernel__endogenous_climb_reading, "Endogenous Climb Reading of Commitment Displacement (Meiji Calendar/Dress Reform)").
narrative_ontology:topic_domain(imposition_pathway_kernel__endogenous_climb_reading, "historical_sociology/state_formation").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(imposition_pathway_kernel__endogenous_climb_reading, 'fdfa8878-67a3-4272-a537-32e15efd1929').
narrative_ontology:cs_kernel_codification('fdfa8878-67a3-4272-a537-32e15efd1929', distributed).
narrative_ontology:cs_authority_grounding('fdfa8878-67a3-4272-a537-32e15efd1929', practice).
narrative_ontology:cs_interpretation_layer_present('fdfa8878-67a3-4272-a537-32e15efd1929').
narrative_ontology:cs_reading_relation('fdfa8878-67a3-4272-a537-32e15efd1929', imposition_pathway_kernel__exogenous_override_reading, forecloses).
narrative_ontology:cs_reading_relation('fdfa8878-67a3-4272-a537-32e15efd1929', imposition_pathway_kernel__hybrid_cascade_reading, influences).
narrative_ontology:cs_axiom('fdfa8878-67a3-4272-a537-32e15efd1929', foundational, no_commitment_displacement_without_prior_fringe_climb).
narrative_ontology:cs_axiom_status(no_commitment_displacement_without_prior_fringe_climb, holdable).
narrative_ontology:cs_axiom_grounding('fdfa8878-67a3-4272-a537-32e15efd1929', no_commitment_displacement_without_prior_fringe_climb, empirically_contingent).
narrative_ontology:cs_axiom('fdfa8878-67a3-4272-a537-32e15efd1929', secondary, state_decree_is_ratification_not_origination).
narrative_ontology:cs_axiom_status(state_decree_is_ratification_not_origination, holdable).
narrative_ontology:cs_axiom_grounding('fdfa8878-67a3-4272-a537-32e15efd1929', state_decree_is_ratification_not_origination, empirically_contingent).
narrative_ontology:cs_reference_frame('fdfa8878-67a3-4272-a537-32e15efd1929', treaty_port_fringe_climb_baseline).
narrative_ontology:cs_drift_state('fdfa8878-67a3-4272-a537-32e15efd1929', post_1873_decree_formalization, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('fdfa8878-67a3-4272-a537-32e15efd1929', '').
narrative_ontology:cs_kernel_id(imposition_pathway_kernel__endogenous_climb_reading, imposition_pathway_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(imposition_pathway_kernel__endogenous_climb_reading, treaty_port_merchants).
narrative_ontology:constraint_beneficiary(imposition_pathway_kernel__endogenous_climb_reading, military_modernizers).
narrative_ontology:constraint_beneficiary(imposition_pathway_kernel__endogenous_climb_reading, meiji_state_bureaucracy).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(imposition_pathway_kernel__endogenous_climb_reading, rural_agricultural_communities).
narrative_ontology:constraint_vindicates(imposition_pathway_kernel__endogenous_climb_reading, fringe_adoption_universality_thesis).
narrative_ontology:constraint_vindicates(imposition_pathway_kernel__endogenous_climb_reading, state_decree_as_ratification_not_origination).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Adopted Western dress, timekeeping, and calendar conventions years before the 1873 decree because they transacted daily with foreign merchants in Yokohama and Kobe. Their early adoption was a competitive advantage in cross-border trade, not compliance with a rule that did not yet exist. They benefit from being read as forerunners rather than as targets of imposition.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__endogenous_climb_reading, treaty_port_merchants, beneficiary,
    moderate, biographical, arbitrage, regional).

% Officers and administrators who studied European military organization adopted Western drill schedules, ranks, and the Gregorian calendar's operational logic for coordination with foreign advisors well before the civil calendar reform. Their fringe practice became the template the state later formalized nationally.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__endogenous_climb_reading, military_modernizers, beneficiary,
    organized, generational, constrained, national).

% Issued the 1873 decree replacing the lunisolar calendar with the Gregorian calendar and promoting Western dress in official contexts. On this reading, the decree is a formalization instrument: it names and generalizes practices already climbing through merchant and military fringes rather than initiating a wholly new commitment. Its exit option is 'mobile' because the bureaucracy could have chosen a slower ratification path without structural cost.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__endogenous_climb_reading, meiji_state_bureaucracy, agenda_setter,
    institutional, generational, mobile, national).

% Continued to organize planting, festivals, and debt cycles around the lunisolar calendar for decades after the decree, absorbing the administrative cost of dual bookkeeping when interacting with state offices. On this reading they are late-climb or non-climb populations whose delayed adoption is evidence the change traveled upward from fringe practice rather than descending uniformly from the state.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__endogenous_climb_reading, rural_agricultural_communities, payer,
    powerless, biographical, trapped, local).

% Objected to abandoning the lunisolar calendar's cosmological and ritual functions but were not consulted in a decree process that, on this reading, was already responding to an established fringe trajectory rather than opening a genuine policy debate. Their objection is recorded but treated as resistance to a climb already underway, not as a live alternative path.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__endogenous_climb_reading, traditionalist_court_officials, excluded,
    moderate, biographical, constrained, national).

% Examine the documentary record of pre-decree adoption in treaty ports and military units to adjudicate whether the 1873 decree originated or ratified the calendar/dress shift. Their evidentiary standard is the pre-decree adoption timeline itself.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__endogenous_climb_reading, historical_sociologists, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Synchronizes administrative, commercial, and military timekeeping/dress conventions with the dominant international system that treaty-port trade and military modernization already required at the fringe, reducing translation and coordination costs across an emerging climb.
% TRANSFER_FUNCTION: Formalizes status and legitimacy from early-adopting fringe groups (merchants, military modernizers) onto the state's own calendar and dress conventions; transfers administrative burden onto late-adopting rural populations who must now reconcile two temporal systems.
% ABSENT_VOICES: Traditionalist court officials and rural communities whose ritual and agricultural calendars were displaced were not treated as parties to a negotiation — on this reading because the decree was experienced by the state as ratifying an existing climb, not opening a decision.
% DISAPPEARANCE_RATIONALE: If the 1873 decree had never been issued, this reading holds the treaty-port and military fringe practices would have continued climbing regardless, eventually reaching a tipping point requiring only later, less centralized formalization — the world looks similar but the timeline and locus of formalization shift. The sibling readings dispute this counterfactual sharply, which is why the verdict is contested rather than settled.
% FOUNDING_PROBLEM: Japan's negotiators and merchants operating under 'unequal treaty' pressure needed calendrical and sartorial compatibility with Western commercial and diplomatic partners to be treated as a modern sovereign equal; this compatibility problem was first solved informally at the treaty-port and military fringe before the state generalized it.
% FOUNDING_PROBLEM_CORROBORATION: Economic historians of the treaty-port system (working from customs records and merchant correspondence, outside both the Meiji state's own historiography and the beneficiary merchant guilds) attest that Western calendar and dress conventions were in active commercial use in Yokohama and Kobe years before 1873, corroborating the pre-decree climb independent of the bureaucracy's own founding narrative.
narrative_ontology:disappearance_verdict(imposition_pathway_kernel__endogenous_climb_reading, contested).
narrative_ontology:founding_problem_status(imposition_pathway_kernel__endogenous_climb_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(imposition_pathway_kernel__endogenous_climb_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(imposition_pathway_kernel__endogenous_climb_reading, 'none', 1).
narrative_ontology:epsilon_provenance(imposition_pathway_kernel__endogenous_climb_reading, 0.28, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(imposition_pathway_kernel__endogenous_climb_reading_tests).
:- end_tests(imposition_pathway_kernel__endogenous_climb_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low-to-moderate (0.28 at interval end) because on this reading the coordination benefit (compatibility with international commerce and diplomacy) was real and substantially captured by the fringe adopters themselves before the state acted; extraction from rural populations exists (dual-calendar administrative burden) but is a byproduct of formalization timing, not a designed rent. Theater_ratio rises sharply at 1873 (0.15 to 0.45) because the decree's dramatized 'sudden reform' framing performs authorship of a change that was already substantially climbed — this is the reading's central empirical claim rendered as a measurement discontinuity.
 *
 * PERSPECTIVAL GAP:
 *   From the treaty-port merchant seat, the 1873 decree changes almost nothing operationally — it merely stops requiring dual bookkeeping with foreign partners. From the rural community seat, the same decree is experienced as an abrupt, externally imposed disruption to agricultural and ritual timing. The engine should compute these as structurally different exposures to the same event, which is exactly what the beneficiary/payer split with differentiated exit_options is designed to produce.
 *
 * DIRECTIONALITY LOGIC:
 *   Treaty-port merchants and military modernizers sit near the beneficiary end: they climbed first, for their own advantage, and the state's later ratification cemented their early-mover position as normative. Rural communities sit toward the target end: they bear the administrative and cultural cost of a calendar shift they did not originate and adopted last, under a decree whose legitimacy on this reading rests entirely on a climb they were not party to. The state bureaucracy is coded as agenda_setter with mobile exit because, on this reading, it had latitude in how and when to formalize an already-existing practice rather than being forced to impose from zero.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling the calendar reform as pure top-down extraction (a snare imposed by state fiat) by insisting the coordination function was substantially achieved by fringe actors before the state's involvement — the state's mandate to 'reform the calendar' was largely already fulfilled by treaty-port and military practice, making the decree closer to ratification-of-fact than to founding-imposition. This matters for mandatrophy: if the state's decree is read as pure origination, the ongoing 'reform' apparatus (theatrical announcement, ministerial credit-claiming) looks like a functioning coordination mechanism; under this reading, the apparatus's declared founding problem is understood as already substantially solved at the moment of decree, sharpening the founding_problem_status of 'dead' relative to the sibling readings.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    pre_decree_adoption_evidentiary_sufficiency,
    'Does the documentary record of pre-1873 Western dress/calendar use in treaty ports and military units establish a genuine ''climb'' (sustained, self-propagating adoption trajectory) or merely scattered elite contact insufficient to support the endogenous_climb_reading''s claim that the decree ratified rather than originated the shift?',
    'Quantitative analysis of customs records, merchant correspondence, and military administrative records for density and trajectory of pre-1873 Western calendar/dress use, compared against a null model of scattered elite contact without self-propagation.',
    'If the pre-decree record shows only scattered elite contact rather than a genuine climb trajectory, this reading''s core premise weakens and the exogenous_override_reading gains evidentiary support for this specific case, even though the two readings remain live for the kernel generally.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pre_decree_adoption_evidentiary_sufficiency, empirical, 'Whether pre-1873 fringe adoption constitutes a genuine climb or scattered contact.').

omega_variable(
    counterfactual_state_capacity_ceiling,
    'Could the Meiji state have issued the 1873 decree successfully in the absence of any prior treaty-port or military fringe adoption, given its administrative capacity at the time?',
    'Comparative analysis against contemporaneous state calendar/dress reforms attempted without documented prior fringe adoption (e.g., other Meiji-era social reforms that failed or required decades of enforcement), assessing whether state capacity alone was sufficient.',
    'If comparable reforms without prior fringe climb succeeded through capacity alone, this reading''s claim of climb-dependency is undermined in favor of exogenous_override_reading; if comparable reforms without prior climb consistently failed or required generations of enforcement, this reading''s premise is strengthened.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(counterfactual_state_capacity_ceiling, empirical, 'Whether state capacity alone, without fringe climb, would have sufficed for successful reform.').

omega_variable(
    kernel_framing_choice_disclosure,
    'Is the choice to read the 1873 decree as ratification-of-climb rather than as origination-event itself a defensible single framing, or does the same evidentiary record equally support the hybrid_cascade_reading''s claim that state-mandated fringes (soldiers, officials) did most of the actual climbing after 1873?',
    'Track post-1873 adoption rates separately for state-employee populations (mandated into Western dress by employment) versus non-state populations (voluntary adoption); if state-employee adoption dominates the aggregate climb curve, the hybrid reading is better supported for the aggregate national outcome even if this reading holds for the pre-1873 treaty-port/military-modernizer subset.',
    'Would not eliminate this reading (the pre-1873 fringe climb among merchants and military modernizers is independently documented) but would clarify that this reading''s explanatory scope is the initiating stage only, with the hybrid reading better explaining the national-scale completion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_choice_disclosure, conceptual, 'Whether this reading and the hybrid_cascade_reading divide the case by temporal stage rather than genuinely competing over the same evidence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(imposition_pathway_kernel__endogenous_climb_reading, 1859, 1889).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(impo_tr_t1859, imposition_pathway_kernel__endogenous_climb_reading, theater_ratio, 1859, 0.1).
narrative_ontology:measurement_basis(impo_tr_t1859, observed).
narrative_ontology:measurement(impo_tr_t1865, imposition_pathway_kernel__endogenous_climb_reading, theater_ratio, 1865, 0.15).
narrative_ontology:measurement_basis(impo_tr_t1865, observed).
narrative_ontology:measurement(impo_tr_t1873, imposition_pathway_kernel__endogenous_climb_reading, theater_ratio, 1873, 0.45).
narrative_ontology:measurement_basis(impo_tr_t1873, observed).
narrative_ontology:measurement(impo_tr_t1879, imposition_pathway_kernel__endogenous_climb_reading, theater_ratio, 1879, 0.42).
narrative_ontology:measurement_basis(impo_tr_t1879, observed).
narrative_ontology:measurement(impo_tr_t1884, imposition_pathway_kernel__endogenous_climb_reading, theater_ratio, 1884, 0.4).
narrative_ontology:measurement_basis(impo_tr_t1884, observed).
narrative_ontology:measurement(impo_tr_t1889, imposition_pathway_kernel__endogenous_climb_reading, theater_ratio, 1889, 0.4).
narrative_ontology:measurement_basis(impo_tr_t1889, observed).

% Extraction over time
narrative_ontology:measurement(impo_be_t1859, imposition_pathway_kernel__endogenous_climb_reading, base_extractiveness, 1859, 0.12).
narrative_ontology:measurement_basis(impo_be_t1859, observed).
narrative_ontology:measurement(impo_be_t1865, imposition_pathway_kernel__endogenous_climb_reading, base_extractiveness, 1865, 0.16).
narrative_ontology:measurement_basis(impo_be_t1865, observed).
narrative_ontology:measurement(impo_be_t1873, imposition_pathway_kernel__endogenous_climb_reading, base_extractiveness, 1873, 0.24).
narrative_ontology:measurement_basis(impo_be_t1873, observed).
narrative_ontology:measurement(impo_be_t1879, imposition_pathway_kernel__endogenous_climb_reading, base_extractiveness, 1879, 0.27).
narrative_ontology:measurement_basis(impo_be_t1879, observed).
narrative_ontology:measurement(impo_be_t1884, imposition_pathway_kernel__endogenous_climb_reading, base_extractiveness, 1884, 0.28).
narrative_ontology:measurement_basis(impo_be_t1884, observed).
narrative_ontology:measurement(impo_be_t1889, imposition_pathway_kernel__endogenous_climb_reading, base_extractiveness, 1889, 0.28).
narrative_ontology:measurement_basis(impo_be_t1889, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(imposition_pathway_kernel__endogenous_climb_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(imposition_pathway_kernel__endogenous_climb_reading, identity_coordination).
narrative_ontology:affects_constraint(imposition_pathway_kernel__endogenous_climb_reading, imposition_pathway_kernel__exogenous_override_reading).
narrative_ontology:affects_constraint(imposition_pathway_kernel__endogenous_climb_reading, imposition_pathway_kernel__hybrid_cascade_reading).

% DUAL FORMULATION NOTE:
% This story is one of three readings of the imposition_pathway_kernel, all applied to the same historical case (Meiji calendar/dress reform) to test which causal ordering of fringe-adoption and state-decree best explains commitment displacement generally. endogenous_climb_reading (this file) holds fringe climb precedes and causes decree; exogenous_override_reading holds state capacity displaces commitments independent of any climb; hybrid_cascade_reading holds decree manufactures an artificial fringe that then climbs organically. Each reading is authored as its own ε-invariant constraint with its own beneficiary/victim structure and its own claimed_type, per the ε-invariance decomposition principle. They are linked bidirectionally via network.affects_constraints so contamination/coupling analysis can compare how the same underlying historical record supports structurally distinct classifications.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
