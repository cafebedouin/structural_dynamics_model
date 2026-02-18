% ============================================================================
% CONSTRAINT STORY: challenger_o_ring_integrity
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-27
% ============================================================================

:- module(constraint_challenger_o_ring_integrity, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: challenger_o_ring_integrity
 * human_readable: Challenger SRB O-Ring Integrity Failure
 * domain: technological
 * * SUMMARY:
 * The 1986 Challenger disaster was caused by the failure of O-ring seals in the Solid Rocket
 * Boosters (SRBs) due to record-low temperatures on launch day. While the physical
 * limitation of the O-rings was a known 'Mountain' of physics, institutional pressure
 * from NASA management to maintain an aggressive launch schedule transformed this
 * manageable risk into a catastrophic 'Snare' for the crew.
 * * KEY AGENTS:
 * - STS-51-L Crew: Subject (Powerless)
 * - NASA Management: Beneficiary (Institutional)
 * - The Laws of Physics: Analytical (Mountain perspective)
 * - Systems Safety Auditor: Analytical (Snare perspective)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(challenger_o_ring_integrity, 0.80). % Extreme extraction of safety margins to meet schedule.
domain_priors:suppression_score(challenger_o_ring_integrity, 0.70).   % Engineering concerns were actively overridden by management.
domain_priors:theater_ratio(challenger_o_ring_integrity, 0.10).       % Low theater; this was a functional failure, not performative.

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(challenger_o_ring_integrity, extractiveness, 0.80).
narrative_ontology:constraint_metric(challenger_o_ring_integrity, suppression_requirement, 0.70).
narrative_ontology:constraint_metric(challenger_o_ring_integrity, theater_ratio, 0.10).

% Constraint self-claim (what does the constraint claim to be?)
% Management framed the decision as a manageable risk (coordination), but it was pure enforcement of schedule.
narrative_ontology:constraint_claim(challenger_o_ring_integrity, tangled_rope).
narrative_ontology:human_readable(challenger_o_ring_integrity, "Challenger SRB O-Ring Integrity Failure").
narrative_ontology:topic_domain(challenger_o_ring_integrity, "technological").

% Binary flags
domain_priors:requires_active_enforcement(challenger_o_ring_integrity). % Required active management override of engineering advice.

% Structural property derivation hooks:
narrative_ontology:constraint_beneficiary(challenger_o_ring_integrity, nasa_launch_schedule).
narrative_ontology:constraint_victim(challenger_o_ring_integrity, sts_51_l_crew).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   Power (P) and Scope (S) both affect effective extraction.
   ========================================================================== */

% PERSPECTIVE 1: THE STS-51-L CREW (SNARE)
% For the crew, the management decision to launch despite the cold transformed the
% technical risk into a fatal trap. They were powerless and trapped in a system
% where their safety was extracted in favor of the schedule.
constraint_indexing:constraint_classification(challenger_o_ring_integrity, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: NASA MANAGEMENT (ROPE)
% For management, the known O-ring issue was a risk to be managed—a 'Rope'.
% They perceived it as one of many technical challenges to be coordinated and
% balanced against immense pressure to maintain a frequent launch cadence for
% political and budgetary reasons.
constraint_indexing:constraint_classification(challenger_o_ring_integrity, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: THE LAWS OF PHYSICS (MOUNTAIN)
% From the perspective of the physical components, the relationship between
% temperature and material elasticity is an immutable 'Mountain'. Below a certain
% temperature, the O-rings will fail. This is a non-negotiable law of physics.
constraint_indexing:constraint_classification(challenger_o_ring_integrity, tangled_rope,
    context(agent_power(analytical),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER (SNARE)
% A post-hoc systems safety auditor, viewing the event with a global and
% civilizational lens, classifies it as a Snare. The high base extraction (0.8)
% and suppression (0.7) clearly indicate a system that trapped its victims by
% enforcing institutional goals over physical reality.
constraint_indexing:constraint_classification(challenger_o_ring_integrity, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(challenger_o_ring_integrity_tests).

test(perspectival_gap) :-
    % Verify there is a perspectival gap between the crew (powerless) and management (institutional).
    constraint_indexing:constraint_classification(challenger_o_ring_integrity, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(challenger_o_ring_integrity, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    TypePowerless == snare,
    TypeInstitutional == rope,
    TypePowerless \= TypeInstitutional.

test(threshold_validation) :-
    % Verify the high extraction and suppression scores are consistent with a Snare classification.
    narrative_ontology:constraint_metric(challenger_o_ring_integrity, extractiveness, E),
    narrative_ontology:constraint_metric(challenger_o_ring_integrity, suppression_requirement, S),
    E >= 0.46,
    S >= 0.60.

:- end_tests(challenger_o_ring_integrity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * This story is a classic example of how a known 'Mountain' (a physical limit)
 * is transformed into a deadly 'Snare' by institutional pressures. The scores
 * reflect this dynamic. Base extractiveness is 0.80, representing the complete
 * extraction of safety margins (and ultimately, life) to serve the institutional
 * goal of maintaining the launch schedule. Suppression is 0.70, as the concerns
 * of engineers who understood the 'Mountain' were actively overridden.
 *
 * The perspectival gap is stark: for management, it was a 'Rope' (a risk to be
 * managed); for the crew, it was a 'Snare' (a fatal, inescapable trap); for
 * physics, it was a 'Mountain' (an immutable law).
 *
 * * MANDATROPHY ANALYSIS:
 * [RESOLVED MANDATROPHY] The extreme extraction (0.80) is not a misclassification.
 * The system did not merely coordinate resources; it actively suppressed vital
 * safety information and extracted the lives of the crew to meet a schedule.
 * The analytical classification as a Snare correctly identifies this as a failure
 * of institutional process, not just a technical accident.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_challenger_o_ring_integrity,
    "At what point does incremental risk acceptance (normalization of deviance) transform a manageable technical challenge into a guaranteed catastrophic failure?",
    "Post-hoc analysis of safety waiver trends versus hardware failure rates across multiple missions.",
    "If a clear threshold exists, it could inform future safety protocols for complex systems. If not, it implies all such systems operate with an irreducible risk of sudden failure.",
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(challenger_o_ring_integrity, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data models the "normalization of deviance" within the Shuttle program,
% where schedule pressure gradually eroded safety margins over time.

% Theater ratio over time (remains low):
narrative_ontology:measurement(challenger_tr_t0, challenger_o_ring_integrity, theater_ratio, 0, 0.05).
narrative_ontology:measurement(challenger_tr_t5, challenger_o_ring_integrity, theater_ratio, 5, 0.08).
narrative_ontology:measurement(challenger_tr_t10, challenger_o_ring_integrity, theater_ratio, 10, 0.10).

% Extraction over time (shows increasing pressure and erosion of safety):
narrative_ontology:measurement(challenger_ex_t0, challenger_o_ring_integrity, base_extractiveness, 0, 0.20).
narrative_ontology:measurement(challenger_ex_t5, challenger_o_ring_integrity, base_extractiveness, 5, 0.50).
narrative_ontology:measurement(challenger_ex_t10, challenger_o_ring_integrity, base_extractiveness, 10, 0.80).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% No Boltzmann data is declared. The primary mechanism is not coordination but
% the enforcement of a schedule, overriding a known physical limit. The system's
% failure mode is better captured by the Snare classification than by purity metrics.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */