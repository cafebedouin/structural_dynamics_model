% ============================================================================
% CONSTRAINT STORY: procedural_legitimacy_decay
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-16
% ============================================================================

:- module(constraint_procedural_legitimacy_decay, []).

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
 * * constraint_id: procedural_legitimacy_decay
 * human_readable: The Hollow Formality Trap
 * domain: social/political
 * * SUMMARY:
 * This constraint represents a bureaucratic process that has lost its
 * justifying social consensus but maintains its legal force. As the gap
 * between 'process' and 'result' grows, the system transitions from a
 * coordination 'Rope' into an extractive 'Snare' or an inertial 'Piton'.
 * * KEY AGENTS:
 * - Applicant: Subject (Powerless)
 * - Administrative Body: Beneficiary (Institutional)
 * - Political Philosopher: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% High extraction (0.76) as participants must expend high effort for low-legitimacy outcomes.
domain_priors:base_extractiveness(procedural_legitimacy_decay, 0.76).
domain_priors:suppression_score(procedural_legitimacy_decay, 0.62).
domain_priors:theater_ratio(procedural_legitimacy_decay, 0.88). % Extreme theater triggers Piton classification.

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(procedural_legitimacy_decay, extractiveness, 0.76).
narrative_ontology:constraint_metric(procedural_legitimacy_decay, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(procedural_legitimacy_decay, theater_ratio, 0.88).

% Constraint self-claim (what does the constraint claim to be?)
narrative_ontology:constraint_claim(procedural_legitimacy_decay, tangled_rope).
narrative_ontology:human_readable(procedural_legitimacy_decay, "The Hollow Formality Trap").
narrative_ontology:topic_domain(procedural_legitimacy_decay, "social/political").

% Binary flags & Structural properties
% The process requires legal/administrative force to compel participation.
domain_priors:requires_active_enforcement(procedural_legitimacy_decay).

% Structural property derivation hooks for Tangled Rope classification.
narrative_ontology:constraint_beneficiary(procedural_legitimacy_decay, administrative_body).
narrative_ontology:constraint_victim(procedural_legitimacy_decay, applicant).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% For the powerless applicant, the decayed process is a snare that extracts
% resources (time, money, hope) with no legitimate return.
constraint_indexing:constraint_classification(procedural_legitimacy_decay, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% Institutional actors who administer the process view it as a vital
% coordination rope, essential for stability and order, regardless of its outcomes.
constraint_indexing:constraint_classification(procedural_legitimacy_decay, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% The observer sees both the coordination claim and the asymmetric extraction,
% classifying it as a Tangled Rope.
constraint_indexing:constraint_classification(procedural_legitimacy_decay, tangled_rope,
    context(agent_power(analytical),
            time_horizon(historical),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: THE SYSTEMS AUDITOR (PITON)
% A different analytical perspective, focused on function vs performance,
% sees the extreme theater ratio (0.88) and classifies it as a Piton—an
% inertial, non-functional process maintained for show.
constraint_indexing:constraint_classification(procedural_legitimacy_decay, piton,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(procedural_legitimacy_decay_tests).

test(perspectival_gap) :-
    % Verify the Snare vs Rope conflict between powerless and institutional agents.
    constraint_indexing:constraint_classification(procedural_legitimacy_decay, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(procedural_legitimacy_decay, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeInstitutional,
    TypePowerless == snare,
    TypeInstitutional == rope.

test(analytical_divergence) :-
    % Verify that different analytical perspectives yield different classifications (Tangled Rope vs Piton).
    constraint_indexing:constraint_classification(procedural_legitimacy_decay, tangled_rope, context(agent_power(analytical), time_horizon(historical), _, _)),
    constraint_indexing:constraint_classification(procedural_legitimacy_decay, piton, context(agent_power(analytical), time_horizon(civilizational), _, _)).

test(threshold_validation_high_extraction) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(procedural_legitimacy_decay, ExtMetricName, E),
    E >= 0.46.

:- end_tests(procedural_legitimacy_decay_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.76) reflects a process that has become a tax on
 * participation rather than a service of coordination. The high theater (0.88)
 * captures the performative nature of the bureaucracy, where following the
 * procedure is more important than achieving the outcome.
 *
 * * PERSPECTIVAL GAP:
 * To the 'powerless' applicant, the decay is a Snare—an obligatory, extractive
 * trap. To the 'institutional' actor whose role is defined by the process, it
 * remains a Rope—a necessary formal sequence for maintaining order.
 *
 * * [RESOLVED MANDATROPHY]:
 * The high extraction could be misread as a pure Snare. Mandatrophy is resolved
 * by the Tangled Rope classification, which correctly acknowledges that a
 * (now vestigial) coordination function exists alongside severe asymmetric
 * extraction. The additional Piton classification from a systems audit perspective
 * further refines this by identifying the mechanism of decay: functional atrophy
 * replaced by institutional theater.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Required for constraints with base_extractiveness > 0.46.
omega_variable(
    omega_legitimacy_pivot,
    'Is the decay caused by emergent procedural complexity or active bad-faith gatekeeping?',
    'Natural language audit of administrative outcome justifications vs documented intent.',
    'If gatekeeping: Snare. If complexity: Mountain of institutional debt.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(procedural_legitimacy_decay, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data models the decay of the process over time. It began as a
% functional, low-extraction mechanism and degraded into a performative,
% highly extractive one. This is required as base_extractiveness > 0.46.

% Theater ratio over time (triggers metric_substitution detection):
narrative_ontology:measurement(pld_tr_t0, procedural_legitimacy_decay, theater_ratio, 0, 0.20).
narrative_ontology:measurement(pld_tr_t5, procedural_legitimacy_decay, theater_ratio, 5, 0.65).
narrative_ontology:measurement(pld_tr_t10, procedural_legitimacy_decay, theater_ratio, 10, 0.88).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(pld_ex_t0, procedural_legitimacy_decay, base_extractiveness, 0, 0.30).
narrative_ontology:measurement(pld_ex_t5, procedural_legitimacy_decay, base_extractiveness, 5, 0.60).
narrative_ontology:measurement(pld_ex_t10, procedural_legitimacy_decay, base_extractiveness, 10, 0.76).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% The process, however decayed, functions as an enforcement mechanism,
% which enables Boltzmann purity analysis.
narrative_ontology:coordination_type(procedural_legitimacy_decay, enforcement_mechanism).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */