% ============================================================================
% CONSTRAINT STORY: institutional_mutation_domestication
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-27
% ============================================================================

:- module(constraint_institutional_mutation_domestication, []).

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
 * * constraint_id: institutional_mutation_domestication
 * human_readable: The Jedi Bureaucratic Capture
 * domain: political/social
 * * SUMMARY:
 * This constraint represents the process by which a high-agency "mutation" (the Jedi)
 * is integrated into a stable bureaucratic structure (the Galactic Republic). It functions
 * as a selective pressure that prioritizes institutional compliance over the
 * adaptive capacity (Force-sensitivity) that originally justified the Jedi's role.
 * The system provides genuine coordination (peacekeeping) but extracts the Jedi's
 * autonomy and ultimately their existence to maintain bureaucratic stability.
 * * KEY AGENTS:
 * - The Padawan (e.g., young Anakin): Subject (Powerless)
 * - The Jedi Council (e.g., Mace Windu): Beneficiary (Institutional)
 * - The System Auditor (e.g., Palpatine, or an external analyst): Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(institutional_mutation_domestication, 0.70). % Mountain <= 0.15, Rope <= 0.15, Snare >= 0.46
domain_priors:suppression_score(institutional_mutation_domestication, 0.80).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(institutional_mutation_domestication, 0.40).       % Piton detection (>= 0.70)

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(institutional_mutation_domestication, extractiveness, 0.70).
narrative_ontology:constraint_metric(institutional_mutation_domestication, suppression_requirement, 0.80).
narrative_ontology:constraint_metric(institutional_mutation_domestication, theater_ratio, 0.40).

% Constraint self-claim (what does the constraint claim to be?)
narrative_ontology:constraint_claim(institutional_mutation_domestication, tangled_rope).
narrative_ontology:human_readable(institutional_mutation_domestication, "The Jedi Bureaucratic Capture").
narrative_ontology:topic_domain(institutional_mutation_domestication, "political/social").

% Binary flags
domain_priors:requires_active_enforcement(institutional_mutation_domestication). % Required for Tangled Rope

% Structural property derivation hooks for Tangled Rope:
narrative_ontology:constraint_beneficiary(institutional_mutation_domestication, galactic_republic_bureaucracy).
narrative_ontology:constraint_victim(institutional_mutation_domestication, force_sensitive_innovators).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% For a young Padawan, the Jedi Order is an inescapable trap. They are taken as
% children, their identity is tied to the institution, and deviation is punished.
% The high extraction (lifelong service) and suppression (no alternatives)
% are felt directly.
constraint_indexing:constraint_classification(institutional_mutation_domestication, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The Jedi Council, as institutional actors, view their rules and allegiance to
% the Senate as a necessary coordination mechanism (Rope) for galactic peace.
% They do not perceive the extraction, seeing it as willing service.
constraint_indexing:constraint_classification(institutional_mutation_domestication, rope,
    context(agent_power(institutional),
            time_horizon(historical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% An analyst sees both the genuine coordination function (peacekeeping) and the
% asymmetric extraction (consuming the Jedi's autonomy and potential). The need
% for active enforcement confirms it is not pure coordination. It is a Tangled Rope.
constraint_indexing:constraint_classification(institutional_mutation_domestication, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(institutional_mutation_domestication_tests).

test(perspectival_gap_council_vs_padawan) :-
    constraint_indexing:constraint_classification(institutional_mutation_domestication, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(institutional_mutation_domestication, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    assertion(TypePowerless == snare),
    assertion(TypeInstitutional == rope),
    assertion(TypePowerless \= TypeInstitutional).

test(analytical_resolution_is_tangled_rope) :-
    constraint_indexing:constraint_classification(institutional_mutation_domestication, TypeAnalytical, context(agent_power(analytical), _, _, _)),
    assertion(TypeAnalytical == tangled_rope).

test(tangled_rope_structural_properties_present) :-
    narrative_ontology:constraint_beneficiary(institutional_mutation_domestication, _),
    narrative_ontology:constraint_victim(institutional_mutation_domestication, _),
    domain_priors:requires_active_enforcement(institutional_mutation_domestication).

:- end_tests(institutional_mutation_domestication_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The scores reflect a system that began with a coordination purpose (Jedi as
 * peacekeepers) but evolved into an extractive one. The base extractiveness (0.70)
 * represents the Republic consuming the Jedi's adaptive capacity for its own
 * bureaucratic stability. The high suppression (0.80) reflects the active
 * discouragement of alternative Force traditions (e.g., Grey Jedi).
 *
 * The Perspectival Gap is stark: the Council (institutional) sees a Rope, essential
 * for order. The Padawan (powerless) experiences a Snare, an inescapable fate.
 *
 * * MANDATROPHY ANALYSIS: [RESOLVED MANDATROPHY]
 * The analytical classification as a Tangled Rope is critical. A simple Snare
 * classification would miss the genuine, if decaying, coordination function the
 * Jedi provided. This prevents the system from mislabeling a complex, co-opted
 * institution as pure, malicious extraction. It correctly identifies the hybrid
 * nature of the constraint.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_institutional_mutation_domestication,
    'Was the Jedi Order''s decline an inevitable result of its biological premise (midichlorians creating a rigid caste) or a contingent outcome of its political capture by the Senate?',
    'Analysis of other systems where high-skill groups were either integrated or remained autonomous, comparing their long-term adaptability.',
    'If inevitable (Mountain-like), then reform was impossible. If contingent (Tangled Rope), then alternative structures could have saved both the Jedi and the Republic.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(institutional_mutation_domestication, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This data models the gradual decay of the Jedi Order into a tool of the
% Republic. Extraction and performative theater increase as its original
% purpose is subverted.

% Theater ratio over time (metric_substitution):
narrative_ontology:measurement(imd_tr_t0, institutional_mutation_domestication, theater_ratio, 0, 0.10).
narrative_ontology:measurement(imd_tr_t5, institutional_mutation_domestication, theater_ratio, 5, 0.25).
narrative_ontology:measurement(imd_tr_t10, institutional_mutation_domestication, theater_ratio, 10, 0.40).

% Extraction over time (extraction_accumulation):
narrative_ontology:measurement(imd_ex_t0, institutional_mutation_domestication, base_extractiveness, 0, 0.50).
narrative_ontology:measurement(imd_ex_t5, institutional_mutation_domestication, base_extractiveness, 5, 0.60).
narrative_ontology:measurement(imd_ex_t10, institutional_mutation_domestication, base_extractiveness, 10, 0.70).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% The Jedi Order's primary function for the Republic was as a peacekeeping force.
narrative_ontology:coordination_type(institutional_mutation_domestication, enforcement_mechanism).

% The decay of the Jedi Order is structurally coupled with rising corruption
% in the political body it serves.
narrative_ontology:affects_constraint(institutional_mutation_domestication, senate_corruption).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */