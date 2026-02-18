% ============================================================================
% CONSTRAINT STORY: emergency_mode_lock_in
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-22
% ============================================================================

:- module(constraint_emergency_mode_lock_in, []).

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
 * * constraint_id: emergency_mode_lock_in
 * human_readable: The Perpetual Crisis Mandate
 * domain: political/organizational
 * * SUMMARY:
 * A scenario where a "Rope" designed for temporary crisis management (e.g.,
 * emergency powers, martial law protocols, or rapid-response technical overrides)
 * becomes the permanent operating substrate. This coordination tool for
 * "short-term survival" becomes a "Snare" for the subject, as their primary
 * civil, legal, and operational agency is liquidated to maintain the
 * "Emergency Mode," trapping them in a territory where the crisis is never
 * declared "over" because the system now depends on the suspension of
 * normal constraints to function.
 * * KEY AGENTS:
 * - Suspended Citizen: Subject (Powerless)
 * - Emergency Administrator: Beneficiary (Institutional)
 * - Constitutional Decay Auditor: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% High extraction (0.91) reflects the total liquidation of normal-time rights
% and agency to fuel the "efficiency" of the emergency mandate.
domain_priors:base_extractiveness(emergency_mode_lock_in, 0.91).
domain_priors:suppression_score(emergency_mode_lock_in, 0.85).   % Normal legal processes are suppressed as "existential risks" or "inefficiencies."
domain_priors:theater_ratio(emergency_mode_lock_in, 0.94).       % Extreme theater: "Sunset Review" sessions that performatively signal an end while finding new crises to justify extension.

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(emergency_mode_lock_in, extractiveness, 0.91).
narrative_ontology:constraint_metric(emergency_mode_lock_in, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(emergency_mode_lock_in, theater_ratio, 0.94).

% Constraint self-claim (what does the constraint claim to be?)
% It claims to be a necessary coordination mechanism for public safety.
narrative_ontology:constraint_claim(emergency_mode_lock_in, tangled_rope).
narrative_ontology:human_readable(emergency_mode_lock_in, "The Perpetual Crisis Mandate").
narrative_ontology:topic_domain(emergency_mode_lock_in, "political/organizational").

% Binary flags and structural properties for Tangled Rope
domain_priors:requires_active_enforcement(emergency_mode_lock_in).
narrative_ontology:constraint_beneficiary(emergency_mode_lock_in, emergency_administrator).
narrative_ontology:constraint_victim(emergency_mode_lock_in, suspended_citizen).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% The citizen is trapped: they live under "temporary" rules that have
% defined their entire biographical horizon, liquidating their agency to
% advocate for a return to normalcy.
constraint_indexing:constraint_classification(emergency_mode_lock_in, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The administrator views the lock-in as a Rope—the essential coordination
% substrate for managing a complex, volatile world without the "drag" of
% standard deliberative processes.
constraint_indexing:constraint_classification(emergency_mode_lock_in, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Detects high extraction (0.91) and suppression (0.85) masking as essential
% coordination. The presence of both beneficiaries and victims, plus active
% enforcement, confirms the Tangled Rope classification.
constraint_indexing:constraint_classification(emergency_mode_lock_in, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(universal))).

% PERSPECTIVE 4: THE SYSTEMS AUDITOR (PITON)
% The extreme theater ratio (0.94) triggers a Piton classification. The
% "Emergency Success Dashboard" is an inertial spike; it signals safety while
% the subject's agency is permanently atrophied. The original function is inert.
constraint_indexing:constraint_classification(emergency_mode_lock_in, piton,
    context(agent_power(analytical),
            time_horizon(historical),
            exit_options(analytical),
            spatial_scope(global))) :-
    domain_priors:theater_ratio(emergency_mode_lock_in, TR), TR > 0.70.

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(emergency_lock_in_tests).

test(perspectival_gap) :-
    % Verify Snare for the powerless citizen vs Rope for the institutional administrator.
    constraint_indexing:constraint_classification(emergency_mode_lock_in, snare,
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(emergency_mode_lock_in, rope,
        context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(emergency_mode_lock_in, tangled_rope,
        context(agent_power(analytical), _, _, _)).

test(piton_trigger) :-
    % Ensure extreme theater (0.94) correctly triggers the Piton classification.
    constraint_indexing:constraint_classification(emergency_mode_lock_in, piton,
        context(agent_power(analytical), _, _, _)).

test(tangled_rope_structural_properties) :-
    % Verify that the necessary structural properties for a Tangled Rope are declared.
    narrative_ontology:constraint_beneficiary(emergency_mode_lock_in, _),
    narrative_ontology:constraint_victim(emergency_mode_lock_in, _),
    domain_priors:requires_active_enforcement(emergency_mode_lock_in).

:- end_tests(emergency_lock_in_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The scores reflect a system that began as a legitimate, temporary coordination
 * mechanism (Scaffold/Rope) but has since degraded into a permanent, extractive
 * state. The high extraction (0.91) and suppression (0.85) represent the
 * liquidation of citizen agency to maintain the "efficiency" of the emergency
 * state. The extreme theater ratio (0.94) captures the performative nature of
 * reviews and sunset clauses that never trigger, serving only to legitimize
 * the ongoing state of exception.
 *
 * * PERSPECTIVAL GAP:
 * The Suspended Citizen feels a Snare because the "temporary" measures
 * have become a permanent cage for their rights. The Administrator
 * sees a Rope because the lock-in coordinates an efficient, top-down
 * social order that can pivot instantly to new threats, and from which they
 * derive power and purpose.
 *
 * * MANDATROPHY ANALYSIS ([RESOLVED MANDATROPHY]):
 * The high extraction (0.91) represents a state of Mandatrophy, where the
 * original coordinating function of the emergency measure has been completely
 * subsumed by its extractive function. The system avoids misclassifying this
 * as a simple Snare by using the Tangled Rope classification from the analytical
 * perspective. This acknowledges the vestigial claim of coordination (the
 * 'emergency') while correctly identifying the dominant, asymmetric extraction
 * that defines the constraint's current reality. The Piton classification
 * further resolves this by highlighting the extreme theatricality (0.94) used
 * to maintain the facade of a temporary crisis, confirming the original
 * function is inert.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Required for high-extraction constraints (> 0.46).
omega_variable(
    omega_crisis_normalization,
    'Is the perpetual emergency state a reversible policy choice (Tangled Rope) or an emergent, irreversible property of complex, high-risk systems (a Mountain of institutional physics)?',
    'Comparative analysis of state capacity and civil liberty restoration in post-crisis jurisdictions over a civilizational time horizon.',
    'If reversible: Snare of current technique. If irreversible: Mountain of Institutional Physics.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(emergency_mode_lock_in, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This constraint began as a legitimate, low-extraction emergency measure.
% Over the interval, its extractive properties intensified and its justification
% became purely theatrical as the "emergency" became permanent.
%
% Theater ratio over time (triggers metric_substitution detection):
narrative_ontology:measurement(eml_tr_t0, emergency_mode_lock_in, theater_ratio, 0, 0.10).
narrative_ontology:measurement(eml_tr_t5, emergency_mode_lock_in, theater_ratio, 5, 0.50).
narrative_ontology:measurement(eml_tr_t10, emergency_mode_lock_in, theater_ratio, 10, 0.94).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(eml_ex_t0, emergency_mode_lock_in, base_extractiveness, 0, 0.20).
narrative_ontology:measurement(eml_ex_t5, emergency_mode_lock_in, base_extractiveness, 5, 0.65).
narrative_ontology:measurement(eml_ex_t10, emergency_mode_lock_in, base_extractiveness, 10, 0.91).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% The constraint functions as a top-down enforcement mechanism, overriding
% normal deliberative processes.
narrative_ontology:coordination_type(emergency_mode_lock_in, enforcement_mechanism).

% Network relationships (structural influence edges)
% The persistence of this constraint structurally degrades other constraints
% related to civil liberties and legal process.
narrative_ontology:affects_constraint(emergency_mode_lock_in, rule_of_law).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */