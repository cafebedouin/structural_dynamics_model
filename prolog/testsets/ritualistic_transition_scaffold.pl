% ============================================================================
% CONSTRAINT STORY: ritual_transition_scaffold
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-23
% ============================================================================

:- module(constraint_ritual_transition_scaffold, []).

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
 * * constraint_id: ritual_transition_scaffold
 * human_readable: The Habit-Building Scaffold
 * domain: social/institutional
 * * SUMMARY:
 * An intentional use of procedural theater to stabilize a chaotic organization.
 * While the rituals themselves may have low direct utility, they act as a Scaffold
 * to re-establish the baseline coordination (Rope) needed for future growth.
 * The structure is explicitly temporary, designed to be dismantled once stable
 * habits are formed.
 * * KEY AGENTS:
 * - The New Hire: Subject (Powerless) - Finding safety in the new "routine."
 * - Senior Management: Beneficiary (Institutional) - Seeing restored order.
 * - The Change Manager: Architect (Organized) - Designing the sunset-bound rituals.
 * - The Efficiency Auditor: Auditor (Analytical) - Monitoring the Scaffold-to-Rope decay.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(ritual_transition_scaffold, 0.35). % Moderate: Labor is used for habit-building, not pure extraction.
domain_priors:suppression_score(ritual_transition_scaffold, 0.60).   % Moderate: Participation is mandatory but justified by crisis.
domain_priors:theater_ratio(ritual_transition_scaffold, 0.72).       % High: The rituals are theatrical by design, but risk hardening into a Piton.

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(ritual_transition_scaffold, extractiveness, 0.35).
narrative_ontology:constraint_metric(ritual_transition_scaffold, suppression_requirement, 0.60).
narrative_ontology:constraint_metric(ritual_transition_scaffold, theater_ratio, 0.72).

% Constraint self-claim (what does the constraint claim to be?)
narrative_ontology:constraint_claim(ritual_transition_scaffold, tangled_rope).
narrative_ontology:human_readable(ritual_transition_scaffold, "The Habit-Building Scaffold").
narrative_ontology:topic_domain(ritual_transition_scaffold, "social/institutional").

% Binary flags
narrative_ontology:has_sunset_clause(ritual_transition_scaffold).      % Mandatory for Scaffold classification.
domain_priors:requires_active_enforcement(ritual_transition_scaffold). % Required for Tangled Rope classification.

% Structural property derivation hooks:
% These are required for Scaffold and Tangled Rope classifications.
narrative_ontology:constraint_beneficiary(ritual_transition_scaffold, organization_members).
narrative_ontology:constraint_victim(ritual_transition_scaffold, new_hires).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE DISORIENTED WORKER (ROPE)
% To someone lost in chaos, the theater provides a "Rope" of predictability.
% The perceived extraction is low because the alternative is high-cost chaos.
constraint_indexing:constraint_classification(ritual_transition_scaffold, rope,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 2: SENIOR MANAGEMENT (ROPE)
% From an institutional perspective, the restored order and predictability
% are seen as a pure coordination good (Rope). The extraction cost is
% abstracted away and considered negligible for organizational stability.
constraint_indexing:constraint_classification(ritual_transition_scaffold, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: THE CHANGE ARCHITECT (SCAFFOLD)
% The architect views this as a temporary support structure that must expire.
constraint_indexing:constraint_classification(ritual_transition_scaffold, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))) :-
    narrative_ontology:has_sunset_clause(ritual_transition_scaffold).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Detects the hybrid nature: it provides a genuine coordination function
% (beneficiary exists) but does so via enforced, extractive means (victim
% exists, enforcement required).
constraint_indexing:constraint_classification(ritual_transition_scaffold, tangled_rope,
    context(agent_power(analytical),
            time_horizon(historical),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ritual_transition_scaffold_tests).

test(perspectival_gap) :-
    % Verify a gap between the institutional view (Rope) and analytical view (Tangled Rope).
    constraint_indexing:constraint_classification(ritual_transition_scaffold, rope, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(ritual_transition_scaffold, tangled_rope, context(agent_power(analytical), _, _, _)).

test(scaffold_properties) :-
    % Verify the presence of a sunset clause and beneficiary triggers Scaffold classification.
    constraint_indexing:constraint_classification(ritual_transition_scaffold, scaffold, context(agent_power(organized), _, _, _)),
    narrative_ontology:has_sunset_clause(ritual_transition_scaffold),
    narrative_ontology:constraint_beneficiary(ritual_transition_scaffold, _).

test(tangled_rope_properties) :-
    % Verify the analytical observer correctly identifies a Tangled Rope.
    constraint_indexing:constraint_classification(ritual_transition_scaffold, tangled_rope, context(agent_power(analytical), _, _, _)),
    narrative_ontology:constraint_beneficiary(ritual_transition_scaffold, _),
    narrative_ontology:constraint_victim(ritual_transition_scaffold, _),
    domain_priors:requires_active_enforcement(ritual_transition_scaffold).

:- end_tests(ritual_transition_scaffold_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * This constraint models a deliberate, temporary use of high-suppression,
 * theatrical coordination to bootstrap order. The key is its intentional,
 * temporary nature, distinguishing it from a decaying Piton.
 * - The `base_extractiveness` (0.35) is moderate, representing the cost of
 *   time spent on rituals instead of direct productive work.
 * - The `theater_ratio` (0.72) is high by design, as the rituals are symbolic.
 * - The `has_sunset_clause` is critical, as it's the structural guarantee
 *   that prevents the Scaffold from hardening into a permanent, extractive fixture.
 *
 * PERSPECTIVAL GAP:
 * The gap is significant. New hires (`powerless`) and management (`institutional`)
 * both perceive a `Rope` because it solves their immediate problem (chaos for the
 * former, instability for the latter). The architect (`organized`) sees the
 * intended temporary `Scaffold`. The external analyst (`analytical`) sees the
 * underlying structural reality: a `Tangled Rope` that combines real
 * coordination with asymmetric extraction and enforcement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_scaffold_hardening,
    'Will the rituals be abandoned once stability is reached, or will they harden into a Piton?',
    'Observation of the theater_ratio and enforcement levels 12-24 months post-stability, and checking if the sunset clause is honored.',
    'If abandoned: Successful Scaffold. If hardened: Degrades into a Piton or an extractive Snare.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(ritual_transition_scaffold, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This models the *intended* lifecycle of the scaffold, where its negative
% aspects (extraction and theater) were initially higher and were meant to
% decrease as the organization stabilized. The final values match the
% base properties.

% Theater ratio over time (models intended decay):
narrative_ontology:measurement(rts_tr_t0, ritual_transition_scaffold, theater_ratio, 0, 0.80).
narrative_ontology:measurement(rts_tr_t5, ritual_transition_scaffold, theater_ratio, 5, 0.75).
narrative_ontology:measurement(rts_tr_t10, ritual_transition_scaffold, theater_ratio, 10, 0.72).

% Extraction over time (models decreasing burden):
narrative_ontology:measurement(rts_ex_t0, ritual_transition_scaffold, base_extractiveness, 0, 0.40).
narrative_ontology:measurement(rts_ex_t5, ritual_transition_scaffold, base_extractiveness, 5, 0.38).
narrative_ontology:measurement(rts_ex_t10, ritual_transition_scaffold, base_extractiveness, 10, 0.35).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type: The rituals are a mechanism to enforce new behavioral norms.
narrative_ontology:coordination_type(ritual_transition_scaffold, enforcement_mechanism).

% Network relationships: This scaffold directly influences how new employees are integrated.
narrative_ontology:affects_constraint(ritual_transition_scaffold, employee_onboarding_process).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */