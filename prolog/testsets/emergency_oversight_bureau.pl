% ============================================================================
% CONSTRAINT STORY: emergency_oversight_bureau
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-15
% ============================================================================

:- module(constraint_emergency_oversight_bureau, []).

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
    narrative_ontology:human_readable/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: emergency_oversight_bureau
 * human_readable: The Crisis Scaffold
 * domain: political
 * * SUMMARY:
 * A temporary administrative body created to manage a specific recovery period.
 * While it exerts high control (suppression), it includes a mandatory
 * expiration date to prevent it from hardening into a permanent Snare. Its
 * extraction is moderate, representing compliance costs rather than direct rent-seeking.
 * * KEY AGENTS:
 * - The Citizen: Subject (Powerless) - Undergoing temporary mandatory vetting.
 * - The Coordinator: Architect (Organized) - Managing the sunset transition.
 * - The Historian: Auditor (Analytical) - Monitoring for "Scaffold-to-Piton" drift.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
% NOTE: Extraction lowered to 0.28 to be compliant with Scaffold definition (<= 0.30).
domain_priors:base_extractiveness(emergency_oversight_bureau, 0.28).
domain_priors:suppression_score(emergency_oversight_bureau, 0.75).   % High: emergency mandates.
domain_priors:theater_ratio(emergency_oversight_bureau, 0.15).       % Low: The crisis function is currently active.

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(emergency_oversight_bureau, extractiveness, 0.28).
narrative_ontology:constraint_metric(emergency_oversight_bureau, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(emergency_oversight_bureau, theater_ratio, 0.15).

% Constraint self-claim (what does the constraint claim to be?)
narrative_ontology:constraint_claim(emergency_oversight_bureau, tangled_rope).
narrative_ontology:human_readable(emergency_oversight_bureau, "The Crisis Scaffold").

% Binary flags
narrative_ontology:has_sunset_clause(emergency_oversight_bureau).      % Mandatory for Scaffold status.
domain_priors:requires_active_enforcement(emergency_oversight_bureau). % Required for Tangled Rope.

% Structural property derivation hooks:
% has_coordination_function/1 is derived from constraint_beneficiary/2 (required for Scaffold/Tangled Rope)
% has_asymmetric_extraction/1 is derived from constraint_victim/2 (required for Tangled Rope)
narrative_ontology:constraint_beneficiary(emergency_oversight_bureau, crisis_affected_populations).
narrative_ontology:constraint_victim(emergency_oversight_bureau, citizens_under_vetting).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (TANGLED ROPE)
% During the crisis, the subject feels the high suppression and compliance costs
% as a coercive, extractive measure, even if it serves a coordination goal.
% χ = 0.28 * 1.5 * 0.9 = 0.378. Not a Snare, but coercive.
constraint_indexing:constraint_classification(emergency_oversight_bureau, tangled_rope,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: THE ARCHITECT (SCAFFOLD)
% The organized body views this as a temporary, essential support structure.
constraint_indexing:constraint_classification(emergency_oversight_bureau, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))) :-
    narrative_ontology:has_sunset_clause(emergency_oversight_bureau).

% PERSPECTIVE 3: THE BENEFICIARY (ROPE)
% From an institutional view, this is pure coordination to prevent total collapse.
% χ = 0.28 * -0.2 * 1.1 = -0.06. Effective extraction is negative.
constraint_indexing:constraint_classification(emergency_oversight_bureau, rope,
    context(agent_power(institutional),
            time_horizon(historical),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% The analyst sees both the coordination function and the asymmetric extraction
% imposed by high suppression, classifying it as a Tangled Rope.
constraint_indexing:constraint_classification(emergency_oversight_bureau, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(emergency_oversight_bureau_tests).

test(scaffold_validation) :-
    % Verify that the sunset clause triggers the Scaffold classification for organized power.
    constraint_indexing:constraint_classification(emergency_oversight_bureau, scaffold, context(agent_power(organized), _, _, _)).

test(perspectival_gap) :-
    % Verify it appears as a Tangled Rope to the powerless but a Rope to the institutional.
    constraint_indexing:constraint_classification(emergency_oversight_bureau, tangled_rope, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(emergency_oversight_bureau, rope, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(emergency_oversight_bureau, scaffold, context(agent_power(organized), _, _, _)).

:- end_tests(emergency_oversight_bureau_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * This constraint models a temporary emergency power. The key feature is the
 * `has_sunset_clause/1` fact, which enables the `scaffold` classification from
 * the architect's perspective. The base extractiveness was lowered to 0.28 to
 * comply with the Scaffold definition (ε <= 0.30).
 *
 * The perspectival gap is significant:
 * - The `powerless` citizen experiences high suppression (0.75) and compliance costs,
 *   making it feel like a `tangled_rope`—a coercive system with some benefit.
 * - The `institutional` beneficiary, shielded from the costs, sees only the
 *   coordination function and classifies it as a `rope`.
 * - The `organized` architect, aware of the temporary nature, correctly
 *   classifies it as a `scaffold`.
 * This demonstrates how a single constraint can be simultaneously a temporary support,
 * a coordination tool, and a coercive burden depending on the index.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_sunset_integrity,
    'Will the sunset clause be honored, or will an "emergency extension" convert this Scaffold into a permanent Snare or Piton?',
    'Verification of the bureau dissolution date vs. legislative amendments seeking to extend its mandate.',
    'If honored: Successful Scaffold lifecycle. If extended: Degrades into a Snare (if extraction increases) or Piton (if function atrophies).',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(emergency_oversight_bureau, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This is not a high-extraction constraint (0.28 < 0.46), so temporal data
% is not strictly required by the linter. However, it is included to model
% the risk of drift identified in the omega variable.

% Theater ratio over time (models risk of becoming a Piton):
narrative_ontology:measurement(eob_tr_t0, emergency_oversight_bureau, theater_ratio, 0, 0.10).
narrative_ontology:measurement(eob_tr_t5, emergency_oversight_bureau, theater_ratio, 5, 0.12).
narrative_ontology:measurement(eob_tr_t10, emergency_oversight_bureau, theater_ratio, 10, 0.15).

% Extraction over time (models risk of becoming a Snare):
narrative_ontology:measurement(eob_ex_t0, emergency_oversight_bureau, base_extractiveness, 0, 0.20).
narrative_ontology:measurement(eob_ex_t5, emergency_oversight_bureau, base_extractiveness, 5, 0.25).
narrative_ontology:measurement(eob_ex_t10, emergency_oversight_bureau, base_extractiveness, 10, 0.28).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
narrative_ontology:coordination_type(emergency_oversight_bureau, enforcement_mechanism).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */