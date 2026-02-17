% ============================================================================
% CONSTRAINT STORY: qwerty_vs_dvorak
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-15
% ============================================================================

:- module(constraint_qwerty_vs_dvorak, []).

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
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:omega_variable/3,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:human_readable/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: qwerty_vs_dvorak
 *   human_readable: QWERTY vs. Dvorak (Technological Lock-in)
 *   domain: technological
 *
 * SUMMARY:
 *   This constraint models the path-dependent lock-in of the QWERTY keyboard
 *   layout. Originally designed to prevent mechanical jams in 19th-century
 *   typewriters, it persists as the dominant standard despite the existence
 *   of more efficient layouts (like Dvorak). The constraint arises from
 *   massive network effects and sunk costs in training and manufacturing,
 *   creating a system with a clear coordination function that simultaneously
 *   imposes a widespread, low-grade extractive cost (in efficiency and ergonomics).
 *
 * KEY AGENTS (by structural relationship):
 *   - Typists and RSI Sufferers: Primary target (powerless/trapped) — bear the cost of inefficiency and ergonomic strain.
 *   - Hardware/Software Manufacturers: Primary beneficiary (institutional/arbitrage) — benefit from the coordination standard, which simplifies production and guarantees interoperability.
 *   - Economic Historian: Analytical observer — sees the full structure of coordination and extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
% Rationale: Moderate (0.40). It extracts "human efficiency" and "physical health."
% The sub-optimal layout imposes a "tax" of slower typing and higher strain
% on billions of hands to maintain the network effect.
domain_priors:base_extractiveness(qwerty_vs_dvorak, 0.40).

% Rationale: High (0.70). Alternatives like Dvorak are technically available
% but socially and economically suppressed. The high cost of re-learning and
% incompatibility with shared hardware makes them practically inaccessible.
domain_priors:suppression_score(qwerty_vs_dvorak, 0.70).

% Rationale: Low (0.12). The costs are real (ergonomic strain, lost speed),
% not performative. The system's function is genuine coordination.
domain_priors:theater_ratio(qwerty_vs_dvorak, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qwerty_vs_dvorak, extractiveness, 0.40).
narrative_ontology:constraint_metric(qwerty_vs_dvorak, suppression_requirement, 0.70).
narrative_ontology:constraint_metric(qwerty_vs_dvorak, theater_ratio, 0.12).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(qwerty_vs_dvorak, tangled_rope).
narrative_ontology:human_readable(qwerty_vs_dvorak, "QWERTY vs. Dvorak (Technological Lock-in)").

% --- Binary flags ---
% The lock-in is not a law of nature; it is maintained by the continuous,
% reinforcing decisions of manufacturers (OS defaults, hardware) and
% institutions (training programs). This constitutes active enforcement.
domain_priors:requires_active_enforcement(qwerty_vs_dvorak).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(qwerty_vs_dvorak, hardware_manufacturers).
narrative_ontology:constraint_beneficiary(qwerty_vs_dvorak, software_developers).
narrative_ontology:constraint_beneficiary(qwerty_vs_dvorak, corporate_it_departments).

% Who bears disproportionate cost?
narrative_ontology:constraint_victim(qwerty_vs_dvorak, professional_typists).
narrative_ontology:constraint_victim(qwerty_vs_dvorak, rsi_sufferers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   where f(d) is the sigmoid directionality function:
     f(d) = -0.20 + 1.70 / (1 + e^(-6*(d - 0.50)))
   The engine derives d from beneficiary/victim membership + exit_options.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   CONTEXT ARITY: All context() terms must have exactly 4 arguments.
   Do not add measurement_basis, beneficiary/victim, or other metadata.
   Linter Rule 23 rejects files with context arity ≠ 4.
   ========================================================================== */

% PERSPECTIVE 1: THE TYPIST (SNARE)
% Agent who bears the most extraction. Engine derives d from:
%   victim membership + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42 → high χ
constraint_indexing:constraint_classification(qwerty_vs_dvorak, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE MANUFACTURER (ROPE)
% Agent who benefits most. Engine derives d from:
%   beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 → low/negative χ
constraint_indexing:constraint_classification(qwerty_vs_dvorak, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ECONOMIC HISTORIAN (TANGLED ROPE)
% Default analytical context. Sees both the coordination function (Rope) and
% the asymmetric extraction (Snare). The metrics (ε=0.4, suppression=0.7) and
% structural data (beneficiary, victim, enforcement) confirm this dual nature.
constraint_indexing:constraint_classification(qwerty_vs_dvorak, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(qwerty_vs_dvorak_tests).

test(perspectival_gap_is_trichotomy) :-
    % Verify the core trichotomy: Snare for victim, Rope for beneficiary, Tangled Rope for analyst.
    constraint_indexing:constraint_classification(qwerty_vs_dvorak, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(qwerty_vs_dvorak, rope, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(qwerty_vs_dvorak, tangled_rope, context(agent_power(analytical), _, _, _)).

test(tangled_rope_structural_requirements_met) :-
    % Verify that the structural data required for a Tangled Rope classification is present.
    narrative_ontology:constraint_beneficiary(qwerty_vs_dvorak, _), % -> has_coordination_function
    narrative_ontology:constraint_victim(qwerty_vs_dvorak, _),     % -> has_asymmetric_extraction
    domain_priors:requires_active_enforcement(qwerty_vs_dvorak).

:- end_tests(qwerty_vs_dvorak_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The metrics were chosen to reflect a sub-optimal but deeply entrenched
 *   standard. Base extractiveness (0.40) captures the quantifiable loss of
 *   efficiency and ergonomic cost imposed on users. Suppression (0.70)
 *   reflects the immense practical difficulty of switching due to network
 *   effects and sunk costs, even though alternatives are not legally banned.
 *
 * PERSPECTIVAL GAP:
 *   The gap is classic. For manufacturers (beneficiaries), QWERTY is a pure
 *   coordination device (Rope) that simplifies the market. For typists
 *   (victims), it is a Snare that extracts their efficiency and well-being
 *   with no viable escape. The analytical observer sees both sides—a system
 *   that genuinely coordinates but does so in an extractive, path-dependent
 *   way—and thus classifies it as a Tangled Rope.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries are institutions that profit from the stability of the
 *   standard. Victims are the end-users who bear the daily friction of a
 *   sub-optimal design. The directionality is clear: value (in the form of
 *   simplicity and interoperability) flows to institutions, while costs (in
 *   the form of inefficiency and strain) are borne by individuals.
 *
 * MANDATROPHY ANALYSIS:
 *   This case is a powerful example of how a system can be both a Rope and a
 *   Snare simultaneously. Mislabeling it as a pure Rope (focusing only on
 *   interoperability) ignores the real, measurable costs. Mislabeling it as
 *   a pure Snare ignores its vital coordination function. The Tangled Rope
 *   classification, derived from the analytical perspective, correctly
 *   captures this dual nature, preventing mischaracterization.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_qwerty_vs_dvorak,
    'Will new input modalities (voice, neural interfaces) render keyboard layout irrelevant, thus dissolving the constraint, or will the layout persist as a cultural artifact?',
    'Monitor the decline of keyboard-based text input as a primary method in professional and personal computing over the next two decades.',
    'If dissolved, the Tangled Rope unravels. If it persists, it may degrade into a Piton—a purely inertial standard with no remaining function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(omega_qwerty_vs_dvorak, empirical, 'The future relevance of keyboard layouts in the face of new input technologies.').

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(qwerty_vs_dvorak, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% The constraint is in a state of deep lock-in. The metrics are stable over
% the measured interval, reflecting its inertial nature.
% Theater ratio over time:
narrative_ontology:measurement(qwerty_tr_t0, qwerty_vs_dvorak, theater_ratio, 0, 0.12).
narrative_ontology:measurement(qwerty_tr_t5, qwerty_vs_dvorak, theater_ratio, 5, 0.12).
narrative_ontology:measurement(qwerty_tr_t10, qwerty_vs_dvorak, theater_ratio, 10, 0.12).

% Extraction over time:
narrative_ontology:measurement(qwerty_ex_t0, qwerty_vs_dvorak, base_extractiveness, 0, 0.40).
narrative_ontology:measurement(qwerty_ex_t5, qwerty_vs_dvorak, base_extractiveness, 5, 0.40).
narrative_ontology:measurement(qwerty_ex_t10, qwerty_vs_dvorak, base_extractiveness, 10, 0.40).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% QWERTY is a quintessential information standard.
narrative_ontology:coordination_type(qwerty_vs_dvorak, information_standard).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are necessary. The structural derivation from beneficiary/victim
% declarations and exit options accurately models the power dynamics.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */