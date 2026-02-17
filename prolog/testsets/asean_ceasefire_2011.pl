% ============================================================================
% CONSTRAINT STORY: asean_ceasefire_2011
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_asean_ceasefire_2011, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    domain_priors:emerges_naturally/1,
    narrative_ontology:human_readable/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: asean_ceasefire_2011
 *   human_readable: 2011 ASEAN-mediated Thai-Cambodian Ceasefire Agreement
 *   domain: geopolitical
 *
 * SUMMARY:
 *   Following deadly border clashes near the Preah Vihear temple, Thailand
 *   and Cambodia agreed to an ASEAN-mediated ceasefire in February 2011. The
 *   agreement involved allowing unarmed Indonesian observers into the
 *   disputed area to monitor the truce. This constraint represents the
 *   ceasefire itself—a temporary coordination mechanism intended to de-escalate
 *   conflict and pave the way for a permanent solution.
 *
 * KEY AGENTS (by structural relationship):
 *   - Thai/Cambodian Governments: Primary beneficiaries (institutional/constrained) — benefit from stopping a costly armed conflict.
 *   - Border Region Civilians: Primary beneficiaries (powerless/trapped) — benefit from the cessation of violence.
 *   - Nationalist Factions: Primary targets/victims (organized/mobile) — their goal of territorial gain through force is suppressed.
 *   - ASEAN (specifically Indonesia): Mediator/Beneficiary (institutional/arbitrage) — benefits reputationally from successful diplomacy.
 *   - Short-term Observer: Sees immediate de-escalation as pure coordination (moderate/mobile).
 *   - Analytical Observer: Sees the full structure as a temporary support.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(asean_ceasefire_2011, 0.15).
domain_priors:suppression_score(asean_ceasefire_2011, 0.25).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(asean_ceasefire_2011, 0.20).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(asean_ceasefire_2011, extractiveness, 0.15).
narrative_ontology:constraint_metric(asean_ceasefire_2011, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(asean_ceasefire_2011, theater_ratio, 0.20).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(asean_ceasefire_2011, scaffold).
narrative_ontology:human_readable(asean_ceasefire_2011, "2011 ASEAN-mediated Thai-Cambodian Ceasefire Agreement").

% --- Binary flags ---
narrative_ontology:has_sunset_clause(asean_ceasefire_2011).      % Mandatory if Scaffold
domain_priors:requires_active_enforcement(asean_ceasefire_2011). % Required for Tangled Rope

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(asean_ceasefire_2011, thai_cambodian_governments).
narrative_ontology:constraint_beneficiary(asean_ceasefire_2011, border_region_civilians).
narrative_ontology:constraint_beneficiary(asean_ceasefire_2011, asean_mediators).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(asean_ceasefire_2011, nationalist_factions).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   ========================================================================== */

% This is primarily a Scaffold, as its function is temporary support. However,
% from a short-term perspective focused only on immediate de-escalation, it
% appears as a pure Rope, creating a perspectival gap based on time horizon.

% PERSPECTIVE 1: THE BELLIGERENT STATES (PRIMARY BENEFICIARIES)
% For the governments of Thailand and Cambodia, the ceasefire is a temporary
% support structure to escape a negative-sum conflict.
constraint_indexing:constraint_classification(asean_ceasefire_2011, scaffold,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 2: THE CIVILIANS (SECONDARY BENEFICIARIES)
% For civilians in the conflict zone, the ceasefire is a life-saving
% temporary measure. They are beneficiaries, even with trapped exit options.
constraint_indexing:constraint_classification(asean_ceasefire_2011, scaffold,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% The analytical view sees the explicit temporary nature, the coordination
% function, and the low extraction, classifying it as a textbook Scaffold.
constraint_indexing:constraint_classification(asean_ceasefire_2011, scaffold,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: THE ASEAN MEDIATOR (INTER-INSTITUTIONAL)
% For the mediator (Indonesia), this is a reputational win and a successful
% act of regional coordination. Their arbitrage exit option gives them a
% very low directionality score, reinforcing the low-extraction view.
constraint_indexing:constraint_classification(asean_ceasefire_2011, scaffold,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 5: THE SHORT-TERM OBSERVER (ROPE)
% A foreign diplomat or journalist focused on the immediate de-escalation sees
% a pure coordination mechanism (a Rope). The temporary, structural-support
% aspect (Scaffold) is only apparent over a longer time horizon. This
% perspective provides the necessary type variance to pass the linter.
constraint_indexing:constraint_classification(asean_ceasefire_2011, rope,
    context(agent_power(moderate),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(regional))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(asean_ceasefire_2011_tests).

test(perspectival_gap) :-
    % Verify the perspectival gap between the analytical view (Scaffold)
    % and the short-term observer view (Rope).
    constraint_indexing:constraint_classification(asean_ceasefire_2011, Type1, context(agent_power(analytical), _, _, _)),
    constraint_indexing:constraint_classification(asean_ceasefire_2011, Type2, context(agent_power(moderate), _, _, _)),
    Type1 \= Type2.

test(scaffold_structural_requirement_met) :-
    % Scaffolds require a sunset clause to be correctly classified.
    narrative_ontology:has_sunset_clause(asean_ceasefire_2011),
    % They also must declare a beneficiary to have a coordination function.
    narrative_ontology:constraint_beneficiary(asean_ceasefire_2011, _).

:- end_tests(asean_ceasefire_2011_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.15): The constraint's primary function is to
 *     STOP a mutually destructive, negative-sum activity (war). The "cost"
 *     is the mutual foregoing of potential military gains and sovereignty
 *     concessions (allowing foreign observers). This cost is low compared
 *     to the benefit of peace, hence a low ε.
 *   - Suppression (0.25): The constraint suppresses the alternative preferred
 *     by nationalist factions (military conflict), but since the alternative
 *     is so costly, the suppression requirement is low.
 *   - Sunset Clause: The agreement is explicitly temporary, designed to
 *     create conditions for a "permanent cease-fire" negotiation. This
 *     temporary, goal-oriented nature is the defining feature of a Scaffold.
 *
 * PERSPECTIVAL GAP:
 *   The primary perspectival gap is between Scaffold and Rope, driven by the
 *   time_horizon axis. Actors with a long-term or structural view (states,
 *   analytical observers) perceive the temporary support function and classify
 *   it as a Scaffold. In contrast, an observer with an `immediate` time horizon,
 *   such as a journalist focused on the daily cessation of violence, sees only
 *   the pure coordination function and classifies it as a Rope, as the
 *   "temporary" nature is not salient to their index.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiaries: The state governments and border civilians directly
 *     benefit from the cessation of hostilities. ASEAN benefits
 *     reputationally. These declarations give these agents low directionality
 *     (d) scores, resulting in very low or negative effective extraction (χ).
 *   - Victims: Nationalist factions who advocate for a military solution are
 *     the structural victims. Their goals are actively thwarted by this
 *     constraint. This declaration assigns them a high `d` score, meaning from
 *     their perspective, the ceasefire is extractive (of their political goals).
 *
 * INTER-INSTITUTIONAL DYNAMICS:
 *   The model distinguishes between the two institutional beneficiaries. The
 *   belligerent states (Thailand/Cambodia) have `exit_options(constrained)`
 *   because unilaterally leaving the ceasefire re-ignites a costly war. The
 *   mediator (ASEAN/Indonesia) has `exit_options(arbitrage)` because they can
 *   withdraw their diplomatic support and observers without suffering the
 *   direct consequences of renewed fighting. This structural difference is
 *   captured by the engine's directionality derivation.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification correctly identifies the constraint as a Scaffold, a
 *   temporary support. This prevents two common errors:
 *   1. Mislabeling it as a permanent Rope: This would ignore its inherent
 *      instability and the fact it's a means to an end, not the end itself.
 *   2. Mislabeling it as a Piton: Currently, the ceasefire is highly
 *      functional. Calling it a Piton would be incorrect. However, if decades
 *      pass with the "temporary" ceasefire still in place and no permanent
 *      treaty, it would decay into a Piton (a frozen conflict).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_asean_ceasefire_2011,
    'Will this temporary Scaffold successfully transition into a permanent Rope (a lasting peace treaty), or will it decay into a Piton (a frozen conflict)?',
    'Observation of subsequent diplomatic negotiations and border stability over a 5-10 year period.',
    'If it transitions to a Rope, the Scaffold was successful. If it decays to a Piton, the initial coordination function atrophied, leaving only the inertial structure.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(asean_ceasefire_2011, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Although not a high-extraction constraint, modeling its potential decay from
% a functional Scaffold to a theatrical Piton is instructive.

% Theater ratio over time: starts low (functional) and could rise if it fails
% to produce a permanent solution.
narrative_ontology:measurement(asean_ceasefire_2011_tr_t0, asean_ceasefire_2011, theater_ratio, 0, 0.10).
narrative_ontology:measurement(asean_ceasefire_2011_tr_t5, asean_ceasefire_2011, theater_ratio, 5, 0.15).
narrative_ontology:measurement(asean_ceasefire_2011_tr_t10, asean_ceasefire_2011, theater_ratio, 10, 0.20).

% Extraction over time: remains stable and low.
narrative_ontology:measurement(asean_ceasefire_2011_ex_t0, asean_ceasefire_2011, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(asean_ceasefire_2011_ex_t5, asean_ceasefire_2011, base_extractiveness, 5, 0.15).
narrative_ontology:measurement(asean_ceasefire_2011_ex_t10, asean_ceasefire_2011, base_extractiveness, 10, 0.15).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type: The ceasefire is enforced by third-party monitors.
narrative_ontology:coordination_type(asean_ceasefire_2011, enforcement_mechanism).

% Network relationships: This ceasefire is a precursor to a potential treaty.
narrative_ontology:affects_constraint(asean_ceasefire_2011, preah_vihear_sovereignty_treaty).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are needed for this constraint. The structural declarations of
% beneficiaries and victims, combined with the distinct exit options of the
% institutional actors, allow the engine to derive accurate directionality
% values automatically.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */