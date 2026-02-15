% ============================================================================
% CONSTRAINT STORY: helsinki_bus_theory
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-15
% ============================================================================

:- module(constraint_helsinki_bus_theory, []).

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
    domain_priors:emerges_naturally/1,
    narrative_ontology:interval/3,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:omega_variable/3,
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: helsinki_bus_theory
 *   human_readable: The Helsinki Bus Station Theory (Creative Persistence)
 *   domain: social/psychological
 *
 * SUMMARY:
 *   This constraint defines the early stages of a creative career as an inescapable
 *   period of imitation and convergence. All "bus lines" (career paths) take
 *   the same route out of the city for the first few "stops" (years), meaning
 *   nascent work will inevitably look like the work of others.
 *   The constraint forces a choice: reset the career and return to the station,
 *   or "stay on the bus" until the route diverges into unique territory.
 *
 * KEY AGENTS (by structural relationship):
 *   - Novice Artists: Primary target (powerless/trapped) — bears the cost of comparison and perceived lack of originality.
 *   - Art Schools / Record Labels: Primary beneficiary (institutional/mobile) — uses the theory as a framework for talent development and expectation management.
 *   - Established Masters: Secondary beneficiary (moderate/mobile) — sees the constraint as a natural law of mastery they have successfully navigated.
 *   - Analytical Observer: Sees the full structure, including the coordination and extraction functions.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
% Rationale: The system extracts years of labor and "uniqueness" from the
% novice, offering only "comparison to others" in return during the initial
% phase. The frustration of feeling derivative is a real cost.
domain_priors:base_extractiveness(helsinki_bus_theory, 0.4).

% Rationale: The similarity to others' work suppresses the agent's belief in
% their own originality, creating a "temptation" to abandon the path.
domain_priors:suppression_score(helsinki_bus_theory, 0.6).

% Rationale: Low theater; the process is functional, not performative.
domain_priors:theater_ratio(helsinki_bus_theory, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(helsinki_bus_theory, extractiveness, 0.4).
narrative_ontology:constraint_metric(helsinki_bus_theory, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(helsinki_bus_theory, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
% These feed the natural_law_signature certification chain. The 'Established Master'
% perceives this as a Mountain, so these metrics are required for that perspective
% to be valid and for the constraint to pass the linter.
narrative_ontology:constraint_metric(helsinki_bus_theory, accessibility_collapse, 0.90).
narrative_ontology:constraint_metric(helsinki_bus_theory, resistance, 0.10).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(helsinki_bus_theory, tangled_rope).

% --- Binary flags ---
domain_priors:requires_active_enforcement(helsinki_bus_theory).

% --- Emergence flag (required for mountain constraints) ---
% Emerges naturally from the structure of learning and markets.
domain_priors:emerges_naturally(helsinki_bus_theory).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(helsinki_bus_theory, art_schools_and_record_labels).
narrative_ontology:constraint_beneficiary(helsinki_bus_theory, established_masters).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(helsinki_bus_theory, novice_artists).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE NOVICE ARTIST (SNARE)
% For the beginner, the theory is a 'Snare'. They are "always being compared to others"
% and feel that their time is being wasted in a derivative loop, strangling their
% sense of creative self-worth and leading to frustration.
constraint_indexing:constraint_classification(helsinki_bus_theory, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE ART SCHOOL / RECORD LABEL (ROPE)
% For an institution managing talent, the theory is a 'Rope'. It provides a framework
% to understand that early work will be derivative. They can use it to guide artists,
% foster persistence, and identify those with the resilience to reach unique territory.
constraint_indexing:constraint_classification(helsinki_bus_theory, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ESTABLISHED MASTER (MOUNTAIN)
% To the master, the constraint is a 'Mountain'—an unchangeable structural reality of
% how mastery works. One cannot simply "leap" to uniqueness; the first kilometer
% is a zero-degree-of-freedom path that all must traverse.
constraint_indexing:constraint_classification(helsinki_bus_theory, mountain,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% The analyst sees both the coordination function (a predictable path for talent
% development) and the asymmetric extraction (the cost borne by novices).
% χ = 0.4 * 1.15 (analytical) * 1.2 (global) = 0.552.
constraint_indexing:constraint_classification(helsinki_bus_theory, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(helsinki_bus_theory_tests).

test(multi_perspective_variance) :-
    constraint_indexing:constraint_classification(helsinki_bus_theory, Type1, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(helsinki_bus_theory, Type2, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(helsinki_bus_theory, Type3, context(agent_power(moderate), _, _, _)),
    Type1 \= Type2,
    Type2 \= Type3,
    Type1 \= Type3.

:- end_tests(helsinki_bus_theory_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (0.4): Moderate, representing the cost of early-career identity and
 *     time. The frustration of feeling derivative is a real cost borne by novices.
 *   - Suppression (0.6): High, as the constant comparison to others' work actively
 *     suppresses a novice's belief in their own originality, creating a strong
 *     temptation to abandon their path.
 *   - NL Profile: The theory is perceived as a natural law by those who have successfully
 *     navigated it. Accessibility collapse is high (0.9) because there is no perceived
 *     shortcut to originality. Resistance is low (0.1) because masters accept it as a
 *     fundamental truth of their domain.
 *
 * PERSPECTIVAL GAP:
 *   - The Novice (Snare): Is trapped in the derivative phase, experiencing high extraction of self-worth.
 *   - The Institution (Rope): Sees a useful coordination tool for managing talent and expectations.
 *   - The Master (Mountain): Views it as an immutable law of nature for creative development.
 *   This gap is a classic example of how a single structure can be perceived radically
 *   differently based on an agent's position, power, and time horizon.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiaries: Institutions (art schools, labels) benefit from a predictable talent pipeline. Established masters benefit as their status is reinforced by a "rite of passage" they have completed.
 *   - Victims: Novice artists bear the direct psychological and temporal costs of the imitative phase.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as a Tangled Rope from the analytical view prevents mislabeling. It's not a pure Snare, because it serves a genuine coordination function for the creative ecosystem. It's not a pure Rope, because of the clear asymmetric cost imposed on novices. It's not a Mountain, because it is a social, not physical, construct, despite appearing immutable to some participants.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% /5 form: narrative detail for story context
omega_variable(
    omega_helsinki_bus_theory,
    "Does every 'bus line' truly diverge into unique territory, or do some lines remain derivative indefinitely (a permanent Snare)?",
    "Longitudinal qualitative studies of creative careers across disciplines, tracking objective output uniqueness vs. subjective artist satisfaction.",
    "If some lines never diverge: The theory is a Snare for those lines. If all diverge: It is a true Rope/Tangled Rope.",
    confidence_without_resolution(medium)
).

% /3 form: typed classification for reporting engine (REQUIRED)
narrative_ontology:omega_variable(omega_helsinki_bus_theory, empirical, 'Whether all creative paths eventually diverge or if some remain derivative indefinitely.').

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(helsinki_bus_theory, 0, 10).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */