% ============================================================================
% CONSTRAINT STORY: overton_window
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-15
% ============================================================================

:- module(constraint_overton_window, []).

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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:human_readable/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: overton_window
 *   human_readable: The Overton Window of Political Discourse
 *   domain: political/social
 *
 * SUMMARY:
 *   The Overton Window represents the range of policies or ideas considered
 *   acceptable to the mainstream population at a given time. It constrains
 *   political actors to a narrow "window" of viable options, where moving
 *   outside is perceived as political suicide. The window shifts not through
 *   direct political mandate, but through sustained social, cultural, and
 *   media pressure, often orchestrated by ideological actors.
 *
 * KEY AGENTS (by structural relationship):
 *   - Radical Reformers: Primary target (powerless/trapped) — bears extraction by having their necessary policies deemed "unthinkable" and suppressed from discourse.
 *   - Political Incumbents: Primary beneficiary (institutional/constrained) — benefits from a simplified political landscape and reduced need to engage with complex or radical ideas.
 *   - Ideological Strategists: Secondary beneficiary (powerful/mobile) — benefits by actively manipulating the window to make their preferred policies seem "sensible" over time.
 *   - Analytical Observer: Sees the full structure of coordination and extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
% Rationale: It extracts political agency by narrowing what is "thinkable,"
% funneling social energy into a limited set of institutionalized outcomes
% that often benefit the status quo. Raised to 0.48 to ensure the powerless
% perspective correctly classifies as a Snare.
domain_priors:base_extractiveness(overton_window, 0.48).

% Rationale: Ideas outside the window are not just ignored; they are actively
% punished through social shaming, media ridicule, or labels like "extremist,"
% making alternatives effectively invisible to the mainstream.
domain_priors:suppression_score(overton_window, 0.85).

% Rationale: The constraint is highly functional, not performative. Its effects
% on political discourse are real and substantial.
domain_priors:theater_ratio(overton_window, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(overton_window, extractiveness, 0.48).
narrative_ontology:constraint_metric(overton_window, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(overton_window, theater_ratio, 0.1).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(overton_window, tangled_rope).
narrative_ontology:human_readable(overton_window, "The Overton Window of Political Discourse").

% --- Binary flags ---
% Rationale: The window is maintained by active enforcement from media outlets,
% political parties, and social pressure groups who police the boundaries of
% "acceptable" thought.
domain_priors:requires_active_enforcement(overton_window).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(overton_window, political_incumbents).
narrative_ontology:constraint_beneficiary(overton_window, ideological_strategists).

% Who bears disproportionate cost?
narrative_ontology:constraint_victim(overton_window, radical_reformers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE RADICAL REFORMER (SNARE)
% Agent whose survival depends on a policy currently outside the window.
% Engine derives d from victim status + trapped exit -> d ≈ 0.95 -> high χ.
% χ = 0.48 * f(0.95) * σ(national) = 0.48 * 1.42 * 1.0 = 0.68 -> Snare
constraint_indexing:constraint_classification(overton_window, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE POLITICAL INCUMBENT (ROPE)
% Agent whose power depends on navigating the window. They are both a
% beneficiary (it simplifies their job) and constrained by it.
% Engine derives d from beneficiary status + constrained exit -> d ≈ 0.4 -> moderate χ.
% χ = 0.48 * f(0.4) * σ(national) = 0.48 * 0.40 * 1.0 = 0.19 -> Rope
constraint_indexing:constraint_classification(overton_window, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: THE IDEOLOGICAL STRATEGIST (ROPE)
% Agent (e.g., think-tank director) working to shift the window.
% Engine derives d from beneficiary status + mobile exit -> d ≈ 0.15 -> low/negative χ.
% χ = 0.48 * f(0.15) * σ(global) = 0.48 * -0.01 * 1.2 = -0.006 -> Rope
constraint_indexing:constraint_classification(overton_window, rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Default analytical context. Sees both the coordination function for
% incumbents and the extractive function against reformers.
% Engine derives d ≈ 0.72 for analytical perspective.
% χ = 0.48 * f(0.72) * σ(global) = 0.48 * 1.15 * 1.2 = 0.6624 -> Tangled Rope
% (Classifies as Tangled Rope over Snare due to the clear coordination function).
constraint_indexing:constraint_classification(overton_window, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(overton_window_tests).

test(perspectival_gap_snare_vs_rope) :-
    % Verify the core perspectival gap between the victim and beneficiaries.
    constraint_indexing:constraint_classification(overton_window, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(overton_window, rope, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(overton_window, rope, context(agent_power(powerful), _, _, _)).

test(analytical_classification_is_tangled_rope) :-
    constraint_indexing:constraint_classification(overton_window, tangled_rope, context(agent_power(analytical), _, _, _)).

test(tangled_rope_structural_gates_pass) :-
    % Verify that all three conditions for Tangled Rope are met.
    narrative_ontology:constraint_beneficiary(overton_window, _), % -> has_coordination_function
    narrative_ontology:constraint_victim(overton_window, _),     % -> has_asymmetric_extraction
    domain_priors:requires_active_enforcement(overton_window).

:- end_tests(overton_window_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness (ε) was set to 0.48 and suppression to 0.85.
 *   These high values reflect the constraint's powerful ability to extract
 *   political agency and actively suppress alternative viewpoints. This
 *   ensures the metrics align with the narrative, particularly allowing the
 *   powerless perspective's effective extraction (χ) to cross the Snare
 *   threshold (χ >= 0.66). The original file's claim of 'mountain' was
 *   structurally incorrect given its high ε and suppression, a classic
 *   false natural law.
 *
 * PERSPECTIVAL GAP:
 *   The gap is profound. For Radical Reformers, the window is a Snare that
 *   strangles necessary change and makes their needs unspeakable in the
 *   mainstream. For Political Incumbents and Ideological Strategists, it's a
 *   Rope—a coordination tool that simplifies the political landscape (for
 *   incumbents) or a lever to be pulled to shift public opinion over time
 *   (for strategists).
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiaries: 'political_incumbents' and 'ideological_strategists'
 *     benefit from the simplified, managed discourse. This declaration drives
 *     their low/negative effective extraction (χ) and Rope classification.
 *   - Victims: 'radical_reformers' bear the cost. Their ideas are suppressed,
 *     and their political agency is extracted. This declaration drives their
 *     high χ and Snare classification.
 *
 * MANDATROPHY ANALYSIS:
 *   The analytical classification as Tangled Rope is crucial. It correctly
 *   identifies that the Overton Window has BOTH a genuine coordination
 *   function (for political elites) AND a severe, asymmetric extractive
 *   function (against reformers). Labeling it a pure Snare would miss the
 *   coordination that gives it stability, while labeling it a pure Rope
 *   would ignore the immense harm it does by suppressing vital but
 *   "unthinkable" policy solutions.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_overton_window,
    "In a hyper-polarized digital era, does a single 'Overton Window' still exist, or has it fractured into multiple, incompatible 'micro-windows' for different political tribes?",
    "Network analysis of cross-partisan idea sharing and sentiment echoes in online discourse.",
    "If fractured, the constraint's nature shifts from a single Tangled Rope governing national discourse to multiple, competing tribal Ropes, fundamentally changing the mechanism of social coordination and suppression.",
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(overton_window, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data for this high-extraction constraint (ε=0.48 > 0.46).
% Models a gradual intensification of discourse management and political
% polarization over the last two decades.
%
% Theater ratio over time (stable and low):
narrative_ontology:measurement(overton_window_tr_t0, overton_window, theater_ratio, 0, 0.1).
narrative_ontology:measurement(overton_window_tr_t5, overton_window, theater_ratio, 5, 0.1).
narrative_ontology:measurement(overton_window_tr_t10, overton_window, theater_ratio, 10, 0.1).

% Extraction over time (extraction_accumulation detection):
narrative_ontology:measurement(overton_window_ex_t0, overton_window, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(overton_window_ex_t5, overton_window, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(overton_window_ex_t10, overton_window, base_extractiveness, 10, 0.48).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% The Overton Window acts as a standard for what constitutes "serious"
% political thought, coordinating messaging among elites.
narrative_ontology:coordination_type(overton_window, information_standard).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides needed. The structural derivation from beneficiary/victim
% declarations and exit options accurately models the dynamics.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */