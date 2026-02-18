% ============================================================================
% CONSTRAINT STORY: solar_system_weirdness
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-15
% ============================================================================

:- module(constraint_solar_system_weirdness, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: solar_system_weirdness
 *   human_readable: The Solar System Configuration Anomaly
 *   domain: technological/scientific
 *
 * SUMMARY:
 *   For decades, astronomers assumed our solar system was a typical model for the
 *   universe. Large-scale surveys since the early 2000s have revealed
 *   that our neat arrangement of four rocky planets and four gas giants is actually
 *   an outlier compared to the more common "super-Earth" or "sub-Neptune" systems
 *   found elsewhere. This constraint represents both the physical reality of the
 *   solar system's configuration and the conceptual paradigm shift it forced in
 *   planetary science.
 *
 * KEY AGENTS (by structural relationship):
 *   - Legacy Copernican Models: Primary target (institutional/trapped) — The prior scientific paradigm that was suppressed and invalidated by new data.
 *   - Modern Planetary Theorists: Primary beneficiary (analytical/arbitrage) — Researchers who use the "weirdness" as a tool to develop more accurate models of planetary formation.
 *   - The Sun/Planets: Physical subjects (powerless/trapped) — The celestial bodies whose configuration is an immutable fact of physics.
 *   - Analytical Observer: The modern astronomer seeing the full structure.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
% Rationale: Purely scientific/informational; no coercive extraction of labor or resources.
domain_priors:base_extractiveness(solar_system_weirdness, 0.20).
% Rationale: The assumption of our system being "normal" suppressed the search
% for anomalies and dominated the Copernican principle for decades. This reflects
% the conceptual aspect of the constraint, not the physical one.
domain_priors:suppression_score(solar_system_weirdness, 0.60).
% Rationale: The constraint is almost entirely functional; the "weirdness" is a
% substantive scientific finding, not a performative act.
domain_priors:theater_ratio(solar_system_weirdness, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(solar_system_weirdness, extractiveness, 0.20).
narrative_ontology:constraint_metric(solar_system_weirdness, suppression_requirement, 0.60).
narrative_ontology:constraint_metric(solar_system_weirdness, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
% These metrics apply to the physical reality aspect of the constraint, which
% is classified as a Mountain. The configuration of the solar system is fixed
% (collapse=1.0) and faces no meaningful resistance (resistance=0.0).
narrative_ontology:constraint_metric(solar_system_weirdness, accessibility_collapse, 1.0).
narrative_ontology:constraint_metric(solar_system_weirdness, resistance, 0.0).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(solar_system_weirdness, rope).
narrative_ontology:human_readable(solar_system_weirdness, "The Solar System Configuration Anomaly").
narrative_ontology:topic_domain(solar_system_weirdness, "technological/scientific").

% --- Emergence flag (required for mountain constraints) ---
% The physical laws of planetary formation emerge naturally.
domain_priors:emerges_naturally(solar_system_weirdness).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(solar_system_weirdness, modern_planetary_theorists).
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(solar_system_weirdness, legacy_copernican_models).

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

% PERSPECTIVE 1: THE SUN/PLANETS (MOUNTAIN)
% Agent who bears the most extraction.
% WHO: powerless (Physical bodies subject to orbital mechanics)
% WHEN: civilizational (The 4.5 billion year history of the system)
% WHERE: trapped (Neat, round orbits with no trajectory exit)
% SCOPE: universal (Governed by universal physical laws)
% WHY: The physical arrangement is an immutable Mountain governed by physics.
constraint_indexing:constraint_classification(solar_system_weirdness, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: LEGACY UNIFORMITARIANS (SNARE)
% Agent who benefits most.
% WHO: institutional (Rule-making academic paradigms of the 20th century)
% WHEN: historical (The era before exoplanet surveys)
% WHERE: trapped (Incapable of imagining Earth as a "weird" outlier)
% SCOPE: global (Academic consensus groups)
% WHY: The "Normalcy Snare" extracted the ability to detect other planet types by
% assuming our system was typical, leading to decades of misdirected search.
constraint_indexing:constraint_classification(solar_system_weirdness, snare,
    context(agent_power(institutional),
            time_horizon(historical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: THE MODERN ASTRONOMER (ROPE)
% Default analytical context (civilizational/analytical/global).
% WHO: analytical (Observer using modern survey data)
% WHEN: biographical (Tracking data since the early 2000s)
% WHERE: arbitrage (Comparing thousands of exoplanets to our own system)
% SCOPE: global (Utilizing telescopes like Kepler and TESS)
% WHY: For the researcher, "weirdness" is a Rope—a functional tool to re-evaluate
% how systems form and to build better models.
constraint_indexing:constraint_classification(solar_system_weirdness, rope,
    context(agent_power(analytical),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(solar_system_weirdness_tests).

test(perspectival_gap) :-
    % Verify perspectival gap between target and beneficiary.
    constraint_indexing:constraint_classification(solar_system_weirdness, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(solar_system_weirdness, TypeBeneficiary, context(agent_power(analytical), _, _, _)),
    TypeTarget \= TypeBeneficiary.

test(multi_perspective_variance) :-
    % Astronomer (Rope) vs Sun (Mountain) vs Legacy Theory (Snare)
    constraint_indexing:constraint_classification(solar_system_weirdness, rope, context(agent_power(analytical), _, _, _)),
    constraint_indexing:constraint_classification(solar_system_weirdness, mountain, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(solar_system_weirdness, snare, context(agent_power(institutional), _, _, _)).

:- end_tests(solar_system_weirdness_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   This story intentionally conflates two related constraints, which is typically
 *   an anti-pattern (see ε-invariance principle). However, it is done here to
 *   model how a physical fact (a Mountain) can become the basis for a conceptual
 *   model that acts as a Snare.
 *   - The physical configuration of the solar system is a Mountain (ε=0, S=0).
 *   - The scientific paradigm of "solar system typicality" was a conceptual Snare
 *     that suppressed alternative theories (S=0.6) and wasted research effort (ε=0.2).
 *   The base metrics (ε=0.2, S=0.6) are chosen to reflect the conceptual Snare, as
 *   this is the aspect with social and scientific impact. The Mountain perspective
 *   is retained to show the physical foundation, and its required NL profile
 *   metrics (accessibility_collapse, resistance) are set to their physical values.
 *
 * PERSPECTIVAL GAP:
 *   - For the planets themselves, the configuration is an immutable Mountain.
 *   - For legacy theorists trapped in the old paradigm, it was a Snare preventing progress.
 *   - For modern astronomers with better data, the anomaly is a Rope—a useful fact
 *     that helps them build better theories.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiaries: Modern planetary theorists who gain career and knowledge capital
 *     by resolving the anomaly.
 *   - Victims: The legacy Copernican models and the researchers invested in them,
 *     whose work was rendered obsolete.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification correctly identifies that a seemingly neutral physical fact
 *   (Mountain) can be instrumentalized into a high-suppression conceptual model
 *   (Snare). The analytical perspective correctly resolves this as a Rope once
 *   the anomaly is understood and used productively, preventing the permanent
 *   mislabeling of a scientific puzzle as pure coercion.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% /5 form: narrative detail for story context
omega_variable(
    omega_solar_system_weirdness_1,
    'Is the solar system weird at the 1% level or the 1-in-a-million level?',
    'Long-term analysis of data from upcoming missions like the Habitable Worlds Observatory.',
    'If 1%: Mountain (minor variance). If 1-in-a-million: Snare (Earth is a precarious fluke).',
    confidence_without_resolution(medium)
).
omega_variable(
    omega_solar_system_weirdness_2,
    'Is the lack of observed Earth-like planets a result of survey bias or actual rarity?',
    'Completion of large-scale surveys looking for Earth-mass planets around G-type stars.',
    'If bias: Rope (just need better tools). If non-existence: Mountain (Earth is truly unique).',
    confidence_without_resolution(high)
).

% /3 form: typed classification for reporting engine (REQUIRED)
narrative_ontology:omega_variable(omega_solar_system_weirdness_1, empirical, 'Calibrating the statistical degree of the solar system anomaly.').
narrative_ontology:omega_variable(omega_solar_system_weirdness_2, empirical, 'Distinguishing between survey bias and actual rarity of Earth-twins.').

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(solar_system_weirdness, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data is not required as base_extractiveness (0.20) is below the
% 0.46 threshold for mandatory lifecycle drift monitoring.

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% No Boltzmann or Network data declared for this constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No directionality overrides are needed; the structural derivation from
% beneficiary/victim declarations is sufficient.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */