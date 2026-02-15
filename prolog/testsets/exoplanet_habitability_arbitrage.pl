% ============================================================================
% CONSTRAINT STORY: exoplanetary_habitability_arbitrage
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-29
% ============================================================================

:- module(constraint_exoplanetary_habitability_arbitrage, []).

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
    narrative_ontology:interval/3,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: exoplanetary_habitability_arbitrage
 *   human_readable: Exoplanetary Habitability Arbitrage Strategy
 *   domain: technological/scientific
 *
 * SUMMARY:
 *   This constraint represents the strategic pivot in the search for extraterrestrial
 *   life, moving from seeking "Earth twins" to identifying anomalies based on our
 *   solar system's observed statistical "weirdness". Researchers arbitrage the
 *   gap between the commonality of "super-Earth" systems and the rarity of
 *   systems like our own to prioritize high-cost observation targets. The
 *   constraint is the research strategy itself, not the underlying physical
 *   rarity of our solar system.
 *
 * KEY AGENTS (by structural relationship):
 *   - Legacy Planet Hunters: Primary target (powerless/trapped) — Their research paradigm, focused on finding Earth 2.0, is de-funded and suppressed by the new strategy.
 *   - Modern Planetary Theorists: Primary beneficiary (analytical/arbitrage) — Benefit from the "rewrite" of planetary formation stories and the reallocation of resources to anomaly detection.
 *   - Analytical Observer: Sees the full structure of the resource reallocation and paradigm shift.
 *
 * DUAL FORMULATION NOTE:
 * This constraint is one of two stories decomposed from the colloquial concept of "Rare Earth".
 * Decomposed because ε differs across observables (ε-invariance principle).
 *   - solar_system_rarity (ε≈0.05, Mountain): The underlying physical fact of our system's statistical rarity. This is a natural law.
 *   - exoplanetary_habitability_arbitrage (ε=0.45, Rope/Snare): The human-devised research strategy that exploits this rarity. This story models the strategy, which is a coordination mechanism with extractive effects on prior research programs.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
% Rationale: High-cost missions like the Habitable Worlds Observatory extract
% resources (funding, telescope time) from general astronomy to fund specialized
% searches based on the "weirdness" arbitrage.
domain_priors:base_extractiveness(exoplanetary_habitability_arbitrage, 0.45).

% Rationale: The new paradigm, focusing on our system's rarity, actively
% suppresses the prior search for "typical" Earth twins by redirecting funding
% and observation priority.
domain_priors:suppression_score(exoplanetary_habitability_arbitrage, 0.55).

% Rationale: The strategy is a functional coordination mechanism for research,
% with very little performative or theatrical action.
domain_priors:theater_ratio(exoplanetary_habitability_arbitrage, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(exoplanetary_habitability_arbitrage, extractiveness, 0.45).
narrative_ontology:constraint_metric(exoplanetary_habitability_arbitrage, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(exoplanetary_habitability_arbitrage, theater_ratio, 0.08).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(exoplanetary_habitability_arbitrage, rope).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(exoplanetary_habitability_arbitrage, modern_planetary_theorists).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(exoplanetary_habitability_arbitrage, legacy_planet_hunters).

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

% PERSPECTIVE 1: THE LEGACY PLANET HUNTER (SNARE)
% Agent whose research paradigm is being de-funded and suppressed.
% Engine derives d from: victim membership + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42 → high χ
constraint_indexing:constraint_classification(exoplanetary_habitability_arbitrage, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE MODERN PLANETARY THEORIST (ROPE)
% Agent who benefits from the new paradigm and resource allocation.
% Engine derives d from: beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 → low/negative χ
constraint_indexing:constraint_classification(exoplanetary_habitability_arbitrage, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (ROPE)
% Default analytical context. Sees the strategy as a functional coordination
% mechanism, albeit one with extractive consequences for some parties.
% Engine derives d ≈ 0.72 → f(d) ≈ 1.15 for analytical perspective.
constraint_indexing:constraint_classification(exoplanetary_habitability_arbitrage, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(exoplanetary_habitability_arbitrage_tests).

test(perspectival_gap) :-
    % Verify perspectival gap between target (Snare) and beneficiary (Rope).
    constraint_indexing:constraint_classification(exoplanetary_habitability_arbitrage, snare,
        context(agent_power(powerless), time_horizon(immediate), exit_options(trapped), spatial_scope(national))),
    constraint_indexing:constraint_classification(exoplanetary_habitability_arbitrage, rope,
        context(agent_power(institutional), time_horizon(generational), exit_options(arbitrage), spatial_scope(global))),
    constraint_indexing:constraint_classification(exoplanetary_habitability_arbitrage, rope,
        context(agent_power(analytical), time_horizon(civilizational), exit_options(analytical), spatial_scope(global))).

test(threshold_validation) :-
    % The base extractiveness should be in the Rope/Tangled Rope range, not Mountain/Snare extremes.
    narrative_ontology:constraint_metric(exoplanetary_habitability_arbitrage, extractiveness, E),
    E > 0.30, E < 0.60.

:- end_tests(exoplanetary_habitability_arbitrage_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The original file conflated the physical reality of solar system rarity (a
 *   Mountain) with the human research strategy built upon it (a Rope/Snare).
 *   This created a metric conflict: the ε=0.45 of the strategy was inconsistent
 *   with the Mountain classification. This version resolves the conflict by
 *   modeling only the strategy. The `emerges_naturally` flag and the Mountain
 *   perspective were removed, as the strategy is a human construct. The base
 *   extractiveness of 0.45 reflects the significant reallocation of funding and
 *   resources toward anomaly detection, away from prior paradigms. The
 *   suppression score of 0.55 reflects how this new paradigm actively makes it
 *   harder to pursue the older "Earth twin" search model.
 *
 * PERSPECTIVAL GAP:
 *   The gap is between the beneficiaries and the victims of a scientific
 *   paradigm shift. For modern theorists, the arbitrage strategy is a Rope—a
 *   powerful coordination tool to efficiently allocate resources. For legacy
 *   planet hunters, whose methods and assumptions are now de-prioritized, the
 *   same strategy is a Snare that traps them in a research cul-de-sac with
 *   dwindling resources.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiaries: `modern_planetary_theorists` who gain funding, prestige, and a new theoretical framework. Their arbitrage exit option gives them a low directionality `d`.
 *   - Victims: `legacy_planet_hunters` who lose funding and whose paradigm is suppressed. Their trapped status gives them a high directionality `d`.
 *   This structural relationship drives the Rope/Snare classification gap.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification correctly identifies the dual nature of the constraint.
 *   It is not pure extraction (a Snare from all views) because it serves a
 *   genuine coordination function: prioritizing astronomical search targets.
 *   It is not pure coordination (a Rope from all views) because this
 *   prioritization has clear, asymmetric extractive effects on a specific group
 *   of researchers. The framework avoids mislabeling this resource conflict as
 *   either pure science or pure politics.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Omega variables — open questions the framework cannot yet resolve
%
% /5 form: narrative detail for story context
omega_variable(
    exoplanetary_habitability_arbitrage_omega_1,
    "Is the focus on 'weird' systems a functional necessity for finding life or a strategic choice to sustain high-tech funding?",
    "Audit of discovery rates for habitable worlds in 'weird' vs 'typical' star systems.",
    "If necessity: Rope (functional coordination). If strategic choice: Tangled Rope (coordination + asymmetric extraction).",
    confidence_without_resolution(medium)
).

omega_variable(
    exoplanetary_habitability_arbitrage_omega_2,
    "Is the solar system's weirdness a 1-in-100 fluke or a 1-in-a-million miracle?",
    "Completion of large-scale, long-term surveys of G-type stars for Earth-mass planets.",
    "Affects the underlying Mountain constraint (solar_system_rarity), which in turn affects the urgency and parameters of this strategy.",
    confidence_without_resolution(low)
).

% /3 form: typed classification for reporting engine (REQUIRED)
narrative_ontology:omega_variable(exoplanetary_habitability_arbitrage_omega_1, conceptual, "Is the research strategy driven by necessity or by funding incentives?").
narrative_ontology:omega_variable(exoplanetary_habitability_arbitrage_omega_2, empirical, "What is the true statistical frequency of solar systems like ours?").

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(exoplanetary_habitability_arbitrage, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Base extractiveness is < 0.46, so temporal measurements are not required.

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% The strategy is a mechanism for allocating scarce research resources.
narrative_ontology:coordination_type(exoplanetary_habitability_arbitrage, resource_allocation).

% Network relationships (structural influence edges)
% The underlying physical reality (a Mountain) enables this strategy.
narrative_ontology:affects_constraint(solar_system_rarity, exoplanetary_habitability_arbitrage).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides needed. The structural derivation from beneficiary/victim
% declarations and exit options accurately models the dynamics.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */