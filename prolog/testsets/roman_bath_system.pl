% ============================================================================
% CONSTRAINT STORY: roman_bath_system
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-06-25
% ============================================================================

:- module(constraint_roman_bath_system, []).

:- use_module(library(plunit)).
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
    constraint_indexing:directionality_override/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: roman_bath_system
 *   human_readable: The Roman System of Public Baths
 *   domain: technological/social
 *
 * SUMMARY:
 *   The Roman Empire developed a massive, state-subsidized network of public
 *   baths (thermae) that provided hygiene, recreation, and social centers for
 *   the populace at little to no cost. This system served a genuine public
 *   health and social cohesion function but was built and maintained through
 *   extractive means, including taxes levied on conquered provinces and the
 *   use of enslaved labor, creating a significant structural tension.
 *
 * KEY AGENTS (by structural relationship):
 *   - Enslaved Laborers / Provincial Taxpayers: Primary targets (powerless/trapped) — bear the cost of construction and maintenance through forced labor and taxation.
 *   - Roman Citizenry: Primary beneficiaries (moderate/mobile) — receive the benefits of hygiene, health, and social life for a nominal fee.
 *   - The Roman State: Institutional beneficiary (institutional/arbitrage) — uses the system to maintain public order, project power, and improve public health.
 *   - Modern Historian/Engineer: Analytical observer — sees the full structure, including both the coordination benefits and the extractive costs.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(roman_bath_system, 0.48). % Represents the massive cost in labor/resources extracted to build and run the system.
domain_priors:suppression_score(roman_bath_system, 0.65).   % Structural property (raw, unscaled). The state-subsidized system made private alternatives for the masses unviable.
domain_priors:theater_ratio(roman_bath_system, 0.15).       % In its prime, highly functional. Not a Piton.

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(roman_bath_system, extractiveness, 0.48).
narrative_ontology:constraint_metric(roman_bath_system, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(roman_bath_system, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
% N/A for this constraint.

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(roman_bath_system, tangled_rope).

% --- Binary flags ---
domain_priors:requires_active_enforcement(roman_bath_system). % Required constant state funding, maintenance, and operation (e.g., heating the hypocaust).

% --- Emergence flag (required for mountain constraints) ---
% N/A. This was a human-engineered system.

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain.
%
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(roman_bath_system, roman_citizenry).
narrative_ontology:constraint_beneficiary(roman_bath_system, roman_state).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(roman_bath_system, enslaved_laborers).
narrative_ontology:constraint_victim(roman_bath_system, provincial_taxpayers).
%
% Gate requirements:
%   Tangled Rope: beneficiary + victim + requires_active_enforcement (all three satisfied)

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   where f(d) is the sigmoid directionality function:
     f(d) = -0.20 + 1.70 / (1 + e^(-6*(d - 0.50)))
   The engine derives d from beneficiary/victim membership + exit_options.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   CONTEXT ARITY: All context() terms must have exactly 4 arguments.
   ========================================================================== */

% PERSPECTIVE 1: THE PRIMARY TARGET (SNARE)
% From the perspective of an enslaved laborer or a heavily taxed provincial,
% the system is pure extraction of their life/resources for others' benefit.
% Engine derives: victim membership + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42 → high χ
constraint_indexing:constraint_classification(roman_bath_system, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(continental))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (ROPE)
% From the perspective of a Roman citizen, the baths are a low-cost public good.
% A pure coordination mechanism for health and social life.
% Engine derives d from beneficiary status + mobile exit -> d ≈ 0.15 -> f(d) ≈ -0.01 -> low χ
constraint_indexing:constraint_classification(roman_bath_system, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 3: THE INSTITUTIONAL BENEFICIARY (ROPE)
% The Roman state sees the system as a tool of governance and social engineering.
% The extractive costs are an investment for achieving social stability.
% Engine derives: beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 → negative χ
constraint_indexing:constraint_classification(roman_bath_system, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% A modern historian sees both the genuine coordination function and the
% asymmetric extractive foundation, plus the active enforcement. This is the
% canonical definition of a Tangled Rope.
% Engine derives d ≈ 0.72 → f(d) ≈ 1.15 for analytical perspective.
constraint_indexing:constraint_classification(roman_bath_system, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).


/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(roman_bath_system_tests).

test(perspectival_gap_target_vs_beneficiary, [nondet]) :-
    constraint_indexing:constraint_classification(roman_bath_system, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(roman_bath_system, rope, context(agent_power(moderate), _, _, _)),
    constraint_indexing:constraint_classification(roman_bath_system, rope, context(agent_power(institutional), _, _, _)).

test(analytical_classification_is_tangled_rope) :-
    constraint_indexing:constraint_classification(roman_bath_system, tangled_rope, context(agent_power(analytical), _, _, _)).

test(tangled_rope_structural_requirements_met) :-
    narrative_ontology:constraint_beneficiary(roman_bath_system, _),
    narrative_ontology:constraint_victim(roman_bath_system, _),
    domain_priors:requires_active_enforcement(roman_bath_system).

:- end_tests(roman_bath_system_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.48): This value reflects the immense capital and labor
 *     investment required for the bath system, including aqueducts, massive
 *     structures, and constant fuel for the hypocaust system. This cost was not
 *     borne by the users but extracted from the broader imperial economy,
 *     primarily through provincial taxes and enslaved labor.
 *   - Suppression (0.65): The state's provision of cheap, high-quality baths
 *     effectively crowded out any potential private market for mass hygiene,
 *     making it the sole option for the vast majority of the urban populace.
 *   - The combination of a genuine coordination function (public health, social
 *     cohesion) and a high, asymmetrically applied extractive cost makes this
 *     a canonical Tangled Rope.
 *
 * PERSPECTIVAL GAP:
 *   The gap is immense and demonstrates the power of the indexical system.
 *   - For an enslaved quarry worker (powerless, trapped), the marble block he cuts
 *     is pure extracted value for a benefit he will never see. The system is a Snare.
 *   - For a Roman citizen (moderate, mobile), the baths are a near-free service, a
 *     marvel of civic engineering that improves their life. The system is a Rope.
 *   - The analytical view acknowledges both realities are true simultaneously. The
 *     Rope experienced by the citizen is structurally dependent on the Snare
 *     experienced by the laborer. This is the definition of a Tangled Rope.
 *
 * DIRECTIONALITY LOGIC:
 *   The `constraint_beneficiary` and `constraint_victim` declarations are key. By
 *   identifying `roman_citizenry` as beneficiaries and `enslaved_laborers`/
 *   `provincial_taxpayers` as victims, we provide the structural data for the
 *   engine to derive directionality (d). A citizen's low `d` value results in a
 *   low effective extraction (χ), classifying the system as a Rope. A laborer's
 *   high `d` value results in a high χ, classifying it as a Snare. The model
 *   thus computationally derives the perspectival gap from first principles.
 *
 * MANDATROPHY ANALYSIS:
 *   This case is a powerful antidote to Mandatrophy. A purely functionalist
 *   analysis might praise the baths as a public good (Rope) and ignore the costs.
 *   A purely critical analysis might condemn them as a tool of empire built on
 *   exploitation (Snare). The Tangled Rope classification avoids this false
 *   dichotomy. It correctly identifies the system as having *both* a genuine
 *   coordination function *and* an asymmetric extractive foundation, which is
 *   a more accurate and complete structural description.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_roman_bath_system,
    'What was the net welfare impact of the Roman bath system, accounting for both the public health gains for citizens and the extractive costs imposed on provincials and slaves?',
    'Quantitative economic history modeling, comparing public health data (e.g., from skeletal remains) against estimates of the economic burden of taxation and the human cost of slavery.',
    'If net welfare was strongly positive, the coordination aspect dominates. If negative, the extractive aspect dominates. This would not change the Tangled Rope classification but would alter our interpretation of its historical valence.',
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(roman_bath_system, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data models the system's evolution from the Republic to the late Empire.
% It starts as a smaller-scale system and grows into a massive imperial institution.

% Theater ratio over time: Initially low, may have increased in the late empire
% as function degraded but spectacle remained important.
narrative_ontology:measurement(roman_bath_system_tr_t0, roman_bath_system, theater_ratio, 0, 0.10).
narrative_ontology:measurement(roman_bath_system_tr_t5, roman_bath_system, theater_ratio, 5, 0.15).
narrative_ontology:measurement(roman_bath_system_tr_t10, roman_bath_system, theater_ratio, 10, 0.30).

% Extraction over time: Increased dramatically as the empire expanded and
% could fund ever-larger projects through conquest and taxation.
narrative_ontology:measurement(roman_bath_system_ex_t0, roman_bath_system, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(roman_bath_system_ex_t5, roman_bath_system, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(roman_bath_system_ex_t10, roman_bath_system, base_extractiveness, 10, 0.45).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% The bath system was a piece of large-scale civic infrastructure.
narrative_ontology:coordination_type(roman_bath_system, global_infrastructure).

% Network relationships (structural influence edges)
% The baths were structurally dependent on the aqueduct system for water.
narrative_ontology:affects_constraint(roman_aqueduct_system, roman_bath_system).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are necessary for this constraint. The standard derivation
% based on the declared beneficiary/victim groups and their exit options
% accurately models the structural dynamics of the Roman bath system.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */