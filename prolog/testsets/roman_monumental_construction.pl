% ============================================================================
% CONSTRAINT STORY: roman_monumental_construction
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_roman_monumental_construction, []).

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
 *   constraint_id: roman_monumental_construction
 *   human_readable: The Roman State's Monopoly on Opus Caementicium Construction
 *   domain: socio_technological
 *
 * SUMMARY:
 *   This constraint describes the socio-technical system surrounding Roman
 *   concrete (opus caementicium) for monumental architecture. It was not merely
 *   a building material, but a system of state-controlled resource extraction
 *   (pozzolana), complex supply chains, and specialized labor. This system
 *   enabled unprecedented feats of engineering (aqueducts, the Pantheon) but
 *   also centralized power, suppressed local building traditions, and relied
 *   on coercive taxation and labor from the provinces.
 *
 * KEY AGENTS (by structural relationship):
 *   - Provincial Populace & Enslaved Labor: Primary target (powerless/trapped) — bore the costs of taxation, resource extraction, and forced labor.
 *   - Roman State & Imperial Elite: Primary beneficiary (institutional/arbitrage) — gained infrastructure, social control, economic power, and propaganda value.
 *   - Modern Historian/Archaeologist: Analytical observer — sees the dual nature of coordination and extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(roman_monumental_construction, 0.48).
domain_priors:suppression_score(roman_monumental_construction, 0.65).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(roman_monumental_construction, 0.20).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(roman_monumental_construction, extractiveness, 0.48).
narrative_ontology:constraint_metric(roman_monumental_construction, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(roman_monumental_construction, theater_ratio, 0.20).

% --- NL Profile Metrics (required for mountain constraints) ---
% Not a mountain; these are not applicable.
% narrative_ontology:constraint_metric(roman_monumental_construction, accessibility_collapse, V).
% narrative_ontology:constraint_metric(roman_monumental_construction, resistance, V).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(roman_monumental_construction, tangled_rope).

% --- Binary flags ---
% narrative_ontology:has_sunset_clause(roman_monumental_construction).
domain_priors:requires_active_enforcement(roman_monumental_construction). % Required for Tangled Rope

% --- Emergence flag (required for mountain constraints) ---
% Not a mountain; this was a human-engineered system.
% domain_priors:emerges_naturally(roman_monumental_construction).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.

% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(roman_monumental_construction, roman_state_and_imperial_elite).

% Who bears disproportionate cost?
narrative_ontology:constraint_victim(roman_monumental_construction, provincial_populace_and_enslaved_labor).

% Gate requirements:
%   Tangled Rope: beneficiary + victim + requires_active_enforcement (all three are present)
%   Snare:        victim required (present)

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   where f(d) is the sigmoid directionality function:
     f(d) = -0.20 + 1.70 / (1 + e^(-6*(d - 0.50)))
   The engine derives d from beneficiary/victim membership + exit_options.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   CONTEXT ARITY: All context() terms must have exactly 4 arguments.
   Linter Rule 23 rejects files with context arity ≠ 4.
   ========================================================================== */

% PERSPECTIVE 1: THE PRIMARY TARGET (SNARE)
% The provincial populace and enslaved labor. They experience the system as
% pure coercion and extraction. Engine derives d from:
%   victim membership + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42 → high χ.
% The high ε (0.48) and suppression (0.65) easily cross the Snare threshold.
constraint_indexing:constraint_classification(roman_monumental_construction, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(continental))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (ROPE)
% The Roman state and ruling elite. For them, it is a magnificent tool of
% coordination and power projection. Engine derives d from:
%   beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 → negative χ.
% The system appears as a pure coordination mechanism with no extractive cost.
constraint_indexing:constraint_classification(roman_monumental_construction, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% A modern historian who sees both the coordination benefits and the extractive
% costs. The analytical perspective must synthesize these two views.
% Engine derives d ≈ 0.72 → f(d) ≈ 1.15. The classifier recognizes the presence
% of both a beneficiary (coordination function) and a victim with active
% enforcement (asymmetric extraction), classifying it as a Tangled Rope.
constraint_indexing:constraint_classification(roman_monumental_construction, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(roman_monumental_construction_tests).

test(perspectival_gap_is_snare_vs_rope, [nondet]) :-
    % Verify the core perspectival gap between the powerless target and the institutional beneficiary.
    constraint_indexing:constraint_classification(roman_monumental_construction, snare, context(agent_power(powerless), _, exit_options(trapped), _)),
    constraint_indexing:constraint_classification(roman_monumental_construction, rope, context(agent_power(institutional), _, exit_options(arbitrage), _)).

test(analytical_view_is_tangled_rope, [nondet]) :-
    % Ensure the analytical observer correctly classifies the constraint as Tangled Rope.
    constraint_indexing:constraint_classification(roman_monumental_construction, tangled_rope, context(agent_power(analytical), _, _, _)).

test(tangled_rope_gate_requirements_met) :-
    % Verify that all three structural requirements for a Tangled Rope are declared.
    narrative_ontology:constraint_beneficiary(roman_monumental_construction, _),
    narrative_ontology:constraint_victim(roman_monumental_construction, _),
    domain_priors:requires_active_enforcement(roman_monumental_construction).

:- end_tests(roman_monumental_construction_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.48): Represents the significant costs imposed by the system, including heavy taxation, land appropriation for quarries, and the use of coerced or enslaved labor. This value is high enough to be perceived as a Snare by its victims.
 *   - Suppression (0.65): The Roman state actively monopolized the best pozzolana sources and engineering talent, creating high barriers to entry for provincial or private alternatives and suppressing indigenous building traditions. This score reflects a high degree of coercion.
 *   - Theater Ratio (0.20): While monumental buildings had a strong propaganda function (theater), they were also highly functional (aqueducts providing water, arenas for social control). The ratio of performative to functional activity was low during the system's peak.
 *
 * PERSPECTIVAL GAP:
 *   The gap is profound and defines the Roman imperial project.
 *   - From the perspective of the provincial taxpayer or enslaved quarry worker (powerless/trapped), the system is a SNAPE. It is a coercive mechanism of extraction from which there is no escape, and the benefits (a distant aqueduct in Rome) are abstract or non-existent.
 *   - From the perspective of the Emperor or a Roman senator (institutional/arbitrage), the system is a ROPE. It is a brilliant coordination tool that marshals the empire's resources to create public goods, project power, and ensure stability. The extractive costs are externalized and thus invisible.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality is explicitly modeled through the beneficiary/victim declarations. The `roman_state_and_imperial_elite` are beneficiaries; their institutional power and arbitrage exit options (choosing which grand projects to fund) give them a very low directionality `d`, resulting in a negative effective extraction (χ). The `provincial_populace_and_enslaved_labor` are victims; their powerlessness and trapped status give them a very high `d`, maximizing χ and leading to the Snare classification. This mapping directly reflects the structural reality of the Roman Empire's political economy.
 *
 * MANDATROPHY ANALYSIS:
 *   This is a canonical example of a Tangled Rope. A purely libertarian analysis might focus only on the coercion and taxation, mislabeling it a simple Snare and ignoring the genuine coordination that produced functional wonders like the aqueducts. Conversely, a purely statist or progress-focused analysis might see only the magnificent end products, mislabeling it a pure Rope and ignoring the immense human cost. The Tangled Rope classification correctly identifies it as an integrated system of BOTH coordination AND extraction, preventing either simplistic mischaracterization.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_roman_construction,
    'To what degree was monumental construction funded by willing participation/civic pride versus coercive extraction and enslaved labor?',
    'Detailed archaeological and textual analysis of provincial tax records, labor sourcing contracts, and epigraphic evidence of private vs. state funding for local projects.',
    'If evidence showed a high degree of voluntary participation and local funding, the base extractiveness (ε) would be lower, leaning the analytical view more towards a Rope. If it showed overwhelming reliance on coercion, it would solidify the Snare/Tangled Rope classification.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(roman_monumental_construction, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data enables drift detection. As a high-extraction constraint
% (ε=0.48 > 0.46), this is required. The interval represents the period from
% the Late Republic (T=0) to the Late Empire (T=10).

% Theater ratio over time (slight increase as spectacle became more central):
narrative_ontology:measurement(rmc_tr_t0, roman_monumental_construction, theater_ratio, 0, 0.10).
narrative_ontology:measurement(rmc_tr_t5, roman_monumental_construction, theater_ratio, 5, 0.15).
narrative_ontology:measurement(rmc_tr_t10, roman_monumental_construction, theater_ratio, 10, 0.20).

% Extraction over time (increased as the empire's fiscal needs grew):
narrative_ontology:measurement(rmc_ex_t0, roman_monumental_construction, base_extractiveness, 0, 0.40).
narrative_ontology:measurement(rmc_ex_t5, roman_monumental_construction, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(rmc_ex_t10, roman_monumental_construction, base_extractiveness, 10, 0.48).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% The constraint is primarily about managing vast resources and labor forces.
narrative_ontology:coordination_type(roman_monumental_construction, resource_allocation).

% --- Network Decomposition (Constraint Families) ---
% This story focuses on the socio-technical SYSTEM. The underlying technology
% is a separate constraint with a much lower ε.

% DUAL FORMULATION NOTE:
% This constraint is one of 2 stories decomposed from the label "Roman Concrete".
% Decomposed because ε differs across observables (ε-invariance principle).
% Related stories:
%   - roman_concrete_recipe (ε≈0.15, Rope)
%
% The recipe (low ε) enables the monumental construction system (high ε).
narrative_ontology:affects_constraint(roman_concrete_recipe, roman_monumental_construction).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are needed for this constraint. The standard derivation chain
% (beneficiary/victim declarations + exit options) accurately models the
% directionality of extraction from the provinces to the imperial center.
%
% constraint_indexing:directionality_override(roman_monumental_construction, PowerAtom, D_Value).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */