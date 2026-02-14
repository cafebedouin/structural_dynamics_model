% ============================================================================
% CONSTRAINT STORY: ship_of_theseus
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-27
% ============================================================================

:- module(constraint_ship_of_theseus, []).

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
    domain_priors:emerges_naturally/1.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: ship_of_theseus
 *   human_readable: Identity Continuity Constraint
 *   domain: philosophical
 *
 * SUMMARY:
 *   The Ship of Theseus is a philosophical thought experiment that raises the question of whether an object that has had all of its component parts replaced remains fundamentally the same object. It represents the challenge of defining identity and continuity in the face of gradual or complete change. This constraint explores the tension between perceived continuity and actual component replacement.
 *
 * KEY AGENTS (by structural relationship):
 *   - Believer in Original Identity: Primary target (powerless/trapped) — bears extraction (cognitive dissonance)
 *   - Law/Contractual Framework: Primary beneficiary (institutional/constrained) — benefits from stable identity for legal reasons
 *   - Pragmatist: Secondary beneficiary (analytical/analytical) — benefits from flexibility in defining identity
 *   - Analytical Observer: Analytical observer (analytical/analytical) — sees full structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ship_of_theseus, 0.35). % Moderate cognitive extraction
domain_priors:suppression_score(ship_of_theseus, 0.30).   % Some suppression of alternative interpretations
domain_priors:theater_ratio(ship_of_theseus, 0.80).       % High theater: the "identity" is a performative label maintained over changing substance.

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ship_of_theseus, extractiveness, 0.35).
narrative_ontology:constraint_metric(ship_of_theseus, suppression_requirement, 0.30).
narrative_ontology:constraint_metric(ship_of_theseus, theater_ratio, 0.80).

% --- NL Profile Metrics (required for mountain constraints) ---
% These feed the natural_law_signature certification chain in
% structural_signatures.pl. Uncomment and set for mountain constraints.
% Without these, the NL signature defaults to 0.5 and fails certification.
%
% narrative_ontology:constraint_metric(ship_of_theseus, accessibility_collapse, 0.95).
% narrative_ontology:constraint_metric(ship_of_theseus, resistance, 0.05).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(ship_of_theseus, piton).

% --- Binary flags ---
% narrative_ontology:has_sunset_clause(ship_of_theseus).      % Mandatory if Scaffold
% domain_priors:requires_active_enforcement(ship_of_theseus). % Required for Tangled Rope

% --- Emergence flag (required for mountain constraints) ---
% Uncomment for constraints that emerge naturally without human design
% or enforcement. Required for the mountain metric gate: without this,
% the classify_from_metrics mountain clause will not fire.
%
% domain_priors:emerges_naturally(ship_of_theseus).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.
% Without these, the engine falls back to generic power-atom assumptions.
%
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(ship_of_theseus, legal_frameworks).
narrative_ontology:constraint_beneficiary(ship_of_theseus, pragmatists).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(ship_of_theseus, believers_in_original_identity).
%
% Gate requirements:
%   Tangled Rope: beneficiary + victim + requires_active_enforcement (all three)
%   Scaffold:     beneficiary + (has_sunset_clause OR no enforcement)
%   Snare:        victim required; beneficiary optional

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

% PERSPECTIVE 1: THE PRIMARY TARGET (TANGLED ROPE)
% Agent who bears the most extraction. Engine derives d from:
%   victim membership + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42 → high χ
constraint_indexing:constraint_classification(ship_of_theseus, tangled_rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (ROPE)
% The legal system benefits from the coordination function of stable identity.
constraint_indexing:constraint_classification(ship_of_theseus, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (PITON)
% The analytical observer sees the "identity" as a label maintained through
% inertia and performance, while the underlying substance has atrophied or
% been replaced. The original function (being *that specific ship*) is gone.
constraint_indexing:constraint_classification(ship_of_theseus, piton,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).


/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ship_of_theseus_tests).

test(perspectival_gap) :-
    % Verify perspectival gap between target and beneficiary.
    constraint_indexing:constraint_classification(ship_of_theseus, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ship_of_theseus, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget \= TypeBeneficiary.

test(piton_threshold_validation) :-
    % A piton classification requires theater_ratio >= 0.70.
    narrative_ontology:constraint_metric(ship_of_theseus, theater_ratio, TR),
    TR >= 0.70.

:- end_tests(ship_of_theseus_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The Ship of Theseus is re-evaluated as a Piton from the analytical perspective. The "identity" of the ship is a label that persists due to institutional inertia and performative maintenance (e.g., museum plaques, legal documents), while its original substance and function (being the actual ship Theseus sailed) has completely atrophied. The theater_ratio is high (0.80) because the activities surrounding the ship are about maintaining the *idea* of the ship, not its material reality. Base extractiveness (0.35) represents the cognitive tax on those who must reconcile the label with the reality. Suppression (0.30) is moderate, as alternative views are possible but institutionally inconvenient.
 *
 * PERSPECTIVAL GAP:
 *   - The 'believer in original identity' (powerless) sees a Tangled Rope. They are trapped by a belief system that has a coordination function (shared history) but also extracts from them via cognitive dissonance as the material reality changes.
 *   - The legal framework (institutional) sees a Rope. It benefits from the pure coordination function of a stable, legally continuous identity, ignoring the material changes which are irrelevant to its function.
 *   - The analytical observer sees a Piton. They recognize that the original function is gone and what remains is an inert label maintained by theater.
 *
 * DIRECTIONALITY LOGIC:
 *   - Victims: 'believers_in_original_identity' bear the cost of reconciling a performative identity with material fact.
 *   - Beneficiaries: 'legal_frameworks' and 'pragmatists' benefit from the coordination provided by the stable (if theatrical) identity, which simplifies contracts, ownership, and history.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying the analytical view as a Piton correctly identifies that the constraint's primary function has degraded. It is no longer about being a specific object but about performing the role of that object. This prevents mislabeling the institutional inertia as a functional coordination (Rope) from all perspectives.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_ship_of_theseus,
    'Is continuity of function more important than material continuity in defining identity?',
    'Examining real-world examples where identity is crucial, such as legal ownership or inheritance rights.',
    'If true, the ship remains the same (Rope); if false, it becomes a different entity maintained by theater (Piton).',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(ship_of_theseus, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data enables drift detection (metric_substitution,
% extraction_accumulation) by providing measurements at multiple time points.
% The ship starts as a functional object (low theater) and degrades into a
% performative one (high theater) as its parts are replaced.
%
% Theater ratio over time (triggers metric_substitution detection):
narrative_ontology:measurement(ship_of_theseus_tr_t0, ship_of_theseus, theater_ratio, 0, 0.10).
narrative_ontology:measurement(ship_of_theseus_tr_t5, ship_of_theseus, theater_ratio, 5, 0.50).
narrative_ontology:measurement(ship_of_theseus_tr_t10, ship_of_theseus, theater_ratio, 10, 0.80).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(ship_of_theseus_ex_t0, ship_of_theseus, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(ship_of_theseus_ex_t5, ship_of_theseus, base_extractiveness, 5, 0.35).
narrative_ontology:measurement(ship_of_theseus_ex_t10, ship_of_theseus, base_extractiveness, 10, 0.35).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% Valid types: information_standard, resource_allocation,
%              enforcement_mechanism, global_infrastructure
narrative_ontology:coordination_type(ship_of_theseus, information_standard).

% Boltzmann floor override (only if domain knowledge justifies)
% Value must be in [0.0, 1.0]
% narrative_ontology:boltzmann_floor_override(ship_of_theseus, [0.0-1.0]).

% Network relationships (structural influence edges)
% Declare when constraints share regulatory domain, causal dependency,
% or institutional coupling.
% narrative_ontology:affects_constraint(ship_of_theseus, [other_constraint_id]).

% --- Network Decomposition (Constraint Families) ---
% When a natural-language label covers multiple constraints with different ε
% values, each gets its own file. Link family members with affects_constraint:
%
% DUAL FORMULATION NOTE:
% This constraint is one of [N] stories decomposed from [colloquial label].
% Decomposed because ε differs across observables (ε-invariance principle).
% Related stories:
%   - [sibling_constraint_1] (ε=[value], [Type])
%   - [sibling_constraint_2] (ε=[value], [Type])
%
% narrative_ontology:affects_constraint(ship_of_theseus, [sibling_constraint_id]).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% Use ONLY when the automatic derivation (beneficiary/victim + exit → d)
% would produce an inaccurate directionality value. The derivation chain
% priority is: override > structural > canonical fallback.
%
% Format: directionality_override(ConstraintID, PowerAtom, D_Value)
%   D_Value in [0.0, 1.0]: 0.0 = full beneficiary, 1.0 = full target
%
% Common override scenarios:
%   - Regulatory capture: institution that appears to benefit but is
%     actually partly captured → override d upward (0.25-0.40)
%   - Indirect beneficiary: agent in victim group who actually benefits
%     through secondary effects → override d downward
%   - Asymmetric institutional: two institutional actors that the
%     derivation can't distinguish → override to differentiate
%
% Example (uncomment if needed):
% constraint_indexing:directionality_override(ship_of_theseus, institutional, 0.30).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */