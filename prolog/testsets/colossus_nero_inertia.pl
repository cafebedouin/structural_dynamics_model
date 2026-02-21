% ============================================================================
% CONSTRAINT STORY: colossus_nero_inertia
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_colossus_nero_inertia, []).

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
    narrative_ontology:coordination_type/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: colossus_nero_inertia
 *   human_readable: The Political and Physical Inertia of the Colossus of Nero
 *   domain: political/social
 *
 * SUMMARY:
 *   The Colossus of Nero was a 30-meter bronze statue of Emperor Nero. After
 *   his death and damnatio memoriae, the statue was too massive and expensive
 *   to simply destroy. This constraint represents its physical and political
 *   inertia: subsequent rulers were forced to engage with it, typically by
 *   repurposing it (changing the head) to project their own power. The object
 *   functioned as both a civic landmark (coordination) and a coercive tool
 *   of imperial propaganda funded by the populace (extraction).
 *
 * KEY AGENTS (by structural relationship):
 *   - Roman Populace: Primary target (powerless/trapped) — bore the immense
 *     cost of its construction and were subjected to its propaganda.
 *   - Imperial Rulers (Vespasian, Commodus, etc.): Primary beneficiary
 *     (institutional/arbitrage) — co-opted the existing monument to
 *     legitimize their rule at a lower cost than building a new one.
 *   - Analytical Observer: Sees the full structure of coordination and
 *     extraction over its lifecycle.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(colossus_nero_inertia, 0.48).
domain_priors:suppression_score(colossus_nero_inertia, 0.75).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(colossus_nero_inertia, 0.80).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(colossus_nero_inertia, extractiveness, 0.48).
narrative_ontology:constraint_metric(colossus_nero_inertia, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(colossus_nero_inertia, theater_ratio, 0.80).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(colossus_nero_inertia, tangled_rope).

% --- Binary flags ---
domain_priors:requires_active_enforcement(colossus_nero_inertia). % Political will to maintain/repurpose vs destroy.

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.

% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(colossus_nero_inertia, imperial_rulers).

% Who bears disproportionate cost?
narrative_ontology:constraint_victim(colossus_nero_inertia, roman_populace).

% Gate requirements check:
%   Tangled Rope: beneficiary + victim + requires_active_enforcement (all three are present).

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

% PERSPECTIVE 1: THE ROMAN POPULACE (SNARE)
% As the primary target, they experience the monument as a pure extraction of
% public funds for an autocrat's glory.
% Engine derives d from: victim membership + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42.
% χ = 0.48 * 1.42 * 1.0 (national scope) ≈ 0.68. This is ≥ 0.66, classifying as Snare.
constraint_indexing:constraint_classification(colossus_nero_inertia, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE IMPERIAL RULERS (ROPE)
% As beneficiaries, they see the statue as a low-cost tool for projecting
% power and providing a landmark for the city, a coordination function.
% Engine derives d from: beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12.
% χ = 0.48 * -0.12 * 1.0 ≈ -0.06. Negative χ is a strong Rope signature.
constraint_indexing:constraint_classification(colossus_nero_inertia, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% The historian sees both the coordination function (civic landmark) and the
% asymmetric extraction and high suppression. This dual nature is the
% definition of a Tangled Rope.
% Engine derives d ≈ 0.72 → f(d) ≈ 1.15 for the analytical perspective.
% χ = 0.48 * 1.15 * 1.2 (global scope) ≈ 0.66. This high χ, combined with the
% recognized coordination function, confirms Tangled Rope.
constraint_indexing:constraint_classification(colossus_nero_inertia, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: THE SYSTEMS AUDITOR (PITON)
% Analyzing the statue in its final phase (e.g., under Commodus), its original
% function has atrophied, replaced by pure political theater. The high theater
% ratio (0.80) makes it a Piton from this perspective.
constraint_indexing:constraint_classification(colossus_nero_inertia, piton,
    context(agent_power(analytical),
            time_horizon(historical),
            exit_options(analytical),
            spatial_scope(regional))) :-
    domain_priors:theater_ratio(colossus_nero_inertia, TR), TR >= 0.70.

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(colossus_nero_inertia_tests).

test(perspectival_gap_target_beneficiary) :-
    % Verify the core perspectival gap between the populace and rulers.
    constraint_indexing:constraint_classification(colossus_nero_inertia, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(colossus_nero_inertia, rope, context(agent_power(institutional), _, _, _)).

test(analytical_view_is_tangled_rope) :-
    % The system's canonical claim should be Tangled Rope.
    constraint_indexing:constraint_classification(colossus_nero_inertia, tangled_rope, context(agent_power(analytical), time_horizon(civilizational), _, _)).

test(piton_degradation_is_detectable) :-
    % Ensure the Piton classification is reachable given high theater.
    (   domain_priors:theater_ratio(colossus_nero_inertia, TR), TR >= 0.70
    ->  constraint_indexing:constraint_classification(colossus_nero_inertia, piton, context(agent_power(analytical), time_horizon(historical), _, _))
    ;   true ).

test(tangled_rope_gate_requirements_met) :-
    % Verify that the structural preconditions for a Tangled Rope are met.
    narrative_ontology:constraint_beneficiary(colossus_nero_inertia, _),
    narrative_ontology:constraint_victim(colossus_nero_inertia, _),
    domain_priors:requires_active_enforcement(colossus_nero_inertia).

:- end_tests(colossus_nero_inertia_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (0.48): Represents the enormous cost in materials
 *     (bronze) and labor extracted from the Roman economy to build the statue.
 *   - Suppression (0.75): Reflects the absolute power of the emperor; there
 *     was no viable alternative or path to oppose its construction or repurposing.
 *   - Theater Ratio (0.80): Represents its final state, particularly under
 *     Commodus, where its function was almost purely theatrical self-aggrandizement,
 *     a key indicator of piton-like degradation.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. For the rulers (institutional), the statue is a tool of
 *   statecraft, a 'Rope' to coordinate civic identity and project power. For
 *   the populace (powerless), it is a 'Snare', a non-consensual extraction
 *   of their resources for the glory of an autocrat. The analytical view of
 *   'Tangled Rope' correctly identifies that it is simultaneously both: a
 *   system with a genuine coordination function that is powered by asymmetric
 *   extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiary (imperial_rulers): They gain immense symbolic capital and
 *     legitimacy by controlling the monument. Their 'arbitrage' exit option
 *     (they can change it, move it, or destroy it) gives them a low directionality
 *     score (d), resulting in a negative effective extraction (χ).
 *   - Victim (roman_populace): They bear the full cost and are the targets of
 *     the propaganda. Their 'trapped' exit status gives them a high directionality
 *     score (d), resulting in a high positive effective extraction (χ).
 *   The engine's automatic derivation from these structural facts correctly models the power asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   This case demonstrates how a single object can be mislabeled. A ruler would
 *   call it a 'Rope' ("a noble landmark for all Romans!"). A dissident would call it
 *   a 'Snare' ("a monument to tyranny!"). The Deferential Realism framework, by
 *   requiring an analytical perspective that acknowledges both the coordination
 *   function (beneficiary exists) and the extraction (victim exists), forces the
 *   more accurate 'Tangled Rope' classification, preventing either partial narrative
 *   from dominating the analysis. The temporal data further shows its decay into a 'Piton'.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_colossus_nero,
    'Was the symbolic coordination value of the Colossus to the Roman state greater than its immense economic and labor cost?',
    'Comparative analysis of economic records of imperial spending vs. historical accounts of civic identity tied to monuments.',
    'If value > cost, its "Rope" component was stronger. If cost > value, its "Snare" component was more dominant, making the Tangled Rope classification even more certain.',
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing. Interval represents its lifecycle from
% construction (~68 AD) to final destruction (~410 AD or later).
narrative_ontology:interval(colossus_nero_inertia, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This constraint is a classic example of lifecycle drift, specifically
% metric substitution (function degrading into theater).
% T=0: Vespasian repurposes it. T=5: Hadrian moves it. T=10: Commodus changes head to Hercules.

% Theater ratio over time (triggers metric_substitution detection):
narrative_ontology:measurement(colossus_tr_t0, colossus_nero_inertia, theater_ratio, 0, 0.20).
narrative_ontology:measurement(colossus_tr_t5, colossus_nero_inertia, theater_ratio, 5, 0.55).
narrative_ontology:measurement(colossus_tr_t10, colossus_nero_inertia, theater_ratio, 10, 0.80).

% Extraction over time (initial cost is highest, then maintenance):
narrative_ontology:measurement(colossus_ex_t0, colossus_nero_inertia, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(colossus_ex_t5, colossus_nero_inertia, base_extractiveness, 5, 0.50).
narrative_ontology:measurement(colossus_ex_t10, colossus_nero_inertia, base_extractiveness, 10, 0.48).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type: The statue allocated prime public space and symbolic
% capital, making it a form of resource allocation.
narrative_ontology:coordination_type(colossus_nero_inertia, resource_allocation).

% Network relationship: The statue's proximity to the Flavian Amphitheatre
% is the most cited reason for the amphitheatre's later nickname, the Colosseum.
% The monument's existence structurally influenced a linguistic convention.
narrative_ontology:affects_constraint(colossus_nero_inertia, colosseum_naming_convention).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */