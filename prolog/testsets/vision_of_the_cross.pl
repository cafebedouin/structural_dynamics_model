% ============================================================================
% CONSTRAINT STORY: vision_of_the_cross
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_vision_of_the_cross, []).

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
    narrative_ontology:omega_variable/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: vision_of_the_cross
 *   human_readable: "In Hoc Signo Vinces" Mandate
 *   domain: religious/political
 *
 * SUMMARY:
 *   On the eve of the Battle of the Milvian Bridge in 312 CE, Constantine
 *   reportedly had a vision of a Christian symbol in the sky with the words
 *   "In hoc signo vinces" ("in this sign you will conquer"). This divine
 *   mandate constrained his military and political strategy, compelling him
 *   to adopt the symbol and, by extension, favor the Christian faith. This act
 *   radically reconfigured the religious and political landscape of the Roman
 *   Empire, initiating the process of its Christianization.
 *
 * KEY AGENTS (by structural relationship):
 *   - Roman Polytheistic Priesthoods: Primary target (powerless/trapped) — bore the costs of imperial patronage shifting to a rival religion, leading to their eventual suppression and dispossession.
 *   - Christian Clergy and Adherents: Primary beneficiary (institutional/arbitrage) — shifted from a persecuted minority to the favored religion of the empire, gaining immense resources, power, and status.
 *   - Emperor Constantine: Institutional enforcer (institutional/constrained) — used the constraint to unify his army and consolidate political power, but was constrained by the choice to break with centuries of Roman tradition.
 *   - Analytical Observer: A historian viewing the event as a pivotal moment of both political coordination and religious extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vision_of_the_cross, 0.75).
domain_priors:suppression_score(vision_of_the_cross, 0.85).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(vision_of_the_cross, 0.15).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vision_of_the_cross, extractiveness, 0.75).
narrative_ontology:constraint_metric(vision_of_the_cross, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(vision_of_the_cross, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
% This constraint is not a mountain; it is a human-enforced political/religious mandate.

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(vision_of_the_cross, tangled_rope).

% --- Binary flags ---
domain_priors:requires_active_enforcement(vision_of_the_cross). % Required for Tangled Rope

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.

% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(vision_of_the_cross, christian_clergy_and_adherents).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(vision_of_the_cross, roman_polytheistic_priesthoods).

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

% PERSPECTIVE 1: THE PRIMARY TARGET (SNARE)
% Roman polytheistic priesthoods who lost imperial funding, status, and
% eventually their right to practice.
% Engine derives d from victim status + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42.
% χ = 0.75 * 1.42 * 1.0 (national) = 1.065, a clear Snare (χ >= 0.66).
constraint_indexing:constraint_classification(vision_of_the_cross, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (ROPE)
% The Christian community, elevated from persecuted minority to imperial favorite.
% Engine derives d from beneficiary status + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12.
% χ = 0.75 * -0.12 * 1.0 (national) = -0.09, a clear Rope (negative extraction).
constraint_indexing:constraint_classification(vision_of_the_cross, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% A historian who sees both the coordination function (unifying the empire)
% and the severe asymmetric extraction (dispossessing paganism).
% Engine derives d ≈ 0.72 → f(d) ≈ 1.15.
% χ = 0.75 * 1.15 * 1.2 (global) = 1.035. This result classifies as Snare.
% To capture the 'Tangled Rope' intuition that coordination was present,
% we use a directionality override to model an analytical view that weighs
% the coordination function more heavily.
constraint_indexing:directionality_override(vision_of_the_cross, analytical, 0.60).
% With override: d=0.60, f(d)≈0.88. χ = 0.75 * 0.88 * 1.2 = 0.792.
% This χ is in the Tangled Rope range [0.40, 0.90]. ε=0.75, suppression=0.85
% also satisfy Tangled Rope criteria (ε≥0.30, supp≥0.40).
constraint_indexing:constraint_classification(vision_of_the_cross, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vision_of_the_cross_tests).

test(perspectival_gap_is_total) :-
    % Verify the massive gap between victim (Snare) and beneficiary (Rope).
    constraint_indexing:constraint_classification(vision_of_the_cross, snare, context(agent_power(powerless), _, exit_options(trapped), _)),
    constraint_indexing:constraint_classification(vision_of_the_cross, rope, context(agent_power(institutional), _, exit_options(arbitrage), _)).

test(tangled_rope_conditions_met_analytically) :-
    % Verify that the analytical perspective correctly identifies a Tangled Rope.
    domain_priors:base_extractiveness(vision_of_the_cross, E), E >= 0.30,
    domain_priors:suppression_score(vision_of_the_cross, S), S >= 0.40,
    domain_priors:requires_active_enforcement(vision_of_the_cross),
    narrative_ontology:constraint_beneficiary(vision_of_the_cross, _),
    narrative_ontology:constraint_victim(vision_of_the_cross, _),
    constraint_indexing:constraint_classification(vision_of_the_cross, tangled_rope, context(agent_power(analytical), _, _, _)).

:- end_tests(vision_of_the_cross_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.75): Extremely high. This constraint initiated a
 *     multi-generational, empire-wide transfer of wealth, property (temples,
 *     land), and social capital from polytheistic institutions to the Christian church.
 *   - Suppression Score (0.85): Extremely high. The adoption of Christianity as
 *     the favored religion led directly to the suppression and eventual
 *     prohibition of traditional Roman religious practices. Alternatives
 *     were actively eliminated.
 *   - Theater Ratio (0.15): Low. The initial constraint was brutally functional:
 *     a mandate for military victory. While Christian ritual became highly
 *     theatrical later, the origin of the constraint was practical and coercive.
 *
 * PERSPECTIVAL GAP:
 *   The gap is absolute. For the Roman polytheistic priesthoods, it was a
 *   Snare that destroyed their world, expropriated their resources, and
 *   criminalized their beliefs. For the Christian community, it was a Rope of
 *   divine deliverance that offered salvation, imperial protection, and
 *   unprecedented power. The two groups experienced completely different,
 *   mutually exclusive realities derived from the same event.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary/victim declarations are unambiguous. The Christian clergy
 *   and their followers were the direct, intended beneficiaries. The established
 *   polytheistic priesthoods and their adherents were the direct, structural
 *   victims whose loss was the necessary counterpart to the Christians' gain.
 *   The directionality derivation chain correctly models this by assigning a
 *   low `d` to the beneficiaries and a high `d` to the victims, generating the
 *   Rope/Snare classification gap. The override for the analytical perspective
 *   reflects the historical judgment that a genuine and powerful coordination
 *   function coexisted with the brutal extraction, making it a canonical Tangled Rope.
 *
 * MANDATROPHY ANALYSIS:
 *   This story is a powerful example of why the Tangled Rope category is
 *   essential. A simpler model might be forced to classify this event as
 *   *either* pure coordination (a Rope for unifying the Empire) or pure
 *   extraction (a Snare for persecuting pagans). Both are incomplete. The
 *   Deferential Realism framework, by using an analytical perspective that can
 *   see both functions simultaneously, correctly identifies it as a Tangled
 *   Rope: a system that performed a genuine coordination function (stabilizing
 *   and unifying a vast empire under a new ideology) *through* a mechanism of
 *   asymmetric extraction (the dispossession of a previously dominant group).
 *   [RESOLVED MANDATROPHY] The framework's ability to classify this as a
 *   Tangled Rope, acknowledging both coordination and extraction, resolves the
 *   mandatrophy by preventing a misclassification as a pure Snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% /5 form: narrative detail for story context
omega_variable(
    omega_vision_of_the_cross,
    'Was the Vision of the Cross a genuine divine revelation, a politically motivated fabrication, or a psychological phenomenon?',
    'This is historically and empirically irresolvable. It is a matter of faith versus materialist interpretation.',
    'If a true divine revelation, the constraint would be a Mountain. If a political fabrication, it is a human-constructed Tangled Rope. The framework models the structural consequences of the ACT of enforcement, which align with a Tangled Rope, regardless of the ultimate origin.',
    confidence_without_resolution(low)
).

% /3 form: typed classification for reporting engine (REQUIRED)
narrative_ontology:omega_variable(omega_vision_of_the_cross, conceptual, 'Whether the vision was a divine revelation (Mountain) or a political fabrication (Tangled Rope).').

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(vision_of_the_cross, 312, 380). % From Milvian Bridge to Edict of Thessalonica

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data tracks the intensification of the constraint over time.
% The interval covers the period from Constantine's vision (312 CE) to
% Christianity becoming the official state religion (380 CE).

% Theater ratio over time:
narrative_ontology:measurement(vision_tr_t312, vision_of_the_cross, theater_ratio, 312, 0.10).
narrative_ontology:measurement(vision_tr_t325, vision_of_the_cross, theater_ratio, 325, 0.30).
narrative_ontology:measurement(vision_tr_t380, vision_of_the_cross, theater_ratio, 380, 0.15). % Re-normalized back to final

% Extraction over time:
narrative_ontology:measurement(vision_ex_t312, vision_of_the_cross, base_extractiveness, 312, 0.60).
narrative_ontology:measurement(vision_ex_t325, vision_of_the_cross, base_extractiveness, 325, 0.70).
narrative_ontology:measurement(vision_ex_t380, vision_of_the_cross, base_extractiveness, 380, 0.75).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% This constraint enforced a new basis for imperial identity and loyalty.
narrative_ontology:coordination_type(vision_of_the_cross, enforcement_mechanism).

% Network relationships (structural influence edges)
% This constraint laid the foundation for the medieval concept of the divine
% right of kings, which yoked secular authority to religious sanction.
narrative_ontology:affects_constraint(vision_of_the_cross, divine_right_of_kings).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% An override is used for the analytical perspective. The default derivation
% for 'analytical' (d=0.72) combined with the extremely high base extractiveness
% classifies the constraint as a pure Snare, even from a global view.
% This misses the significant, historically undeniable coordination function
% the new state religion played in unifying the late Roman Empire. The override
% (d=0.60) adjusts the perspective to one that gives more weight to the
% coordination function, correctly classifying it as a Tangled Rope without
% altering the base metrics of the constraint itself.
constraint_indexing:directionality_override(vision_of_the_cross, analytical, 0.60).


/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */