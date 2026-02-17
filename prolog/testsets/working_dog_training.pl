% ============================================================================
% CONSTRAINT STORY: working_dog_training
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-10
% ============================================================================

:- module(constraint_working_dog_training, []).

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
 *   constraint_id: working_dog_training
 *   human_readable: Training of Working Dogs for Specific Tasks
 *   domain: social
 *
 * SUMMARY:
 *   This constraint describes the training regime imposed on working dogs,
 *   such as those used for bomb detection or search and rescue. It balances
 *   the needs of the handler/organization with the well-being of the dog,
 *   in order to achieve desired functional outcomes. The effectiveness of this
 *   training, and the ethical considerations surrounding it, depend on the
 *   specific methods employed and the overall care provided.
 *
 * KEY AGENTS (by structural relationship):
 *   - Working Dogs: Primary target (powerless/trapped) — subject to training
 *   - Dog Handlers/Organizations: Primary beneficiary (institutional/arbitrage) — benefit from dog's capabilities
 *   - General Public: Secondary beneficiary (moderate/mobile) - Indirectly benefits from safety and security
 *   - Analytical observer — sees full structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(working_dog_training, 0.35).
domain_priors:suppression_score(working_dog_training, 0.42).   % Structural property (raw, unscaled). Moderate suppression reflects the structured environment and lack of alternatives for the dog.
domain_priors:theater_ratio(working_dog_training, 0.15).       % Low theater; training is for functional outcomes.

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(working_dog_training, extractiveness, 0.35).
narrative_ontology:constraint_metric(working_dog_training, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(working_dog_training, theater_ratio, 0.15).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(working_dog_training, tangled_rope).
narrative_ontology:human_readable(working_dog_training, "Training of Working Dogs for Specific Tasks").

% --- Binary flags ---
domain_priors:requires_active_enforcement(working_dog_training). % Training is a form of active enforcement of behaviors.

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.
% Without these, the engine falls back to generic power-atom assumptions.
%
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(working_dog_training, dog_handlers).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(working_dog_training, working_dogs).
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

% PERSPECTIVE 1: THE PRIMARY TARGET (THE DOG)
% Agent who bears the most extraction. Engine derives d from:
%   victim membership + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42 → high χ
% The training is a coercive structure with a coordination benefit (the task).
constraint_indexing:constraint_classification(working_dog_training, tangled_rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (THE HANDLER)
% Agent who benefits most. Engine derives d from:
%   beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 → low/negative χ
% From this view, it's a pure coordination mechanism to achieve a goal.
constraint_indexing:constraint_classification(working_dog_training, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% Default analytical context. Recognizes both the coordination function
% (beneficiary) and the coercive extraction (victim + enforcement), classifying
% it as a Tangled Rope.
constraint_indexing:constraint_classification(working_dog_training, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(working_dog_training_tests).

test(perspectival_gap) :-
    % Verify perspectival gap between target and beneficiary.
    constraint_indexing:constraint_classification(working_dog_training, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(working_dog_training, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget \= TypeBeneficiary.

test(threshold_validation_for_tangled_rope) :-
    % Verify metrics are consistent with a Tangled Rope classification.
    narrative_ontology:constraint_metric(working_dog_training, extractiveness, E),
    narrative_ontology:constraint_metric(working_dog_training, suppression_requirement, S),
    E >= 0.30,
    S >= 0.40.

:- end_tests(working_dog_training_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness is moderate (0.35) as dogs are expected to
 *   perform tasks that require effort and potentially risk, but ethical training
 *   provides rewards and enrichment. The suppression score is raised to 0.42
 *   to reflect that the dog has no real alternative to the training regime;
 *   it is a coercive structure, even if benevolently applied. This, combined
 *   with the clear beneficiary (handler) and victim (dog), and the need for
 *   active enforcement (the training itself), meets the criteria for a
 *   Tangled Rope. Theater ratio is low (0.15) because the focus is
 *   primarily on functional outcomes.
 *
 * PERSPECTIVAL GAP:
 *   The working dog experiences the training as a coercive, high-extraction
 *   constraint (Tangled Rope), while the handler views it as a necessary and
 *   mutually beneficial arrangement for coordination (Rope). The analytical
 *   observer sides with the dog's perspective, recognizing that the combination
 *   of coordination and coercion is the definition of a Tangled Rope.
 *
 * DIRECTIONALITY LOGIC:
 *   Dog handlers benefit from the dog's ability to perform specific tasks,
 *   enhancing their own capabilities (e.g., finding bombs, rescuing people).
 *   Working dogs bear the costs of training, which involves physical
 *   and mental exertion and limitations on their freedom. The
 *   directionality reflects this asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this as a Tangled Rope correctly captures the dual nature of
 *   the activity: it has a genuine coordination function (achieving a task)
 *   but also involves asymmetric extraction and coercion. This prevents both
 *   the mislabeling of abusive training as pure coordination (Rope) and the
 *   dismissal of the functional benefits by labeling it a pure Snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_dog_ethics,
    'To what degree can positive reinforcement fully negate the extractive/coercive aspect of training?',
    'Longitudinal studies on dog health, stress biomarkers, and voluntary participation rates across different training paradigms.',
    'If reinforcement negates coercion, ε could be lowered, shifting classification to Rope. If not, it remains a Tangled Rope.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(working_dog_training, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data is not strictly required as base_extractiveness < 0.46,
% but is included to model the formalization of training methods over time.
%
% Theater ratio over time (triggers metric_substitution detection):
narrative_ontology:measurement(working_dog_training_tr_t0, working_dog_training, theater_ratio, 0, 0.05).
narrative_ontology:measurement(working_dog_training_tr_t5, working_dog_training, theater_ratio, 5, 0.10).
narrative_ontology:measurement(working_dog_training_tr_t10, working_dog_training, theater_ratio, 10, 0.15).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(working_dog_training_ex_t0, working_dog_training, base_extractiveness, 0, 0.30).
narrative_ontology:measurement(working_dog_training_ex_t5, working_dog_training, base_extractiveness, 5, 0.33).
narrative_ontology:measurement(working_dog_training_ex_t10, working_dog_training, base_extractiveness, 10, 0.35).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% Training is a mechanism to enforce a specific set of desired behaviors.
narrative_ontology:coordination_type(working_dog_training, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides needed; the structural derivation from beneficiary/victim
% and exit options accurately models the directionality.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */