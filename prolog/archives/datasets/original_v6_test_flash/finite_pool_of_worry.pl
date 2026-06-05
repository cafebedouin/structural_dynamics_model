% ============================================================================
% CONSTRAINT STORY: finite_pool_of_worry
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_finite_pool_of_worry, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: finite_pool_of_worry
 *   human_readable: The Finite Pool of Worry Hypothesis
 *   domain: psychological/social
 *
 * SUMMARY:
 *   The finite pool of worry describes a psychological limit where
 *   individuals and societies can only effectively focus on a limited number
 *   of issues simultaneously. This constraint results in certain issues being
 *   neglected while attention and resources are directed toward others,
 *   creating a dynamic where focusing on one problem inherently detracts from
 *   others. Governments and media organizations can exploit this limitation
 *   to control public discourse, while the general public and unattended
 *   issues bear the cost.
 *
 * KEY AGENTS:
 *   - General Public: Primary target (powerless/trapped) - limited cognitive capacity
 *   - Governments: Primary beneficiary (institutional/arbitrage) - control discourse
 *   - News Media: Secondary actor (moderate/constrained) - shapes public agenda
 *   - Neglected Issues: Victim (powerless/trapped) - important topics without attention
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(finite_pool_of_worry, 0.55).
domain_priors:suppression_score(finite_pool_of_worry, 0.4).
domain_priors:theater_ratio(finite_pool_of_worry, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(finite_pool_of_worry, extractiveness, 0.55).
narrative_ontology:constraint_metric(finite_pool_of_worry, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(finite_pool_of_worry, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(finite_pool_of_worry, tangled_rope).
narrative_ontology:human_readable(finite_pool_of_worry, "The Finite Pool of Worry Hypothesis").
narrative_ontology:topic_domain(finite_pool_of_worry, "psychological/social").

domain_priors:requires_active_enforcement(finite_pool_of_worry).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(finite_pool_of_worry, governments).
narrative_ontology:constraint_beneficiary(finite_pool_of_worry, news_media).
narrative_ontology:constraint_victim(finite_pool_of_worry, general_public).
narrative_ontology:constraint_victim(finite_pool_of_worry, neglected_issues).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% The general public is often trapped within the finite pool of worry, unable to focus on multiple issues simultaneously due to cognitive limitations and emotional overwhelm. They are the primary targets of this constraint, bearing the cost of neglected issues.
constraint_indexing:constraint_classification(finite_pool_of_worry, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(national))).

% Governments benefit from the finite pool of worry as they can strategically manage public attention by focusing on specific issues, diverting attention from others. They have arbitrage options by shifting focus as needed.
constraint_indexing:constraint_classification(finite_pool_of_worry, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% News media is both a beneficiary and a victim. They benefit from focusing on specific events and creating narratives that capture public attention (increased viewership, clicks, etc.). However, they are constrained by the need to simplify complex issues, and they also bear the responsibility for potential neglect of other important matters.
constraint_indexing:constraint_classification(finite_pool_of_worry, tangled_rope,
    context(agent_power(moderate),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(national))).

% Important but unaddressed topics are trapped outside public discourse. They bear the full cost of inattention. They lack power and cannot escape the finite pool of worry.
constraint_indexing:constraint_classification(finite_pool_of_worry, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% From an analytical perspective, the finite pool of worry is a tangled rope. It represents a psychological limit that both structures and distorts public attention, influencing resource allocation and policy decisions. This perspective sees both the coordination (focus on pressing issues) and the extraction (neglect of other important concerns).
constraint_indexing:constraint_classification(finite_pool_of_worry, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(finite_pool_of_worry_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(finite_pool_of_worry, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(finite_pool_of_worry, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(finite_pool_of_worry, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(finite_pool_of_worry_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness: 0.55 - Significant extraction as important issues are neglected due to the focus on a select few. Suppression: 0.40 - Moderate suppression as the public's ability to address multiple issues is limited, and certain viewpoints may be marginalized.
 *
 * PERSPECTIVAL GAP:
 *   The General Public experiences this as a Snare because their limited cognitive capacity prevents them from addressing many issues at once. Governments, as beneficiaries, may see this as a Rope, allowing them to efficiently manage public attention. The News Media occupies a Tangled Rope position, benefiting from focusing on specific issues but constrained by the need to simplify.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality is determined by the agent's structural position. The General Public, lacking power and being trapped, bears the most extraction. Governments, with their ability to arbitrage and shift focus, benefit. News Media has a mixed position, reflecting both benefits and constraints. Neglected issues cannot escape or organize and therefore endure extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by demonstrating that different perspectives yield different classifications. What appears to be a necessary focus from the government's view (Rope) results in certain issues being actively neglected, which functions as a Snare for the public. Analytical Observer captures the complexity of the extraction in the Tangled Rope perspective.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cognitive_capacity_limit,
    'To what extent is the finite pool of worry due to inherent limitations in human cognitive capacity versus socially constructed attention mechanisms?',
    'Cognitive psychology experiments examining attention allocation under stress vs. sociological analysis of media agenda-setting and framing effects.',
    'If cognitive: constraint is more mountain-like and difficult to overcome. If social: constraint is more snare-like and susceptible to manipulation or mitigation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cognitive_capacity_limit, empirical, 'Distinguishing cognitive capacity from social construction').

omega_variable(
    issue_salience_measurement,
    'How can the salience of different issues be accurately measured to determine the degree of neglect within the finite pool of worry?',
    'Public opinion surveys, media content analysis, and behavioral data from online search and social media engagement.',
    'Improved measurement could reveal which issues are most severely neglected and inform strategies for increasing their visibility.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(issue_salience_measurement, empirical, 'Accurate measurement of issue salience').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(finite_pool_of_worry, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fini_tr_t0, finite_pool_of_worry, theater_ratio, 0, 0.1).
narrative_ontology:measurement(fini_tr_t5, finite_pool_of_worry, theater_ratio, 5, 0.2).
narrative_ontology:measurement(fini_tr_t10, finite_pool_of_worry, theater_ratio, 10, 0.3).

% Extraction over time
narrative_ontology:measurement(fini_be_t0, finite_pool_of_worry, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(fini_be_t5, finite_pool_of_worry, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(fini_be_t10, finite_pool_of_worry, base_extractiveness, 10, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(finite_pool_of_worry, information_standard).
narrative_ontology:affects_constraint(finite_pool_of_worry, availability_heuristic).
narrative_ontology:affects_constraint(finite_pool_of_worry, confirmation_bias).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
