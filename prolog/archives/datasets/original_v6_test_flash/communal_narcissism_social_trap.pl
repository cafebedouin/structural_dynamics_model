% ============================================================================
% CONSTRAINT STORY: communal_narcissism_social_trap
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_communal_narcissism_social_trap, []).

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
 *   constraint_id: communal_narcissism_social_trap
 *   human_readable: The Altruistic Extraction Snare
 *   domain: social/psychological
 *
 * SUMMARY:
 *   A social structure where a central "benefactor" uses performative
 *   prosociality (communal narcissism) to extract emotional and material
 *   labor from a group. This takes the form of public displays of generosity,
 *   advocacy, or devotion to a cause, which are then leveraged to create
 *   obligation, guilt, or dependence in others. The communal narcissist
 *   positions themselves as indispensable, creating a social trap where group
 *   members feel compelled to provide support, resources, and validation,
 *   even at their own expense. Over time, this dynamic can become highly
 *   extractive, with the narcissist benefiting from the group's collective
 *   efforts while contributing little of genuine substance.
 *
 * KEY AGENTS:
 *   - communal_narcissist: The central figure who benefits from the altruistic performance (institutional/arbitrage)
 *   - group_members: Individuals who are targeted by the narcissist's behavior and provide the resources (powerless/trapped)
 *   - outside_observer: An outsider who can assess the dynamics with greater objectivity (moderate/constrained)
 *   - psychology_community: The psychological scientific community that has seen through it all and see the communal narcissist's acts as manipulative piton (organized/mobile)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(communal_narcissism_social_trap, 0.75).
domain_priors:suppression_score(communal_narcissism_social_trap, 0.8).
domain_priors:theater_ratio(communal_narcissism_social_trap, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(communal_narcissism_social_trap, extractiveness, 0.75).
narrative_ontology:constraint_metric(communal_narcissism_social_trap, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(communal_narcissism_social_trap, theater_ratio, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(communal_narcissism_social_trap, snare).
narrative_ontology:human_readable(communal_narcissism_social_trap, "The Altruistic Extraction Snare").
narrative_ontology:topic_domain(communal_narcissism_social_trap, "social/psychological").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(communal_narcissism_social_trap, communal_narcissist).
narrative_ontology:constraint_victim(communal_narcissism_social_trap, group_members).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Individual group member feeling trapped and exploited.
constraint_indexing:constraint_classification(communal_narcissism_social_trap, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% The central figure benefiting from the altruistic performance.
constraint_indexing:constraint_classification(communal_narcissism_social_trap, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(local))).

% An outside observer sees the mixed coordination and extraction.
constraint_indexing:constraint_classification(communal_narcissism_social_trap, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% Psychological studies that have studied the community narcissism sees the communal narcissist's acts as manipulative piton.
constraint_indexing:constraint_classification(communal_narcissism_social_trap, piton,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(communal_narcissism_social_trap_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(communal_narcissism_social_trap, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(communal_narcissism_social_trap, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(communal_narcissism_social_trap, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(communal_narcissism_social_trap, TR),
    TR >= 0.70.

:- end_tests(communal_narcissism_social_trap_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.75): High. The narcissist extracts emotional labor, resources, and social capital from the group. Suppression (0.80): High. Group members often feel pressured to comply due to social expectations, guilt, or fear of being ostracized. Theater ratio (0.60): Moderate. While there may be some genuine altruistic intent initially, the performance becomes increasingly performative over time as the extraction increases. The 'altruism' is primarily for show.
 *
 * PERSPECTIVAL GAP:
 *   The narcissist views the situation as a mutually beneficial arrangement (Rope), where their leadership and vision are supported by the group's efforts. Group members, however, experience a sense of being trapped and exploited (Snare), as their needs are consistently subordinated to the narcissist's agenda. An outside observer can see the mixed coordination and extraction (Tangled Rope), recognizing the genuine benefits of the group's activities while also acknowledging the asymmetric distribution of costs and benefits. The psychology community sees the communal narcissist's acts as manipulative piton.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by the flow of resources and obligations. The narcissist receives resources and validation, while group members provide them. The exit options are also asymmetric, with the narcissist wielding more control and freedom than the individual members. The analytical observer can see the system's flaws and the group members, but may not possess the means to correct it.
 *
 * MANDATROPHY ANALYSIS:
 *   This resolves the mandatrophy by distinguishing between genuine altruism and performative altruism used as a tool for extraction. Genuine altruism would be classified as a Rope, where benefits are mutually distributed. The communal narcissism social trap, however, becomes a Snare when the altruism becomes a performance designed to create obligation and extract resources from others. In some cases it could be a Tangled Rope depending on perspective.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    level_of_awareness,
    'To what extent are the group members aware of the narcissist''s manipulative behavior?',
    'Surveys and interviews with group members to assess their perception of the leader''s motives.',
    'If members are aware, the constraint shifts towards a tangled rope. If unaware, it remains a snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(level_of_awareness, empirical, 'Determines the level of exploitation versus perceived benefit within the group.').

omega_variable(
    long_term_impact,
    'What is the long-term impact on the group members'' psychological well-being?',
    'Longitudinal studies tracking the mental health of group members over time.',
    'High negative impact strengthens the snare classification. Minimal impact could indicate a different dynamic.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(long_term_impact, empirical, 'Assesses the severity of the trap''s consequences.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(communal_narcissism_social_trap, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comm_tr_t0, communal_narcissism_social_trap, theater_ratio, 0, 0.3).
narrative_ontology:measurement(comm_tr_t5, communal_narcissism_social_trap, theater_ratio, 5, 0.5).
narrative_ontology:measurement(comm_tr_t10, communal_narcissism_social_trap, theater_ratio, 10, 0.6).

% Extraction over time
narrative_ontology:measurement(comm_be_t0, communal_narcissism_social_trap, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(comm_be_t5, communal_narcissism_social_trap, base_extractiveness, 5, 0.65).
narrative_ontology:measurement(comm_be_t10, communal_narcissism_social_trap, base_extractiveness, 10, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(communal_narcissism_social_trap, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
