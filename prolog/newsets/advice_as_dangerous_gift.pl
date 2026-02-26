% ============================================================================
% CONSTRAINT STORY: advice_as_dangerous_gift
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_advice_as_dangerous_gift, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: advice_as_dangerous_gift
 *   human_readable: The Hazard of Counsel
 *   domain: social/philosophical
 *
 * SUMMARY:
 *   Drawing from J.R.R. Tolkien's insight that 'advice is a dangerous gift,
 *   even from the wise to the wise,' this constraint models the structural
 *   tension inherent in giving counsel. While often intended as a
 *   coordination good (a Rope), advice carries inherent risks of extraction
 *   (a Snare). It can undermine the recipient's autonomy, create dependency,
 *   transfer the consequences of failure, and serve the giver's ego more than
 *   the recipient's need. The constraint's structure is a classic Tangled
 *   Rope, where the helpful function is inseparable from its potential for
 *   harm.
 *
 * KEY AGENTS:
 *   - Advice Giver: Primary beneficiary (institutional/arbitrage) — benefits from ego gratification, status reinforcement, and the feeling of being helpful.
 *   - Advice Recipient: Primary victim (powerless/trapped) — bears the risk of bad outcomes and diminished autonomy; often in a vulnerable position.
 *   - Interpersonal Relationship: Secondary victim (powerless/trapped) — the social bond itself is damaged by bad or unsolicited advice, leading to resentment and distance.
 *   - Professional Advisor: Organized agent (organized/mobile) — attempts to reframe the constraint as a temporary Scaffold with a sunset clause.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(advice_as_dangerous_gift, 0.55).
domain_priors:suppression_score(advice_as_dangerous_gift, 0.65).
domain_priors:theater_ratio(advice_as_dangerous_gift, 0.75).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(advice_as_dangerous_gift, extractiveness, 0.55).
narrative_ontology:constraint_metric(advice_as_dangerous_gift, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(advice_as_dangerous_gift, theater_ratio, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(advice_as_dangerous_gift, tangled_rope).
narrative_ontology:human_readable(advice_as_dangerous_gift, "The Hazard of Counsel").
narrative_ontology:topic_domain(advice_as_dangerous_gift, "social/philosophical").

domain_priors:requires_active_enforcement(advice_as_dangerous_gift).
narrative_ontology:has_sunset_clause(advice_as_dangerous_gift).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(advice_as_dangerous_gift, advice_giver).
narrative_ontology:constraint_victim(advice_as_dangerous_gift, advice_recipient).
narrative_ontology:constraint_victim(advice_as_dangerous_gift, interpersonal_relationship).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE RECIPIENT (SNARE) — Trapped by a pressing problem and social pressure, the recipient experiences advice as a coercive force. Rejecting it risks social sanction, while accepting it transfers risk and undermines autonomy. The 'gift' becomes a trap. d≈0.95, f(d)≈1.42, σ=0.9 → χ≈0.70.
constraint_indexing:constraint_classification(advice_as_dangerous_gift, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: THE GIVER (ROPE) — From the giver's perspective, advice is a pure coordination good, a gift of wisdom to solve a problem. They benefit from feeling helpful and wise, and can give or withhold counsel at will, experiencing no extraction. d≈0.05, f(d)≈-0.12, σ=0.8 → χ≈-0.05.
constraint_indexing:constraint_classification(advice_as_dangerous_gift, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(local))).

% PERSPECTIVE 3: ANALYTICAL (TANGLED ROPE) — The observer sees the dual function: a genuine coordination attempt (sharing knowledge) tangled with asymmetric extraction (risk transfer, ego gratification for the giver, dependency for the receiver). The active enforcement is social pressure. d≈0.72, f(d)≈1.15, σ=1.2 → χ≈0.76.
constraint_indexing:constraint_classification(advice_as_dangerous_gift, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: THE REGRETFUL GIVER (PITON) — After advice has been given and led to a bad outcome, the act is seen as a degraded ritual. The function (to help) has failed, but the social form persists due to inertia. The high theater_ratio (0.75) reflects the performative aspect of 'giving wisdom' over its actual utility. The constraint is now a piton, an inertial remnant of a failed coordination attempt.
constraint_indexing:constraint_classification(advice_as_dangerous_gift, piton,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 5: THE PROFESSIONAL (SCAFFOLD) — In structured contexts like therapy or coaching, advice is framed as a temporary support system designed to build the recipient's own capacity. The relationship has an explicit or implicit sunset clause: the goal is for the recipient to no longer need the advice. This professional framing attempts to manage the 'danger' by making the support temporary.
constraint_indexing:constraint_classification(advice_as_dangerous_gift, scaffold,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(advice_as_dangerous_gift_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(advice_as_dangerous_gift, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(advice_as_dangerous_gift, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(advice_as_dangerous_gift, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(advice_as_dangerous_gift, TR),
    TR >= 0.70.

:- end_tests(advice_as_dangerous_gift_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55): High. The extraction is not financial but psychological and relational. It represents the transfer of risk to the recipient, the creation of dependency, and the potential for the giver's ego to be served at the recipient's expense. Suppression (0.65): High. Social norms, power dynamics (e.g., parent-child, manager-employee), and the recipient's own vulnerability create significant pressure to accept advice, suppressing the alternative of independent problem-solving. Theater Ratio (0.75): High. The act of giving advice is often highly performative, a ritual of demonstrating wisdom or concern. The functional value of the advice can be secondary to the performance itself, leading to the Piton perspective when the advice fails.
 *
 * PERSPECTIVAL GAP:
 *   The gap is profound. The giver genuinely sees a Rope, a pure gift of help. The recipient, feeling cornered and at risk, experiences a Snare. The professional therapist or coach attempts to construct a Scaffold, a temporary support structure. An observer reflecting on failed advice sees a Piton, a hollow ritual. The analytical observer recognizes the inseparable nature of help and harm, classifying it as a Tangled Rope. The 'correct' classification is the full set of perspectives, as the nature of the constraint is defined by one's structural position relative to it.
 *
 * DIRECTIONALITY LOGIC:
 *   The Giver (beneficiary + arbitrage) has a low 'd' value, resulting in negative effective extraction (a net benefit), hence the Rope classification. The Recipient (victim + trapped) has a high 'd' value, maximizing effective extraction and triggering the Snare classification. The Analytical observer's canonical 'd' value places the effective extraction squarely in the Tangled Rope range. The different classifications are a direct result of the agents' differing structural relationships to the flow of risk and autonomy.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy of labeling 'advice' as either purely good (coordination) or purely bad (extraction). The DR framework shows it is structurally both simultaneously. A naive analysis might call all advice a Rope, ignoring the harm to the recipient. A cynical analysis might call all advice a Snare, ignoring the genuine coordination function. The Tangled Rope classification, derived from the analytical perspective, correctly identifies the hybrid nature and explains why different agents can have such divergent, yet valid, experiences of the same phenomenon.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    intent_vs_outcome,
    'Is the extractive ''danger'' of advice rooted in the giver''s (potentially subconscious) intent, or is it an emergent structural property of the act itself, regardless of intent?',
    'Psychological studies correlating giver''s intent (altruistic vs. ego-driven) with recipient''s reported outcomes (autonomy, dependency, success).',
    'If intent-driven, the constraint is a Snare from more perspectives. If structural, the Tangled Rope classification holds as the default analytical view.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intent_vs_outcome, conceptual, 'Whether the harm from advice is driven by giver''s intent or the structure of the act.').

omega_variable(
    autonomy_threshold,
    'At what point does helpful guidance cross the line into creating harmful dependency, thereby increasing the constraint''s extractiveness?',
    'Longitudinal studies of mentorship and advisory relationships, measuring recipient autonomy over time against frequency and type of advice given.',
    'A clear threshold would allow for better calibration of the ''Scaffold'' perspective and provide a clear failure condition where it degrades into a Snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(autonomy_threshold, empirical, 'The threshold where helpful guidance becomes harmful dependency.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(advice_as_dangerous_gift, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(advi_tr_t0, advice_as_dangerous_gift, theater_ratio, 0, 0.3).
narrative_ontology:measurement(advi_tr_t10, advice_as_dangerous_gift, theater_ratio, 10, 0.55).
narrative_ontology:measurement(advi_tr_t20, advice_as_dangerous_gift, theater_ratio, 20, 0.75).

% Extraction over time
narrative_ontology:measurement(advi_be_t0, advice_as_dangerous_gift, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(advi_be_t10, advice_as_dangerous_gift, base_extractiveness, 10, 0.4).
narrative_ontology:measurement(advi_be_t20, advice_as_dangerous_gift, base_extractiveness, 20, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
