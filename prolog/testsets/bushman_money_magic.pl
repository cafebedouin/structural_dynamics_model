% ============================================================================
% CONSTRAINT STORY: bushman_money_magic
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2023-10-27
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_bushman_money_magic, []).

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
 *   constraint_id: bushman_money_magic
 *   human_readable: The Trickster's Asymmetric Scam
 *   domain: economic/social
 *
 * SUMMARY:
 *   This constraint models a classic asymmetric scam, often found in
 *   folklore, where a trickster figure uses a 'money magic' narrative to
 *   induce a victim to hand over their wealth (e.g., cattle), promising it
 *   will be magically multiplied. The trickster then absconds with the
 *   original stake. The core mechanism is the temporary creation of a
 *   narrative reality that suppresses the victim's rational assessment of
 *   risk and alternatives.
 *
 * KEY AGENTS:
 *   - Trickster Figure: Primary beneficiary (powerful/arbitrage) — controls the narrative and extracts all value.
 *   - Deceived Ranchers: Primary victim (powerless/trapped) — loses their wealth by buying into the false narrative.
 *   - Skeptical Community: Observers (moderate/mobile) — witness the performative aspect of the scam without being trapped by it.
 *   - Analytical Observer: Analyst (analytical/analytical) — deconstructs the scam into its structural components.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bushman_money_magic, 0.85).
domain_priors:suppression_score(bushman_money_magic, 0.75).
domain_priors:theater_ratio(bushman_money_magic, 0.9).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bushman_money_magic, extractiveness, 0.85).
narrative_ontology:constraint_metric(bushman_money_magic, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(bushman_money_magic, theater_ratio, 0.9).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bushman_money_magic, snare).
narrative_ontology:human_readable(bushman_money_magic, "The Trickster's Asymmetric Scam").
narrative_ontology:topic_domain(bushman_money_magic, "economic/social").

domain_priors:requires_active_enforcement(bushman_money_magic).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bushman_money_magic, trickster_figure).
narrative_ontology:constraint_victim(bushman_money_magic, deceived_ranchers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE VICTIM (SNARE) — Psychologically trapped by a narrative of greed and magic, the rancher has no exit. They experience the interaction as pure, coercive extraction. d≈0.95, f(d)≈1.42, σ=0.8 → χ≈0.97.
constraint_indexing:constraint_classification(bushman_money_magic, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE TRICKSTER (ROPE) — The perpetrator experiences the scam as a well-coordinated plan. They hold all informational power and can exit at any time. The 'coordination' is with their own deception. d≈0.15, f(d)≈-0.01, σ=0.8 → χ≈-0.01. The negative effective extraction signifies a pure beneficiary.
constraint_indexing:constraint_classification(bushman_money_magic, rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(local))).

% PERSPECTIVE 3: THE OBSERVER (PITON) — A community member who is not a target sees the 'magic' for what it is: pure theater. The constraint's function is entirely performative, maintained by the trickster's narrative. The high theater_ratio (0.90) triggers the Piton classification, as the observer sees a ritual with no real function.
constraint_indexing:constraint_classification(bushman_money_magic, piton,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(local))).

% PERSPECTIVE 4: THE ANALYST (SNARE) — The analytical view cuts through all narrative and theater to see the raw structure: a high-extraction, high-suppression mechanism with no coordination function. This matches the system's claimed_type.
constraint_indexing:constraint_classification(bushman_money_magic, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(bushman_money_magic_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(bushman_money_magic, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(bushman_money_magic, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(bushman_money_magic, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(bushman_money_magic, TR),
    TR >= 0.70.

:- end_tests(bushman_money_magic_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.85) is very high, as the scam's sole purpose is the complete transfer of the victim's assets. Suppression (0.75) is high because the trickster's narrative is designed to eliminate alternatives and create a sense of magical inevitability, trapping the victim psychologically. Theater Ratio (0.90) is extremely high, as the entire 'magic' process is a performance designed to obscure the simple act of theft. The scam has no real function beyond its theatricality.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is extreme and illustrative. The victim experiences a coercive Snare, feeling trapped and losing everything. The trickster, in contrast, experiences a perfect Rope—a plan executed flawlessly with no extraction *from their perspective*. They are the pure beneficiary. An outside observer who isn't fooled sees a Piton: a hollow, performative ritual that accomplishes nothing tangible. The analytical view confirms the victim's experience, classifying the structure as a Snare.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality is unambiguous. The 'trickster_figure' is the sole beneficiary and has arbitrage exit, deriving a very low 'd' value and seeing the constraint as a net subsidy (Rope). The 'deceived_ranchers' are the sole victims and are trapped, deriving a very high 'd' value and experiencing maximal extraction (Snare). This clean separation of roles is characteristic of pure extraction mechanisms.
 *
 * MANDATROPHY ANALYSIS:
 *   This case presents no mandatrophy. The analytical classification is clearly a Snare. The trickster's self-serving 'Rope' perspective is a textbook example of a perspectival illusion that the Deferential Realism system is designed to capture and contextualize. It correctly identifies that the perpetrator does not experience their own scheme as extractive, while simultaneously affirming that the underlying structure is, in fact, purely extractive for the target. The system does not average these views but holds them in tension.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    victim_culpability,
    'Is the victim''s participation driven by pure deception, or by a form of willful ignorance fueled by greed?',
    'Psychological profiling of victims; analysis of prior warnings or community skepticism they ignored.',
    'If pure deception, the Snare is absolute. If willful ignorance, the victim has more agency than the ''trapped'' exit implies, potentially shifting the classification for their perspective toward Tangled Rope (as they are coordinating with their own downfall).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_culpability, conceptual, 'Degree of victim agency vs. pure deception').

omega_variable(
    social_enforcement,
    'To what extent does the surrounding community''s silence or tacit encouragement enable the trickster''s scam?',
    'Ethnographic study of community responses to known trickster figures and scams.',
    'If the community is a passive enabler, the suppression metric is accurate. If the community actively ostracizes those who question the trickster, the suppression is even higher and is socially distributed, not just perpetrator-driven.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(social_enforcement, empirical, 'Role of community silence in enabling the scam').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bushman_money_magic, 0, 2).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bush_tr_t0, bushman_money_magic, theater_ratio, 0, 0.9).
narrative_ontology:measurement(bush_tr_t1, bushman_money_magic, theater_ratio, 1, 0.9).
narrative_ontology:measurement(bush_tr_t2, bushman_money_magic, theater_ratio, 2, 0.9).

% Extraction over time
narrative_ontology:measurement(bush_be_t0, bushman_money_magic, base_extractiveness, 0, 0.85).
narrative_ontology:measurement(bush_be_t1, bushman_money_magic, base_extractiveness, 1, 0.85).
narrative_ontology:measurement(bush_be_t2, bushman_money_magic, base_extractiveness, 2, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
