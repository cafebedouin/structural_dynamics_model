% ============================================================================
% CONSTRAINT STORY: bushman_money_magic
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
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
 *   constraint_id: bushman_money_magic
 *   human_readable: The Trickster's Asymmetric Scam
 *   domain: economic/social
 *
 * SUMMARY:
 *   This constraint represents a "magic scam" where a trickster figure
 *   (Jackal) utilizes deception and temporary narrative control to extract
 *   wealth from victims (cattle ranchers). The success of the scam relies on
 *   the trickster's ability to manipulate perceptions and exploit
 *   vulnerabilities.
 *
 * KEY AGENTS:
 *   - Jackal Trickster: Beneficiary, exploits deception for personal gain
 *   - Cattle Ranchers: Victims, trapped in a cycle of extortion
 *   - Regional Authority: Potentially helpful enforcer, but may be limited or corrupt
 *   - National Law Enforcement: Distant and largely ineffective
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bushman_money_magic, 0.75).
domain_priors:suppression_score(bushman_money_magic, 0.8).
domain_priors:theater_ratio(bushman_money_magic, 0.75).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bushman_money_magic, extractiveness, 0.75).
narrative_ontology:constraint_metric(bushman_money_magic, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(bushman_money_magic, theater_ratio, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bushman_money_magic, snare).
narrative_ontology:human_readable(bushman_money_magic, "The Trickster's Asymmetric Scam").
narrative_ontology:topic_domain(bushman_money_magic, "economic/social").

domain_priors:requires_active_enforcement(bushman_money_magic).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bushman_money_magic, jackal_trickster).
narrative_ontology:constraint_victim(bushman_money_magic, cattle_ranchers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% The cattle ranchers are the primary victims, trapped in a cycle of extortion and deception with limited ability to exit or resist in the immediate term. High perceived extractiveness and suppression of alternatives.
constraint_indexing:constraint_classification(bushman_money_magic, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% From the trickster's perspective, this is a highly profitable and easily executed scheme with minimal risk. They benefit greatly, experiencing the arrangement as advantageous coordination for personal gain, and they can exit any time they want.
constraint_indexing:constraint_classification(bushman_money_magic, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(local))).

% The regional authority sees this as a local law enforcement issue, where there is a coordination problem preventing proper capture of the perpetrators, and it is also a matter of some extraction from local ranchers that might create issues for longer-term stability. There is active enforcement of a kind, but the enforcement itself is not always reliable.
constraint_indexing:constraint_classification(bushman_money_magic, tangled_rope,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(regional))).

% From the national law enforcement perspective, this is a lower-priority matter due to its local impact and relatively low value, and so resources are not applied efficiently, if at all. At this scale, the function has atrophied, leaving behind a theatrical process.
constraint_indexing:constraint_classification(bushman_money_magic, piton,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(bushman_money_magic_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(bushman_money_magic, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(bushman_money_magic, TypeOther, context(agent_power(institutional), _, _, _)),
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
 *   The high extractiveness score (0.75) reflects the significant loss suffered by the cattle ranchers. The high suppression score (0.80) signifies the ranchers' limited ability to resist or escape the scam. The moderate theater ratio (0.75) signifies the performance of regional authority that can be trusted to resolve the situation fairly.
 *
 * PERSPECTIVAL GAP:
 *   The cattle ranchers experience a pure snare, while the trickster enjoys the benefits of a seemingly harmless scheme. The regional authority, depending on its efficacy, might perceive it as a tangled rope or even piton, if the enforcement function has atrophied. At the level of national law enforcement, the process seems more like a piton, if the problem is under-resourced.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality is derived from the beneficiary and victim declarations. The trickster is the beneficiary with negative directionality. The ranchers are the victims with positive directionality. The regional and national authorities represent enforcement perspectives with corresponding directionality scores.
 *
 * MANDATROPHY ANALYSIS:
 *   This scam cannot be easily mislabeled. It is neither a necessary component of a legitimately productive scheme or practice, nor is it a natural law. The regional authority's perspective is Tangled Rope because they are actively trying to enforce the law, but are not fully effective due to the trickster's deception skills and the ranchers' lack of resources. The active enforcement is what makes it Tangled Rope rather than Snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    jackal_capture_success,
    'What is the probability of successfully capturing the trickster, considering their deception skills and the ranchers'' lack of resources?',
    'Track the trickster''s tactics and the ranchers'' attempts to protect their assets.',
    'If the trickster is easily captured, it would dissuade similar scams. If capture is nearly impossible, the extraction will continue.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(jackal_capture_success, empirical, 'Determine the success rate of capturing the trickster.').

omega_variable(
    regional_intervention,
    'How effective are the regional authorities in protecting the ranchers, and can they be trusted to address the issue fairly?',
    'Analyze the level of enforcement and its impact on the ranchers'' vulnerability.',
    'If regional authorities are capable and reliable, it would disrupt the scam. If they are corrupt or inept, the extraction would continue.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(regional_intervention, empirical, 'Understand the role and efficacy of regional authorities.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bushman_money_magic, 0, 4).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bush_tr_t0, bushman_money_magic, theater_ratio, 0, 0.2).
narrative_ontology:measurement(bush_tr_t2, bushman_money_magic, theater_ratio, 2, 0.5).
narrative_ontology:measurement(bush_tr_t4, bushman_money_magic, theater_ratio, 4, 0.75).

% Extraction over time
narrative_ontology:measurement(bush_be_t0, bushman_money_magic, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(bush_be_t2, bushman_money_magic, base_extractiveness, 2, 0.6).
narrative_ontology:measurement(bush_be_t4, bushman_money_magic, base_extractiveness, 4, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bushman_money_magic, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
