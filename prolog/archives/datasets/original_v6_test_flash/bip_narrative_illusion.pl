% ============================================================================
% CONSTRAINT STORY: bip_narrative_illusion
% ============================================================================
% Version: 0.1 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [DRAFT]
% ============================================================================

:- module(constraint_bip_narrative_illusion, []).

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
 *   constraint_id: bip_narrative_illusion
 *   human_readable: The Black Iron Prison (BIP) and Sensory Optimization
 *   domain: philosophical/social/technological
 *
 * SUMMARY:
 *   The Black Iron Prison (BIP) is a philosophical concept describing a
 *   system where individuals are subtly controlled by pervasive narratives,
 *   technological infrastructure, and sensory manipulation. The modern world
 *   increasingly resembles this 'prison,' with corporate and institutional
 *   power shaping perceptions and limiting individual autonomy. Sensory
 *   optimization is a key feature of BIP.
 *
 * KEY AGENTS:
 *   - Individual: Primary victim (powerless/trapped) - experiences direct extraction and suppression of autonomy.
 *   - Corporate Power: Primary beneficiary (institutional/arbitrage) - benefits from a controlled and predictable populace.
 *   - Institutional Control: Secondary beneficiary (institutional/constrained) - reinforces established norms and power structures.
 *   - Dissenter: Secondary victim (moderate/constrained) - faces barriers to exit but attempts to exit. Bears costs of pushback from BIP.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bip_narrative_illusion, 0.7).
domain_priors:suppression_score(bip_narrative_illusion, 0.8).
domain_priors:theater_ratio(bip_narrative_illusion, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bip_narrative_illusion, extractiveness, 0.7).
narrative_ontology:constraint_metric(bip_narrative_illusion, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(bip_narrative_illusion, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bip_narrative_illusion, snare).
narrative_ontology:human_readable(bip_narrative_illusion, "The Black Iron Prison (BIP) and Sensory Optimization").
narrative_ontology:topic_domain(bip_narrative_illusion, "philosophical/social/technological").

domain_priors:requires_active_enforcement(bip_narrative_illusion).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bip_narrative_illusion, corporate_power).
narrative_ontology:constraint_beneficiary(bip_narrative_illusion, institutional_control).
narrative_ontology:constraint_victim(bip_narrative_illusion, individual_autonomy).
narrative_ontology:constraint_victim(bip_narrative_illusion, human_potential).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Perspective 1: The average individual, trapped within the BIP. They perceive the system as overwhelmingly extractive, with limited exit options. Their autonomy and potential are suppressed by the forces of corporate and institutional power.
constraint_indexing:constraint_classification(bip_narrative_illusion, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% Perspective 2: A dissenter trying to exist in the BIP. Limited power and exit options. Benefits some from the ability to speak freely about issues, but bears the cost of being targeted.
constraint_indexing:constraint_classification(bip_narrative_illusion, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% Perspective 3: Corporate power, benefiting from the BIP and arbitrage. They see this system as a beneficial source of power and control. It allows them to maximize profits and exert influence.
constraint_indexing:constraint_classification(bip_narrative_illusion, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% Perspective 4: An analytical observer, examining the BIP from a universal perspective. They see the system as a tangled rope, with both coordination and extraction. They recognize the power dynamics and the suppression of individual autonomy.
constraint_indexing:constraint_classification(bip_narrative_illusion, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(bip_narrative_illusion_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(bip_narrative_illusion, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(bip_narrative_illusion, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(bip_narrative_illusion, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(bip_narrative_illusion_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.7) because of the pervasive nature of the control mechanisms and the difficulty in escaping their influence. Suppression is also high (0.8) as alternative narratives and perspectives are often marginalized or silenced. Theater ratio is low (0.3) because while the illusion is enforced, it remains transparent to a degree.
 *
 * PERSPECTIVAL GAP:
 *   The individual is most vulnerable to this extraction, feeling powerless and trapped. Corporate power, conversely, views the BIP as beneficial for their interests. The analytical observer sees a more complex picture of mixed effects and power dynamics.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by the agent's relative position. Individuals face high extraction, while corporations benefit. Dissenters also face high extraction as they push back against the dominant narrative and established power. The analytical observer seeks to evaluate the truth with an outside view.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification of the BIP as a snare is appropriate because it emphasizes the extraction and suppression experienced by individuals. While there may be some coordination benefits (e.g., social order), these are secondary to the primary effect of limiting individual autonomy and potential.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    degree_of_free_will,
    'To what extent do individuals possess genuine free will versus being products of deterministic forces within the BIP?',
    'Philosophical inquiry, neurological research, social science studies examining human agency and decision-making.',
    'High free will: the BIP is a challenge, not a prison. Low free will: BIP is totalizing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(degree_of_free_will, conceptual, 'The degree to which free will exists within the BIP.').

omega_variable(
    nature_of_sensory_optimization,
    'Is sensory optimization a tool for liberation or further enslavement within the BIP?',
    'Empirical studies of the effects of sensory optimization on individual well-being, cognitive function, and social behavior; ethical analysis of the motivations and consequences of sensory optimization technologies.',
    'Liberation: sensory optimization empowers individuals. Enslavement: sensory optimization reinforces conformity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(nature_of_sensory_optimization, empirical, 'The true nature of sensory optimization within the BIP.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bip_narrative_illusion, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bip__tr_t0, bip_narrative_illusion, theater_ratio, 0, 0.1).
narrative_ontology:measurement(bip__tr_t5, bip_narrative_illusion, theater_ratio, 5, 0.2).
narrative_ontology:measurement(bip__tr_t10, bip_narrative_illusion, theater_ratio, 10, 0.3).

% Extraction over time
narrative_ontology:measurement(bip__be_t0, bip_narrative_illusion, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(bip__be_t5, bip_narrative_illusion, base_extractiveness, 5, 0.6).
narrative_ontology:measurement(bip__be_t10, bip_narrative_illusion, base_extractiveness, 10, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bip_narrative_illusion, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
