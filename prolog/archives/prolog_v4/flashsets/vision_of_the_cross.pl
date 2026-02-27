% ============================================================================
% CONSTRAINT STORY: vision_of_the_cross
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
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
 *   constraint_id: vision_of_the_cross
 *   human_readable: "In Hoc Signo Vinces" Mandate
 *   domain: religious/political
 *
 * SUMMARY:
 *   On the eve of the Battle of the Milvian Bridge in 312 CE, Constantine
 *   reportedly had a vision of a Christian symbol in the sky with the words
 *   "In hoc signo vinces" ("in this sign you will conquer"). This vision led
 *   Constantine to adopt Christianity, which had profound and lasting effects
 *   on the Roman Empire and the course of Western civilization. The mandate
 *   acted as both a tool for political consolidation and religious expansion.
 *
 * KEY AGENTS:
 *   - Constantine: Primary beneficiary (powerful/constrained) - benefited from unification, but constrained by need to maintain church support.
 *   - Early Christian Church: Primary beneficiary (institutional/arbitrage) - Gained legitimacy, resources, and widespread influence.
 *   - Roman Empire Pagan Sects: Primary victim (powerless/trapped) - Faced suppression and marginalization.
 *   - Political Dissidents: Secondary victim (powerless/trapped) - Subject to persecution under religious pretense.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vision_of_the_cross, 0.65).
domain_priors:suppression_score(vision_of_the_cross, 0.75).
domain_priors:theater_ratio(vision_of_the_cross, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vision_of_the_cross, extractiveness, 0.65).
narrative_ontology:constraint_metric(vision_of_the_cross, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(vision_of_the_cross, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vision_of_the_cross, tangled_rope).
narrative_ontology:human_readable(vision_of_the_cross, "\"In Hoc Signo Vinces\" Mandate").
narrative_ontology:topic_domain(vision_of_the_cross, "religious/political").

domain_priors:requires_active_enforcement(vision_of_the_cross).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vision_of_the_cross, constantine).
narrative_ontology:constraint_beneficiary(vision_of_the_cross, early_christian_church).
narrative_ontology:constraint_victim(vision_of_the_cross, roman_empire_pagan_sects).
narrative_ontology:constraint_victim(vision_of_the_cross, political_dissidents).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% From the perspective of the early Christian Church, Constantine's vision provided a crucial boost in legitimacy and power, facilitating the spread of Christianity across the Roman Empire and beyond. They benefited from the preferential treatment and resources allocated by Constantine.
constraint_indexing:constraint_classification(vision_of_the_cross, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% Pagan sects within the Roman Empire were suppressed and marginalized as Constantine's vision led to the rise of Christianity as the dominant religion. They faced increasing restrictions on their practices and beliefs.
constraint_indexing:constraint_classification(vision_of_the_cross, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(continental))).

% Constantine benefited from the unification and political stability that came with adopting Christianity, but he was also constrained by the need to maintain the support of the church and navigate the complex religious landscape of the Roman Empire. While powerful, exiting the mandate was not a feasible option.
constraint_indexing:constraint_classification(vision_of_the_cross, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(continental))).

% Those who politically dissented from Constantine's rule were vulnerable to persecution under the guise of religious enforcement.
constraint_indexing:constraint_classification(vision_of_the_cross, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% From a civilizational perspective, the "In Hoc Signo Vinces" mandate exhibits characteristics of a tangled rope. It facilitated the coordination and spread of Christianity while simultaneously extracting resources and suppressing alternative belief systems, shaping the religious and political landscape for centuries.
constraint_indexing:constraint_classification(vision_of_the_cross, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vision_of_the_cross_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(vision_of_the_cross, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(vision_of_the_cross, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(vision_of_the_cross, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vision_of_the_cross_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.65): High. Significant resources and power were extracted from pagan religions and transferred to the Christian Church. Suppression (0.75): High. Pagan practices were actively suppressed, and the mandate enforced a specific religious doctrine. Theater ratio (0.30): Moderate. While the mandate had a strong performative aspect in promoting Christian symbols and rituals, it also led to real changes in the structure of the Roman Empire.
 *
 * PERSPECTIVAL GAP:
 *   The early Christian Church viewed the mandate as divine intervention, leading to legitimacy and expansion. Pagan sects experienced the mandate as suppression and loss of their religious freedom. Constantine experienced it as a means of unifying his empire, though he was also constrained by the need to maintain the support of the church. This diversity of experiences highlights the indexical nature of the mandate.
 *
 * DIRECTIONALITY LOGIC:
 *   Constantine and the early Christian Church are beneficiaries because they gained power, resources, and influence. Pagan sects and political dissidents are victims because they faced suppression, marginalization, and persecution. The directionality scores reflect these structural relationships, with beneficiaries receiving lower scores and victims receiving higher scores.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandate prevents mislabeling coordination as pure extraction by acknowledging the real benefits to the early Christian Church in terms of legitimacy and expansion. It also prevents mislabeling pure extraction as coordination by recognizing the significant suppression of alternative belief systems and the persecution of political dissidents.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    historical_accuracy,
    'Is the vision of the cross a historical fact, or a later fabrication?',
    'Further historical research and analysis of primary sources.',
    'If fabricated, the mandate''s legitimacy is undermined. If factual, it supports the church''s claim of divine endorsement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_accuracy, empirical, 'The factual basis of the vision itself').

omega_variable(
    interpretation_of_vision,
    'How was the vision of the cross interpreted by Constantine and the early Christian Church?',
    'Analysis of contemporary theological and political writings.',
    'Different interpretations could lead to different policies and outcomes. If it was strictly interpreted as a mandate for military conquest, it could justify violence and oppression.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(interpretation_of_vision, conceptual, 'The meaning ascribed to the vision and its implications for policy').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vision_of_the_cross, 0, 200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(visi_tr_t0, vision_of_the_cross, theater_ratio, 0, 0.1).
narrative_ontology:measurement(visi_tr_t100, vision_of_the_cross, theater_ratio, 100, 0.2).
narrative_ontology:measurement(visi_tr_t200, vision_of_the_cross, theater_ratio, 200, 0.3).

% Extraction over time
narrative_ontology:measurement(visi_be_t0, vision_of_the_cross, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(visi_be_t100, vision_of_the_cross, base_extractiveness, 100, 0.5).
narrative_ontology:measurement(visi_be_t200, vision_of_the_cross, base_extractiveness, 200, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vision_of_the_cross, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
