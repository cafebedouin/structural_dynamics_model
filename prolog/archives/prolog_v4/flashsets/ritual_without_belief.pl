% ============================================================================
% CONSTRAINT STORY: ritual_without_belief
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ritual_without_belief, []).

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
 *   constraint_id: ritual_without_belief
 *   human_readable: The Hollow Orthopraxy
 *   domain: social/organizational/religious
 *
 * SUMMARY:
 *   The 'Hollow Orthopraxy' describes a social or religious scenario where
 *   the external performance of a ritual or protocol is strictly enforced,
 *   even though the underlying belief or functional utility has vanished.
 *   This constraint often arises in long-standing institutions that have
 *   become disconnected from their original purpose. While the ritual may
 *   have once served a vital function, such as promoting social cohesion or
 *   expressing religious devotion, it has now become a mere formality, devoid
 *   of genuine meaning or practical value.
 *
 * KEY AGENTS:
 *   - Individual Believers: Primary victims (powerless/trapped) – Forced to conform to rituals they no longer believe in.
 *   - Legacy Institution: Primary beneficiary (institutional/constrained) – Maintains the ritual for the sake of continuity and legitimacy.
 *   - Ritual Enforcers: Secondary actors (moderate/mobile) – Enforce the ritual for power and status, but also bear the cost of suppressing dissent.
 *   - Organizational Adaptability: Victim (moderate/mobile). Loses ability to adapt to current context.
 *   - Analytical Observer: Analytical perspective (analytical/analytical) – Recognizes the ritual as a hollow performance.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ritual_without_belief, 0.6).
domain_priors:suppression_score(ritual_without_belief, 0.7).
domain_priors:theater_ratio(ritual_without_belief, 0.85).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ritual_without_belief, extractiveness, 0.6).
narrative_ontology:constraint_metric(ritual_without_belief, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(ritual_without_belief, theater_ratio, 0.85).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ritual_without_belief, piton).
narrative_ontology:human_readable(ritual_without_belief, "The Hollow Orthopraxy").
narrative_ontology:topic_domain(ritual_without_belief, "social/organizational/religious").

domain_priors:requires_active_enforcement(ritual_without_belief).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ritual_without_belief, legacy_institution).
narrative_ontology:constraint_beneficiary(ritual_without_belief, ritual_enforcers).
narrative_ontology:constraint_victim(ritual_without_belief, individual_believers).
narrative_ontology:constraint_victim(ritual_without_belief, organizational_adaptability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Perspective of an individual believer who no longer finds meaning in the ritual but is pressured to conform. They are trapped within the system, experiencing high extraction due to the suppression of alternative expressions of belief or belonging. The snare arises from the cognitive dissonance and emotional cost of performing a ritual they no longer believe in.
constraint_indexing:constraint_classification(ritual_without_belief, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% Perspective of the legacy institution that maintains the ritual. The institution benefits from the continued performance of the ritual, as it provides a sense of continuity and legitimacy. However, the institution is also constrained by its own traditions and may be unable to adapt to changing circumstances. The theater ratio is high, as the external performance of the ritual is more important than its underlying meaning.
constraint_indexing:constraint_classification(ritual_without_belief, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% Perspective of the individuals responsible for enforcing the ritual. They benefit from the power and status that comes with their role, but they also bear the cost of suppressing dissent and maintaining the system. They have some mobility, as they could potentially choose to leave their role, but they are also constrained by their own beliefs and the expectations of their community. Tangled rope: enforces the ritual (extraction) but is also bound by it.
constraint_indexing:constraint_classification(ritual_without_belief, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(regional))).

% Perspective of the organization as a whole regarding its own ability to adapt to changing circumstances. Its adaptability is harmed as legacy rituals are enforced. There is a mixed coordination/extraction profile as some members see the benefit of tradition while others seek change. The mobility is limited as some of its members are legacy institutions that can't easily exit. Tangled rope.
constraint_indexing:constraint_classification(ritual_without_belief, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(regional))).

% Perspective of an analytical observer who sees the ritual as a hollow performance. The observer recognizes that the ritual no longer serves its original purpose and that it is maintained primarily for its symbolic value. The high theater ratio and declining meaning indicate a piton.
constraint_indexing:constraint_classification(ritual_without_belief, piton,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ritual_without_belief_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ritual_without_belief, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ritual_without_belief, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(ritual_without_belief, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(ritual_without_belief, TR),
    TR >= 0.70.

:- end_tests(ritual_without_belief_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.60): High. The ritual extracts conformity, time, and emotional energy from individuals who no longer believe in it. Suppression (0.70): High. Dissent is suppressed through social pressure, fear of ostracism, or formal sanctions. Theater Ratio (0.85): Very High. The external performance of the ritual is more important than its underlying meaning or practical value. Performative compliance is prized over internal conviction.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap arises from the differing structural positions of the key agents. The individual believer experiences the ritual as a snare, as they are trapped within the system and forced to conform. The legacy institution sees the ritual as a piton, as it is maintained primarily for its symbolic value. The ritual enforcers experience the ritual as a tangled rope, as they benefit from the power and status that comes with their role, but they also bear the cost of suppressing dissent. The analytical observer sees the ritual as a piton, as it is a degraded institution that no longer serves its original purpose.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality values are determined by the agents' structural relationships to the constraint. The individual believer, as the primary victim, has a high directionality value, as they bear the full cost of the ritual. The legacy institution, as the primary beneficiary, has a low directionality value, as they benefit from the ritual's continued performance. The ritual enforcers have a moderate directionality value, as they both benefit from and bear the cost of the ritual.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by recognizing that the same ritual can be classified differently depending on the observer's perspective. The key is to identify the structural relationships between the agents and the constraint and to understand how these relationships shape their experiences. The piton is not inherently bad or wrong, but it is important to recognize that it is a degraded institution that may need to be reformed or replaced. The mandate is resolved once we can identify each perspective and why it exists.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    original_belief_threshold,
    'What percentage of adherents must cease to believe in the ritual''s efficacy before it is considered ''hollow''?',
    'Sociological surveys, historical analysis of belief systems',
    'High threshold: ritual is deemed functional despite widespread skepticism. Low threshold: ritual is deemed hollow despite pockets of genuine belief.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(original_belief_threshold, empirical, 'Threshold for considering a ritual ''hollow''').

omega_variable(
    functional_utility_definition,
    'What criteria define the ''functional utility'' of a ritual? Is it limited to tangible outcomes, or does it include social cohesion and psychological benefits?',
    'Interdisciplinary analysis incorporating sociology, psychology, and anthropology',
    'Narrow definition: more rituals classified as hollow. Broad definition: fewer rituals classified as hollow.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(functional_utility_definition, conceptual, 'Criteria for defining the ''functional utility'' of a ritual').

omega_variable(
    alternative_expression_availability,
    'To what extent are alternative expressions of belief or belonging available to individuals who no longer find meaning in the ritual?',
    'Sociological studies of social networks and cultural diversity',
    'High availability: lower suppression, weaker snare. Low availability: higher suppression, stronger snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_expression_availability, empirical, 'Availability of alternative expressions of belief').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ritual_without_belief, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ritu_tr_t0, ritual_without_belief, theater_ratio, 0, 0.3).
narrative_ontology:measurement(ritu_tr_t5, ritual_without_belief, theater_ratio, 5, 0.6).
narrative_ontology:measurement(ritu_tr_t10, ritual_without_belief, theater_ratio, 10, 0.85).

% Extraction over time
narrative_ontology:measurement(ritu_be_t0, ritual_without_belief, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(ritu_be_t5, ritual_without_belief, base_extractiveness, 5, 0.4).
narrative_ontology:measurement(ritu_be_t10, ritual_without_belief, base_extractiveness, 10, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ritual_without_belief, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
