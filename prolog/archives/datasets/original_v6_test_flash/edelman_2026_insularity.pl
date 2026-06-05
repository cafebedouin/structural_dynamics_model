% ============================================================================
% CONSTRAINT STORY: edelman_2026_insularity
% ============================================================================
% Version: 0.1 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_edelman_2026_insularity, []).

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
 *   constraint_id: edelman_2026_insularity
 *   human_readable: The Insular Trust Mindset
 *   domain: social/economic
 *
 * SUMMARY:
 *   The Insular Trust Mindset describes a global condition where a
 *   significant portion of the population exhibits unwillingness or hesitancy
 *   to trust individuals or groups perceived as having different values,
 *   sources of information, or backgrounds. This phenomenon manifests as
 *   social fragmentation, economic disparities, and limited opportunities for
 *   cross-cultural or interdisciplinary collaboration. The constraint's high
 *   suppression indicates that the system actively restricts alternatives and
 *   reinforces insularity, while the moderate extractiveness suggests that
 *   those within the trusted groups benefit at the expense of those outside.
 *
 * KEY AGENTS:
 *   - Outsiders: Primary victim (powerless/trapped) - Lack of access and opportunity due to distrust.
 *   - Bridging Social Capital: Secondary victim (moderate/constrained) - Efforts to build trust are actively suppressed.
 *   - Insular Cliques: Primary beneficiary (institutional/arbitrage) - Benefits from amplified power and exclusion of others.
 *   - Information Gatekeepers: Secondary beneficiary (powerful/mobile) - Controls information flow within insular groups.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(edelman_2026_insularity, 0.55).
domain_priors:suppression_score(edelman_2026_insularity, 0.7).
domain_priors:theater_ratio(edelman_2026_insularity, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(edelman_2026_insularity, extractiveness, 0.55).
narrative_ontology:constraint_metric(edelman_2026_insularity, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(edelman_2026_insularity, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(edelman_2026_insularity, snare).
narrative_ontology:human_readable(edelman_2026_insularity, "The Insular Trust Mindset").
narrative_ontology:topic_domain(edelman_2026_insularity, "social/economic").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(edelman_2026_insularity, insular_cliques).
narrative_ontology:constraint_beneficiary(edelman_2026_insularity, information_gatekeepers).
narrative_ontology:constraint_victim(edelman_2026_insularity, outsiders).
narrative_ontology:constraint_victim(edelman_2026_insularity, bridging_social_capital).
narrative_ontology:constraint_victim(edelman_2026_insularity, open_dialogue).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Perspective 1: The Outsider (Snare) - Individuals from different backgrounds or with differing values find themselves trapped within a system where their voices are suppressed and opportunities are limited due to lack of trust. Their ability to exit these insular systems is severely restricted.
constraint_indexing:constraint_classification(edelman_2026_insularity, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% Perspective 2: The Bridging Social Capital (Tangled Rope) - Entities attempting to bridge divides experience a mix of coordination and extraction. They benefit from promoting understanding but are constrained by the resistance and active suppression from those benefiting from insularity. They are constrained in their ability to arbitrage the situation due to the inherent complexity of building trust across divides.
constraint_indexing:constraint_classification(edelman_2026_insularity, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% Perspective 3: The Insular Clique (Rope) - Benefits from the insular trust mindset, experiencing it as coordination, with their power amplified within the trusted group. They can arbitrage this situation by exploiting the lack of trust towards outsiders for economic or social gains.
constraint_indexing:constraint_classification(edelman_2026_insularity, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(local))).

% Perspective 4: The Analytical Observer (Tangled Rope) - Analyzes the global phenomenon of insular trust as a complex interplay between coordination and extraction. Recognizes the benefits for in-groups but also the costs imposed on out-groups and the overall societal cohesion. Sees the difficulty in arbitraging the situation due to deeply rooted social and economic factors.
constraint_indexing:constraint_classification(edelman_2026_insularity, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(edelman_2026_insularity_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(edelman_2026_insularity, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(edelman_2026_insularity, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(edelman_2026_insularity, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(edelman_2026_insularity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55): Moderate. Those within the insular groups extract benefits such as economic advantages, social status, and political influence, while those outside are denied these opportunities. Suppression (0.70): High. Active suppression of alternative viewpoints, limited access to information and opportunities, and social ostracization contribute to a high degree of suppression. Theater ratio (0.30): Low. The emphasis on actual, verifiable shared values and origins over symbolic displays is relatively strong, reducing the theater ratio.
 *
 * PERSPECTIVAL GAP:
 *   The perspectives highlight the different experiences within this system. Outsiders feel trapped and suppressed, while Insular Cliques experience coordination and amplified power. Analytical observers recognize the mixed effects, acknowledging the benefits for in-groups but also the overall societal costs. The Bridging Social Capital perspective highlights the challenges and constraints faced by those attempting to overcome insularity.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality values are determined by the agent's structural position and relationship to the constraint. Beneficiaries experience low or negative effective extraction, while victims experience high extraction. The Analytical Observer's perspective acknowledges the mixed effects and complexities of the constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy in this scenario is resolved by recognizing the distinct perspectives and the structural relationships that define them. The Snare classification for the Outsider reflects the true experience of being trapped and suppressed, while the Rope classification for the Insular Clique demonstrates the benefits and advantages gained from insularity. The Tangled Rope classification for Bridging Social Capital acknowledges the challenges and constraints faced by those attempting to overcome insularity, highlighting the complex interplay between coordination and extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    trust_definition_boundary,
    'What is the threshold for defining ''different values'' that triggers distrust?',
    'Sociological studies, surveys assessing public attitudes towards diverse values.',
    'Narrow threshold -> More people considered ''outsiders,'' exacerbating insularity. Wide threshold -> More inclusivity, reducing insularity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(trust_definition_boundary, empirical, 'Boundary definition of ''different values''').

omega_variable(
    information_ecosystem_homogeneity,
    'To what extent does echo chambers and filter bubbles reinforce insularity?',
    'Analysis of social media algorithms, content consumption habits, network analysis.',
    'High echo chamber effect -> Increased insularity, reduced exposure to diverse perspectives. Low echo chamber effect -> Greater openness to different viewpoints.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(information_ecosystem_homogeneity, empirical, 'Impact of echo chambers on insularity').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(edelman_2026_insularity, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(edel_tr_t0, edelman_2026_insularity, theater_ratio, 0, 0.15).
narrative_ontology:measurement(edel_tr_t5, edelman_2026_insularity, theater_ratio, 5, 0.22).
narrative_ontology:measurement(edel_tr_t10, edelman_2026_insularity, theater_ratio, 10, 0.3).

% Extraction over time
narrative_ontology:measurement(edel_be_t0, edelman_2026_insularity, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(edel_be_t5, edelman_2026_insularity, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(edel_be_t10, edelman_2026_insularity, base_extractiveness, 10, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(edelman_2026_insularity, enforcement_mechanism).
narrative_ontology:affects_constraint(edelman_2026_insularity, echo_chamber_reinforcement).
narrative_ontology:affects_constraint(edelman_2026_insularity, misinformation_proliferation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
