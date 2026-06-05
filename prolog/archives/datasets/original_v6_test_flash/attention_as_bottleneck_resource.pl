% ============================================================================
% CONSTRAINT STORY: attention_as_bottleneck_resource
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-04-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_attention_as_bottleneck_resource, []).

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
 *   constraint_id: attention_as_bottleneck_resource
 *   human_readable: The Finite Cognitive Aperture
 *   domain: social/technological/economic
 *
 * SUMMARY:
 *   The finite cognitive aperture describes the scenario where the limited
 *   capacity of human attention becomes a primary bottleneck for social and
 *   economic coordination. As information volume increases exponentially, the
 *   competition for attention intensifies, leading to potential extraction
 *   and manipulation. This constraint analyzes the flow of attention as a
 *   limited resource and its impact on individuals, organizations, and public
 *   discourse.
 *
 * KEY AGENTS:
 *   - Individual Attention Spans: Primary victim (powerless/trapped) - Bears the cost of information overload and reduced focus.
 *   - Information Aggregators: Primary beneficiary (powerful/mobile) - Captures and directs attention for economic and political gain.
 *   - Attention Economy Platforms: Secondary beneficiary (institutional/arbitrage) - Monetizes user attention through advertising and data collection.
 *   - Public Discourse Coherence: Secondary victim (powerless/trapped) - Suffers from fragmentation and manipulation of information.
 *   - Informed Citizenry: Moderate impact (moderate/constrained) - Citizens benefit from access to information but are also constrained by attention limits.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(attention_as_bottleneck_resource, 0.6).
domain_priors:suppression_score(attention_as_bottleneck_resource, 0.7).
domain_priors:theater_ratio(attention_as_bottleneck_resource, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(attention_as_bottleneck_resource, extractiveness, 0.6).
narrative_ontology:constraint_metric(attention_as_bottleneck_resource, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(attention_as_bottleneck_resource, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(attention_as_bottleneck_resource, tangled_rope).
narrative_ontology:human_readable(attention_as_bottleneck_resource, "The Finite Cognitive Aperture").
narrative_ontology:topic_domain(attention_as_bottleneck_resource, "social/technological/economic").

domain_priors:requires_active_enforcement(attention_as_bottleneck_resource).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(attention_as_bottleneck_resource, information_aggregators).
narrative_ontology:constraint_beneficiary(attention_as_bottleneck_resource, attention_economy_platforms).
narrative_ontology:constraint_victim(attention_as_bottleneck_resource, individual_attention_spans).
narrative_ontology:constraint_victim(attention_as_bottleneck_resource, public_discourse_coherence).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Perspective 1: Individual Attention Spans (Snare) - Individuals are trapped within their own cognitive limits and increasingly bombarded by information, leading to a sense of overwhelm and reduced capacity for focused attention. No exit option.
constraint_indexing:constraint_classification(attention_as_bottleneck_resource, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% Perspective 2: Informed Citizenry (Tangled Rope) - Citizens benefit from access to information but are simultaneously constrained by the limits of their attention, resulting in a fragmented understanding of complex issues and susceptibility to manipulation. Constrained exit.
constraint_indexing:constraint_classification(attention_as_bottleneck_resource, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% Perspective 3: Attention Economy Platforms (Rope) - Platforms benefit from aggregating and monetizing user attention, viewing it as a resource that enables their business model. Arbitrage exit - they can shift strategies as needed.
constraint_indexing:constraint_classification(attention_as_bottleneck_resource, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% Perspective 4: Information Aggregators (Tangled Rope) - These entities (news organizations, social media algorithms) both benefit from and are constrained by the finite attention spans of their audience. They benefit by capturing and directing attention but are constrained by the need to provide easily digestible content, which can lead to oversimplification and sensationalism. Mobile exit.
constraint_indexing:constraint_classification(attention_as_bottleneck_resource, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% Perspective 5: Analytical Observer (Tangled Rope) - An observer analyzing the system sees a complex interaction between actors vying for attention. Coordination through information dissemination is coupled with extraction as attention is monetized and manipulated. Analytical exit.
constraint_indexing:constraint_classification(attention_as_bottleneck_resource, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(attention_as_bottleneck_resource_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(attention_as_bottleneck_resource, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(attention_as_bottleneck_resource, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(attention_as_bottleneck_resource, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(attention_as_bottleneck_resource_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.6): High. The system extracts attention from individuals and directs it toward specific ends, often for commercial or political gain. Suppression (0.7): High. The sheer volume of information and the design of attention-grabbing platforms suppress individuals' ability to focus and engage in critical thinking. Theater Ratio (0.3): Low. While there is some performative element in online interactions, the primary driver is the extraction of attention and its monetization. The focus is more on functional engagement rather than theatrical display.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap arises from the different relationships actors have with attention. Individuals experience it as a limited resource being drained (Snare). Platforms see it as a resource to be harvested (Rope). Aggregators both benefit from directing attention and are constrained by its limits (Tangled Rope).
 *
 * DIRECTIONALITY LOGIC:
 *   Platforms and aggregators are beneficiaries because they gain economic and political power by capturing attention. Individuals and public discourse are victims because their attention is exploited, manipulated, or fragmented. Citizens are in the middle, constrained by information but still possessing some agency.
 *
 * MANDATROPHY ANALYSIS:
 *   The claim is classified as Tangled Rope because it incorporates both coordination and extraction. The coordination aspect lies in the flow of information and the connection of people, while the extraction aspect is the monetization and manipulation of attention. Mislabeling this as pure extraction would ignore the coordination benefits of information access, while mislabeling it as pure coordination would ignore the extractive practices of attention economy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cognitive_capacity_elasticity,
    'To what extent can human cognitive capacity be expanded through training, technology, or other interventions?',
    'Longitudinal studies on the effects of cognitive training programs, neurofeedback, and other cognitive enhancement techniques.',
    'If highly elastic, the constraint may weaken over time. If inelastic, the constraint will intensify and become a more significant bottleneck.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cognitive_capacity_elasticity, empirical, 'The potential for expanding human cognitive capacity.').

omega_variable(
    information_filtering_effectiveness,
    'How effective are algorithms and other information filtering mechanisms at surfacing relevant and high-quality information while minimizing misinformation and cognitive overload?',
    'A/B testing of different information filtering algorithms, user surveys on information satisfaction, and analysis of the spread of misinformation.',
    'If highly effective, the constraint may be mitigated. If ineffective, the constraint will exacerbate, leading to increased polarization and cognitive fatigue.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(information_filtering_effectiveness, empirical, 'The effectiveness of information filtering mechanisms.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(attention_as_bottleneck_resource, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(atte_tr_t0, attention_as_bottleneck_resource, theater_ratio, 0, 0.1).
narrative_ontology:measurement(atte_tr_t5, attention_as_bottleneck_resource, theater_ratio, 5, 0.2).
narrative_ontology:measurement(atte_tr_t10, attention_as_bottleneck_resource, theater_ratio, 10, 0.3).

% Extraction over time
narrative_ontology:measurement(atte_be_t0, attention_as_bottleneck_resource, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(atte_be_t5, attention_as_bottleneck_resource, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(atte_be_t10, attention_as_bottleneck_resource, base_extractiveness, 10, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(attention_as_bottleneck_resource, information_standard).
narrative_ontology:affects_constraint(attention_as_bottleneck_resource, algorithmic_bias_amplification).
narrative_ontology:affects_constraint(attention_as_bottleneck_resource, filter_bubble_formation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
