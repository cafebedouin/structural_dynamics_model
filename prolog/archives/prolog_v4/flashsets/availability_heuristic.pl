% ============================================================================
% CONSTRAINT STORY: availability_heuristic
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-08
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_availability_heuristic, []).

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
 *   constraint_id: availability_heuristic
 *   human_readable: Availability Heuristic (as exploited by information systems)
 *   domain: cognitive/social/economic
 *
 * SUMMARY:
 *   The availability heuristic, a cognitive shortcut where people
 *   overestimate the importance of information that is readily available, is
 *   exploited by information systems to shape beliefs and behaviors. While
 *   providing easy access to information can be beneficial (coordination),
 *   the heuristic becomes extractive when systems prioritize sensational,
 *   biased, or emotionally charged content. This creates a structural
 *   asymmetry where individuals are easily manipulated, while information
 *   system operators and narrative shapers benefit from increased engagement
 *   and influence.
 *
 * KEY AGENTS:
 *   - Individual Decision Makers: Primary target (powerless/trapped) – susceptible to biased information due to cognitive limitations and limited access to diverse sources.
 *   - Information System Operators: Primary beneficiary (institutional/arbitrage) – drive engagement and advertising revenue by exploiting the heuristic.
 *   - Advertisers: Secondary beneficiary (powerful/mobile) – leverage vivid and readily available examples to influence consumer choices.
 *   - Public Discourse Quality: Secondary target (powerless/trapped) – abstract collective good degraded by the prevalence of misinformation and polarized narratives.
 *   - Narrative Shapers: Primary beneficiary (organized/mobile) – Use biased readily available information for personal or political narrative shaping.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(availability_heuristic, 0.55).
domain_priors:suppression_score(availability_heuristic, 0.65).
domain_priors:theater_ratio(availability_heuristic, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(availability_heuristic, extractiveness, 0.55).
narrative_ontology:constraint_metric(availability_heuristic, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(availability_heuristic, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(availability_heuristic, tangled_rope).
narrative_ontology:human_readable(availability_heuristic, "Availability Heuristic (as exploited by information systems)").
narrative_ontology:topic_domain(availability_heuristic, "cognitive/social/economic").

domain_priors:requires_active_enforcement(availability_heuristic).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(availability_heuristic, information_system_operators).
narrative_ontology:constraint_beneficiary(availability_heuristic, advertisers).
narrative_ontology:constraint_beneficiary(availability_heuristic, narrative_shapers).
narrative_ontology:constraint_victim(availability_heuristic, individual_decision_makers).
narrative_ontology:constraint_victim(availability_heuristic, public_discourse_quality).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Perspective 1: The Misinformed Individual (Snare) - Lacks the resources or cognitive capacity to systematically analyze information sources, relying on readily available, often biased, information. Trapped by their cognitive limitations and the pervasive nature of manipulated information environments.
constraint_indexing:constraint_classification(availability_heuristic, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(global))).

% Perspective 2: The Concerned Citizen (Tangled Rope) - Aware of the availability heuristic but constrained by time, resources, and access to diverse information sources. Benefits from some easily accessible correctives but remains vulnerable to biased narratives. Coordination (availability of fact checking) is mixed with extraction (cognitive burden of analysis).
constraint_indexing:constraint_classification(availability_heuristic, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% Perspective 3: Information System Operator (Rope) - Benefits directly from the availability heuristic, using it to drive engagement, advertising revenue, or narrative control. Experiences the availability heuristic as a coordination mechanism to efficiently influence behavior. Operators can arbitrage by promoting favored narratives and suppressing dissenting views.
constraint_indexing:constraint_classification(availability_heuristic, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% Perspective 4: Analytical Observer (Tangled Rope) - Sees the interplay between cognitive biases, information system incentives, and the erosion of public discourse. Understands the systematic nature of the extraction and the limited agency of individuals trapped within these systems. Analytical perspective recognizes that both coordination (easy access to information) and asymmetric extraction are at play.
constraint_indexing:constraint_classification(availability_heuristic, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(availability_heuristic_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(availability_heuristic, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(availability_heuristic, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(availability_heuristic, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(availability_heuristic_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55): Moderate. The availability heuristic extracts cognitive resources from individual decision-makers, leading to suboptimal choices and distorted perceptions. Information systems and narrative shapers derive value from this extraction. Suppression (0.65): Moderate-High. Algorithmic curation and filter bubbles suppress alternative viewpoints, making it difficult for individuals to access a balanced representation of information. The suppression is actively enforced by the design of these systems. Theater Ratio (0.30): Low. While there is some performative element, the exploitation is primarily driven by structural incentives rather than superficial displays.
 *
 * PERSPECTIVAL GAP:
 *   The individual (Snare) experiences the availability heuristic as a trap, with no escape from biased information. Information system operators (Rope) benefit from the heuristic, experiencing it as a tool for effective engagement. Analytical observers (Tangled Rope) understand the systemic nature of the extraction and the limitations of individual agency. The concerned citizen (Tangled Rope) recognizes the problem but faces constraints in addressing it effectively.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality is derived from the relationship between the agents and the flow of information. Beneficiaries (information systems, advertisers, narrative shapers) experience a low d-value, as the heuristic provides them with a tool to influence others. Victims (individual decision-makers, public discourse quality) experience a high d-value, as their cognitive resources and informational environment are manipulated.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    individual_cognitive_capacity,
    'What is the practical limit to an individual''s ability to evaluate information sources critically and resist the availability heuristic?',
    'Cognitive psychology studies on information processing, critical thinking skills, and resistance to persuasion. Meta-analysis of interventions designed to improve media literacy.',
    'If capacity is severely limited: the individual perspective remains Snare; interventions are largely ineffective. If capacity is expandable: interventions can shift the perspective towards scaffold or rope, increasing individual agency.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(individual_cognitive_capacity, empirical, 'Assesses the limits of individual cognitive capacity in information processing.').

omega_variable(
    information_system_incentives,
    'To what extent are information systems incentivized to exploit the availability heuristic for financial gain or narrative control, versus providing unbiased information access?',
    'Economic analysis of information system revenue models, network analysis of information flow, and empirical studies on the impact of algorithmic curation on user beliefs.',
    'If incentives favor exploitation: the information system operator perspective remains Rope from their view (extraction is toward them), but the individual perspective is more likely Snare. If incentives are aligned with unbiased access: the individual perspective can shift towards Tangled Rope or even Rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(information_system_incentives, empirical, 'Analyzes the incentives within information systems.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(availability_heuristic, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(avai_tr_t0, availability_heuristic, theater_ratio, 0, 0.1).
narrative_ontology:measurement(avai_tr_t5, availability_heuristic, theater_ratio, 5, 0.2).
narrative_ontology:measurement(avai_tr_t10, availability_heuristic, theater_ratio, 10, 0.3).

% Extraction over time
narrative_ontology:measurement(avai_be_t0, availability_heuristic, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(avai_be_t5, availability_heuristic, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(avai_be_t10, availability_heuristic, base_extractiveness, 10, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(availability_heuristic, information_standard).
narrative_ontology:affects_constraint(availability_heuristic, confirmation_bias).
narrative_ontology:affects_constraint(availability_heuristic, echo_chamber_effects).
narrative_ontology:affects_constraint(availability_heuristic, filter_bubble).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
