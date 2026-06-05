% ============================================================================
% CONSTRAINT STORY: brain_network_paradigm_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_brain_network_paradigm_2026, []).

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
 *   constraint_id: brain_network_paradigm_2026
 *   human_readable: Distributed Brain Network Scientific Paradigm
 *   domain: technological/scientific
 *
 * SUMMARY:
 *   This constraint represents the dominant scientific paradigm in
 *   neuroscience (c. 2026) that emphasizes the study of distributed brain
 *   networks using methods such as fMRI and EEG. While this paradigm has led
 *   to important insights, it also poses constraints on alternative research
 *   approaches and junior researchers who may feel pressured to conform to
 *   the dominant methodology to secure funding and publications.
 *
 * KEY AGENTS:
 *   - Incumbent Research Groups: Beneficiaries (institutional/arbitrage) - receive funding and prestige for network-level studies.
 *   - Network Analysis Tool Developers: Beneficiaries (powerful/mobile) - benefit from widespread adoption of their tools.
 *   - Alternative Research Approaches: Victims (powerless/trapped) - suppressed by funding priorities and publication biases.
 *   - Junior Researchers: Victims (moderate/constrained) - constrained by the need to adopt the dominant paradigm.
 *   - Analytical Observer: Sees full structure (analytical/analytical) - recognizes both the coordination benefits and potential drawbacks.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(brain_network_paradigm_2026, 0.55).
domain_priors:suppression_score(brain_network_paradigm_2026, 0.45).
domain_priors:theater_ratio(brain_network_paradigm_2026, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(brain_network_paradigm_2026, extractiveness, 0.55).
narrative_ontology:constraint_metric(brain_network_paradigm_2026, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(brain_network_paradigm_2026, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(brain_network_paradigm_2026, tangled_rope).
narrative_ontology:human_readable(brain_network_paradigm_2026, "Distributed Brain Network Scientific Paradigm").
narrative_ontology:topic_domain(brain_network_paradigm_2026, "technological/scientific").

domain_priors:requires_active_enforcement(brain_network_paradigm_2026).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(brain_network_paradigm_2026, incumbent_research_groups).
narrative_ontology:constraint_beneficiary(brain_network_paradigm_2026, network_analysis_tool_developers).
narrative_ontology:constraint_victim(brain_network_paradigm_2026, alternative_research_approaches).
narrative_ontology:constraint_victim(brain_network_paradigm_2026, junior_researchers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Perspective 1: Alternative research approaches (e.g., single-neuron electrophysiology, computational modeling of specific circuits) are suppressed by funding priorities and publication biases favoring network-level studies. Trapped due to lack of funding/recognition for non-network approaches.
constraint_indexing:constraint_classification(brain_network_paradigm_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% Perspective 2: Junior researchers are constrained by the need to adopt the dominant paradigm to secure funding and publications, but also benefit from the availability of established methods and data sets. Some agency, but constrained career paths.
constraint_indexing:constraint_classification(brain_network_paradigm_2026, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% Perspective 3: Incumbent research groups benefit from the paradigm by receiving funding and prestige for network-level studies. They also arbitrage existing knowledge and established methods. Experience is mainly coordination (rope).
constraint_indexing:constraint_classification(brain_network_paradigm_2026, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% Perspective 4: Developers of network analysis tools benefit directly from the paradigm's dominance as their tools become widely adopted. They are mobile and benefit from the current direction.
constraint_indexing:constraint_classification(brain_network_paradigm_2026, rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(global))).

% Perspective 5: Analytical observer recognizes both the coordination benefits (e.g., standardized methods, large datasets) and the potential for oversimplification and suppression of alternative approaches. Mixed perspective of extraction and coordination.
constraint_indexing:constraint_classification(brain_network_paradigm_2026, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(brain_network_paradigm_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(brain_network_paradigm_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(brain_network_paradigm_2026, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(brain_network_paradigm_2026, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(brain_network_paradigm_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness score reflects the pressure on researchers to conform to the dominant paradigm. The suppression score reflects the difficulty in obtaining funding and recognition for alternative approaches. The theater ratio reflects the degree to which research is driven by genuine scientific inquiry versus performative adherence to the paradigm.
 *
 * PERSPECTIVAL GAP:
 *   The gap arises from the different positions of the agents relative to the dominant paradigm. Incumbent groups benefit directly, while alternative approaches are suppressed. Junior researchers face a mixed situation, being both constrained and enabled. The analytical observer recognizes the tension between coordination and extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (incumbent groups and tool developers) have low directionality and experience the paradigm as coordination. Victims (alternative approaches and junior researchers) have high directionality and experience it as extraction. The analytical observer sees both aspects.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    optimal_network_resolution,
    'What is the optimal level of granularity for brain network analysis?',
    'Comparison of network-level findings with single-neuron data and computational models.',
    'Determines whether the current paradigm oversimplifies brain function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(optimal_network_resolution, empirical, 'Optimal level of granularity for brain network analysis').

omega_variable(
    non_network_funding_viability,
    'What is the long-term viability of funding for non-network neuroscience research?',
    'Analysis of funding trends and publication rates for different neuroscience subfields.',
    'Determines the extent to which alternative approaches are suppressed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(non_network_funding_viability, empirical, 'Long-term funding viability for non-network approaches').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(brain_network_paradigm_2026, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(brai_tr_t0, brain_network_paradigm_2026, theater_ratio, 0, 0.1).
narrative_ontology:measurement(brai_tr_t5, brain_network_paradigm_2026, theater_ratio, 5, 0.2).
narrative_ontology:measurement(brai_tr_t10, brain_network_paradigm_2026, theater_ratio, 10, 0.3).

% Extraction over time
narrative_ontology:measurement(brai_be_t0, brain_network_paradigm_2026, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(brai_be_t5, brain_network_paradigm_2026, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(brai_be_t10, brain_network_paradigm_2026, base_extractiveness, 10, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(brain_network_paradigm_2026, information_standard).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
