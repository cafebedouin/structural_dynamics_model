% ============================================================================
% CONSTRAINT STORY: xi_mao_ideological_centralization
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-08
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_xi_mao_ideological_centralization, []).

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
 *   constraint_id: xi_mao_ideological_centralization
 *   human_readable: Ideological Centralization and the Leadership Core
 *   domain: political
 *
 * SUMMARY:
 *   This constraint analyzes the structural centralization of power through
 *   ideological orthodoxy and the dismantling of institutional succession
 *   norms. The process involves increasing extractiveness from dissenting
 *   voices and institutional norms, while benefiting the leadership core
 *   through enhanced control. The rising theater ratio reflects an increasing
 *   emphasis on performative displays of loyalty over genuine institutional
 *   function.
 *
 * KEY AGENTS:
 *   - Leadership Core: Primary beneficiary (institutional/arbitrage) - gains power and control through centralization.
 *   - Dissenting Voices: Primary victim (powerless/trapped) - face suppression and limited opportunities for expression.
 *   - Institutional Succession Norms: Secondary victim (moderate/constrained) - weakened and undermined by centralization.
 *   - Party Cadres: Mixed role (powerful/constrained) - benefit from stability, but constrained by ideological requirements.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(xi_mao_ideological_centralization, 0.8).
domain_priors:suppression_score(xi_mao_ideological_centralization, 0.75).
domain_priors:theater_ratio(xi_mao_ideological_centralization, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(xi_mao_ideological_centralization, extractiveness, 0.8).
narrative_ontology:constraint_metric(xi_mao_ideological_centralization, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(xi_mao_ideological_centralization, theater_ratio, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(xi_mao_ideological_centralization, snare).
narrative_ontology:human_readable(xi_mao_ideological_centralization, "Ideological Centralization and the Leadership Core").
narrative_ontology:topic_domain(xi_mao_ideological_centralization, "political").

domain_priors:requires_active_enforcement(xi_mao_ideological_centralization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(xi_mao_ideological_centralization, leadership_core).
narrative_ontology:constraint_victim(xi_mao_ideological_centralization, dissenting_voices).
narrative_ontology:constraint_victim(xi_mao_ideological_centralization, institutional_succession_norms).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Individuals and groups expressing dissenting opinions find themselves trapped within a system that actively suppresses their views, lacking the power to exit or effectively challenge the centralized ideology. They bear the brunt of the extraction, facing consequences for deviating from the official line.
constraint_indexing:constraint_classification(xi_mao_ideological_centralization, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% Established procedures and norms for leadership succession are weakened and undermined, becoming largely performative rituals with little actual influence on the selection of future leaders. These norms are constrained by the centralized power structure and lack the power to effectively shape the political landscape.
constraint_indexing:constraint_classification(xi_mao_ideological_centralization, piton,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% The central leadership benefits from the ideological centralization, using it to consolidate power, maintain control, and justify their actions. They experience this as a coordination mechanism that strengthens their position and facilitates the implementation of their policies. The 'arbitrage' comes from the ability to set the terms of discourse and enforce conformity, creating political rents.
constraint_indexing:constraint_classification(xi_mao_ideological_centralization, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% Party cadres experience a mix of coordination and extraction. They benefit from the stability and resources provided by the centralized system, but are also constrained by the strict ideological requirements and limited opportunities for advancement outside of the approved path. They benefit from coordination but are also subject to asymmetric extraction if they deviate.
constraint_indexing:constraint_classification(xi_mao_ideological_centralization, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% From a global perspective, the suppression of dissenting voices within a major nation represents a loss for intellectual diversity and freedom of expression worldwide. This perspective views the centralized ideology as a snare, limiting the exchange of ideas and hindering intellectual progress. Global actors are trapped by the influence the system can exert through soft power and censorship.
constraint_indexing:constraint_classification(xi_mao_ideological_centralization, snare,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(global))).

% From an analytical perspective, the ideological centralization represents a complex interplay of coordination and extraction. While it may provide stability and facilitate policy implementation, it also suppresses dissent, limits intellectual freedom, and undermines institutional norms. The long-term consequences of this centralization are uncertain, making it a tangled rope that requires careful analysis.
constraint_indexing:constraint_classification(xi_mao_ideological_centralization, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(xi_mao_ideological_centralization_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(xi_mao_ideological_centralization, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(xi_mao_ideological_centralization, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(xi_mao_ideological_centralization, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(xi_mao_ideological_centralization, TR),
    TR >= 0.70.

:- end_tests(xi_mao_ideological_centralization_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.80): High. The system actively extracts from those who deviate from the official ideology, limiting their opportunities and suppressing their voices. Suppression (0.75): High. Dissenting voices are actively suppressed through censorship, surveillance, and other mechanisms. Theater ratio (0.60): Moderate. There is a significant emphasis on performative displays of loyalty and adherence to the ideology, which can overshadow genuine institutional function.
 *
 * PERSPECTIVAL GAP:
 *   The leadership core sees the ideological centralization as a necessary tool for maintaining stability and promoting national unity (Rope). Dissenting voices experience it as a repressive force that silences their opinions and limits their freedom (Snare). Institutional succession norms are degraded over time. Global actors may see it as a loss for intellectual diversity (Snare). Analytical observers recognize both the potential benefits and the significant costs (Tangled Rope).
 *
 * DIRECTIONALITY LOGIC:
 *   The leadership core benefits from the centralization, giving them low d. Dissenting voices bear the costs, giving them high d. Institutional succession norms are undermined, but are not entirely powerless, giving them moderate d. Party cadres experience a mix of benefits and costs, resulting in moderate d. The analytical observer sees the full complexity of the situation, resulting in a balanced d.
 *
 * MANDATROPHY ANALYSIS:
 *   The high level of extractiveness and suppression indicate a Snare. While the leadership core may claim that the centralization is necessary for stability and unity, the evidence suggests that it is primarily a mechanism for consolidating power and suppressing dissent. The victims cannot exit, and the system is actively enforced, all characteristics of a snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    degree_of_genuine_belief,
    'To what extent is the ideological adherence genuine, and to what extent is it performative?',
    'Surveys of public opinion, analysis of internal party documents, observation of elite behavior.',
    'If genuine: the system may be more resilient and resistant to change. If performative: the system may be more vulnerable to internal contradictions and external pressures.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(degree_of_genuine_belief, empirical, 'The balance between genuine belief and performative adherence to the ideology.').

omega_variable(
    stability_vs_innovation,
    'What is the trade-off between the stability provided by ideological centralization and the innovation stifled by the suppression of dissent?',
    'Comparative analysis of economic and social indicators, assessment of technological progress, evaluation of cultural dynamism.',
    'If stability outweighs innovation: the system may be sustainable in the short term, but may face long-term challenges. If innovation outweighs stability: the system may be more adaptable and resilient, but may face short-term instability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(stability_vs_innovation, conceptual, 'The trade-off between stability and innovation in the context of ideological centralization.').

omega_variable(
    succession_mechanism_efficacy,
    'What are the true selection criteria for future leaders, and how effective are they at ensuring competent and accountable leadership?',
    'Analysis of leadership selection processes, evaluation of leader performance, assessment of corruption levels.',
    'If the criteria are meritocratic and effective: the system may be able to adapt and evolve. If the criteria are based on patronage and loyalty: the system may become more rigid and vulnerable to decline.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(succession_mechanism_efficacy, empirical, 'The efficacy of the leadership succession mechanism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(xi_mao_ideological_centralization, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(xi_m_tr_t0, xi_mao_ideological_centralization, theater_ratio, 0, 0.4).
narrative_ontology:measurement(xi_m_tr_t5, xi_mao_ideological_centralization, theater_ratio, 5, 0.5).
narrative_ontology:measurement(xi_m_tr_t10, xi_mao_ideological_centralization, theater_ratio, 10, 0.6).

% Extraction over time
narrative_ontology:measurement(xi_m_be_t0, xi_mao_ideological_centralization, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(xi_m_be_t5, xi_mao_ideological_centralization, base_extractiveness, 5, 0.7).
narrative_ontology:measurement(xi_m_be_t10, xi_mao_ideological_centralization, base_extractiveness, 10, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(xi_mao_ideological_centralization, enforcement_mechanism).
narrative_ontology:affects_constraint(xi_mao_ideological_centralization, economic_reform_constraints).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
