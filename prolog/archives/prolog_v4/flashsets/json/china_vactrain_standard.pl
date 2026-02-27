% ============================================================================
% CONSTRAINT STORY: china_vactrain_standard
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_china_vactrain_standard, []).

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
 *   constraint_id: china_vactrain_standard
 *   human_readable: China's Ultra-High-Speed Vacuum-Tube Maglev Standard
 *   domain: technological/economic
 *
 * SUMMARY:
 *   China's development of an ultra-high-speed vacuum-tube maglev standard
 *   presents a complex interplay of technological innovation, economic
 *   strategy, and geopolitical influence. While the standard aims to
 *   establish a leading position in transportation technology, it also
 *   creates a potential barrier for foreign competitors and may stifle
 *   innovation in alternative transport solutions. The constraint manifests
 *   differently for various actors, ranging from significant benefits for
 *   Chinese manufacturers to potential disadvantages for foreign firms.
 *
 * KEY AGENTS:
 *   - Chinese Vactrain Manufacturers: Primary beneficiary (institutional/arbitrage) - Gains protected domestic market and potential global export advantage.
 *   - Chinese Infrastructure Developers: Secondary beneficiary (moderate/constrained) - Benefit from guaranteed domestic market but constrained by the standard itself.
 *   - Foreign Vactrain Manufacturers: Primary target (powerless/trapped) - Largely excluded from Chinese market and face difficulty competing without adhering to the standard.
 *   - Alternative Transport Technologies: Secondary target (moderate/mobile) - May face reduced investment and market share due to the prioritization of vactrain technology.
 *   - Incumbent Rail Industry: Attempt to adapt but increasingly performative
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(china_vactrain_standard, 0.55).
domain_priors:suppression_score(china_vactrain_standard, 0.45).
domain_priors:theater_ratio(china_vactrain_standard, 0.75).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(china_vactrain_standard, extractiveness, 0.55).
narrative_ontology:constraint_metric(china_vactrain_standard, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(china_vactrain_standard, theater_ratio, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(china_vactrain_standard, tangled_rope).
narrative_ontology:human_readable(china_vactrain_standard, "China's Ultra-High-Speed Vacuum-Tube Maglev Standard").
narrative_ontology:topic_domain(china_vactrain_standard, "technological/economic").

domain_priors:requires_active_enforcement(china_vactrain_standard).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(china_vactrain_standard, chinese_vactrain_manufacturers).
narrative_ontology:constraint_beneficiary(china_vactrain_standard, chinese_infrastructure_developers).
narrative_ontology:constraint_victim(china_vactrain_standard, foreign_vactrain_manufacturers).
narrative_ontology:constraint_victim(china_vactrain_standard, alternative_transport_technologies).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Foreign manufacturers are largely excluded from the Chinese market and have difficulty competing globally without adhering to the Chinese standard. They are trapped because developing an alternative standard would be prohibitively expensive and time-consuming.
constraint_indexing:constraint_classification(china_vactrain_standard, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% Chinese infrastructure developers are constrained by the national standard but benefit from the guaranteed market and reduced competition within China. They have limited exit options due to government mandates but can influence the standard's evolution.
constraint_indexing:constraint_classification(china_vactrain_standard, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% Chinese manufacturers benefit from the standard as it creates a protected domestic market and a potential advantage in exporting the technology globally. They can arbitrage the standard to gain market share and influence its development.
constraint_indexing:constraint_classification(china_vactrain_standard, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% An analytical observer sees the standard as a tangled rope, providing coordination benefits within China but also extracting value from foreign competitors and potentially hindering innovation in alternative transport technologies. It's a hybrid system with both coordination and asymmetric extraction.
constraint_indexing:constraint_classification(china_vactrain_standard, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% The incumbent rail industry may find its position eroded by the emergence of vactrain technology. Constrained, they attempt to slow progress or co-opt the technology for their own use. But the effort to adapt existing rail infrastructure is largely performative. As such, they are characterized as a Piton.
constraint_indexing:constraint_classification(china_vactrain_standard, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(china_vactrain_standard_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(china_vactrain_standard, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(china_vactrain_standard, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(china_vactrain_standard, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(china_vactrain_standard, TR),
    TR >= 0.70.

:- end_tests(china_vactrain_standard_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55): Moderate-high. The standard allows Chinese manufacturers to extract value by creating a protected domestic market and potentially dominating the global market. Suppression (0.45): Moderate. Foreign competitors face barriers to entry due to the standard, but alternative transport technologies and foreign vactrain efforts still exist. Theater ratio (0.75): High. The incumbent rail industry's attempts to adapt existing infrastructure to the new standard are largely performative, aimed at maintaining relevance rather than achieving genuine integration. This performative aspect contributes significantly to the overall theater ratio.
 *
 * PERSPECTIVAL GAP:
 *   The Chinese vactrain standard is a tangled rope. Chinese manufacturers perceive it as a rope facilitating domestic coordination and global expansion. Foreign manufacturers view it as a snare, trapping them outside the Chinese market. Infrastructure developers experience it as a constraint, balancing benefits and limitations. The analytical observer sees the standard as a tangled rope due to its mixed effects. The incumbent rail industry sees its prospects fading.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from the structural positions of the agents. Beneficiaries (Chinese manufacturers) have low directionality, experiencing the standard as a coordination mechanism. Victims (foreign manufacturers) have high directionality, bearing the costs of exclusion and market barriers. The analytical observer's perspective reflects a balanced view of coordination and extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The standard prevents mislabeling coordination as extraction by ensuring that the beneficiaries (Chinese manufacturers) genuinely benefit from the coordination aspects of the standard. However, it prevents mislabeling extraction as coordination by ensuring that the victims (foreign manufacturers) experience genuine extraction due to market barriers and exclusion. The standard prevents classifying a snare as a mountain by highlighting the constructed and enforced nature of the technological standard, rather than a natural law of transportation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    global_adoption_viability,
    'Will the Chinese vactrain standard become a globally adopted standard or remain a national standard?',
    'Analysis of international collaborations, technology transfer agreements, and competitive standards development.',
    'Global adoption would solidify China''s technological dominance. Limited adoption would diminish the standard''s long-term economic benefits.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(global_adoption_viability, empirical, 'Viability of global adoption of the Chinese vactrain standard').

omega_variable(
    technological_lockin_potential,
    'Does the standard create technological lock-in, hindering future innovation and alternative technologies?',
    'Assessment of the standard''s flexibility, adaptability, and compatibility with future technologies.',
    'High lock-in could stifle innovation. Flexible standards could encourage further development and integration of new technologies.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technological_lockin_potential, conceptual, 'Potential for technological lock-in due to the standard').

omega_variable(
    alternative_transport_resilience,
    'Can alternative transport technologies like Hyperloop, or advanced air travel successfully compete with and erode the benefits of the Chinese Vactrain standard over a longer time horizon?',
    'Comparative cost-benefit analyses, performance benchmarks, and investment trends in alternative transportation technologies.',
    'Erosion of benefits would shift the standard''s classification more towards a piton. Continued dominance would reinforce the tangled rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_transport_resilience, empirical, 'Resilience of competing transport technologies').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(china_vactrain_standard, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(chin_tr_t0, china_vactrain_standard, theater_ratio, 0, 0.55).
narrative_ontology:measurement(chin_tr_t5, china_vactrain_standard, theater_ratio, 5, 0.65).
narrative_ontology:measurement(chin_tr_t10, china_vactrain_standard, theater_ratio, 10, 0.75).

% Extraction over time
narrative_ontology:measurement(chin_be_t0, china_vactrain_standard, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(chin_be_t5, china_vactrain_standard, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(chin_be_t10, china_vactrain_standard, base_extractiveness, 10, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(china_vactrain_standard, resource_allocation).
narrative_ontology:affects_constraint(china_vactrain_standard, chinese_semiconductor_policy).
narrative_ontology:affects_constraint(china_vactrain_standard, rare_earth_supply_chains).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
