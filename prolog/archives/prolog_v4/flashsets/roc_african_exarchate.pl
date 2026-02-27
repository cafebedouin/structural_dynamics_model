% ============================================================================
% CONSTRAINT STORY: roc_african_exarchate
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_roc_african_exarchate, []).

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
 *   constraint_id: roc_african_exarchate
 *   human_readable: Russian Orthodox Church's African Exarchate as a Geopolitical Tool
 *   domain: geopolitical/religious
 *
 * SUMMARY:
 *   This constraint describes the use of the Russian Orthodox Church (ROC),
 *   specifically its African Exarchate, as an instrument of Russian state
 *   soft power. The ROC, with the backing of the Russian state, seeks to
 *   expand its influence in Africa, often at the expense of existing Orthodox
 *   churches under the Ecumenical Patriarchate of Constantinople. This
 *   expansion involves a mixture of genuine religious outreach, political
 *   maneuvering, and the projection of Russian geopolitical interests.
 *
 * KEY AGENTS:
 *   - Russian State: Primary beneficiary (institutional/arbitrage) - gains geopolitical influence and soft power.
 *   - ROC Leadership: Secondary beneficiary (powerful/constrained) - gains power and resources but is constrained by reliance on the Russian State.
 *   - African Orthodox Churches: Primary victim (powerless/trapped) - targeted for conversion and subject to ROC influence.
 *   - Ecumenical Patriarchate of Constantinople: Secondary victim (moderate/constrained) - sees its authority undermined.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(roc_african_exarchate, 0.65).
domain_priors:suppression_score(roc_african_exarchate, 0.7).
domain_priors:theater_ratio(roc_african_exarchate, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(roc_african_exarchate, extractiveness, 0.65).
narrative_ontology:constraint_metric(roc_african_exarchate, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(roc_african_exarchate, theater_ratio, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(roc_african_exarchate, tangled_rope).
narrative_ontology:human_readable(roc_african_exarchate, "Russian Orthodox Church's African Exarchate as a Geopolitical Tool").
narrative_ontology:topic_domain(roc_african_exarchate, "geopolitical/religious").

domain_priors:requires_active_enforcement(roc_african_exarchate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(roc_african_exarchate, russian_state).
narrative_ontology:constraint_beneficiary(roc_african_exarchate, roc_leadership).
narrative_ontology:constraint_victim(roc_african_exarchate, african_orthodox_churches).
narrative_ontology:constraint_victim(roc_african_exarchate, ecumenical_patriarchate_of_constantinople).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% The African Orthodox churches are targeted for conversion and are largely trapped due to lack of resources and alternative support. They experience high extraction and limited options.
constraint_indexing:constraint_classification(roc_african_exarchate, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(continental))).

% The Ecumenical Patriarchate of Constantinople, as the traditional head of Orthodox Christianity, is constrained by the ROC's actions and sees its authority undermined. While not entirely trapped, its options are limited due to political considerations and the risk of schism.
constraint_indexing:constraint_classification(roc_african_exarchate, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% The Russian state benefits from the increased influence and soft power projection afforded by the ROC's expansion in Africa. It experiences this as a coordination mechanism to further its geopolitical aims.
constraint_indexing:constraint_classification(roc_african_exarchate, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% ROC leadership benefits from increased power and resources but is constrained by its reliance on the Russian state and potential reputational damage.
constraint_indexing:constraint_classification(roc_african_exarchate, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% An analytical observer sees the ROC's African Exarchate as a tangled rope, exhibiting both coordination (religious outreach, community building) and extraction (undermining competing orthodox authorities, promoting russian state interests). The relative balance shifts over time depending on political conditions.
constraint_indexing:constraint_classification(roc_african_exarchate, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(roc_african_exarchate_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(roc_african_exarchate, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(roc_african_exarchate, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(roc_african_exarchate, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(roc_african_exarchate_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) is moderate-high as the ROC extracts allegiance and resources from African churches and undermines the authority of the Ecumenical Patriarchate. The suppression (0.70) is high due to the ROC's backing by the Russian state and limited exit options for African churches. The theater ratio (0.40) is moderate as there is some genuine religious activity alongside the geopolitical maneuvering.
 *
 * PERSPECTIVAL GAP:
 *   The African Orthodox Churches experience the ROC's expansion as a snare, with limited exit options and high extraction. The Ecumenical Patriarchate experiences it as a constrained situation where their authority is being challenged. The Russian state views it as a rope, a tool for furthering its geopolitical interests. The ROC leadership is in a complex situation, benefiting from the expansion but also constrained by its dependence on the Russian state.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality is derived from the structural relationships between the agents. The Russian state and ROC leadership benefit from the constraint, while the African Orthodox Churches and Ecumenical Patriarchate are targeted. The exit options also influence the directionality, with trapped agents experiencing higher extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The ROC's activity could be misconstrued as purely religious (a Rope), but the political and power dynamics reveal the Tangled Rope nature of the constraint. The asymmetrical power relationship and the undermining of the Ecumenical Patriarchate's authority are key indicators that this is not simply a matter of religious coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    degree_of_genuine_faith,
    'To what extent is the ROC''s expansion driven by genuine religious conviction versus geopolitical strategy?',
    'Analysis of internal ROC documents and communications; surveys of clergy and laity; observation of actual activities.',
    'If driven by faith, the constraint is closer to a rope; if driven by geopolitics, the constraint is closer to a snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(degree_of_genuine_faith, empirical, 'Degree of genuine faith vs. geopolitical strategy').

omega_variable(
    african_agency,
    'To what extent do African Orthodox communities genuinely embrace the ROC versus being coerced or manipulated?',
    'Surveys and interviews with African Orthodox leaders and congregants; analysis of local power dynamics.',
    'If genuine embrace, the constraint is closer to a rope; if coercion, the constraint is closer to a snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(african_agency, empirical, 'Degree of African agency and genuine embrace').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(roc_african_exarchate, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(roc__tr_t0, roc_african_exarchate, theater_ratio, 0, 0.2).
narrative_ontology:measurement(roc__tr_t5, roc_african_exarchate, theater_ratio, 5, 0.3).
narrative_ontology:measurement(roc__tr_t10, roc_african_exarchate, theater_ratio, 10, 0.4).

% Extraction over time
narrative_ontology:measurement(roc__be_t0, roc_african_exarchate, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(roc__be_t5, roc_african_exarchate, base_extractiveness, 5, 0.6).
narrative_ontology:measurement(roc__be_t10, roc_african_exarchate, base_extractiveness, 10, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(roc_african_exarchate, enforcement_mechanism).
narrative_ontology:affects_constraint(roc_african_exarchate, russian_interference_africa).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
