% ============================================================================
% CONSTRAINT STORY: hagia_sophia_status
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-08
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hagia_sophia_status, []).

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
 *   constraint_id: hagia_sophia_status
 *   human_readable: The enforced religious and political status of the Hagia Sophia
 *   domain: religious/political
 *
 * SUMMARY:
 *   The enforced religious and political status of the Hagia Sophia is a
 *   contested issue reflecting Turkey's internal political dynamics and its
 *   relations with the broader world. Its changing roles from cathedral to
 *   mosque to museum and back to a mosque represent the shifting power
 *   dynamics and ideological orientations of the Turkish state. The
 *   constraint has a strong theatrical component as the act is used to
 *   generate support domestically and signal intentions internationally.
 *
 * KEY AGENTS:
 *   - Turkish Government: Primary beneficiary (institutional/arbitrage) - Gaining political capital.
 *   - Sunni Muslim Community: Beneficiary (organized/constrained) - Gaining symbolic validation.
 *   - Orthodox Christians: Primary victim (powerless/trapped) - Losing a significant religious site.
 *   - Turkish Secularists: Victim (moderate/constrained) - Seeing secular principles eroded.
 *   - Historical Preservation: Victim (powerless/trapped) - The site's historical significance is downgraded and some artifacts covered or removed.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hagia_sophia_status, 0.65).
domain_priors:suppression_score(hagia_sophia_status, 0.7).
domain_priors:theater_ratio(hagia_sophia_status, 0.8).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hagia_sophia_status, extractiveness, 0.65).
narrative_ontology:constraint_metric(hagia_sophia_status, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(hagia_sophia_status, theater_ratio, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hagia_sophia_status, snare).
narrative_ontology:human_readable(hagia_sophia_status, "The enforced religious and political status of the Hagia Sophia").
narrative_ontology:topic_domain(hagia_sophia_status, "religious/political").

domain_priors:requires_active_enforcement(hagia_sophia_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hagia_sophia_status, turkish_government).
narrative_ontology:constraint_beneficiary(hagia_sophia_status, sunni_muslim_community).
narrative_ontology:constraint_victim(hagia_sophia_status, orthodox_christians).
narrative_ontology:constraint_victim(hagia_sophia_status, secularists).
narrative_ontology:constraint_victim(hagia_sophia_status, historical_preservation).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Perspective 1: Orthodox Christians view the conversion of Hagia Sophia into a mosque as a loss of a significant historical and religious site, limiting their access and influence.
constraint_indexing:constraint_classification(hagia_sophia_status, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% Perspective 2: Turkish secularists, while constrained in their ability to directly influence the decision, see the shift in status as a degradation of secular principles, with little practical alternative.
constraint_indexing:constraint_classification(hagia_sophia_status, piton,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% Perspective 3: The Turkish government benefits politically from the conversion, reinforcing its nationalistic agenda and consolidating support among its base.
constraint_indexing:constraint_classification(hagia_sophia_status, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% Perspective 4: Sunni Muslim Community benefits in the sense of religious victory but is also constrained by the expectation to respect the site and maintain it appropriately, thus a tangled rope.
constraint_indexing:constraint_classification(hagia_sophia_status, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% Perspective 5: Those focused on historical preservation are greatly affected, with little to no power or option to alter the decision.
constraint_indexing:constraint_classification(hagia_sophia_status, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% Analytical Perspective: An analytical observer sees the overall situation as a Tangled Rope, balancing the extraction from some communities with the coordination for others, and noting an increase in theater.
constraint_indexing:constraint_classification(hagia_sophia_status, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hagia_sophia_status_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(hagia_sophia_status, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(hagia_sophia_status, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(hagia_sophia_status, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(hagia_sophia_status, TR),
    TR >= 0.70.

:- end_tests(hagia_sophia_status_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.65) is relatively high, as many groups are negatively impacted. Suppression (0.7) is also high, since the government has restricted alternatives. The theater ratio (0.8) is high since the government uses the status as a symbol to increase political support.
 *
 * PERSPECTIVAL GAP:
 *   The perspectives vary significantly based on each group's values, power, and exit options. While the Turkish government sees political benefits and consolidation of power, the Orthodox Christians see the loss of a sacred site, and secularists lament the erosion of secular principles. The analytical observer sees the overall situation as a Tangled Rope, balancing the extraction from some communities with the coordination for others.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality logic is derived from the beneficiaries and victims declared above and their exit options. Those who benefit from the new status and have arbitrage options (the Turkish government) have a low d-value. Those who lose due to the decision and are trapped have a high d-value (Orthodox Christians, preservationists). Secularists, who are constrained but not entirely trapped, have a moderate d-value. The Sunni community is moderately benefitted due to religious victory.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    extent_of_religious_freedom,
    'To what extent is religious freedom genuinely protected for all groups in Turkey?',
    'Monitoring legal and social practices affecting religious minorities.',
    'Determines whether the enforcement is coordination or pure extraction. High religious freedom implies the constraint is coordination (lower suppression).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extent_of_religious_freedom, empirical, 'Assesses the breadth and depth of religious freedom in Turkey.').

omega_variable(
    political_stability,
    'How stable is the current political regime in Turkey?',
    'Observing election outcomes, policy changes, and public opinion.',
    'Instability can lead to greater extraction/suppression and a regime that is less tolerant of dissent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(political_stability, empirical, 'Measures the stability of the political environment.').

omega_variable(
    international_relations,
    'What is the current state of Turkey''s relationship with other nations, especially Greece and other EU members?',
    'Analyzing diplomatic interactions and public statements.',
    'Strained relations can lead to higher suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(international_relations, empirical, 'Assesses Turkey''s standing in global politics.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hagia_sophia_status, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hagi_tr_t0, hagia_sophia_status, theater_ratio, 0, 0.3).
narrative_ontology:measurement(hagi_tr_t5, hagia_sophia_status, theater_ratio, 5, 0.6).
narrative_ontology:measurement(hagi_tr_t10, hagia_sophia_status, theater_ratio, 10, 0.8).

% Extraction over time
narrative_ontology:measurement(hagi_be_t0, hagia_sophia_status, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(hagi_be_t5, hagia_sophia_status, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(hagi_be_t10, hagia_sophia_status, base_extractiveness, 10, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hagia_sophia_status, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
