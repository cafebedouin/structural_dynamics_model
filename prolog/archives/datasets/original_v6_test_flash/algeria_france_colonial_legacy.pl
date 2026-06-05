% ============================================================================
% CONSTRAINT STORY: algeria_france_colonial_legacy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_algeria_france_colonial_legacy, []).

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
 *   constraint_id: algeria_france_colonial_legacy
 *   human_readable: The persistent structural legacy of French colonization in Algeria
 *   domain: geopolitical/economic
 *
 * SUMMARY:
 *   This constraint represents the enduring economic, political, and cultural
 *   effects of France's 132-year colonization of Algeria (1830-1962). The
 *   legacy manifests as resource dependency, political fragmentation, and
 *   cultural suppression, creating a persistent power imbalance between
 *   Algeria and France. The constraint is viewed differently from the
 *   perspective of Algerian and French actors, as well as an analytical
 *   observer.
 *
 * KEY AGENTS:
 *   - Algerian Economy: Primary victim (powerless/trapped) - bears the brunt of colonial economic structures and neo-colonial exploitation.
 *   - Algerian Political Institutions: Secondary victim (moderate/constrained) - struggles with fragmentation and external influence.
 *   - French Corporations: Primary beneficiary (institutional/arbitrage) - profits from preferential access and neo-colonial exploitation.
 *   - French Government: Secondary beneficiary (institutional/constrained) - maintains influence but faces historical responsibility.
 *   - Analytical Observer: Assesses the overall impact from a civilizational perspective.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(algeria_france_colonial_legacy, 0.65).
domain_priors:suppression_score(algeria_france_colonial_legacy, 0.7).
domain_priors:theater_ratio(algeria_france_colonial_legacy, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(algeria_france_colonial_legacy, extractiveness, 0.65).
narrative_ontology:constraint_metric(algeria_france_colonial_legacy, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(algeria_france_colonial_legacy, theater_ratio, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(algeria_france_colonial_legacy, tangled_rope).
narrative_ontology:human_readable(algeria_france_colonial_legacy, "The persistent structural legacy of French colonization in Algeria").
narrative_ontology:topic_domain(algeria_france_colonial_legacy, "geopolitical/economic").

domain_priors:requires_active_enforcement(algeria_france_colonial_legacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(algeria_france_colonial_legacy, french_corporations).
narrative_ontology:constraint_beneficiary(algeria_france_colonial_legacy, french_government).
narrative_ontology:constraint_victim(algeria_france_colonial_legacy, algerian_economy).
narrative_ontology:constraint_victim(algeria_france_colonial_legacy, algerian_political_institutions).
narrative_ontology:constraint_victim(algeria_france_colonial_legacy, algerian_culture).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% The Algerian economy is trapped by the legacy of colonial economic structures, resource extraction patterns, and dependence on former colonizer. Limited exit options due to historical path dependency.
constraint_indexing:constraint_classification(algeria_france_colonial_legacy, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% Algerian political institutions are constrained by the legacy of colonial administration, political fragmentation, and neo-colonial influence. Some benefit from ties with France, but overall experience extraction.
constraint_indexing:constraint_classification(algeria_france_colonial_legacy, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% French corporations benefit from preferential access to Algerian markets, resource extraction, and investment opportunities due to historical ties and neo-colonial influence. They experience this legacy as a rope, enabling coordination.
constraint_indexing:constraint_classification(algeria_france_colonial_legacy, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% The French government benefits from maintaining political and economic influence in Algeria, but is also constrained by historical responsibility and the need to manage relations with its former colony. This is a tangled rope due to the complexities of post-colonial power dynamics.
constraint_indexing:constraint_classification(algeria_france_colonial_legacy, tangled_rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(continental))).

% From a civilizational perspective, the legacy of French colonization is a tangled rope characterized by enduring power imbalances, economic exploitation, and cultural suppression. Requires active enforcement through neo-colonial mechanisms.
constraint_indexing:constraint_classification(algeria_france_colonial_legacy, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(algeria_france_colonial_legacy_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(algeria_france_colonial_legacy, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(algeria_france_colonial_legacy, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(algeria_france_colonial_legacy, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(algeria_france_colonial_legacy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness: High (0.65) - reflects the ongoing economic exploitation and resource extraction facilitated by historical ties and neo-colonial influence. Suppression: High (0.70) - reflects the limited agency of Algerian actors due to historical path dependency and external influence. Theater Ratio: Moderate (0.40) - some performative aspects in international relations and development aid, but the underlying power imbalance remains.
 *
 * PERSPECTIVAL GAP:
 *   The Algerian economy and political institutions experience the colonial legacy as a snare, trapping them in a cycle of dependency and exploitation. French corporations, on the other hand, view the legacy as a rope, facilitating preferential access and economic opportunities. The French government experiences a tangled rope, balancing the benefits of influence with the constraints of historical responsibility.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality is derived from the structural relationships between Algeria and France. Algerian actors are primarily victims, experiencing high extraction and limited agency. French actors are primarily beneficiaries, profiting from historical ties and neo-colonial influence. The analytical observer captures the overall power imbalance and enduring impact of colonization.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as a tangled rope resolves the mandatrophy by acknowledging both the coordination and extraction aspects of the colonial legacy. While some coordination may exist (e.g., trade agreements, development aid), it is overshadowed by the asymmetric power dynamics and ongoing exploitation. The structural data confirms the dominance of extraction over genuine coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    resource_dependency_diversification,
    'Can Algeria diversify its economy away from resource dependency inherited from colonial extraction?',
    'Analysis of Algerian economic policies, investment patterns, and trade relationships.',
    'Successful diversification would weaken the snare effect. Failure would perpetuate colonial economic structures.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(resource_dependency_diversification, empirical, 'Algeria''s ability to diversify its economy.').

omega_variable(
    political_institutional_reform,
    'Can Algerian political institutions overcome fragmentation and neo-colonial influence to achieve genuine sovereignty?',
    'Assessment of political reforms, electoral processes, and civil society development in Algeria.',
    'Successful reform would strengthen Algerian agency and reduce external extraction. Failure would reinforce the tangled rope dynamic.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(political_institutional_reform, conceptual, 'Algeria''s capacity for political reform.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(algeria_france_colonial_legacy, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(alge_tr_t0, algeria_france_colonial_legacy, theater_ratio, 0, 0.3).
narrative_ontology:measurement(alge_tr_t30, algeria_france_colonial_legacy, theater_ratio, 30, 0.4).
narrative_ontology:measurement(alge_tr_t60, algeria_france_colonial_legacy, theater_ratio, 60, 0.45).

% Extraction over time
narrative_ontology:measurement(alge_be_t0, algeria_france_colonial_legacy, base_extractiveness, 0, 0.7).
narrative_ontology:measurement(alge_be_t30, algeria_france_colonial_legacy, base_extractiveness, 30, 0.65).
narrative_ontology:measurement(alge_be_t60, algeria_france_colonial_legacy, base_extractiveness, 60, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(algeria_france_colonial_legacy, resource_allocation).
narrative_ontology:affects_constraint(algeria_france_colonial_legacy, french_foreign_policy).
narrative_ontology:affects_constraint(algeria_france_colonial_legacy, algerian_political_instability).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
