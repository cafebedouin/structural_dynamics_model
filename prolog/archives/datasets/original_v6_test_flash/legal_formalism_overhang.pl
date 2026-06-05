% ============================================================================
% CONSTRAINT STORY: legal_formalism_overhang
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legal_formalism_overhang, []).

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
 *   constraint_id: legal_formalism_overhang
 *   human_readable: The Ghost of Statutes Past
 *   domain: political/legal/social
 *
 * SUMMARY:
 *   The 'Ghost of Statutes Past' describes situations where legal formalism,
 *   or strict adherence to the literal text of historical laws, persists even
 *   after the social or technological context that gave them meaning has
 *   vanished. This can lead to outcomes that are not only inefficient but
 *   also unjust, as the original intent of the law is distorted or becomes
 *   entirely irrelevant. Rent seekers and the legal establishment benefit
 *   from this situation, while social progress and future generations bear
 *   the costs. The extractiveness of the constraint increases over time as
 *   outdated laws become increasingly disconnected from the present.
 *
 * KEY AGENTS:
 *   - Future Generations: Primary victim (powerless/trapped) - Inherit and are bound by outdated laws.
 *   - Legal Establishment: Institutional actor (institutional/constrained) - Enforces laws through inertia and precedent.
 *   - Social Progress: Secondary victim (moderate/constrained) - Hindered by outdated laws that impede innovation and justice.
 *   - Rent Seekers: Primary beneficiary (organized/arbitrage) - Exploit loopholes and ambiguities for personal gain.
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) - Sees the balance between legal stability and the need for reform.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legal_formalism_overhang, 0.55).
domain_priors:suppression_score(legal_formalism_overhang, 0.6).
domain_priors:theater_ratio(legal_formalism_overhang, 0.7).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legal_formalism_overhang, extractiveness, 0.55).
narrative_ontology:constraint_metric(legal_formalism_overhang, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(legal_formalism_overhang, theater_ratio, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legal_formalism_overhang, tangled_rope).
narrative_ontology:human_readable(legal_formalism_overhang, "The Ghost of Statutes Past").
narrative_ontology:topic_domain(legal_formalism_overhang, "political/legal/social").

domain_priors:requires_active_enforcement(legal_formalism_overhang).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legal_formalism_overhang, legal_establishment).
narrative_ontology:constraint_beneficiary(legal_formalism_overhang, rent_seekers).
narrative_ontology:constraint_victim(legal_formalism_overhang, social_progress).
narrative_ontology:constraint_victim(legal_formalism_overhang, future_generations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Future generations bear the costs of outdated laws without the power to easily reform them, representing a pure extraction scenario. Trapped exit because legal precedent is path dependent. Powerless due to lack of present-day political agency.
constraint_indexing:constraint_classification(legal_formalism_overhang, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% The legal establishment, bound by precedent and tradition, often maintains these outdated laws through inertia, even when their original purpose is no longer relevant. High theater as legal formalism supersedes functional application. Constrained exit because reversing legal precedent carries significant transaction costs. Experiences the constraint as inertial (piton).
constraint_indexing:constraint_classification(legal_formalism_overhang, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% Social progress is hampered by these antiquated laws, creating a tangled rope scenario. Progress both benefits from the stability of the legal system and is extracted from by its rigidity. Constrained because overturning precedent requires collective action.
constraint_indexing:constraint_classification(legal_formalism_overhang, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% Rent seekers exploit the ambiguities and loopholes in outdated laws for personal gain, turning a relic into a resource. Arbitrage exit — rent seekers can always find a new legal loophole. Organized due to lobbying capacity and financial resources to influence legal interpretations.
constraint_indexing:constraint_classification(legal_formalism_overhang, rope,
    context(agent_power(organized),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% Analyzes the constraint as a mix of coordination (legal stability) and extraction (outdated laws hindering progress). Sees that legal formalism has both benefits and costs; extraction exceeds coordination.
constraint_indexing:constraint_classification(legal_formalism_overhang, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legal_formalism_overhang_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(legal_formalism_overhang, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(legal_formalism_overhang, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(legal_formalism_overhang, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(legal_formalism_overhang, TR),
    TR >= 0.70.

:- end_tests(legal_formalism_overhang_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55): Moderate. Outdated laws extract from social progress and future generations by hindering innovation and creating unjust outcomes. Suppression (0.60): Moderate-high. The legal system's reliance on precedent and tradition makes it difficult to reform outdated laws, suppressing alternative legal interpretations and social progress. Theater ratio (0.70): High. Legal formalism often prioritizes adherence to the letter of the law over its intended purpose, resulting in a performative adherence that does not serve the interests of justice or efficiency.
 *
 * PERSPECTIVAL GAP:
 *   The perspectives differ significantly. Future generations experience a snare, as they are trapped by laws created in a different context. The legal establishment sees a piton, a degraded institution maintained through inertia. Social progress is entangled, both benefiting from and being extracted from by the legal system. Rent seekers exploit the situation for personal gain. The analytical observer recognizes the need for balance between stability and reform.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality is derived from the structural relationships of the agents. Future generations bear the costs and have no exit, leading to a high d value. Rent seekers benefit and have arbitrage options, resulting in a low d value. The legal establishment is constrained by its institutional role, leading to a moderate d value. Social progress experiences a mixture of benefits and costs, resulting in a moderate d value for the analytical perspective.
 *
 * MANDATROPHY ANALYSIS:
 *   This is not pure extraction because the legal system provides stability and predictability, but the outdated laws lead to outcomes that are inefficient and unjust. The system also has a performative character, in that adherence to the letter of the law overtakes the need for just outcomes.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    updating_mechanism_feasibility,
    'Can a mechanism for periodically updating laws to reflect current social and technological realities be implemented without undermining the stability of the legal system?',
    'Comparative analysis of legal systems with different update mechanisms; experimentation with sunset clauses for new legislation',
    'If feasible: The constraint shifts from a tangled rope to a scaffold. If not feasible: The constraint remains a tangled rope, with the potential to degrade into a snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(updating_mechanism_feasibility, empirical, 'Feasibility of updating mechanisms').

omega_variable(
    formalism_vs_functionalism_balance,
    'What is the appropriate balance between legal formalism (strict adherence to the text of the law) and legal functionalism (interpreting laws in light of their intended purpose and current context)?',
    'Philosophical debate; case studies of legal interpretations with different degrees of formalism',
    'If formalism is prioritized: The constraint persists or worsens. If functionalism is prioritized: The constraint is mitigated, but legal stability may be compromised.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(formalism_vs_functionalism_balance, conceptual, 'Balance between formalism and functionalism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legal_formalism_overhang, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lega_tr_t0, legal_formalism_overhang, theater_ratio, 0, 0.5).
narrative_ontology:measurement(lega_tr_t50, legal_formalism_overhang, theater_ratio, 50, 0.6).
narrative_ontology:measurement(lega_tr_t100, legal_formalism_overhang, theater_ratio, 100, 0.7).

% Extraction over time
narrative_ontology:measurement(lega_be_t0, legal_formalism_overhang, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(lega_be_t50, legal_formalism_overhang, base_extractiveness, 50, 0.48).
narrative_ontology:measurement(lega_be_t100, legal_formalism_overhang, base_extractiveness, 100, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legal_formalism_overhang, enforcement_mechanism).
narrative_ontology:affects_constraint(legal_formalism_overhang, regulatory_capture).
narrative_ontology:affects_constraint(legal_formalism_overhang, path_dependency).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
