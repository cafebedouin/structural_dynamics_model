% ============================================================================
% CONSTRAINT STORY: eu_asylum_outsourcing_framework
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-08-22
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_eu_asylum_outsourcing_framework, []).

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
 *   constraint_id: eu_asylum_outsourcing_framework
 *   human_readable: EU framework for outsourcing asylum procedures to third countries
 *   domain: political
 *
 * SUMMARY:
 *   The EU framework for outsourcing asylum procedures to third countries is
 *   a complex policy with both potential benefits and risks. It aims to
 *   reduce pressure on EU asylum systems and deter irregular migration but
 *   raises concerns about human rights, access to legal recourse, and the
 *   stability of outsourcing countries. The framework's impact depends on how
 *   effectively human rights are monitored and protected in these countries.
 *
 * KEY AGENTS:
 *   - Asylum Seekers: Primary target (powerless/trapped)
 *   - EU Member States: Primary beneficiary (institutional/arbitrage)
 *   - Human Rights Organizations: Watchdog/Advocate (moderate/constrained)
 *   - Border Control Industry: Beneficiary (powerful/mobile)
 *   - International Law: Undermined (institutional/constrained)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(eu_asylum_outsourcing_framework, 0.65).
domain_priors:suppression_score(eu_asylum_outsourcing_framework, 0.75).
domain_priors:theater_ratio(eu_asylum_outsourcing_framework, 0.7).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(eu_asylum_outsourcing_framework, extractiveness, 0.65).
narrative_ontology:constraint_metric(eu_asylum_outsourcing_framework, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(eu_asylum_outsourcing_framework, theater_ratio, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(eu_asylum_outsourcing_framework, tangled_rope).
narrative_ontology:human_readable(eu_asylum_outsourcing_framework, "EU framework for outsourcing asylum procedures to third countries").
narrative_ontology:topic_domain(eu_asylum_outsourcing_framework, "political").

domain_priors:requires_active_enforcement(eu_asylum_outsourcing_framework).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(eu_asylum_outsourcing_framework, eu_member_states).
narrative_ontology:constraint_beneficiary(eu_asylum_outsourcing_framework, border_control_industry).
narrative_ontology:constraint_victim(eu_asylum_outsourcing_framework, asylum_seekers).
narrative_ontology:constraint_victim(eu_asylum_outsourcing_framework, human_rights_organizations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Asylum seekers are trapped within the system, facing potential rejection, human rights abuses, and limited access to legal recourse.
constraint_indexing:constraint_classification(eu_asylum_outsourcing_framework, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% Human rights organizations are constrained by limited resources and political influence but can still advocate for asylum seekers' rights and monitor conditions in outsourcing countries. They benefit from increased scrutiny but are hampered by the policy itself.
constraint_indexing:constraint_classification(eu_asylum_outsourcing_framework, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% EU member states benefit from reduced pressure on their asylum systems and potentially lower costs but also face reputational risks and legal challenges. They arbitrage the system for political expediency.
constraint_indexing:constraint_classification(eu_asylum_outsourcing_framework, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(continental))).

% International law principles, such as the right to asylum and non-refoulement, are weakened and undermined by the framework, becoming performative rather than functional.
constraint_indexing:constraint_classification(eu_asylum_outsourcing_framework, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% The border control industry benefits financially from the framework, receiving contracts for managing asylum processing centers in third countries but faces some public scrutiny. It benefits from increased activity but is mobile - can shift focus if politically unfavorable.
constraint_indexing:constraint_classification(eu_asylum_outsourcing_framework, tangled_rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(global))).

% The analytical observer recognizes both the coordination function (reducing pressure on EU systems) and the extractive nature (potential for human rights abuses and undermining international law) of the framework.
constraint_indexing:constraint_classification(eu_asylum_outsourcing_framework, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(eu_asylum_outsourcing_framework_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(eu_asylum_outsourcing_framework, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(eu_asylum_outsourcing_framework, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(eu_asylum_outsourcing_framework, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(eu_asylum_outsourcing_framework, TR),
    TR >= 0.70.

:- end_tests(eu_asylum_outsourcing_framework_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.65): High. Asylum seekers face significant risks of rejection, human rights abuses, and limited access to legal recourse. EU member states benefit from reduced pressure on their asylum systems, while border control industries profit financially. Suppression (0.75): High. The framework restricts asylum seekers' options and limits their ability to seek protection within the EU. It also suppresses dissenting voices from human rights organizations and challenges international law principles. Theater ratio (0.70): High. While the framework includes some provisions for monitoring and oversight, there is a risk of these being performative rather than effective in preventing abuses. The theater has increased over the interval as the gap between stated goals and actual outcomes widens.
 *
 * PERSPECTIVAL GAP:
 *   Asylum seekers experience the framework as a snare, facing significant risks and limited options. EU member states see it as a rope, easing pressure on their asylum systems. Human rights organizations view it as a tangled rope, constrained by limited resources but still able to advocate for asylum seekers' rights. The analytical observer recognizes the mixed coordination and extraction of the framework.
 *
 * DIRECTIONALITY LOGIC:
 *   Asylum seekers, being the primary targets with limited exit options, have a high directionality value (close to 1), experiencing significant extraction. EU member states, benefiting from reduced pressure on their asylum systems and having alternative policy options, have a low directionality value (close to 0), experiencing coordination benefits. Human rights organizations, constrained but not entirely powerless, have a moderate directionality value.
 *
 * MANDATROPHY ANALYSIS:
 *   The framework is classified as a tangled rope because it exhibits both coordination and extraction. It coordinates efforts to manage migration flows but also extracts rights and protections from asylum seekers. Resolving the mandatrophy requires careful monitoring of human rights and access to legal recourse in outsourcing countries to ensure that the framework does not become a pure snare. The high extractiveness and suppression, combined with a rising theater ratio, indicate a degradation of the framework over time, moving it closer to a pure snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    human_rights_monitoring_effectiveness,
    'How effectively can human rights abuses in outsourcing countries be monitored and addressed?',
    'Independent investigations and reporting by human rights organizations and international bodies.',
    'If effective: framework is less extractive. If ineffective: framework becomes a pure snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(human_rights_monitoring_effectiveness, empirical, 'Effectiveness of human rights monitoring in outsourcing countries.').

omega_variable(
    legal_recourse_accessibility,
    'To what extent can asylum seekers access legal recourse and challenge decisions made in outsourcing countries?',
    'Analysis of legal frameworks and access to legal representation in outsourcing countries.',
    'If accessible: framework is less extractive. If inaccessible: framework becomes a pure snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legal_recourse_accessibility, empirical, 'Accessibility of legal recourse for asylum seekers.').

omega_variable(
    third_country_stability,
    'How stable and safe are the third countries to which asylum procedures are outsourced?',
    'Assessment of political stability, security situation, and human rights record in outsourcing countries.',
    'If stable and safe: framework is less extractive. If unstable and unsafe: framework becomes a pure snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(third_country_stability, empirical, 'Stability and safety of third countries.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(eu_asylum_outsourcing_framework, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eu_a_tr_t0, eu_asylum_outsourcing_framework, theater_ratio, 0, 0.3).
narrative_ontology:measurement(eu_a_tr_t5, eu_asylum_outsourcing_framework, theater_ratio, 5, 0.5).
narrative_ontology:measurement(eu_a_tr_t10, eu_asylum_outsourcing_framework, theater_ratio, 10, 0.7).

% Extraction over time
narrative_ontology:measurement(eu_a_be_t0, eu_asylum_outsourcing_framework, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(eu_a_be_t5, eu_asylum_outsourcing_framework, base_extractiveness, 5, 0.6).
narrative_ontology:measurement(eu_a_be_t10, eu_asylum_outsourcing_framework, base_extractiveness, 10, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(eu_asylum_outsourcing_framework, enforcement_mechanism).
narrative_ontology:affects_constraint(eu_asylum_outsourcing_framework, schengen_agreement).
narrative_ontology:affects_constraint(eu_asylum_outsourcing_framework, dublin_regulation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
