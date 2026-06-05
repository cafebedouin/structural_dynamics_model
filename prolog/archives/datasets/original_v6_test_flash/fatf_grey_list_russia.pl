% ============================================================================
% CONSTRAINT STORY: fatf_grey_list_russia
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-01
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fatf_grey_list_russia, []).

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
 *   constraint_id: fatf_grey_list_russia
 *   human_readable: FATF/EU 'Grey List' Sanction on the Russian Federation
 *   domain: geopolitical/economic
 *
 * SUMMARY:
 *   The FATF grey list sanction on Russia is a geopolitical and economic
 *   constraint designed to encourage compliance with international standards
 *   for combating money laundering, terrorist financing, and proliferation
 *   financing. It extracts costs from the Russian economy while aiming to
 *   benefit the international financial system. The effectiveness and
 *   fairness of this measure are subject to debate.
 *
 * KEY AGENTS:
 *   - FATF Member States: Primary beneficiaries (institutional/arbitrage) - gain security in international financial transactions.
 *   - Russian Economy: Primary victim (powerless/trapped) - suffers economic consequences and restricted access to finance.
 *   - Russian Financial Institutions: Constrained actor (moderate/constrained) - navigate increased scrutiny and potential reputational damage.
 *   - Legitimate Russian Businesses: Secondary victim (moderate/constrained) - face increased transaction costs and potential trade barriers.
 *   - International Financial System: Beneficiary (institutional/arbitrage) - benefits from reduced risks of illicit financial activities.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fatf_grey_list_russia, 0.6).
domain_priors:suppression_score(fatf_grey_list_russia, 0.7).
domain_priors:theater_ratio(fatf_grey_list_russia, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fatf_grey_list_russia, extractiveness, 0.6).
narrative_ontology:constraint_metric(fatf_grey_list_russia, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(fatf_grey_list_russia, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fatf_grey_list_russia, tangled_rope).
narrative_ontology:human_readable(fatf_grey_list_russia, "FATF/EU 'Grey List' Sanction on the Russian Federation").
narrative_ontology:topic_domain(fatf_grey_list_russia, "geopolitical/economic").

domain_priors:requires_active_enforcement(fatf_grey_list_russia).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fatf_grey_list_russia, fatf_member_states).
narrative_ontology:constraint_beneficiary(fatf_grey_list_russia, international_financial_system).
narrative_ontology:constraint_victim(fatf_grey_list_russia, russian_economy).
narrative_ontology:constraint_victim(fatf_grey_list_russia, russian_financial_institutions).
narrative_ontology:constraint_victim(fatf_grey_list_russia, legitimate_russian_businesses).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% The Russian economy is largely trapped and bears the brunt of increased transaction costs, reduced access to international finance, and reputational damage.
constraint_indexing:constraint_classification(fatf_grey_list_russia, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(national))).

% Russian financial institutions are constrained by the grey listing but may also benefit from increased scrutiny of illicit financial flows, potentially leveling the playing field.
constraint_indexing:constraint_classification(fatf_grey_list_russia, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% FATF member states benefit from the enhanced security of the international financial system and the reduced risk of money laundering and terrorist financing.
constraint_indexing:constraint_classification(fatf_grey_list_russia, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% From an analytical perspective, the grey listing is a tangled rope, balancing the coordination benefits of maintaining the integrity of the international financial system with the extractive costs imposed on the targeted nation.
constraint_indexing:constraint_classification(fatf_grey_list_russia, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fatf_grey_list_russia_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(fatf_grey_list_russia, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(fatf_grey_list_russia, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(fatf_grey_list_russia, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(fatf_grey_list_russia_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.60) reflects the economic costs imposed on Russia through increased scrutiny and reduced access to international finance. Suppression (0.70) represents the limited options for Russia to circumvent the sanctions. The theater ratio (0.30) is relatively low because the actions have a significant practical effect, not just performative.
 *
 * PERSPECTIVAL GAP:
 *   The Russian economy views the grey listing as a snare, as it is trapped and bears the full cost of the sanctions. FATF member states see it as a rope, coordinating global efforts to combat illicit finance. Russian financial institutions experience it as a tangled rope, facing constraints but also potentially benefiting from increased transparency.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by the agent's position relative to the extraction flow. Victims experience high directionality, beneficiaries experience low directionality, and constrained actors fall somewhere in between.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    effectiveness_of_aml_cft_measures,
    'How effective are the AML/CFT measures imposed by FATF in curbing illicit financial flows from Russia?',
    'Tracking financial flows and analyzing the impact of FATF measures on reducing illicit activities.',
    'If effective, it validates the grey listing as a useful tool. If ineffective, it raises questions about its efficacy and justification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(effectiveness_of_aml_cft_measures, empirical, 'Assessing the effectiveness of AML/CFT measures.').

omega_variable(
    unintended_consequences,
    'What are the unintended consequences of the grey listing on legitimate businesses and individuals in Russia?',
    'Conducting surveys and gathering data on the impact on legitimate economic activities.',
    'Revealing significant unintended consequences could lead to a reevaluation of the grey listing''s scope and implementation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(unintended_consequences, empirical, 'Evaluating unintended consequences on legitimate businesses and individuals.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fatf_grey_list_russia, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fatf_tr_t0, fatf_grey_list_russia, theater_ratio, 0, 0.2).
narrative_ontology:measurement(fatf_tr_t5, fatf_grey_list_russia, theater_ratio, 5, 0.3).
narrative_ontology:measurement(fatf_tr_t10, fatf_grey_list_russia, theater_ratio, 10, 0.4).

% Extraction over time
narrative_ontology:measurement(fatf_be_t0, fatf_grey_list_russia, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(fatf_be_t5, fatf_grey_list_russia, base_extractiveness, 5, 0.6).
narrative_ontology:measurement(fatf_be_t10, fatf_grey_list_russia, base_extractiveness, 10, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fatf_grey_list_russia, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
