% ============================================================================
% CONSTRAINT STORY: us_sanctions_icc_israel_case
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_sanctions_icc_israel_case, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: us_sanctions_icc_israel_case
 *   human_readable: US Sanctions Threat Against ICC Officials Investigating Israel
 *   domain: political
 *
 * SUMMARY:
 *   A bipartisan bill in the US Senate proposes sanctions (visa bans, asset
 *   freezes) against officials of the International Criminal Court (ICC)
 *   involved in prosecuting Israeli nationals for alleged war crimes. This
 *   action sets a precedent that could undermine the ICC's ability to
 *   function effectively, impacting the international rule of law. The US
 *   threat reveals how political and strategic interests impact the
 *   international legal system and its capacity to address war crimes.
 *
 * KEY AGENTS:
 *   - Israeli Officials: Primary beneficiary (institutional/arbitrage) - Gains protection from ICC investigation
 *   - US Political Establishment: Secondary beneficiary (institutional/constrained) - Reinforces alliance with Israel, asserts influence in international law
 *   - ICC Officials: Primary victim (moderate/constrained) - Faces sanctions threat, undermining legal mandate
 *   - International Rule of Law: Ultimate victim (powerless/trapped) - Erosion of norms, sets a dangerous precedent
 *   - Analytical Observer: Analyzes the long-term impacts and geopolitical implications (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_sanctions_icc_israel_case, 0.65).
domain_priors:suppression_score(us_sanctions_icc_israel_case, 0.75).
domain_priors:theater_ratio(us_sanctions_icc_israel_case, 0.75).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_sanctions_icc_israel_case, extractiveness, 0.65).
narrative_ontology:constraint_metric(us_sanctions_icc_israel_case, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(us_sanctions_icc_israel_case, theater_ratio, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_sanctions_icc_israel_case, tangled_rope).
narrative_ontology:human_readable(us_sanctions_icc_israel_case, "US Sanctions Threat Against ICC Officials Investigating Israel").
narrative_ontology:topic_domain(us_sanctions_icc_israel_case, "political").

domain_priors:requires_active_enforcement(us_sanctions_icc_israel_case).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_sanctions_icc_israel_case, israeli_officials).
narrative_ontology:constraint_beneficiary(us_sanctions_icc_israel_case, us_political_establishment).
narrative_ontology:constraint_victim(us_sanctions_icc_israel_case, icc_officials).
narrative_ontology:constraint_victim(us_sanctions_icc_israel_case, international_rule_of_law).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% The international rule of law is trapped as it lacks enforcement mechanisms against powerful states. The threat of sanctions severely undermines the court's ability to hold powerful actors accountable, setting a precedent that weakens the entire system.
constraint_indexing:constraint_classification(us_sanctions_icc_israel_case, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% ICC officials are constrained. They face potential sanctions (visa bans, asset freezes) if they pursue investigations against Israeli nationals. However, they also have a professional obligation and legal mandate to pursue justice impartially. They experience both extraction (threat of sanctions) and coordination (fulfilling their legal mandate, promoting justice, potentially garnering support within the international legal community).
constraint_indexing:constraint_classification(us_sanctions_icc_israel_case, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% Israeli officials benefit from the sanctions threat. It provides them with a degree of protection from potential prosecution by the ICC, thus enabling them to pursue their national security objectives without fear of legal repercussions. Exit option: leverage US political establishment to circumvent potential ICC actions.
constraint_indexing:constraint_classification(us_sanctions_icc_israel_case, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% The US political establishment might see the ICC as an institution that has atrophied in its original purpose (preventing aggression and war crimes) and has become a tool to exert political pressure. The threat of sanctions may be a performative action (high theater) with a limited enforcement effect. Due to the lack of consequences to the action, the constraint is a piton.
constraint_indexing:constraint_classification(us_sanctions_icc_israel_case, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% An analytical observer sees the sanctions threat as a form of tangled rope: The US uses its power to influence international legal norms and institutions to protect its allies and its own interests (extraction). However, there is also a coordination element: The US seeks to maintain its influence in the international arena and uphold what it perceives as a just international order (coordination). This is a mixed motive.
constraint_indexing:constraint_classification(us_sanctions_icc_israel_case, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_sanctions_icc_israel_case_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(us_sanctions_icc_israel_case, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(us_sanctions_icc_israel_case, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_sanctions_icc_israel_case, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(us_sanctions_icc_israel_case, TR),
    TR >= 0.70.

:- end_tests(us_sanctions_icc_israel_case_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness: (0.65): High. The sanctions threat significantly extracts from the ICC's ability to function impartially and effectively. It deters investigations and prosecutions, leading to a direct impairment of the court's capacity to address war crimes. Suppression: (0.75): High. The sanctions threat suppresses the ICC's ability to pursue justice. It also undermines the ICC's reputation and legitimacy, discouraging other states from cooperating with the court. Theater ratio (0.75): High. The sanctions threat has real potential for actual implementation and the ability to cause real consequences. The consequences of the actions will have an effect over a long term.
 *
 * PERSPECTIVAL GAP:
 *   The ICC officials see the sanctions threat as undermining their ability to pursue justice impartially (Tangled Rope). Israeli officials benefit from the sanctions threat, which provides a degree of protection from potential prosecution by the ICC (Rope). The US political establishment might see the action as a way to assert its influence in the international arena and protect its allies (Piton). The international rule of law views the sanctions threat as a dangerous precedent, eroding international legal norms and institutions (Snare).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from the beneficiary/victim status and exit options of the key agents. Israeli officials (beneficiaries with arbitrage) experience negative extraction. ICC officials (victims with constrained exit options) experience high extraction. The international rule of law (victim with trapped exit) experiences maximal extraction.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    us_commitment_to_international_law,
    'How committed is the US to the international rule of law when it conflicts with its national interests or the interests of its allies?',
    'Track the US''s behavior in similar situations involving other countries and international institutions, analyze official statements and policy documents, and assess the level of domestic political support for international law.',
    'If the US is highly committed, the sanctions threat is a temporary aberration. If the US is not committed, the threat is a signal of a broader shift away from international law.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(us_commitment_to_international_law, empirical, 'The degree of commitment of the US to international law.').

omega_variable(
    icc_effectiveness_without_us_support,
    'How effective can the ICC be in prosecuting war crimes and crimes against humanity without the support of major powers like the US?',
    'Examine the ICC''s track record in prosecuting cases involving nationals of states that are not parties to the Rome Statute, assess the level of cooperation the ICC receives from other states, and analyze the financial and political resources available to the ICC.',
    'If the ICC can be effective, the sanctions threat has a limited impact. If the ICC cannot be effective, the sanctions threat severely undermines its ability to function.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(icc_effectiveness_without_us_support, empirical, 'The degree of ICC effectiveness without the support of major powers such as the US.').

omega_variable(
    us_domestic_political_cost_of_sanctions,
    'What are the domestic political costs for the US in imposing sanctions on ICC officials?',
    'Assess public opinion through polls, analyze media coverage and academic commentary, and monitor the level of opposition from domestic political actors and civil society organizations.',
    'If the political costs are high, the US may be forced to reconsider its policy. If the political costs are low, the US may be emboldened to take further action.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(us_domestic_political_cost_of_sanctions, empirical, 'The degree of domestic political costs for the US in imposing sanctions on ICC officials.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_sanctions_icc_israel_case, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_s_tr_t0, us_sanctions_icc_israel_case, theater_ratio, 0, 0.1).
narrative_ontology:measurement(us_s_tr_t5, us_sanctions_icc_israel_case, theater_ratio, 5, 0.4).
narrative_ontology:measurement(us_s_tr_t10, us_sanctions_icc_israel_case, theater_ratio, 10, 0.75).

% Extraction over time
narrative_ontology:measurement(us_s_be_t0, us_sanctions_icc_israel_case, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(us_s_be_t5, us_sanctions_icc_israel_case, base_extractiveness, 5, 0.6).
narrative_ontology:measurement(us_s_be_t10, us_sanctions_icc_israel_case, base_extractiveness, 10, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(us_sanctions_icc_israel_case, international_court_credibility).
narrative_ontology:affects_constraint(us_sanctions_icc_israel_case, us_foreign_policy_consistency).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
