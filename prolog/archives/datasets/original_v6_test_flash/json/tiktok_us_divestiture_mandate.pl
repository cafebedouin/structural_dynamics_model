% ============================================================================
% CONSTRAINT STORY: tiktok_us_divestiture_mandate
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-01
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_tiktok_us_divestiture_mandate, []).

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
 *   constraint_id: tiktok_us_divestiture_mandate
 *   human_readable: TikTok "Trust & Safety" Divestiture Mandate
 *   domain: geopolitical/technological/economic
 *
 * SUMMARY:
 *   The US government's mandate for ByteDance to divest TikTok's US
 *   operations stems from national security concerns related to data privacy
 *   and potential foreign influence. This action aims to protect US user data
 *   and prevent the Chinese government from accessing or manipulating it.
 *   However, the mandate also raises concerns about economic protectionism,
 *   international relations, and the long-term impact on the global
 *   technology landscape. The effectiveness of this measure depends on
 *   whether a US-controlled entity can truly guarantee data security and
 *   whether it sets a precedent for similar interventions in the future.
 *
 * KEY AGENTS:
 *   - US Government: Primary beneficiary (institutional/constrained) - seeks to protect national security and user data.
 *   - ByteDance: Primary victim (institutional/constrained) - forced to relinquish control of a valuable asset.
 *   - TikTok US Users: Secondary victim (powerless/trapped) - potential disruption of service and uncertainty regarding data privacy.
 *   - Potential US Acquirers: Secondary beneficiary (institutional/arbitrage) - opportunity to expand market share and gain access to a large user base.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tiktok_us_divestiture_mandate, 0.6).
domain_priors:suppression_score(tiktok_us_divestiture_mandate, 0.7).
domain_priors:theater_ratio(tiktok_us_divestiture_mandate, 0.75).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tiktok_us_divestiture_mandate, extractiveness, 0.6).
narrative_ontology:constraint_metric(tiktok_us_divestiture_mandate, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(tiktok_us_divestiture_mandate, theater_ratio, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tiktok_us_divestiture_mandate, tangled_rope).
narrative_ontology:human_readable(tiktok_us_divestiture_mandate, "TikTok \"Trust & Safety\" Divestiture Mandate").
narrative_ontology:topic_domain(tiktok_us_divestiture_mandate, "geopolitical/technological/economic").

domain_priors:requires_active_enforcement(tiktok_us_divestiture_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tiktok_us_divestiture_mandate, us_government).
narrative_ontology:constraint_beneficiary(tiktok_us_divestiture_mandate, potential_us_acquirers).
narrative_ontology:constraint_victim(tiktok_us_divestiture_mandate, bytedance).
narrative_ontology:constraint_victim(tiktok_us_divestiture_mandate, tiktok_us_users).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% US users have limited alternatives and may experience a degraded TikTok experience under new ownership, or be forced to switch to less desirable platforms. Their data privacy concerns may not be fully addressed, and they lack control over the divestiture process.
constraint_indexing:constraint_classification(tiktok_us_divestiture_mandate, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% ByteDance is forced to relinquish control of a valuable asset under pressure. While they may receive financial compensation, the loss of strategic market access and future growth potential constitutes significant extraction. Constrained exit due to the political pressure.
constraint_indexing:constraint_classification(tiktok_us_divestiture_mandate, snare,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% US companies that could acquire TikTok's US operations benefit from gaining a large user base and market share. They experience the mandate as a coordination mechanism, facilitating their expansion in the social media landscape. Arbitrage exit as other targets are available.
constraint_indexing:constraint_classification(tiktok_us_divestiture_mandate, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% The US government benefits from the perceived increase in national security by controlling TikTok's US operations and data. However, they also face potential diplomatic backlash and concerns about setting a precedent for intervention in foreign companies. The exit is constrained as complete ban is even more problematic.
constraint_indexing:constraint_classification(tiktok_us_divestiture_mandate, tangled_rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% The divestiture mandate degrades existing international norms around cross-border data flow and investment. These norms, while imperfect, provided a framework for managing technological interdependence. This perspective sees the mandate as an inertial constraint maintained through performative action with limited true effectiveness.
constraint_indexing:constraint_classification(tiktok_us_divestiture_mandate, piton,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(tiktok_us_divestiture_mandate_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(tiktok_us_divestiture_mandate, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(tiktok_us_divestiture_mandate, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(tiktok_us_divestiture_mandate, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(tiktok_us_divestiture_mandate, TR),
    TR >= 0.70.

:- end_tests(tiktok_us_divestiture_mandate_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.60): High. ByteDance faces significant financial and strategic losses due to the forced divestiture. The potential for lost future revenue and market dominance contributes to this high extraction score. US users face a potential reduction in the user experience. Suppression (0.70): High. The mandate leaves ByteDance with limited options and suppresses its ability to operate freely in the US market. The limited exit for users gives high suppression. Theater Ratio (0.75): High. The public justifications and political maneuvering surrounding the mandate contribute to a high theater ratio. Mandatrophy Resolved: The classification acknowledges the conflicting perspectives and aims to identify the dominant structural relationship, which in this case is the US government extracting from ByteDance under the guise of national security. However, there may be benefits for US acquirers.
 *
 * PERSPECTIVAL GAP:
 *   The perspectives diverge significantly based on the agents' positions. The US government and potential US acquirers view the mandate as a necessary step to protect national security and promote competition, while ByteDance and US users experience it as a coercive measure that restricts their freedom and limits their options. The US government does not want to ban it outright, so this represents a tangled rope rather than a snare.
 *
 * DIRECTIONALITY LOGIC:
 *   The US government benefits from perceived increased national security, while ByteDance is forced to cede control of a key asset. US users are impacted by the changes to the platform and potentially reduced access or functionality.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    data_security_guarantees,
    'Can a US-controlled entity truly guarantee the security of US user data from foreign influence?',
    'Independent audits of data handling practices and security protocols under new ownership.',
    'If yes, the mandate genuinely improves national security (Rope). If not, it''s a primarily extractive measure (Snare).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(data_security_guarantees, empirical, 'Whether US control guarantees data security').

omega_variable(
    retaliatory_measures,
    'Will the mandate trigger retaliatory measures from other countries against US tech companies operating abroad?',
    'Monitoring policy changes and investment restrictions imposed by other nations.',
    'If yes, the mandate could harm US economic interests (Snare for US companies). If not, the impact is localized to TikTok (less extractive).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(retaliatory_measures, empirical, 'Likelihood of retaliatory measures').

omega_variable(
    precedent_setting,
    'Does the mandate set a precedent for governments to intervene in foreign tech companies based on national security concerns, regardless of evidence?',
    'Analysis of policy statements and legal justifications used by other governments in similar cases.',
    'If yes, the mandate undermines international investment norms (Piton). If no, it''s a unique case with limited broader implications.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(precedent_setting, conceptual, 'Whether the mandate sets a dangerous precedent').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tiktok_us_divestiture_mandate, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tikt_tr_t0, tiktok_us_divestiture_mandate, theater_ratio, 0, 0.2).
narrative_ontology:measurement(tikt_tr_t5, tiktok_us_divestiture_mandate, theater_ratio, 5, 0.5).
narrative_ontology:measurement(tikt_tr_t10, tiktok_us_divestiture_mandate, theater_ratio, 10, 0.75).

% Extraction over time
narrative_ontology:measurement(tikt_be_t0, tiktok_us_divestiture_mandate, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(tikt_be_t5, tiktok_us_divestiture_mandate, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(tikt_be_t10, tiktok_us_divestiture_mandate, base_extractiveness, 10, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tiktok_us_divestiture_mandate, enforcement_mechanism).
narrative_ontology:affects_constraint(tiktok_us_divestiture_mandate, cross_border_data_flow).
narrative_ontology:affects_constraint(tiktok_us_divestiture_mandate, foreign_investment_review).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
