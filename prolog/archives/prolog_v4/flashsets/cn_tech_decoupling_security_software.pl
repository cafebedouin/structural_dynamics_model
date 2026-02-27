% ============================================================================
% CONSTRAINT STORY: cn_tech_decoupling_security_software
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-08
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_cn_tech_decoupling_security_software, []).

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
 *   constraint_id: cn_tech_decoupling_security_software
 *   human_readable: Mandate for Chinese SOEs to replace US security software
 *   domain: technological
 *
 * SUMMARY:
 *   A state mandate requires Chinese state-owned enterprises (SOEs) to
 *   replace US-origin network security software with domestic alternatives.
 *   This policy aims to enhance national security and foster technological
 *   independence, but it also presents challenges and creates structural
 *   tensions.
 *
 * KEY AGENTS:
 *   - Chinese SOEs: Primary target (powerless/trapped) - forced to adopt potentially inferior domestic software.
 *   - US Software Vendors: Secondary target (moderate/constrained) - lose market access in China.
 *   - Domestic Software Vendors: Primary beneficiary (institutional/arbitrage) - guaranteed market share and government support.
 *   - Chinese Government: Institutional actor (institutional/constrained) - aims to improve national security and tech independence.
 *   - Analytical Observer: Evaluates the mandate's overall impact (analytical/analytical).
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cn_tech_decoupling_security_software, 0.6).
domain_priors:suppression_score(cn_tech_decoupling_security_software, 0.7).
domain_priors:theater_ratio(cn_tech_decoupling_security_software, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cn_tech_decoupling_security_software, extractiveness, 0.6).
narrative_ontology:constraint_metric(cn_tech_decoupling_security_software, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(cn_tech_decoupling_security_software, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cn_tech_decoupling_security_software, tangled_rope).
narrative_ontology:human_readable(cn_tech_decoupling_security_software, "Mandate for Chinese SOEs to replace US security software").
narrative_ontology:topic_domain(cn_tech_decoupling_security_software, "technological").

domain_priors:requires_active_enforcement(cn_tech_decoupling_security_software).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(cn_tech_decoupling_security_software, domestic_software_vendors).
narrative_ontology:constraint_beneficiary(cn_tech_decoupling_security_software, chinese_government).
narrative_ontology:constraint_victim(cn_tech_decoupling_security_software, chinese_soes).
narrative_ontology:constraint_victim(cn_tech_decoupling_security_software, us_software_vendors).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Chinese SOEs are forced to adopt potentially inferior domestic software, facing increased security risks and operational inefficiencies with limited recourse.
constraint_indexing:constraint_classification(cn_tech_decoupling_security_software, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% US software vendors are significantly constrained due to the forced removal of their products from the Chinese market.
constraint_indexing:constraint_classification(cn_tech_decoupling_security_software, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% Domestic software vendors benefit from guaranteed market share, enabling further development and innovation, but also leading to less competitive pressure.
constraint_indexing:constraint_classification(cn_tech_decoupling_security_software, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% The Chinese government views the mandate as a means to enhance national security and technological independence, though at the cost of economic efficiency and potential retaliatory measures.
constraint_indexing:constraint_classification(cn_tech_decoupling_security_software, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% An analytical observer recognizes the mandate as a tangled rope, featuring both the coordination of resources toward national tech independence and the asymmetric extraction of value from SOEs and US software vendors.
constraint_indexing:constraint_classification(cn_tech_decoupling_security_software, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cn_tech_decoupling_security_software_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(cn_tech_decoupling_security_software, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(cn_tech_decoupling_security_software, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(cn_tech_decoupling_security_software, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(cn_tech_decoupling_security_software_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is moderate-high (0.60) reflecting the significant costs imposed on SOEs and US vendors. Suppression (0.70) indicates limited exit options for SOEs. The theater ratio (0.30) is relatively low, as the mandate involves real changes in software infrastructure, not just performative compliance.
 *
 * PERSPECTIVAL GAP:
 *   Chinese SOEs view the mandate as a snare due to the lack of choice and potential security risks. US Software vendors view it as a snare because they're losing market access. Domestic software vendors see it as a rope enabling growth. The Chinese government views it as a tangled rope, balancing national security goals with economic costs. The analytical observer views it as a tangled rope due to the blend of coordination (domestic software development) and extraction (costs to SOEs).
 *
 * DIRECTIONALITY LOGIC:
 *   The SOEs have limited exit options (trapped), so they experience higher extraction (higher d value), even if they are nominally powerful. The domestic software vendors have arbitrage because the government is providing subsidies. The US Software vendors have constrained because they can sell elsewhere, but it's still a loss of revenue.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandate can be seen as legitimate coordination from the government's perspective (a necessary step for national security) or as pure extraction from the SOEs' perspective (forced adoption of inferior products). The tangled rope classification captures both aspects. The key is whether the domestic alternatives are truly adequate; if they are not, the "coordination" aspect is primarily theatrical.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    domestic_software_capability,
    'How closely can domestic software vendors match the performance and security standards of the replaced US software?',
    'Independent security audits and performance benchmarking of domestic vs. US software.',
    'If domestic software is comparable: reduced impact on SOE efficiency. If domestic software is inferior: increased security risks and operational costs for SOEs.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(domestic_software_capability, empirical, 'Capability of domestic security software to replace US software').

omega_variable(
    enforcement_severity,
    'How strictly will the mandate be enforced, and what penalties will be imposed for non-compliance?',
    'Analysis of government directives and actual penalties levied against non-compliant SOEs.',
    'Strict enforcement: higher compliance rates but potentially greater economic disruption. Lax enforcement: limited effectiveness of the mandate in achieving its goals.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(enforcement_severity, empirical, 'Severity of mandate enforcement').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cn_tech_decoupling_security_software, 0, 5).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cn_t_tr_t0, cn_tech_decoupling_security_software, theater_ratio, 0, 0.2).
narrative_ontology:measurement(cn_t_tr_t2, cn_tech_decoupling_security_software, theater_ratio, 2, 0.25).
narrative_ontology:measurement(cn_t_tr_t5, cn_tech_decoupling_security_software, theater_ratio, 5, 0.3).

% Extraction over time
narrative_ontology:measurement(cn_t_be_t0, cn_tech_decoupling_security_software, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(cn_t_be_t2, cn_tech_decoupling_security_software, base_extractiveness, 2, 0.52).
narrative_ontology:measurement(cn_t_be_t5, cn_tech_decoupling_security_software, base_extractiveness, 5, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(cn_tech_decoupling_security_software, enforcement_mechanism).
narrative_ontology:affects_constraint(cn_tech_decoupling_security_software, us_china_trade_relations).
narrative_ontology:affects_constraint(cn_tech_decoupling_security_software, china_cybersecurity_law).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
