% ============================================================================
% CONSTRAINT STORY: cn_tech_decoupling_security_software
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-03
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
 *   This policy reflects a broader trend of technological decoupling between
 *   China and the US, driven by national security concerns and a desire for
 *   greater self-reliance. The mandate aims to bolster the domestic software
 *   industry and reduce vulnerabilities to foreign technology. However, it
 *   also poses challenges for SOEs, which may face increased costs,
 *   operational disruptions, and potentially lower levels of security.
 *
 * KEY AGENTS:
 *   - Chinese SOEs: Primary target (powerless/trapped) - forced to adopt domestic software.
 *   - US Security Software Vendors: Secondary target (moderate/constrained) - lose market access.
 *   - Domestic Security Software Vendors: Primary beneficiary (institutional/arbitrage) - gain market share and government support.
 *   - CCP Cybersecurity Apparatus: Beneficiary (institutional/constrained) - gains control and reduces foreign reliance.
 *   - Analytical Observer: (analytical/analytical) - assesses the broader implications and trade-offs.
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
narrative_ontology:constraint_beneficiary(cn_tech_decoupling_security_software, domestic_security_software_vendors).
narrative_ontology:constraint_beneficiary(cn_tech_decoupling_security_software, ccp_cybersecurity_apparatus).
narrative_ontology:constraint_victim(cn_tech_decoupling_security_software, chinese_soes).
narrative_ontology:constraint_victim(cn_tech_decoupling_security_software, us_security_software_vendors).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% SOEs are forced to adopt potentially inferior domestic software, facing disruption, costs, and security vulnerabilities.
constraint_indexing:constraint_classification(cn_tech_decoupling_security_software, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% US vendors lose access to a major market and face reputational damage as alternatives are sought.
constraint_indexing:constraint_classification(cn_tech_decoupling_security_software, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% Chinese vendors benefit from increased market share and government support, but must deliver functional replacements.
constraint_indexing:constraint_classification(cn_tech_decoupling_security_software, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% The CCP gains greater control over data security and reduces reliance on foreign technology, but also faces potential vulnerabilities and economic costs.
constraint_indexing:constraint_classification(cn_tech_decoupling_security_software, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% From a broad analytical perspective, this mandate is a tangled rope, balancing national security goals with economic disruption and potential security risks.
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
 *   Extractiveness (0.60): High. The mandate imposes significant costs and disruptions on SOEs, as they must replace existing, potentially superior, US software with domestic alternatives. This extraction is driven by the political goal of reducing reliance on foreign technology. Suppression (0.70): High. The mandate effectively suppresses the option for SOEs to continue using US security software. The government actively enforces the policy, limiting the ability of SOEs to choose their preferred solutions. Theater Ratio (0.30): Relatively low. While there may be some performative aspects to the mandate (e.g., demonstrating commitment to national security), the primary driver is a genuine effort to achieve technological self-reliance and enhance cybersecurity.
 *
 * PERSPECTIVAL GAP:
 *   The SOEs experience the mandate as a snare, as they are forced to adopt potentially inferior domestic software, facing increased costs and security vulnerabilities. US vendors also see it as a snare, due to the loss of market access. Domestic vendors benefit and see the mandate as a rope, offering an opportunity to expand their market share. The CCP aims to turn the situation into a rope for themselves, but due to the potential for vulnerabilities from immature software and economic disruptions, it ends up being a tangled rope. The analytical observer sees a tangled rope as the constraint offers benefits to national security while also risking costs from the enforcement.
 *
 * DIRECTIONALITY LOGIC:
 *   Chinese SOEs: Victims + trapped → d=0.95, f(d)=1.42. High extraction. US Security Software Vendors: Victims + constrained → d=0.75, f(d)=1.1. Significant extraction. Domestic Security Software Vendors: Beneficiaries + arbitrage → d=0.05, f(d)=-0.12. Negative extraction (net benefit). CCP Cybersecurity Apparatus: Beneficiaries + constrained → d=0.15, f(d)=-0.01. Negative extraction (net benefit). Analytical observer: Analytical d=0.72, f(d)=1.15.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents mislabeling coordination as pure extraction (or vice versa) by explicitly identifying both the beneficiaries (domestic software vendors and the CCP) and the victims (SOEs and US vendors). The tangled rope classification accurately reflects the mixed nature of the constraint, where national security goals are intertwined with economic costs and potential security risks. By analyzing the different perspectives, the mandatrophy is resolved.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    domestic_software_capabilities,
    'How quickly and effectively can domestic vendors develop software to replace US offerings?',
    'Comparative performance testing and security audits of domestic vs. US software.',
    'If domestic software is inferior, SOEs face security vulnerabilities and operational disruptions. If comparable or superior, the mandate is more effective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(domestic_software_capabilities, empirical, 'Capabilities of domestic security software').

omega_variable(
    enforcement_effectiveness,
    'How strictly is the mandate enforced across different SOEs and sectors?',
    'Analysis of SOE compliance reports and independent audits of software usage.',
    'Weak enforcement allows continued reliance on US software, undermining the mandate''s goals. Strict enforcement maximizes decoupling but increases disruption.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(enforcement_effectiveness, empirical, 'Effectiveness of mandate enforcement').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cn_tech_decoupling_security_software, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cn_t_tr_t0, cn_tech_decoupling_security_software, theater_ratio, 0, 0.2).
narrative_ontology:measurement(cn_t_tr_t5, cn_tech_decoupling_security_software, theater_ratio, 5, 0.3).
narrative_ontology:measurement(cn_t_tr_t10, cn_tech_decoupling_security_software, theater_ratio, 10, 0.4).

% Extraction over time
narrative_ontology:measurement(cn_t_be_t0, cn_tech_decoupling_security_software, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(cn_t_be_t5, cn_tech_decoupling_security_software, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(cn_t_be_t10, cn_tech_decoupling_security_software, base_extractiveness, 10, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(cn_tech_decoupling_security_software, enforcement_mechanism).
narrative_ontology:affects_constraint(cn_tech_decoupling_security_software, us_china_trade_relations).
narrative_ontology:affects_constraint(cn_tech_decoupling_security_software, domestic_tech_innovation_china).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
