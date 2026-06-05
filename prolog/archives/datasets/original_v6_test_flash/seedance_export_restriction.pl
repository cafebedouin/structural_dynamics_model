% ============================================================================
% CONSTRAINT STORY: seedance_export_restriction
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_seedance_export_restriction, []).

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
 *   constraint_id: seedance_export_restriction
 *   human_readable: US Export Restrictions on ByteDance's SeeDance AI
 *   domain: political/technological/economic
 *
 * SUMMARY:
 *   The US government's export restrictions on ByteDance's SeeDance AI aim to
 *   protect US national security and promote domestic AI industry. However,
 *   these restrictions also have implications for international research
 *   collaboration and ByteDance's market access. Different actors experience
 *   the restrictions in different ways, leading to varying perspectives on
 *   their effectiveness and legitimacy.
 *
 * KEY AGENTS:
 *   - ByteDance: Primary target (powerless/trapped) - bears the cost of market access restrictions
 *   - US AI Industry: Primary beneficiary (institutional/arbitrage) - benefits from reduced competition
 *   - US National Security Agencies: Enforcer and beneficiary (institutional/constrained) - benefits from reduced security risks but constrained by enforcement costs
 *   - International Research Community: Secondary target (moderate/constrained) - access to SeeDance AI is limited
 *   - Analytical Observer: Evaluates the overall impact (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(seedance_export_restriction, 0.55).
domain_priors:suppression_score(seedance_export_restriction, 0.7).
domain_priors:theater_ratio(seedance_export_restriction, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(seedance_export_restriction, extractiveness, 0.55).
narrative_ontology:constraint_metric(seedance_export_restriction, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(seedance_export_restriction, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(seedance_export_restriction, tangled_rope).
narrative_ontology:human_readable(seedance_export_restriction, "US Export Restrictions on ByteDance's SeeDance AI").
narrative_ontology:topic_domain(seedance_export_restriction, "political/technological/economic").

domain_priors:requires_active_enforcement(seedance_export_restriction).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(seedance_export_restriction, us_ai_industry).
narrative_ontology:constraint_beneficiary(seedance_export_restriction, us_national_security_agencies).
narrative_ontology:constraint_victim(seedance_export_restriction, bytedance).
narrative_ontology:constraint_victim(seedance_export_restriction, international_research_community).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% ByteDance experiences the restrictions as a Snare, limiting its market access and technology development opportunities. They have limited exit options due to the political nature of the restrictions.
constraint_indexing:constraint_classification(seedance_export_restriction, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% US AI companies benefit from reduced competition. They experience the constraint as a Rope, coordinating to maintain their market position and technological advantage. They have arbitrage options as they can invest in and develop similar AI technologies.
constraint_indexing:constraint_classification(seedance_export_restriction, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% The international research community experiences a Tangled Rope. Access to SeeDance AI is limited, hindering research, but they may also benefit from the development of alternative AI technologies and open-source solutions. They are constrained but not entirely trapped, as they can seek alternative collaborations and technologies.
constraint_indexing:constraint_classification(seedance_export_restriction, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% US national security agencies experience the restrictions as a Tangled Rope. They benefit from reduced potential for foreign access to advanced AI, but are also constrained by the need to actively enforce the restrictions and monitor compliance.
constraint_indexing:constraint_classification(seedance_export_restriction, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% From an analytical perspective, the export restrictions represent a Tangled Rope. They serve as a tool for national security and industrial policy, but also stifle global innovation and potentially lead to retaliatory measures. The long-term consequences are uncertain.
constraint_indexing:constraint_classification(seedance_export_restriction, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(seedance_export_restriction_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(seedance_export_restriction, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(seedance_export_restriction, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(seedance_export_restriction, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(seedance_export_restriction_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is moderate (0.55) as the restrictions do impact ByteDance's market access and technology development, but they are not completely prevented from operating. The suppression is high (0.70) because the export restrictions are actively enforced by the US government, limiting ByteDance's ability to circumvent the rules. The theater ratio is relatively low (0.30) suggesting there is little performative aspect to the restrictions - they are genuinely intended to limit access.
 *
 * PERSPECTIVAL GAP:
 *   ByteDance views the restrictions as a Snare, significantly hindering its business. The US AI industry sees it as a Rope, helping them maintain their competitive advantage. The international research community perceives a Tangled Rope, as they are both limited and potentially stimulated by the restrictions. The analytical observer recognizes the multifaceted nature of the constraint, acknowledging both its benefits and drawbacks.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality is determined by the structural position of each agent relative to the constraint. ByteDance, as the target, experiences the highest extraction. US AI firms, as beneficiaries, experience the lowest. The international research community and US national security agencies fall in between, experiencing both benefits and constraints.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ai_technology_transfer_risk,
    'What is the actual risk of AI technology transfer to foreign entities?',
    'Intelligence gathering, technology audits, and expert assessments of AI development efforts.',
    'If the risk is high, the restrictions are justified. If the risk is low, the restrictions may be overly restrictive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ai_technology_transfer_risk, empirical, 'The degree of actual risk of AI technology transfer.').

omega_variable(
    innovation_impact,
    'What is the long-term impact of these restrictions on AI innovation?',
    'Comparative analysis of AI development trends in the US and other countries.',
    'If the restrictions stifle innovation, the US may lose its competitive edge. If the restrictions encourage domestic innovation, the US may benefit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(innovation_impact, empirical, 'Long-term impact on AI innovation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(seedance_export_restriction, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seed_tr_t0, seedance_export_restriction, theater_ratio, 0, 0.2).
narrative_ontology:measurement(seed_tr_t5, seedance_export_restriction, theater_ratio, 5, 0.3).
narrative_ontology:measurement(seed_tr_t10, seedance_export_restriction, theater_ratio, 10, 0.4).

% Extraction over time
narrative_ontology:measurement(seed_be_t0, seedance_export_restriction, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(seed_be_t5, seedance_export_restriction, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(seed_be_t10, seedance_export_restriction, base_extractiveness, 10, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(seedance_export_restriction, enforcement_mechanism).
narrative_ontology:affects_constraint(seedance_export_restriction, ai_arms_race).
narrative_ontology:affects_constraint(seedance_export_restriction, us_china_tech_competition).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
