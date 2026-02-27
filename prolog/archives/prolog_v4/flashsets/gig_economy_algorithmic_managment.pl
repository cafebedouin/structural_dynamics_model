% ============================================================================
% CONSTRAINT STORY: gig_economy_algorithmic_managment
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gig_economy_algorithmic_managment, []).

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
 *   constraint_id: gig_economy_algorithmic_managment
 *   human_readable: Algorithmic Management in the Gig Economy
 *   domain: economic/technological
 *
 * SUMMARY:
 *   The "Gig Economy" labor market is governed by algorithmic management,
 *   where platforms use opaque, data-driven systems to assign tasks, set
 *   prices, and evaluate performance. This model creates a power imbalance
 *   between platform companies and gig workers, raising concerns about
 *   exploitation, precarity, and lack of worker agency. While algorithms
 *   enable efficient task allocation and cost reduction, they also create new
 *   forms of extraction and control. Regulation and collective action are
 *   potential countervailing forces.
 *
 * KEY AGENTS:
 *   - Platform Companies: Primary beneficiary (institutional/arbitrage) – benefit from efficient task allocation, reduced labor costs, and scalable operations.
 *   - Gig Workers: Primary victim (powerless/trapped) – bear the costs of precarious work, income instability, and algorithmic control.
 *   - Consumers: Secondary beneficiary (moderate/mobile) – benefit from lower prices and increased convenience.
 *   - Regulators: Constrained actor (moderate/constrained) - Seek to balance innovation and worker protection.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gig_economy_algorithmic_managment, 0.65).
domain_priors:suppression_score(gig_economy_algorithmic_managment, 0.7).
domain_priors:theater_ratio(gig_economy_algorithmic_managment, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gig_economy_algorithmic_managment, extractiveness, 0.65).
narrative_ontology:constraint_metric(gig_economy_algorithmic_managment, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(gig_economy_algorithmic_managment, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gig_economy_algorithmic_managment, snare).
narrative_ontology:human_readable(gig_economy_algorithmic_managment, "Algorithmic Management in the Gig Economy").
narrative_ontology:topic_domain(gig_economy_algorithmic_managment, "economic/technological").

domain_priors:requires_active_enforcement(gig_economy_algorithmic_managment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gig_economy_algorithmic_managment, platform_companies).
narrative_ontology:constraint_beneficiary(gig_economy_algorithmic_managment, consumers).
narrative_ontology:constraint_victim(gig_economy_algorithmic_managment, gig_workers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Gig workers experience algorithmic management as a Snare. They are often trapped due to limited alternative employment options, and the algorithms extract labor surplus with little transparency or recourse. The local scope reflects that the impact is directly felt in their daily work and earnings.
constraint_indexing:constraint_classification(gig_economy_algorithmic_managment, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% Platform companies view algorithmic management as a Rope, facilitating efficient task allocation, pricing, and performance evaluation across a vast network of workers and consumers. They have arbitrage options (market expansion, tech innovation) and experience the system as coordination. The global scope reflects their operational scale.
constraint_indexing:constraint_classification(gig_economy_algorithmic_managment, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% Regulators experience algorithmic management as a Tangled Rope. They are constrained by technological complexity and lobbying efforts but also benefit from the platform's contribution to the economy and the efficiencies it creates. They attempt to balance innovation with worker protection and fair labor practices.
constraint_indexing:constraint_classification(gig_economy_algorithmic_managment, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% From an analytical perspective, algorithmic management in the gig economy represents a Tangled Rope. It combines elements of coordination (efficient task allocation) with asymmetric extraction (labor surplus capture, precarity), enforced by opaque algorithmic systems. The global scope reflects the widespread adoption of this model.
constraint_indexing:constraint_classification(gig_economy_algorithmic_managment, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gig_economy_algorithmic_managment_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(gig_economy_algorithmic_managment, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(gig_economy_algorithmic_managment, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(gig_economy_algorithmic_managment, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gig_economy_algorithmic_managment_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.65): High. Algorithmic management enables platforms to extract a significant portion of the labor surplus from gig workers, often with little transparency or recourse. Suppression (0.70): High. Workers have limited alternative employment options and face significant barriers to collective action or challenging algorithmic decisions. Theater ratio (0.30): Low. The system's functionality outweighs performative aspects, as it's directly tied to task assignment, pricing, and performance evaluation.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap arises from the power imbalance between platforms and workers. Platforms experience the system as coordination, while workers experience it as extraction. Regulators attempt to bridge this gap, but their effectiveness is limited by technological complexity and political constraints. The analytical observer recognizes the inherent tension between coordination and extraction in this model.
 *
 * DIRECTIONALITY LOGIC:
 *   Platform companies benefit from algorithmic management through cost reduction and increased efficiency (low d). Gig workers bear the cost of precarious work and algorithmic control (high d). Regulators aim to mitigate the negative effects of algorithmic management (moderate d). The analytical observer seeks to understand the overall impact of this model (variable d depending on analytical focus).
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as a Snare resolves the mandatrophy by highlighting the extractive aspects of algorithmic management in the gig economy. While algorithms may enable coordination, they also create new forms of exploitation and control. The Snare classification emphasizes the need for regulatory oversight and worker empowerment to mitigate these risks. Alternative classifications, such as Rope or Scaffold, may overlook the power imbalance and extractive dynamics inherent in this model.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    algorithmic_opacity,
    'To what extent are the algorithms'' decision-making processes transparent and accountable?',
    'Independent audits of algorithms, explainable AI research, regulatory standards for transparency.',
    'Higher transparency may shift the classification towards Tangled Rope, as workers gain agency and understanding. Lower transparency reinforces the Snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(algorithmic_opacity, empirical, 'The level of algorithmic transparency and accountability.').

omega_variable(
    worker_bargaining_power,
    'How much bargaining power do gig workers possess, individually or collectively?',
    'Analysis of unionization efforts, policy interventions (e.g., minimum wage laws), and platform responses to worker demands.',
    'Increased bargaining power could shift the classification towards Tangled Rope, as workers gain more favorable terms and exit options. Limited bargaining power reinforces the Snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(worker_bargaining_power, empirical, 'The degree of worker bargaining power in the gig economy.').

omega_variable(
    regulatory_effectiveness,
    'How effective are regulatory interventions in mitigating the negative effects of algorithmic management?',
    'Evaluation of policy outcomes (e.g., wage increases, improved working conditions), analysis of enforcement mechanisms, and stakeholder feedback.',
    'Effective regulation may shift the classification towards Tangled Rope, as the extraction is curtailed. Ineffective regulation reinforces the Snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_effectiveness, empirical, 'The effectiveness of regulatory interventions.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gig_economy_algorithmic_managment, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gig__tr_t0, gig_economy_algorithmic_managment, theater_ratio, 0, 0.2).
narrative_ontology:measurement(gig__tr_t5, gig_economy_algorithmic_managment, theater_ratio, 5, 0.3).
narrative_ontology:measurement(gig__tr_t10, gig_economy_algorithmic_managment, theater_ratio, 10, 0.4).

% Extraction over time
narrative_ontology:measurement(gig__be_t0, gig_economy_algorithmic_managment, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(gig__be_t5, gig_economy_algorithmic_managment, base_extractiveness, 5, 0.6).
narrative_ontology:measurement(gig__be_t10, gig_economy_algorithmic_managment, base_extractiveness, 10, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gig_economy_algorithmic_managment, resource_allocation).
narrative_ontology:affects_constraint(gig_economy_algorithmic_managment, online_labor_market_dynamics).
narrative_ontology:affects_constraint(gig_economy_algorithmic_managment, data_privacy_gig_economy).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
