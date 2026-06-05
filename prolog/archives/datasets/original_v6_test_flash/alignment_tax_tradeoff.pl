% ============================================================================
% CONSTRAINT STORY: alignment_tax_tradeoff
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_alignment_tax_tradeoff, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: alignment_tax_tradeoff
 *   human_readable: The Safety-Performance Divergence
 *   domain: technological/AI/economic
 *
 * SUMMARY:
 *   The safety-performance divergence describes a scenario in AI development
 *   where systems aligned with human values incur a "tax" in terms of
 *   computational overhead, reduced capability, or increased latency. This
 *   tax creates a competitive disadvantage against unaligned systems that can
 *   achieve higher performance at lower costs. This dynamic introduces a
 *   structural tension between safety and market incentives.
 *
 * KEY AGENTS:
 *   - Unaligned AI Developers: Primary beneficiary (institutional/arbitrage) - benefit from increased competitiveness and market share.
 *   - Aligned AI Developers: Primary victim (moderate/constrained) - face competitive pressure and reduced market share.
 *   - Society: Ultimate victim (powerless/trapped) - bears the risks of unaligned AI systems.
 *   - Early Adopters: Secondary beneficiary (moderate/mobile) - benefit from higher performance/lower costs initially, but are exposed to risk
 *   - AI Alignment Research Community: organized agents (organized/arbitrage) - work to reduce the 'alignment tax' (Scaffold).
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(alignment_tax_tradeoff, 0.6).
domain_priors:suppression_score(alignment_tax_tradeoff, 0.45).
domain_priors:theater_ratio(alignment_tax_tradeoff, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(alignment_tax_tradeoff, extractiveness, 0.6).
narrative_ontology:constraint_metric(alignment_tax_tradeoff, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(alignment_tax_tradeoff, theater_ratio, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(alignment_tax_tradeoff, tangled_rope).
narrative_ontology:human_readable(alignment_tax_tradeoff, "The Safety-Performance Divergence").
narrative_ontology:topic_domain(alignment_tax_tradeoff, "technological/AI/economic").

domain_priors:requires_active_enforcement(alignment_tax_tradeoff).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(alignment_tax_tradeoff, unaligned_ai_developers).
narrative_ontology:constraint_beneficiary(alignment_tax_tradeoff, early_adopters).
narrative_ontology:constraint_victim(alignment_tax_tradeoff, society).
narrative_ontology:constraint_victim(alignment_tax_tradeoff, aligned_ai_developers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Perspective of society as a whole, which bears the risk of unaligned AI. Unable to exit the situation due to the pervasive nature of AI systems.
constraint_indexing:constraint_classification(alignment_tax_tradeoff, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% Perspective of developers who prioritize safety and alignment. They are constrained by the competitive landscape, facing pressure to reduce the alignment tax. They also benefit from a safer ecosystem.
constraint_indexing:constraint_classification(alignment_tax_tradeoff, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% Perspective of developers who prioritize performance over alignment. They benefit from increased competitiveness and market share due to lower overhead costs.
constraint_indexing:constraint_classification(alignment_tax_tradeoff, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% Perspective of early adopters and consumers who benefit from higher performance and lower costs initially, but are also exposed to higher risks associated with unaligned AI. Mobile because they have the ability to switch to aligned AI as it matures.
constraint_indexing:constraint_classification(alignment_tax_tradeoff, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% Perspective of standards bodies initially designed to create safe AI systems. Risk becoming pitons if they focus on theatrical compliance rather than robust safety measures.
constraint_indexing:constraint_classification(alignment_tax_tradeoff, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% The alignment research community is working to reduce the 'alignment tax', making safe AI more competitive. A temporary scaffold intended to build more general alignment strategies.
constraint_indexing:constraint_classification(alignment_tax_tradeoff, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(alignment_tax_tradeoff_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(alignment_tax_tradeoff, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(alignment_tax_tradeoff, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(alignment_tax_tradeoff, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(alignment_tax_tradeoff, TR),
    TR >= 0.70.

:- end_tests(alignment_tax_tradeoff_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is relatively high (0.60) because the competitive pressure on aligned AI developers is significant. The suppression value (0.45) reflects the constraints on aligned AI developers, but also acknowledges the arbitrage opportunity available to unaligned developers. The theater ratio is low (0.20) because safety is a real constraint
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap arises from differing incentives and risk exposures. Unaligned AI developers see a coordination mechanism (Rope): focusing on performance benefits all participants in the short term. Aligned AI developers see a Tangled Rope: safety benefits society but harms their competitive position. Society sees a Snare: they bear the ultimate risk. 
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality is determined by whether agents benefit from or bear the costs of the alignment tax. Unaligned developers benefit, giving them a low d-value. Aligned developers and society bear the costs, giving them higher d-values.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    alignment_tax_magnitude,
    'How significant is the performance overhead required for aligning AI systems?',
    'Empirical measurement of the performance difference between aligned and unaligned systems across various tasks.',
    'If the overhead is high, the competitive disadvantage is more pronounced. If it''s low, the trade-off is less significant.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alignment_tax_magnitude, empirical, 'Magnitude of performance overhead caused by AI alignment.').

omega_variable(
    societal_risk_perception,
    'How does society perceive the risks associated with unaligned AI?',
    'Public opinion surveys and analysis of media coverage on AI safety.',
    'If risks are perceived as low, there is less pressure for alignment. If perceived as high, there may be policy interventions to mandate alignment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(societal_risk_perception, empirical, 'Perceived risk of unaligned AI.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(alignment_tax_tradeoff, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(alig_tr_t0, alignment_tax_tradeoff, theater_ratio, 0, 0.1).
narrative_ontology:measurement(alig_tr_t5, alignment_tax_tradeoff, theater_ratio, 5, 0.15).
narrative_ontology:measurement(alig_tr_t10, alignment_tax_tradeoff, theater_ratio, 10, 0.2).

% Extraction over time
narrative_ontology:measurement(alig_be_t0, alignment_tax_tradeoff, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(alig_be_t5, alignment_tax_tradeoff, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(alig_be_t10, alignment_tax_tradeoff, base_extractiveness, 10, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
