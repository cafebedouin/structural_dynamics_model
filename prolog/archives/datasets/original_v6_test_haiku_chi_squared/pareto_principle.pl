% ============================================================================
% CONSTRAINT STORY: pareto_principle
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_pareto_principle, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: pareto_principle
 *   human_readable: The Pareto Principle (80/20 Rule)
 *   domain: statistical/economic/social
 *
 * SUMMARY:
 *   The Pareto Principle (80/20 Rule) is a constraint that operates as both a
 *   statistical observation and a normative allocation framework, creating
 *   structural inequality in resource distribution across economies,
 *   organizations, and social systems. The principle observes that in many
 *   domains, roughly 80% of consequences derive from 20% of causes. This
 *   observation, if empirically valid, provides genuine optimization insight.
 *   However, the constraint operates as a tangled hybrid: it functions as a
 *   coordination mechanism for resource-constrained allocation while
 *   simultaneously extracting from long-tail contributors through systematic
 *   deprioritization. The theater ratio (0.61) reflects that the principle is
 *   often invoked without domain-specific verification, treated as a
 *   universal natural law despite weak empirical support. The extractiveness
 *   has increased over the measurement interval (0.18 → 0.32) as the
 *   principle shifted from an organizational heuristic to a normalized
 *   framework justifying inequality across business, science, and social
 *   policy. The principle exhibits all six DR types from different structural
 *   perspectives: for the concentrated producers it is a coordination tool
 *   (Rope); for the long-tail contributors it is pure extraction (Snare); for
 *   institutional optimizers it is rational allocation (Rope); for the theory
 *   establishment it is performative citation (Piton); for statistical
 *   naturalists it appears as mathematical inevitability (false Mountain);
 *   and for alternative allocation systems (open-source, participatory
 *   platforms) it represents a temporary constraint being displaced
 *   (Scaffold).
 *
 * KEY AGENTS:
 *   - High Performers / Concentrated Producers: Primary beneficiary (institutional/arbitrage) — capture disproportionate resources, recognition, and rewards based on Pareto allocation principle
 *   - Long-Tail Contributors: Primary victim (powerless/trapped) — majority of workers, creators, researchers whose output is systematically underinvested and undervalued
 *   - Organizational Management / Resource Allocators: Secondary beneficiary (institutional/arbitrage) — use Pareto Principle to justify resource concentration and efficiency optimization
 *   - Mid-Level Performers: Mixed actor (moderate/constrained) — benefit from concentration relative to true tail but bear costs of defunding marginal work
 *   - Management Theory Establishment: Institutional maintainer (institutional/arbitrage) — perpetuates principle through business education, consulting, and ritualistic citation
 *   - Statistical Natural Law Proponents: Analytical observer (analytical/analytical) — risk naturalizing contingent empirical pattern as mathematical inevitability
 *   - Alternative Allocation Coalition: Organized reformers (organized/mobile) — platforms and systems (GitHub, Wikipedia, participatory funding) building alternative distributions with explicit sunset logic
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(pareto_principle, 0.32).
domain_priors:suppression_score(pareto_principle, 0.38).
domain_priors:theater_ratio(pareto_principle, 0.61).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(pareto_principle, extractiveness, 0.32).
narrative_ontology:constraint_metric(pareto_principle, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(pareto_principle, theater_ratio, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(pareto_principle, tangled_rope).
narrative_ontology:human_readable(pareto_principle, "The Pareto Principle (80/20 Rule)").
narrative_ontology:topic_domain(pareto_principle, "statistical/economic/social").

domain_priors:requires_active_enforcement(pareto_principle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(pareto_principle, high_performers).
narrative_ontology:constraint_beneficiary(pareto_principle, concentrated_producers).
narrative_ontology:constraint_beneficiary(pareto_principle, administrative_optimizers).
narrative_ontology:constraint_victim(pareto_principle, long_tail_contributors).
narrative_ontology:constraint_victim(pareto_principle, marginal_actors).
narrative_ontology:constraint_victim(pareto_principle, statistical_misclassification).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LONG-TAIL CONTRIBUTOR (SNARE) — An individual in the 80% of causes (workers, artists, researchers, small businesses) that produces only 20% of measurable outcomes. Trapped by structural inequality and resource scarcity. d≈0.92, f(d)≈1.40, σ=1.2 → χ≈0.53. Cannot exit the constraint without abandoning their field entirely.
constraint_indexing:constraint_classification(pareto_principle, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: RESOURCE ALLOCATOR / ORGANIZATIONAL MANAGER (ROPE) — From the optimization perspective, the Pareto Principle is a coordination mechanism enabling efficient resource allocation. Concentrating investment in the 20% of causes that produce 80% of outcomes maximizes output. d≈0.08, f(d)≈-0.10, σ=1.1 → χ≈-0.03. Net beneficiary; sees the constraint as a tool for rational allocation.
constraint_indexing:constraint_classification(pareto_principle, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: MID-LEVEL PERFORMER / SECONDARY PRODUCER (TANGLED ROPE) — Agents slightly above the 20% threshold who benefit from concentrated resource allocation (receives more attention than true tail) but also bear costs when marginal contributors are defunded or deprioritized. d≈0.58, f(d)≈0.77, σ=1.0 → χ≈0.25. Mixed experience: coordination mechanism plus extraction from those below.
constraint_indexing:constraint_classification(pareto_principle, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: MANAGEMENT THEORY ESTABLISHMENT / BUSINESS CONSULTING (PITON) — Pareto Principle has become a ritualistic citation in business discourse, management consulting, and productivity advice despite weak empirical support and domain-specific variation. Theater ratio (0.61) reflects that the principle is often invoked without verification of actual ratios in specific contexts. The constraint persists through institutional inertia: MBA curricula teach it as canonical, consultants cite it as evidence-based, executives internalize it as natural law. d≈0.05, f(d)≈-0.12, σ=1.2 → χ≈-0.02. Maintains performative utility.
constraint_indexing:constraint_classification(pareto_principle, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: STATISTICAL NATURAL LAW (FALSE SUMMIT) — Claims the Pareto Principle is a mathematical inevitability under certain distribution assumptions (power laws, self-similar processes). From this view, unequal outcome distribution is not extractive but inherent to how stochastic systems work. However, the principle's empirical status is far weaker than claimed (ε=0.32, not ≤0.25), and suppression is real (0.38), revealing this as a false summit where a contingent empirical pattern is naturalized as mathematical law.
constraint_indexing:constraint_classification(pareto_principle, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 6: COMPETITIVE EQUILIBRIUM / ALTERNATIVE ALLOCATION COALITION (SCAFFOLD) — Movement toward decentralized systems (open-source contribution, user-generated content platforms, participatory resource allocation) that show flatter outcome distributions and broader contributor recognition. These models have explicit or implicit sunset: as technology enables distributed contribution (GitHub, Wikipedia, Substack), the Pareto constraint loses enforcement power. d≈0.42, f(d)≈0.42, σ=1.2 → χ≈0.17. Low effective extraction because exit pathways exist.
constraint_indexing:constraint_classification(pareto_principle, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(pareto_principle_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(pareto_principle, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(pareto_principle, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(pareto_principle, TR),
    TR >= 0.70.

:- end_tests(pareto_principle_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.32): Moderate. The principle extracts value from long-tail contributors through systematic deprioritization and defunding, but the extraction is justified as optimization rather than coercion. Over the interval, extractiveness increased (0.18 → 0.32) as the principle shifted from descriptive observation to normative allocation framework. Suppression (0.38): Moderate. Barriers to long-tail actor resistance include: (1) difficulty organizing dispersed contributors, (2) legitimacy of optimization narrative, (3) information asymmetry (high performers appear more productive because we measure their specific outputs, not opportunity costs of neglected alternatives). However, suppression is not total — alternative systems are emerging. Theater ratio (0.61): Moderate-high. Pareto is often invoked in business and policy contexts without empirical verification of actual domain-specific ratios. MBA curricula teach it as canonical fact; consultants cite it as evidence-based; executives treat it as natural law. The ritualistic citation increased from 0.42 to 0.61 over the interval, reflecting deeper institutional embedding. Claimed type (Tangled Rope): Requires beneficiaries (high performers, optimizers), victims (long-tail contributors), and active enforcement (resource concentration mechanisms). All three are present.
 *
 * PERSPECTIVAL GAP:
 *   The Pareto Principle exhibits maximum perspectival divergence. The resource allocator (institutional/arbitrage) sees a coordination mechanism (Rope) — they experience the principle as enabling rational efficiency. The long-tail contributor (powerless/trapped) sees extraction (Snare) — they experience systematic deprioritization with no exit. The mid-level performer (moderate/constrained) sees the hybrid nature (Tangled Rope) — they benefit relative to the tail but lose resources that might have been allocated to marginal innovations. The management theory establishment (institutional/arbitrage) sees performative citation (Piton) — the principle persists through institutional inertia despite domain-specific variation. The statistical naturalist (analytical/analytical) risks seeing a mathematical law (false Mountain) — but the low accessibility collapse (0.32, not ≥0.85) and moderate suppression (0.38, not ≤0.05) reveal the naturalization as unjustified. The alternative allocation coalition (organized/mobile) sees a temporary constraint (Scaffold) — they are building systems where contribution distributions are flatter and contribution recognition is broader.
 *
 * DIRECTIONALITY LOGIC:
 *   High performers / optimizers: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiary; negative effective extraction. Long-tail contributors: Victim + trapped → d≈0.92, f(d)≈1.40. Maximum extraction. Mid-level performers: Victim + constrained → d≈0.58, f(d)≈0.77. Significant extraction but constrained by their proximity to the 20% threshold. Management establishment: Institutional + arbitrage → d≈0.05, f(d)≈-0.12. Piton classification derives from theater gate (0.61 ≥ 0.70 threshold not met, but high enough to show degradation). Alternative allocation coalition: Organized + mobile → d≈0.42, f(d)≈0.42. Low effective extraction because exit pathways exist (switching to decentralized platforms). Statistical naturalists: Analytical → d≈0.72, f(d)≈1.15. False Mountain classification.
 *
 * MANDATROPHY ANALYSIS:
 *   The Pareto Principle resolves mandatrophy by revealing the dual nature of the constraint: it is simultaneously (1) a genuine coordination mechanism for resource allocation under scarcity (legitimate optimization insight) and (2) an extraction mechanism that systematically devalues long-tail contribution (asymmetric resource distribution). The principle is not a deceptive Snare pretending to be Rope; it is a genuine Tangled Rope that combines coordination and extraction. The mandatrophy question — 'Is this coordination or extraction?' — is answered: it is both, depending on your position in the distribution. The false summit risk emerges when the principle is naturalized as a mathematical law (statistical mountain) without acknowledging the choice to measure certain contributions and ignore others. The extractiveness has increased over time (0.18 → 0.32) not because the statistical regularity changed but because the principle shifted from descriptive observation to normative justification for inequality. This trajectory suggests the constraint is degrading toward Piton — maintained through institutional inertia (business education, consulting, executive belief) rather than through functional necessity. The alternative allocation coalition perspective (Scaffold) shows the sunset logic: distributed platforms, open-source contribution models, and participatory funding mechanisms are building systems where the Pareto constraint loses enforcement power. The principle's theater ratio (0.61) reflects that the gap between ritualistic invocation and empirical verification has widened, a signature of inertial degradation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    empirical_universality_of_80_20,
    'Is the 80/20 split a universal empirical regularity or a context-dependent pattern that often fails?',
    'Large-scale cross-domain empirical study: measure outcome distribution across 100+ domains (firms, fields, markets, scientific domains). Quantify frequency with which 80% outcome comes from ≤20% causes vs other distributions (70/30, 90/10, flatter).',
    'If universal (>80% of domains show 80/20 ±5%): validates Mountain perspective and strengthens optimization frame. If context-dependent (<50%): Pareto Principle is domain-specific heuristic, not natural law. Reclassifies as Tangled Rope from all institutional perspectives.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(empirical_universality_of_80_20, empirical, 'Whether 80/20 split is universal or context-dependent').

omega_variable(
    mechanism_vs_artifact,
    'Does outcome inequality arise from structural causation (high performers are inherently more productive) or measurement artifact (we measure and reward only certain types of contribution)?',
    'Comparison of outcomes under different measurement regimes: (a) formal metrics (sales, citations, output volume), (b) broad contribution metrics (including enabling, mentoring, infrastructure work), (c) subjective peer assessment. If (b) and (c) show flatter distributions while (a) shows 80/20, inequality is partly measurement artifact.',
    'If structural: Pareto reflects real performance differences; optimization is justified. If artifact: Pareto reflects what we choose to count; concentration is enforced by selective measurement. Reclassifies toward Snare for long-tail contributors.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mechanism_vs_artifact, empirical, 'Whether inequality reflects inherent productivity or measurement selection').

omega_variable(
    feedback_loop_malignancy,
    'Does resource concentration based on Pareto allocation create positive feedback that perpetuates inequality, even when initial performance differences were smaller?',
    'Longitudinal study of initial talent distribution vs outcome distribution after concentrated resource allocation. Track whether concentrated funding amplifies small initial differences into large outcome gaps. Test whether equalized resource allocation flattens outcomes.',
    'If strong feedback: Pareto Principle is enforcement mechanism (Snare) that creates the inequality it purports to describe. If weak feedback: Principle reveals pre-existing differences (Rope). Determines whether defunding long-tail is optimization or extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(feedback_loop_malignancy, empirical, 'Whether Pareto allocation creates positive feedback that perpetuates inequality').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(pareto_principle, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pareto_tr_t0, pareto_principle, theater_ratio, 0, 0.42).
narrative_ontology:measurement(pareto_tr_t5, pareto_principle, theater_ratio, 5, 0.52).
narrative_ontology:measurement(pareto_tr_t10, pareto_principle, theater_ratio, 10, 0.61).

% Extraction over time
narrative_ontology:measurement(pareto_be_t0, pareto_principle, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(pareto_be_t5, pareto_principle, base_extractiveness, 5, 0.26).
narrative_ontology:measurement(pareto_be_t10, pareto_principle, base_extractiveness, 10, 0.32).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(pareto_principle, resource_allocation).
narrative_ontology:affects_constraint(pareto_principle, income_inequality_reproduction).
narrative_ontology:affects_constraint(pareto_principle, scientific_citation_concentration).
narrative_ontology:affects_constraint(pareto_principle, wealth_concentration_dynamics).

% DUAL FORMULATION NOTE:
% The Pareto Principle decomposes into two structurally distinct claims: (1) Statistical Regularity: outcome distributions in many domains follow power laws, producing approximate 80/20 splits (ε≈0.08, Mountain for natural law; ε≈0.25 for empirical claim). (2) Normative Allocation Framework: the principle is used to justify resource concentration, creating extraction mechanisms. These are linked: the statistical claim provides legitimacy for the allocation framework. Upstream constraint is the statistical observation; downstream constraints are specific institutional applications (income concentration, citation patterns, wealth dynamics). The dual formulation note explains why three separate constraint stories are needed.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(pareto_principle, analytical, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
