% ============================================================================
% CONSTRAINT STORY: alignment_tax_tradeoff
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-29
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
 *   constraint_id: alignment_tax_tradeoff
 *   human_readable: The Safety-Performance Divergence
 *   domain: technological/AI/economic
 *
 * SUMMARY:
 *   The Safety-Performance Divergence, or 'Alignment Tax', describes the
 *   competitive disadvantage incurred by efforts to make AI systems safe and
 *   aligned with human values. As AI capabilities accelerate, the
 *   computational overhead, performance reduction, or development delays
 *   required for robust safety create a powerful incentive for market actors
 *   to prioritize raw capability, externalizing the associated risks onto
 *   society. This creates a classic race-to-the-bottom dynamic where the
 *   safest actors are the least competitive.
 *
 * KEY AGENTS:
 *   - Capability-Focused AI Developers: Primary beneficiaries (institutional/arbitrage) - Gain market share and first-mover advantage by minimizing the alignment tax.
 *   - Global Civil Society: Primary victim (powerless/trapped) - Bears the externalized costs of misaligned AI, from misinformation to systemic risks, without recourse.
 *   - Safety-Conscious AI Developers: Secondary victim (powerful/constrained) - Must pay the alignment tax, putting them at a competitive disadvantage in the market.
 *   - Short-Term Investors: Secondary beneficiary (institutional/arbitrage) - Profit from rapid deployment and market capture, discounting long-term tail risks.
 *   - Regulators: Institutional actors (institutional/constrained) - Attempt to manage the tradeoff with policy scaffolds, but are often outpaced by technological development.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(alignment_tax_tradeoff, 0.65).
domain_priors:suppression_score(alignment_tax_tradeoff, 0.75).
domain_priors:theater_ratio(alignment_tax_tradeoff, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(alignment_tax_tradeoff, extractiveness, 0.65).
narrative_ontology:constraint_metric(alignment_tax_tradeoff, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(alignment_tax_tradeoff, theater_ratio, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(alignment_tax_tradeoff, tangled_rope).
narrative_ontology:human_readable(alignment_tax_tradeoff, "The Safety-Performance Divergence").
narrative_ontology:topic_domain(alignment_tax_tradeoff, "technological/AI/economic").

domain_priors:requires_active_enforcement(alignment_tax_tradeoff).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(alignment_tax_tradeoff, capability_focused_ai_developers).
narrative_ontology:constraint_beneficiary(alignment_tax_tradeoff, short_term_investors).
narrative_ontology:constraint_victim(alignment_tax_tradeoff, global_civil_society).
narrative_ontology:constraint_victim(alignment_tax_tradeoff, safety_conscious_ai_developers).
narrative_ontology:constraint_victim(alignment_tax_tradeoff, future_generations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE PUBLIC (SNARE) — Trapped within the global system, society bears the externalized risks of unaligned AI without consent or recourse. The competitive race suppresses safer alternatives, creating a coercive environment. d≈0.95, f(d)≈1.42, σ=1.2 → χ≈1.11.
constraint_indexing:constraint_classification(alignment_tax_tradeoff, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: THE LEADING LAB (ROPE) — From this perspective, the tradeoff is a pure coordination challenge: how to efficiently allocate resources to maximize performance. The 'tax' is a cost to be minimized, and the risks are future hypotheticals. d≈0.05, f(d)≈-0.12, σ=1.2 → χ≈-0.09. Negative extraction indicates a net subsidy from the system.
constraint_indexing:constraint_classification(alignment_tax_tradeoff, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: THE SAFETY-CONSCIOUS COMPETITOR (TANGLED ROPE) — This actor experiences both the coordination benefits of a shared technological frontier and the extractive penalty of the alignment tax. They are constrained by market realities and cannot afford to fall too far behind on performance. d≈0.65, f(d)≈1.00, σ=1.2 → χ≈0.78.
constraint_indexing:constraint_classification(alignment_tax_tradeoff, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER (TANGLED ROPE) — The observer sees the full structure: a genuine coordination problem (advancing AI) coupled with a powerful extractive mechanism (externalizing risk for competitive gain). The high suppression and extraction metrics clearly indicate a hybrid system. d≈0.72, f(d)≈1.15, σ=1.2 → χ≈0.90.
constraint_indexing:constraint_classification(alignment_tax_tradeoff, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 5: THE REGULATOR (SCAFFOLD) — Regulators view their interventions (e.g., AI Acts, executive orders) as temporary scaffolds designed to guide development until better, more permanent solutions are found. They tolerate high initial friction (suppression) because the policies have implicit or explicit sunset clauses (review periods). This perspective sees the problem as manageable and temporary.
constraint_indexing:constraint_classification(alignment_tax_tradeoff, scaffold,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(alignment_tax_tradeoff_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(alignment_tax_tradeoff, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(alignment_tax_tradeoff, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(alignment_tax_tradeoff, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(alignment_tax_tradeoff_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.65) is high, representing the significant competitive advantage gained by externalizing risk. This is not a direct financial transfer but a structural appropriation of market position and an imposition of unpriced negative externalities. Suppression (0.75) is very high, reflecting the intense market pressure and venture capital incentives that make it difficult for any single actor to prioritize safety at the cost of performance. Theater Ratio (0.40) is moderate, accounting for the phenomenon of 'safety-washing,' where firms engage in performative safety measures to appease public and regulatory concerns without fundamentally altering their capability-first approach.
 *
 * PERSPECTIVAL GAP:
 *   The gap is profound. For the capability-focused developer, the tradeoff is a simple optimization problem (Rope). For the public, it is a coercive imposition of risk (Snare). For the safety-conscious developer, it is a frustrating hybrid system that rewards recklessness (Tangled Rope). This divergence highlights that the 'nature' of the constraint is not intrinsic but is determined by an agent's structural relationship to the costs and benefits.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality `d` is derived from structural positions. The capability-focused developer is a beneficiary with arbitrage exit, yielding a low `d` and negative effective extraction (χ < 0). Global society is a victim with trapped exit, yielding a high `d` and extremely high positive extraction (χ > 1.0). The safety-conscious developer is a victim but with constrained exit (not fully trapped), resulting in a moderately high `d` and a χ value characteristic of a Tangled Rope. These derived values, not subjective assessments, drive the different classifications from a single set of base properties.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint story resolves the mandatrophy of whether AI is a 'tool' (Rope) or a 'threat' (Snare). The framework demonstrates it is both simultaneously, depending on the observer's index. By fixing the base properties (ε, suppression) and allowing the classification to vary with the indexical tuple (P,T,E,S), the system correctly models the reality that the same technology can function as a coordination mechanism for its creators while acting as a coercive, extractive force upon those who bear its externalized risks.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fundamental_tradeoff_existence,
    'Is the safety-performance tradeoff a fundamental property of intelligence, or a temporary engineering problem solvable with better alignment techniques?',
    'Discovery of alignment methods that are provably safe and computationally cheap, or a theoretical proof of the tradeoff''s necessity.',
    'If solvable, the constraint is a Scaffold that will eventually be dismantled. If fundamental, it has a Mountain-like core, and policy must focus on managing an irreducible risk.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fundamental_tradeoff_existence, empirical, 'Whether the alignment tax is a permanent feature or a temporary engineering hurdle.').

omega_variable(
    externality_magnitude,
    'What is the true societal cost (in economic, political, and existential terms) of deploying marginally less-aligned but more performant AI systems?',
    'Advanced risk modeling, longitudinal studies of AI impacts, and extensive red-teaming of deployed systems.',
    'A high cost confirms the Snare classification for the public and justifies strong regulation. A low cost suggests the tradeoff is a more manageable Rope-like coordination problem.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(externality_magnitude, empirical, 'The magnitude of risk externalized onto society by prioritizing performance over safety.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(alignment_tax_tradeoff, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(alig_tr_t0, alignment_tax_tradeoff, theater_ratio, 0, 0.1).
narrative_ontology:measurement(alig_tr_t7, alignment_tax_tradeoff, theater_ratio, 7, 0.25).
narrative_ontology:measurement(alig_tr_t15, alignment_tax_tradeoff, theater_ratio, 15, 0.4).

% Extraction over time
narrative_ontology:measurement(alig_be_t0, alignment_tax_tradeoff, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(alig_be_t7, alignment_tax_tradeoff, base_extractiveness, 7, 0.45).
narrative_ontology:measurement(alig_be_t15, alignment_tax_tradeoff, base_extractiveness, 15, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(alignment_tax_tradeoff, information_standard).
narrative_ontology:affects_constraint(alignment_tax_tradeoff, labor_market_automation).
narrative_ontology:affects_constraint(alignment_tax_tradeoff, information_ecosystem_integrity).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
