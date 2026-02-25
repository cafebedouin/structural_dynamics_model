% ============================================================================
% CONSTRAINT STORY: antifragility
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_antifragility, []).

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
 *   constraint_id: antifragility
 *   human_readable: Antifragility (Gaining from Disorder)
 *   domain: technological/economic/biological
 *
 * SUMMARY:
 *   Antifragility describes the property of systems that gain capability from
 *   stressors, shocks, and volatility. This is distinct from resilience
 *   (recovering) or robustness (resisting). The core structural tension
 *   arises because the system's gain in strength is often derived from the
 *   harm or failure of its individual components. This creates a dynamic of
 *   internal extraction for external benefit, where the system as a whole
 *   benefits at the direct cost of its most fragile parts.
 *
 * KEY AGENTS:
 *   - Complex Adaptive Systems: Primary beneficiary (abstract) — The ecosystem, market, or organization that becomes more resilient.
 *   - System Architects: Primary beneficiary (institutional/arbitrage) — Agents who design or manage systems to be antifragile (e.g., chaos engineers).
 *   - Individual Fragile Components: Primary victim (powerless/trapped) — The parts of the system that are damaged or destroyed by stressors.
 *   - Over-Optimized Incumbents: Secondary victim (powerful/constrained) — Brittle, established entities that are harmed by volatility.
 *   - Legacy System Maintainers: Institutional actor (institutional/constrained) — Regulators or managers who try to suppress volatility, often performatively.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(antifragility, 0.5).
domain_priors:suppression_score(antifragility, 0.65).
domain_priors:theater_ratio(antifragility, 0.75).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(antifragility, extractiveness, 0.5).
narrative_ontology:constraint_metric(antifragility, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(antifragility, theater_ratio, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(antifragility, tangled_rope).
narrative_ontology:human_readable(antifragility, "Antifragility (Gaining from Disorder)").
narrative_ontology:topic_domain(antifragility, "technological/economic/biological").

domain_priors:requires_active_enforcement(antifragility).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(antifragility, complex_adaptive_systems).
narrative_ontology:constraint_beneficiary(antifragility, evolutionary_processes).
narrative_ontology:constraint_beneficiary(antifragility, system_architects).
narrative_ontology:constraint_victim(antifragility, individual_fragile_components).
narrative_ontology:constraint_victim(antifragility, over_optimized_incumbents).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FRAGILE COMPONENT (SNARE) — A small business failing in a recession, a species going extinct, or a single server terminated by Chaos Monkey. From this perspective, the 'stressor' that strengthens the system is a catastrophic, uncompensated event. The component is sacrificed for the benefit of the whole. d≈0.95, f(d)≈1.42, σ=1.0 → χ≈0.71.
constraint_indexing:constraint_classification(antifragility, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: SYSTEM ARCHITECT (ROPE) — An engineer implementing chaos engineering, a central banker allowing creative destruction, or evolution itself. This agent benefits from the increased resilience of the total system and sees the imposed stress as a pure coordination mechanism to achieve that goal. d≈0.05, f(d)≈-0.12, σ=1.2 → χ≈-0.07. The negative extraction indicates a net subsidy.
constraint_indexing:constraint_classification(antifragility, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: RESILIENT COMPONENT (TANGLED ROPE) — A muscle fiber growing stronger after being stressed, or a company that successfully adapts during a downturn. This agent experiences both the cost of the stressor and the benefit of adaptation. It is a victim, but its mobility allows it to navigate the constraint. d≈0.85, f(d)≈1.15, σ=0.9 → χ≈0.52.
constraint_indexing:constraint_classification(antifragility, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 4: LEGACY MAINTAINER (PITON) — A regulator trying to bail out failing, fragile incumbents to prevent volatility. Their actions are largely performative attempts to preserve a system that has lost its function, fighting against the natural antifragile dynamics of the market/ecosystem. The theater_ratio of 0.75 meets the Piton gate (≥0.70).
constraint_indexing:constraint_classification(antifragility, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER (MOUNTAIN) — This perspective frames antifragility as an immutable, amoral law of complex systems, akin to natural selection. It naturalizes the extraction by viewing it as a fundamental feature of reality. The engine will flag this as a false summit, as the base properties (ε=0.50, suppression=0.65) are inconsistent with a true Mountain.
constraint_indexing:constraint_classification(antifragility, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(antifragility_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(antifragility, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(antifragility, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(antifragility, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(antifragility, TR),
    TR >= 0.70.

:- end_tests(antifragility_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (ε=0.50): Represents the significant cost imposed on sacrificed components for the benefit of the whole system. Suppression (0.65): High, as individual components within such a system often cannot opt out of being exposed to potentially fatal stressors. Theater Ratio (0.75): High, reflecting the co-opting of 'antifragility' as a management buzzword, often applied performatively without implementing the necessary structural properties like redundancy and decentralized control. The ratio has increased over the interval as the term gained popularity.
 *
 * PERSPECTIVAL GAP:
 *   The gap is profound. The System Architect, who can deploy stressors from a position of safety, sees a beneficial coordination mechanism (Rope). The fragile component that is destroyed by that same stressor experiences a pure extractive trap (Snare). The component that survives and adapts sees a mix of harm and benefit (Tangled Rope). A regulator trying to prevent this 'creative destruction' engages in performative, inertial actions (Piton). Finally, an observer can mistake this brutal extractive process for an amoral law of nature (Mountain), a classification the engine identifies as a false summit.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (System Architects) with arbitrage exit options have a low 'd' value, resulting in negative effective extraction (χ < 0), classifying the constraint as a Rope. Victims (Fragile Components) with trapped exit options have a high 'd' value, leading to high positive effective extraction (χ > 0.66), classifying it as a Snare. The analytical observer's 'Mountain' classification is a perspectival error, stemming from framing a contingent, extractive process as a natural law.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy between 'natural law' and 'extractive system' by showing they can be perspectives on the same phenomenon. The analytical tendency to classify antifragility as a Mountain is a naturalization error. The framework correctly identifies the underlying structure as a Tangled Rope (ε=0.50, suppression=0.65) and demonstrates that the Mountain view is a false summit, while the Snare view is the ground-truth experience for the system's powerless components.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    extraction_necessity,
    'Is the sacrifice of components a necessary feature of antifragility, or a bug of its current implementations?',
    'Formal modeling of complex systems to find pathways to systemic resilience without component failure; empirical study of systems that adapt via non-destructive transformation.',
    'If sacrifice is necessary, the constraint is fundamentally a Tangled Rope/Snare. If not, it could be re-engineered into a Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_necessity, conceptual, 'Whether antifragility requires sacrificial components').

omega_variable(
    scale_invariance,
    'Does antifragility at one scale create fragility at a higher scale?',
    'Multi-scale analysis of economic and ecological systems. For example, does a highly antifragile financial sector (that profits from volatility) make the overall economy more fragile?',
    'If the property is not scale-invariant, then the ''beneficiary'' at one level may be the ''victim'' at another, changing the entire directionality map.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(scale_invariance, empirical, 'Whether antifragility at one scale induces fragility at another').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(antifragility, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anti_tr_t0, antifragility, theater_ratio, 0, 0.5).
narrative_ontology:measurement(anti_tr_t5, antifragility, theater_ratio, 5, 0.65).
narrative_ontology:measurement(anti_tr_t10, antifragility, theater_ratio, 10, 0.75).

% Extraction over time
narrative_ontology:measurement(anti_be_t0, antifragility, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(anti_be_t5, antifragility, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(anti_be_t10, antifragility, base_extractiveness, 10, 0.5).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(antifragility, resource_allocation).
narrative_ontology:affects_constraint(antifragility, market_creative_destruction).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
