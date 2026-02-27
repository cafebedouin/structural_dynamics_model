% ============================================================================
% CONSTRAINT STORY: maladaptive_selection_process
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_maladaptive_selection_process, []).

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
 *   constraint_id: maladaptive_selection_process
 *   human_readable: The Evolutionary Dead-End: Maladaptive Selection Process
 *   domain: organizational/technological
 *
 * SUMMARY:
 *   The Evolutionary Dead-End describes a widespread organizational and
 *   technological failure mode: a system (typically claiming to be a pure
 *   coordination mechanism — a Rope) implements selection criteria that
 *   produce short-term optimization at the cost of long-term viability. The
 *   decoupling is not accidental — it emerges from the structural incentive
 *   asymmetry between those who design the selection mechanism (benefiting
 *   from immediate control and predictability) and those bearing the cost of
 *   system failure (the future, the powerless, the adaptive variants that
 *   cannot reach selection). The constraint exhibits the full lifecycle of
 *   institutional degradation: it begins as a genuine coordination mechanism
 *   (Rope, theater=0.35), evolves into mixed coordination and extraction
 *   (Tangled Rope, theater=0.52), and eventually becomes largely performative
 *   theater masking structural collapse (Piton, theater=0.68). Classic
 *   examples include: organizations optimizing for quarterly earnings at the
 *   expense of R&D (financial services), technological platforms optimizing
 *   for engagement metrics that predict user harm (social media), academic
 *   systems optimizing for publication counts that destroy research quality
 *   (academia), military organizations optimizing for weapons readiness
 *   metrics that degrade strategic flexibility (defense), and biological
 *   evolutionary systems where local optimization traps populations in
 *   maladaptive equilibria (conservation biology). The constraint's theater
 *   ratio (0.68) reflects that by the final interval, significant
 *   organizational effort is devoted to defending the metric against evidence
 *   of its failure — staff explain discrepancies, redefine variables, adjust
 *   measurement procedures, create workarounds — all theater that preserves
 *   the metric's apparent functionality despite its decoupling from actual
 *   system health.
 *
 * KEY AGENTS:
 *   - Selection Mechanism Owner: Institutional beneficiary (institutional/arbitrage) — controls metric design, captures predictability and governance authority
 *   - Long-term System Viability: Primary victim (powerless/trapped) — abstract collective good that cannot advocate for itself until system is already failing
 *   - Excluded Adaptive Variants: Secondary victim (powerless/trapped) — designs and strategies that would ensure survival cannot reach the selection process because they don't optimize the maladaptive metric
 *   - Implementation Engineer: Secondary actor (moderate/constrained) — caught between pressure to optimize the metric and intuitive sense that optimization doesn't correspond to system health
 *   - Organizational Pressure Valve: Institutional actor (organized/constrained) — formal and informal workarounds that maintain appearance of system functionality while the metric decouples
 *   - Reform Coalition: Powerful organized agents (powerful/mobile) — technologists, systems theorists, some forward-thinking leadership who see the metric decoupling and are building alternative measurement frameworks
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing the constraint as inevitable rather than recognizing it as a solvable design problem
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(maladaptive_selection_process, 0.52).
domain_priors:suppression_score(maladaptive_selection_process, 0.65).
domain_priors:theater_ratio(maladaptive_selection_process, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(maladaptive_selection_process, extractiveness, 0.52).
narrative_ontology:constraint_metric(maladaptive_selection_process, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(maladaptive_selection_process, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(maladaptive_selection_process, tangled_rope).
narrative_ontology:human_readable(maladaptive_selection_process, "The Evolutionary Dead-End: Maladaptive Selection Process").
narrative_ontology:topic_domain(maladaptive_selection_process, "organizational/technological").

domain_priors:requires_active_enforcement(maladaptive_selection_process).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(maladaptive_selection_process, selection_mechanism_owners).
narrative_ontology:constraint_beneficiary(maladaptive_selection_process, short_term_optimizers).
narrative_ontology:constraint_victim(maladaptive_selection_process, long_term_system_viability).
narrative_ontology:constraint_victim(maladaptive_selection_process, excluded_adaptive_variants).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ADAPTIVE VARIANT (SNARE) — The design that would ensure long-term viability has no selection mechanism to prove itself before collapse. Trapped by the decoupled fitness metric; cannot exit. Bears full cost of system failure. d≈0.92, f(d)≈1.38, σ=1.2 → χ≈0.72.
constraint_indexing:constraint_classification(maladaptive_selection_process, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: IMPLEMENTATION ENGINEER (TANGLED ROPE) — Benefits from clear selection metrics (know what to optimize); constrained by the fact that optimizing the metric does not optimize system survival. Mixed: coordination (the metrics do coordinate behavior) and extraction (the metrics extract from future viability). d≈0.68, f(d)≈1.05, σ=1.0 → χ≈0.55.
constraint_indexing:constraint_classification(maladaptive_selection_process, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: SELECTION MECHANISM OWNER (ROPE) — Experiences the constraint as pure coordination: the mechanism communicates selection pressures to the system's components. Sees the decoupling as a measurement/reporting problem, not a structural problem. d≈0.08, f(d)≈-0.08, σ=1.2 → χ≈-0.04. Net beneficiary through institutional arbitrage (designs selection criteria, captures control).
constraint_indexing:constraint_classification(maladaptive_selection_process, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ORGANIZATIONAL PRESSURE VALVE (PITON) — Workarounds, hacks, and informal corrections to the selection metric persist in practice but are increasingly performative theater. Employees know the metric is decoupled but comply through inertia and career risk. The constraint is maintained not because it works but because formal alternatives require institutional reorganization. theater_ratio=0.68. d≈0.55, f(d)≈0.75, σ=0.9 → χ≈0.35.
constraint_indexing:constraint_classification(maladaptive_selection_process, piton,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 5: REFORM COALITION (SCAFFOLD) — Powerful agents (technologists, systems theorists, some leadership) see the constraint as a temporary coordination failure with a sunset: new measurement frameworks (capability maturity models, long-term viability metrics, forecasting systems) are building alternative selection mechanisms that will eventually bypass the maladaptive metric. d≈0.35, f(d)≈0.30, σ=1.2 → χ≈0.18. Low effective extraction because the coalition has exit options and sees an intervention point.
constraint_indexing:constraint_classification(maladaptive_selection_process, scaffold,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (MOUNTAIN) — From a deep time perspective, some selection lag and metric decoupling appears inherent to complex systems: any measurement system induces Goodhart's law (the metric becomes the goal), any learning system requires feedback delay, and any optimization has local minima. The civilizational view risks naturalizing what is actually a contingent design choice as an immutable law. However, the structural data (ε=0.52, suppression=0.65, theater=0.68, requires_active_enforcement=true) contradicts the mountain classification — the engine will compute this as a false summit.
constraint_indexing:constraint_classification(maladaptive_selection_process, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(maladaptive_selection_process_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(maladaptive_selection_process, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(maladaptive_selection_process, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(maladaptive_selection_process, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(maladaptive_selection_process, TR),
    TR >= 0.70.

:- end_tests(maladaptive_selection_process_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The selection mechanism owner captures asymmetric benefits through control and predictability during the optimization phase; the costs (system degradation, loss of adaptive capacity) are distributed across the future and the excluded. The asymmetry is not as severe as a pure Snare (0.70+) because implementation engineers and organizational pressure valves maintain some capacity to work around the metric. However, the extraction is substantial — the owner imposes a constraint that extracts from long-term viability. Suppression (0.65): Significant. Multiple factors suppress the emergence of alternative selection mechanisms: institutional inertia (the metric is embedded in formal processes), career risk (those questioning the metric are seen as doubters or disloyal), difficulty of articulating alternatives (long-term viability is hard to measure precisely), and information asymmetry (the owner controls metric definitions and can redefine failure as success). Suppression increases over time as the metric becomes more entrenched. Theater ratio (0.68): High, increasing. Initially the metric is genuinely functional (theater=0.35) — it does coordinate behavior toward legitimate goals. But as the environment shifts or the metric design proves maladaptive, the system invests increasing effort in defending the metric against evidence (theater=0.52 at midpoint). By the final interval, significant organizational energy is devoted to explaining away failures and maintaining metric credibility — pure theater masking structural collapse.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates the polarization that emerges when a coordination mechanism decouples from its intended function. The selection mechanism owner perceives a pure Rope — they are solving the coordination problem of directing system behavior. The reform coalition sees a temporary Scaffold — new measurement frameworks offer a sunset path. The organizational pressure valve sees a degraded Piton — the metric persists through inertia despite acknowledged failure. The implementation engineer sees a mixed Tangled Rope — they benefit from the metric's clarity but suffer from its decoupling. The excluded adaptive variant sees pure Snare extraction — they bear the full cost while having no mechanism to prove their fitness. The civilizational analytical observer risks seeing an immutable Mountain — metric decoupling is 'inevitable in complex systems' — but the structural data reveals this as a false summit: the decoupling is a contingent result of asymmetric incentive design, not a law of nature.
 *
 * DIRECTIONALITY LOGIC:
 *   Selection mechanism owner: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.08. Net beneficiary. Controls metric design, captures governance authority, experiences coordination benefit. Long-term system viability: Victim + trapped → d≈0.92, f(d)≈1.38. Maximum extraction — abstract collective that cannot exit or organize. Excluded adaptive variants: Victim + trapped → d≈0.92, f(d)≈1.38. Cannot reach selection process; trapped by decoupled metric. Implementation engineer: Mixed beneficiary/victim + constrained → d≈0.68, f(d)≈1.05. Benefits from metric clarity; harmed by its decoupling; has some exit options (job mobility, internal dissent) but real constraints. Organizational pressure valve: Organized + constrained → d≈0.55, f(d)≈0.75. Workarounds provide some relief but do not solve the fundamental decoupling; constrained by need to maintain appearances. Reform coalition: Powerful + mobile → d≈0.35, f(d)≈0.30. Has power and exit options; sees the constraint as solvable; low effective extraction because they can shape the intervention. Analytical observer: analytical → d≈0.72, f(d)≈1.15. Mountain classification is perspectival; the engine's false summit detector should flag this.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by clearly distinguishing between two structural claims that are often conflated: (1) 'Any selection system will eventually decouple from true fitness (Mountain, inevitable)' and (2) 'This specific selection metric has decoupled from long-term system viability due to asymmetric incentive design (Tangled Rope, solvable).' The false summit error is the tendency to upgrade claim 2 to claim 1 — 'We can't do anything about metric decoupling; it's inherent to complex systems.' The structural data (ε=0.52, suppression=0.65, requires_active_enforcement=true, beneficiaries, victims) supports claim 2, not claim 1. The constraint is maintained by active enforcement (continuous investment in defending the metric) not by natural inevitability. The mountain perspective sees what is actually a design choice and tries to naturalize it. The analysis reveals that mandatrophy is resolved by refusing the naturalization — the constraint is a Tangled Rope (mixed coordination and extraction with an enforcement requirement), and it has a sunset: alternative measurement frameworks (capability maturity models, long-term viability forecasting) offer genuine exits for reform coalitions, producing the scaffold structure from their perspective. The system is not trapped in an evolutionary dead-end by nature; it is trapped by a specific institutional choice to couple selection to short-term metrics. That choice can be unmade.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    metric_decoupling_visibility,
    'At what point does the decoupling between selection metric and long-term outcome become visible to the system''s decision-makers?',
    'Historical case analysis: timeline from metric adoption to first evidence of decoupling to first acknowledgment by leadership; comparative analysis of organizations that detected decoupling early vs late',
    'If visibility emerges <5 years: selection mechanism can self-correct (Scaffold rather than Snare). If visibility emerges >15 years: lock-in and cascading failures dominate (pure Snare from most perspectives).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(metric_decoupling_visibility, empirical, 'Timeline to visibility of metric decoupling').

omega_variable(
    selection_metric_gamification,
    'Is the decoupling in the metric itself (the metric measures the wrong thing), in the implementation (the metric is gamed), or in the environment (the metric was correct but fitness landscape shifted)?',
    'Case-by-case analysis of specific selection systems: reverse-engineer the metric''s original design intent; compare intended vs actual measurement; assess whether environment changed since design',
    'If metric defect: coordination problem solvable by redesign (Rope). If implementation gaming: extraction by optimizers (Tangled Rope or Snare). If environmental shift: system-design lag (Scaffold with external sunset).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(selection_metric_gamification, conceptual, 'Source of metric-fitness decoupling').

omega_variable(
    extinction_clock_precision,
    'Can the system''s collapse timeline be predicted with sufficient precision to trigger intervention before lock-in?',
    'Forecasting models for system degradation; empirical calibration against historical organizational/technological failures; sensitivity analysis of intervention timing',
    'If predictable: scaffold sunset can be scheduled (active intervention window exists). If unpredictable: system enters chaotic regime where rescue becomes impossible (pure snare from powerless perspective).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(extinction_clock_precision, empirical, 'Predictability of system collapse from maladaptive selection').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(maladaptive_selection_process, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(maladapt_tr_t0, maladaptive_selection_process, theater_ratio, 0, 0.35).
narrative_ontology:measurement(maladapt_tr_t5, maladaptive_selection_process, theater_ratio, 5, 0.52).
narrative_ontology:measurement(maladapt_tr_t10, maladaptive_selection_process, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(maladapt_be_t0, maladaptive_selection_process, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(maladapt_be_t5, maladaptive_selection_process, base_extractiveness, 5, 0.4).
narrative_ontology:measurement(maladapt_be_t10, maladaptive_selection_process, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(maladaptive_selection_process, resource_allocation).
narrative_ontology:affects_constraint(maladaptive_selection_process, goodharts_law_metric_corruption).
narrative_ontology:affects_constraint(maladaptive_selection_process, organizational_lock_in).

% DUAL FORMULATION NOTE:
% The maladaptive selection process is upstream of specific organizational failures (goodharts_law_metric_corruption tracks the measurement corruption; organizational_lock_in tracks the downstream institutional rigidity). The selection process has its own ε reflecting the career incentive asymmetry and suppression of alternatives; downstream constraints inherit the maladaptive metric as their causal input.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(maladaptive_selection_process, powerful, 0.38).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
