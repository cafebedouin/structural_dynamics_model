% ============================================================================
% CONSTRAINT STORY: mandatrophic_margin_collapse_diagnostic
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_mandatrophic_margin_collapse_diagnostic, []).

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
 *   constraint_id: mandatrophic_margin_collapse_diagnostic
 *   human_readable: Mandatrophy (Systemic Resilience Wasting)
 *   domain: institutional/technological
 *
 * SUMMARY:
 *   Mandatrophy is the systematic extraction of a system's resilience — its
 *   margin of safety, redundancy, and absorptive capacity — to satisfy a
 *   high-priority administrative or political mandate (cost reduction,
 *   throughput increase, efficiency metric, target achievement). The
 *   constraint is 'invisible' because the system remains nominally functional
 *   even as its capacity to handle disturbance collapses. The margin is not
 *   depleted gradually; it is actively harvested by institutional pressure to
 *   optimize toward theoretical limits. When crisis arrives — pandemic,
 *   natural disaster, supply shock, geopolitical event — the system fails not
 *   because the crisis was too large but because the margin needed to absorb
 *   it was extracted during normal times to satisfy mandates. Mandatrophy
 *   affects healthcare systems optimized to zero spare bed capacity, power
 *   grids optimized to minimal reserve margin, supply chains optimized to
 *   just-in-time delivery with no slack, staffing models optimized to minimum
 *   headcount, and financial systems optimized to maximum leverage. The
 *   extraction is structural: institutional incentives reward mandate
 *   satisfaction, and the only way to satisfy mandates is to harvest margins.
 *   The victims are the abstract system resilience and all future operators
 *   who inherit fragile infrastructure.
 *
 * KEY AGENTS:
 *   - System Resilience Margin: Primary victim (powerless/trapped) — absorbs extraction, cannot organize or exit, bears full cost when crisis arrives
 *   - Downstream Operators: Secondary victim (powerless/trapped) — hospital staff during pandemic, grid operators during weather extremes, supply chain managers during geopolitical shock; inherit fragile systems with no margin
 *   - Mandate-Enforcing Institution: Primary beneficiary (institutional/arbitrage) — captures performance gains from optimization, can shift accountability when crisis emerges, has exit through institutional mobility
 *   - Safety Advocates (Engineers, Safety Officers, Resilience Researchers): Secondary actor (organized/constrained) — recognize extraction mechanism, push back against mandate pressure, but constrained by organizational hierarchy
 *   - Resilience Theater (Degraded Institutional Form): Tertiary actor (institutional/arbitrage) — maintains performative resilience narrative while extracting margins; persists through inertia
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing mandatrophy as immutable consequence of optimization
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(mandatrophic_margin_collapse_diagnostic, 0.68).
domain_priors:suppression_score(mandatrophic_margin_collapse_diagnostic, 0.72).
domain_priors:theater_ratio(mandatrophic_margin_collapse_diagnostic, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(mandatrophic_margin_collapse_diagnostic, extractiveness, 0.68).
narrative_ontology:constraint_metric(mandatrophic_margin_collapse_diagnostic, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(mandatrophic_margin_collapse_diagnostic, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(mandatrophic_margin_collapse_diagnostic, snare).
narrative_ontology:human_readable(mandatrophic_margin_collapse_diagnostic, "Mandatrophy (Systemic Resilience Wasting)").
narrative_ontology:topic_domain(mandatrophic_margin_collapse_diagnostic, "institutional/technological").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(mandatrophic_margin_collapse_diagnostic, mandate_enforcing_institution).
narrative_ontology:constraint_victim(mandatrophic_margin_collapse_diagnostic, system_resilience_margin).
narrative_ontology:constraint_victim(mandatrophic_margin_collapse_diagnostic, downstream_operators).
narrative_ontology:constraint_victim(mandatrophic_margin_collapse_diagnostic, crisis_absorbing_capacity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SYSTEM RESILIENCE (SNARE) — The margin of safety in any complex system (power grid capacity buffer, hospital staffing redundancy, engineering safety factor, supply chain slack) cannot exit the extraction process. Mandatrophy harvests this margin to meet administrative targets, leaving the system nominally functional but fragile. When crisis arrives, the system lacks absorptive capacity. The margin bears the full cost of mandate satisfaction with no benefit and no escape.
constraint_indexing:constraint_classification(mandatrophic_margin_collapse_diagnostic, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: FUTURE CRISIS-ABSORBING CAPACITY (SNARE) — Downstream operators (hospital staff during pandemic, grid operators during weather extremes, supply chain managers during geopolitical shock) inherit a system stripped of resilience. They cannot exit the inherited fragility. Crisis exposure and harm are maximized because margins were extracted to satisfy past mandates. Structural extraction with no alternative path.
constraint_indexing:constraint_classification(mandatrophic_margin_collapse_diagnostic, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: MANDATE-ENFORCING INSTITUTION (ROPE) — From the institutional perspective, mandatrophy appears as efficient coordination: meet the target (cost reduction, throughput increase, efficiency metric) by optimizing the system to its theoretical limits. The institution benefits from apparent performance gain and mandate satisfaction. Has arbitrage exit: can declare victory, reallocate to next priority, shift accountability when crisis emerges.
constraint_indexing:constraint_classification(mandatrophic_margin_collapse_diagnostic, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: SAFETY ADVOCATES (TANGLED ROPE) — Engineers, safety officers, and resilience researchers see both coordination value (explicit targeting of efficiency gains) and extraction (illegitimate harvesting of safety margins). They push back against margin collapse but face organizational pressure to meet mandates. Constrained exit: cannot fully abandon the system but can advocate for resilience-aware mandates. Mixed experience of coordination (legitimate efficiency) and extraction (illegitimate margin capture).
constraint_indexing:constraint_classification(mandatrophic_margin_collapse_diagnostic, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: RESILIENCE THEATER (PITON) — In degraded institutional forms, 'resilience' becomes a performative concept: resilience plans are written but not funded, redundancy is designed but not implemented, safety margins are documented but not enforced. The institution maintains the rhetorical appearance of caring about resilience while extracting margins. The narrative persists through inertia despite low functional value. Theater ratio reflects this: institutions talk extensively about resilience while systematically eliminating it.
constraint_indexing:constraint_classification(mandatrophic_margin_collapse_diagnostic, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a mathematical/systems theory perspective, mandatrophy could appear as an immutable consequence of optimization: any system pushed to theoretical limits necessarily loses margin. But this naturalizes what is actually a choice: systems can be designed with redundancy, targets can include resilience metrics, mandates can be capped below theoretical limits. The mountain classification is a false summit — confusing a contingent institutional design choice with a law of nature.
constraint_indexing:constraint_classification(mandatrophic_margin_collapse_diagnostic, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(mandatrophic_margin_collapse_diagnostic_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(mandatrophic_margin_collapse_diagnostic, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(mandatrophic_margin_collapse_diagnostic, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(mandatrophic_margin_collapse_diagnostic, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(mandatrophic_margin_collapse_diagnostic, TR),
    TR >= 0.70.

:- end_tests(mandatrophic_margin_collapse_diagnostic_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. Mandatrophy systematically extracts the margin that makes systems crisis-resilient. The extraction is not a side effect — it is the intended mechanism by which mandates are satisfied. Margin extraction enables apparent performance gains (lower costs, higher throughput) while degrading real system capacity. The trajectory from 0.35 to 0.68 shows accelerating extraction as optimization pressure increases over time. Suppression (0.72): High. The extraction is suppressed through institutional opacity — margins are not explicitly tracked as separate from system function, resilience language is performative rather than structural, and the connection between margin extraction and crisis vulnerability is only revealed post-failure. Operators are not told 'we are harvesting your safety margin to meet budgets'; they are told 'we are optimizing efficiency.' Theater ratio (0.55): Moderate. Resilience becomes increasingly theatrical — plans are written, committees are formed, narratives emphasize robustness — while actual margins are eliminated. The rising theater ratio (0.32 → 0.55) reflects institutional emphasis on resilience rhetoric even as structural margin extraction accelerates. Claimed type: Snare. The constraint exhibits all three snare gates: high extractiveness (0.68 ≥ 0.46), high suppression (0.72 ≥ 0.60), and the core snare dynamic — the system's resilience bears costs with no benefits and no exit option.
 *
 * PERSPECTIVAL GAP:
 *   The mandate-enforcing institution sees coordination (Rope) — efficient allocation of resources to meet targets. The safety advocates see mixed experience (Tangled Rope) — legitimate efficiency goals but illegitimate margin extraction. Future operators see pure extraction (Snare) — they inherit fragility with no option to refuse it. The resilience theater sees its own degradation (Piton) — maintaining performative rhetoric while actual capacity collapses. The analytical observer risks seeing immutable constraint (Mountain) — 'optimization always requires margin elimination' — but the structural data reveals this as false summit: margins can be preserved through deliberate design choice, including resilience in mandate definition, or capping mandate intensity below theoretical limits. The perspectival gap reveals that what appears as coordination from the institutional view is extraction from the system resilience view because the margin-holder (abstract system capacity) cannot negotiate, cannot exit, and has no institutional voice.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary (mandate-enforcing institution) has institutional power and arbitrage exit options, yielding low directionality (d ≈ 0.15). The victims (system resilience margin, future operators) have no power and no exit, yielding high directionality (d ≈ 0.95). The safety advocates are organized but constrained by hierarchy (d ≈ 0.55). Each perspective's experienced extractiveness (chi) is computed from base extractiveness (0.68), directionality f(d), and scope modifier. The snare classification holds across all non-institutional perspectives — the extraction is structural to the mandate satisfaction mechanism.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION MECHANISM: This constraint demonstrates why mandatrophy must be resolved before classification confidence reaches publication stage. The false mountain perspective — 'margin extraction is inevitable in optimized systems' — naturalizes what is actually a contingent institutional choice. The resolution requires explicit analysis of: (1) mandate design (can mandates include resilience metrics?), (2) margin tracking (can we measure margin separately from function?), (3) institutional incentives (do current incentives reward margin preservation or margin extraction?), (4) crisis history (do past crises correlate with prior margin extraction?). Without mandatrophy resolution, institutions will classify this constraint as immutable natural law rather than remediable institutional design failure. The snare classification is correct if margins are extractable and profitable; the classification shifts to tangled_rope only if some margin preservation is institutionally incentivized. The mandatrophy is resolved by showing that margin extraction is a choice, not a law.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    margin_extraction_visibility,
    'What proportion of margin extraction is detectable through routine system monitoring vs. only revealed under crisis stress?',
    'Comparative analysis of pre-crisis system metrics vs. post-crisis failure analysis; correlation between margin consumption rate and crisis impact magnitude',
    'If most extraction is invisible until crisis: mandatrophy classification as snare is correct (suppression ≥ 0.60). If extraction is visible in real-time: classification shifts toward tangled_rope (suppression lower, but extraction still present).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(margin_extraction_visibility, empirical, 'Visibility of margin extraction before crisis revelation').

omega_variable(
    mandate_intent_distinction,
    'Can we distinguish between mandates that explicitly require margin extraction vs. mandates that incidentally produce it through optimization pressure?',
    'Analysis of mandate language, institutional intent statements, and performance metric design; comparison of explicit vs. implicit margin-extraction mandates across organizations',
    'If explicit: institution knowingly harms resilience (aggravates snare classification). If incidental: institution is structured to extract unconsciously (piton classification more likely). If mixed: tangled_rope (some explicit, some incidental).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandate_intent_distinction, conceptual, 'Whether margin extraction is explicit mandate or incidental consequence of optimization').

omega_variable(
    resilience_recovery_timeline,
    'After margin extraction, what is the typical timeline for recovery of system resilience under crisis-free conditions?',
    'Historical case studies of margin restoration in healthcare systems, infrastructure, supply chains; measurement of capital/slack rebuilding timelines post-crisis',
    'If recovery is fast (< 1 year): system damage is temporary (scaffold or tangled_rope). If recovery is slow (5+ years) or impossible: system degradation is permanent (snare). If no recovery occurs: piton (degraded institutional form persists).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(resilience_recovery_timeline, empirical, 'Timeline and feasibility of resilience margin recovery').

omega_variable(
    substitutability_of_margin,
    'Can crisis-absorbing capacity be rapidly substituted for after margin extraction, or is the margin itself irreplaceable?',
    'Analysis of crisis response scenarios; assessment of whether emergency resource mobilization can compensate for lost system margins',
    'If substitutable: snare classification is weakened (system can recover post-crisis). If irreplaceable: snare classification is confirmed (permanent extraction of function). If partially substitutable: tangled_rope (some recovery possible, but with cost).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(substitutability_of_margin, empirical, 'Whether lost margin capacity can be substituted during crisis response').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(mandatrophic_margin_collapse_diagnostic, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mand_tr_t0, mandatrophic_margin_collapse_diagnostic, theater_ratio, 0, 0.32).
narrative_ontology:measurement(mand_tr_t5, mandatrophic_margin_collapse_diagnostic, theater_ratio, 5, 0.43).
narrative_ontology:measurement(mand_tr_t10, mandatrophic_margin_collapse_diagnostic, theater_ratio, 10, 0.55).

% Extraction over time
narrative_ontology:measurement(mand_be_t0, mandatrophic_margin_collapse_diagnostic, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(mand_be_t5, mandatrophic_margin_collapse_diagnostic, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(mand_be_t10, mandatrophic_margin_collapse_diagnostic, base_extractiveness, 10, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(mandatrophic_margin_collapse_diagnostic, enforcement_mechanism).
narrative_ontology:affects_constraint(mandatrophic_margin_collapse_diagnostic, just_in_time_supply_fragility).
narrative_ontology:affects_constraint(mandatrophic_margin_collapse_diagnostic, healthcare_capacity_erosion).
narrative_ontology:affects_constraint(mandatrophic_margin_collapse_diagnostic, grid_reserve_margin_collapse).
narrative_ontology:affects_constraint(mandatrophic_margin_collapse_diagnostic, financial_leverage_accumulation).

% DUAL FORMULATION NOTE:
% Mandatrophy is a meta-constraint that structures how other domain-specific constraints are designed and enforced. It affects every constraint that involves margin or redundancy. The upstream constraints (mandate design, institutional incentive alignment) determine whether mandatrophy will manifest in specific domains (healthcare, infrastructure, finance). This story models the structural mechanism; domain-specific stories model where the extraction occurs.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(mandatrophic_margin_collapse_diagnostic, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
