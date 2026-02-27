% ============================================================================
% CONSTRAINT STORY: fittss_law
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fittss_law, []).

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
 *   constraint_id: fittss_law
 *   human_readable: Fitts's Law (Industrial Application)
 *   domain: technological/economic
 *
 * SUMMARY:
 *   Fitts's Law—a model of rapid aimed movement stating that movement time
 *   increases logarithmically with distance and inversely with target
 *   width—is presented as a universal principle of human ergonomics when
 *   applied to industrial interface design. The constraint models the
 *   structural tension between legitimate coordination (standardized
 *   interface design, reproducible ergonomic principles) and extraction (work
 *   intensification, cognitive autonomy reduction, lock-in effects). From the
 *   interface design and productivity optimization sectors, Fitts's Law
 *   appears as a genuine coordination mechanism that solves the problem of
 *   interface consistency and justifies efficient design. From the worker's
 *   perspective, the constraint appears as a snare: mandatory compliance with
 *   scientifically-justified movement patterns that reduce autonomy and
 *   intensify output requirements. The constraint's extractiveness has grown
 *   over 30 years (from 0.18 to 0.38) as digital interfaces have become
 *   ubiquitous and mandatory, while theater_ratio has risen (from 0.35 to
 *   0.58) as the gap between the law's ergonomic promise (optimized movement
 *   = healthier, happier workers) and its actual application (optimized
 *   movement = mandatory speedup) has widened.
 *
 * KEY AGENTS:
 *   - Workers subject to Fitts-optimized design: Primary victim (powerless/trapped) — no choice in interface design, no exit from workplace geometry
 *   - Interface design industry: Primary beneficiary (institutional/arbitrage) — uses Fitts as scientific justification for design patterns; reduces iteration costs
 *   - Productivity optimization firms: Beneficiary (institutional/arbitrage) — sells Fitts-based workplace redesign and work acceleration services
 *   - Workforce collective: Secondary victim (moderate/constrained) — constrained by employment and skill lock-in; also benefits from productivity gains (job security)
 *   - Labor regulators and ergonomic standards bodies: Institutional observer (organized/constrained) — invoke Fitts in regulations; largely performative (theater=0.58)
 *   - Analytical observer: Civilizational view (analytical/analytical) — sees constraint as contingent institutional choice, not natural law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fittss_law, 0.38).
domain_priors:suppression_score(fittss_law, 0.42).
domain_priors:theater_ratio(fittss_law, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fittss_law, extractiveness, 0.38).
narrative_ontology:constraint_metric(fittss_law, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(fittss_law, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fittss_law, tangled_rope).
narrative_ontology:human_readable(fittss_law, "Fitts's Law (Industrial Application)").
narrative_ontology:topic_domain(fittss_law, "technological/economic").

domain_priors:requires_active_enforcement(fittss_law).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fittss_law, interface_designers).
narrative_ontology:constraint_beneficiary(fittss_law, productivity_optimization_firms).
narrative_ontology:constraint_victim(fittss_law, workers_subject_to_design).
narrative_ontology:constraint_victim(fittss_law, cognitive_autonomy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: OPTIMIZED WORKER (SNARE) — Subject to interface design constraints derived from Fitts's Law. No meaningful exit: must use employer-mandated tools and layouts. Cannot negotiate physical workspace geometry. d≈0.93, f(d)≈1.40, σ=1.2 → χ≈0.63. High effective extraction through mandatory compliance with scientifically-justified movement patterns.
constraint_indexing:constraint_classification(fittss_law, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: WORKFORCE COLLECTIVE (TANGLED ROPE) — Constrained by employment dependency and skill lock-in to specific interface designs. Also benefits from productivity gains (higher throughput = job security in competitive sectors). d≈0.68, f(d)≈1.05, σ=1.0 → χ≈0.40. Mixed: coordination (shared interface standards reduce training costs) and extraction (work intensification through optimized movement).
constraint_indexing:constraint_classification(fittss_law, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INTERFACE DESIGN INDUSTRY (ROPE) — Benefits from Fitts's Law as a coordination standard. Arbitrage position: can apply scientific principles to reduce design iteration costs and justify design choices to stakeholders. d≈0.08, f(d)≈-0.10, σ=1.2 → χ≈-0.04. Net beneficiary through coordination efficiency; negative effective extraction.
constraint_indexing:constraint_classification(fittss_law, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: PRODUCTIVITY OPTIMIZATION SECTOR (ROPE) — Uses Fitts's Law to justify work speedup, layout optimization, ergonomic standardization. Arbitrage: sells these services as efficiency gains. d≈0.10, f(d)≈-0.08, σ=1.2 → χ≈-0.03. Net beneficiary through scientific legitimation of productivity extraction.
constraint_indexing:constraint_classification(fittss_law, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: LABOR REGULATORS (PITON) — Fitts's Law is invoked to justify ergonomic standards and workplace design regulations, but the standards are largely performative: compliance with Fitts-based layouts coexists with other pressures (cost minimization, rapid reconfiguration for demand shifts) that override ergonomic principles. theater_ratio=0.58 indicates substantial performativity. Regulations cite scientific justification but lack enforcement and override mechanisms. d≈0.45, f(d)≈0.42, σ=1.0 → χ≈0.24.
constraint_indexing:constraint_classification(fittss_law, piton,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — Fitts's Law is a genuine coordination mechanism (interface standards, design reproducibility) layered with extraction (work intensification, cognitive load externalization, autonomy reduction). The constraint is not a natural law of ergonomics but a sociotechnical choice: movement efficiency ≠ overall human flourishing. d≈0.70, f(d)≈1.12, σ=1.2 → χ≈0.50. High effective extraction from the civilizational scope.
constraint_indexing:constraint_classification(fittss_law, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fittss_law_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(fittss_law, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(fittss_law, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(fittss_law, TR),
    TR >= 0.70.

:- end_tests(fittss_law_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The constraint exhibits genuine coordination benefits (interface standardization, reproducible design) layered with extraction (work speedup, autonomy reduction, cognitive load externalization). The value reflects that Fitts optimization does deliver measurable speed gains, which are partly passed to workers (job security in competitive sectors) and partly captured by employers (productivity extraction). The growth trajectory (0.18 → 0.38) reflects the intensification of digital ubiquity and the gap between promise (ergonomics) and practice (speedup). Suppression (0.42): Moderate. Workers cannot easily exit workplace design decisions, but the suppression is not total — ergonomic standards exist, remote work offers some escape, and labor organizing can negotiate workspace parameters. Theater_ratio (0.58): Moderate-high. The performativity lies in the gap between Fitts's promise (optimized movement improves human flourishing) and its application (optimized movement = higher output requirements). Ergonomic standards and regulations cite Fitts extensively but lack mechanisms to prevent override by cost or productivity pressures.
 *
 * PERSPECTIVAL GAP:
 *   The fundamental perspectival gap is between the designer/optimizer view (Rope: coordination problem solved scientifically) and the worker view (Snare: mandatory compliance with external optimization). The interface design industry sees a successful application of universal principles. The worker sees imposed movement patterns that eliminate discretion. The regulatory observer sees a piton: Fitts is invoked in ergonomic standards that are largely honored in violation (theater_ratio=0.58). The analytical observer sees the constraint as fundamentally contingent: Fitts optimization is one possible ergonomic choice, selected because it aligns with productivity extraction, not because it maximizes human welfare. The perspectival gap widens as extractiveness increases — early in the interval (t=0, ε=0.18), Fitts was more purely coordinating; as digital systems became mandatory (t=30, ε=0.38), the extraction component became dominant.
 *
 * DIRECTIONALITY LOGIC:
 *   Interface design industry: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiary. Productivity optimization sector: Beneficiary + arbitrage → d≈0.10, f(d)≈-0.08. Net beneficiary. Workers: Victim + trapped → d≈0.93, f(d)≈1.40. Maximum extraction. Workforce collective: Victim + constrained → d≈0.68, f(d)≈1.05. Mixed: constrained exit (employment dependency) but also benefits (job security). Labor regulators: Institutional + constrained → d≈0.45, f(d)≈0.42. Piton classification dominates; moderate extraction through performative standards. Analytical observer: Derives d from the constraint's contingency and asymmetry → d≈0.70, f(d)≈1.12. High extraction from civilizational view because the constraint naturalizes a contingent choice.
 *
 * MANDATROPHY ANALYSIS:
 *   TANGLED ROPE RESOLUTION: The constraint avoids mandatrophy (false classification as pure extraction) by explicitly modeling the genuine coordination benefits (interface standardization, reproducible design principles, reduced iteration costs) alongside the extraction mechanism (work intensification, autonomy reduction). The beneficiary/victim split is asymmetric: the interface design industry captures net benefits through efficiency and arbitrage; the worker bears costs through mandatory compliance and cognitive load. The constraint persists not because it maximizes human welfare but because it aligns productivity extraction with scientific legitimation. The theater component (0.58) reflects this gap: Fitts is invoked as ergonomic science, but the application prioritizes output optimization over worker autonomy. The mandatrophy is resolved by recognizing that Tangled Rope is the correct classification: the constraint has both a real coordination function (standardized design) and real asymmetric extraction (work speedup + autonomy reduction).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cognitive_load_neutrality,
    'Does optimizing movement time via Fitts''s Law increase or decrease overall cognitive load and decision-making burden on workers?',
    'Longitudinal cognitive load studies comparing Fitts-optimized vs non-optimized interfaces; measurement of error rates, decision time, and mental fatigue',
    'If neutral/positive: Fitts application is coordination (Rope/Tangled Rope). If negative: Fitts application is extraction mechanism (Snare from worker perspective).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cognitive_load_neutrality, empirical, 'Whether Fitts optimization affects cognitive load').

omega_variable(
    skill_transferability,
    'Do workers who master Fitts-optimized interfaces in one domain gain transferable skills or become locked into domain-specific movement patterns that don''t transfer?',
    'Cross-domain skill transfer studies; worker retraining timelines when moving between Fitts-optimized systems with different geometries or paradigms',
    'If highly transferable: workers maintain agency across employers (constrained exit → mobile). If locked in: workers face switching costs and skill obsolescence (constrained exit → trapped).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(skill_transferability, empirical, 'Whether Fitts skills transfer across domains').

omega_variable(
    optimization_asymmetry,
    'Is Fitts''s Law optimization applied symmetrically (beneficial to both worker and employer) or asymmetrically (beneficial to employer, cost to worker in reduced autonomy)?',
    'Analysis of interface design decisions: Are layouts optimized for worker speed, worker choice, or employer productivity metrics? Do workers have control over layout parameters?',
    'If symmetric: coordination-dominant (Rope). If asymmetric: extraction-dominant (Snare/Tangled Rope).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(optimization_asymmetry, conceptual, 'Whether Fitts optimization benefits are symmetric').

omega_variable(
    natural_law_vs_contingent_design,
    'Is the application of Fitts''s Law to industrial design a universal principle (natural law of movement efficiency) or a contingent institutional choice (one of many possible ergonomic frameworks)?',
    'Historical analysis of ergonomic approaches pre-Fitts; comparison with alternative frameworks (biomechanical, cognitive, autonomy-preserving); analysis of what Fitts optimization gains vs what it sacrifices',
    'If natural law: constraint is Mountain. If contingent design choice: constraint is Tangled Rope/Snare with alternatives possible.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_contingent_design, conceptual, 'Whether Fitts application is universal or contingent').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fittss_law, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fitts_tr_t0, fittss_law, theater_ratio, 0, 0.35).
narrative_ontology:measurement(fitts_tr_t15, fittss_law, theater_ratio, 15, 0.5).
narrative_ontology:measurement(fitts_tr_t30, fittss_law, theater_ratio, 30, 0.58).

% Extraction over time
narrative_ontology:measurement(fitts_be_t0, fittss_law, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(fitts_be_t15, fittss_law, base_extractiveness, 15, 0.29).
narrative_ontology:measurement(fitts_be_t30, fittss_law, base_extractiveness, 30, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fittss_law, information_standard).
narrative_ontology:affects_constraint(fittss_law, human_factors_engineering_capture).
narrative_ontology:affects_constraint(fittss_law, productivity_measurement_systems).

% DUAL FORMULATION NOTE:
% Fitts's Law as a physical principle (movement time = f(distance, width)) is a Mountain: ε≈0.05, accessibility_collapse≥0.85. Fitts's Law as an industrial application (mandatory interface design based on movement optimization) is a Tangled Rope: ε≈0.38, requires_active_enforcement=true. These are structurally distinct constraints; the industrial application story depends on the physical principle but adds institutional and extraction layers.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
