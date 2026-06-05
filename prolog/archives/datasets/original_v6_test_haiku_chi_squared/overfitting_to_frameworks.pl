% ============================================================================
% CONSTRAINT STORY: overfitting_to_frameworks
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_overfitting_to_frameworks, []).

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
 *   constraint_id: overfitting_to_frameworks
 *   human_readable: The Rigidity of the Map: Overfitting to Evaluative Frameworks
 *   domain: technological/cognitive/organizational
 *
 * SUMMARY:
 *   The rigidity of the map occurs when organizations or evaluation systems
 *   optimize behavior so tightly to a specific metric or framework that the
 *   framework decouples from the external reality it is meant to represent. A
 *   standardized test optimized for test-taking, a healthcare system
 *   optimized for billable procedures, a school system optimized for
 *   graduation rates, a research funding system optimized for citation
 *   metrics, or a corporate performance system optimized for quarterly
 *   earnings — each exhibits the same structural pattern. The framework
 *   begins as coordination: it provides a common language and measurement
 *   standard. But as agents learn to optimize the metric, the framework
 *   becomes a Goodhart trap. The map is no longer territory — it is an
 *   artifact that all actors are forced to navigate. Field practitioners
 *   discover that following the metric produces worse real-world outcomes;
 *   external reality (student learning, patient health, research impact,
 *   genuine earnings) drifts further from the metric; yet the institutional
 *   machinery persists in enforcing optimization toward the decoupled
 *   framework. The constraint exhibits high theater ratio (0.68) because much
 *   of the observed compliance is performative — actors go through the
 *   motions of framework-aligned behavior while privately acknowledging the
 *   gap. Over the interval, theater has risen from 0.35 to 0.68 as the gap
 *   has widened and practitioners have become more cynical about the
 *   framework's utility.
 *
 * KEY AGENTS:
 *   - Framework Custodians: Institutional beneficiaries (organized/arbitrage) — standards bodies, accreditation agencies, testing organizations that benefit from metric optimization without bearing the costs of framework-reality decoupling
 *   - Metric Optimizers: Secondary beneficiaries (moderate/constrained) — agents (researchers, managers, teachers) who gain short-term career or institutional rewards from metric optimization despite knowing the framework is decoupled
 *   - Field Practitioners: Primary victims (moderate/constrained) — mid-level agents forced to work within frameworks they recognize as misaligned; face suppression via performance reviews and compliance requirements
 *   - External Reality: Collective victim (powerless/trapped) — the actual phenomena (student learning, patient health, ecosystem integrity, research validity) that the framework is meant to represent; bears the cost of misalignment with no voice in metric selection
 *   - Critical Observer Coalition: Organized secondary actors (organized/mobile) — researchers, auditors, practitioners who see both the coordination function and the extraction; can articulate the gap but face friction in implementing alternatives
 *   - Bureaucratic Enforcement Layer: Institutional maintenance (institutional/arbitrage) — compliance machinery that sustains metric optimization through inertia, independent of whether the framework still serves its original purpose
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(overfitting_to_frameworks, 0.52).
domain_priors:suppression_score(overfitting_to_frameworks, 0.65).
domain_priors:theater_ratio(overfitting_to_frameworks, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(overfitting_to_frameworks, extractiveness, 0.52).
narrative_ontology:constraint_metric(overfitting_to_frameworks, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(overfitting_to_frameworks, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(overfitting_to_frameworks, tangled_rope).
narrative_ontology:human_readable(overfitting_to_frameworks, "The Rigidity of the Map: Overfitting to Evaluative Frameworks").
narrative_ontology:topic_domain(overfitting_to_frameworks, "technological/cognitive/organizational").

domain_priors:requires_active_enforcement(overfitting_to_frameworks).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(overfitting_to_frameworks, framework_custodians).
narrative_ontology:constraint_beneficiary(overfitting_to_frameworks, metric_optimizers).
narrative_ontology:constraint_victim(overfitting_to_frameworks, external_reality_alignment).
narrative_ontology:constraint_victim(overfitting_to_frameworks, adaptive_capacity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EXTERNAL REALITY (SNARE) — The actual environment that the framework is meant to represent. As optimization tightens around the metric, the framework decouples from reality. External reality has no voice in metric selection and no exit option. d≈0.95, f(d)≈1.42, σ=1.0 → χ≈0.74. Pure extraction: reality is the victim that bears the cost of miscalibrated frameworks.
constraint_indexing:constraint_classification(overfitting_to_frameworks, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: FIELD PRACTITIONER (SNARE) — Mid-level agents (teachers, doctors, engineers, managers) who must work within the framework daily. Face pressure to optimize metrics even when they perceive framework-reality misalignment. Exit is constrained by career, compliance, and performance review requirements. d≈0.80, f(d)≈1.20, σ=0.9 → χ≈0.63. Significant extraction: pressured to sacrifice practical judgment for metric compliance.
constraint_indexing:constraint_classification(overfitting_to_frameworks, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: CRITICAL OBSERVER COALITION (TANGLED ROPE) — Researchers, auditors, and practitioners who can exit (publish critique, move institutions, build alternative frameworks) but also benefit from understanding how frameworks function. See both the coordination function (frameworks do provide real measurement) and the extraction (overoptimization blinds the field). d≈0.55, f(d)≈0.75, σ=1.2 → χ≈0.47. Hybrid: genuine coordination value meets asymmetric extraction of attention and legitimacy.
constraint_indexing:constraint_classification(overfitting_to_frameworks, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 4: FRAMEWORK CUSTODIAN (ROPE) — Standards bodies, accreditation agencies, testing organizations that maintain and certify the framework. Benefit from standardization (coordination), minimal extraction burden because they define the rules. Can arbitrage (switch frameworks if incentives shift). d≈0.08, f(d)≈-0.08, σ=1.2 → χ≈-0.04. Net beneficiary via coordination function: the framework creates value for them.
constraint_indexing:constraint_classification(overfitting_to_frameworks, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: BUREAUCRATIC ENFORCEMENT LAYER (PITON) — Institutional machinery (compliance departments, audit trails, reporting systems) that sustains metric optimization even after field practitioners recognize the framework-reality gap. Enforcement persists through inertia: the machinery works but the original functional purpose has atrophied. theater_ratio=0.68 satisfies piton gate. d≈0.10, f(d)≈-0.05, σ=1.0 → χ≈-0.03.
constraint_indexing:constraint_classification(overfitting_to_frameworks, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(universal))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (MOUNTAIN) — From a cognitive-information-theoretic perspective, the map-reality gap appears as an immutable property of representation itself: any metric that becomes a target stops being a good metric (Goodhart's Law). This perspective risks naturalizing the constraint as an inherent limit of measurement. However, the structural data (ε=0.52, suppression=0.65, active enforcement, identified beneficiaries) reveals this as a false summit — the constraint is contingent on specific institutional choices, not inevitable.
constraint_indexing:constraint_classification(overfitting_to_frameworks, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(overfitting_to_frameworks_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(overfitting_to_frameworks, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(overfitting_to_frameworks, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(overfitting_to_frameworks, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(overfitting_to_frameworks, TR),
    TR >= 0.70.

:- end_tests(overfitting_to_frameworks_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint extracts from field practitioners (who bear the cost of framework-reality misalignment) and from external reality (which becomes increasingly misrepresented). But it is not pure extraction — the framework does provide genuine coordination value initially. The extractiveness has risen from 0.28 to 0.52 over the interval as optimization has tightened and the gap has widened. Suppression (0.65): High. Suppression comes from multiple mechanisms: (1) career dependence on metric performance (practitioners cannot safely ignore the framework without professional consequence), (2) institutional inertia (alternative frameworks are costly to implement), (3) information asymmetry (framework custodians control what gets measured), and (4) collective action problems (practitioners see the gap individually but lack coordination to demand change). Theater ratio (0.68): High and rising. Much observable behavior is compliance theater — actors perform metric-aligned action while perceiving the framework as decoupled. The rise from 0.35 to 0.68 reflects that cynicism has become normalized; the framework is maintained through institutional routine rather than genuine belief in its efficacy.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits maximum perspectival divergence. The framework custodian sees pure coordination (Rope) — they benefit from standardization and observe metrics working in their favor. The metric optimizer sees modest reward (Tangled Rope) — they benefit from metric optimization but may start to recognize the framework gap. The field practitioner sees extraction under suppression (Snare) — they are pressured to optimize metrics they know are misaligned with real outcomes and have limited exit. External reality sees pure victimhood (Snare) — the actual phenomena become increasingly misrepresented as optimization tightens. The bureaucratic enforcement layer sees a degraded ritual (Piton) — the machinery persists through inertia even as its original purpose has atrophied. The analytical observer risks seeing an immutable law (Mountain via Goodhart) — but the structural data reveals that decoupling is a contingent outcome of specific institutional choices, not an inevitable property of measurement itself. The perspectival gap arises from the fundamental asymmetry: those who define and control the framework benefit from optimization; those who must work within it and those affected by its decoupling bear the costs.
 *
 * DIRECTIONALITY LOGIC:
 *   Framework custodians: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.08. Net beneficiaries. They can define the framework and adjust it if it stops serving them; their exit is costless. Metric optimizers: Mixed (beneficiary in short term + victim in long term) + constrained → d≈0.45, f(d)≈0.55. They gain from optimization but are constrained by career dependence on the framework; they cannot easily exit without cost. Field practitioners: Victim + constrained → d≈0.80, f(d)≈1.20. Significant extraction: they must optimize metrics they recognize as decoupled and cannot exit without professional cost. External reality: Victim + trapped → d≈0.95, f(d)≈1.42. Maximum extraction: has no voice, no exit, only bears costs. Critical observer coalition: Victim + mobile → d≈0.55, f(d)≈0.75. Can exit (publish, move institutions) but also see coordination function; mobile exit reduces effective extraction. Bureaucratic enforcement layer: Institutional + arbitrage → d≈0.10, f(d)≈-0.05. Net beneficiaries via inertia; they maintain the machinery that benefits framework custodians.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION OF MANDATROPHY: This constraint is a textbook case of coordination gone extractive. The mandate is coordination (create a common measurement framework). The trap is that optimization toward the frame creates decoupling from reality, converting coordination into extraction. The mandatrophy resolves by recognizing that: (1) The framework did coordinate initially — it provided real value in standardization. (2) Optimization pressure (incentive misalignment) converted coordination into extraction — agents learned to optimize metrics rather than optimize for the underlying reality the metrics were meant to represent. (3) The extraction is sustained by suppression (career dependence, institutional inertia) that prevents practitioners from correcting the framework gap. (4) The constraint is contingent on institutional design choices, not inevitable. Alternative designs exist: frameworks with built-in sunset clauses, frameworks that include external-reality feedback loops, frameworks that explicitly discourage metric optimization (e.g., randomized-audit systems), frameworks that distribute custodianship among stakeholders. The fact that these alternatives exist means the constraint is a Tangled Rope: genuine coordination function (standardization) hybridized with asymmetric extraction (metric optimization pressure) sustained by active institutional enforcement (career reviews, compliance machinery). The false mountain view (Goodhart as inevitable law) obscures the contingent political economy of framework design and enforcement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    framework_measurement_independence,
    'Can a measurement framework retain fidelity to external reality if subject to continuous optimization pressure, or is decoupling inevitable once agents'' rewards depend on the metric?',
    'Longitudinal studies comparing framework stability and reality-alignment in optimized vs non-optimized contexts; analysis of field corrections after practitioners discover framework gaps',
    'If decoupling is inevitable: constraint is quasi-structural (near-mountain). If preventable through institutional design: constraint is contingent (snare/tangled rope). Classification depends on answer.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(framework_measurement_independence, empirical, 'Whether measurement frameworks inevitably decouple from reality under optimization pressure').

omega_variable(
    practitioner_awareness_threshold,
    'What level of practitioner awareness of framework-reality misalignment is required before the field begins active framework correction?',
    'Survey data on practitioner perception of framework utility; correlation between awareness prevalence and framework revision rates; case studies of field corrections',
    'If awareness alone drives correction: constraint is scaffold with low theater as correction begins. If awareness persists without correction: constraint is snare (enforcement suppresses action). Classification timing depends.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(practitioner_awareness_threshold, empirical, 'Critical awareness threshold for triggering framework correction').

omega_variable(
    alternative_framework_viability,
    'Do genuinely alternative evaluative frameworks exist that could capture external reality better without creating their own decoupling dynamics?',
    'Comparative analysis of framework designs; meta-study of framework revision histories; attempts to implement alternative measurement approaches',
    'If viable alternatives exist: constraint is contingent political choice (snare/tangled rope). If all frameworks eventually overfit: constraint approaches mountain status. Affects whether exit is genuinely available.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_framework_viability, conceptual, 'Whether genuinely superior alternative frameworks exist').

omega_variable(
    beneficiary_intentionality,
    'Do framework custodians and metric optimizers actively maintain the framework-reality gap for extraction, or does decoupling arise as an unintended consequence of optimization?',
    'Analysis of design choices in framework specification; interviews with framework maintainers; historical reconstruction of when gap was first detected and whether addressed',
    'If intentional: constraint is pure snare. If unintended: constraint is tangled rope (coordination gone wrong). Affects mandatrophy resolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_intentionality, preference, 'Whether framework custodians intentionally maintain decoupling').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(overfitting_to_frameworks, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(overfit_tr_t0, overfitting_to_frameworks, theater_ratio, 0, 0.35).
narrative_ontology:measurement(overfit_tr_t5, overfitting_to_frameworks, theater_ratio, 5, 0.52).
narrative_ontology:measurement(overfit_tr_t10, overfitting_to_frameworks, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(overfit_be_t0, overfitting_to_frameworks, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(overfit_be_t5, overfitting_to_frameworks, base_extractiveness, 5, 0.4).
narrative_ontology:measurement(overfit_be_t10, overfitting_to_frameworks, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(overfitting_to_frameworks, information_standard).
narrative_ontology:affects_constraint(overfitting_to_frameworks, metric_reductionism).
narrative_ontology:affects_constraint(overfitting_to_frameworks, goodharts_law_operationalization).

% DUAL FORMULATION NOTE:
% The overfitting-to-frameworks constraint is upstream of specific metric-based extractions (standardized testing, healthcare coding, research evaluation) but represents a distinct structural pattern. Downstream constraints inherit the framework rigidity and may see higher extractiveness as a result of this upstream constraint's enforcement machinery.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(overfitting_to_frameworks, institutional, 0.1).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
