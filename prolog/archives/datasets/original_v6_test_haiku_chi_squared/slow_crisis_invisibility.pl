% ============================================================================
% CONSTRAINT STORY: slow_crisis_invisibility
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_slow_crisis_invisibility, []).

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
 *   constraint_id: slow_crisis_invisibility
 *   human_readable: Generational Baseline Shift — Slow Crisis Invisibility
 *   domain: social/environmental
 *
 * SUMMARY:
 *   The generational baseline shift constraint describes a structural
 *   misalignment between the timescale of environmental degradation and the
 *   timescale of human institutional perception and response. Environmental
 *   crises that unfold across decades or centuries become invisible within
 *   political cycles measured in years and electoral terms. Each generation
 *   inherits a degraded baseline as 'normal,' suppressing awareness of
 *   cumulative loss. The constraint is enforced through a combination of
 *   cognitive limitation (humans experience time comparatively, relative to
 *   their own lifespan), institutional structure (political systems optimize
 *   for immediate electoral cycles), and feedback mechanisms (the very
 *   process of baseline shift normalizes itself, eliminating the perceptual
 *   reference points that would trigger alarm). This creates a systematic
 *   extraction from future generations and ecological commons, mediated
 *   through the enforced invisibility of slow change. The constraint exhibits
 *   both coordination and extraction functions: institutions coordinate on
 *   present-value extraction (rational behavior within immediate frames)
 *   while simultaneously suppressing the alternative coordination on
 *   long-term sustainability. The theater ratio (0.65) reflects that
 *   environmental monitoring and regulation are substantially performative —
 *   assessments and compliance rituals substitute for genuine long-term
 *   adjustment.
 *
 * KEY AGENTS:
 *   - Future Generations: Primary victim (powerless/trapped) — inherit shifted baseline as normal; cannot negotiate or exit temporal sequence
 *   - Short-Term Extraction Interests: Primary beneficiary (institutional/arbitrage) — political cycles, corporate quarterly earnings, immediate development priorities capture value; exit via arbitrage into later quarters/jurisdictions
 *   - Environmental Monitoring Coalition: Secondary actor (organized/constrained) — scientists, NGOs, long-term data institutions see both coordination function (baseline measurement) and extraction mechanism (institutional suppression); constrained but not powerless
 *   - Affected Communities: Secondary victim (moderate/mobile) — experience loss of traditional resource baselines and livelihood practices; partial mobility through migration or livelihood switching
 *   - Environmental Regulation Apparatus: Institutional observer (institutional/constrained) — designed to detect and respond to changes but has atrophied; theater-heavy compliance systems; degrades to piton perspective
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional timing misalignment as inherent cognitive limit
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(slow_crisis_invisibility, 0.58).
domain_priors:suppression_score(slow_crisis_invisibility, 0.68).
domain_priors:theater_ratio(slow_crisis_invisibility, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(slow_crisis_invisibility, extractiveness, 0.58).
narrative_ontology:constraint_metric(slow_crisis_invisibility, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(slow_crisis_invisibility, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(slow_crisis_invisibility, tangled_rope).
narrative_ontology:human_readable(slow_crisis_invisibility, "Generational Baseline Shift — Slow Crisis Invisibility").
narrative_ontology:topic_domain(slow_crisis_invisibility, "social/environmental").

domain_priors:requires_active_enforcement(slow_crisis_invisibility).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(slow_crisis_invisibility, short_term_extraction_agents).
narrative_ontology:constraint_beneficiary(slow_crisis_invisibility, immediate_benefit_interests).
narrative_ontology:constraint_victim(slow_crisis_invisibility, future_generations).
narrative_ontology:constraint_victim(slow_crisis_invisibility, environmental_commons).
narrative_ontology:constraint_victim(slow_crisis_invisibility, long_timescale_integrity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FUTURE GENERATIONS (SNARE) — Each generation inherits a shifted baseline as normal. The degraded state becomes the reference point; only comparative historical records reveal the loss. Trapped by temporal sequence; cannot exit or negotiate. Perception of deprivation is systematically suppressed by baseline shift itself. d≈0.95, f(d)≈1.42, σ=1.2 → χ≈0.98.
constraint_indexing:constraint_classification(slow_crisis_invisibility, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: SHORT-TERM EXTRACTION INTERESTS (ROPE) — Political cycles, corporate profit horizons, and quarterly earnings reports create coordination on present value extraction. Each extraction action is individually rational within the immediate frame. Institutional agents experience low effective extraction because they are the beneficiaries. d≈0.05, f(d)≈-0.12, σ=1.0 → χ≈-0.07. Negative effective extraction = net coordination benefit.
constraint_indexing:constraint_classification(slow_crisis_invisibility, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: ENVIRONMENTAL MONITORING COALITION (TANGLED ROPE) — Scientists, NGOs, and long-term data institutions see both the coordination function (baseline measurement, calibration, shared standards) and the extraction mechanism (institutional suppression of data, defunding of long-term monitoring, discounting of slow change). Constrained by institutional power asymmetry; cannot exit but have partial agency through data dissemination. d≈0.65, f(d)≈1.00, σ=1.2 → χ≈0.70.
constraint_indexing:constraint_classification(slow_crisis_invisibility, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: ENVIRONMENTAL REGULATION APPARATUS (PITON) — Regulatory frameworks (EPA standards, climate targets, biodiversity indices) are designed to detect and respond to changes, but the apparatus itself has atrophied: standards are not updated to reflect new baseline, enforcement mechanisms assume crisis-timescale responses, and institutional theater (compliance reporting, assessment rituals) substitutes for actual monitoring and adaptation. theater_ratio=0.65 indicates substantial performative content. d≈0.50, f(d)≈0.65, σ=1.0 → χ≈0.42.
constraint_indexing:constraint_classification(slow_crisis_invisibility, piton,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: AFFECTED COMMUNITIES (TANGLED ROPE) — Local populations experience both coordination benefits (shared livelihood systems, ecological knowledge) and extraction (inability to maintain traditional practices as baseline shifts, loss of resource access, climate migration pressure). Mobile exit option (migration, livelihood switching) is available but costly. Partial perception of the constraint because baseline shift naturalizes itself. d≈0.55, f(d)≈0.75, σ=0.9 → χ≈0.39.
constraint_indexing:constraint_classification(slow_crisis_invisibility, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, the constraint appears to be an inherent property of finite human perception and institutional temporality: humans cannot reliably perceive changes slower than a biographical span, and institutions cannot sustain attention to problems that mature across political cycles. This perspective risks naturalizing what is actually a structural coordination failure. Accessibility collapse ≥ 0.85, resistance ≤ 0.15 required by schema; however, structural data (ε=0.58, suppression=0.68, theater=0.65) contradicts mountain gates. Engine will compute false summit.
constraint_indexing:constraint_classification(slow_crisis_invisibility, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(slow_crisis_invisibility_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(slow_crisis_invisibility, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(slow_crisis_invisibility, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(slow_crisis_invisibility, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(slow_crisis_invisibility, TR),
    TR >= 0.70.

:- end_tests(slow_crisis_invisibility_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint enables net extraction of environmental value across generations. Immediate beneficiaries (extractive industries, development interests, short-term growth) capture rents during the invisibility window. The rents are captured not through force but through structural timing: decisions made under degraded baseline information are systematically biased toward immediate benefit. The value is not seized all at once but accumulated incrementally across each invisible year. Suppression (0.68): High. Multiple enforcement mechanisms: (1) Cognitive limit — baseline shift is psychologically normalized, erasing the reference point for alarm. (2) Institutional structure — political systems cannot sustain attention beyond electoral cycles. (3) Data suppression — long-term monitoring is chronically underfunded; trends emerge only after decades of collection. (4) Knowledge decay — each generation loses direct experience of the prior baseline; historical records require conscious maintenance. Theater ratio (0.65): Moderate-high. Environmental regulation is substantially performative. Agencies publish assessments and compliance reports (ritual), but genuine adaptation occurs at much slower pace. Standards are designed for crisis-timescale response (emergency cleanup, pollution reduction) but fail at slow-timescale adaptation (land-use transformation, species reintroduction, climate adjustment). The constraint has strengthened over the interval as environmental complexity has outpaced institutional capacity and political attention has fragmented.
 *
 * PERSPECTIVAL GAP:
 *   The primary perspectival gap is temporal: future generations see a snare (extraction, no exit), while immediate institutional beneficiaries see rope or piton (coordination on present value, degraded-but-persistent). The analytical observer risks seeing a mountain (humans are cognitively limited to biographical timescales — an inherent law), but structural data reveals this as a false summit: the constraint is not inevitable but enforced through specific institutional structures (electoral cycles, budget horizons, incentive alignment). The environmental monitoring coalition sees tangled rope — they perform coordination function (provide data, set standards) while simultaneously being suppressed by the very institutions they serve. Affected communities experience a different extraction than future generations: their loss is spatial and immediate (local ecological collapse within their lifetime) rather than temporal and abstract (baseline shift). The regulation apparatus sees its own degradation (piton) — it persists through institutional inertia and legislative mandate, but its core function (detection and response to environmental change) has decayed because the decays it was designed to detect operate below institutional timescales.
 *
 * DIRECTIONALITY LOGIC:
 *   Future generations: Victim + trapped → d≈0.95, f(d)≈1.42. Maximum extraction. Short-term extraction interests: Beneficiary + arbitrage → d≈0.05, f(d)≈-0.12. Net beneficiary through institutional timing arbitrage. Environmental monitoring coalition: Victim + constrained → d≈0.65, f(d)≈1.00. They provide data (coordination function) but are systematically discounted in policy; constrained by institutional power asymmetry. Affected communities: Victim + mobile → d≈0.55, f(d)≈0.75. Mixed: they can migrate or switch livelihoods (mobile exit), but the cost of exit is high; they also benefit from some coordination functions (shared livelihood systems, ecological knowledge networks). Regulation apparatus: Institutional + constrained → d≈0.50, f(d)≈0.65. Neither pure beneficiary nor victim; plays both coordinator (sets standards) and enforcer of baseline invisibility (maintains performative compliance). Analytical observer: analytical → d≈0.72, f(d)≈1.15. Cognitive imperceptibility thesis risks naturalizing the constraint; empirical research on institutional timescale misalignment reveals contingency.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by decomposing the natural law thesis (mountain) into institutional structure thesis (tangled rope). The analytical observer's claim that 'humans cannot perceive changes slower than a biographical span' conflates two distinct structural features: (1) individual cognitive limits (real, but weaker than often claimed — humans can track multi-decadal trends if they have institutional support), and (2) institutional incentive misalignment (political cycles force discounting of slow change regardless of cognitive capacity). The constraint is NOT an inherent cognitive limit but a choice to build institutions on short timescales. This is confirmed by counterexample: long-duration cultures and institutions (religious orders, forest management systems, indigenous governance) successfully maintained multi-generational baselines without modern communication technology. The falsity of the natural law thesis is the key diagnostic: if slow-change invisibility were inherent to human cognition, it would be invariant across cultures and institutions. Its variance across institutional designs reveals it as enforced, not inherent. The snare classification for future generations is correct: they experience extraction (loss of baseline, loss of ecological options) with no exit (born into degraded baseline). The tangled rope classification for institutional beneficiaries is correct: they coordinate on present-value extraction while simultaneously suppressing alternative coordination on long-term sustainability. The scaffold classification is strategically absent because no genuine sunset clause exists — the constraint persists across generations until institutional redesign breaks the timescale mismatch.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    baseline_perception_threshold,
    'What rate of change (% per decade or generations) becomes cognitively imperceptible to institutional decision-making?',
    'Comparative analysis of policy response times across environmental domains (ozone depletion vs ocean acidification vs Arctic albedo feedback); correlation between change rate and institutional awareness lag',
    'If threshold ≈ 3-5% per generation: most current slow crises are sub-threshold, validating snare classification for future generations. If threshold > 10% per generation: cognitive limit is not the primary constraint, shifting classification toward institutional choice (tangled rope) rather than structural inevitability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(baseline_perception_threshold, empirical, 'Cognitive imperceptibility threshold for slow environmental change').

omega_variable(
    institutional_update_cycle_decoupling,
    'Is the mismatch between political decision cycles (2-4 years) and environmental change timescales (decades to centuries) a technical coordination problem that could be solved by institutional redesign (long-term budgeting, intergenerational councils) or a fundamental constraint on human collective action?',
    'Historical case studies of long-term institutions that successfully maintained attention to multi-generational problems (forest management, levee systems, religious institutions); analysis of their structural features (succession rules, knowledge preservation, sunset clauses); assessment of whether these features are replicable in democratic political systems',
    'If solvable by redesign: constraint reclassifies from Snare/Mountain toward Scaffold (temporary, has exit path). If fundamental: snare classification confirmed; extraction is baked into temporal structure of political systems.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_update_cycle_decoupling, conceptual, 'Whether slow-change invisibility is solvable by institutional redesign').

omega_variable(
    baseline_shift_feedback_loop_closure,
    'Does the suppression of baseline awareness (through normalization of degraded conditions) create a positive feedback loop that prevents corrective perception, or are there sufficient counter-mechanisms (historical records, ecological memory, intergenerational transmission) to enable perception?',
    'Analysis of how indigenous and long-duration societies maintained awareness of multi-generational baselines (oral history, sacred sites, ecological calendars) compared to modern institutional amnesia; measurement of information retention decay rates in cultural memory',
    'If feedback loop is open: perception is difficult but possible with institutional effort (tangled rope). If feedback loop is closed: perception becomes increasingly impossible with each generational shift (compound snare). Closure implies the constraint self-amplifies over time.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(baseline_shift_feedback_loop_closure, empirical, 'Whether baseline shift creates closed positive feedback loop').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(slow_crisis_invisibility, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(slowcrisis_tr_t0, slow_crisis_invisibility, theater_ratio, 0, 0.35).
narrative_ontology:measurement(slowcrisis_tr_t30, slow_crisis_invisibility, theater_ratio, 30, 0.52).
narrative_ontology:measurement(slowcrisis_tr_t60, slow_crisis_invisibility, theater_ratio, 60, 0.65).

% Extraction over time
narrative_ontology:measurement(slowcrisis_be_t0, slow_crisis_invisibility, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(slowcrisis_be_t30, slow_crisis_invisibility, base_extractiveness, 30, 0.45).
narrative_ontology:measurement(slowcrisis_be_t60, slow_crisis_invisibility, base_extractiveness, 60, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(slow_crisis_invisibility, resource_allocation).
narrative_ontology:affects_constraint(slow_crisis_invisibility, tragedy_of_the_commons_intergenerational).
narrative_ontology:affects_constraint(slow_crisis_invisibility, political_cycle_discount_rate).
narrative_ontology:affects_constraint(slow_crisis_invisibility, ecological_memory_loss).

% DUAL FORMULATION NOTE:
% Slow crisis invisibility is upstream of specific environmental constraints (carbon cycle dynamics, biodiversity loss) but represents a distinct structural constraint on institutional perception. The visibility bottleneck is a meta-constraint that affects how well institutions can coordinate on any slow-timescale problem. Decomposition principle: if ε changes when measuring the same problem through different observables (e.g., perceptual threshold vs institutional incentive), those are separate constraints in the family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(slow_crisis_invisibility, analytical, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
