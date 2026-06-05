% ============================================================================
% CONSTRAINT STORY: hedonic_adaptation_baseline
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hedonic_adaptation_baseline, []).

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
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: hedonic_adaptation_baseline
 *   human_readable: The Hedonic Adaptation Baseline
 *   domain: psychological/biological
 *
 * SUMMARY:
 *   Hedonic adaptation is the biological baseline from which human well-being
 *   operates. Despite major positive or negative life events — winning the
 *   lottery, surviving catastrophe, entering disability — individuals return
 *   to a relatively stable level of life satisfaction within months to a few
 *   years. This constraint is not enforced by any agent; it emerges from the
 *   intrinsic properties of the human affective system, particularly the
 *   regulation of dopaminergic and opioidergic signaling. The baseline is
 *   invariant across wealth, culture, and policy intervention, making it a
 *   prime candidate for classification as a natural law (Mountain). However,
 *   the constraint is often misunderstood as an obstacle to well-being,
 *   leading to misdirected institutional efforts to abolish it through wealth
 *   redistribution, policy reform, or pharmaceutical enhancement. The
 *   Deferential Realism framework clarifies that the baseline itself is not
 *   extractive or suppressive — it is simply the floor upon which human
 *   psychology operates. No agent benefits from adaptation; no agent is
 *   victimized by it. The constraint has zero degrees of freedom for all
 *   indices, which is the defining signature of a mountain.
 *
 * KEY AGENTS:
 *   - Individual Experiencing Adaptation: The conscious agent seeking sustained happiness; confronts the irreducible biological return trajectory (powerless/trapped/biographical)
 *   - Population-Level Observer: Researcher studying hedonic trends across demographics and centuries; confirms invariance across cultures and conditions (moderate/analytical/generational)
 *   - Neuroscientific Baseline: The mechanistic understanding of dopaminergic auto-regulation and neuroplasticity; provides the law-like explanation for adaptation (analytical/analytical/civilizational)
 *   - Institutional Frameworks: Policy and therapeutic actors (healthcare, psychology, economics) that attempt to engineer sustained well-being; discover the baseline through repeated failure to bypass it (institutional/arbitrage/civilizational)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hedonic_adaptation_baseline, 0.08).
domain_priors:suppression_score(hedonic_adaptation_baseline, 0.02).
domain_priors:theater_ratio(hedonic_adaptation_baseline, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hedonic_adaptation_baseline, extractiveness, 0.08).
narrative_ontology:constraint_metric(hedonic_adaptation_baseline, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(hedonic_adaptation_baseline, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hedonic_adaptation_baseline, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(hedonic_adaptation_baseline, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hedonic_adaptation_baseline, mountain).
narrative_ontology:human_readable(hedonic_adaptation_baseline, "The Hedonic Adaptation Baseline").
narrative_ontology:topic_domain(hedonic_adaptation_baseline, "psychological/biological").

domain_priors:emerges_naturally(hedonic_adaptation_baseline).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDIVIDUAL EXPERIENCING ADAPTATION (MOUNTAIN) — The conscious agent seeking sustained happiness confronts an irreducible biological floor. No degree of freedom exists: hedonic return occurs across all demographics, wealth levels, and life circumstances. Adaptation is not chosen or enforceable by policy; it is an intrinsic property of the affective system. d≈0.95, f(d)≈1.42, σ=1.0 → χ≈0.11. Even maximum directionality yields minimal effective extraction because base ε is so low.
constraint_indexing:constraint_classification(hedonic_adaptation_baseline, mountain,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: POPULATION-LEVEL OBSERVER (MOUNTAIN) — Across centuries, continents, and cultures, hedonic baseline shows invariance. Lottery winners, disaster survivors, and the chronically ill all exhibit the same return trajectory. The constraint is not contingent on policy, wealth redistribution, or social innovation. It is a feature of the human organism itself. d≈0.70, f(d)≈1.15, σ=1.2 → χ≈0.09. The civilizational time horizon reinforces the mountain classification — no sunset, no reform pathway.
constraint_indexing:constraint_classification(hedonic_adaptation_baseline, mountain,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 3: NEUROSCIENTIFIC BASELINE (MOUNTAIN) — At the mechanistic level, hedonic adaptation is a property of dopaminergic and opioidergic signaling: repeated exposure to a stimulus reduces the firing of reward neurons (habituation), and tonic baseline dopamine auto-regulates to a set-point determined by genetic and developmental factors. This is not extractive — it is a law of neurotransmitter biology. ε=0.08 reflects the fact that adaptation occurs whether or not anyone benefits from it. No suppression mechanism is required; the constraint operates by biological necessity. d≈0.72, f(d)≈1.15, σ=1.0 → χ≈0.09.
constraint_indexing:constraint_classification(hedonic_adaptation_baseline, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 4: INSTITUTIONAL INVARIANCE (MOUNTAIN) — No institutional arrangement — not progressive taxation, not wealth redistribution, not counseling interventions, not pharmaceutical enhancement — has successfully abolished the hedonic baseline. Institutional actors discover the baseline through repeated failure to engineer sustained happiness via policy. This is the institutional experience of a natural law: the constraint is hard, not because of enforcement, but because it is not contingent on social choice. d≈0.05, f(d)≈-0.12, σ=1.0 → χ≈-0.01. Negative effective extraction: institutions that cooperate with the baseline rather than resist it see better outcomes.
constraint_indexing:constraint_classification(hedonic_adaptation_baseline, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hedonic_adaptation_baseline_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(hedonic_adaptation_baseline, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(hedonic_adaptation_baseline, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(hedonic_adaptation_baseline, ExtMetricName, E),
    domain_priors:suppression_score(hedonic_adaptation_baseline, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(hedonic_adaptation_baseline),
    narrative_ontology:constraint_metric(hedonic_adaptation_baseline, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(hedonic_adaptation_baseline, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(hedonic_adaptation_baseline_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): Minimal. The hedonic baseline is not extractive — no agent benefits while others pay. The baseline operates uniformly across all agents. The small non-zero value reflects measurement uncertainty and the fact that understanding the baseline (e.g., via cognitive science) requires some research effort, but this is not extraction in the constraint sense; it is documentation of a natural law. Suppression (0.02): Minimal. Hedonic adaptation does not require enforcement or the elimination of alternatives. It occurs spontaneously as a property of neurotransmitter signaling. No coercive mechanism is needed. Theater ratio (0.15): Low. The baseline requires no performative maintenance. It functions without ritual or pretense. The small non-zero value reflects that psychologists and self-help industries perform 'happiness interventions' that have minimal effect on baseline but considerable theater (motivational speaking, wellness programs). The low theater score indicates that the constraint's operation is primarily functional, not performative. Accessibility collapse (0.92): Very high. The baseline is inaccessible to circumvention. All evidence suggests no agent has found a way to abolish the return trajectory. Resistance (0.08): Very low. The baseline does not resist investigation — it is easily observable and well-documented. What does not resist is the attempt to escape it, because escape is impossible.
 *
 * PERSPECTIVAL GAP:
 *   Unlike most constraints, the hedonic baseline shows minimal perspectival gap. All four perspectives (individual, population, neuroscientific, institutional) agree on the mountain classification. This consensus reflects that the baseline is genuinely invariant — not contingent on the observer's structural relationship to it. The individual experiencing adaptation and the neuroscientist studying it have radically different roles, but they observe the same phenomenon. The institutional framework tries to engineer around the baseline and fails in the same ways that individual effort fails. This lack of perspectival gap is itself a strong signature of a natural law: the constraint does not appear differently depending on where you stand; it appears the same to all observers. The only variation is in what individuals do with knowledge of the baseline — some accept it and adjust their well-being strategies accordingly (shifting from hedonic-pleasure-seeking to meaning-seeking), while others deny it and invest effort in futile resistance. But the baseline itself remains invariant.
 *
 * DIRECTIONALITY LOGIC:
 *   No directionality overrides are needed because hedonic adaptation is not a relational constraint. It does not benefit some agents at the expense of others. All agents experience adaptation with the same basic parameters. Directionality derivation for the individual (victim + trapped → d≈0.95) is technically valid but misleading: it suggests the individual is being extracted from, when in fact the individual is simply confronting the properties of their own biology. The individual is not victimized by the baseline; they are constrained by it. The distinction matters: victimization implies an agent doing the extraction, whereas a natural law applies uniformly. The low extracted value (χ≈0.11 even with d≈0.95) correctly reflects that there is no effective extraction, because base ε is so low that even maximum directionality yields minimal chi.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    individual_differences_ceiling,
    'Do substantial individual differences in baseline happiness (temperament, genetic setpoint) represent degrees of freedom within the mountain, or a range of mountain peaks at different elevations?',
    'Twin studies comparing MZ/DZ correlation in long-term life satisfaction; analysis of genetic polymorphisms associated with dopaminergic tone and baseline affect; longitudinal tracking of individuals across decades',
    'If range of peaks: the baseline is still invariant (no escape from adaptation), but the final elevation varies by individual — does not change mountain classification. If degrees of freedom: suggests hidden leverage points for long-term well-being that adaptation does not foreclose — potential scaffold exits or rope coordination mechanisms.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(individual_differences_ceiling, empirical, 'Whether individual differences in baseline represent degrees of freedom or a range of mountain peaks').

omega_variable(
    adaptation_timeline_mechanisms,
    'Are the documented timescales of hedonic return (months to years for most events) a fundamental property of neuroplasticity, or do they vary by event valence, social context, and psychological coping strategies in ways that suggest leverage points for intervention?',
    'Meta-analysis of adaptation timelines across life events (income change, marriage, disability, bereavement); tests of whether cognitive-behavioral and mindfulness interventions alter the timeline vs merely shift the post-adaptation mood; investigation of whether social support networks significantly delay adaptation',
    'If fundamental: timescale is part of the mountain specification. If malleable: psychological interventions could create a scaffold escape route, converting the mountain into tangled rope (coordination + extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(adaptation_timeline_mechanisms, empirical, 'Whether adaptation timescales are fundamental or malleable via intervention').

omega_variable(
    set_point_stability_over_lifespan,
    'Does the hedonic baseline itself shift systematically across the lifespan (childhood, adolescence, aging), or does the underlying set-point remain stable while the events triggering adaptation change?',
    'Longitudinal studies tracking life satisfaction from childhood through old age; analysis of whether aging-related changes in affect are adaptation to accumulated events or genuine set-point shifts; comparison of cohort effects vs age effects',
    'If stable set-point: mountain classification is reinforced — the baseline is truly constant. If lifespan variation: baseline might not be a mountain but a developmental sequence of constraints, each locally mountain-like but globally evolving.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(set_point_stability_over_lifespan, empirical, 'Whether hedonic set-point is stable or shifts across the lifespan').

omega_variable(
    meaning_and_purpose_escape,
    'Do individuals who report high meaning, purpose, or transcendental experiences (contemplative practitioners, community volunteers, artists with calling) show different adaptation patterns or elevated baselines compared to hedonic-pleasure-seeking populations?',
    'Comparison of long-term life satisfaction trajectories in meaning-seeking vs hedonic-focused cohorts; analysis of whether reported life satisfaction shows adaptation to events while meaning/purpose metrics remain stable; qualitative analysis of whether ''escape'' mechanisms reframe adaptation rather than prevent it',
    'If purpose/meaning provides true escape: hedonic baseline is a mountain only for hedonic-based well-being; alternative well-being metrics might be ropes (coordinated social meaning-making) or scaffolds (temporary meaning structures). If adaptation applies equally: mountain classification is robust across all well-being metrics.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(meaning_and_purpose_escape, conceptual, 'Whether meaning-seeking provides an escape from hedonic adaptation or merely reframes it').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hedonic_adaptation_baseline, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hedonic_tr_t0, hedonic_adaptation_baseline, theater_ratio, 0, 0.15).
narrative_ontology:measurement(hedonic_tr_t30, hedonic_adaptation_baseline, theater_ratio, 30, 0.15).
narrative_ontology:measurement(hedonic_tr_t60, hedonic_adaptation_baseline, theater_ratio, 60, 0.15).

% Extraction over time
narrative_ontology:measurement(hedonic_be_t0, hedonic_adaptation_baseline, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(hedonic_be_t30, hedonic_adaptation_baseline, base_extractiveness, 30, 0.08).
narrative_ontology:measurement(hedonic_be_t60, hedonic_adaptation_baseline, base_extractiveness, 60, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hedonic_adaptation_baseline, information_standard).
narrative_ontology:affects_constraint(hedonic_adaptation_baseline, well_being_policy_effectiveness).
narrative_ontology:affects_constraint(hedonic_adaptation_baseline, lottery_paradox_durability).
narrative_ontology:affects_constraint(hedonic_adaptation_baseline, disability_adaptation_mechanisms).

% DUAL FORMULATION NOTE:
% The hedonic adaptation baseline is an upstream natural law that constrains the design space for all well-being policy and intervention. Downstream constraints (well-being policy effectiveness, disability adaptation timelines) represent institutional attempts to work with or around this baseline. The network links reflect causal dependency: understanding the baseline is necessary for understanding why downstream policy constraints exist.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
