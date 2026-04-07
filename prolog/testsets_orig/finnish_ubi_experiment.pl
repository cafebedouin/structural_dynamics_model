% ============================================================================
% CONSTRAINT STORY: finnish_ubi_experiment
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_finnish_ubi_experiment, []).

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
 *   constraint_id: finnish_ubi_experiment
 *   human_readable: Finnish Basic Income Experiment (2017-2018)
 *   domain: economic/social/welfare_policy
 *
 * SUMMARY:
 *   The Finnish Basic Income Experiment (2017–2018) distributes €560/month to
 *   2,000 randomly selected unemployed recipients while maintaining the
 *   existing means-tested welfare system in parallel. The experiment exhibits
 *   structural tensions characteristic of tangled rope constraints: it
 *   simultaneously provides genuine coordination benefits (simplified
 *   administration, reduced welfare stigma) and extracts legitimacy from
 *   limited experimental data to justify much larger policy claims. The
 *   constraint's extractiveness (0.52) reflects that policymakers and
 *   progressive advocates derive substantial policy legitimacy from modest
 *   experimental results, while the true fiscal burden of scaling remains
 *   uncertain. The theater ratio (0.65) indicates that narrative framing and
 *   selective outcome reporting constitute a significant portion of the
 *   experiment's public function — initial implementation is narrative-heavy,
 *   with empirical clarification occurring slowly. The constraint embeds a
 *   deeper layer: work-as-legitimacy ideology means the experiment itself
 *   must be justified primarily through employment effects, which may be the
 *   least robust measurement.
 *
 * KEY AGENTS:
 *   - Welfare recipients (2,000 individuals): Primary victims (powerless/trapped) — receive unconditional income but bear extraction of legitimacy and data while unable to verify actual treatment effects
 *   - Finnish Government / KELA Administration: Primary beneficiary (institutional/arbitrage) — simplifies welfare delivery, reduces administrative overhead, collects valuable policy data
 *   - Global progressive policy coalition (think tanks, academics, political advocates): Organized beneficiaries (organized/constrained) — extract legitimacy and narrative support for much larger UBI proposals globally
 *   - Employed workers in experimental regions: Secondary victims (moderate/constrained) — fund UBI through taxation while constrained by labor market conditions
 *   - Means-tested welfare bureaucracy: Institutional inertia actor (institutional/arbitrage) — threatens job roles and budgets; maintains inertial defense through technical arguments
 *   - Employment-as-legitimacy ideology: Civilizational-level victim (powerless/trapped) — the constraint reifies work as sole source of social legitimacy, precluding exploration of alternatives
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(finnish_ubi_experiment, 0.52).
domain_priors:suppression_score(finnish_ubi_experiment, 0.48).
domain_priors:theater_ratio(finnish_ubi_experiment, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(finnish_ubi_experiment, extractiveness, 0.52).
narrative_ontology:constraint_metric(finnish_ubi_experiment, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(finnish_ubi_experiment, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(finnish_ubi_experiment, tangled_rope).
narrative_ontology:human_readable(finnish_ubi_experiment, "Finnish Basic Income Experiment (2017-2018)").
narrative_ontology:topic_domain(finnish_ubi_experiment, "economic/social/welfare_policy").

domain_priors:requires_active_enforcement(finnish_ubi_experiment).
narrative_ontology:has_sunset_clause(finnish_ubi_experiment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(finnish_ubi_experiment, welfare_recipients).
narrative_ontology:constraint_beneficiary(finnish_ubi_experiment, labor_market_participants).
narrative_ontology:constraint_beneficiary(finnish_ubi_experiment, administrative_efficiency_gains).
narrative_ontology:constraint_victim(finnish_ubi_experiment, fiscal_sustainability_credibility).
narrative_ontology:constraint_victim(finnish_ubi_experiment, means_tested_welfare_advocates).
narrative_ontology:constraint_victim(finnish_ubi_experiment, employment_outcomes_verification).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: WELFARE-DEPENDENT INDIVIDUAL (SNARE) — Trapped in the experimental regime with no exit. Receives €560/month unconditionally but cannot opt out of the experiment. Cannot verify whether the treatment actually improves wellbeing or merely creates dependency. Extractive mechanism: researchers and policymakers extract data and legitimacy from the subject's participation while maintaining full power to terminate the benefit.
constraint_indexing:constraint_classification(finnish_ubi_experiment, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: EMPLOYED WORKER / EXPERIMENTAL REGION (TANGLED ROPE) — Constrained by regional labor market conditions and tax implications. Benefits from reduced administrative friction (UBI replaces multiple welfare programs). Extraction occurs through implicit subsidy mechanism: receives UBI if eligible while tax-paying employed workers fund expansion. Mixed coordination and extraction: UBI simplifies payment mechanisms (coordination benefit) while extracting revenue from employed cohort.
constraint_indexing:constraint_classification(finnish_ubi_experiment, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: FINNISH GOVERNMENT / KELA (ROPE) — Primary beneficiary. Experiences UBI as a coordination mechanism: consolidates fragmented means-tested programs into a single payment stream, reducing administrative overhead and verification burden. Can exit (and did, terminating the experiment in 2018). Benefits from simplified welfare delivery and data collection for policy design. Low effective extraction — the constraint solves a genuine coordination problem.
constraint_indexing:constraint_classification(finnish_ubi_experiment, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: GLOBAL PROGRESSIVE POLICY COALITION (TANGLED ROPE) — Organized actors (social democrats, inequality advocates, think tanks) see UBI as a solution to automation and precarity. Constrained by the need for scalable evidence. Extraction occurs through claim-staking: the experiment's legitimacy is extracted to justify much larger policy proposals globally, even if local results are modest or ambiguous. Active enforcement required to maintain the narrative (publishing selective analyses, emphasizing employment retention over wage effects).
constraint_indexing:constraint_classification(finnish_ubi_experiment, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: MEANS-TESTED WELFARE SYSTEM (PITON) — The experiment exists alongside and threatens the existing welfare bureaucracy. Theater ratio high (0.65): means-testing appears necessary for targeting but creates complexity, error, and stigma that UBI would eliminate. The existing system persists through institutional inertia despite known inefficiency. UBI is simultaneously a replacement mechanism and a threat to welfare professional identity and budgets. Classification reflects that the constraint (the dual system) is maintained performatively — administrators defend means-testing through technical arguments while UBI evidence mounts.
constraint_indexing:constraint_classification(finnish_ubi_experiment, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: EXPERIMENTAL RESEARCH COALITION (SCAFFOLD) — University researchers, statisticians, and evaluation experts see the experiment as a temporary coordination mechanism to resolve the 'what would UBI actually do?' question. Sunset built in: the experiment was designed to end in 2018 to make room for full-scale policy decision. Theater ratio declining as longitudinal data replaces narrative: initial implementation (theater-heavy) yields to empirical outcomes. If the sunset is executed as planned and results feed into genuine policy choice, this is genuinely scaffold.
constraint_indexing:constraint_classification(finnish_ubi_experiment, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: EMPLOYMENT-AS-LEGITIMACY IDEOLOGY (SNARE) — The constraint embeds a deeper extraction: work is framed as the primary source of social legitimacy and dignity. UBI experiments are evaluated primarily on whether they affect employment rates, reifying the ideology that human worth derives from wage labor. Trapped: any policy innovation must prove it doesn't reduce employment, preventing exploration of alternatives (reduced work hours, care work revaluation, capability expansion). The ideology extracts legitimacy from participants while precluding exit from the employment-centric frame.
constraint_indexing:constraint_classification(finnish_ubi_experiment, snare,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(finnish_ubi_experiment_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(finnish_ubi_experiment, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(finnish_ubi_experiment, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(finnish_ubi_experiment, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(finnish_ubi_experiment, TR),
    TR >= 0.70.

:- end_tests(finnish_ubi_experiment_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The experiment extracts substantial legitimacy from modest data: employment effects are minimal (no significant increase in employment or hours), yet the constraint is used to justify proposals for much larger, permanent UBI schemes in other jurisdictions. This legitimacy extraction occurs asymmetrically — policymakers and advocates benefit from being able to claim 'the Finnish experiment shows UBI works,' while the actual causal evidence is limited. The extraction increased over the interval (0.38 → 0.52) as initial narrative coverage emphasizing wellbeing and stress reduction gave way to results showing employment remained flat. Suppression (0.48): Moderate. Recipients are trapped in the experimental regime (cannot opt out), but suppression is not total because the benefit is unconditional (no behavioral requirements) and the experiment was transparent in design. Suppression derives from the fact that employment-as-legitimacy framing is implicit rather than explicit — participants and observers are constrained to evaluate success through employment metrics, not through alternative wellbeing measures. Theater ratio (0.65): Moderate-high. Significant performative content: the experiment is framed as a policy test, but the logistics (maintaining two parallel welfare systems, collecting extensive survey data, coordinating with researchers) require substantial bureaucratic theater. Initial implementation (2017) emphasized wellbeing narratives and experimental excitement (higher theater). By end (2018), empirical results dominated discourse (lower theater relative to initial period, but still substantial because the results are selectively framed).
 *
 * PERSPECTIVAL GAP:
 *   The analytical observer must navigate a landscape where the empirical results (employment effect approximately zero, modest wellbeing improvements, no significant wage effects) support different narratives depending on political commitments. A genuinely disinterested analytical perspective at global/civilizational scope sees a constraint that reifies employment-as-legitimacy while failing to produce the empirical evidence needed to resolve the fundamental question: would permanent, universal UBI differ meaningfully from temporary, targeted UBI? This gap between the local experiment (which can show effects of temporary payments to unemployed people) and the global claim (permanent UBI solves precarity and automation) is the seat of the constraint's extractiveness.
 *
 * DIRECTIONALITY LOGIC:
 *   The directional flows are complex. Recipients nominally benefit from unconditional income (d → low, beneficiary status) but lose autonomy (d → high, victim status) — the net effect derives from whether they perceive the benefit as freely given or extractive. Employed workers fund the program (d → high, victim status) but benefit from social insurance expansion (d → low, beneficiary status) — constrained exit options (regional labor markets) raise their effective extraction experience. Government derives arbitrage benefits (exit capability, administrative simplification) pushing d low. Progressive advocates experience the constraint through constrained exit (unable to walk away from the narrative they've invested in) while benefiting from legitimacy spillovers, creating mixed directionality. The employment-as-legitimacy ideology operates at civilizational scope with near-total suppression of alternatives — this is the deepest extraction, operating through what remains unthought rather than through visible enforcement.
 *
 * MANDATROPHY ANALYSIS:
 *   UNRESOLVED MANDATROPHY: The constraint exhibits features of both Rope (genuine coordination gain in welfare simplification) and Snare (extraction of legitimacy from limited evidence). The resolution depends on whether the experiment's results actually drive full-scale policy decisions or whether results are selectively used to justify predetermined conclusions. If Finland implements full UBI based on this evidence: the experiment was genuine coordination and evidence-gathering (Rope/Scaffold). If Finland abandons the experiment and other countries cite it to justify UBI proposals they fund differently: the experiment was primarily extractive (Snare/Tangled Rope). The mandatrophy remains unresolved because the constraining outcome — the actual policy decision — has not yet occurred. The theater ratio (0.65) suggests that the constraint's function is shifting from empirical investigation toward narrative justification, which indicates extraction is becoming dominant. Resolution mechanism: track whether Finland's actual welfare policy in years 3-5 post-experiment incorporates UBI elements, and whether other jurisdictions that cite the experiment implement comparable designs or fundamentally different ones.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    employment_effect_measurement_sensitivity,
    'Does the measured employment effect depend critically on how employment is counted (hours vs participation vs wage levels) and on which cohort is analyzed?',
    'Comprehensive sensitivity analysis across multiple employment definitions and subpopulation stratifications; audit of outcome variable choices by independent evaluators',
    'If highly sensitive: employment results are fragile and the experiment validates policy-maker discretion in interpretation. If robust: results provide genuine guidance for full-scale rollout.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(employment_effect_measurement_sensitivity, empirical, 'Sensitivity of employment outcomes to measurement definition').

omega_variable(
    behavioral_response_generalization,
    'Do behavioral responses (job search, hours worked, consumption patterns) observed in a temporary 2-year experiment generalize to permanent UBI?',
    'Comparison with longer-running UBI pilots (Kenya GiveDirectly, Stockton SEED); theoretical modeling of endowment effects and belief updates under temporary vs permanent treatment',
    'If generalizable: experiment results provide valid policy guidance. If not: temporary experiment primarily reveals how people behave when they expect the benefit to end, not how they''d behave under permanent UBI.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(behavioral_response_generalization, empirical, 'Whether temporary experiment effects generalize to permanent policy').

omega_variable(
    fiscal_burden_estimation_extraction,
    'Who bears the true fiscal burden of scaling the experiment to full population, and are policymakers extracting legitimacy from a small-scale experiment that they know cannot scale?',
    'Transparent fiscal modeling: cost to scale to full Finnish population with various replacement rates; comparison of official cost estimates with independent analysis; tracking of policy communications before vs after experiment completion',
    'If scaling is genuinely feasible: UBI is a coordinated policy solution. If scaling is prohibitive: the experiment is an extraction mechanism — generating legitimacy for a proposal that cannot actually be implemented.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fiscal_burden_estimation_extraction, empirical, 'Fiscal scalability of UBI to full population').

omega_variable(
    narrative_selection_bias,
    'Are positive narratives (improved wellbeing, reduced stress) being selectively amplified relative to neutral or negative empirical outcomes (minimal employment effect, no wage growth)?',
    'Meta-analysis of media coverage, policy briefs, and research publication patterns; comparison of prominence given to different outcome dimensions; tracking of narrative shifts before and after formal results release',
    'If selection bias is large: the constraint is primarily extractive (generating legitimacy for a predetermined narrative). If balanced: the constraint is genuinely informative coordination.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(narrative_selection_bias, empirical, 'Narrative selectivity in experiment communication').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(finnish_ubi_experiment, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ubi_fin_tr_t0, finnish_ubi_experiment, theater_ratio, 0, 0.4).
narrative_ontology:measurement(ubi_fin_tr_t12, finnish_ubi_experiment, theater_ratio, 12, 0.55).
narrative_ontology:measurement(ubi_fin_tr_t24, finnish_ubi_experiment, theater_ratio, 24, 0.65).

% Extraction over time
narrative_ontology:measurement(ubi_fin_be_t0, finnish_ubi_experiment, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(ubi_fin_be_t12, finnish_ubi_experiment, base_extractiveness, 12, 0.45).
narrative_ontology:measurement(ubi_fin_be_t24, finnish_ubi_experiment, base_extractiveness, 24, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(finnish_ubi_experiment, resource_allocation).
narrative_ontology:affects_constraint(finnish_ubi_experiment, welfare_bureaucracy_efficiency).
narrative_ontology:affects_constraint(finnish_ubi_experiment, employment_legitimacy_ideology).
narrative_ontology:affects_constraint(finnish_ubi_experiment, global_ubi_movement_rhetoric).

% DUAL FORMULATION NOTE:
% The Finnish UBI experiment can be decomposed into two structurally distinct constraints: (1) welfare_simplification_coordination (ε ≈ 0.15, Rope) — the genuine coordination gain of replacing fragmented means-tested programs with unified UBI administration, and (2) ubi_legitimacy_extraction (ε ≈ 0.60, Snare) — the extraction of policy legitimacy from limited experimental evidence to justify much larger claims globally. The present constraint story treats them as unified (ε = 0.52, Tangled Rope) because they are empirically entangled in the same policy mechanism. Separate stories with independent ε values would artificially decompose what is genuinely a hybrid constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(finnish_ubi_experiment, powerless, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
