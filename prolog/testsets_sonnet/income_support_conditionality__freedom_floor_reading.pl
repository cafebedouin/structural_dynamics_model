% ============================================================================
% CONSTRAINT STORY: income_support_conditionality__freedom_floor_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_income_support_conditionality__freedom_floor_reading, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: income_support_conditionality__freedom_floor_reading
 *   human_readable: Unconditional Income Floor as Decommodifying Exit Option (Freedom Floor Reading)
 *   domain: political_economy/social_policy/labor
 *
 * SUMMARY:
 *   This story instantiates the freedom-floor reading of the
 *   income_support_conditionality kernel: unconditional income transfers are
 *   treated as decommodifying labor power by removing the coercive threat of
 *   destitution that otherwise compels acceptance of any offered job. Under
 *   this reading, workers gain a genuine, collectively-funded exit option,
 *   and the employers who previously depended on the absence of that option
 *   enter the victim set — they lose the leverage of destitution-avoidance
 *   and must compete on wages and conditions instead. This is a structurally
 *   distinct constraint from the dependency_trap_reading (which treats the
 *   same transfer as eroding work incentive and self-sufficiency) and the
 *   wage_subsidy_reading (which treats it as subsidizing employer wage
 *   suppression). Each reading has its own ε, its own beneficiary/victim
 *   structure, and its own type; they are linked only through the shared
 *   kernel, not merged into one story.
 *
 * KEY AGENTS:
 *   - low_wage_workers: primary beneficiary (powerless/mobile) — gains credible exit option
 *   - informal_care_providers: beneficiary (powerless/mobile) — unpaid labor recognized independent of wage work
 *   - low_wage_employers: primary payer (organized/constrained) — loses coercive hiring leverage
 *   - labor_intensive_industry_associations: secondary payer (powerful/mobile) — sector-level opposition
 *   - welfare_state_administrators: agenda_setter (institutional/analytical) — designs and funds the floor
 *   - labor_economists_freedom_school: analytical observer — documents the decommodification effect
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(income_support_conditionality__freedom_floor_reading, 0.18).
domain_priors:suppression_score(income_support_conditionality__freedom_floor_reading, 0.12).
domain_priors:theater_ratio(income_support_conditionality__freedom_floor_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(income_support_conditionality__freedom_floor_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(income_support_conditionality__freedom_floor_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(income_support_conditionality__freedom_floor_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(income_support_conditionality__freedom_floor_reading, accessibility_collapse, 0.2).
narrative_ontology:constraint_metric(income_support_conditionality__freedom_floor_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(income_support_conditionality__freedom_floor_reading, rope).
narrative_ontology:human_readable(income_support_conditionality__freedom_floor_reading, "Unconditional Income Floor as Decommodifying Exit Option (Freedom Floor Reading)").
narrative_ontology:topic_domain(income_support_conditionality__freedom_floor_reading, "political_economy/social_policy/labor").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(income_support_conditionality__freedom_floor_reading, '7bb0953d-3208-45ec-923f-dda6a400876d').
narrative_ontology:cs_kernel_codification('7bb0953d-3208-45ec-923f-dda6a400876d', distributed).
narrative_ontology:cs_authority_grounding('7bb0953d-3208-45ec-923f-dda6a400876d', distributed).
narrative_ontology:cs_reading_relation('7bb0953d-3208-45ec-923f-dda6a400876d', income_support_conditionality__dependency_trap_reading, coexists_with).
narrative_ontology:cs_reading_relation('7bb0953d-3208-45ec-923f-dda6a400876d', income_support_conditionality__wage_subsidy_reading, influences).
narrative_ontology:cs_axiom('7bb0953d-3208-45ec-923f-dda6a400876d', foundational, subsistence_independent_of_labor_is_positive_freedom).
narrative_ontology:cs_axiom_status(subsistence_independent_of_labor_is_positive_freedom, holdable).
narrative_ontology:cs_axiom_grounding('7bb0953d-3208-45ec-923f-dda6a400876d', subsistence_independent_of_labor_is_positive_freedom, deontological).
narrative_ontology:cs_axiom('7bb0953d-3208-45ec-923f-dda6a400876d', foundational, coercive_labor_acceptance_requires_destitution_threat).
narrative_ontology:cs_axiom_status(coercive_labor_acceptance_requires_destitution_threat, holdable).
narrative_ontology:cs_axiom_grounding('7bb0953d-3208-45ec-923f-dda6a400876d', coercive_labor_acceptance_requires_destitution_threat, empirically_contingent).
narrative_ontology:cs_reference_frame('7bb0953d-3208-45ec-923f-dda6a400876d', wage_labor_conditioned_subsistence).
narrative_ontology:cs_drift_state('7bb0953d-3208-45ec-923f-dda6a400876d', post_pilot_basic_income_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('7bb0953d-3208-45ec-923f-dda6a400876d', '').
narrative_ontology:cs_kernel_id(income_support_conditionality__freedom_floor_reading, income_support_conditionality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(income_support_conditionality__freedom_floor_reading, low_wage_workers).
narrative_ontology:constraint_beneficiary(income_support_conditionality__freedom_floor_reading, informal_care_providers).
narrative_ontology:constraint_beneficiary(income_support_conditionality__freedom_floor_reading, workers_in_precarious_sectors).
narrative_ontology:constraint_victim(income_support_conditionality__freedom_floor_reading, low_wage_employers).
narrative_ontology:constraint_victim(income_support_conditionality__freedom_floor_reading, labor_intensive_industry_associations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(income_support_conditionality__freedom_floor_reading, general_taxpayers).
narrative_ontology:constraint_victim(income_support_conditionality__freedom_floor_reading, workers_in_precarious_sectors).
narrative_ontology:constraint_victim(income_support_conditionality__freedom_floor_reading, general_taxpayers).
narrative_ontology:constraint_vindicates(income_support_conditionality__freedom_floor_reading, positive_liberty_labor_theory).
narrative_ontology:constraint_vindicates(income_support_conditionality__freedom_floor_reading, exit_option_bargaining_theory).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Previously compelled to accept any available job on offer or face destitution; the unconditional transfer establishes a subsistence floor independent of employment, converting a forced-acceptance posture into a genuine choice to accept, negotiate, or refuse a given job. Exit from a specific coercive employer becomes survivable rather than catastrophic.
narrative_ontology:constraint_stakeholder(income_support_conditionality__freedom_floor_reading, low_wage_workers, beneficiary,
    powerless, biographical, mobile, national).

% Perform unpaid caregiving and household labor previously uncompensated and structurally required to be paired with wage work for survival. The floor recognizes and subsidizes this labor directly, removing the necessity of taking market employment purely to meet subsistence needs.
narrative_ontology:constraint_stakeholder(income_support_conditionality__freedom_floor_reading, informal_care_providers, beneficiary,
    powerless, biographical, mobile, national).

% Employed in high-turnover, low-protection sectors (gig work, seasonal labor, hospitality). The floor raises their reservation wage and gives them a credible walk-away option, but they still pay into the scheme through general taxation and bear some transition friction as sector wage structures adjust.
narrative_ontology:constraint_stakeholder(income_support_conditionality__freedom_floor_reading, workers_in_precarious_sectors, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(income_support_conditionality__freedom_floor_reading, workers_in_precarious_sectors, payer).

% Previously relied on the absence of a survivable exit option to fill low-wage, high-discomfort, or exploitative positions at below-market compensation. The floor removes the coercive leverage of destitution-avoidance, forcing wage increases, working-condition improvements, or automation to fill the same roles. Their exit options are constrained by sunk investment in labor-intensive business models.
narrative_ontology:constraint_stakeholder(income_support_conditionality__freedom_floor_reading, low_wage_employers, payer,
    organized, biographical, constrained, national).

% Lobby against or seek exemptions from the floor's funding mechanisms (typically progressive taxation or capital levies), arguing labor supply contraction in low-wage sectors. Have greater capacity to relocate production, automate, or pass costs through than individual employers, but the constraint's coordination-level effect on their sector's labor pool is the object of their opposition.
narrative_ontology:constraint_stakeholder(income_support_conditionality__freedom_floor_reading, labor_intensive_industry_associations, payer,
    powerful, generational, mobile, national).

% Fund the transfer through the tax system and also stand to become recipients if their own circumstances shift into eligibility for the unconditional floor; most inhabit a position where they finance a floor they might one day need themselves.
narrative_ontology:constraint_stakeholder(income_support_conditionality__freedom_floor_reading, general_taxpayers, payer,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(income_support_conditionality__freedom_floor_reading, general_taxpayers, beneficiary).

% Design and administer the unconditional transfer mechanism, choosing the floor level, funding structure, and universality rules. Because the transfer is unconditional it requires minimal means-testing and enforcement infrastructure, distinguishing this reading from conditional welfare regimes.
narrative_ontology:constraint_stakeholder(income_support_conditionality__freedom_floor_reading, welfare_state_administrators, agenda_setter,
    institutional, generational, analytical, national).

% Study the effect of unconditional transfers on labor market power, documenting reservation-wage shifts, quit rates, and bargaining outcomes as evidence for the decommodification thesis. Their framing treats the transfer's coordination function (a credible collective exit option) as primary.
narrative_ontology:constraint_stakeholder(income_support_conditionality__freedom_floor_reading, labor_economists_freedom_school, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(income_support_conditionality__freedom_floor_reading, diffuse).
narrative_ontology:fixing_cost_class(income_support_conditionality__freedom_floor_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a universal subsistence floor that solves the collective-action problem of individually unbearable exit costs from coercive employment — no single worker can afford to refuse a bad job alone, but a floor funded collectively lets every worker refuse simultaneously, which is what gives the refusal option teeth.
% TRANSFER_FUNCTION: Moves general tax revenue (disproportionately from capital-intensive and labor-intensive firms and higher earners) to all residents unconditionally, with the practical effect of transferring bargaining leverage from employers who previously depended on destitution-avoidance to workers who can now decline specific job offers without existential risk.
% ABSENT_VOICES: Low-wage employers who depend on labor-intensive, low-margin business models are present as payers but their sector-specific transition costs (automation lag, price pass-through limits) are underrepresented in the coordination framing, which centers worker freedom rather than employer adjustment capacity.
% DISAPPEARANCE_RATIONALE: If the unconditional floor were withdrawn, workers in precarious and low-wage sectors would lose their credible exit option and revert to accepting employment terms driven by subsistence necessity rather than negotiated preference; wage floors in the affected sectors would likely fall and employer leverage over working conditions would return to its prior coercive baseline.
% FOUNDING_PROBLEM: Labor markets structurally compel participation in employment relationships regardless of terms, because the alternative to any job is destitution — this constraint was built to sever that compulsion by guaranteeing subsistence independent of employment status.
% FOUNDING_PROBLEM_CORROBORATION: Independent labor economists studying reservation-wage effects and quit-rate data in jurisdictions with basic income pilots (outside the administering agencies and outside worker advocacy groups) corroborate that the compulsion-to-accept-any-job dynamic persists in the absence of such a floor and measurably weakens where a floor exists; this is not solely asserted by beneficiary groups.
narrative_ontology:disappearance_verdict(income_support_conditionality__freedom_floor_reading, world_rearranges).
narrative_ontology:founding_problem_status(income_support_conditionality__freedom_floor_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(income_support_conditionality__freedom_floor_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(income_support_conditionality__freedom_floor_reading, 'none', 1).
narrative_ontology:epsilon_provenance(income_support_conditionality__freedom_floor_reading, 0.18, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(income_support_conditionality__freedom_floor_reading_tests).
:- end_tests(income_support_conditionality__freedom_floor_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored low (0.18) and falling over the interval because, under this reading, the constraint's operation is coordinative: it funds a shared exit option rather than extracting rents from a captured population. Suppression is low (0.12) because no party is coerced into participating in the floor itself — funding is via general taxation, not targeted coercion of specific victims, and workers are not compelled to use the floor rather than work. Theater ratio is low and falling (0.18 to 0.10) because the administrative apparatus (universal, unconditional) requires minimal means-testing theater relative to conditional welfare regimes. Accessibility collapse is modest (0.20): workers retain the option to work, negotiate, or exit, and the floor does not eliminate alternatives, it adds one. Resistance (0.35) reflects genuine organized employer opposition, which is real but insufficient under this reading to overturn the coordination function.
 *
 * PERSPECTIVAL GAP:
 *   From the low-wage worker seat, the constraint reads as liberation: a floor that converts coerced acceptance into genuine choice. From the low-wage employer seat, the same constraint reads as an imposed cost that removes previously available leverage — the engine should compute a divergent seat classification here precisely because the structural data (employer as payer with constrained exit, worker as beneficiary with newly mobile exit) supports genuinely different lived experiences of the same funding mechanism.
 *
 * DIRECTIONALITY LOGIC:
 *   Low-wage workers and informal care providers derive low d (near-beneficiary) because the floor subsidizes their subsistence independent of labor market participation and their exit options improve directly. Low-wage employers and labor-intensive industry associations derive higher d (near-target) because they bear the funding cost through taxation AND lose the specific coercive leverage the constraint was built to remove — this is a double-sided cost, not merely a tax transfer, which the beneficiary/victim declaration is designed to capture. General taxpayers sit closer to symmetric since they fund the scheme but also retain latent eligibility.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading's founding problem — involuntary labor market compulsion via destitution-avoidance — remains live wherever the floor is absent or clawed back, corroborated by independent labor-economics evidence rather than solely by beneficiary self-report. The classification as rope (rather than tangled_rope or snare) depends on the floor being genuinely unconditional and universal; if administrators reintroduce conditionality or means-testing, the mandatrophy risk is that the coordination function degrades into a targeted, enforcement-heavy mechanism resembling the dependency_trap or wage_subsidy readings — a different constraint entirely, not a drift within this one.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    labor_supply_response_direction,
    'Does unconditional income support empirically increase worker bargaining leverage and voluntary exit (this reading) or does it produce labor supply withdrawal read as dependency (sibling reading), or does it primarily flow through to employer wage suppression (sibling reading)?',
    'Longitudinal labor market data from basic income pilots and universal transfer programs: track reservation wages, quit rates, wage floors in low-wage sectors, and employer wage-setting behavior post-implementation, disaggregated by sector and controlling for macro conditions.',
    'If empirical data show falling quit rates and stagnant sector wages, the wage_subsidy_reading gains support over this reading. If data show rising reservation wages and improved working conditions in previously coercive sectors, this reading is corroborated. The kernel is genuinely under-determined until sector-specific data disambiguates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(labor_supply_response_direction, empirical, 'Which kernel reading the labor market response actually vindicates.').

omega_variable(
    employer_victim_status_stability,
    'Is treating employers as victims of this constraint stable, or does employer adaptation (automation, price pass-through, sector consolidation) neutralize the cost within a few years, converting the constraint''s effective structure toward a rope with no meaningful victim?',
    'Track employer adaptation trajectories over a 10-20 year interval: automation investment rates, sector consolidation, and profit margin recovery in affected low-wage industries.',
    'If employers fully adapt without sustained cost, the victim declaration becomes transitional rather than structural, and the constraint may shift from rope-with-transitional-victims toward pure rope over the medium term.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(employer_victim_status_stability, empirical, 'Whether employer victim status is a durable structural feature or a transitional adjustment cost.').

omega_variable(
    universality_conditionality_drift_risk,
    'Does the coordination function of this reading depend structurally on the transfer remaining unconditional and universal, such that any reintroduction of means-testing would constitute a different constraint rather than drift within this one?',
    'Compare classification outcomes for jurisdictions that begin with universal floors and subsequently add conditionality, tracking whether beneficiary/victim structure and ε shift discontinuously at the point conditionality is introduced.',
    'If conditionality reintroduction produces a discontinuous shift in beneficiary/victim structure, this confirms the ε-invariance principle requires treating conditional and unconditional transfers as separate constraints, not points on a spectrum.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(universality_conditionality_drift_risk, conceptual, 'Whether unconditionality is a load-bearing structural feature distinguishing this constraint from a conditional-transfer sibling.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(income_support_conditionality__freedom_floor_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(inco_tr_t0, income_support_conditionality__freedom_floor_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(inco_tr_t4, income_support_conditionality__freedom_floor_reading, theater_ratio, 4, 0.15).
narrative_ontology:measurement(inco_tr_t8, income_support_conditionality__freedom_floor_reading, theater_ratio, 8, 0.13).
narrative_ontology:measurement(inco_tr_t12, income_support_conditionality__freedom_floor_reading, theater_ratio, 12, 0.11).
narrative_ontology:measurement(inco_tr_t16, income_support_conditionality__freedom_floor_reading, theater_ratio, 16, 0.1).
narrative_ontology:measurement(inco_tr_t20, income_support_conditionality__freedom_floor_reading, theater_ratio, 20, 0.1).

% Extraction over time
narrative_ontology:measurement(inco_be_t0, income_support_conditionality__freedom_floor_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(inco_be_t4, income_support_conditionality__freedom_floor_reading, base_extractiveness, 4, 0.26).
narrative_ontology:measurement(inco_be_t8, income_support_conditionality__freedom_floor_reading, base_extractiveness, 8, 0.22).
narrative_ontology:measurement(inco_be_t12, income_support_conditionality__freedom_floor_reading, base_extractiveness, 12, 0.2).
narrative_ontology:measurement(inco_be_t16, income_support_conditionality__freedom_floor_reading, base_extractiveness, 16, 0.19).
narrative_ontology:measurement(inco_be_t20, income_support_conditionality__freedom_floor_reading, base_extractiveness, 20, 0.18).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(income_support_conditionality__freedom_floor_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(income_support_conditionality__freedom_floor_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(income_support_conditionality__freedom_floor_reading, 0.12).
narrative_ontology:affects_constraint(income_support_conditionality__freedom_floor_reading, income_support_conditionality__dependency_trap_reading).
narrative_ontology:affects_constraint(income_support_conditionality__freedom_floor_reading, income_support_conditionality__wage_subsidy_reading).

% DUAL FORMULATION NOTE:
% This story is one of three constraints decomposed from the colloquial label 'unconditional income support' per the ε-invariance principle. The freedom_floor_reading (this file) treats the transfer as decommodifying labor and assigns employers to the victim set. The dependency_trap_reading treats the same transfer as eroding work incentive and assigns recipients themselves to the victim set (skill atrophy, long-term dependency). The wage_subsidy_reading treats the transfer as enabling employer wage suppression and assigns the state/taxpayer to the extracted party while employers become beneficiaries. All three share a kernel (income_support_conditionality) but are structurally distinct constraints with different ε, different beneficiary/victim sets, and different claimed types (rope vs. snare vs. tangled_rope respectively, in the sibling files) — they must not be merged or averaged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
