% ============================================================================
% CONSTRAINT STORY: technological_displacement_axiom_flat_control
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_technological_displacement_axiom_flat_control, []).

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
    narrative_ontology:measurement_basis/2,
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
    narrative_ontology:flat_control_of/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: technological_displacement_axiom_flat_control
 *   human_readable: The Technological Labor Transition Axiom (Displacement Offset by New, Better Jobs)
 *   domain: political_economy/labor_economics/technology_governance
 *
 * SUMMARY:
 *   The technological labor transition axiom is the widely cited historical
 *   claim that new technologies displace workers only temporarily: better
 *   jobs eventually emerge, and apprenticeship-based mobility ladders persist
 *   across the transition. This claim functions as a policy heuristic, drawn
 *   selectively from episodes like agricultural mechanization and
 *   mid-20th-century industrial automation, where over multi-decade and often
 *   intergenerational timescales, aggregate employment did recover. The axiom
 *   is invoked by technology deployers, policy incumbents, and the economics
 *   commentariat to justify deferring active transition support (wage
 *   insurance, retraining subsidy, apprenticeship-pipeline preservation) on
 *   the premise that the market will self-correct. The claim/metric
 *   divergence here is deliberate: the axiom presents itself as a settled
 *   empirical regularity (which would make it closer to a rope, a genuine
 *   coordination device around a real historical pattern), but the authored
 *   metrics describe rising extractiveness, rising theater, and rising
 *   suppression requirement over the interval as the gap between the axiom's
 *   promise and the lived experience of specific displaced cohorts widens and
 *   the political function of repeating the axiom (deferring intervention)
 *   becomes more load-bearing than its descriptive accuracy.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(technological_displacement_axiom_flat_control, 0.61).
domain_priors:suppression_score(technological_displacement_axiom_flat_control, 0.58).
domain_priors:theater_ratio(technological_displacement_axiom_flat_control, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(technological_displacement_axiom_flat_control, extractiveness, 0.61).
narrative_ontology:constraint_metric(technological_displacement_axiom_flat_control, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(technological_displacement_axiom_flat_control, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(technological_displacement_axiom_flat_control, accessibility_collapse, 0.47).
narrative_ontology:constraint_metric(technological_displacement_axiom_flat_control, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(technological_displacement_axiom_flat_control, tangled_rope).
narrative_ontology:human_readable(technological_displacement_axiom_flat_control, "The Technological Labor Transition Axiom (Displacement Offset by New, Better Jobs)").
narrative_ontology:topic_domain(technological_displacement_axiom_flat_control, "political_economy/labor_economics/technology_governance").

domain_priors:requires_active_enforcement(technological_displacement_axiom_flat_control).

% --- Construction-pair linkage (forced-flat control of a kernel) ---
narrative_ontology:flat_control_of(technological_displacement_axiom_flat_control, technological_displacement_axiom).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(technological_displacement_axiom_flat_control, technology_capital_owners).
narrative_ontology:constraint_beneficiary(technological_displacement_axiom_flat_control, policy_incumbents).
narrative_ontology:constraint_beneficiary(technological_displacement_axiom_flat_control, economics_commentariat).
narrative_ontology:constraint_victim(technological_displacement_axiom_flat_control, displaced_manual_and_clerical_workers).
narrative_ontology:constraint_victim(technological_displacement_axiom_flat_control, mid_career_tradespeople).
narrative_ontology:constraint_victim(technological_displacement_axiom_flat_control, apprenticeship_dependent_entrants).
narrative_ontology:constraint_vindicates(technological_displacement_axiom_flat_control, creative_destruction_doctrine).
narrative_ontology:constraint_vindicates(technological_displacement_axiom_flat_control, labor_market_self_correction_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Deploy automating and displacing technologies while citing the historical axiom to defer regulatory intervention, retraining mandates, or transition taxation. Capture productivity gains immediately; the promised 'new, better jobs' arrive, if at all, on a timeline and in a geography the owners do not have to internalize the cost of bridging.
narrative_ontology:constraint_stakeholder(technological_displacement_axiom_flat_control, technology_capital_owners, beneficiary,
    institutional, generational, arbitrage, global).

% Legislators, labor ministries, and central bankers invoke the axiom to justify light-touch industrial policy — betting that markets will self-correct rather than committing public funds to transition support. Political cover is real: citing historical precedent (agriculture, manufacturing) lets them defer costly intervention while claiming continuity with past adjustment episodes.
narrative_ontology:constraint_stakeholder(technological_displacement_axiom_flat_control, policy_incumbents, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(technological_displacement_axiom_flat_control, policy_incumbents, beneficiary).

% Academic economists, think tanks, and financial press repeat the axiom as settled historical pattern in testimony, editorials, and models. Their professional standing and consulting income are partly built on being the interpreters of this historical pattern; repudiating it devalues their own expertise and forecasting franchise.
narrative_ontology:constraint_stakeholder(technological_displacement_axiom_flat_control, economics_commentariat, beneficiary,
    organized, biographical, arbitrage, national).

% Lose jobs to automation, offshoring-enabled software, or algorithmic substitution in a specific labor market and time window. The 'new jobs' the axiom promises materialize, if at all, in different regions, different skill categories, and often a full career-cohort later — arriving too late to catch this generation of displaced workers, who bear the transition cost directly and immediately.
narrative_ontology:constraint_stakeholder(technological_displacement_axiom_flat_control, displaced_manual_and_clerical_workers, payer,
    powerless, biographical, trapped, regional).

% Skilled workers 15-25 years into a trade find their craft partially automated or de-skilled by new tooling. Too invested to retrain from scratch, too experienced to be entry-level again, they absorb wage compression and status loss while being told by policy and commentary that the transition is historically normal and self-resolving.
narrative_ontology:constraint_stakeholder(technological_displacement_axiom_flat_control, mid_career_tradespeople, payer,
    moderate, biographical, constrained, regional).

% Young workers who would have entered a trade through apprenticeship find the apprenticeship pipeline itself hollowed out — fewer master craftspeople hiring apprentices because automation reduced the crew size needed. The axiom's second clause (apprenticeship-based mobility survives) is precisely what fails for this group; they inherit the disappearance of the ladder the axiom assumes still exists.
narrative_ontology:constraint_stakeholder(technological_displacement_axiom_flat_control, apprenticeship_dependent_entrants, payer,
    powerless, biographical, trapped, regional).

% Economic historians who study the actual multi-decade lag structures, geographic mismatch, and skill-mismatch of past transitions (textiles, agriculture, manufacturing) are rarely invited into the policy rooms where the axiom is invoked as settled precedent; their more qualified, lagged, and uneven account of history is available but structurally sidelined by the cleaner policy-usable version.
narrative_ontology:constraint_stakeholder(technological_displacement_axiom_flat_control, labor_historians, excluded,
    analytical, generational, analytical, global).

% A minority within the economics profession who model transition lags, skill mismatch persistence, and geographic immobility rather than assuming frictionless reallocation. They publish counter-evidence but do not control the policy-facing narrative or its funding structures.
narrative_ontology:constraint_stakeholder(technological_displacement_axiom_flat_control, labor_economists_dissenting, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(technological_displacement_axiom_flat_control, technology_capital_owners).
narrative_ontology:fixing_cost_class(technological_displacement_axiom_flat_control, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The axiom genuinely coordinates expectations: it lets firms invest in new technology, workers plan around an implicit social contract, and policymakers avoid premature intervention, on the shared belief that displacement is temporary and mobility ladders persist. Historically, some technological transitions (agriculture to manufacturing over generations) did roughly fit this pattern, which gives the axiom real evidentiary grounding for some readings of some cases.
% TRANSFER_FUNCTION: Moves adjustment costs (wage loss, retraining expense, geographic relocation burden, apprenticeship-ladder loss) from technology deployers and policy incumbents onto displaced and mid-transition workers, by deferring collective intervention on the premise that the market will self-correct within a tolerable timeframe — a timeframe that is asserted, not measured, for the specific transition at hand.
% ABSENT_VOICES: Displaced workers experiencing the current transition, labor historians with lagged/uneven data, and apprenticeship-pipeline entrants whose ladder has already collapsed are rarely in the room where the axiom is invoked to justify policy inaction; their empirical counter-evidence exists but is structurally excluded from the policy-facing conversation, which favors the axiom's cleaner historical narrative.
% DISAPPEARANCE_RATIONALE: If the axiom's authority collapsed — if policymakers and firms stopped treating 'displacement is temporary, new jobs arrive, mobility survives' as a default historical prior — active transition-support policy (wage insurance, mandated retraining funding, apprenticeship subsidy, geographic relocation support) would become the default posture rather than the exception, and technology deployers would internalize transition costs they currently externalize by appeal to historical precedent.
% FOUNDING_PROBLEM: The axiom generalizes from genuine historical episodes (agricultural mechanization, industrial automation) where, over multi-decade horizons and often across generational cohorts, aggregate employment did recover and new occupational categories did emerge — a real pattern that policymakers reasonably wanted a durable heuristic for, so they would not over-intervene in every technological change.
% FOUNDING_PROBLEM_CORROBORATION: Technology capital owners, policy incumbents, and much of the economics commentariat attest the pattern still holds and cite historical transitions as evidence. Labor historians studying the actual multi-decade, geographically uneven, and often intergenerational lag structure of past transitions — along with dissenting labor economists modeling persistent skill and geographic mismatch — attest that the pattern historically held only unevenly, with significant permanently-worse-off cohorts, and that the apprenticeship-mobility clause specifically fails when automation reduces the crew sizes that sustain apprenticeship pipelines. This corroboration comes from outside the beneficiary set.
narrative_ontology:disappearance_verdict(technological_displacement_axiom_flat_control, world_rearranges).
narrative_ontology:founding_problem_status(technological_displacement_axiom_flat_control, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(technological_displacement_axiom_flat_control, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-13',
    'unspecified', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'unspecified').
narrative_ontology:story_seed(technological_displacement_axiom_flat_control, 'none', 1).
narrative_ontology:epsilon_provenance(technological_displacement_axiom_flat_control, 0.61, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(technological_displacement_axiom_flat_control_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(technological_displacement_axiom_flat_control, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(technological_displacement_axiom_flat_control_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.61 at interval end) reflects that displaced workers, mid-career tradespeople, and apprenticeship entrants bear real, immediate, geographically concentrated costs while the promised offsetting jobs are diffuse, delayed, and often inaccessible to the specific cohort that was displaced — a structural mismatch the axiom's framing elides by operating at the level of aggregate, multi-decade statistics rather than the cohort-and-region level where the cost actually lands. Suppression (0.58) is moderate-high: there is no formal coercion, but the axiom's institutional repetition in policy testimony, financial press, and economic modeling forecloses funding for active transition support, which functions as a suppression of the alternative (a managed-transition policy regime) even without direct coercion of workers. Theater ratio (0.52, crossing the Goodhart threshold) reflects that citing the axiom increasingly substitutes for actually measuring transition lag and mismatch for the specific technology and cohort in question — the invocation becomes performance of historical literacy rather than an empirically checked claim about THIS transition. Accessibility collapse (0.47) is moderate: workers can still find some historical counter-narratives and dissenting economists, but the policy-facing discourse has substantially collapsed toward the axiom as default prior. Resistance (0.6) is real and rising: labor historians, dissenting economists, and directly affected worker organizations increasingly contest the axiom's application to current automation and AI-driven displacement.
 *
 * PERSPECTIVAL GAP:
 *   From the institutional seat (technology owners, policy incumbents), the axiom reads as descriptively accurate coordination — a genuine historical pattern that justifies patience over intervention. From the payer seats (displaced workers, tradespeople, apprenticeship entrants), the identical historical claim reads as an extraction device: it defers help specifically during the window when help is needed, on the promise of benefits accruing to a different cohort, region, or generation. The engine should register this divergence directly from the structural data — trapped exit options and biographical time horizons for payers versus arbitrage exit and generational time horizons for beneficiaries — without either seat's reading being privileged as 'the' classification.
 *
 * DIRECTIONALITY LOGIC:
 *   Technology capital owners and policy incumbents sit near the beneficiary end: they capture productivity gains and political cover respectively, with minimal exposure to the specific transition costs. The economics commentariat benefits reputationally and professionally from being the interpreters of a historically-grounded axiom. Displaced workers, mid-career tradespeople, and apprenticeship entrants sit near the full-target end: trapped or constrained exit, immediate biographical time horizon, and no capacity to arbitrage the axiom's long time horizon against their own short one — the axiom's 'eventually' is measured in decades while their needs are measured in months.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (avoiding over-intervention in technological change that historically did self-correct) was genuinely live for certain past transitions. Whether it remains live for the current wave of automation and AI-driven displacement is exactly the contested question the six_questions genealogy interview is designed to surface: policy incumbents and commentariat attest the pattern still holds; labor historians and dissenting economists attest that the specific mechanism the axiom depends on (apprenticeship-based mobility surviving the transition) has empirically failed for the current cohort, because automation reduces the crew sizes that sustain apprenticeship pipelines in the first place. This is a genealogy mismatch worth flagging: the founding problem's *general form* may still be live in some domains while its *specific instantiation* (apprenticeship survival) has already died for this transition — a distinction the axiom's clean historical framing does not allow.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    aggregate_vs_cohort_measurement_ambiguity,
    'Does ''displacement is temporally offset by new jobs'' hold at the aggregate national level even while failing at the level of the specific displaced cohort and region — and if so, which level is the correct one for evaluating the axiom''s truth?',
    'Cohort-tracked longitudinal labor data comparing displaced workers'' lifetime earnings and occupational trajectories against a matched non-displaced control group, disaggregated by region and skill category, across multiple historical transitions and the current one.',
    'If the axiom only holds in aggregate while failing at the cohort level, it functions as a mountain-shaped claim (statistically true in aggregate, natural-seeming) that is being used to license extraction at the cohort level where it does not hold — a textbook false-summit pattern if beneficiaries are invoking aggregate truth to justify inaction on cohort-level harm.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(aggregate_vs_cohort_measurement_ambiguity, empirical, 'Aggregate self-correction versus cohort-level permanent harm as the correct unit of evaluation.').

omega_variable(
    apprenticeship_ladder_specific_failure,
    'Is the apprenticeship-mobility clause of the axiom (that upward mobility via apprenticeship survives the transition) a separate empirical claim from the general jobs-offset clause, with its own distinct and more clearly falsified truth value?',
    'Direct measurement of apprenticeship program enrollment, completion, and placement rates in trades undergoing automation, compared pre- and post-automation, isolated from the general employment-recovery statistic.',
    'If apprenticeship-based mobility has a measurably different (and more negative) trajectory than aggregate job creation, the axiom''s two clauses should arguably be treated as two separate constraints with two separate epsilon values — the general jobs-offset claim might be closer to a contested rope/tangled-rope while the apprenticeship-survival claim is closer to a snare for entrants. This story treats them as one bundled claim per the flat-construction instruction, but the internal ambiguity is real and worth flagging rather than resolving by fiat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(apprenticeship_ladder_specific_failure, conceptual, 'Whether the apprenticeship-survival clause is a separable, more clearly false sub-claim within the bundled axiom.').

omega_variable(
    time_horizon_of_offset_undefined,
    'The axiom asserts displacement is ''temporally offset'' but does not specify the offset window — is a multi-decade or intergenerational offset an acceptable fulfillment of the claim''s promise to the specific individuals displaced, or does an offset window exceeding a working lifetime functionally falsify the claim for those individuals regardless of aggregate statistics?',
    'Philosophical/policy specification of an acceptable offset window (e.g., within one worker''s remaining working years) against which historical and current transition data could be tested, rather than leaving ''eventually'' undefined.',
    'If any offset window is acceptable no matter how long, the axiom becomes unfalsifiable and functions purely as suppression of intervention; if a finite window is specified, most historical transitions likely fail it for the directly displaced cohort even while succeeding at the societal level.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(time_horizon_of_offset_undefined, conceptual, 'Undefined offset-window renders the axiom''s temporal claim potentially unfalsifiable.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(technological_displacement_axiom_flat_control, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tech_tr_t0, technological_displacement_axiom_flat_control, theater_ratio, 0, 0.3).
narrative_ontology:measurement_basis(tech_tr_t0, observed).
narrative_ontology:measurement(tech_tr_t8, technological_displacement_axiom_flat_control, theater_ratio, 8, 0.35).
narrative_ontology:measurement_basis(tech_tr_t8, observed).
narrative_ontology:measurement(tech_tr_t16, technological_displacement_axiom_flat_control, theater_ratio, 16, 0.4).
narrative_ontology:measurement_basis(tech_tr_t16, observed).
narrative_ontology:measurement(tech_tr_t24, technological_displacement_axiom_flat_control, theater_ratio, 24, 0.45).
narrative_ontology:measurement_basis(tech_tr_t24, observed).
narrative_ontology:measurement(tech_tr_t32, technological_displacement_axiom_flat_control, theater_ratio, 32, 0.49).
narrative_ontology:measurement_basis(tech_tr_t32, observed).
narrative_ontology:measurement(tech_tr_t40, technological_displacement_axiom_flat_control, theater_ratio, 40, 0.52).
narrative_ontology:measurement_basis(tech_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(tech_be_t0, technological_displacement_axiom_flat_control, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(tech_be_t0, observed).
narrative_ontology:measurement(tech_be_t8, technological_displacement_axiom_flat_control, base_extractiveness, 8, 0.47).
narrative_ontology:measurement_basis(tech_be_t8, observed).
narrative_ontology:measurement(tech_be_t16, technological_displacement_axiom_flat_control, base_extractiveness, 16, 0.52).
narrative_ontology:measurement_basis(tech_be_t16, observed).
narrative_ontology:measurement(tech_be_t24, technological_displacement_axiom_flat_control, base_extractiveness, 24, 0.56).
narrative_ontology:measurement_basis(tech_be_t24, observed).
narrative_ontology:measurement(tech_be_t32, technological_displacement_axiom_flat_control, base_extractiveness, 32, 0.59).
narrative_ontology:measurement_basis(tech_be_t32, observed).
narrative_ontology:measurement(tech_be_t40, technological_displacement_axiom_flat_control, base_extractiveness, 40, 0.61).
narrative_ontology:measurement_basis(tech_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(tech_su_t0, technological_displacement_axiom_flat_control, suppression_requirement, 0, 0.4).
narrative_ontology:measurement_basis(tech_su_t0, observed).
narrative_ontology:measurement(tech_su_t8, technological_displacement_axiom_flat_control, suppression_requirement, 8, 0.46).
narrative_ontology:measurement_basis(tech_su_t8, observed).
narrative_ontology:measurement(tech_su_t16, technological_displacement_axiom_flat_control, suppression_requirement, 16, 0.5).
narrative_ontology:measurement_basis(tech_su_t16, observed).
narrative_ontology:measurement(tech_su_t24, technological_displacement_axiom_flat_control, suppression_requirement, 24, 0.53).
narrative_ontology:measurement_basis(tech_su_t24, observed).
narrative_ontology:measurement(tech_su_t32, technological_displacement_axiom_flat_control, suppression_requirement, 32, 0.56).
narrative_ontology:measurement_basis(tech_su_t32, observed).
narrative_ontology:measurement(tech_su_t40, technological_displacement_axiom_flat_control, suppression_requirement, 40, 0.58).
narrative_ontology:measurement_basis(tech_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(technological_displacement_axiom_flat_control, resource_allocation).
narrative_ontology:boltzmann_floor_override(technological_displacement_axiom_flat_control, 0.15).
narrative_ontology:affects_constraint(technological_displacement_axiom_flat_control, automation_retraining_policy_mandate).
narrative_ontology:affects_constraint(technological_displacement_axiom_flat_control, ai_labor_displacement_current_wave).

% DUAL FORMULATION NOTE:
% This story treats the historical axiom as one bundled constraint per the flat-construction instruction. A decomposed reading would likely split the general jobs-offset clause from the apprenticeship-mobility-survival clause into two separate constraints with distinct epsilon values (see the apprenticeship_ladder_specific_failure omega); that decomposition is deliberately not performed here so the flat construction can be compared against decomposed variants elsewhere in the corpus.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
