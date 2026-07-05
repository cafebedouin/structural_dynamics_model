% ============================================================================
% CONSTRAINT STORY: flexible_employment_legitimacy__market_efficiency_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_flexible_employment_legitimacy__market_efficiency_reading, []).

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
 *   constraint_id: flexible_employment_legitimacy__market_efficiency_reading
 *   human_readable: Flexible Employment as Market-Clearing Mechanism (Market Efficiency Reading)
 *   domain: economic/labor/technological
 *
 * SUMMARY:
 *   This story instantiates the market-efficiency reading of the contested
 *   kernel around flexible employment legitimacy. On this reading, the rise
 *   of platform-mediated gig work and flexible staffing arrangements is a
 *   genuine market-clearing mechanism: wage convergence toward a common
 *   platform rate reflects real-time price discovery for blue-collar and
 *   service labor scarcity, algorithmic dispatch is treated as neutral
 *   coordination infrastructure rather than an instrument of control, and
 *   worker-reported preference for schedule autonomy is treated as revealed
 *   preference rather than adaptation to a constrained choice set. This is
 *   deliberately ONE of three sibling readings of the same kernel
 *   (developmental_state_reading, precarity_extraction_reading) — each is
 *   authored as its own ε-invariant constraint with its own
 *   beneficiary/victim structure, per the ε-invariance principle. This story
 *   does not describe or average over the contest; the contest itself is
 *   routed to omega variables and to cs_structure.reading_relations/axioms.
 *
 * KEY AGENTS:
 *   - platform_workers_seeking_flexibility: primary beneficiary (moderate/mobile) — trades schedule fixity for autonomy and multi-app arbitrage
 *   - platform_operators: agenda-setter (institutional/arbitrage) — builds and maintains the matching algorithm, framed here as neutral infrastructure
 *   - consumers_of_on_demand_services and small_business_labor_purchasers: secondary beneficiaries — draw on the liquid labor pool
 *   - displaced_traditional_employers: excluded voice — contests whether convergence is efficiency or floor-racing
 *   - labor_economists_and_regulators: analytical observer — the seat positioned to corroborate or undercut the reading from outside beneficiary self-report
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(flexible_employment_legitimacy__market_efficiency_reading, 0.28).
domain_priors:suppression_score(flexible_employment_legitimacy__market_efficiency_reading, 0.22).
domain_priors:theater_ratio(flexible_employment_legitimacy__market_efficiency_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(flexible_employment_legitimacy__market_efficiency_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(flexible_employment_legitimacy__market_efficiency_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(flexible_employment_legitimacy__market_efficiency_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(flexible_employment_legitimacy__market_efficiency_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(flexible_employment_legitimacy__market_efficiency_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(flexible_employment_legitimacy__market_efficiency_reading, rope).
narrative_ontology:human_readable(flexible_employment_legitimacy__market_efficiency_reading, "Flexible Employment as Market-Clearing Mechanism (Market Efficiency Reading)").
narrative_ontology:topic_domain(flexible_employment_legitimacy__market_efficiency_reading, "economic/labor/technological").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(flexible_employment_legitimacy__market_efficiency_reading, 'e8f33ffc-c197-4962-919d-e6327211c384').
narrative_ontology:cs_kernel_codification('e8f33ffc-c197-4962-919d-e6327211c384', distributed).
narrative_ontology:cs_authority_grounding('e8f33ffc-c197-4962-919d-e6327211c384', diffuse_epistemic).
narrative_ontology:cs_reading_relation('e8f33ffc-c197-4962-919d-e6327211c384', flexible_employment_legitimacy__precarity_extraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('e8f33ffc-c197-4962-919d-e6327211c384', flexible_employment_legitimacy__developmental_state_reading, influences).
narrative_ontology:cs_axiom('e8f33ffc-c197-4962-919d-e6327211c384', foundational, wage_convergence_reflects_genuine_scarcity_price).
narrative_ontology:cs_axiom_status(wage_convergence_reflects_genuine_scarcity_price, holdable).
narrative_ontology:cs_axiom_grounding('e8f33ffc-c197-4962-919d-e6327211c384', wage_convergence_reflects_genuine_scarcity_price, empirically_contingent).
narrative_ontology:cs_axiom('e8f33ffc-c197-4962-919d-e6327211c384', foundational, algorithmic_matching_is_neutral_infrastructure).
narrative_ontology:cs_axiom_status(algorithmic_matching_is_neutral_infrastructure, holdable).
narrative_ontology:cs_axiom_grounding('e8f33ffc-c197-4962-919d-e6327211c384', algorithmic_matching_is_neutral_infrastructure, empirically_contingent).
narrative_ontology:cs_axiom('e8f33ffc-c197-4962-919d-e6327211c384', secondary, worker_schedule_choice_constitutes_revealed_preference).
narrative_ontology:cs_axiom_status(worker_schedule_choice_constitutes_revealed_preference, holdable).
narrative_ontology:cs_axiom_grounding('e8f33ffc-c197-4962-919d-e6327211c384', worker_schedule_choice_constitutes_revealed_preference, instrumental).
narrative_ontology:cs_reference_frame('e8f33ffc-c197-4962-919d-e6327211c384', neoclassical_labor_market_clearing).
narrative_ontology:cs_drift_state('e8f33ffc-c197-4962-919d-e6327211c384', post_gig_economy_scaling_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('e8f33ffc-c197-4962-919d-e6327211c384', '').
narrative_ontology:cs_kernel_id(flexible_employment_legitimacy__market_efficiency_reading, flexible_employment_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(flexible_employment_legitimacy__market_efficiency_reading, platform_workers_seeking_flexibility).
narrative_ontology:constraint_beneficiary(flexible_employment_legitimacy__market_efficiency_reading, platform_operators).
narrative_ontology:constraint_beneficiary(flexible_employment_legitimacy__market_efficiency_reading, consumers_of_on_demand_services).
narrative_ontology:constraint_beneficiary(flexible_employment_legitimacy__market_efficiency_reading, small_business_labor_purchasers).
narrative_ontology:constraint_vindicates(flexible_employment_legitimacy__market_efficiency_reading, labor_market_clearing_efficiency).
narrative_ontology:constraint_vindicates(flexible_employment_legitimacy__market_efficiency_reading, algorithmic_matching_neutrality).
narrative_ontology:constraint_vindicates(flexible_employment_legitimacy__market_efficiency_reading, worker_autonomy_maximization_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Choose gig and platform work to set their own hours, combine multiple income streams, or fit paid work around caregiving, study, or health constraints. On this reading, the absence of a fixed schedule or single employer is the point, not a defect: they can log on and off multiple apps, compare effective hourly rates in real time, and reallocate their labor toward whichever platform is paying more, which is treated as evidence the market is clearing efficiently.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__market_efficiency_reading, platform_workers_seeking_flexibility, beneficiary,
    moderate, biographical, mobile, national).

% Design the matching algorithms and pricing mechanisms that connect available workers to available demand in near real time. On this reading they function as a neutral clearinghouse: the algorithm surfaces wage signals (surge pricing, task rates) that reflect current supply and demand for blue-collar and service labor, and the operator's role is coordination infrastructure rather than wage-setting.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__market_efficiency_reading, platform_operators, agenda_setter,
    institutional, generational, arbitrage, national).

% Purchase rides, deliveries, and task labor on demand at prices that adjust with real-time availability. They benefit from the liquidity the flexible labor pool provides — service is available at hours and in quantities that a fixed-shift workforce could not efficiently supply.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__market_efficiency_reading, consumers_of_on_demand_services, beneficiary,
    organized, immediate, mobile, national).

% Hire flexible labor through platforms or staffing pools to handle variable demand — seasonal retail, event staffing, delivery capacity — without carrying the fixed cost of permanent headcount during slack periods. The matching mechanism lets them scale labor input to actual order volume.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__market_efficiency_reading, small_business_labor_purchasers, beneficiary,
    moderate, biographical, mobile, regional).

% Firms that formerly employed similar labor on standard contracts and now compete against platform-sourced flexible labor on price. They would argue the wage convergence the market-efficiency reading celebrates is partly a race to the regulatory floor rather than pure efficiency gain, but their objection is treated as a competitiveness complaint, not evidence bearing on the constraint's classification.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__market_efficiency_reading, displaced_traditional_employers, excluded,
    moderate, biographical, constrained, national).

% Study wage-convergence data, platform algorithm behavior, and worker outcome surveys to assess whether flexible employment functions as advertised. They can commission audits of algorithmic pricing and publish findings that would corroborate or undercut the market-clearing account from outside the platforms' own reporting.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__market_efficiency_reading, labor_economists_and_regulators, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(flexible_employment_legitimacy__market_efficiency_reading, diffuse).
narrative_ontology:fixing_cost_class(flexible_employment_legitimacy__market_efficiency_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Matches variable, fragmented labor supply (workers wanting non-standard hours or supplemental income) to variable, fragmented demand (consumers and businesses needing labor at irregular intervals) faster and at finer granularity than fixed-shift employment can, using algorithmic dispatch and real-time price signals.
% TRANSFER_FUNCTION: Moves labor hours from workers to whichever demand source is currently paying the clearing rate, and moves the marginal cost of demand variability from employers (who would otherwise carry idle fixed-shift capacity) onto the flexible labor pool, which absorbs the timing risk in exchange for schedule autonomy.
% ABSENT_VOICES: Displaced traditional employers and unions representing standard-contract labor would contest whether the 'clearing rate' is a genuine scarcity signal or a race-to-floor artifact of undercutting standard labor protections; on this reading their objection is categorized as a competitiveness or distributional complaint external to the efficiency question, not incorporated into it.
% DISAPPEARANCE_RATIONALE: Under the market-efficiency reading, if flexible employment arrangements were abolished, the world would partially rearrange (workers seeking supplemental or irregular income would lose an option, and demand-side labor purchasers would face higher fixed costs) but proponents of this reading hold the underlying labor supply/demand mismatch would simply resurface through informal or gray-market channels rather than resolve into stable formal employment — hence contested rather than a clean world_rearranges or world_unchanged verdict.
% FOUNDING_PROBLEM: Labor supply (workers with irregular availability, seeking supplemental income, or excluded from standard full-time contracts) and labor demand (businesses and consumers with spiky, unpredictable need for services) were poorly matched by traditional fixed-shift employment, producing both worker underemployment and unmet consumer/business demand at peak times.
% FOUNDING_PROBLEM_CORROBORATION: Platform operators and worker-flexibility advocacy groups attest the matching problem remains live and cite worker surveys showing many participants value schedule control. However, this attestation comes substantially from parties that benefit from the arrangement; labor economists studying involuntary part-time rates and platform dependency (a source partly outside the beneficiary set, though not fully independent since some research is platform-funded) offer more qualified corroboration, noting the founding problem is live for a subset of workers but the reading may overstate its generality across the full platform workforce.
narrative_ontology:disappearance_verdict(flexible_employment_legitimacy__market_efficiency_reading, contested).
narrative_ontology:founding_problem_status(flexible_employment_legitimacy__market_efficiency_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(flexible_employment_legitimacy__market_efficiency_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(flexible_employment_legitimacy__market_efficiency_reading, 'none', 1).
narrative_ontology:epsilon_provenance(flexible_employment_legitimacy__market_efficiency_reading, 0.28, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(flexible_employment_legitimacy__market_efficiency_reading_tests).
:- end_tests(flexible_employment_legitimacy__market_efficiency_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low-moderate (0.28 at interval end) because, under this reading, the wage and scheduling outcomes are treated as market-clearing prices rather than extracted surplus — there is a real coordination function and no declared victim group. Suppression is low (0.22): workers can multi-app, exit to traditional employment, or decline shifts without formal penalty structures beyond opportunity cost. Theater ratio is low (0.18) because the coordination function (matching supply to demand) is treated as substantially real rather than a performative cover, on this reading. Accessibility collapse is moderate (0.35): once workers understand the algorithm's rate-setting behavior, some workaround options (multi-apping, rate comparison) remain open, which is why this is authored as a rope-family claim rather than mountain-grade natural necessity. Resistance is moderate-low (0.3): organized labor and some workers push back, but on this reading that resistance is read as distributional preference rather than evidence the coordination story is false.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (platform_operators) and the beneficiary worker seat should compute close to the claimed type under this reading's own metrics, since no victim group is declared here — the structural asymmetry that a payer/victim seat would introduce is deliberately absent from THIS story. That asymmetry is instead carried entirely by the sibling reading (precarity_extraction_reading), which declares victims and enforcement over the same underlying platform arrangement. The gap between the two readings' computed types IS the object under study — this story does not import that gap, it stays clean on its own terms.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (workers seeking flexibility, consumers, small-business labor purchasers, platform operators) are declared with no offsetting victim group in this story, so directionality across stakeholders skews toward the beneficiary end almost uniformly — this is a structural feature of the reading, not an oversight. The excluded stakeholder (displaced_traditional_employers) is not a victim of THIS constraint's operation as authored here; they are a voice excluded from the coordination-function narrative, which is why they are marked excluded rather than victim. If a future revision decided their competitive displacement constituted extraction, that data would belong in the sibling precarity_extraction_reading, not retrofitted here.
 *
 * MANDATROPHY ANALYSIS:
 *   The market-efficiency reading treats the founding problem (mismatched fragmented labor supply and demand) as still live, which forecloses classifying the arrangement as pure inertial residue (piton) or captured extraction (snare) from this seat's own axioms. Mandatrophy risk is bounded here specifically because no enforcement mechanism is declared (requires_active_enforcement is false) — the reading holds itself out as a voluntary, exit-rich mechanism, so its own internal logic would require reclassification if evidence showed the algorithm suppresses effective exit (see the platform_algorithm_neutrality_ambiguity omega below).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    wage_convergence_signal_or_floor,
    'Does observed wage convergence across gig platforms reflect a genuine scarcity-driven market-clearing price, or a race toward the lowest common regulatory floor as platforms compete on labor cost rather than labor quality?',
    'Compare wage trajectories in jurisdictions with strong minimum-standard floors for gig work against jurisdictions without them; if convergence rates and levels are similar regardless of floor strength, that favors the market-clearing account; if convergence tracks the regulatory floor specifically, that favors the extraction account.',
    'If convergence tracks scarcity, this reading''s core empirical premise holds and the rope classification is well-supported; if convergence tracks the floor, the empirically_contingent axiom below would be substantially undermined and pressure would build toward the precarity_extraction_reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(wage_convergence_signal_or_floor, empirical, 'Whether wage convergence is a scarcity signal or a regulatory-floor artifact.').

omega_variable(
    platform_algorithm_neutrality_ambiguity,
    'Is the platform matching algorithm a neutral price-discovery mechanism, or does it actively shape worker behavior (e.g., through opaque incentive structures, forced acceptance-rate thresholds, or information asymmetry about true demand) in ways that constitute a form of managerial control inconsistent with the ''neutral coordination'' premise?',
    'Independent algorithmic audit (as sought by labor_economists_and_regulators) comparing worker-facing information against the platform''s actual demand-forecasting and pricing data; discrepancy would indicate the algorithm functions as a control mechanism rather than a transparent clearinghouse.',
    'If the algorithm is found to actively manage rather than merely clear, the market_efficiency_reading''s foundational premise (algorithmic matching neutrality) would need to be classified as overridden, and the constraint would structurally converge toward the precarity_extraction_reading''s account of the same platform relationship.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(platform_algorithm_neutrality_ambiguity, empirical, 'Whether the matching algorithm is neutral coordination or disguised managerial control.').

omega_variable(
    kernel_framing_selection_ambiguity,
    'Given that the same observable data (wage levels, worker surveys, platform growth) can be read through the market_efficiency, precarity_extraction, or developmental_state framings with different beneficiary/victim structures, what signals guided selecting the market-efficiency framing for THIS story rather than treating the underlying platform-labor phenomenon as a single ambiguous constraint?',
    'This is resolved structurally, not empirically: per the ε-invariance principle, the framing ambiguity itself is the reason three separate constraint stories exist rather than one constraint with a measurement parameter. The signal for this file''s framing choice was the source material''s explicit instruction to author the market_efficiency reading specifically.',
    'No single resolution collapses the three readings into one; the readings coexist as parallel accounts of the same underlying platform-labor phenomenon, each internally coherent on its own axioms, linked via network.affects_constraints and cs_structure.reading_relations rather than merged.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_framing_selection_ambiguity, conceptual, 'Documents why this story instantiates one reading of a multiply-readable kernel rather than a single averaged constraint.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(flexible_employment_legitimacy__market_efficiency_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(flex_tr_t0, flexible_employment_legitimacy__market_efficiency_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(flex_tr_t4, flexible_employment_legitimacy__market_efficiency_reading, theater_ratio, 4, 0.13).
narrative_ontology:measurement(flex_tr_t8, flexible_employment_legitimacy__market_efficiency_reading, theater_ratio, 8, 0.14).
narrative_ontology:measurement(flex_tr_t12, flexible_employment_legitimacy__market_efficiency_reading, theater_ratio, 12, 0.15).
narrative_ontology:measurement(flex_tr_t16, flexible_employment_legitimacy__market_efficiency_reading, theater_ratio, 16, 0.16).
narrative_ontology:measurement(flex_tr_t20, flexible_employment_legitimacy__market_efficiency_reading, theater_ratio, 20, 0.17).
narrative_ontology:measurement(flex_tr_t24, flexible_employment_legitimacy__market_efficiency_reading, theater_ratio, 24, 0.18).

% Extraction over time
narrative_ontology:measurement(flex_be_t0, flexible_employment_legitimacy__market_efficiency_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(flex_be_t4, flexible_employment_legitimacy__market_efficiency_reading, base_extractiveness, 4, 0.22).
narrative_ontology:measurement(flex_be_t8, flexible_employment_legitimacy__market_efficiency_reading, base_extractiveness, 8, 0.24).
narrative_ontology:measurement(flex_be_t12, flexible_employment_legitimacy__market_efficiency_reading, base_extractiveness, 12, 0.25).
narrative_ontology:measurement(flex_be_t16, flexible_employment_legitimacy__market_efficiency_reading, base_extractiveness, 16, 0.26).
narrative_ontology:measurement(flex_be_t20, flexible_employment_legitimacy__market_efficiency_reading, base_extractiveness, 20, 0.27).
narrative_ontology:measurement(flex_be_t24, flexible_employment_legitimacy__market_efficiency_reading, base_extractiveness, 24, 0.28).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(flexible_employment_legitimacy__market_efficiency_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(flexible_employment_legitimacy__market_efficiency_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(flexible_employment_legitimacy__market_efficiency_reading, 0.12).
narrative_ontology:affects_constraint(flexible_employment_legitimacy__market_efficiency_reading, precarity_extraction_reading).
narrative_ontology:affects_constraint(flexible_employment_legitimacy__market_efficiency_reading, developmental_state_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the flexible_employment_legitimacy kernel. market_efficiency_reading (this file) treats wage convergence as scarcity signal and the algorithm as neutral coordination, yielding a rope classification with no declared victims. precarity_extraction_reading treats the same wage convergence as extraction enabled by algorithmic information asymmetry, declaring platform workers as victims and platform operators/investors as beneficiaries, yielding a substantially more extractive classification (likely tangled_rope or snare). developmental_state_reading treats the arrangement as an incomplete transitional form requiring active state intervention toward formalization, which would most likely classify as a scaffold contingent on a declared sunset/transition path. All three share the same underlying empirical substrate (platform gig labor markets) but diverge on beneficiary/victim structure, enforcement characterization, and claimed type — exactly the decomposition the ε-invariance principle requires rather than a single story with an observable parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
