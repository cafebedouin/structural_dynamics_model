% ============================================================================
% CONSTRAINT STORY: income_support_commitment__targeting_efficiency_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_income_support_commitment__targeting_efficiency_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: income_support_commitment__targeting_efficiency_reading
 *   human_readable: Universal Grant Consolidation Swap (Targeting-Efficiency Reading)
 *   domain: political economy/social policy/welfare state theory
 *
 * SUMMARY:
 *   A consolidated universal grant replaces the stack of need-calibrated
 *   programs — SNAP, housing vouchers, childcare subsidies, the EITC, TANF —
 *   with a single flat payment to every household, financed substantially by
 *   terminating the targeted channels. The arrangement is presented as
 *   universal solidarity: everyone receives the same grant, stigma
 *   disappears, cliffs vanish. Its net incidence runs the other way. A Queens
 *   parent whose household received roughly $31,100 in combined targeted
 *   benefits receives a $12,000 grant — a net loss of $19,100 — while middle-
 *   and upper-income households collect the same grant with nothing
 *   comparable to lose. The poor are the arrangement's nominal beneficiaries
 *   and its actual victims: the universalist frame performs inclusion while
 *   the financing consolidates a regressive transfer. KEY AGENTS (by
 *   structural relationship): deep_poverty_single_parents — primary bearer of
 *   the net loss (powerless/trapped); disabled_benefit_recipients — primary
 *   bearer, calibrated support irreplaceable (moderate/trapped);
 *   high_cost_metro_renters — secondary bearer (moderate/constrained);
 *   middle_income_households — principal collecting seat (organized/mobile);
 *   high_income_households — collecting seat with restructuring latitude
 *   (powerful/arbitrage); previously_excluded_childless_adults — incidental
 *   gainers the universalist frame truthfully describes (moderate/mobile);
 *   fiscal_authority_coalition — administers the swap and pockets
 *   administrative savings (institutional/arbitrage);
 *   anti_poverty_advocacy_coalitions — objecting challengers outside the
 *   deciding coalition (organized/constrained); distributional_economists —
 *   analytical observers publishing the incidence tables
 *   (analytical/analytical).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(income_support_commitment__targeting_efficiency_reading, 0.78).
domain_priors:suppression_score(income_support_commitment__targeting_efficiency_reading, 0.63).
domain_priors:theater_ratio(income_support_commitment__targeting_efficiency_reading, 0.5).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(income_support_commitment__targeting_efficiency_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(income_support_commitment__targeting_efficiency_reading, suppression_requirement, 0.63).
narrative_ontology:constraint_metric(income_support_commitment__targeting_efficiency_reading, theater_ratio, 0.5).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(income_support_commitment__targeting_efficiency_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(income_support_commitment__targeting_efficiency_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(income_support_commitment__targeting_efficiency_reading, snare).
narrative_ontology:human_readable(income_support_commitment__targeting_efficiency_reading, "Universal Grant Consolidation Swap (Targeting-Efficiency Reading)").
narrative_ontology:topic_domain(income_support_commitment__targeting_efficiency_reading, "political economy/social policy/welfare state theory").

domain_priors:requires_active_enforcement(income_support_commitment__targeting_efficiency_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(income_support_commitment__targeting_efficiency_reading, '99772ca9-1ac6-41a9-8e51-386272841b4a').
narrative_ontology:cs_kernel_codification('99772ca9-1ac6-41a9-8e51-386272841b4a', distributed).
narrative_ontology:cs_authority_grounding('99772ca9-1ac6-41a9-8e51-386272841b4a', distributed).
narrative_ontology:cs_reading_relation('99772ca9-1ac6-41a9-8e51-386272841b4a', income_support_commitment__freedom_floor_reading, forecloses).
narrative_ontology:cs_reading_relation('99772ca9-1ac6-41a9-8e51-386272841b4a', income_support_commitment__dependency_trap_reading, coexists_with).
narrative_ontology:cs_axiom('99772ca9-1ac6-41a9-8e51-386272841b4a', foundational, support_must_track_demonstrated_need).
narrative_ontology:cs_axiom_status(support_must_track_demonstrated_need, holdable).
narrative_ontology:cs_axiom_grounding('99772ca9-1ac6-41a9-8e51-386272841b4a', support_must_track_demonstrated_need, instrumental).
narrative_ontology:cs_axiom('99772ca9-1ac6-41a9-8e51-386272841b4a', secondary, consolidated_financing_shifts_net_incidence_upward).
narrative_ontology:cs_axiom_status(consolidated_financing_shifts_net_incidence_upward, holdable).
narrative_ontology:cs_axiom_grounding('99772ca9-1ac6-41a9-8e51-386272841b4a', consolidated_financing_shifts_net_incidence_upward, empirically_contingent).
narrative_ontology:cs_reference_frame('99772ca9-1ac6-41a9-8e51-386272841b4a', need_calibrated_targeting_framework).
narrative_ontology:cs_drift_state('99772ca9-1ac6-41a9-8e51-386272841b4a', contemporary_universalist_challenge, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('99772ca9-1ac6-41a9-8e51-386272841b4a', '').
narrative_ontology:cs_kernel_id(income_support_commitment__targeting_efficiency_reading, income_support_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(income_support_commitment__targeting_efficiency_reading, middle_income_households).
narrative_ontology:constraint_beneficiary(income_support_commitment__targeting_efficiency_reading, high_income_households).
narrative_ontology:constraint_beneficiary(income_support_commitment__targeting_efficiency_reading, previously_excluded_childless_adults).
narrative_ontology:constraint_victim(income_support_commitment__targeting_efficiency_reading, deep_poverty_single_parents).
narrative_ontology:constraint_victim(income_support_commitment__targeting_efficiency_reading, disabled_benefit_recipients).
narrative_ontology:constraint_victim(income_support_commitment__targeting_efficiency_reading, high_cost_metro_renters).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(income_support_commitment__targeting_efficiency_reading, fiscal_authority_coalition).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Raises children in a high-cost borough on a stacked package of targeted benefits — SNAP, a housing voucher, a childcare subsidy, the EITC, and TANF cash — worth roughly $31,100 a year, each piece sized to a specific need and each with its own eligibility test. The swap replaces the entire stack with a flat $12,000 grant. She cannot reapply to programs that have been repealed, cannot substitute her own earnings for subsidized childcare at her wage level, and has no individual recourse; her remaining option is collective political action she has little time, money, or standing to mount.
narrative_ontology:constraint_stakeholder(income_support_commitment__targeting_efficiency_reading, deep_poverty_single_parents, payer,
    powerless, immediate, trapped, national).

% Receive supports calibrated to costs that scale with impairment — personal care, medical transport, specialized diets, accessible housing — through programs that assess need individually. A flat grant ignores all of it. Their needs are non-negotiable and cannot be deferred, downsized, or relocated; the organizations that represent them testified against consolidation and were outvoted.
narrative_ontology:constraint_stakeholder(income_support_commitment__targeting_efficiency_reading, disabled_benefit_recipients, payer,
    moderate, biographical, trapped, national).

% Hold housing vouchers sized to local rents in cities where a modest apartment consumes most of a flat grant. They can technically move to cheaper regions, but relocation means leaving jobs, schools, and care networks, so exit is costly and partial rather than real.
narrative_ontology:constraint_stakeholder(income_support_commitment__targeting_efficiency_reading, high_cost_metro_renters, payer,
    moderate, biographical, constrained, regional).

% Receive the full grant while paying little of it back through the clawback schedule and having had little or no targeted-benefit footprint to lose. They are the modal net gainers, numerous enough to be the swap's decisive electoral constituency, and they experience the arrangement as a straightforward dividend.
narrative_ontology:constraint_stakeholder(income_support_commitment__targeting_efficiency_reading, middle_income_households, beneficiary,
    organized, biographical, mobile, national).

% Receive the same grant, surrender part of it through surtaxes, and come out ahead or neutral after tax planning. They gain additionally from a simplified benefit landscape and face no barrier to restructuring income around the clawback thresholds.
narrative_ontology:constraint_stakeholder(income_support_commitment__targeting_efficiency_reading, high_income_households, beneficiary,
    powerful, generational, arbitrage, national).

% Were ineligible for most of the consolidated programs — the EITC largely requires dependent children, TANF serves families, many rental programs prioritize them — and now receive the flat grant unconditionally. For them the swap is a genuine first-time transfer, which is why the universalist frame is not empty even though the aggregate incidence runs against the poorest.
narrative_ontology:constraint_stakeholder(income_support_commitment__targeting_efficiency_reading, previously_excluded_childless_adults, beneficiary,
    moderate, immediate, mobile, national).

% Enacted the consolidation and administers its financing: one payment stream replaces dozens of programs, eligibility offices close, caseworker headcount falls, and the administrative savings help balance the budget. The coalition controls the clawback schedule and can adjust it, but reversing course means rebuilding abolished machinery and confronting its own majority constituency.
narrative_ontology:constraint_stakeholder(income_support_commitment__targeting_efficiency_reading, fiscal_authority_coalition, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(income_support_commitment__targeting_efficiency_reading, fiscal_authority_coalition, beneficiary).

% Document the net-loss incidence household by household, publish the arithmetic, litigate procedural challenges, and lobby for restoring targeted channels or layered supplements. They were outside the budget negotiation that produced the swap and remain outside the coalition that sustains it; their influence runs through public opinion and elections they do not control.
narrative_ontology:constraint_stakeholder(income_support_commitment__targeting_efficiency_reading, anti_poverty_advocacy_coalitions, excluded,
    organized, biographical, constrained, national).

% Compute and publish the incidence tables the debate turns on — who gains, who loses, by how much, under which financing assumptions. They hold no stake in the outcome and their analyses circulate to every seat.
narrative_ontology:constraint_stakeholder(income_support_commitment__targeting_efficiency_reading, distributional_economists, observer,
    analytical, biographical, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(income_support_commitment__targeting_efficiency_reading, middle_income_households).
narrative_ontology:fixing_cost_class(income_support_commitment__targeting_efficiency_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a real delivery problem: a single uniform payment reaches every household with no eligibility determination, no take-up gap, no benefit cliff, and a fraction of the administrative overhead of the consolidated programs — coverage becomes automatic and stigma-free by construction.
% TRANSFER_FUNCTION: Moves purchasing power through a flat per-household grant financed chiefly by terminating need-calibrated programs. Net incidence runs from deep-poverty households whose prior packages exceeded the grant (about -$19,100 for the exemplar household) toward middle- and upper-income households that receive the grant without comparable losses, with a minority of previously excluded poor households gaining for the first time.
% ABSENT_VOICES: The deep-poverty households whose net position deteriorates are absent from the fiscal negotiation that decided the swap; their interests enter only as refracted through universalist advocacy that counts them as included. Disability representatives and tenant organizations objected in testimony and were outvoted; no seat in the budget coalition represents a net-loser household.
% DISAPPEARANCE_RATIONALE: If the swap vanished overnight, large benefit streams would need re-routing: net-loser households would reclaim targeted packages worth tens of thousands, net-winner majorities would lose a grant they have incorporated into household budgets, closed eligibility offices and their staffs would have to be rebuilt, and the fiscal arithmetic the coalition balanced on would reopen — the welfare state would visibly reorganize around restored need-testing.
% FOUNDING_PROBLEM: Fragmented means-testing: a patchwork of programs with overlapping eligibility tests, benefit cliffs that penalize work and marriage, chronic take-up failure among eligible households, and administrative burden that excludes precisely the people the programs exist to serve.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the net-winner coalition: administrative-burden scholarship and government accountability studies document take-up failure and cliff effects in the pre-swap patchwork, and anti-poverty researchers attest the problem persists — while disputing that consolidating financing is the remedy. No attesting source belongs to the seats that capture the swap's gains.
narrative_ontology:disappearance_verdict(income_support_commitment__targeting_efficiency_reading, world_rearranges).
narrative_ontology:founding_problem_status(income_support_commitment__targeting_efficiency_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(income_support_commitment__targeting_efficiency_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(income_support_commitment__targeting_efficiency_reading, 'none', 1).
narrative_ontology:epsilon_provenance(income_support_commitment__targeting_efficiency_reading, 0.78, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(income_support_commitment__targeting_efficiency_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(income_support_commitment__targeting_efficiency_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(income_support_commitment__targeting_efficiency_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is high (0.78 at interval end) because the financing converts need-calibrated transfers into a flat grant whose incidence is regressive against the deepest poverty; it stops short of saturation because a minority of previously excluded poor households genuinely gain. Suppression (0.63) is structural, not internalized: the targeted channels are statutorily closed, so exit is a legislative act rather than an individual choice — and suppression is authored as a raw structural property, unscaled; only extractiveness is scaled by directionality and scope downstream. Theater (0.50) reflects a universalist frame performing inclusion while incidence tables show exclusion — a growing share of the arrangement's communicative activity defends the frame rather than delivers the function. Accessibility collapse is moderate (0.42): the old channels are legally dead for individuals, but legislative restoration, state supplements, and charitable stopgaps persist as partial alternatives. Resistance (0.60) is sustained advocacy, litigation, and electoral challenge; the dispersed powerless victims have coalition potential only through the advocacy seats that aggregate them, and that aggregation is real but resource-starved. The three measurement series share one six-point grid. Suppression_requirement is tracked deliberately because the enforcement picture genuinely changes over the interval — application channels close, offices shutter, restoration bills fail — rather than staying static. Trajectories are monotonic with no oscillation, so no intermittent-reinforcement reading applies. Points at T=24 and T=30 are marked projected: they assume the enactment trajectory holds.
 *
 * PERSPECTIVAL GAP:
 *   From the fiscal coalition's seat the swap is a coordination achievement it built and balances budgets on; from the trapped payer seats the same statute operates as confiscation of calibrated support; from the previously excluded seat it is a first dividend. Nothing in the authored metrics adjudicates between these readings — the engine computes divergent per-seat classifications from power, exit options, and declared position, and the divergence is the finding.
 *
 * DIRECTIONALITY LOGIC:
 *   Trapped, powerless payers sit nearest the full-target end of the directionality range, and the derivation amplifies their effective extraction accordingly. Mobile and arbitrage beneficiaries sit nearest the beneficiary end, damping theirs. The fiscal coalition's institutional power and restructuring latitude place it near the beneficiary end even though it appears in no victim list — and its secondary collection of administrative savings reinforces that placement. Previously excluded poor households derive partial-beneficiary directionality: the one seat where the universalist frame's promise is locally true. The excluded advocates receive the canonical fallback; their alignment with the victims is commentary-grade, not a structural declaration, and drives no classification.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards both directions. Against mislabeling as rope: the universal-delivery coordination story is genuine but is doing cover work — the same statute that simplifies delivery consolidates a regressive transfer, and the snare verdict keeps the efficiency framing from laundering the incidence. Against overcorrection: the founding problem (fragmented means-testing, cliffs, take-up failure) is live and externally corroborated, so this is not a zombie mandate; it is a live-mandate arrangement whose chosen remedy inverts its own distributive purpose. No mandatrophy resolution is declared: the mandate has not outlived its function — the function has been captured by its financing. The piton signature does not fit: extraction is concentrated, not diffuse, and a clear seat captures the gains.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_instantiation,
    'This constraint is the targeting_efficiency_reading of the income_support_commitment kernel; what structural differences would instantiating a sibling reading produce?',
    'Generate the sibling stories (freedom_floor_reading, dependency_trap_reading) over the same swap arrangement and compare computed types, epsilon, and per-seat classifications.',
    'Classification is reading-indexed: the same swap assessed by the freedom_floor reading would carry lower epsilon and plausibly a rope or scaffold type; treating any single reading''s verdict as the kernel''s verdict misclassifies the family.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_instantiation, conceptual, 'Committer structure: this story is one reading of a three-reading kernel.').

omega_variable(
    incidence_measurement_dispute,
    'Do net losses actually concentrate on deep-poverty households, or do dynamic responses — labor supply, price and rent adjustment, informal support substitution — offset the static incidence?',
    'Longitudinal panel data on post-consolidation household income and consumption, disaggregated by pre-swap benefit depth.',
    'If dynamic offsets hold for the poorest, effective extraction falls and the snare reading weakens toward tangled_rope; if static losses persist, the snare classification is confirmed and deepened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(incidence_measurement_dispute, empirical, 'Static versus dynamic incidence of the consolidated-financing swap.').

omega_variable(
    enactment_trajectory_uncertainty,
    'Will the consolidation reach full enactment on the projected schedule, or stall at partial pilots and hold-harmless phases?',
    'Legislative tracking of restoration bills, appropriation levels for legacy-channel wind-down, and take-up data during transition phases.',
    'If enactment stalls, the measurements after T=18 overstate realized extraction and the arrangement operates as a contested proposal rather than a settled snare; the projected-basis tail of the measurement series should be discounted.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enactment_trajectory_uncertainty, empirical, 'Whether the projected segment of the interval materializes.').

omega_variable(
    coordination_extraction_separability,
    'Is the universal-delivery efficiency gain — one payment, no eligibility machinery, no cliffs — separable from the regressive financing that consolidates the targeted channels?',
    'Revenue-neutral redesign pilots: a flat grant paired with progressive clawback schedules that preserve net incidence for deep-poverty households.',
    'If separable, the extraction is a financing choice riding on genuine coordination value and the snare verdict attaches to the financing rather than the delivery; if inseparable, part of the measured extraction is the price of the coordination itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_separability, conceptual, 'Whether delivery efficiency and regressive incidence can be unbundled.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(income_support_commitment__targeting_efficiency_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(targeting_efficiency_reading_tr_t0, income_support_commitment__targeting_efficiency_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement_basis(targeting_efficiency_reading_tr_t0, observed).
narrative_ontology:measurement(targeting_efficiency_reading_tr_t6, income_support_commitment__targeting_efficiency_reading, theater_ratio, 6, 0.34).
narrative_ontology:measurement_basis(targeting_efficiency_reading_tr_t6, observed).
narrative_ontology:measurement(targeting_efficiency_reading_tr_t12, income_support_commitment__targeting_efficiency_reading, theater_ratio, 12, 0.38).
narrative_ontology:measurement_basis(targeting_efficiency_reading_tr_t12, observed).
narrative_ontology:measurement(targeting_efficiency_reading_tr_t18, income_support_commitment__targeting_efficiency_reading, theater_ratio, 18, 0.42).
narrative_ontology:measurement_basis(targeting_efficiency_reading_tr_t18, observed).
narrative_ontology:measurement(targeting_efficiency_reading_tr_t24, income_support_commitment__targeting_efficiency_reading, theater_ratio, 24, 0.46).
narrative_ontology:measurement_basis(targeting_efficiency_reading_tr_t24, projected).
narrative_ontology:measurement(targeting_efficiency_reading_tr_t30, income_support_commitment__targeting_efficiency_reading, theater_ratio, 30, 0.5).
narrative_ontology:measurement_basis(targeting_efficiency_reading_tr_t30, projected).

% Extraction over time
narrative_ontology:measurement(targeting_efficiency_reading_be_t0, income_support_commitment__targeting_efficiency_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement_basis(targeting_efficiency_reading_be_t0, observed).
narrative_ontology:measurement(targeting_efficiency_reading_be_t6, income_support_commitment__targeting_efficiency_reading, base_extractiveness, 6, 0.62).
narrative_ontology:measurement_basis(targeting_efficiency_reading_be_t6, observed).
narrative_ontology:measurement(targeting_efficiency_reading_be_t12, income_support_commitment__targeting_efficiency_reading, base_extractiveness, 12, 0.68).
narrative_ontology:measurement_basis(targeting_efficiency_reading_be_t12, observed).
narrative_ontology:measurement(targeting_efficiency_reading_be_t18, income_support_commitment__targeting_efficiency_reading, base_extractiveness, 18, 0.72).
narrative_ontology:measurement_basis(targeting_efficiency_reading_be_t18, observed).
narrative_ontology:measurement(targeting_efficiency_reading_be_t24, income_support_commitment__targeting_efficiency_reading, base_extractiveness, 24, 0.75).
narrative_ontology:measurement_basis(targeting_efficiency_reading_be_t24, projected).
narrative_ontology:measurement(targeting_efficiency_reading_be_t30, income_support_commitment__targeting_efficiency_reading, base_extractiveness, 30, 0.78).
narrative_ontology:measurement_basis(targeting_efficiency_reading_be_t30, projected).

% Suppression requirement over time
narrative_ontology:measurement(targeting_efficiency_reading_su_t0, income_support_commitment__targeting_efficiency_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement_basis(targeting_efficiency_reading_su_t0, observed).
narrative_ontology:measurement(targeting_efficiency_reading_su_t6, income_support_commitment__targeting_efficiency_reading, suppression_requirement, 6, 0.47).
narrative_ontology:measurement_basis(targeting_efficiency_reading_su_t6, observed).
narrative_ontology:measurement(targeting_efficiency_reading_su_t12, income_support_commitment__targeting_efficiency_reading, suppression_requirement, 12, 0.53).
narrative_ontology:measurement_basis(targeting_efficiency_reading_su_t12, observed).
narrative_ontology:measurement(targeting_efficiency_reading_su_t18, income_support_commitment__targeting_efficiency_reading, suppression_requirement, 18, 0.58).
narrative_ontology:measurement_basis(targeting_efficiency_reading_su_t18, observed).
narrative_ontology:measurement(targeting_efficiency_reading_su_t24, income_support_commitment__targeting_efficiency_reading, suppression_requirement, 24, 0.61).
narrative_ontology:measurement_basis(targeting_efficiency_reading_su_t24, projected).
narrative_ontology:measurement(targeting_efficiency_reading_su_t30, income_support_commitment__targeting_efficiency_reading, suppression_requirement, 30, 0.63).
narrative_ontology:measurement_basis(targeting_efficiency_reading_su_t30, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(income_support_commitment__targeting_efficiency_reading, resource_allocation).
narrative_ontology:affects_constraint(income_support_commitment__targeting_efficiency_reading, income_support_commitment__freedom_floor_reading).
narrative_ontology:affects_constraint(income_support_commitment__targeting_efficiency_reading, income_support_commitment__dependency_trap_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'income support policy' decomposes into three reading-stories of the income_support_commitment kernel, linked here. Need-indexed distribution, unconditional floors, and work-incentive design are structurally distinct claims with different epsilon values, beneficiary structures, and failure modes; forcing them into one story would make epsilon observer-dependent. This story (targeting_efficiency_reading) links to both siblings; the coupling runs through shared financing — whichever reading governs determines whether the targeted channels survive.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
