% ============================================================================
% CONSTRAINT STORY: income_support_conditionality__wage_subsidy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_income_support_conditionality__wage_subsidy_reading, []).

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
 *   constraint_id: income_support_conditionality__wage_subsidy_reading
 *   human_readable: Unconditional Income Support as Employer Wage-Suppression Subsidy
 *   domain: economic/political/labor
 *
 * SUMMARY:
 *   An unconditional cash transfer program is introduced to eliminate
 *   destitution and close welfare coverage gaps. In labor markets with weak
 *   bargaining power — retail, hospitality, gig platforms, agriculture —
 *   employers observe the new income floor and adjust wage offers downward,
 *   since workers can now survive on lower nominal pay. The transfer's
 *   poverty-reduction function is real (worker subsistence is maintained),
 *   but a substantial share of its fiscal value is redirected into reduced
 *   employer labor costs rather than into improved worker living standards.
 *   This is the wage_subsidy reading of the broader income-support kernel:
 *   coordination (subsistence guarantee) and extraction (employer wage-cost
 *   transfer) operate through the same mechanism simultaneously.
 *
 * KEY AGENTS:
 *   - low_wage_employers: primary beneficiary (organized/arbitrage) — captures transfer value via wage repricing
 *   - gig_platform_operators: primary beneficiary (institutional/arbitrage) — prices piece-rate labor against the subsidized floor
 *   - low_wage_workers: primary target (powerless/constrained) — receives transfer but sees wage gains erode
 *   - transfer_administering_agency: agenda-setter (institutional/analytical) — designs the transfer, does not regulate wage-setting
 *   - organized_labor: excluded voice (organized/mobile) — absent from transfer design, would demand paired wage floors
 *   - taxpayers_general: diffuse payer (moderate/trapped) — funds the transfer that partially subsidizes private payrolls
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(income_support_conditionality__wage_subsidy_reading, 0.62).
domain_priors:suppression_score(income_support_conditionality__wage_subsidy_reading, 0.48).
domain_priors:theater_ratio(income_support_conditionality__wage_subsidy_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(income_support_conditionality__wage_subsidy_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(income_support_conditionality__wage_subsidy_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(income_support_conditionality__wage_subsidy_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(income_support_conditionality__wage_subsidy_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(income_support_conditionality__wage_subsidy_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(income_support_conditionality__wage_subsidy_reading, tangled_rope).
narrative_ontology:human_readable(income_support_conditionality__wage_subsidy_reading, "Unconditional Income Support as Employer Wage-Suppression Subsidy").
narrative_ontology:topic_domain(income_support_conditionality__wage_subsidy_reading, "economic/political/labor").

domain_priors:requires_active_enforcement(income_support_conditionality__wage_subsidy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(income_support_conditionality__wage_subsidy_reading, '12de6172-efbb-4602-9642-a6ce7173ec60').
narrative_ontology:cs_kernel_codification('12de6172-efbb-4602-9642-a6ce7173ec60', distributed).
narrative_ontology:cs_authority_grounding('12de6172-efbb-4602-9642-a6ce7173ec60', distributed).
narrative_ontology:cs_reading_relation('12de6172-efbb-4602-9642-a6ce7173ec60', income_support_conditionality__freedom_floor_reading, coexists_with).
narrative_ontology:cs_reading_relation('12de6172-efbb-4602-9642-a6ce7173ec60', income_support_conditionality__dependency_trap_reading, coexists_with).
narrative_ontology:cs_axiom('12de6172-efbb-4602-9642-a6ce7173ec60', foundational, unconditional_transfers_are_captured_by_wage_setters).
narrative_ontology:cs_axiom_status(unconditional_transfers_are_captured_by_wage_setters, holdable).
narrative_ontology:cs_axiom_grounding('12de6172-efbb-4602-9642-a6ce7173ec60', unconditional_transfers_are_captured_by_wage_setters, empirically_contingent).
narrative_ontology:cs_axiom('12de6172-efbb-4602-9642-a6ce7173ec60', secondary, subsistence_guarantee_without_wage_floor_is_incomplete_coordination).
narrative_ontology:cs_axiom_status(subsistence_guarantee_without_wage_floor_is_incomplete_coordination, holdable).
narrative_ontology:cs_axiom_grounding('12de6172-efbb-4602-9642-a6ce7173ec60', subsistence_guarantee_without_wage_floor_is_incomplete_coordination, instrumental).
narrative_ontology:cs_reference_frame('12de6172-efbb-4602-9642-a6ce7173ec60', transfer_as_pure_poverty_alleviation).
narrative_ontology:cs_drift_state('12de6172-efbb-4602-9642-a6ce7173ec60', post_wage_passthrough_studies, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('12de6172-efbb-4602-9642-a6ce7173ec60', '').
narrative_ontology:cs_kernel_id(income_support_conditionality__wage_subsidy_reading, income_support_conditionality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(income_support_conditionality__wage_subsidy_reading, low_wage_employers).
narrative_ontology:constraint_beneficiary(income_support_conditionality__wage_subsidy_reading, gig_platform_operators).
narrative_ontology:constraint_victim(income_support_conditionality__wage_subsidy_reading, low_wage_workers).
narrative_ontology:constraint_victim(income_support_conditionality__wage_subsidy_reading, part_time_service_workers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(income_support_conditionality__wage_subsidy_reading, part_time_service_workers).
narrative_ontology:constraint_victim(income_support_conditionality__wage_subsidy_reading, taxpayers_general).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate businesses in retail, hospitality, and agriculture where labor cost is the dominant input. With a guaranteed income floor in place, they can post wages below what would otherwise be needed to secure subsistence-level labor, because the transfer fills the gap between the wage and survival. They lobby to keep the transfer unconditional and universal rather than tied to employment status, since conditionality on job-seeking would raise their bargaining costs.
narrative_ontology:constraint_stakeholder(income_support_conditionality__wage_subsidy_reading, low_wage_employers, beneficiary,
    organized, biographical, arbitrage, national).

% Price per-task and per-delivery rates assuming workers have a subsistence floor elsewhere. The lower the effective wage they must offer to attract labor, the more the transfer functions as a direct subsidy to their unit economics; they can scale headcount without raising piece rates because the income floor absorbs the difference.
narrative_ontology:constraint_stakeholder(income_support_conditionality__wage_subsidy_reading, gig_platform_operators, beneficiary,
    institutional, generational, arbitrage, global).

% Receive the unconditional transfer but find that nominal wages for the jobs available to them fall or stagnate roughly in proportion to the transfer's value, because employers adjust postings to the new floor. Their total income rises little or not at all relative to a counterfactual without the transfer and with a binding minimum wage; leaving low-wage work entirely is nominally possible (the transfer alone covers subsistence) but abandons any path to higher earnings, since the labor market recalibrates around the subsidized wage.
narrative_ontology:constraint_stakeholder(income_support_conditionality__wage_subsidy_reading, low_wage_workers, payer,
    powerless, biographical, constrained, national).

% Depend on both the transfer and supplemental part-time wages to reach a livable income. As employers reduce hours or rates knowing the transfer covers the shortfall, these workers see the transfer partially redirected into a slimmer wage bill rather than into an improved standard of living; they benefit from the floor existing at all, but the specific benefit is captured upstream by their employer's pricing decisions.
narrative_ontology:constraint_stakeholder(income_support_conditionality__wage_subsidy_reading, part_time_service_workers, payer,
    powerless, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(income_support_conditionality__wage_subsidy_reading, part_time_service_workers, beneficiary).

% Designs and disburses the unconditional transfer, sets its universality and amount, and defends it publicly as a poverty-reduction and dignity measure. Does not set wage floors or monitor whether employer pricing absorbs the transfer's value; treats wage-setting as outside its jurisdiction even though its own transfer design shapes the wage-setting environment.
narrative_ontology:constraint_stakeholder(income_support_conditionality__wage_subsidy_reading, transfer_administering_agency, agenda_setter,
    institutional, generational, analytical, national).

% Would argue for binding minimum-wage floors and sectoral bargaining alongside any income transfer, precisely to prevent employer capture of the subsidy through wage adjustment. Rarely consulted in transfer design, which is typically negotiated between fiscal policymakers and business lobbies rather than through labor-market bargaining institutions.
narrative_ontology:constraint_stakeholder(income_support_conditionality__wage_subsidy_reading, organized_labor, excluded,
    organized, biographical, mobile, national).

% Fund the transfer through general taxation. Where the transfer functions as a wage subsidy, the public purse effectively covers part of employer labor costs that would otherwise appear on employer balance sheets, transferring a private cost onto a public one without a corresponding public return beyond subsistence maintenance.
narrative_ontology:constraint_stakeholder(income_support_conditionality__wage_subsidy_reading, taxpayers_general, payer,
    moderate, generational, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(income_support_conditionality__wage_subsidy_reading, low_wage_employers).
narrative_ontology:fixing_cost_class(income_support_conditionality__wage_subsidy_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Guarantees a subsistence floor so that no worker starves or is forced into destitution regardless of employment status, solving a genuine collective problem of income volatility and labor-market exclusion.
% TRANSFER_FUNCTION: Moves general tax revenue to workers as an income floor, but because employers reset wage offers against that floor, a portion of the transfer is re-captured as reduced labor cost for employers — money nominally sent to workers ends up subsidizing employer payrolls.
% ABSENT_VOICES: Organized labor and wage-floor advocates are structurally absent from transfer design, which is typically negotiated between treasury officials and employer associations; their institutional counter-proposal (binding wage floors alongside the transfer) is rarely on the table.
% DISAPPEARANCE_RATIONALE: Employers and platforms would face pressure to raise nominal wages to retain labor if the transfer vanished, so their pricing would rearrange. Workers dispute whether they would be better or worse off net of the lost subsistence floor. The rearrangement is real but its direction and magnitude are exactly what the sibling readings disagree about.
% FOUNDING_PROBLEM: Designed to solve destitution and the coverage gaps of conditional welfare (means-testing failures, bureaucratic exclusion, precarity from job loss) by giving every person an unconditional subsistence floor.
% FOUNDING_PROBLEM_CORROBORATION: Labor economists studying wage pass-through in transfer-receiving regions attest that employer wage offers adjust downward following transfer introduction in low-bargaining-power sectors — this is corroboration from outside the employer beneficiary set. The administering agency and employer associations attest the founding problem (destitution) remains solved and characterize wage effects as incidental; that attestation comes from parties who benefit from the subsidy reading being wrong, so it does not independently corroborate.
narrative_ontology:disappearance_verdict(income_support_conditionality__wage_subsidy_reading, contested).
narrative_ontology:founding_problem_status(income_support_conditionality__wage_subsidy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(income_support_conditionality__wage_subsidy_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(income_support_conditionality__wage_subsidy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(income_support_conditionality__wage_subsidy_reading, 0.62, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(income_support_conditionality__wage_subsidy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(income_support_conditionality__wage_subsidy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(income_support_conditionality__wage_subsidy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) reflects that a substantial — though not total — share of the transfer's value is captured through employer wage adjustment rather than reaching workers as net income gain; it is not 1.0 because some portion of the transfer does genuinely raise worker welfare, especially for those outside employment altogether. Suppression (0.48) is moderate: there is no direct coercion, but structural dependency on low-wage sectors combined with the absence of a binding wage floor constrains workers' ability to resist the repricing dynamic. Theater ratio (0.40) is elevated because the transfer is publicly defended in poverty-alleviation terms while its wage-market side effects go largely unaddressed by its administrators — the public justification increasingly diverges from a fuller account of its distributive effects as pass-through accumulates. Accessibility collapse (0.50) is moderate: workers could in principle organize for wage floors or exit low-wage sectors, but the transfer's existence somewhat reduces the political urgency of doing so. Resistance (0.55) is substantial because labor advocates and some economists actively contest the wage-subsidy framing.
 *
 * PERSPECTIVAL GAP:
 *   From the administering agency's seat, this looks like a rope: a clean poverty-reduction mechanism with no obvious extraction. From the worker's seat, the same structure computes as tangled — genuine subsistence protection bundled with a wage-suppression mechanism they cannot see directly because it operates through employer pricing decisions rather than through the transfer's own terms. The engine's per-seat computation should surface this divergence rather than requiring the story to resolve it in either direction.
 *
 * DIRECTIONALITY LOGIC:
 *   Low-wage employers and gig platforms are structural beneficiaries: they capture the transfer's value through wage-cost reduction without directly administering or funding it, and their exit options (arbitrage — relocating hiring, adjusting piece rates) put them near the beneficiary end of directionality. Low-wage and part-time workers are targets: they are the nominal recipients but the value is partially redirected upstream, and their constrained exit (few alternative sectors, geographic and skill limits) pushes their effective directionality toward the target end despite formally 'receiving' the transfer. Taxpayers are diffuse payers with no meaningful exit (trapped, general taxation) — they fund a subsidy whose ultimate beneficiary is not always the party the policy is named for.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (destitution, welfare-coverage gaps) remains partially live for those outside employment, but for the specific subpopulation of low-wage employed workers, the transfer's function has partially drifted from poverty alleviation toward wage subsidization — a shift the administering agency's framing does not acknowledge. Classifying this as tangled_rope rather than snare preserves the genuine coordination function (subsistence guarantee, which really does prevent destitution) while still naming the asymmetric extraction (employer capture via repricing) that requires active enforcement — in this case, the enforcement being the political maintenance of transfer unconditionality against proposals to pair it with binding wage floors.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    wage_passthrough_magnitude,
    'What fraction of the transfer''s fiscal value is actually captured by employers through wage adjustment, versus retained by workers as net income gain?',
    'Empirical labor-market studies comparing wage offers in transfer-receiving regions against matched control regions without the transfer, controlling for local labor-market tightness and minimum-wage law.',
    'High pass-through (>50%) strongly supports the wage_subsidy reading and the tangled_rope classification; low pass-through (<15%) would support the freedom_floor reading instead and argue against this reading applying to that context.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(wage_passthrough_magnitude, empirical, 'How much of the transfer employers capture via wage repricing.').

omega_variable(
    kernel_reading_selection_ambiguity,
    'Is the kernel text of unconditional income support itself neutral between the wage_subsidy, freedom_floor, and dependency_trap readings, or does the specific design (universality, amount, absence of paired wage floors) already commit it to one reading over the others?',
    'Comparative institutional analysis: transfers introduced alongside binding wage floors or sectoral bargaining versus transfers introduced without them, observing which reading''s predicted effects actually materialize in each design.',
    'If design variables determine the reading, then ''income support'' is not one contested kernel but a family of distinct policy instruments each entailing a different constraint — this would argue for even finer decomposition than the current three-reading split.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_ambiguity, conceptual, 'Whether design details already select among the three kernel readings.').

omega_variable(
    employer_bargaining_power_variation,
    'Does the wage-subsidy dynamic hold uniformly, or only in sectors where employers already hold strong wage-setting power (monopsony-like local labor markets, gig platforms with algorithmic pricing)?',
    'Cross-sector comparison of wage pass-through in concentrated versus competitive local labor markets following transfer introduction.',
    'If the effect is concentrated in monopsony sectors, the tangled_rope classification may only hold for a subset of employers/workers, not economy-wide — this would refine rather than overturn the classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(employer_bargaining_power_variation, empirical, 'Whether wage capture depends on employer market power.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(income_support_conditionality__wage_subsidy_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(inco_tr_t0, income_support_conditionality__wage_subsidy_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(inco_tr_t4, income_support_conditionality__wage_subsidy_reading, theater_ratio, 4, 0.25).
narrative_ontology:measurement(inco_tr_t8, income_support_conditionality__wage_subsidy_reading, theater_ratio, 8, 0.29).
narrative_ontology:measurement(inco_tr_t12, income_support_conditionality__wage_subsidy_reading, theater_ratio, 12, 0.32).
narrative_ontology:measurement(inco_tr_t16, income_support_conditionality__wage_subsidy_reading, theater_ratio, 16, 0.35).
narrative_ontology:measurement(inco_tr_t20, income_support_conditionality__wage_subsidy_reading, theater_ratio, 20, 0.38).
narrative_ontology:measurement(inco_tr_t24, income_support_conditionality__wage_subsidy_reading, theater_ratio, 24, 0.4).

% Extraction over time
narrative_ontology:measurement(inco_be_t0, income_support_conditionality__wage_subsidy_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(inco_be_t4, income_support_conditionality__wage_subsidy_reading, base_extractiveness, 4, 0.42).
narrative_ontology:measurement(inco_be_t8, income_support_conditionality__wage_subsidy_reading, base_extractiveness, 8, 0.48).
narrative_ontology:measurement(inco_be_t12, income_support_conditionality__wage_subsidy_reading, base_extractiveness, 12, 0.53).
narrative_ontology:measurement(inco_be_t16, income_support_conditionality__wage_subsidy_reading, base_extractiveness, 16, 0.57).
narrative_ontology:measurement(inco_be_t20, income_support_conditionality__wage_subsidy_reading, base_extractiveness, 20, 0.6).
narrative_ontology:measurement(inco_be_t24, income_support_conditionality__wage_subsidy_reading, base_extractiveness, 24, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(inco_su_t0, income_support_conditionality__wage_subsidy_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(inco_su_t4, income_support_conditionality__wage_subsidy_reading, suppression_requirement, 4, 0.34).
narrative_ontology:measurement(inco_su_t8, income_support_conditionality__wage_subsidy_reading, suppression_requirement, 8, 0.38).
narrative_ontology:measurement(inco_su_t12, income_support_conditionality__wage_subsidy_reading, suppression_requirement, 12, 0.41).
narrative_ontology:measurement(inco_su_t16, income_support_conditionality__wage_subsidy_reading, suppression_requirement, 16, 0.44).
narrative_ontology:measurement(inco_su_t20, income_support_conditionality__wage_subsidy_reading, suppression_requirement, 20, 0.46).
narrative_ontology:measurement(inco_su_t24, income_support_conditionality__wage_subsidy_reading, suppression_requirement, 24, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(income_support_conditionality__wage_subsidy_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(income_support_conditionality__wage_subsidy_reading, 0.15).
narrative_ontology:affects_constraint(income_support_conditionality__wage_subsidy_reading, freedom_floor_reading).
narrative_ontology:affects_constraint(income_support_conditionality__wage_subsidy_reading, dependency_trap_reading).
narrative_ontology:affects_constraint(income_support_conditionality__wage_subsidy_reading, minimum_wage_floor_policy).
narrative_ontology:affects_constraint(income_support_conditionality__wage_subsidy_reading, gig_platform_piece_rate_setting).

% DUAL FORMULATION NOTE:
% This story is one of three ε-invariant readings of the income_support_conditionality kernel. wage_subsidy_reading (this file) authors employers/platforms as beneficiaries and low-wage/part-time workers as victims, classified tangled_rope. freedom_floor_reading authors workers as beneficiaries with no victims, classified closer to rope, on the premise that decommodification dominates wage pass-through. dependency_trap_reading authors recipients themselves as bearing long-run costs (skill atrophy, incentive erosion) rather than employers capturing value, with a different beneficiary/victim structure again. The three do not average into a single ε; each is a distinct constraint sharing only the kernel text (the unconditional-transfer design) as common cause.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
