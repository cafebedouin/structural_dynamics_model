% ============================================================================
% CONSTRAINT STORY: redistributive_stabilization_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_redistributive_stabilization_reading, []).

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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: redistributive_stabilization_reading
 *   human_readable: Redistributive Stabilization Reading of the Stability-Legitimacy Kernel
 *   domain: political_economy/democratic_theory
 *
 * SUMMARY:
 *   This story instantiates the redistributive-stabilization reading of a
 *   contested kernel: what actually makes an unequal market order legitimate
 *   and durable. On this reading, stability is bought cheaply — Piketty's
 *   hospitals-and-schools argument — through wealth taxation and transfer
 *   spending calibrated to keep after-tax capital returns from permanently
 *   outrunning growth (r ≈ g). Both elites and citizens end up as
 *   beneficiaries: citizens get material sufficiency, elites get a
 *   legitimated, socially peaceful property order at a price far below what
 *   expropriation or revolutionary rupture would cost them. No victim class
 *   is required for this reading to hold — the arrangement is read as a
 *   solvable coordination problem, not as extraction with a manufactured
 *   coordination cover. The taxation apparatus requires active enforcement (a
 *   functioning fiscal state, real collection capacity) but the story's own
 *   metrics describe low extraction and low suppression once the arrangement
 *   is adopted and stable.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(redistributive_stabilization_reading, 0.28).
domain_priors:suppression_score(redistributive_stabilization_reading, 0.22).
domain_priors:theater_ratio(redistributive_stabilization_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(redistributive_stabilization_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(redistributive_stabilization_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(redistributive_stabilization_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(redistributive_stabilization_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(redistributive_stabilization_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(redistributive_stabilization_reading, rope).
narrative_ontology:human_readable(redistributive_stabilization_reading, "Redistributive Stabilization Reading of the Stability-Legitimacy Kernel").
narrative_ontology:topic_domain(redistributive_stabilization_reading, "political_economy/democratic_theory").

domain_priors:requires_active_enforcement(redistributive_stabilization_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(redistributive_stabilization_reading, '50309c3d-ff38-45e4-92c7-0769b2900843').
narrative_ontology:cs_kernel_codification('50309c3d-ff38-45e4-92c7-0769b2900843', distributed).
narrative_ontology:cs_authority_grounding('50309c3d-ff38-45e4-92c7-0769b2900843', distributed).
narrative_ontology:cs_reading_relation('50309c3d-ff38-45e4-92c7-0769b2900843', redistributive_stabilization_reading__repressive_stabilization_reading, coexists_with).
narrative_ontology:cs_reading_relation('50309c3d-ff38-45e4-92c7-0769b2900843', redistributive_stabilization_reading__collapse_inevitability_reading, coexists_with).
narrative_ontology:cs_reading_relation('50309c3d-ff38-45e4-92c7-0769b2900843', redistributive_stabilization_reading__democratic_legitimacy_reading, influences).
narrative_ontology:cs_axiom('50309c3d-ff38-45e4-92c7-0769b2900843', foundational, redistribution_produces_mutual_benefit_legitimacy).
narrative_ontology:cs_axiom_status(redistribution_produces_mutual_benefit_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('50309c3d-ff38-45e4-92c7-0769b2900843', redistribution_produces_mutual_benefit_legitimacy, empirically_contingent).
narrative_ontology:cs_axiom('50309c3d-ff38-45e4-92c7-0769b2900843', secondary, r_approx_g_is_sufficient_stability_condition).
narrative_ontology:cs_axiom_status(r_approx_g_is_sufficient_stability_condition, holdable).
narrative_ontology:cs_axiom_grounding('50309c3d-ff38-45e4-92c7-0769b2900843', r_approx_g_is_sufficient_stability_condition, instrumental).
narrative_ontology:cs_reference_frame('50309c3d-ff38-45e4-92c7-0769b2900843', postwar_social_democratic_settlement).
narrative_ontology:cs_drift_state('50309c3d-ff38-45e4-92c7-0769b2900843', post_2008_wealth_concentration_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('50309c3d-ff38-45e4-92c7-0769b2900843', '').
narrative_ontology:cs_kernel_id(redistributive_stabilization_reading, stability_legitimacy_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(redistributive_stabilization_reading, median_income_citizens).
narrative_ontology:constraint_beneficiary(redistributive_stabilization_reading, capital_holding_elites).
narrative_ontology:constraint_beneficiary(redistributive_stabilization_reading, the_state_apparatus).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(redistributive_stabilization_reading, capital_holding_elites).
narrative_ontology:constraint_vindicates(redistributive_stabilization_reading, r_approx_g_norm_sustainability).
narrative_ontology:constraint_vindicates(redistributive_stabilization_reading, material_sufficiency_legitimacy_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receive public hospitals, schools, pensions, and transfers funded substantially by wealth and capital taxation. Their material security and mobility depend on the transfer system holding; they have no realistic exit from the national fiscal arrangement but broadly consent to it because it delivers visible services.
narrative_ontology:constraint_stakeholder(redistributive_stabilization_reading, median_income_citizens, beneficiary,
    organized, generational, constrained, national).

% Pay wealth and capital taxes calibrated to keep after-tax return on capital (r) from permanently outrunning growth (g). In exchange they get a stable property regime, social peace, enforceable contracts, and a legitimated claim to remaining wealth. Some have partial exit via jurisdictional arbitrage, but most retain a stake in the domestic order that makes wholesale flight costly.
narrative_ontology:constraint_stakeholder(redistributive_stabilization_reading, capital_holding_elites, beneficiary,
    powerful, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(redistributive_stabilization_reading, capital_holding_elites, payer).

% Designs and administers the tax-and-transfer schedule, sets the r ≈ g target implicitly through fiscal and tax policy, and justifies taxation as the price of legitimacy and social peace rather than punitive redistribution. Collects no personal rent; its interest is in the arrangement's continued function as a coordination device.
narrative_ontology:constraint_stakeholder(redistributive_stabilization_reading, the_state_apparatus, agenda_setter,
    institutional, generational, analytical, national).

% Depend most heavily on the transfer floor but have the least voice in setting the tax rate or the r ≈ g target; benefit from the arrangement's stability but are not consulted on its design and would bear the sharpest costs if the transfer floor were cut.
narrative_ontology:constraint_stakeholder(redistributive_stabilization_reading, low_wealth_precarious_households, excluded,
    powerless, immediate, trapped, local).

% Study whether the r ≈ g norm and redistribution actually produce durable legitimacy or merely defer contestation. Track wealth concentration data, tax incidence, and public trust indicators to test whether the coordination story holds.
narrative_ontology:constraint_stakeholder(redistributive_stabilization_reading, political_economists, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a genuine collective-action problem: without some mechanism holding capital returns close to growth and funding broad material sufficiency, wealth concentration compounds until social peace and property security themselves become unsustainable for everyone, including capital holders. Taxation-funded hospitals and schools purchase durable legitimacy for the property order at a price both sides can sustain.
% TRANSFER_FUNCTION: Moves a calibrated share of capital income and accumulated wealth from capital-holding elites to public provision (hospitals, schools, pensions, transfers) reaching median and low-income citizens, administered by the state.
% ABSENT_VOICES: Low-wealth precarious households benefit most from the transfer floor but have the least influence over where the r ≈ g target is set or how steeply the tax schedule bites; their material dependence is real but their bargaining power in setting the terms is thin.
% DISAPPEARANCE_RATIONALE: If the redistributive mechanism vanished, capital returns would compound unchecked relative to growth, public services would erode, and the material-sufficiency basis for consent to the existing property order would collapse — reopening exactly the legitimacy contest the arrangement exists to close.
% FOUNDING_PROBLEM: Unchecked capital concentration (r persistently exceeding g) erodes the material basis of mass consent to a market economy and property regime, threatening the property order's own survival through unrest, expropriation, or revolutionary rupture.
% FOUNDING_PROBLEM_CORROBORATION: Long-run wealth-concentration data (Piketty, Saez, Zucman) and cross-national studies of tax incidence and public trust, produced by academic economists outside both the state apparatus and capital-holding constituencies, corroborate that the r-g gap remains a live structural pressure rather than a resolved historical episode.
narrative_ontology:disappearance_verdict(redistributive_stabilization_reading, world_rearranges).
narrative_ontology:founding_problem_status(redistributive_stabilization_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(redistributive_stabilization_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-11',
    'unspecified', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'unspecified').
narrative_ontology:story_seed(redistributive_stabilization_reading, 'none', 1).
narrative_ontology:epsilon_provenance(redistributive_stabilization_reading, 0.28, 'claude-sonnet-5', 'surveillance_guillotines_2026_20260811_115130', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(redistributive_stabilization_reading_tests).
:- end_tests(redistributive_stabilization_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.28 at interval end) because, by this reading's own lights, the wealth tax is calibrated to a sustainable r ≈ g target rather than punitive confiscation — it returns social peace and enforceable property rights to the payers. Suppression is low (0.22) because compliance rests substantially on perceived legitimacy and the visible return of hospitals and schools, not on heavy coercive enforcement; some tax administration and enforcement infrastructure is real (hence requires_active_enforcement: true) but it is not the dominant mechanism holding the arrangement together. Theater ratio is modest (0.20) reflecting that some redistributive spending is symbolic or slow to materialize relative to promise, without dominating the function. All three metrics drift only mildly upward across the interval, consistent with a coordination arrangement under gradual strain rather than one degrading into extraction.
 *
 * PERSPECTIVAL GAP:
 *   From the state's administrative seat and from the citizen and elite beneficiary seats, this reading computes as low-extraction coordination. A structurally different reading of the same underlying kernel — the repressive-stabilization reading — would treat the identical tax-and-transfer schedule as enforced pacification of a restive population, with elites as targets bearing coerced extraction and low-wealth households as an unacknowledged victim class. This story does not adjudicate between readings; it authors only the redistributive-stabilization account cleanly, per the ε-invariance principle.
 *
 * DIRECTIONALITY LOGIC:
 *   Both capital-holding elites and median-income citizens are declared beneficiaries under this reading: citizens receive material sufficiency directly; elites receive a durable, legitimated claim to their remaining wealth and social peace, which this reading treats as a real benefit that outweighs the tax paid. The state apparatus is the agenda-setter, administering the schedule without capturing rents itself. Low-wealth precarious households are marked excluded rather than victim — they benefit from the floor but lack a voice in setting its terms, which is a governance-input gap, not an extraction relationship.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (capital concentration eroding mass consent to the property order) is authored as live, corroborated by long-run wealth-concentration data from outside both the state and capital-holding constituencies. Because the founding problem remains live and the disappearance verdict is world_rearranges, this reading does not present as a hollowed-out mandate — the coordination function it claims is still doing real work by its own lights, distinguishing it from a scaffold or piton reading of the same kernel.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    material_sufficiency_vs_procedural_legitimacy,
    'Does legitimacy in this reading actually derive from material sufficiency (hospitals, schools, transfers) as claimed, or is the redistributive schedule itself downstream of procedural democratic consent that could hold independently of the transfer level?',
    'Comparative study of polities with similar transfer generosity but different procedural-democratic quality (or vice versa) to see which predicts stability better.',
    'If procedural consent is doing the real legitimating work, this reading is misattributing causal weight to material sufficiency and is closer to the democratic_legitimacy_reading than it claims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(material_sufficiency_vs_procedural_legitimacy, conceptual, 'Whether material sufficiency or procedural consent is the true legitimating mechanism.').

omega_variable(
    r_g_target_construction_or_discovery,
    'Is the r ≈ g norm a natural equilibrium condition for social peace, or a politically constructed target that happens to serve elite interests in avoiding more aggressive redistribution?',
    'Historical comparison of periods and polities where r ≈ g was breached without collapse, versus periods where breach preceded upheaval, controlling for other legitimacy variables.',
    'If r ≈ g is a constructed target rather than a discovered stability threshold, capital_holding_elites function partly as an agenda-setting beneficiary shaping the target itself, which would push this reading toward tangled_rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(r_g_target_construction_or_discovery, empirical, 'Whether the r≈g threshold is a discovered equilibrium or an elite-favorable political construction.').

omega_variable(
    excluded_household_voice_gap,
    'Does the absence of low_wealth_precarious_households from setting the tax-and-transfer schedule constitute a benign governance gap (as this reading holds) or a suppressed victim relationship that this reading is structurally motivated to understate?',
    'Track whether transfer floors are cut disproportionately during fiscal contraction relative to capital tax rates, and whether excluded households had any effective recourse.',
    'If cuts fall disproportionately on the excluded group during contraction, the excluded/beneficiary framing understates a latent victim relationship and the reading would need revision toward tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(excluded_household_voice_gap, empirical, 'Whether excluded low-wealth households are genuinely non-victims or an undercounted victim class.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(redistributive_stabilization_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(redi_tr_t0, redistributive_stabilization_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(redi_tr_t8, redistributive_stabilization_reading, theater_ratio, 8, 0.14).
narrative_ontology:measurement(redi_tr_t16, redistributive_stabilization_reading, theater_ratio, 16, 0.16).
narrative_ontology:measurement(redi_tr_t24, redistributive_stabilization_reading, theater_ratio, 24, 0.18).
narrative_ontology:measurement(redi_tr_t32, redistributive_stabilization_reading, theater_ratio, 32, 0.19).
narrative_ontology:measurement(redi_tr_t40, redistributive_stabilization_reading, theater_ratio, 40, 0.2).

% Extraction over time
narrative_ontology:measurement(redi_be_t0, redistributive_stabilization_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(redi_be_t8, redistributive_stabilization_reading, base_extractiveness, 8, 0.24).
narrative_ontology:measurement(redi_be_t16, redistributive_stabilization_reading, base_extractiveness, 16, 0.26).
narrative_ontology:measurement(redi_be_t24, redistributive_stabilization_reading, base_extractiveness, 24, 0.27).
narrative_ontology:measurement(redi_be_t32, redistributive_stabilization_reading, base_extractiveness, 32, 0.28).
narrative_ontology:measurement(redi_be_t40, redistributive_stabilization_reading, base_extractiveness, 40, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(redi_su_t0, redistributive_stabilization_reading, suppression_requirement, 0, 0.18).
narrative_ontology:measurement(redi_su_t8, redistributive_stabilization_reading, suppression_requirement, 8, 0.19).
narrative_ontology:measurement(redi_su_t16, redistributive_stabilization_reading, suppression_requirement, 16, 0.2).
narrative_ontology:measurement(redi_su_t24, redistributive_stabilization_reading, suppression_requirement, 24, 0.21).
narrative_ontology:measurement(redi_su_t32, redistributive_stabilization_reading, suppression_requirement, 32, 0.22).
narrative_ontology:measurement(redi_su_t40, redistributive_stabilization_reading, suppression_requirement, 40, 0.22).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(redistributive_stabilization_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(redistributive_stabilization_reading, 0.15).
narrative_ontology:affects_constraint(redistributive_stabilization_reading, repressive_stabilization_reading).
narrative_ontology:affects_constraint(redistributive_stabilization_reading, collapse_inevitability_reading).
narrative_ontology:affects_constraint(redistributive_stabilization_reading, democratic_legitimacy_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of four sibling readings of the stability_legitimacy_kernel. Each reading authors a distinct beneficiary/victim structure and a distinct epsilon under the same kernel text (what makes an unequal order stable and legitimate). This reading (redistributive_stabilization) posits mutual elite/citizen benefit and low extraction; repressive_stabilization posits coerced pacification with elite/state extraction from a citizen victim class; collapse_inevitability posits the redistributive fix as structurally doomed and thus low-legitimacy theater; democratic_legitimacy relocates the legitimating mechanism to procedural consent rather than material transfer. All four are linked via affects_constraints per the epsilon-invariance decomposition principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
