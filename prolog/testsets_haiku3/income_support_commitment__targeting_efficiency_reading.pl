% ============================================================================
% CONSTRAINT STORY: income_support_commitment__targeting_efficiency_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
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
 *   human_readable: Means-Tested Income Support Concentration (Targeting Efficiency Reading)
 *   domain: political_economy/social_policy
 *
 * SUMMARY:
 *   This constraint story instantiates ONE READING of the contested kernel of
 *   income support commitment: the targeting-efficiency reading, which holds
 *   that public income support should be concentrated on demonstrated need
 *   rather than universally distributed. The reading claims that finite
 *   public resources are best allocated by means-testing — by directing
 *   support toward those with the greatest measured deprivation. Under this
 *   reading, a person earning $31,100 annually receives targeted support;
 *   under the freedom-floor reading (sibling), that same person would receive
 *   unconditional universal income; under the dependency-trap reading
 *   (sibling), that person might receive work-conditioned support to avoid
 *   behavioral hazard. This story describes ONLY the targeting-efficiency
 *   instantiation. The constraint operates as a snare: the poor are nominally
 *   the beneficiaries (they receive the support) but are simultaneously the
 *   victims (they must prove deprivation to access it, face cliff effects at
 *   income thresholds, and are trapped in the means-testing apparatus
 *   itself). The extraction comes not from the income transfer itself but
 *   from the bureaucratic and behavioral costs of the targeting mechanism,
 *   which subordinates recipient autonomy to administrator determination and
 *   creates perverse incentives to remain below thresholds. The reading's
 *   beneficiaries are fiscal conservatives (who get spending caps justified
 *   by scarcity), program administrators (whose institutional authority and
 *   career pathways depend on the targeting apparatus), and efficiency
 *   advocates (whose intellectual authority derives from the technical
 *   frame). The founding problem — resource scarcity — is contested outside
 *   the benefiting constituency; corroboration for it comes primarily from
 *   those who profit from the constraint.
 *
 * KEY AGENTS:
 *   - low_income_households_targeted_program_current_recipients — powerless, trapped in means-testing, bear the extraction through documentation burdens, cliff effects, and loss of autonomy
 *   - program_administrators_and_gatekeepers — institutional, arbitrage exit, agenda-setters who control the targeting determination process and justify scarcity
 *   - fiscal_conservatives_and_efficiency_advocates — powerful, organized, beneficiaries of the scarcity-frames that justify spending caps
 *   - labor_market_advocates_behavioral_assumption_constituency — powerful, beneficiaries of the work-incentive assumptions embedded in targeting logic
 *   - universal_income_advocates_and_freedom_floor_constituency — organized, excluded from the primary debate, trapped by the framing constraints of the targeting-efficiency reading
 *   - legislatures_and_elected_bodies — institutional, agenda-setters operating within the fiscal constraint narrative the targeting reading establishes
 *   - persons_above_targeting_threshold — moderate, mobile, excluded by the eligibility rules themselves
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(income_support_commitment__targeting_efficiency_reading, 0.71).
domain_priors:suppression_score(income_support_commitment__targeting_efficiency_reading, 0.68).
domain_priors:theater_ratio(income_support_commitment__targeting_efficiency_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(income_support_commitment__targeting_efficiency_reading, extractiveness, 0.71).
narrative_ontology:constraint_metric(income_support_commitment__targeting_efficiency_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(income_support_commitment__targeting_efficiency_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(income_support_commitment__targeting_efficiency_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(income_support_commitment__targeting_efficiency_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(income_support_commitment__targeting_efficiency_reading, snare).
narrative_ontology:human_readable(income_support_commitment__targeting_efficiency_reading, "Means-Tested Income Support Concentration (Targeting Efficiency Reading)").
narrative_ontology:topic_domain(income_support_commitment__targeting_efficiency_reading, "political_economy/social_policy").

domain_priors:requires_active_enforcement(income_support_commitment__targeting_efficiency_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(income_support_commitment__targeting_efficiency_reading, '5dfee597-d5ad-4e29-b2de-1c5e5f625464').
narrative_ontology:cs_kernel_codification('5dfee597-d5ad-4e29-b2de-1c5e5f625464', distributed).
narrative_ontology:cs_authority_grounding('5dfee597-d5ad-4e29-b2de-1c5e5f625464', extraction).
narrative_ontology:cs_interpretation_layer_present('5dfee597-d5ad-4e29-b2de-1c5e5f625464').
narrative_ontology:cs_reading_relation('5dfee597-d5ad-4e29-b2de-1c5e5f625464', income_support_commitment__freedom_floor_reading, coexists_with).
narrative_ontology:cs_reading_relation('5dfee597-d5ad-4e29-b2de-1c5e5f625464', income_support_commitment__dependency_trap_reading, coexists_with).
narrative_ontology:cs_axiom('5dfee597-d5ad-4e29-b2de-1c5e5f625464', foundational, resource_scarcity_binding_constraint).
narrative_ontology:cs_axiom_status(resource_scarcity_binding_constraint, holdable).
narrative_ontology:cs_axiom_grounding('5dfee597-d5ad-4e29-b2de-1c5e5f625464', resource_scarcity_binding_constraint, empirically_contingent).
narrative_ontology:cs_axiom('5dfee597-d5ad-4e29-b2de-1c5e5f625464', secondary, targeting_efficiency_optimization_imperative).
narrative_ontology:cs_axiom_status(targeting_efficiency_optimization_imperative, holdable).
narrative_ontology:cs_axiom_grounding('5dfee597-d5ad-4e29-b2de-1c5e5f625464', targeting_efficiency_optimization_imperative, instrumental).
narrative_ontology:cs_reference_frame('5dfee597-d5ad-4e29-b2de-1c5e5f625464', resource_scarcity_constrains_distribution).
narrative_ontology:cs_drift_state('5dfee597-d5ad-4e29-b2de-1c5e5f625464', contemporary_universal_income_advocacy_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('5dfee597-d5ad-4e29-b2de-1c5e5f625464', '').
narrative_ontology:cs_kernel_id(income_support_commitment__targeting_efficiency_reading, income_support_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_victim(income_support_commitment__targeting_efficiency_reading, low_income_households_targeted_program_current_recipients).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(income_support_commitment__targeting_efficiency_reading, low_income_households_targeted_program_current_recipients).
narrative_ontology:constraint_beneficiary(income_support_commitment__targeting_efficiency_reading, fiscal_conservatives_and_efficiency_advocates).
narrative_ontology:constraint_beneficiary(income_support_commitment__targeting_efficiency_reading, labor_market_advocates_behavioral_assumption_constituency).
narrative_ontology:constraint_vindicates(income_support_commitment__targeting_efficiency_reading, fiscal_scarcity_axiom).
narrative_ontology:constraint_vindicates(income_support_commitment__targeting_efficiency_reading, behavioral_hazard_efficiency_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Currently receive concentrated income support calibrated to demonstrated need (e.g., Queens parent receives $31,100 annually through targeted programs including SNAP, LIHEAP, child tax credits, subsidized childcare). Under this reading's logic, their support is justified and efficient because it targets actual scarcity. However, the targeting mechanism itself requires proving deprivation, submitting to income verification, and navigating multiple bureaucratic systems. The extraction operates through mandatory documentation of poverty status; their benefit persists only so long as they remain below the threshold, creating a perverse incentive structure around remaining poor to retain support. Exit from the constraint would mean earning above the threshold and losing the support entirely — a cliff effect that operates as a poverty trap.
narrative_ontology:constraint_stakeholder(income_support_commitment__targeting_efficiency_reading, low_income_households_targeted_program_current_recipients, payer,
    powerless, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(income_support_commitment__targeting_efficiency_reading, low_income_households_targeted_program_current_recipients, beneficiary).

% Administer the targeting mechanism, set eligibility criteria, conduct means-testing, and enforce program boundaries. They maintain institutional authority over who is 'deserving' based on demonstrated need. Their power derives from control of the determination process itself — the ability to define what counts as need and what counts as proof. They justify the system as responsible stewardship of scarce public resources and prevention of 'moral hazard.' Institutionally invested in the perpetuation of the targeting apparatus because their administrative role, job classifications, and career pathways depend on its continuation.
narrative_ontology:constraint_stakeholder(income_support_commitment__targeting_efficiency_reading, program_administrators_and_gatekeepers, agenda_setter,
    institutional, generational, arbitrage, national).

% Benefit from the targeting-efficiency frame because it legitimates spending constraints and resource scarcity as inevitable rather than chosen. The claim that 'we can only help those who truly need it' becomes political cover for total spending caps and benefit erosion. They cite efficiency arguments to justify refusing expansions even when marginal costs would be trivial. The reading provides intellectual authority for austerity; they advocate for it across media, think tanks, and legislative settings.
narrative_ontology:constraint_stakeholder(income_support_commitment__targeting_efficiency_reading, fiscal_conservatives_and_efficiency_advocates, beneficiary,
    powerful, generational, arbitrage, national).

% Benefit from the targeting-efficiency reading because it embeds a behavioral assumption about work incentives — that unconditional support would reduce labor supply or skill accumulation, and that selective support 'targets' those who would not work anyway. This group includes business-oriented organizations, labor-market economists advancing behavioral-hazard models, and policymakers invested in labor-supply-side narratives. The reading legitimates work requirements, recertification burdens, and the architecture of conditionality itself as efficiency measures rather than as punishment or social policing.
narrative_ontology:constraint_stakeholder(income_support_commitment__targeting_efficiency_reading, labor_market_advocates_behavioral_assumption_constituency, beneficiary,
    powerful, generational, arbitrage, national).

% Argue that the targeting-efficiency reading itself creates extraction and behavioral traps that unconditional support would eliminate. They claim the efficiency frame masks the dignity costs and bureaucratic extraction costs of means-testing. They are structurally excluded from the conversation that legitimates targeting — the policy debate is framed around 'how to target better' rather than 'whether to target at all.' Legislative proposals to move toward universal provision are consistently reframed as infeasible or fiscally irresponsible by the targeting-efficiency logic.
narrative_ontology:constraint_stakeholder(income_support_commitment__targeting_efficiency_reading, universal_income_advocates_and_freedom_floor_constituency, excluded,
    organized, generational, trapped, national).

% Set the total spending envelope for income support, choose between targeted and universal approaches, and enact the statutory framework that grants agencies then administer. They operate within a fiscal constraint narrative (the targeting-efficiency reading influences their choice set) and electoral cycles that make universal expansion politically costly and targeting retrenchment politically easier. Positioned as both agenda-setters (they choose the architecture) and observers (they are influenced by efficiency claims made by economists and administrators).
narrative_ontology:constraint_stakeholder(income_support_commitment__targeting_efficiency_reading, legislatures_and_elected_bodies, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(income_support_commitment__targeting_efficiency_reading, legislatures_and_elected_bodies, observer).

% Earn above the means-testing threshold and are categorically ineligible for targeted support, even if facing genuine material hardship. The targeting logic excludes them as 'not sufficiently in need,' creating a middle-income zone of people who neither qualify for support nor have sufficient income to easily cover major costs. Under a universal reading they would receive the floor; under this targeting reading they are left to the market. Their exclusion is enforced by the eligibility rules themselves and justified by the efficiency frame.
narrative_ontology:constraint_stakeholder(income_support_commitment__targeting_efficiency_reading, persons_above_targeting_threshold_excluded_from_programs, excluded,
    moderate, biographical, mobile, national).

% Provide intellectual justification for the targeting-efficiency reading through cost-benefit analysis, behavioral economics, and fiscal simulations. They author the efficiency claim as technical and apolitical — presenting the constraint as a consequence of mathematical optimization rather than a choice about distribution. Their work circulates through academic journals, policy reports, and legislative testimony, providing the epistemic authority that makes the targeting frame appear inevitable rather than contestable.
narrative_ontology:constraint_stakeholder(income_support_commitment__targeting_efficiency_reading, economists_and_policy_analysts_targeting_efficiency_school, observer,
    organized, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(income_support_commitment__targeting_efficiency_reading, program_administrators_and_gatekeepers).
narrative_ontology:fixing_cost_class(income_support_commitment__targeting_efficiency_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allocates limited public resources toward households experiencing the greatest material hardship, using means-testing to concentrate support where measured need is highest. In this reading, the coordination problem is managing scarcity: if resources are finite, targeting those with the most acute deprivation reduces waste on people who can meet their needs through market or family channels.
% TRANSFER_FUNCTION: Moves income and in-kind support (food, housing assistance, childcare subsidies, heating aid) from the general tax base to households below income thresholds. The magnitude and composition varies by program and state, but the mechanism is means-tested eligibility: you must prove deprivation to receive the transfer.
% ABSENT_VOICES: Universal-income advocates and freedom-floor constituency are structurally excluded from the primary debate. The policy conversation is framed around 'how to target more efficiently' rather than 'whether targeting is the right architecture.' Persons above the threshold who would benefit from a universal floor but are ineligible under means-testing are likewise absent — the eligibility rule silences their claims. Labor-market advocates for unconditional support (citing autonomy, dignity, and exit capacity) are marginalized as economically naive or fiscally irresponsible by the efficiency frame.
% DISAPPEARANCE_RATIONALE: If the targeting-efficiency constraint disappeared — if means-testing were replaced with universal unconditional income support funded through general taxation — the lives of current recipients would change substantially: they would no longer need to prove eligibility or navigate multiple agencies, would not face cliff effects at income thresholds, and would receive benefits regardless of employment status. The administrative apparatus would shrink. The behavioral incentives would shift. The constraint's disappearance would rearrange the distribution of income, the structure of work incentives, and the dignity/autonomy implications of receiving support.
% FOUNDING_PROBLEM: Public budgets are finite. If income support is distributed universally to every adult, the total cost becomes very large relative to available revenue, forcing either high tax rates or lower benefit levels per person. The targeting approach purports to solve this constraint by concentrating support where deprivation is measured to be greatest, thereby maximizing welfare per dollar spent.
% FOUNDING_PROBLEM_CORROBORATION: Fiscal conservatives and targeting-efficiency advocates attest the founding problem is live: resources are genuinely scarce, universal provision would be prohibitively expensive, and targeting is therefore necessary. However, outside this benefiting constituency, the corroboration weakens significantly. Economists studying cross-national welfare systems (Scandinavian models, for instance) attest that universal basic income and high-end income support can coexist with sustainable tax bases — the problem is not insoluble scarcity but *political choice* about redistribution. Persons experiencing the cliff effects of means-testing attest the founding problem has been oversold relative to the bureaucratic extraction costs of proving need. The 'resource scarcity' framing itself has been contested by post-Keynesian and MMT economists who argue fiscal constraints are political, not physical.
narrative_ontology:disappearance_verdict(income_support_commitment__targeting_efficiency_reading, world_rearranges).
narrative_ontology:founding_problem_status(income_support_commitment__targeting_efficiency_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(income_support_commitment__targeting_efficiency_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(income_support_commitment__targeting_efficiency_reading, 'none', 1).
narrative_ontology:epsilon_provenance(income_support_commitment__targeting_efficiency_reading, 0.71, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is high (0.71 at interval end) but rises slowly, stabilizing after interval point 25. The slow rise reflects the gradual tightening of means-testing criteria, incremental erosion of benefit levels in real terms, and accumulation of behavioral conditionality over time — the constraint does not spike but deepens. The final plateau indicates the constraint has reached a stable state where political resistance (resistance=0.72) holds further intensification in check. Suppression is also high (0.68) because the constraint's persistence depends on active enforcement of the eligibility determination process, ongoing bureaucratic gatekeeping, and the framing of scarcity as inevitable rather than chosen. Theater is moderate (0.42) — there is genuine administrative activity (processing applications, conducting verification, managing programs) but a growing share is defensive theater that protects the targeting apparatus itself against universal-provision challenges rather than actually delivering more support. Accessibility collapse (0.58) is moderate because alternatives (universal income, negative income tax, unconditional transfers) are conceptually available and politically articulated by the excluded-voices constituency, so complete collapse does not occur. However, the targeting frame dominates policy conversation and legislative possibility space. One shared time grid was used for all three metrics — each is authored at every time point so the temporal picture is internally coherent. The measurement series tracks the constraint as it has evolved from 1960s (Great Society means-tested programs) to present: extractiveness and suppression have risen with the administrative infrastructure maturation and with pushback against universal proposals; theater has risen as the efficiency framing became more prominent relative to actual benefit delivery.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter and beneficiary seats (administrators, efficiency advocates, fiscal conservatives) perceive this as rational stewardship and technical optimization: the reading is one seat's legitimate policy choice among alternatives. From the payer seat (current recipients), the same structure operates as a dignity-stripping bureaucratic trap — they are forced to disclose their poverty, submit to verification, navigate multiple programs with overlapping eligibility rules, and face perverse incentives to stay poor. The excluded-voices seat (universal advocates) perceives the targeting constraint itself as the problem: it manufactures scarcity narratively and operationalizes it administratively, when unconditional support would eliminate both the scarcity claim and the extraction. The engine computes these seat-level divergences from the power atoms (powerless recipients vs. institutional administrators), exit options (trapped vs. arbitrage), and structural relationships to the constraint. The authored claim (snare) reflects the targeting-efficiency reading's own framing; the metrics reflect its actual operation. The gap between claim and metrics is the measurement space where the constraint's true structure emerges.
 *
 * DIRECTIONALITY LOGIC:
 *   Current recipients are the structural targets (d → 1.0): they are powerless, trapped (no real exit option; earning above the threshold loses support entirely), and bear the extraction through documentation burden, autonomy loss, and behavioral conditionality. Program administrators and efficiency advocates are near the beneficiary end (d → 0.0): they benefit from the constraint's persistence — administrators gain institutional authority and career pathways; efficiency advocates gain intellectual authority from the scarcity frame. Legislators sit near symmetric (d ≈ 0.5): they are influenced by efficiency claims but must also respond to recipient resistance and political pressure for more generous support. Universal-income advocates, though powerful and organized, are structurally excluded rather than coordinated — the constraint exists partly to keep them out of the decision space. Persons above the threshold have moderate power but mobile exit (they can organize politically, work, or relocate), so they sit lower on the target end than recipients but still bear the constraint's normative weight — they are told they are 'not in need' despite genuine hardship, which operates as a low-level extraction through legitimacy denial. The directionality derivation from beneficiary/victim + power + exit is straightforward: beneficiaries are the organized seats with arbitrage exit (administrators, efficiency advocates, fiscal conservatives); victims are the powerless trapped recipients. No overrides are needed because the structural data produces the correct directionality without adjustment.
 *
 * MANDATROPHY ANALYSIS:
 *   The targeting-efficiency reading faces a classic mandatrophy problem: its founding problem (resource scarcity) is now contested rather than live. The founding problem status is 'contested' because evidence from Scandinavian welfare states, post-Keynesian fiscal theory, and historical welfare-spending expansions suggest scarcity is political choice rather than physical inevitability. However, the constraint persists because the agenda-setters and beneficiaries have institutional and intellectual investment in perpetuating the scarcity frame. The constraint is not a response to a live coordination problem but a rent-collection device wearing the mask of necessity. This is the snare signature: the coordination narrative (we must target because resources are limited) is cover for the extraction narrative (administrators and efficiency advocates benefit from the perpetuation of the means-testing apparatus). The mandatrophy diagnosis resolves the classification: the snare reading is accurate because the founding problem's death has not triggered constraint dissolution. Instead, the constraint has calcified into a bureaucratic apparatus maintained by those who gain from it. The alternative readings (freedom-floor, dependency-trap) would dissolve or substantially transform this constraint by reframing the founding problem: freedom-floor reading asks 'what does human dignity require?' rather than 'what do resources permit?'; dependency-trap reading asks 'what behavioral incentives are optimal?' rather than accepting the scarcity constraint. This reading's persistence despite its founding problem's death is the hallmark of mandatrophy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    resource_scarcity_binding_vs_chosen,
    'Is public resource scarcity an objective physical/fiscal constraint, or a politically chosen constraint on redistribution?',
    'Comparative fiscal analysis of welfare states with different targeting architectures; empirical study of tax capacity and deficits in high-support countries; counterfactual fiscal modeling of universal provision funded through progressive taxation.',
    'If scarcity is chosen rather than binding, the entire efficiency justification for targeting collapses, and the constraint reclassifies from coordinated-necessity to pure extraction (snare persisting via false-summit capture). If scarcity is truly binding, the snare reading remains valid — but acknowledges the targeting logic as a forced choice, not a technical optimization.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(resource_scarcity_binding_vs_chosen, empirical, 'Whether the resource scarcity the targeting-efficiency reading invokes is an objective fiscal limit or a policy choice.').

omega_variable(
    means_testing_administrative_cost_vs_benefit,
    'What is the total cost (administrative + behavioral + dignity + documentation burden) of the means-testing apparatus compared to the benefit of concentrating support on the highest-need households?',
    'Administrative cost accounting across welfare agencies; behavioral economics studies of cliff effects and poverty traps; surveys and qualitative interviews with recipients on burden and autonomy loss; comparative cost analysis of universal vs. targeted provision.',
    'If administrative and behavioral costs exceed the efficiency gain from targeting, the constraint is revealed as producing negative net welfare — a clear extraction mechanism. If costs are lower than benefits, the targeting logic is partially vindicated, though dignity/autonomy extraction would still persist.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(means_testing_administrative_cost_vs_benefit, empirical, 'Whether means-testing''s costs exceed its efficiency benefits.').

omega_variable(
    behavioral_hazard_vs_autonomy_enhancement,
    'Does unconditional income support reduce work effort and skill accumulation (as behavioral-hazard logic predicts), or enhance autonomy and enable better labor-market matching (as freedom-floor logic predicts)?',
    'Long-term experimental and quasi-experimental studies of universal vs. conditional support (UBI pilots, negative income tax experiments, comparisons across welfare regimes); panel data on earnings, employment, and skill acquisition under different support architectures.',
    'If behavioral hazard is substantial, the targeting constraint''s work-incentive justification is stronger and the snare reading is less severe. If autonomy enhancement dominates, the behavioral-assumption foundation of the targeting reading is undercut and the extraction mechanism stands revealed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(behavioral_hazard_vs_autonomy_enhancement, empirical, 'Whether unconditional support creates behavioral hazard or autonomy enhancement.').

omega_variable(
    agenda_setter_capture_vs_neutral_administration,
    'Are program administrators and efficiency advocates genuinely neutral technical actors optimizing resource allocation, or are they captured beneficiaries perpetuating the targeting apparatus for institutional and intellectual rent?',
    'Analysis of administrator institutional interests and career incentives; examination of efficiency economists'' funding sources and ideological commitments; comparative study of how rapidly means-testing rules tighten when budgets contract vs. expand; investigation of administrator resistance to universal alternative proposals.',
    'If capture is substantial, the snare reading is confirmed: the constraint persists because the agenda-setters benefit from it, not because it solves an objective problem. If administrators are genuinely neutral, the targeting constraint reflects legitimate technical tradeoffs, though the distributional choice remains contested.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(agenda_setter_capture_vs_neutral_administration, empirical, 'Whether agenda-setters are captured beneficiaries or neutral technicians.').

omega_variable(
    kernel_reading_foreclosure_structure,
    'Do these three readings (targeting-efficiency, freedom-floor, dependency-trap) logically foreclose one another within any single institutional framework, or do they coexist as genuinely live alternatives held by different political constituencies?',
    'Textual and logical analysis of each reading''s core premises; examination of whether a single policymaker could coherently hold more than one reading; study of legislative history showing whether the readings have been debated as mutually exclusive or as positions on a continuous spectrum.',
    'If readings foreclose one another, the kernel is undergoing a transformation where only one reading will survive institutionalization. If they coexist, the constraint is genuinely contested and multiple readings remain available to different political actors. This distinction affects the stability and malleability of the targeting-efficiency constraint over time.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure_structure, conceptual, 'Whether the income-support-commitment readings logically foreclose one another or coexist as live alternatives.').

omega_variable(
    mandatrophy_vs_equilibrium_persistence,
    'Does the targeting-efficiency constraint persist because its founding problem (resource scarcity) remains live and binding, or because it has become institutionally entrenched despite the founding problem''s death?',
    'Historical analysis of policy shifts and founding-problem validation over the 40-year interval; examination of legislative and administrative responsiveness to evidence that scarcity is political rather than binding; comparison of targeting-constraint strength before and after major efficiency challenges (Scandinavian expansions, post-Keynesian fiscal theory adoption, etc.).',
    'If the founding problem is still live, the constraint is a legitimate response to genuine scarcity and the snare reading is qualified by necessity. If the founding problem is dead but the constraint persists, mandatrophy diagnosis is confirmed: the constraint is now pure institutional inertia and rent collection, and should reclassify toward piton (inertial persistence) rather than active snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandatrophy_vs_equilibrium_persistence, empirical, 'Whether the founding problem of resource scarcity remains live or has died.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(income_support_commitment__targeting_efficiency_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(inco_tr_t0, income_support_commitment__targeting_efficiency_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(inco_tr_t0, observed).
narrative_ontology:measurement(inco_tr_t5, income_support_commitment__targeting_efficiency_reading, theater_ratio, 5, 0.32).
narrative_ontology:measurement_basis(inco_tr_t5, observed).
narrative_ontology:measurement(inco_tr_t10, income_support_commitment__targeting_efficiency_reading, theater_ratio, 10, 0.35).
narrative_ontology:measurement_basis(inco_tr_t10, observed).
narrative_ontology:measurement(inco_tr_t15, income_support_commitment__targeting_efficiency_reading, theater_ratio, 15, 0.38).
narrative_ontology:measurement_basis(inco_tr_t15, observed).
narrative_ontology:measurement(inco_tr_t20, income_support_commitment__targeting_efficiency_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement_basis(inco_tr_t20, observed).
narrative_ontology:measurement(inco_tr_t25, income_support_commitment__targeting_efficiency_reading, theater_ratio, 25, 0.41).
narrative_ontology:measurement_basis(inco_tr_t25, observed).
narrative_ontology:measurement(inco_tr_t30, income_support_commitment__targeting_efficiency_reading, theater_ratio, 30, 0.42).
narrative_ontology:measurement_basis(inco_tr_t30, observed).
narrative_ontology:measurement(inco_tr_t35, income_support_commitment__targeting_efficiency_reading, theater_ratio, 35, 0.42).
narrative_ontology:measurement_basis(inco_tr_t35, observed).
narrative_ontology:measurement(inco_tr_t40, income_support_commitment__targeting_efficiency_reading, theater_ratio, 40, 0.42).
narrative_ontology:measurement_basis(inco_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(inco_be_t0, income_support_commitment__targeting_efficiency_reading, base_extractiveness, 0, 0.58).
narrative_ontology:measurement_basis(inco_be_t0, observed).
narrative_ontology:measurement(inco_be_t5, income_support_commitment__targeting_efficiency_reading, base_extractiveness, 5, 0.61).
narrative_ontology:measurement_basis(inco_be_t5, observed).
narrative_ontology:measurement(inco_be_t10, income_support_commitment__targeting_efficiency_reading, base_extractiveness, 10, 0.64).
narrative_ontology:measurement_basis(inco_be_t10, observed).
narrative_ontology:measurement(inco_be_t15, income_support_commitment__targeting_efficiency_reading, base_extractiveness, 15, 0.67).
narrative_ontology:measurement_basis(inco_be_t15, observed).
narrative_ontology:measurement(inco_be_t20, income_support_commitment__targeting_efficiency_reading, base_extractiveness, 20, 0.68).
narrative_ontology:measurement_basis(inco_be_t20, observed).
narrative_ontology:measurement(inco_be_t25, income_support_commitment__targeting_efficiency_reading, base_extractiveness, 25, 0.7).
narrative_ontology:measurement_basis(inco_be_t25, observed).
narrative_ontology:measurement(inco_be_t30, income_support_commitment__targeting_efficiency_reading, base_extractiveness, 30, 0.71).
narrative_ontology:measurement_basis(inco_be_t30, observed).
narrative_ontology:measurement(inco_be_t35, income_support_commitment__targeting_efficiency_reading, base_extractiveness, 35, 0.71).
narrative_ontology:measurement_basis(inco_be_t35, observed).
narrative_ontology:measurement(inco_be_t40, income_support_commitment__targeting_efficiency_reading, base_extractiveness, 40, 0.71).
narrative_ontology:measurement_basis(inco_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(inco_su_t0, income_support_commitment__targeting_efficiency_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(inco_su_t0, observed).
narrative_ontology:measurement(inco_su_t5, income_support_commitment__targeting_efficiency_reading, suppression_requirement, 5, 0.59).
narrative_ontology:measurement_basis(inco_su_t5, observed).
narrative_ontology:measurement(inco_su_t10, income_support_commitment__targeting_efficiency_reading, suppression_requirement, 10, 0.62).
narrative_ontology:measurement_basis(inco_su_t10, observed).
narrative_ontology:measurement(inco_su_t15, income_support_commitment__targeting_efficiency_reading, suppression_requirement, 15, 0.64).
narrative_ontology:measurement_basis(inco_su_t15, observed).
narrative_ontology:measurement(inco_su_t20, income_support_commitment__targeting_efficiency_reading, suppression_requirement, 20, 0.65).
narrative_ontology:measurement_basis(inco_su_t20, observed).
narrative_ontology:measurement(inco_su_t25, income_support_commitment__targeting_efficiency_reading, suppression_requirement, 25, 0.67).
narrative_ontology:measurement_basis(inco_su_t25, observed).
narrative_ontology:measurement(inco_su_t30, income_support_commitment__targeting_efficiency_reading, suppression_requirement, 30, 0.68).
narrative_ontology:measurement_basis(inco_su_t30, observed).
narrative_ontology:measurement(inco_su_t35, income_support_commitment__targeting_efficiency_reading, suppression_requirement, 35, 0.68).
narrative_ontology:measurement_basis(inco_su_t35, observed).
narrative_ontology:measurement(inco_su_t40, income_support_commitment__targeting_efficiency_reading, suppression_requirement, 40, 0.68).
narrative_ontology:measurement_basis(inco_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(income_support_commitment__targeting_efficiency_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(income_support_commitment__targeting_efficiency_reading, 0.12).
narrative_ontology:affects_constraint(income_support_commitment__targeting_efficiency_reading, income_support_commitment__freedom_floor_reading).
narrative_ontology:affects_constraint(income_support_commitment__targeting_efficiency_reading, income_support_commitment__dependency_trap_reading).

% DUAL FORMULATION NOTE:
% This constraint (targeting-efficiency reading) is one of three siblings decomposed from the contested kernel income_support_commitment. The three readings differ in their core premise about scarcity (empirically_contingent: is it binding?), their foundational normative claim about distribution justice (deontological freedom vs. instrumental efficiency), and their assessment of behavioral hazard. All three operate from the same kernel (the institutional commitment to income support) but generate structurally distinct constraints. Targeting-efficiency frames support as a scarce public good requiring means-testing for efficiency; freedom-floor frames it as a right requiring unconditional universality; dependency-trap frames it as a behavioral incentive mechanism requiring conditionality. Each constraint story carries its own ε, beneficiary/victim structure, and stakeholder surface. Links via network.affects_constraints indicate the family structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
