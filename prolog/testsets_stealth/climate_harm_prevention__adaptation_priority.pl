% ============================================================================
% CONSTRAINT STORY: climate_harm_prevention__adaptation_priority
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_harm_prevention__adaptation_priority, []).

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
 *   constraint_id: climate_harm_prevention__adaptation_priority
 *   human_readable: Adaptation-First Climate Response with Accepted Higher Warming Trajectory
 *   domain: climate policy/political economy/intergenerational ethics
 *
 * SUMMARY:
 *   This story instantiates the adaptation_priority reading of the
 *   climate_harm_prevention kernel: the claim that a legitimate climate
 *   response, given that economy-wide mitigation is politically and
 *   economically infeasible under existing arrangements, consists in
 *   prioritizing near-term resilience for presently exposed populations while
 *   accepting a higher warming trajectory as the planning basis. The
 *   arrangement under contest — and the referent of every metric here — is
 *   that standing adaptation-first architecture: the budget splits, fund
 *   pipelines, infrastructure standards, and feasibility discourse that
 *   channel climate expenditure toward present protection and treat deep
 *   emission cuts as off the table. Authored by this reading's own lights,
 *   the arrangement delivers real protection (genuine coordination) while
 *   conceding, as the reading itself does, that the accepted trajectory lands
 *   residual costs on parties with no seat — future generations and
 *   low-adaptation-capacity regions — and that maintaining the feasibility
 *   premise requires active political enforcement. The expected structural
 *   delta is confirmed: present vulnerable populations are the primary
 *   beneficiaries, future generations and low-capacity regions bear residual
 *   costs, and expenditure is front-loaded. Sibling readings
 *   (mitigation_priority, degrowth_reading) are separate constraints with
 *   their own epsilon values and victim sets; see
 *   network.dual_formulation_note and the kernel_reading_committer_structure
 *   omega.
 *
 * KEY AGENTS:
 *   - present_vulnerable_populations: Primary declared beneficiary (powerless/trapped) — receives front-loaded resilience investment
 *   - future_generations: Primary target (powerless/trapped) — inherits the accepted trajectory with no seat in any forum
 *   - low_adaptation_capacity_regions: Secondary target (organized/trapped) — receives partial finance, bears uncompensated residual damages
 *   - major_donor_governments: Agenda setter (institutional/mobile) — sets allocation splits and defines political feasibility
 *   - adaptation_finance_institutions: Agenda setter and collector (institutional/arbitrage) — administers pipelines, collects management fees
 *   - incumbent_fossil_asset_holders: Structural beneficiary (powerful/arbitrage) — deferred transition preserves asset values
 *   - adaptation_engineering_sector: Secondary beneficiary (organized/mobile) — revenue scales with resilience spending
 *   - donor_country_taxpayers: Dual-positioned funder (moderate/constrained) — pays for adaptation, partially offset at home
 *   - mitigation_advocacy_coalition: Excluded voice (organized/constrained) — ruled out by the feasibility premise
 *   - ipcc_assessment_community: Analytical observer (analytical/analytical) — quantifies limits and residual damages
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_harm_prevention__adaptation_priority, 0.42).
domain_priors:suppression_score(climate_harm_prevention__adaptation_priority, 0.5).
domain_priors:theater_ratio(climate_harm_prevention__adaptation_priority, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_harm_prevention__adaptation_priority, extractiveness, 0.42).
narrative_ontology:constraint_metric(climate_harm_prevention__adaptation_priority, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(climate_harm_prevention__adaptation_priority, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_harm_prevention__adaptation_priority, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(climate_harm_prevention__adaptation_priority, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_harm_prevention__adaptation_priority, tangled_rope).
narrative_ontology:human_readable(climate_harm_prevention__adaptation_priority, "Adaptation-First Climate Response with Accepted Higher Warming Trajectory").
narrative_ontology:topic_domain(climate_harm_prevention__adaptation_priority, "climate policy/political economy/intergenerational ethics").

domain_priors:requires_active_enforcement(climate_harm_prevention__adaptation_priority).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_harm_prevention__adaptation_priority, 'aadd5099-2cdf-4d77-8127-3a50ea250e50').
narrative_ontology:cs_kernel_codification('aadd5099-2cdf-4d77-8127-3a50ea250e50', distributed).
narrative_ontology:cs_authority_grounding('aadd5099-2cdf-4d77-8127-3a50ea250e50', distributed).
narrative_ontology:cs_reading_relation('aadd5099-2cdf-4d77-8127-3a50ea250e50', climate_harm_prevention__mitigation_priority, forecloses).
narrative_ontology:cs_reading_relation('aadd5099-2cdf-4d77-8127-3a50ea250e50', climate_harm_prevention__degrowth_reading, coexists_with).
narrative_ontology:cs_axiom('aadd5099-2cdf-4d77-8127-3a50ea250e50', foundational, growth_framework_mitigation_infeasible).
narrative_ontology:cs_axiom_status(growth_framework_mitigation_infeasible, holdable).
narrative_ontology:cs_axiom_grounding('aadd5099-2cdf-4d77-8127-3a50ea250e50', growth_framework_mitigation_infeasible, empirically_contingent).
narrative_ontology:cs_axiom('aadd5099-2cdf-4d77-8127-3a50ea250e50', foundational, present_vulnerable_protection_first).
narrative_ontology:cs_axiom_status(present_vulnerable_protection_first, holdable).
narrative_ontology:cs_axiom_grounding('aadd5099-2cdf-4d77-8127-3a50ea250e50', present_vulnerable_protection_first, deontological).
narrative_ontology:cs_axiom('aadd5099-2cdf-4d77-8127-3a50ea250e50', secondary, higher_trajectory_acceptance_as_planning_basis).
narrative_ontology:cs_axiom_status(higher_trajectory_acceptance_as_planning_basis, holdable).
narrative_ontology:cs_axiom_grounding('aadd5099-2cdf-4d77-8127-3a50ea250e50', higher_trajectory_acceptance_as_planning_basis, instrumental).
narrative_ontology:cs_reference_frame('aadd5099-2cdf-4d77-8127-3a50ea250e50', feasibility_bounded_present_protection).
narrative_ontology:cs_drift_state('aadd5099-2cdf-4d77-8127-3a50ea250e50', post_renewable_cost_collapse, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('aadd5099-2cdf-4d77-8127-3a50ea250e50', '').
narrative_ontology:cs_kernel_id(climate_harm_prevention__adaptation_priority, climate_harm_prevention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_harm_prevention__adaptation_priority, present_vulnerable_populations).
narrative_ontology:constraint_beneficiary(climate_harm_prevention__adaptation_priority, adaptation_finance_institutions).
narrative_ontology:constraint_beneficiary(climate_harm_prevention__adaptation_priority, adaptation_engineering_sector).
narrative_ontology:constraint_beneficiary(climate_harm_prevention__adaptation_priority, incumbent_fossil_asset_holders).
narrative_ontology:constraint_victim(climate_harm_prevention__adaptation_priority, future_generations).
narrative_ontology:constraint_victim(climate_harm_prevention__adaptation_priority, low_adaptation_capacity_regions).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(climate_harm_prevention__adaptation_priority, donor_country_taxpayers).
narrative_ontology:constraint_victim(climate_harm_prevention__adaptation_priority, donor_country_taxpayers).
narrative_ontology:constraint_vindicates(climate_harm_prevention__adaptation_priority, political_feasibility_doctrine).
narrative_ontology:constraint_vindicates(climate_harm_prevention__adaptation_priority, positive_discount_rate_practice).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Live in flood plains, drought belts, and heat-exposed cities where impacts are already arriving. Receive seawalls, early-warning systems, heat-action plans, and climate-proofed housing financed through adaptation budgets. Cannot move away from exposure at meaningful scale; their protection depends on projects funded under the current allocation.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__adaptation_priority, present_vulnerable_populations, beneficiary,
    powerless, immediate, trapped, regional).

% Will inherit the warming trajectory accepted today: higher seas, shifted weather patterns, stressed ecosystems, and infrastructure designed to a superseded baseline. Have no seat in any negotiating forum; appear only through proxy commissioners, constitutional litigation brought on their behalf, and discount rates chosen by others. Cannot decline the inheritance.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__adaptation_priority, future_generations, payer,
    powerless, generational, trapped, global).

% Small island states, least-developed countries, and deltaic nations with thin fiscal capacity. Receive a share of adaptation finance but face damages that outpace what local projects can offset as warming climbs; coastlines and agricultural zones cannot be relocated. Hold formal votes in climate forums with little agenda-setting leverage.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__adaptation_priority, low_adaptation_capacity_regions, payer,
    organized, biographical, trapped, continental).

% Set national budget splits between resilience programs at home and abroad, decide what counts as politically deliverable, and shape the multilateral funds' project pipelines. Can redirect allocations, attach conditions, or disengage from commitments; they administer the arrangement rather than answer to it.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__adaptation_priority, major_donor_governments, agenda_setter,
    institutional, generational, mobile, global).

% Multilateral funds and development banks that screen, approve, and disburse resilience projects. Collect management fees and build institutional mandates around the adaptation portfolio; their program continuity depends on the current allocation priorities remaining in place.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__adaptation_priority, adaptation_finance_institutions, agenda_setter,
    institutional, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(climate_harm_prevention__adaptation_priority, adaptation_finance_institutions, beneficiary).

% Owners of reserves, pipelines, refineries, and combustion supply chains. Every year in which economy-wide emission cuts are treated as off the table extends the productive life of these assets and delays stranding. Not named in the arrangement's public rationale, yet the accepted trajectory is what preserves their balance sheets.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__adaptation_priority, incumbent_fossil_asset_holders, beneficiary,
    powerful, biographical, arbitrage, global).

% Consultancies, construction firms, insurers, and water-management companies whose order books scale with resilience spending. Compete for funded projects; revenue follows the allocation, wherever it goes.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__adaptation_priority, adaptation_engineering_sector, beneficiary,
    organized, biographical, mobile, continental).

% Fund adaptation budgets through general taxation while also facing climate damages at home that the same budgets partially offset. Can vote on the governments setting allocations and can, at greater cost, emigrate; their exposure is mediated rather than direct.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__adaptation_priority, donor_country_taxpayers, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(climate_harm_prevention__adaptation_priority, donor_country_taxpayers, payer).

% Climate scientists, NGOs, youth movements, and litigants who argue that rapid emission cuts remain technically and economically available. Operate outside the allocation process; their proposals are ruled out by the feasibility premise that structures the conversation, leaving protest, courts, and internal advocacy as remaining channels.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__adaptation_priority, mitigation_advocacy_coalition, excluded,
    organized, biographical, constrained, global).

% Assesses adaptation limits, residual damages, and the gap between pledged and delivered resilience across scenarios. Produces the shared evidence base every other seat argues with; holds no allocation power.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__adaptation_priority, ipcc_assessment_community, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_harm_prevention__adaptation_priority, incumbent_fossil_asset_holders).
narrative_ontology:fixing_cost_class(climate_harm_prevention__adaptation_priority, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Pools resources that no exposed community can provision alone — seawalls, early-warning networks, climate-proofed infrastructure, agricultural adjustment — and provides a shared accepted-trajectory planning basis so engineers, insurers, and planners design to a common assumption instead of gambling on disputed futures.
% TRANSFER_FUNCTION: Moves adaptation finance from donor-country budgets and multilateral funds toward resilience projects in exposed regions; moves the costs of deferred emission cuts onto future generations and low-adaptation-capacity regions as residual damages; and preserves incumbent fossil asset values by extending the window before stranding.
% ABSENT_VOICES: Future generations are absent without remedy — no seat, no vote, represented only by proxy commissioners and litigation guardians. Low-adaptation-capacity regions hold formal seats but little agenda power; their loss-and-damage demands are acknowledged and underfunded. Mitigation advocates are excluded by the feasibility premise itself: the conversation's terms define their position as out of bounds before it is argued.
% DISAPPEARANCE_RATIONALE: If the adaptation-first arrangement vanished overnight, multilateral fund pipelines, national budget splits, infrastructure design standards, and the consultancy sector built around resilience spending would all reorganize; incumbent asset holders would face immediate repricing expectations; and the feasibility discourse that structures climate politics would lose its institutional carrier. Every seated party's position depends on the arrangement persisting.
% FOUNDING_PROBLEM: By the 2010s, locked-in warming and stalled mitigation had produced present-day exposure that emission cuts could no longer prevent in time; the founding problem was how to protect already-exposed populations given fiscal limits and a political system unwilling to pursue economy-wide mitigation.
% FOUNDING_PROBLEM_CORROBORATION: IPCC Working Group II assessments — produced outside the benefiting parties — attest that adaptation needs exceed current action, that soft adaptation limits are being approached, and that present-day losses are attributable to warming; disaster-loss databases and insurance-sector assessments corroborate from independent seats. No corroborating source attests that the founding problem is dead.
narrative_ontology:disappearance_verdict(climate_harm_prevention__adaptation_priority, world_rearranges).
narrative_ontology:founding_problem_status(climate_harm_prevention__adaptation_priority, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_harm_prevention__adaptation_priority, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth+rescue1', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(climate_harm_prevention__adaptation_priority, 'none', 1).
narrative_ontology:epsilon_provenance(climate_harm_prevention__adaptation_priority, 0.42, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_harm_prevention__adaptation_priority_tests).
:- end_tests(climate_harm_prevention__adaptation_priority_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Epsilon is authored at 0.42 by this reading's own lights over the standing adaptation-first arrangement: substantial real protection is delivered where exposure is addressable, but the reading itself concedes that the accepted trajectory assigns residual costs to unrepresented parties and that deferral preserves incumbent asset values — the arrangement is not cost-free coordination even from its endorsing seat. Suppression (0.50) records agenda-level option closure rather than physical coercion: the feasibility premise removes mitigation from the decision set, and funding conditionality steers recipients toward resilience projects. Per the framework's separation rule, suppression is authored as a raw structural property and is not scaled by power or scope — only extractiveness is scaled, by directionality and spatial scope in the engine's computation. Theater ratio (0.32) reflects real seawalls, early-warning systems, and climate-proofed infrastructure alongside pledge-versus-disbursement gaps and resilience rhetoric wrapped around business-as-usual. Accessibility collapse (0.58): inside the policy framework the feasibility premise collapses alternatives nearly completely, but mitigation and contraction programs remain live outside it, so collapse is partial rather than mountain-grade. Resistance (0.52): island-state objections, loss-and-damage demands, youth litigation, and scientific advocacy persistently contest the accepted trajectory without displacing it. All three tracked metrics run on one shared seven-point grid (T=0..30, mapped to roughly 1995-2025); base_extractiveness and suppression_requirement rise monotonically — extraction accumulates as locked-in warming compounds, and the enforcement requirement grows as cheap renewables make the infeasibility premise progressively harder to sustain by inertia alone. No cyclical dynamics are modeled: the record shows a ratchet, not oscillation.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the agenda-setter seats compute differently from the same structure. From the donor-government and finance-institution seats the arrangement is pragmatic realism: protection delivered within binding fiscal and electoral limits, with residual harm regretted but unavoidable. From the future-generations seat — reachable only through proxy and litigation — the same structure is the mechanism that converts their exposure into present-day budgetary convenience. Low-adaptation-capacity regions experience both faces at once: recipients of project finance and net bearers of damages the finance cannot offset. Incumbent asset holders experience the arrangement as quiet preservation. The engine computes these per-seat classifications from the structural data; the authored claim does not adjudicate between them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality: present_vulnerable_populations (trapped, powerless) are subsidized heavily by the arrangement's protective flows; incumbent_fossil_asset_holders (arbitrage exit) sit nearest the full-beneficiary end since the accepted trajectory is precisely what preserves their assets; adaptation_finance_institutions and adaptation_engineering_sector collect fees and contracts tied to the allocation. Victim declarations drive high directionality: future_generations (trapped, no exit conceivable) and low_adaptation_capacity_regions (geographically immobile, partially compensated at best) sit near the full-target end. Donor governments and finance institutions, as agenda setters with mobile or arbitrage exit, derive low-to-moderate directionality — they administer the arrangement they fund. Donor_country_taxpayers sit near symmetric: they fund the budgets and receive partial domestic offset. Larger spatial scopes (global fund architecture, planetary trajectory) modestly amplify effective extraction for target seats through verification difficulty; the engine owns that arithmetic. No directionality overrides are authored: the derivation chain from beneficiary/victim declarations plus power and exit produces the correct relationships for every seat, and any override keyed on a power atom would collide across same-power seats with opposed positions (e.g., the two powerless seats sit at opposite ends).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — protecting populations already exposed to impacts that mitigation can no longer prevent in time — remains live: IPCC assessments document widening adaptation gaps and rising present-day attribution-confirmed losses, corroborated from outside the benefiting parties. Mandatrophy is therefore NOT resolved, and the classification guards against two mislabels. Reading the arrangement as pure extraction ignores the large volume of delivered protection for populations that no alternative on offer would shield sooner; reading it as pure coordination ignores that the same structure maintains option-closure on mitigation and assigns residual costs to parties excluded from every table. The theater ratio remains a minority share (0.32), so the arrangement is not yet running on performance alone — the degraded-inertial signature does not fit while real function dominates. The live risk this story tracks temporally is drift: if hard adaptation limits arrive before any mitigation turn, the protective function decays while the enforcement apparatus persists, which is the pathway by which this hybrid degrades toward pure extraction or inertial performance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mitigation_feasibility_premise,
    'Is economy-wide mitigation genuinely infeasible under existing political-economic arrangements, or is ''infeasibility'' an artifact of incumbent power and short electoral horizons that a sufficiently determined coalition could overcome?',
    'Comparative policy analysis of jurisdictions that enacted deep mitigation packages after 2015 (renewable cost curves, carbon pricing coverage, industrial policy), tested against the claim''s own political-feasibility standard.',
    'If feasible, the arrangement''s enabling premise fails and the accepted trajectory converts from tragic necessity into avoided cost — residual burdens become imposed rather than inevitable, pushing the structure toward pure extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mitigation_feasibility_premise, empirical, 'Whether the feasibility premise is descriptive or self-serving.').

omega_variable(
    kernel_reading_committer_structure,
    'This story instantiates the adaptation_priority reading of the climate_harm_prevention kernel; how would the mitigation_priority and degrowth_reading siblings restructure the beneficiary and victim sets for the same expenditure streams?',
    'Compile the sibling stories and compare computed seat classifications and epsilon across readings; the disagreement is located in the scope of the feasibility claim and the normative response to it.',
    'Under mitigation_priority the same deferral reads as transition-delay imposition (epsilon rises sharply); under degrowth_reading it reads as growth-accommodation with contraction refused (different victim weighting); only this reading authors the arrangement as tragic-necessity coordination.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Committer structure: one reading among three of the climate_harm_prevention kernel.').

omega_variable(
    social_discount_rate_ambiguity,
    'What ethical weight do the accepted-trajectory costs carried by future generations receive, and is the operative discount rate a technical parameter or a value choice made without the affected parties?',
    'Deliberative processes granting future-generation proxies formal standing (ombudsperson institutions, litigation guardianship, mandated low-discount sensitivity reporting in project appraisal).',
    'A near-zero effective discount rate raises measured extraction toward the target end and strengthens the case that the arrangement imposes rather than merely accepts residual harm; a high rate sustains the tragic-necessity framing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(social_discount_rate_ambiguity, preference, 'Discount-rate choice as hidden value parameter determining future-generation cost weight.').

omega_variable(
    adaptation_hard_limits_threshold,
    'At what warming level do adaptation measures hit hard biophysical limits beyond which resilience spending protects nothing?',
    'Successive IPCC assessment cycles quantifying soft versus hard adaptation limits by region and sector, combined with observed project failure rates at rising temperature anomalies.',
    'Beyond hard limits the coordination function decays — resilience expenditure stops protecting anyone and the accepted trajectory converts from managed risk into open-ended imposition, collapsing the arrangement''s justification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(adaptation_hard_limits_threshold, empirical, 'Biophysical ceiling on the arrangement''s protective function.').

omega_variable(
    loss_and_damage_transfer_adequacy,
    'Does loss-and-damage financing reach levels that compensate low-adaptation-capacity regions for residual harms, or does it remain symbolic relative to independently assessed damages?',
    'Track fund capitalization and disbursement against independently assessed residual damage estimates for the same regions and periods.',
    'Adequate transfers move those regions toward the compensated, symmetric side of the ledger; symbolic transfers leave them bearing the accepted trajectory''s costs without offset, sharpening the asymmetry.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(loss_and_damage_transfer_adequacy, empirical, 'Whether residual-harm compensation is material or ceremonial.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_harm_prevention__adaptation_priority, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_harm_prevention__adaptation_priority, theater_ratio, 0, 0.12).
narrative_ontology:measurement(clim_tr_t5, climate_harm_prevention__adaptation_priority, theater_ratio, 5, 0.16).
narrative_ontology:measurement(clim_tr_t10, climate_harm_prevention__adaptation_priority, theater_ratio, 10, 0.2).
narrative_ontology:measurement(clim_tr_t15, climate_harm_prevention__adaptation_priority, theater_ratio, 15, 0.24).
narrative_ontology:measurement(clim_tr_t20, climate_harm_prevention__adaptation_priority, theater_ratio, 20, 0.27).
narrative_ontology:measurement(clim_tr_t25, climate_harm_prevention__adaptation_priority, theater_ratio, 25, 0.3).
narrative_ontology:measurement(clim_tr_t30, climate_harm_prevention__adaptation_priority, theater_ratio, 30, 0.32).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_harm_prevention__adaptation_priority, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(clim_be_t5, climate_harm_prevention__adaptation_priority, base_extractiveness, 5, 0.24).
narrative_ontology:measurement(clim_be_t10, climate_harm_prevention__adaptation_priority, base_extractiveness, 10, 0.28).
narrative_ontology:measurement(clim_be_t15, climate_harm_prevention__adaptation_priority, base_extractiveness, 15, 0.33).
narrative_ontology:measurement(clim_be_t20, climate_harm_prevention__adaptation_priority, base_extractiveness, 20, 0.37).
narrative_ontology:measurement(clim_be_t25, climate_harm_prevention__adaptation_priority, base_extractiveness, 25, 0.4).
narrative_ontology:measurement(clim_be_t30, climate_harm_prevention__adaptation_priority, base_extractiveness, 30, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_harm_prevention__adaptation_priority, suppression_requirement, 0, 0.28).
narrative_ontology:measurement(clim_su_t5, climate_harm_prevention__adaptation_priority, suppression_requirement, 5, 0.33).
narrative_ontology:measurement(clim_su_t10, climate_harm_prevention__adaptation_priority, suppression_requirement, 10, 0.37).
narrative_ontology:measurement(clim_su_t15, climate_harm_prevention__adaptation_priority, suppression_requirement, 15, 0.41).
narrative_ontology:measurement(clim_su_t20, climate_harm_prevention__adaptation_priority, suppression_requirement, 20, 0.45).
narrative_ontology:measurement(clim_su_t25, climate_harm_prevention__adaptation_priority, suppression_requirement, 25, 0.48).
narrative_ontology:measurement(clim_su_t30, climate_harm_prevention__adaptation_priority, suppression_requirement, 30, 0.5).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_harm_prevention__adaptation_priority, resource_allocation).
narrative_ontology:affects_constraint(climate_harm_prevention__adaptation_priority, climate_harm_prevention__mitigation_priority).
narrative_ontology:affects_constraint(climate_harm_prevention__adaptation_priority, climate_harm_prevention__degrowth_reading).

% DUAL FORMULATION NOTE:
% 'Legitimate climate response' is a colloquial label covering three structurally distinct claims; per the epsilon-invariance principle it decomposes into a three-story constraint family. This story (adaptation_priority) authors epsilon over the adaptation-first arrangement by its own lights. climate_harm_prevention__mitigation_priority is the upstream sibling whose feasibility affirmation this reading's premise directly negates; climate_harm_prevention__degrowth_reading shares this reading's infeasibility diagnosis and diverges on prescription. Each member links the others via affects_constraints; cross-reading comparison of victim sets and epsilon is the corpus-level measurement the family exists to enable.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
