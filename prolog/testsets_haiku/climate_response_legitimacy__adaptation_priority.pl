% ============================================================================
% CONSTRAINT STORY: climate_response_legitimacy__adaptation_priority
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_response_legitimacy__adaptation_priority, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: climate_response_legitimacy__adaptation_priority
 *   human_readable: Climate Response via Adaptation Priority Reading
 *   domain: climate_policy/political_economy/intergenerational
 *
 * SUMMARY:
 *   The adaptation-priority reading of legitimate climate response accepts
 *   higher warming trajectories (2–3°C above pre-industrial) as inevitable
 *   and frames the legitimate policy response as protecting vulnerable
 *   populations through resilience infrastructure and adaptive capacity
 *   rather than pursuing deep emissions reduction. Wealthy nations set the
 *   policy frame, carbon-intensive industries benefit by preserving business
 *   models, and low-income regions enter the victim set immediately via the
 *   $350B annual adaptation finance gap. The constraint is a tangled rope: it
 *   coordinates a wealthy-nation consensus on 'realistic' climate response
 *   (genuine coordination function for wealthy-nation governments seeking
 *   climate action without economic disruption) while extracting
 *   intergenerational and geographic costs. The measurement series shows
 *   rising extractiveness and theater_ratio over time: early phase (t=0–10)
 *   shows extractiveness rising as adaptation-finance mechanisms are
 *   formalized but gaps widen; plateau phase (t=20–40) shows theater_ratio
 *   rising as performative adaptation commitments increase while core
 *   mitigation stalls — indicating Goodhart drift where the adaptation frame
 *   substitutes proxy goals (adaptation finance, resilience projects) for the
 *   actual problem (emissions reduction and warming prevention).
 *
 * KEY AGENTS:
 *   - Wealthy-nation governments: agenda-setters, primary beneficiaries; set the frame for what counts as legitimate response; preserve growth and carbon-intensive sectors.
 *   - Carbon-intensive industries: beneficiaries; operate under a policy regime that defers deep emissions reductions; profit from resilience and adaptation markets.
 *   - Low-income regions & small island states: primary victims; face impacts of warming they did not cause, constrained by $350B annual adaptation finance gap; exit is trapped (migration or state collapse).
 *   - Subsistence communities: identity-locked victims; face coercive adaptation interventions; cultural extinction via technocratic transition programs.
 *   - Future generations: structural victims; intergenerational costs defer mitigation and compound warming liability.
 *   - Mitigation advocates & degrowth advocates: excluded voices; present in forums but their core claims are framed as economically unrealistic; their testimony is recorded but does not reshape the policy frame.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_legitimacy__adaptation_priority, 0.68).
domain_priors:suppression_score(climate_response_legitimacy__adaptation_priority, 0.72).
domain_priors:theater_ratio(climate_response_legitimacy__adaptation_priority, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_legitimacy__adaptation_priority, extractiveness, 0.68).
narrative_ontology:constraint_metric(climate_response_legitimacy__adaptation_priority, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(climate_response_legitimacy__adaptation_priority, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_response_legitimacy__adaptation_priority, accessibility_collapse, 0.51).
narrative_ontology:constraint_metric(climate_response_legitimacy__adaptation_priority, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_legitimacy__adaptation_priority, tangled_rope).
narrative_ontology:human_readable(climate_response_legitimacy__adaptation_priority, "Climate Response via Adaptation Priority Reading").
narrative_ontology:topic_domain(climate_response_legitimacy__adaptation_priority, "climate_policy/political_economy/intergenerational").

domain_priors:requires_active_enforcement(climate_response_legitimacy__adaptation_priority).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_legitimacy__adaptation_priority, '2377ce58-04e1-4776-8d3a-7439d40192a5').
narrative_ontology:cs_kernel_codification('2377ce58-04e1-4776-8d3a-7439d40192a5', distributed).
narrative_ontology:cs_authority_grounding('2377ce58-04e1-4776-8d3a-7439d40192a5', extraction).
narrative_ontology:cs_interpretation_layer_present('2377ce58-04e1-4776-8d3a-7439d40192a5').
narrative_ontology:cs_reading_relation('2377ce58-04e1-4776-8d3a-7439d40192a5', climate_response_legitimacy__mitigation_priority, coexists_with).
narrative_ontology:cs_reading_relation('2377ce58-04e1-4776-8d3a-7439d40192a5', climate_response_legitimacy__degrowth_transformation, influences).
narrative_ontology:cs_axiom('2377ce58-04e1-4776-8d3a-7439d40192a5', foundational, legitimate_response_accepts_warming_inevitability).
narrative_ontology:cs_axiom_status(legitimate_response_accepts_warming_inevitability, holdable).
narrative_ontology:cs_axiom_grounding('2377ce58-04e1-4776-8d3a-7439d40192a5', legitimate_response_accepts_warming_inevitability, empirically_contingent).
narrative_ontology:cs_axiom('2377ce58-04e1-4776-8d3a-7439d40192a5', secondary, vulnerable_population_protection_subsumes_emissions_reduction).
narrative_ontology:cs_axiom_status(vulnerable_population_protection_subsumes_emissions_reduction, holdable).
narrative_ontology:cs_axiom_grounding('2377ce58-04e1-4776-8d3a-7439d40192a5', vulnerable_population_protection_subsumes_emissions_reduction, deontological).
narrative_ontology:cs_reference_frame('2377ce58-04e1-4776-8d3a-7439d40192a5', pragmatic_climate_action_within_growth_preservation).
narrative_ontology:cs_drift_state('2377ce58-04e1-4776-8d3a-7439d40192a5', post_ipc_ar6_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('2377ce58-04e1-4776-8d3a-7439d40192a5', '').
narrative_ontology:cs_kernel_id(climate_response_legitimacy__adaptation_priority, climate_response_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_legitimacy__adaptation_priority, wealthy_nations_development_model).
narrative_ontology:constraint_beneficiary(climate_response_legitimacy__adaptation_priority, carbon_intensive_industries).
narrative_ontology:constraint_beneficiary(climate_response_legitimacy__adaptation_priority, high_gdp_regions).
narrative_ontology:constraint_victim(climate_response_legitimacy__adaptation_priority, low_income_regions).
narrative_ontology:constraint_victim(climate_response_legitimacy__adaptation_priority, small_island_developing_states).
narrative_ontology:constraint_victim(climate_response_legitimacy__adaptation_priority, subsistence_communities).
narrative_ontology:constraint_victim(climate_response_legitimacy__adaptation_priority, future_generations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(climate_response_legitimacy__adaptation_priority, wealthy_nations_governments).
narrative_ontology:constraint_beneficiary(climate_response_legitimacy__adaptation_priority, wealthy_nation_constituencies).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set climate policy framework accepting higher warming trajectories (2–3°C above pre-industrial baseline) while committing to adaptive capacity building in vulnerable regions. Frame adaptation as the legitimate response, framing mitigation-first approaches as economically disruptive and technologically uncertain. Preserve domestic development models and carbon-intensive sectors; direct adaptation finance toward developing regions while maintaining growth trajectories. Control the narrative of what constitutes legitimate climate response.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__adaptation_priority, wealthy_nations_governments, agenda_setter,
    institutional, generational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(climate_response_legitimacy__adaptation_priority, wealthy_nations_governments, beneficiary).

% Operate under a policy regime that defers deep emissions reductions in favor of adaptation spending. Adaptation finance requirements create markets for resilience infrastructure, insurance products, and climate-resilient agriculture in developing regions — profitable sectors where technological and institutional barriers to entry remain high. Their extraction capacity is protected by the focus on warming-as-fait-accompli and adaptation-as-primary-response, which leaves their business models intact and opens new revenue streams.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__adaptation_priority, carbon_intensive_industries, beneficiary,
    powerful, biographical, constrained, global).

% Face the impacts of warming they did not cause (cumulative historical responsibility lies with wealthy nations) while bearing the costs of adaptation infrastructure they cannot finance independently. The $350B annual adaptation finance gap reflects the structural fact: wealthy-nation policy accepts warming trajectories that impose accelerating climate risks on low-income regions, then frames adaptation as the regions' responsibility. Exit looks like migration or state collapse; no alternatives exist within the framework.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__adaptation_priority, low_income_regions, payer,
    powerless, generational, trapped, local).
narrative_ontology:stakeholder_secondary_role(climate_response_legitimacy__adaptation_priority, low_income_regions, excluded).

% Face existential threats from sea-level rise driven by warming trajectories wealthy nations accept as inevitable. No adaptation infrastructure can solve territorial disappearance; their adaptation needs are not separable from their survival. The constraint forces them to invest in impossible adaptations while their core claim — that warming itself must be prevented — is dismissed as economically unrealistic.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__adaptation_priority, small_island_developing_states, payer,
    powerless, generational, trapped, local).

% Depend on climate-sensitive ecosystems (pastoralism, fishing, rain-fed agriculture) for livelihood and cultural identity. Adaptation in the wealthy-nation reading means technocratic interventions: dam construction, crop switching, livelihood transition. These interventions carry coercive logic: accept adaptation programs designed elsewhere or lose access to climate finance, international development support, and institutional legitimacy. Their identity and mode of life are framed as obstacles to adaptation rather than values to preserve.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__adaptation_priority, subsistence_communities, payer,
    powerless, biographical, identity_locked, local).

% Have no voice in present policy. Accepting higher warming trajectories defers mitigation costs to a time when warming is locked in and adaptation costs are orders of magnitude higher. Intergenerational extraction: present wealthy nations consume carbon budget; future generations inherit degraded climate. The adaptation-priority reading converts this temporal asymmetry into policy (delay mitigation; invest in adaptation) and calls it legitimate.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__adaptation_priority, future_generations, payer,
    powerless, civilizational, trapped, global).

% Argue that prioritizing adaptation over mitigation accepts warming as inevitable and locks in catastrophic outcomes; that deep emissions cuts are technically and economically feasible; that adaptation alone cannot address tipping-point thresholds. They are present in negotiating forums but their core claim — that mitigation must be primary — is framed as economically unrealistic within the adaptation-priority reading's authority structure.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__adaptation_priority, mitigation_advocates, excluded,
    organized, generational, constrained, global).

% Argue that adaptation-priority and mitigation-priority readings both preserve the growth imperative in wealthy nations; that genuine climate response requires dismantling carbon-intensive consumption and work patterns; that adaptation in a warming world is impossible at scale without economic transformation. They are structurally excluded from the dominant policy frame, which treats growth as a non-negotiable baseline.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__adaptation_priority, degrowth_advocates, excluded,
    organized, generational, constrained, global).

% Produces evidence on warming trajectories, tipping points, and adaptation limits. The adaptation-priority reading appropriates scientific consensus on warming inevitability while downplaying scientific consensus on tipping-point risks and adaptation adequacy gaps. Scientists testify but do not control how their evidence is framed; the policy reading interprets the evidence, not the reverse.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__adaptation_priority, climate_science_authority, observer,
    institutional, generational, analytical, global).

% Benefit from the constraint by preserving consumption patterns, employment in carbon-intensive industries, and asset values in high-carbon infrastructure. Adaptation-as-primary-response means costs are externalized to distant populations and deferred to future time periods; present constituencies bear only the adaptation finance commitment, which is politically diffuse and financially manageable. Exit would mean accepting mitigation costs now.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__adaptation_priority, wealthy_nation_constituencies, beneficiary,
    organized, biographical, mobile, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_response_legitimacy__adaptation_priority, wealthy_nations_governments).
narrative_ontology:fixing_cost_class(climate_response_legitimacy__adaptation_priority, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a shared framework for legitimate climate response grounded in accepting warming as inevitable rather than preventable, allowing wealthy nations to coordinate on what counts as adequate action without dismantling growth or accepting deep mitigation costs. Solves the wealthy-nation coordination problem: how to appear responsive to climate risk while preserving economic models.
% TRANSFER_FUNCTION: Transfers intergenerational climate risk from present to future; transfers adaptation costs from wealthy to low-income regions via the adequacy gap; transfers mitigation responsibility from high-emitting to low-emitting nations; transfers carbon budget consumption rights to wealthy-nation industries; transfers the framing authority (what constitutes legitimate response) from low-income nations and future generations to wealthy-nation policymakers.
% ABSENT_VOICES: Low-income nations have representation in negotiating forums but their core claim — that mitigation must be primary because adaptation cannot address tipping points or territorial disappearance — is structurally inaudible within the adaptation-priority frame. Mitigation advocates are present but frame their position as economically naive. Degrowth advocates are excluded from the dominant policy conversation. Future generations have no voice at all. Small island states testify that adaptation is existentially insufficient but their testimony is recorded as a special case rather than a revision of the framework itself.
% DISAPPEARANCE_RATIONALE: If this constraint (accepting warming as legitimate response trajectory) disappeared, wealthy nations would face pressure to commit to deep emissions reductions; carbon-intensive industries would face regulatory overhaul; intergenerational claims would enter present policy as binding rather than rhetorical; low-income regions would shift from adaptation-deficit finance recipients to mitigation-equity claimants. The global climate-response economy, policy narratives, and power distribution would reorganize around different assumptions about what counts as legitimate.
% FOUNDING_PROBLEM: Early climate negotiations faced a coordination challenge: wealthy nations wanted to act on climate risk without dismantling their economic models or committing to the depth of mitigation the science indicated was necessary. Adaptation-priority framing offered a solution: accept warming as inevitable (removing the need to eliminate emissions), commit to helping vulnerable populations adapt (addressing humanitarian concerns), preserve growth and industrial structure (meeting wealthy-nation interests).
% FOUNDING_PROBLEM_CORROBORATION: Wealthy-nation governments and fossil-fuel-aligned economists attest the founding problem is still live: mitigation at the depth the science requires is economically disruptive and technologically uncertain; adaptation is a pragmatic, immediately actionable response. Climate scientists, low-income nations, and small island delegations attest the founding problem has been resolved by evidence (renewable energy cost curves, IPCC scenarios): the problem was coordination to avoid deep action, not technical feasibility. The constraint persists as a power-consolidation mechanism, not as a solution to the founding problem.
narrative_ontology:disappearance_verdict(climate_response_legitimacy__adaptation_priority, world_rearranges).
narrative_ontology:founding_problem_status(climate_response_legitimacy__adaptation_priority, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_response_legitimacy__adaptation_priority, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(climate_response_legitimacy__adaptation_priority, 'none', 1).
narrative_ontology:epsilon_provenance(climate_response_legitimacy__adaptation_priority, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_response_legitimacy__adaptation_priority_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_response_legitimacy__adaptation_priority, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_response_legitimacy__adaptation_priority_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness starts at 0.48 and rises to 0.68 (t=30) before plateauing. This trajectory reflects two phases: (1) formalization phase (t=0–15), where adaptation mechanisms are built, adaptation finance commitments are made, and low-income regions integrate into the adaptation-as-primary-response frame — extraction rises as the gap between committed finance and actual need widens and becomes structurally recognized. (2) Inertial phase (t=20–40), where the frame is locked in, theaters of adaptation activity proliferate, but deep extraction remains constant — new adaptation projects are funded but emissions continue rising, warming accelerates, and adaptation-adequacy gaps widen further. Suppression rises in tandem (0.54 to 0.74): early suppression comes from framing choice (adaptation as primary, mitigation as secondary); later suppression comes from institutional lock-in (adaptation mechanisms become bureaucratic, path-dependent; exiting requires explicit rejection of the frame, which triggers diplomatic and financial consequences). Theater_ratio rises gradually (0.22 to 0.42): the ratio of performative to functional activity increases as adaptation projects proliferate while their adequacy decreases — the constraint evolves toward Piton characteristics (maintained activity, declining function). The accessibility_collapse and resistance scores reflect the constraint's legitimacy foundation: alternatives (mitigation, degrowth) are not physically closed off but are framed as impossible within the wealthy-nation policy space; resistance comes from mitigation advocates and excluded communities, but their resistance lacks institutional power to reshape the frame.
 *
 * PERSPECTIVAL GAP:
 *   Wealthy-nation governments and carbon-intensive industries perceive the constraint as genuine, pragmatic coordination (a real climate response that is politically feasible and economically manageable). Low-income regions, small island states, and future generations perceive it as enforced extraction (accepting warming they did not cause, financing adaptation they did not require, excluded from mitigation decisions). The engine should compute this perspectival divergence from the structural data: wealthy-nation agendas-setters derive d near 0.1 (beneficiary, low extraction burden); low-income victims derive d near 0.85 (target, high extraction burden, trapped exit). The mitigation and degrowth advocates who are excluded derive a different type (snare rather than rope) from their seat, because they are not coordinated — their exclusion from the frame is the constraint's primary function. The authored claim is tangled_rope (the reading's own framing: genuine coordination + asymmetric extraction), but seats on the payer side (low-income regions, future generations) compute as snare (pure extraction, no coordination benefit, trapped exit) — this divergence is the key analytic output.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries: wealthy-nation governments set the frame (d≈0.15, agenda-setter status, institutional power, mobile exit—they can exit by shifting to mitigation if growth preservation becomes impossible, though political costs are high); carbon-intensive industries (d≈0.2, powerful status, constrained exit—can lobby but cannot openly oppose climate action; preservation of business models rides on the frame; exit means accepting deep decarbonization). Victims: low-income regions (d≈0.85, powerless status, trapped exit—adaptation is the only option within the frame; exiting means rejecting the international climate architecture); small island states (d≈0.9, powerless status, trapped exit—adaptation cannot solve territorial disappearance; exit is extinction). Excluded voices: mitigation and degrowth advocates (d≈0.7–0.8 from the constraint's perspective—they are structurally opposed, ejected from the frame, bear the cost of their exclusion). Future generations (d≈1.0, powerless, civilizational time horizon, trapped exit—no voice, compounding liability). The directionality_overrides are not needed here; the derivation from beneficiary/victim + exit produces accurate d values. The asymmetry is structural, not observationally ambiguous.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint avoids mandatrophy diagnosis in early phase (t=0–10) because it is genuinely responsive to a founding problem (wealthy nations seeking climate legitimacy) and it does coordinate a real function (international consensus on climate response). By t=20–30, mandatrophy signals emerge: theater_ratio rises to 0.40+ while extractiveness plateaus, indicating that adaptation activity is increasingly performative; founding_problem_status enters contested (climate scientists and low-income nations attest the founding problem—'mitigation is economically unrealistic'—has been solved by technology cost curves; the constraint persists as power consolidation, not problem-solving). The theater trajectory and founding-problem-status mismatch are the key signals. If the constraint were genuinely solving the adaptation problem, extraction would decrease over time (as adaptive capacity increases, vulnerability decreases). Instead, extraction plateaus while theater rises, indicating the real function (wealth transfer, emissions deferral) is decoupled from the stated function (adaptation adequacy). This is the Piton signature: the constraint is increasingly maintained by institutional inertia and performance, not by solving the problem it claims to solve. A mandatrophy_analysis omega should note: by t=30, the constraint shows strong piton characteristics (theater rising, function declining, power consolidation stable) overlaid on the tangled_rope structure. The constraint is evolving toward degradation while maintaining institutional legitimacy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    adaptation_adequacy_boundary,
    'At what warming threshold do adaptation costs exceed the capacity of even wealthy regions to implement, and what happens to the legitimacy frame when that threshold is approached?',
    'Empirical evidence from IPCC tipping-point assessments and regional adaptation-cost modeling; observation of whether policy frames shift when projections cross adequacy boundaries (3°C, 4°C, 5°C warming scenarios).',
    'If adaptation is provably insufficient at projected warming levels, the legitimacy of the adaptation-priority reading collapses and the policy frame must shift toward mitigation or degrowth. If adaptation remains theoretically possible at high cost, the frame persists but extraction intensifies (higher costs borne by payers).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(adaptation_adequacy_boundary, empirical, 'Empirical boundary where adaptation legitimacy claim fails.').

omega_variable(
    intergenerational_cost_accumulation,
    'How do intergenerational costs compound under adaptation-priority vs. mitigation-priority pathways, and what is the threshold at which deferral becomes catastrophic for future generations?',
    'Long-term cost modeling (50–100 year horizons); comparison of present-value calculations under different discount rates (which encode intergenerational equity assumptions); observation of whether future-generation representation enters policy frameworks in response to compounding-cost evidence.',
    'High intergenerational cost accumulation would vindicate the degrowth reading''s claim that adaptation cannot address the structural problem. The constraint''s legitimacy depends partly on suppressing intergenerational accounting or using discount rates that minimize future costs.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intergenerational_cost_accumulation, empirical, 'Temporal asymmetry: who pays extraction now vs. later.').

omega_variable(
    mitigation_technical_feasibility_shift,
    'If renewable energy and decarbonization technologies become dramatically cheaper or faster to deploy than current projections, does the legitimacy claim of adaptation-priority shift?',
    'Observation of technology cost curves, deployment timelines, and policy response when technical barriers to mitigation fall. Does the adaptation-priority frame persist or is it abandoned once the ''economic realism'' argument no longer holds?',
    'If mitigation becomes obviously cheaper than adaptation, the constraint''s core legitimacy claim (that adaptation is pragmatic given mitigation''s cost) is undercut. The frame persists only through explicit suppression of technical evidence or through power-consolidation mechanisms independent of pragmatism claims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mitigation_technical_feasibility_shift, empirical, 'Whether legitimacy claim is evidence-responsive or power-driven.').

omega_variable(
    growth_model_locked_in,
    'Is the adaptation-priority reading structurally incompatible with any future transition away from growth? Does accepting higher warming while preserving growth lock in a trajectory that forecloses the degrowth reading?',
    'Analysis of whether carbon-budget consumption and infrastructure lock-in create path dependence that rules out degrowth transformation. Observation of whether policy shifts toward growth reduction become politically feasible once adaptation-priority is institutionalized.',
    'If adaptation-priority does foreclose degrowth transformation, the two readings have a genuine logical incompatibility, not just a political rivalry. This would upgrade the reading-relation to forecloses rather than influences.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(growth_model_locked_in, conceptual, 'Logical structure of competing climate-response kernels.').

omega_variable(
    low_income_region_coalition_power,
    'Can low-income regions coalesce into a blocking coalition within climate negotiations, and does such a coalition change the power distribution of the constraint?',
    'Observation of voting patterns in UNFCCC negotiations, formation of south-south alliances, and whether unified refusal to accept adaptation-only frameworks forces renegotiation of what counts as legitimate response.',
    'Coalition power would shift directionality: currently powerless victims become organized payers with veto capacity. The constraint would require higher enforcement intensity or would collapse. This is a material test of whether the constraint''s persistence is grounded in the power atoms or in legitimacy narratives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(low_income_region_coalition_power, empirical, 'Whether powerless victims can organize countervailing power.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_legitimacy__adaptation_priority, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_response_legitimacy__adaptation_priority, theater_ratio, 0, 0.22).
narrative_ontology:measurement_basis(clim_tr_t0, observed).
narrative_ontology:measurement(clim_tr_t5, climate_response_legitimacy__adaptation_priority, theater_ratio, 5, 0.26).
narrative_ontology:measurement_basis(clim_tr_t5, observed).
narrative_ontology:measurement(clim_tr_t10, climate_response_legitimacy__adaptation_priority, theater_ratio, 10, 0.31).
narrative_ontology:measurement_basis(clim_tr_t10, observed).
narrative_ontology:measurement(clim_tr_t15, climate_response_legitimacy__adaptation_priority, theater_ratio, 15, 0.36).
narrative_ontology:measurement_basis(clim_tr_t15, observed).
narrative_ontology:measurement(clim_tr_t20, climate_response_legitimacy__adaptation_priority, theater_ratio, 20, 0.39).
narrative_ontology:measurement_basis(clim_tr_t20, observed).
narrative_ontology:measurement(clim_tr_t25, climate_response_legitimacy__adaptation_priority, theater_ratio, 25, 0.4).
narrative_ontology:measurement_basis(clim_tr_t25, observed).
narrative_ontology:measurement(clim_tr_t30, climate_response_legitimacy__adaptation_priority, theater_ratio, 30, 0.41).
narrative_ontology:measurement_basis(clim_tr_t30, observed).
narrative_ontology:measurement(clim_tr_t40, climate_response_legitimacy__adaptation_priority, theater_ratio, 40, 0.42).
narrative_ontology:measurement_basis(clim_tr_t40, projected).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_response_legitimacy__adaptation_priority, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(clim_be_t0, observed).
narrative_ontology:measurement(clim_be_t5, climate_response_legitimacy__adaptation_priority, base_extractiveness, 5, 0.54).
narrative_ontology:measurement_basis(clim_be_t5, observed).
narrative_ontology:measurement(clim_be_t10, climate_response_legitimacy__adaptation_priority, base_extractiveness, 10, 0.59).
narrative_ontology:measurement_basis(clim_be_t10, observed).
narrative_ontology:measurement(clim_be_t15, climate_response_legitimacy__adaptation_priority, base_extractiveness, 15, 0.63).
narrative_ontology:measurement_basis(clim_be_t15, observed).
narrative_ontology:measurement(clim_be_t20, climate_response_legitimacy__adaptation_priority, base_extractiveness, 20, 0.66).
narrative_ontology:measurement_basis(clim_be_t20, observed).
narrative_ontology:measurement(clim_be_t25, climate_response_legitimacy__adaptation_priority, base_extractiveness, 25, 0.67).
narrative_ontology:measurement_basis(clim_be_t25, observed).
narrative_ontology:measurement(clim_be_t30, climate_response_legitimacy__adaptation_priority, base_extractiveness, 30, 0.68).
narrative_ontology:measurement_basis(clim_be_t30, observed).
narrative_ontology:measurement(clim_be_t40, climate_response_legitimacy__adaptation_priority, base_extractiveness, 40, 0.68).
narrative_ontology:measurement_basis(clim_be_t40, projected).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_response_legitimacy__adaptation_priority, suppression_requirement, 0, 0.54).
narrative_ontology:measurement_basis(clim_su_t0, observed).
narrative_ontology:measurement(clim_su_t5, climate_response_legitimacy__adaptation_priority, suppression_requirement, 5, 0.6).
narrative_ontology:measurement_basis(clim_su_t5, observed).
narrative_ontology:measurement(clim_su_t10, climate_response_legitimacy__adaptation_priority, suppression_requirement, 10, 0.64).
narrative_ontology:measurement_basis(clim_su_t10, observed).
narrative_ontology:measurement(clim_su_t15, climate_response_legitimacy__adaptation_priority, suppression_requirement, 15, 0.68).
narrative_ontology:measurement_basis(clim_su_t15, observed).
narrative_ontology:measurement(clim_su_t20, climate_response_legitimacy__adaptation_priority, suppression_requirement, 20, 0.7).
narrative_ontology:measurement_basis(clim_su_t20, observed).
narrative_ontology:measurement(clim_su_t25, climate_response_legitimacy__adaptation_priority, suppression_requirement, 25, 0.71).
narrative_ontology:measurement_basis(clim_su_t25, observed).
narrative_ontology:measurement(clim_su_t30, climate_response_legitimacy__adaptation_priority, suppression_requirement, 30, 0.72).
narrative_ontology:measurement_basis(clim_su_t30, observed).
narrative_ontology:measurement(clim_su_t40, climate_response_legitimacy__adaptation_priority, suppression_requirement, 40, 0.74).
narrative_ontology:measurement_basis(clim_su_t40, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_response_legitimacy__adaptation_priority, resource_allocation).
narrative_ontology:boltzmann_floor_override(climate_response_legitimacy__adaptation_priority, 0.18).
narrative_ontology:affects_constraint(climate_response_legitimacy__adaptation_priority, climate_response_legitimacy__mitigation_priority).
narrative_ontology:affects_constraint(climate_response_legitimacy__adaptation_priority, climate_response_legitimacy__degrowth_transformation).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the climate_response_legitimacy kernel. The sibling readings (mitigation_priority, degrowth_transformation) are separate constraint stories linked via this network entry. The three stories share a fixed referent (the standing arrangements for climate policy) but author different ε values because each reading interprets the arrangement through different legitimacy lenses. The adaptation-priority reading authors ε as 0.68 (extraction is substantial because asymmetric costs are imposed on low-income regions and future generations while wealthy-nation growth is preserved). The mitigation-priority reading would author lower ε (framing deep emissions reduction as compatible with growth). The degrowth reading would author higher ε (framing growth preservation itself as extraction from future generations and lower-income populations). Decomposition is required by ε-invariance: a single constraint cannot simultaneously embody all three readings' ε values.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(climate_response_legitimacy__adaptation_priority, powerless, 0.88).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
