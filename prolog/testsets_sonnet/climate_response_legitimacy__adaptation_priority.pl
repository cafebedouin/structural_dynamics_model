% ============================================================================
% CONSTRAINT STORY: climate_response_legitimacy__adaptation_priority
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
 *   constraint_id: climate_response_legitimacy__adaptation_priority
 *   human_readable: Adaptation-Priority Climate Legitimacy Reading
 *   domain: climate policy / political economy / intergenerational ethics
 *
 * SUMMARY:
 *   This story instantiates the adaptation-priority reading of the contested
 *   'climate response legitimacy' kernel: the position that a legitimate
 *   climate response accepts the warming trajectory as effectively given and
 *   prioritizes protecting vulnerable populations through resilience
 *   infrastructure and adaptive capacity, rather than centering further
 *   emissions reduction or structural economic transformation. This is a
 *   genuinely held, well-resourced policy position (reflected in COP finance
 *   architecture, bilateral adaptation funds, and much development-agency
 *   practice), not a strawman. Its structural signature: low-income regions
 *   with negligible historical emissions enter the victim set immediately
 *   through the chronic adaptation-finance gap (roughly $350B/year
 *   underfunded relative to assessed need), wealthy emitter states retain
 *   their development and emissions latitude, and the compounding costs of
 *   higher accepted warming are pushed onto future generations who have no
 *   seat at the table. The sibling readings — mitigation_priority and
 *   degrowth_transformation — are NOT represented in this file; they are
 *   separate constraints with their own ε and stakeholder structures, linked
 *   via network.affects_constraints.
 *
 * KEY AGENTS:
 *   - wealthy_emitter_states: agenda-setter, retains emissions headroom while funding adaptation as discharge of obligation
 *   - fossil_incumbent_industries: beneficiary, adaptation framing removes supply-side pressure
 *   - low_income_coastal_populations / sahel_agrarian_communities / small_island_states: powerless payers, bear physical impacts under chronic adaptation underfunding
 *   - future_generations: powerless payer, inherits compounded costs of a higher accepted warming trajectory
 *   - adaptation_finance_intermediaries / resilience_engineering_sector: beneficiaries whose institutional survival depends on the deficit persisting
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_legitimacy__adaptation_priority, 0.68).
domain_priors:suppression_score(climate_response_legitimacy__adaptation_priority, 0.52).
domain_priors:theater_ratio(climate_response_legitimacy__adaptation_priority, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_legitimacy__adaptation_priority, extractiveness, 0.68).
narrative_ontology:constraint_metric(climate_response_legitimacy__adaptation_priority, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(climate_response_legitimacy__adaptation_priority, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_response_legitimacy__adaptation_priority, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(climate_response_legitimacy__adaptation_priority, resistance, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_legitimacy__adaptation_priority, tangled_rope).
narrative_ontology:human_readable(climate_response_legitimacy__adaptation_priority, "Adaptation-Priority Climate Legitimacy Reading").
narrative_ontology:topic_domain(climate_response_legitimacy__adaptation_priority, "climate policy / political economy / intergenerational ethics").

domain_priors:requires_active_enforcement(climate_response_legitimacy__adaptation_priority).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_legitimacy__adaptation_priority, 'f9e1ff57-1195-4249-9a1c-969063fa9cbd').
narrative_ontology:cs_kernel_codification('f9e1ff57-1195-4249-9a1c-969063fa9cbd', distributed).
narrative_ontology:cs_authority_grounding('f9e1ff57-1195-4249-9a1c-969063fa9cbd', distributed).
narrative_ontology:cs_reading_relation('f9e1ff57-1195-4249-9a1c-969063fa9cbd', climate_response_legitimacy__mitigation_priority, coexists_with).
narrative_ontology:cs_reading_relation('f9e1ff57-1195-4249-9a1c-969063fa9cbd', climate_response_legitimacy__degrowth_transformation, influences).
narrative_ontology:cs_axiom('f9e1ff57-1195-4249-9a1c-969063fa9cbd', foundational, warming_trajectory_treated_as_background_constant).
narrative_ontology:cs_axiom_status(warming_trajectory_treated_as_background_constant, holdable).
narrative_ontology:cs_axiom_grounding('f9e1ff57-1195-4249-9a1c-969063fa9cbd', warming_trajectory_treated_as_background_constant, empirically_contingent).
narrative_ontology:cs_axiom('f9e1ff57-1195-4249-9a1c-969063fa9cbd', foundational, protective_infrastructure_discharges_climate_obligation).
narrative_ontology:cs_axiom_status(protective_infrastructure_discharges_climate_obligation, holdable).
narrative_ontology:cs_axiom_grounding('f9e1ff57-1195-4249-9a1c-969063fa9cbd', protective_infrastructure_discharges_climate_obligation, instrumental).
narrative_ontology:cs_reference_frame('f9e1ff57-1195-4249-9a1c-969063fa9cbd', unfccc_common_but_differentiated_responsibility).
narrative_ontology:cs_drift_state('f9e1ff57-1195-4249-9a1c-969063fa9cbd', post_paris_agreement_finance_shortfall_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('f9e1ff57-1195-4249-9a1c-969063fa9cbd', '').
narrative_ontology:cs_kernel_id(climate_response_legitimacy__adaptation_priority, climate_response_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_legitimacy__adaptation_priority, wealthy_emitter_states).
narrative_ontology:constraint_beneficiary(climate_response_legitimacy__adaptation_priority, fossil_incumbent_industries).
narrative_ontology:constraint_beneficiary(climate_response_legitimacy__adaptation_priority, adaptation_finance_intermediaries).
narrative_ontology:constraint_victim(climate_response_legitimacy__adaptation_priority, low_income_coastal_populations).
narrative_ontology:constraint_victim(climate_response_legitimacy__adaptation_priority, sahel_agrarian_communities).
narrative_ontology:constraint_victim(climate_response_legitimacy__adaptation_priority, small_island_states).
narrative_ontology:constraint_victim(climate_response_legitimacy__adaptation_priority, future_generations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(climate_response_legitimacy__adaptation_priority, resilience_engineering_sector).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set the terms of international climate finance and diplomatic framing, championing 'resilience' and 'adaptive capacity' as the legitimate response while continuing high-emissions development pathways domestically. They fund adaptation programs abroad at a fraction of the assessed need, retain the emissions headroom that produces the warming being adapted to, and frame further mitigation demands as unrealistic or economically destabilizing.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__adaptation_priority, wealthy_emitter_states, agenda_setter,
    institutional, generational, arbitrage, global).

% Continue extraction and production largely undisturbed under a policy consensus that treats warming as a fixed trajectory to be managed rather than a rate to be reduced. Actively fund and amplify adaptation-first framing because it removes political pressure from supply-side constraints on their core business.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__adaptation_priority, fossil_incumbent_industries, beneficiary,
    organized, biographical, arbitrage, global).

% Multilateral funds, consultancies, and contractors that design, disburse, and audit adaptation infrastructure projects. Capture administrative fees and technical-assistance contracts from the finance flow; their institutional survival depends on the adaptation deficit remaining large and continuously fundable, not on it closing.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__adaptation_priority, adaptation_finance_intermediaries, beneficiary,
    organized, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(climate_response_legitimacy__adaptation_priority, adaptation_finance_intermediaries, agenda_setter).

% Face rising sea levels, storm intensity, and salinization with adaptation infrastructure funded at a fraction (roughly a third, by most assessments) of the estimated $350B annual need. Cannot migrate easily, cannot bill emitters directly, and depend on unreliable, often delayed multilateral disbursement for sea walls, drainage, and relocation support.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__adaptation_priority, low_income_coastal_populations, payer,
    powerless, biographical, trapped, regional).

% Experience desertification and rainfall disruption that outpaces the drought-resistant seed programs and irrigation projects provided as adaptation aid. Bear a warming trajectory set almost entirely by emissions they did not produce, with resilience funding contingent on donor political cycles rather than assessed need.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__adaptation_priority, sahel_agrarian_communities, payer,
    powerless, biographical, trapped, regional).

% Face existential territorial loss under any warming trajectory above roughly 1.5C, which the adaptation-priority reading implicitly accepts as a managed baseline. Their diplomatic voice at COP proceedings is acknowledged rhetorically but structurally outweighed by the voting and funding power of wealthy emitter blocs; they argue adaptation cannot substitute for the mitigation that would preserve their land at all.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__adaptation_priority, small_island_states, excluded,
    powerless, civilizational, trapped, global).
narrative_ontology:stakeholder_secondary_role(climate_response_legitimacy__adaptation_priority, small_island_states, payer).

% Inherit a higher-warming baseline than a mitigation-first pathway would produce, plus whatever adaptation infrastructure survives; are not present at any negotiating table and bear compounding costs (sea level, ecosystem collapse, resource conflict) that scale nonlinearly with the deferred emissions this reading tolerates.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__adaptation_priority, future_generations, payer,
    powerless, civilizational, trapped, global).

% Engineering, insurance, and infrastructure firms that design and build seawalls, early-warning systems, and climate-resilient agriculture. Have a direct commercial stake in adaptation being the dominant policy paradigm, since a mitigation-first shift would redirect capital toward decarbonization rather than resilience construction.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__adaptation_priority, resilience_engineering_sector, beneficiary,
    organized, biographical, mobile, global).

% Produce the warming trajectory assessments and adaptation-gap reports that all readings cite; do not set policy but their attribution science and cost-of-inaction estimates are used selectively by every faction in the kernel contest to support its preferred reading.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__adaptation_priority, climate_science_and_ipcc_reporting_bodies, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_response_legitimacy__adaptation_priority, diffuse).
narrative_ontology:fixing_cost_class(climate_response_legitimacy__adaptation_priority, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Directs available climate finance toward concrete, deliverable protective infrastructure (seawalls, early-warning systems, drought-resistant agriculture) for populations already experiencing climate impacts, rather than deferring all action to contested and slower-moving emissions-reduction politics.
% TRANSFER_FUNCTION: Moves a declared but chronically underfunded finance stream from wealthy emitter states to vulnerable regions, while the emissions headroom and development latitude that produce the underlying warming remain with the wealthy states; also transfers compounding physical and fiscal risk forward onto future generations.
% ABSENT_VOICES: Small island states and youth/future-generation representatives argue at COP proceedings that adaptation funding, however welcome, cannot substitute for the mitigation that would preserve their territory or bequeath a stable baseline; their objections are heard in plenary sessions but do not alter the finance allocation or the accepted warming trajectory.
% DISAPPEARANCE_RATIONALE: If the adaptation-priority framing lost legitimacy overnight, the diplomatic and finance architecture built around 'resilience and adaptive capacity' as the primary deliverable would need to be renegotiated toward binding mitigation targets or structural economic transformation; adaptation finance intermediaries and resilience engineering firms would lose their institutional rationale, and wealthy states would face direct pressure on emissions rather than being able to discharge climate obligations through infrastructure grants.
% FOUNDING_PROBLEM: Even under best-case mitigation, some warming and associated impacts are already locked in by historical emissions; vulnerable populations facing immediate, life-threatening exposure (flooding, drought, storms) need protective infrastructure now, independent of how the mitigation debate resolves.
% FOUNDING_PROBLEM_CORROBORATION: UN Environment Programme adaptation-gap reports and independent development economists outside the beneficiary set corroborate that the underlying protective need is real and underfunded (roughly a third of assessed need met); however, the same reports and small-island-state diplomats attest that adaptation-priority framing has been used by wealthy emitter states and fossil incumbents to displace mitigation commitments rather than complement them, converting a genuine humanitarian problem into a legitimating cover for continued emissions.
narrative_ontology:disappearance_verdict(climate_response_legitimacy__adaptation_priority, world_rearranges).
narrative_ontology:founding_problem_status(climate_response_legitimacy__adaptation_priority, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_response_legitimacy__adaptation_priority, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(climate_response_legitimacy__adaptation_priority, 'none', 1).
narrative_ontology:epsilon_provenance(climate_response_legitimacy__adaptation_priority, 0.68, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored as substantial (0.68 at interval end) and rising because the gap between assessed adaptation need and delivered finance has widened even as rhetorical commitment to 'resilience' has intensified — the constraint increasingly extracts legitimacy from vulnerable populations' suffering without closing the funding gap that would resolve it. Suppression is moderate (0.52): there is no direct coercive apparatus, but small island states and youth delegations face structural marginalization in negotiating fora, and dissenting mitigation-first voices are diplomatically outweighed rather than silenced outright. Theater ratio is authored at 0.44 and rising, reflecting the growing share of adaptation-finance activity that is pledging, reporting, and conference activity relative to disbursed, functioning infrastructure — a documented pattern in adaptation-finance tracking literature. Accessibility collapse is moderate (0.42): alternative framings (mitigation-first, degrowth) remain politically live and are not foreclosed, only structurally disadvantaged in current finance architecture. Resistance is substantial (0.61) — small island states, youth movements, and some development economists actively contest the adequacy of adaptation-only framing.
 *
 * DIRECTIONALITY LOGIC:
 *   Wealthy emitter states and fossil incumbents sit near the full-beneficiary end: they set the agenda, retain emissions latitude, and can exit the political costs of insufficient mitigation by pointing to adaptation funding as discharge of responsibility (arbitrage exit). Adaptation finance intermediaries and the resilience engineering sector are structural beneficiaries whose institutional interest is served by the deficit persisting rather than closing — a subtler capture dynamic. Low-income coastal and Sahel populations, and small island states, are trapped targets: no meaningful exit from the physical exposure, no leverage over the finance flow, high time-horizon stakes (some civilizational, given territorial loss). Future generations are the purest full-target case: zero present voice, maximal compounding exposure,완전히 trapped by the accident of not yet existing.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — vulnerable populations facing locked-in impacts need protection now — remains genuinely live; this constraint is not simple institutional fossilization. The mandatrophy risk is subtler: the founding problem's genuine liveness is used to legitimate treating warming trajectory as fixed rather than contested, which is a policy choice, not a scientific necessity. Classifying this as tangled_rope (not snare) preserves the real coordination function — protective infrastructure delivery is not fake — while flagging that the same structure asymmetrically extracts political cover for continued high emissions from populations who cannot bargain for better terms. A pure snare framing would deny that any genuine adaptation delivery occurs; a pure rope framing would deny the asymmetric extraction the founding-problem corroboration documents.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    trajectory_acceptance_as_policy_choice_or_necessity,
    'Is treating the current warming trajectory as effectively fixed a realistic acknowledgment of locked-in emissions, or a policy choice that forecloses more aggressive mitigation while claiming inevitability?',
    'Compare the counterfactual emissions pathways modeled as technically and economically feasible in IPCC mitigation scenarios against the trajectory implicitly accepted by current adaptation-dominant finance allocation; a wide gap between feasible and accepted trajectories would indicate policy choice rather than necessity.',
    'If trajectory acceptance is a policy choice rather than physical necessity, the adaptation-priority reading functions partly as a legitimating cover for continued emissions rather than a pure humanitarian response, strengthening the tangled_rope classification over a pure rope reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(trajectory_acceptance_as_policy_choice_or_necessity, conceptual, 'Whether accepting the warming trajectory is empirical realism or a legitimating policy stance.').

omega_variable(
    adaptation_finance_capture_extent,
    'What share of adaptation finance intermediary institutions'' operating survival depends on the adaptation deficit remaining open versus genuinely working to close it?',
    'Audit trail analysis of multilateral adaptation fund administrative overhead ratios, contractor renewal incentives, and disbursement timelines relative to pledged amounts over multiple funding cycles.',
    'High capture would support treating adaptation_finance_intermediaries as a concentrated beneficiary class analogous to a snare dynamic within the broader tangled_rope structure; low capture would support a more benign rope reading of the finance mechanism itself.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(adaptation_finance_capture_extent, empirical, 'Whether adaptation-finance institutions are structurally incentivized to perpetuate rather than close the funding gap.').

omega_variable(
    sibling_reading_disagreement_locus,
    'Where exactly does this reading''s core premise conflict with the mitigation_priority and degrowth_transformation readings — is it a disagreement about facts (feasibility of rapid decarbonization), values (whose development model has priority), or both?',
    'Structural comparison of the three readings'' foundational axioms: adaptation_priority treats emissions trajectory as background-fixed and protection as foreground; mitigation_priority treats emissions trajectory as the primary controllable variable; degrowth_transformation treats the wealthy-nation growth model itself as the object requiring transformation. These are not fully commensurable on a single empirical axis.',
    'Locating the disagreement clarifies whether the three readings could in principle be synthesized (complementary policy layers) or are genuinely mutually exclusive as political programs competing for the same finite finance and political capital.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_disagreement_locus, conceptual, 'Whether the kernel contest is fundamentally empirical, distributive, or structural.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_legitimacy__adaptation_priority, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_response_legitimacy__adaptation_priority, theater_ratio, 0, 0.28).
narrative_ontology:measurement(clim_tr_t5, climate_response_legitimacy__adaptation_priority, theater_ratio, 5, 0.32).
narrative_ontology:measurement(clim_tr_t10, climate_response_legitimacy__adaptation_priority, theater_ratio, 10, 0.35).
narrative_ontology:measurement(clim_tr_t15, climate_response_legitimacy__adaptation_priority, theater_ratio, 15, 0.38).
narrative_ontology:measurement(clim_tr_t20, climate_response_legitimacy__adaptation_priority, theater_ratio, 20, 0.4).
narrative_ontology:measurement(clim_tr_t25, climate_response_legitimacy__adaptation_priority, theater_ratio, 25, 0.42).
narrative_ontology:measurement(clim_tr_t30, climate_response_legitimacy__adaptation_priority, theater_ratio, 30, 0.44).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_response_legitimacy__adaptation_priority, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(clim_be_t5, climate_response_legitimacy__adaptation_priority, base_extractiveness, 5, 0.51).
narrative_ontology:measurement(clim_be_t10, climate_response_legitimacy__adaptation_priority, base_extractiveness, 10, 0.56).
narrative_ontology:measurement(clim_be_t15, climate_response_legitimacy__adaptation_priority, base_extractiveness, 15, 0.6).
narrative_ontology:measurement(clim_be_t20, climate_response_legitimacy__adaptation_priority, base_extractiveness, 20, 0.63).
narrative_ontology:measurement(clim_be_t25, climate_response_legitimacy__adaptation_priority, base_extractiveness, 25, 0.66).
narrative_ontology:measurement(clim_be_t30, climate_response_legitimacy__adaptation_priority, base_extractiveness, 30, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_response_legitimacy__adaptation_priority, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(clim_su_t5, climate_response_legitimacy__adaptation_priority, suppression_requirement, 5, 0.42).
narrative_ontology:measurement(clim_su_t10, climate_response_legitimacy__adaptation_priority, suppression_requirement, 10, 0.45).
narrative_ontology:measurement(clim_su_t15, climate_response_legitimacy__adaptation_priority, suppression_requirement, 15, 0.47).
narrative_ontology:measurement(clim_su_t20, climate_response_legitimacy__adaptation_priority, suppression_requirement, 20, 0.49).
narrative_ontology:measurement(clim_su_t25, climate_response_legitimacy__adaptation_priority, suppression_requirement, 25, 0.51).
narrative_ontology:measurement(clim_su_t30, climate_response_legitimacy__adaptation_priority, suppression_requirement, 30, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_response_legitimacy__adaptation_priority, resource_allocation).
narrative_ontology:boltzmann_floor_override(climate_response_legitimacy__adaptation_priority, 0.15).
narrative_ontology:affects_constraint(climate_response_legitimacy__adaptation_priority, climate_response_legitimacy__mitigation_priority).
narrative_ontology:affects_constraint(climate_response_legitimacy__adaptation_priority, climate_response_legitimacy__degrowth_transformation).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the climate_response_legitimacy kernel, each instantiated as a separate story with its own epsilon: adaptation_priority (this file, tangled_rope — genuine protective coordination plus asymmetric extraction of political cover from powerless populations), mitigation_priority (separate file — technology/carbon-pricing framing), and degrowth_transformation (separate file — structural transformation framing). The readings are linked bidirectionally via affects_constraints because finance and political capital allocated to one reading structurally reduces what is available to the others; they are not merged into one story because their epsilon values, beneficiary/victim structures, and enforcement mechanisms differ substantially.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
