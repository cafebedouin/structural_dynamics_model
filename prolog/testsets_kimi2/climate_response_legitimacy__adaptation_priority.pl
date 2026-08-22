% ============================================================================
% CONSTRAINT STORY: climate_response_legitimacy__adaptation_priority
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   constraint_id: climate_response_legitimacy__adaptation_priority
 *   human_readable: Adaptation-Priority Legitimacy Frame for Climate Response
 *   domain: climate policy / political economy / intergenerational ethics
 *
 * SUMMARY:
 *   This constraint instantiates the adaptation_priority reading of the
 *   contested kernel climate_response_legitimacy. Under this reading,
 *   legitimate climate action is defined as accepting future warming
 *   trajectories while prioritizing the protection of vulnerable populations
 *   through resilience infrastructure and adaptive-capacity building. The
 *   constraint operates through international climate finance architecture
 *   (UNFCCC, Green Climate Fund, NDC adaptation components), bilateral
 *   adaptation aid, and the political framing that wealthy nations can
 *   preserve their development models while meeting climate obligations via
 *   adaptation support. The structural delta from sibling readings is
 *   immediate: low-income regions enter the victim set through a chronic
 *   adaptation-finance deficit ($350 billion gap), wealthy regions preserve
 *   their economic trajectory, and intergenerational costs are deferred but
 *   compounded by higher locked-in warming. The colloquial label 'climate
 *   response' conflates three structurally distinct constraints; this file
 *   isolates the adaptation-priority claim with its own epsilon and
 *   classification.
 *
 * KEY AGENTS:
 *   - wealthy_nations: Primary agenda-setter (institutional/arbitrage) â dominates finance governance, preserves development model
 *   - low_income_regions: Primary payer (powerless/trapped) â bears adaptation deficit and climate impacts
 *   - future_generations: Secondary payer (powerless/trapped) â inherits compounded warming costs
 *   - multilateral_climate_institutions: Agenda-setter (institutional/constrained) â administers adaptation architecture, depends on donor nations
 *   - fossil_fuel_incumbents: Beneficiary (powerful/arbitrage) â warming trajectory acceptance extends asset viability
 *   - climate_vulnerable_small_island_states: Payer (powerless/trapped) â territorial loss under accepted warming
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_legitimacy__adaptation_priority, 0.72).
domain_priors:suppression_score(climate_response_legitimacy__adaptation_priority, 0.7).
domain_priors:theater_ratio(climate_response_legitimacy__adaptation_priority, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_legitimacy__adaptation_priority, extractiveness, 0.72).
narrative_ontology:constraint_metric(climate_response_legitimacy__adaptation_priority, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(climate_response_legitimacy__adaptation_priority, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_response_legitimacy__adaptation_priority, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(climate_response_legitimacy__adaptation_priority, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_legitimacy__adaptation_priority, tangled_rope).
narrative_ontology:human_readable(climate_response_legitimacy__adaptation_priority, "Adaptation-Priority Legitimacy Frame for Climate Response").
narrative_ontology:topic_domain(climate_response_legitimacy__adaptation_priority, "climate policy / political economy / intergenerational ethics").

domain_priors:requires_active_enforcement(climate_response_legitimacy__adaptation_priority).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_legitimacy__adaptation_priority, '59a96a94-b237-4e4c-933b-8931440d70d5').
narrative_ontology:cs_kernel_codification('59a96a94-b237-4e4c-933b-8931440d70d5', distributed).
narrative_ontology:cs_authority_grounding('59a96a94-b237-4e4c-933b-8931440d70d5', distributed).
narrative_ontology:cs_reading_relation('59a96a94-b237-4e4c-933b-8931440d70d5', climate_response_legitimacy__mitigation_priority, influences).
narrative_ontology:cs_reading_relation('59a96a94-b237-4e4c-933b-8931440d70d5', climate_response_legitimacy__degrowth_transformation, forecloses).
narrative_ontology:cs_axiom('59a96a94-b237-4e4c-933b-8931440d70d5', foundational, adaptation_sufficiency_at_accepted_warming).
narrative_ontology:cs_axiom_status(adaptation_sufficiency_at_accepted_warming, holdable).
narrative_ontology:cs_axiom_grounding('59a96a94-b237-4e4c-933b-8931440d70d5', adaptation_sufficiency_at_accepted_warming, empirically_contingent).
narrative_ontology:cs_reference_frame('59a96a94-b237-4e4c-933b-8931440d70d5', pragmatic_adaptation_governance).
narrative_ontology:cs_drift_state('59a96a94-b237-4e4c-933b-8931440d70d5', contemporary_cop_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('59a96a94-b237-4e4c-933b-8931440d70d5', '').
narrative_ontology:cs_kernel_id(climate_response_legitimacy__adaptation_priority, climate_response_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_legitimacy__adaptation_priority, wealthy_nations).
narrative_ontology:constraint_beneficiary(climate_response_legitimacy__adaptation_priority, fossil_fuel_incumbents).
narrative_ontology:constraint_beneficiary(climate_response_legitimacy__adaptation_priority, multilateral_climate_institutions).
narrative_ontology:constraint_victim(climate_response_legitimacy__adaptation_priority, low_income_regions).
narrative_ontology:constraint_victim(climate_response_legitimacy__adaptation_priority, future_generations).
narrative_ontology:constraint_victim(climate_response_legitimacy__adaptation_priority, climate_vulnerable_small_island_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Dominate UNFCCC negotiations, Green Climate Fund governance, and NDC architecture. Preserve existing economic development models by framing climate legitimacy around adaptation finance rather than binding mitigation or structural economic transformation. Set the terms of adaptation pledges and disbursement conditions while avoiding emission-reduction obligations that would disrupt domestic industries.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__adaptation_priority, wealthy_nations, agenda_setter,
    institutional, biographical, arbitrage, global).

% Benefit from the warming-trajectory acceptance embedded in the adaptation-priority frame, which extends the economic viability of hydrocarbon reserves and delays structural energy-transition mandates. Support resilience framing as an alternative to emission caps or demand reduction.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__adaptation_priority, fossil_fuel_incumbents, beneficiary,
    powerful, biographical, arbitrage, global).

% Administer adaptation finance, resilience programming, and NDC tracking. Their institutional budgets and mandates depend on wealthy-nation contributions and political buy-in. They legitimize the adaptation-priority frame through reporting, project certification, and coproduced policy documents, even when finance flows fall far short of assessed needs.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__adaptation_priority, multilateral_climate_institutions, agenda_setter,
    institutional, generational, constrained, global).

% Bear disproportionate climate impacts with insufficient adaptation finance. Face a chronic adaptation-finance gap (approximately $350 billion annually) and depend on slow, conditional, donor-driven resilience projects. Cannot exit warming trajectories or unilaterally fund infrastructure at the scale required by accepted emission pathways.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__adaptation_priority, low_income_regions, payer,
    powerless, immediate, trapped, national).

% Inherit the compounded costs of deferred mitigation: higher locked-in warming, irreversible tipping points, and adaptation limits exceeded. They are structurally unrepresented in contemporary finance and negotiation frameworks and cannot reject the warming trajectory chosen by current decision-makers.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__adaptation_priority, future_generations, payer,
    powerless, civilizational, trapped, global).

% Face existential territorial loss and sovereign disappearance under warming trajectories accepted by the adaptation-priority frame. Their demands for mitigation urgency and loss-and-damage compensation are routinely subordinated to resilience-planning narratives. Receive marginal adaptation finance that cannot compensate for non-economic loss and damage.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__adaptation_priority, climate_vulnerable_small_island_states, payer,
    powerless, immediate, trapped, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_response_legitimacy__adaptation_priority, diffuse).
narrative_ontology:fixing_cost_class(climate_response_legitimacy__adaptation_priority, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates international burden-sharing for climate impacts by creating a structured finance and planning channel from wealthy nations to vulnerable regions for resilience infrastructure, disaster preparedness, and adaptive-capacity building.
% TRANSFER_FUNCTION: Moves adaptation finance, technology, and political attention from wealthy nations to vulnerable regions in the name of resilience, while simultaneously moving the costs of deferred mitigation â higher locked-in warming, uncompensated loss and damage, and exceeded adaptation limits â to low-income regions and future generations.
% ABSENT_VOICES: Future generations are structurally excluded from all negotiation frameworks. Indigenous and frontline communities with non-monetizable cosmologies and territorial claims are often excluded from adaptation planning. Degrowth advocates and mitigation-urgency scientists are marginalized in finance-dominated COP processes.
% DISAPPEARANCE_RATIONALE: If the adaptation-priority legitimacy frame vanished, wealthy nations would face immediate pressure to accept deeper mitigation obligations or structural economic transformation. Vulnerable regions would lose the fragile adaptation-finance architecture that currently exists. The UNFCCC process would reorganize around emission-reduction targets, loss-and-damage liability, or justice-oriented redistribution rather than resilience-within-warming.
% FOUNDING_PROBLEM: Climate impacts are unevenly distributed, with vulnerable populations facing existential risks they did not cause, and international coordination is needed to finance protective infrastructure and build adaptive capacity.
% FOUNDING_PROBLEM_CORROBORATION: IPCC Working Group II and vulnerable-country negotiators attest the problem remains live from outside the wealthy-nation beneficiary set. Independent civil-society audits, loss-and-damage case studies, and adaptation-finance tracking from Oxfam and CARE International corroborate that the current arrangement fails to solve it adequately.
narrative_ontology:disappearance_verdict(climate_response_legitimacy__adaptation_priority, world_rearranges).
narrative_ontology:founding_problem_status(climate_response_legitimacy__adaptation_priority, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_response_legitimacy__adaptation_priority, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(climate_response_legitimacy__adaptation_priority, 'none', 1).
narrative_ontology:epsilon_provenance(climate_response_legitimacy__adaptation_priority, 0.72, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness (0.72) is high because the constraint systematically under-delivers adaptation finance relative to need while accepting warming that locks in greater damages; the cost of preserved wealthy-nation development models is borne by trapped vulnerable regions and future generations. Suppression (0.70) reflects the active suppression of mitigation-urgency and degrowth alternatives through institutional framing, agenda-setting, and finance conditionality. Theater ratio (0.58) is elevated: adaptation pledges chronically outstrip disbursements, and resilience planning functions as performative legitimacy for wealthy nations. Accessibility collapse (0.45) is moderate â mitigation and degrowth alternatives remain intellectually available but are politically collapsed within this legitimacy frame. Resistance (0.52) reflects growing but structurally weak opposition from vulnerable-country negotiators and climate-justice movements.
 *
 * PERSPECTIVAL GAP:
 *   The wealthy-nation seat and the multilateral-institution seat should compute toward coordination (low directionality, negative effective extraction) because they administer the finance and preserve their models. The low-income-region, small-island-state, and future-generation seats should compute toward target (high directionality, amplified effective extraction) because they bear the uncompensated costs of accepted warming and finance shortfalls. The fossil-fuel-incumbent seat computes as beneficiary. The engine derives this divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Wealthy nations and fossil fuel incumbents are structural beneficiaries: they collect preserved development trajectories and extended asset viability (d near beneficiary end). Low-income regions, small-island states, and future generations are structural targets: they pay through adaptation deficits, loss and damage, and locked-in warming (d near target end). Multilateral institutions sit near symmetric but slightly toward beneficiary: they gain institutional relevance and budgets but are constrained by donor-nation priorities.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â that vulnerable populations face climate risks they did not cause â remains live, which prevents simple snare classification. However, the arrangement has drifted: the adaptation frame now serves to legitimate continued emissions and wealthy-nation development preservation. Tangled rope classification captures this hybridity: genuine coordination function (resilience finance does protect some populations) fused with asymmetric extraction (the same frame excuses inadequate mitigation and preserves the beneficiary structure). A snare reading would miss the real coordination; a rope reading would miss the extraction. The temporal measurements show rising theater and extractiveness over the interval, confirming drift rather than stable coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    adaptation_limits_at_accepted_warming,
    'At the warming levels accepted by this legitimacy frame, do hard adaptation limits render the protection of vulnerable populations impossible regardless of finance scale?',
    'IPCC assessment of adaptation limits and loss-and-damage thresholds at 2.0Â°C+ warming; empirical tracking of adaptation project failures under extreme warming events.',
    'If adaptation limits are breached, the foundational empirically_contingent axiom collapses and the constraint reclassifies toward snare (coordination story is cover for inaction). If limits remain distant, the tangled rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(adaptation_limits_at_accepted_warming, empirical, 'Whether adaptation can remain sufficient at accepted warming trajectories').

omega_variable(
    finance_gap_ambiguity,
    'Is the $350 billion adaptation finance gap a contingent policy failure or a structural feature of the wealthy-nation beneficiary structure?',
    'Historical analysis of climate finance pledge-to-delivery ratios; comparison with analogous North-South finance obligations; political-economy analysis of donor-country budget priorities.',
    'If structural, the constraint''s extraction is endogenous to the arrangement (tangled rope confirmed). If contingent, extraction is lower and the constraint may function more as scaffold or rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(finance_gap_ambiguity, conceptual, 'Whether the adaptation deficit is structural or contingent').

omega_variable(
    intergenerational_transfer_mechanism,
    'Does the adaptation-priority frame compensate future generations through resilience investment, or does it extract from them via deferred-mitigation compounding?',
    'Integrated assessment modeling comparing adaptation-spending versus mitigation-deferral cost streams; social cost of carbon intergenerational transfer analysis.',
    'If net extraction from the future, future_generations directionality moves toward full target and the constraint skews snare-like. If net compensation, intergenerational directionality moderates.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(intergenerational_transfer_mechanism, empirical, 'Net directional transfer to or from future generations').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_legitimacy__adaptation_priority, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_response_legitimacy__adaptation_priority, theater_ratio, 0, 0.2).
narrative_ontology:measurement(clim_tr_t6, climate_response_legitimacy__adaptation_priority, theater_ratio, 6, 0.28).
narrative_ontology:measurement(clim_tr_t12, climate_response_legitimacy__adaptation_priority, theater_ratio, 12, 0.38).
narrative_ontology:measurement(clim_tr_t18, climate_response_legitimacy__adaptation_priority, theater_ratio, 18, 0.46).
narrative_ontology:measurement(clim_tr_t24, climate_response_legitimacy__adaptation_priority, theater_ratio, 24, 0.52).
narrative_ontology:measurement(clim_tr_t30, climate_response_legitimacy__adaptation_priority, theater_ratio, 30, 0.58).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_response_legitimacy__adaptation_priority, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(clim_be_t6, climate_response_legitimacy__adaptation_priority, base_extractiveness, 6, 0.42).
narrative_ontology:measurement(clim_be_t12, climate_response_legitimacy__adaptation_priority, base_extractiveness, 12, 0.51).
narrative_ontology:measurement(clim_be_t18, climate_response_legitimacy__adaptation_priority, base_extractiveness, 18, 0.6).
narrative_ontology:measurement(clim_be_t24, climate_response_legitimacy__adaptation_priority, base_extractiveness, 24, 0.67).
narrative_ontology:measurement(clim_be_t30, climate_response_legitimacy__adaptation_priority, base_extractiveness, 30, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_response_legitimacy__adaptation_priority, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(clim_su_t6, climate_response_legitimacy__adaptation_priority, suppression_requirement, 6, 0.38).
narrative_ontology:measurement(clim_su_t12, climate_response_legitimacy__adaptation_priority, suppression_requirement, 12, 0.48).
narrative_ontology:measurement(clim_su_t18, climate_response_legitimacy__adaptation_priority, suppression_requirement, 18, 0.58).
narrative_ontology:measurement(clim_su_t24, climate_response_legitimacy__adaptation_priority, suppression_requirement, 24, 0.65).
narrative_ontology:measurement(clim_su_t30, climate_response_legitimacy__adaptation_priority, suppression_requirement, 30, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(climate_response_legitimacy__adaptation_priority, climate_response_legitimacy__mitigation_priority).
narrative_ontology:affects_constraint(climate_response_legitimacy__adaptation_priority, climate_response_legitimacy__degrowth_transformation).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel climate_response_legitimacy. It decomposes from the colloquial label 'legitimate climate response' into three structurally distinct claims: adaptation_priority (this file), mitigation_priority, and degrowth_transformation. Each has a different epsilon, beneficiary/victim structure, and classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
