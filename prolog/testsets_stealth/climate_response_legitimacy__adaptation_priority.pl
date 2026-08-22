% ============================================================================
% CONSTRAINT STORY: climate_response_legitimacy__adaptation_priority
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
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
    narrative_ontology:constraint_vindicates/2,
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
 *   constraint_id: climate_response_legitimacy__adaptation_priority
 *   human_readable: Adaptation-Priority Climate Response Legitimacy (Accepted Warming Trajectory)
 *   domain: climate_policy/political_economy/intergenerational_ethics
 *
 * SUMMARY:
 *   This story instantiates the adaptation_priority reading of the contested
 *   kernel climate_response_legitimacy: a legitimate climate response is one
 *   that accepts the warming trajectory and prioritizes protecting vulnerable
 *   populations through resilience infrastructure and adaptive capacity. The
 *   arrangement has a genuine coordination function — impacts are locked in,
 *   protection saves lives now, and impacts spill across borders. It also
 *   carries asymmetric extraction: accepting the trajectory preserves wealthy
 *   development models from decarbonization costs, while the adaptation
 *   finance gap (~$350B/year) leaves low-income regions and future
 *   generations bearing compounding costs the arrangement defers but does not
 *   remove. The sibling readings (mitigation_priority,
 *   degrowth_transformation) are separate constraints; the contest among
 *   readings is routed to omega variables, not folded into this story's
 *   classification. Claim and metrics are authored independently: the claimed
 *   type is tangled_rope and the metrics describe substantially extractive
 *   operation — the engine computes per-seat classifications from the
 *   structural data, and any divergence between claim and computed type is
 *   the measurement the corpus exists to take.
 *
 * KEY AGENTS:
 *   - wealthy_industrial_nations: agenda_setter and primary beneficiary (institutional/arbitrage) — sets adaptation finance terms, preserves development model under accepted trajectory
 *   - low_income_regions: primary payer (organized/trapped) — bears sharpest impacts, faces ~$350B/year adaptation finance gap
 *   - climate_frontline_communities: payer and partial beneficiary (powerless/constrained) — intended protection recipients, absorb the deficit directly
 *   - future_generations: payer (powerless/trapped) — inherit compounded warming, no seat in any negotiation
 *   - fossil_fuel_exporting_states: beneficiary (institutional/arbitrage) — accepted trajectory preserves fuel demand and sovereign wealth
 *   - adaptation_engineering_sector: beneficiary (organized/mobile) — resilience infrastructure as a growth market
 *   - mitigation_advocacy_coalition: excluded (organized/constrained) — priority ordering defined outside this reading's legitimacy criterion
 *   - ipcc_assessment_body: analytical observer — independently corroborates both the founding problem and the deficit
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_legitimacy__adaptation_priority, 0.66).
domain_priors:suppression_score(climate_response_legitimacy__adaptation_priority, 0.52).
domain_priors:theater_ratio(climate_response_legitimacy__adaptation_priority, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_legitimacy__adaptation_priority, extractiveness, 0.66).
narrative_ontology:constraint_metric(climate_response_legitimacy__adaptation_priority, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(climate_response_legitimacy__adaptation_priority, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_response_legitimacy__adaptation_priority, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(climate_response_legitimacy__adaptation_priority, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_legitimacy__adaptation_priority, tangled_rope).
narrative_ontology:human_readable(climate_response_legitimacy__adaptation_priority, "Adaptation-Priority Climate Response Legitimacy (Accepted Warming Trajectory)").
narrative_ontology:topic_domain(climate_response_legitimacy__adaptation_priority, "climate_policy/political_economy/intergenerational_ethics").

domain_priors:requires_active_enforcement(climate_response_legitimacy__adaptation_priority).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_legitimacy__adaptation_priority, 'c654b2dc-9ded-44da-a792-5a11f94a0e83').
narrative_ontology:cs_kernel_codification('c654b2dc-9ded-44da-a792-5a11f94a0e83', distributed).
narrative_ontology:cs_authority_grounding('c654b2dc-9ded-44da-a792-5a11f94a0e83', distributed).
narrative_ontology:cs_reading_relation('c654b2dc-9ded-44da-a792-5a11f94a0e83', climate_response_legitimacy__mitigation_priority, influences).
narrative_ontology:cs_reading_relation('c654b2dc-9ded-44da-a792-5a11f94a0e83', climate_response_legitimacy__degrowth_transformation, coexists_with).
narrative_ontology:cs_axiom('c654b2dc-9ded-44da-a792-5a11f94a0e83', foundational, impact_protection_first_obligation).
narrative_ontology:cs_axiom_status(impact_protection_first_obligation, holdable).
narrative_ontology:cs_axiom_grounding('c654b2dc-9ded-44da-a792-5a11f94a0e83', impact_protection_first_obligation, deontological).
narrative_ontology:cs_axiom('c654b2dc-9ded-44da-a792-5a11f94a0e83', foundational, warming_trajectory_effectively_fixed).
narrative_ontology:cs_axiom_status(warming_trajectory_effectively_fixed, holdable).
narrative_ontology:cs_axiom_grounding('c654b2dc-9ded-44da-a792-5a11f94a0e83', warming_trajectory_effectively_fixed, empirically_contingent).
narrative_ontology:cs_reference_frame('c654b2dc-9ded-44da-a792-5a11f94a0e83', impact_protection_primacy).
narrative_ontology:cs_drift_state('c654b2dc-9ded-44da-a792-5a11f94a0e83', contemporary_adaptation_gap_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('c654b2dc-9ded-44da-a792-5a11f94a0e83', '').
narrative_ontology:cs_kernel_id(climate_response_legitimacy__adaptation_priority, climate_response_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_legitimacy__adaptation_priority, wealthy_industrial_nations).
narrative_ontology:constraint_beneficiary(climate_response_legitimacy__adaptation_priority, fossil_fuel_exporting_states).
narrative_ontology:constraint_beneficiary(climate_response_legitimacy__adaptation_priority, adaptation_engineering_sector).
narrative_ontology:constraint_victim(climate_response_legitimacy__adaptation_priority, low_income_regions).
narrative_ontology:constraint_victim(climate_response_legitimacy__adaptation_priority, climate_frontline_communities).
narrative_ontology:constraint_victim(climate_response_legitimacy__adaptation_priority, future_generations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(climate_response_legitimacy__adaptation_priority, climate_frontline_communities).
narrative_ontology:constraint_vindicates(climate_response_legitimacy__adaptation_priority, protection_over_prevention_doctrine).
narrative_ontology:constraint_vindicates(climate_response_legitimacy__adaptation_priority, warming_trajectory_irreversibility).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Dominate the negotiation forums, fund governance bodies, and bilateral channels through which adaptation finance is pledged, conditioned, and disbursed; decide how much protection funding flows and on what terms. Under the accepted warming trajectory they face no binding constraint on their development model: growth continues, energy demand is met as before, and the costs of rapid decarbonization are never incurred. Exiting the arrangement would mean voluntarily adopting binding mitigation targets at high near-term cost; instead they fund adaptation selectively while emissions continue, and can shift residual costs through border adjustments, insurance, and domestic hardening.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__adaptation_priority, wealthy_industrial_nations, agenda_setter,
    institutional, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(climate_response_legitimacy__adaptation_priority, wealthy_industrial_nations, beneficiary).

% Negotiate as a bloc (G77, AOSIS, African Group) for adaptation finance but hold little unilateral leverage. They face the sharpest impacts of the accepted trajectory — drought, flooding, heat extremes, coastal loss — with assessed adaptation needs around $350B per year against delivered finance roughly an order of magnitude smaller. They cannot exit the climate, refusing the finance deepens their exposure, and outward migration is bounded by wealthy-nation immigration regimes. Their position inside the arrangement is accepting underfunded protection because the alternative is no protection at all.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__adaptation_priority, low_income_regions, payer,
    organized, biographical, trapped, regional).

% Are the named recipients of protection: sea defenses, early-warning systems, climate-resilient agriculture and water infrastructure. They receive real protection where projects arrive and are funded, and absorb the residual loss where the finance gap bites — late, partial, or absent defenses. Some hold land, livelihoods, and heritage ties that make relocation costly or impossible; others migrate under distress. They bear the difference between promised and delivered protection directly.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__adaptation_priority, climate_frontline_communities, payer,
    powerless, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(climate_response_legitimacy__adaptation_priority, climate_frontline_communities, beneficiary).

% Hold no seat in any negotiation and are represented only by advocacy proxies and constitutional litigation. They inherit the warming the accepted trajectory compounds: every year of continued emissions raises the adaptation burden they will carry and narrows the options left to them. No protection flow reaches them within the current interval; what reaches them is accumulated exposure.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__adaptation_priority, future_generations, payer,
    powerless, civilizational, trapped, global).

% Benefit from an accepted warming trajectory because it preserves demand for exported fuels and avoids stranding sovereign wealth. They support adaptation-centered framing in negotiations — it displaces binding mitigation commitments — and fund visible adaptation projects as goodwill signaling while expanding production. Their exit would mean accepting demand destruction for their primary revenue source; instead they move between adaptation diplomacy and expanded output.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__adaptation_priority, fossil_fuel_exporting_states, beneficiary,
    institutional, generational, arbitrage, global).

% Supplies the resilience build-out: sea walls, climate-proofed grids and ports, hydrological engineering, risk modeling, and consulting. Revenue scales with adaptation finance flows and with each new impact assessment that raises projected need. Income does not depend on protection reaching the most exposed communities, only on projects being commissioned; capital and staff move to wherever the next funding window opens.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__adaptation_priority, adaptation_engineering_sector, beneficiary,
    organized, biographical, mobile, global).

% Scientists turned advocates, climate justice movements, small island state delegates pressing for both adaptation and steep cuts, and litigation campaigns. They argue emissions reduction must lead and that adaptation without mitigation accepts escalating harm. Within this reading's framework their priority ordering is defined as outside what a legitimate response is — they participate in the process, but the arrangement's legitimacy criterion excludes their core claim, and the resources they compete for are allocated by the other seats.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__adaptation_priority, mitigation_advocacy_coalition, excluded,
    organized, generational, constrained, global).

% Produces the periodic assessments of impacts, adaptation needs, and mitigation gaps that all other seats cite. Its working-group reports independently corroborate both the founding problem (impacts are real, growing, and partially locked in) and the deficit (adaptation finance falls far short of assessed need). It holds no allocation power and no enforcement capacity; it documents the structure within which the other seats operate.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__adaptation_priority, ipcc_assessment_body, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_response_legitimacy__adaptation_priority, wealthy_industrial_nations).
narrative_ontology:fixing_cost_class(climate_response_legitimacy__adaptation_priority, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates protection against climate impacts that are already locked in and worsening: pooling adaptation finance, directing resilience infrastructure (sea defenses, early-warning systems, climate-resilient agriculture and water systems) toward the most exposed populations, and building adaptive capacity where local resources cannot. This solves a real collective-action problem — impacts cross borders, and unprotected regions generate spillovers (migration, instability, disease) that no region escapes alone.
% TRANSFER_FUNCTION: Moves adaptation finance from wealthy-nation budgets toward resilience projects in exposed regions — at roughly an order of magnitude below assessed need (~$350B/year gap) — while moving the costs of the accepted warming trajectory (unmitigated emissions) onto low-income regions and future generations, and preserving wealthy development models from the costs of rapid decarbonization.
% ABSENT_VOICES: Future generations hold no seat and are represented only by advocacy proxies. Mitigation-first advocates and degrowth proponents are present in the process but their priority orderings are defined as outside this reading's legitimacy criterion. The ecosystems and nonhuman interests bearing warming impacts have no representation at all.
% DISAPPEARANCE_RATIONALE: If the arrangement vanished overnight, hundreds of millions in exposed regions would lose funded protection (early warning, defenses, resilient agriculture) with nothing replacing it; wealthy nations would face immediate pressure to either adopt binding mitigation or abandon the vulnerable openly; the adaptation finance architecture (funds, pledges, national plans) would collapse and climate negotiation would reorganize around the sibling readings' competing legitimacy claims.
% FOUNDING_PROBLEM: Warming already underway and partially locked in regardless of mitigation; vulnerable populations lack the resources to protect themselves from impacts they did little to cause; a response framework was needed for the harms that prevention could no longer avert, and for the decades-long lag before any mitigation pathway would reduce impacts.
% FOUNDING_PROBLEM_CORROBORATION: IPCC Working Group II impact and adaptation assessments, UNEP Adaptation Gap Reports, and frontline community testimony corroborate the founding problem from outside the benefiting parties: impacts are real, growing, and adaptation finance falls far short of assessed need. No corroborator attests that accepting the trajectory is the necessary solution — that step is contested by the sibling readings — but the problem itself is independently attested.
narrative_ontology:disappearance_verdict(climate_response_legitimacy__adaptation_priority, world_rearranges).
narrative_ontology:founding_problem_status(climate_response_legitimacy__adaptation_priority, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_response_legitimacy__adaptation_priority, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(climate_response_legitimacy__adaptation_priority, 'none', 1).
narrative_ontology:epsilon_provenance(climate_response_legitimacy__adaptation_priority, 0.66, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness 0.66 reflects a real but underfunded protection flow set against a large deferred-cost transfer: the arrangement does protect people (the coordination component) while its trajectory acceptance shifts compounding costs onto those with least capacity (the extraction component). Suppression 0.52 is structural rather than coercive: alternatives are crowded out through agenda control, finance conditionality, and framing rather than prohibition. Theater 0.45: a substantial share of adaptation activity is pledge-and-report cycles, pilot projects, and finance that does not reach frontline delivery, though real infrastructure is built. Accessibility_collapse 0.38: alternatives (mitigation-first, degrowth) remain live and contested — they are the sibling readings of this kernel. Resistance 0.62: mitigation advocates, climate justice movements, small island states, and litigation campaigns actively contest the trajectory acceptance. All three tracked series run on one shared grid (T=0,5,10,15,20,25): extractiveness rises as locked-in warming compounds and the gap persists; theater rises as pledge-delivery gaps widen; suppression_requirement rises because holding the adaptation frame requires progressively more active defense as impacts worsen and loss-and-damage claims extend beyond adaptation's scope.
 *
 * PERSPECTIVAL GAP:
 *   The agenda_setter seat and the payer seats should compute differently. From wealthy_industrial_nations' position the arrangement is responsible stewardship: they fund protection, they read adaptation as pragmatic generosity, and the accepted trajectory reads as realism about locked-in warming. From low_income_regions' seat the same structure is underfunded protection that leaves them absorbing compounding impacts while the wealthy development model persists — protection substituted for prevention they never agreed to. From future_generations' seat it is pure deferral with compounding interest. The engine computes this divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   wealthy_industrial_nations sit near the beneficiary end: the arrangement subsidizes them (no binding mitigation constraint) and they control its terms, though they bear some adaptation finance cost, keeping d slightly above the pure-beneficiary floor. fossil_fuel_exporting_states and adaptation_engineering_sector are beneficiaries with arbitrage and mobile exit respectively. low_income_regions and climate_frontline_communities sit near the target end: they bear the trajectory's costs and the deficit; frontline communities receive real protection flows, which damps their directionality below the pure-target end but leaves them net targets. future_generations sit at the full-target end: trapped, powerless, with no protection flow reaching them in the interval — only accumulated exposure. Spatial scope is global for the arrangement as a whole, which scales effective extraction upward for the trapped target seats because verification of finance delivery across jurisdictions is hard.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification prevents misreading in both directions. Reading the arrangement as a snare would dismiss genuine protection flows that save lives now — early warning and defenses demonstrably reduce mortality, and the founding problem (locked-in impacts) is independently corroborated as live. Reading it as a rope would obscure the extraction: the trajectory acceptance is a choice, not a given, and its costs compound on those with least capacity. The founding problem is live, so no mandatrophy resolution applies — the mandate has not outlived its function; rather, the arrangement's form (underfunded, trajectory-accepting) serves the agenda_setter's interests disproportionately while performing protection. If the adaptation gap closed and the trajectory were no longer treated as fixed, the coordination component would dominate and extraction would fall toward coordination cost.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'Is the adaptation_priority reading the correct instantiation of the climate_response_legitimacy kernel, or do the mitigation_priority or degrowth_transformation readings better capture what makes a climate response legitimate?',
    'Comparative structural analysis across the three sibling stories: track each reading''s victim/beneficiary set and extractiveness over a common interval; the reading whose victim set best matches independently assessed burden distribution (IPCC vulnerability attribution) gains support.',
    'If mitigation_priority is adopted, this reading''s victim set shrinks (the trajectory is no longer accepted) and its extraction falls toward coordination cost; if degrowth_transformation is adopted, wealthy consumption itself enters the target set and the adaptation frame becomes a component rather than the frame. This story''s classification is valid only within its own reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'This constraint is one reading of a contested kernel; classification is reading-indexed.').

omega_variable(
    warming_trajectory_fixity,
    'Is the warming trajectory this reading accepts genuinely fixed (locked in regardless of action), or does rapid mitigation still materially alter it — the empirical premise on which trajectory acceptance rests?',
    'Integrated assessment of committed warming versus achievable near-term mitigation pathways; compare impact trajectories under immediate-deep-cut scenarios against the accepted-trajectory scenario.',
    'If the trajectory is materially alterable, the acceptance premise fails, the arrangement''s deferred costs become a choice rather than a given, and this reading collapses toward mitigation_priority with the victim set expanding immediately.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(warming_trajectory_fixity, empirical, 'Empirical premise of trajectory acceptance: is the accepted warming actually locked in?').

omega_variable(
    adaptation_deficit_scale,
    'What is the true scale of the adaptation finance gap — assessed need versus delivered finance — and does the ~$350B/year estimate understate or overstate it?',
    'Independent audit of adaptation finance flows (UNEP Adaptation Gap methodology plus grassroots delivery tracking), separating grants from debt-creating instruments.',
    'If the gap is closing, the coordination component strengthens and extraction falls; if it is widening, the arrangement trends toward pure extraction — protection rhetoric operating as cover for continued emissions.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(adaptation_deficit_scale, empirical, 'Scale and trajectory of the adaptation finance gap.').

omega_variable(
    finance_delivery_capture,
    'Does adaptation finance reach frontline communities, or is it captured by intermediaries — contractors, consultants, national elites, debt service?',
    'Disbursement-to-community tracking studies; compare fund flows at allocation versus delivery points; measure the share reaching local adaptation outcomes.',
    'If capture is high, the coordination function is hollow at the delivery end, the theater ratio understates dysfunction, and the arrangement moves toward pure extraction with identifiable capturers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(finance_delivery_capture, empirical, 'Whether protection resources reach the protected.').

omega_variable(
    intergenerational_deferral_weighting,
    'How should the arrangement''s deferral of compounding costs onto future generations be weighed against present protection benefits — does the deferral make the arrangement net-extractive across generations even if net-protective within them?',
    'Explicit intergenerational accounting: apply competing discount-rate and rights-based frameworks to the arrangement''s cost stream; the classification is preference-dependent.',
    'Under strong intergenerational weighting, the deferred compounded warming dominates and the arrangement trends toward pure extraction across the generational boundary; under weak weighting, present protection dominates and the hybrid coordination reading holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intergenerational_deferral_weighting, preference, 'Value-dependence of the intergenerational cost deferral.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_legitimacy__adaptation_priority, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_response_legitimacy__adaptation_priority, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(clim_tr_t0, observed).
narrative_ontology:measurement(clim_tr_t5, climate_response_legitimacy__adaptation_priority, theater_ratio, 5, 0.32).
narrative_ontology:measurement_basis(clim_tr_t5, observed).
narrative_ontology:measurement(clim_tr_t10, climate_response_legitimacy__adaptation_priority, theater_ratio, 10, 0.35).
narrative_ontology:measurement_basis(clim_tr_t10, observed).
narrative_ontology:measurement(clim_tr_t15, climate_response_legitimacy__adaptation_priority, theater_ratio, 15, 0.39).
narrative_ontology:measurement_basis(clim_tr_t15, observed).
narrative_ontology:measurement(clim_tr_t20, climate_response_legitimacy__adaptation_priority, theater_ratio, 20, 0.42).
narrative_ontology:measurement_basis(clim_tr_t20, observed).
narrative_ontology:measurement(clim_tr_t25, climate_response_legitimacy__adaptation_priority, theater_ratio, 25, 0.45).
narrative_ontology:measurement_basis(clim_tr_t25, observed).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_response_legitimacy__adaptation_priority, base_extractiveness, 0, 0.5).
narrative_ontology:measurement_basis(clim_be_t0, observed).
narrative_ontology:measurement(clim_be_t5, climate_response_legitimacy__adaptation_priority, base_extractiveness, 5, 0.53).
narrative_ontology:measurement_basis(clim_be_t5, observed).
narrative_ontology:measurement(clim_be_t10, climate_response_legitimacy__adaptation_priority, base_extractiveness, 10, 0.57).
narrative_ontology:measurement_basis(clim_be_t10, observed).
narrative_ontology:measurement(clim_be_t15, climate_response_legitimacy__adaptation_priority, base_extractiveness, 15, 0.6).
narrative_ontology:measurement_basis(clim_be_t15, observed).
narrative_ontology:measurement(clim_be_t20, climate_response_legitimacy__adaptation_priority, base_extractiveness, 20, 0.64).
narrative_ontology:measurement_basis(clim_be_t20, observed).
narrative_ontology:measurement(clim_be_t25, climate_response_legitimacy__adaptation_priority, base_extractiveness, 25, 0.66).
narrative_ontology:measurement_basis(clim_be_t25, observed).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_response_legitimacy__adaptation_priority, suppression_requirement, 0, 0.42).
narrative_ontology:measurement_basis(clim_su_t0, observed).
narrative_ontology:measurement(clim_su_t5, climate_response_legitimacy__adaptation_priority, suppression_requirement, 5, 0.46).
narrative_ontology:measurement_basis(clim_su_t5, observed).
narrative_ontology:measurement(clim_su_t10, climate_response_legitimacy__adaptation_priority, suppression_requirement, 10, 0.5).
narrative_ontology:measurement_basis(clim_su_t10, observed).
narrative_ontology:measurement(clim_su_t15, climate_response_legitimacy__adaptation_priority, suppression_requirement, 15, 0.55).
narrative_ontology:measurement_basis(clim_su_t15, observed).
narrative_ontology:measurement(clim_su_t20, climate_response_legitimacy__adaptation_priority, suppression_requirement, 20, 0.6).
narrative_ontology:measurement_basis(clim_su_t20, observed).
narrative_ontology:measurement(clim_su_t25, climate_response_legitimacy__adaptation_priority, suppression_requirement, 25, 0.64).
narrative_ontology:measurement_basis(clim_su_t25, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_response_legitimacy__adaptation_priority, resource_allocation).
narrative_ontology:affects_constraint(climate_response_legitimacy__adaptation_priority, climate_response_legitimacy__mitigation_priority).
narrative_ontology:affects_constraint(climate_response_legitimacy__adaptation_priority, climate_response_legitimacy__degrowth_transformation).

% DUAL FORMULATION NOTE:
% The colloquial concept 'legitimate climate response' decomposes into three structurally distinct constraints — one per kernel reading — because each reading instantiates a different beneficiary/victim structure and a different epsilon: adaptation_priority (this story: victims are low-income regions via the adaptation deficit and future generations via compounded warming; beneficiaries are wealthy development models), mitigation_priority (victims are present consumers via carbon costs; beneficiaries are future generations and clean-technology sectors), and degrowth_transformation (victims are wealthy-nation consumption itself; beneficiaries are the global South and future generations). Each story carries its own stable epsilon; the family link enables contamination analysis — erosion of the mitigation reading's carbon budget raises this reading's extractiveness, and this reading's preservation of the growth model enlarges the degrowth reading's required scope.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
