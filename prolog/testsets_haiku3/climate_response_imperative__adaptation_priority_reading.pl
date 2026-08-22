% ============================================================================
% CONSTRAINT STORY: climate_response_imperative__adaptation_priority_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_response_imperative__adaptation_priority_reading, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: climate_response_imperative__adaptation_priority_reading
 *   human_readable: Climate Response as Adaptation-Priority Imperative (Resilience Reading)
 *   domain: climate_policy/political_economy/intergenerational_justice
 *
 * SUMMARY:
 *   This constraint instantiates ONE READING of the contested kernel
 *   'climate_response_imperative'. The adaptation-priority reading declares
 *   that climate response is fundamentally about resilience-building and
 *   damage reduction in exposed regions, with mitigation as a long-term
 *   aspiration deferred to wealthier nations at their chosen pace. This
 *   reading creates a tangled structure: it coordinates a real adaptation
 *   function (mobilizing capital for climate impacts on vulnerable regions)
 *   while simultaneously extracting from those regions by assigning them the
 *   costs of impacts they did not cause, financing those costs through debt
 *   mechanisms, and indefinitely deferring the emissions reductions that
 *   would limit future impacts. The constraint is not a false mountain
 *   (adaptation is necessary) but rather a selective operationalization of
 *   climate science that marginalizes the evidence on mitigation urgency and
 *   adaptation limits at higher warming. The committer frame traces this
 *   reading through omega variables (Rule 2) and cs_structure (Rule 4) rather
 *   than embedding it in the base narrative.
 *
 * KEY AGENTS:
 *   - Wealthy coastal nations (agenda-setter, beneficiary via avoided mitigation costs)
 *   - Developing vulnerable nations (payer, bearing immediate and intergenerational costs)
 *   - Adaptation finance intermediaries (beneficiary via institutional growth and fees)
 *   - Engineering contractors (beneficiary via procurement contracts)
 *   - Future generations (payer, inheriting constrained adaptation capacity)
 *   - Mitigation advocates (excluded, marginalized in policy priority-setting)
 *   - Climate science community (observer, producing evidence on adequacy/limits)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_imperative__adaptation_priority_reading, 0.78).
domain_priors:suppression_score(climate_response_imperative__adaptation_priority_reading, 0.71).
domain_priors:theater_ratio(climate_response_imperative__adaptation_priority_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_imperative__adaptation_priority_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(climate_response_imperative__adaptation_priority_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(climate_response_imperative__adaptation_priority_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_response_imperative__adaptation_priority_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(climate_response_imperative__adaptation_priority_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_imperative__adaptation_priority_reading, tangled_rope).
narrative_ontology:human_readable(climate_response_imperative__adaptation_priority_reading, "Climate Response as Adaptation-Priority Imperative (Resilience Reading)").
narrative_ontology:topic_domain(climate_response_imperative__adaptation_priority_reading, "climate_policy/political_economy/intergenerational_justice").

domain_priors:requires_active_enforcement(climate_response_imperative__adaptation_priority_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_imperative__adaptation_priority_reading, '9e3a9243-8e38-474f-af75-302d370cd483').
narrative_ontology:cs_kernel_codification('9e3a9243-8e38-474f-af75-302d370cd483', distributed).
narrative_ontology:cs_authority_grounding('9e3a9243-8e38-474f-af75-302d370cd483', distributed).
narrative_ontology:cs_reading_relation('9e3a9243-8e38-474f-af75-302d370cd483', climate_response_imperative__mitigation_priority_reading, influences).
narrative_ontology:cs_reading_relation('9e3a9243-8e38-474f-af75-302d370cd483', climate_response_imperative__degrowth_reading, influences).
narrative_ontology:cs_axiom('9e3a9243-8e38-474f-af75-302d370cd483', foundational, adaptation_sufficient_within_present_economic_structure).
narrative_ontology:cs_axiom_status(adaptation_sufficient_within_present_economic_structure, holdable).
narrative_ontology:cs_axiom_grounding('9e3a9243-8e38-474f-af75-302d370cd483', adaptation_sufficient_within_present_economic_structure, empirically_contingent).
narrative_ontology:cs_axiom('9e3a9243-8e38-474f-af75-302d370cd483', foundational, mitigation_is_long_term_aspiration_not_immediate_imperative).
narrative_ontology:cs_axiom_status(mitigation_is_long_term_aspiration_not_immediate_imperative, holdable).
narrative_ontology:cs_axiom_grounding('9e3a9243-8e38-474f-af75-302d370cd483', mitigation_is_long_term_aspiration_not_immediate_imperative, instrumental).
narrative_ontology:cs_reference_frame('9e3a9243-8e38-474f-af75-302d370cd483', current_climate_impacts_immediate_adaptation_necessity).
narrative_ontology:cs_drift_state('9e3a9243-8e38-474f-af75-302d370cd483', contemporary_escalating_impacts, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('9e3a9243-8e38-474f-af75-302d370cd483', '').
narrative_ontology:cs_kernel_id(climate_response_imperative__adaptation_priority_reading, climate_response_imperative).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_imperative__adaptation_priority_reading, wealthy_coastal_nations).
narrative_ontology:constraint_beneficiary(climate_response_imperative__adaptation_priority_reading, adaptation_finance_intermediaries).
narrative_ontology:constraint_beneficiary(climate_response_imperative__adaptation_priority_reading, engineering_contractors).
narrative_ontology:constraint_victim(climate_response_imperative__adaptation_priority_reading, developing_vulnerable_nations).
narrative_ontology:constraint_victim(climate_response_imperative__adaptation_priority_reading, future_generations).
narrative_ontology:constraint_victim(climate_response_imperative__adaptation_priority_reading, climate_displaced_populations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(climate_response_imperative__adaptation_priority_reading, high_emissions_corporations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Wealthy industrialized nations frame climate response as adaptation and resilience-building in exposed regions, de-prioritizing the emissions reductions that would require domestic economic restructuring. They deploy adaptation finance (at concessional rates, often debt-financed) as the primary response, positioning themselves as aid providers rather than emissions debtors. They set the international agenda through conference negotiations and funding mechanisms.
narrative_ontology:constraint_stakeholder(climate_response_imperative__adaptation_priority_reading, wealthy_coastal_nations, agenda_setter,
    institutional, biographical, arbitrage, global).

% Bear the heaviest immediate costs of climate impacts (flooding, drought, sea-level rise) despite minimal historical emissions responsibility. Forced to divert scarce capital to adaptation infrastructure (seawalls, irrigation, relocation) rather than development. Accept adaptation finance on terms set by donors, often structured as loans that lock economies into debt servicing and external dependencies. Cannot exit the climate-exposed geography; cannot reject the finance without facing immediate humanitarian crisis.
narrative_ontology:constraint_stakeholder(climate_response_imperative__adaptation_priority_reading, developing_vulnerable_nations, payer,
    powerless, civilizational, trapped, global).

% Multilateral development banks, bilateral aid agencies, and climate funds administer adaptation finance, capturing administrative fees, technical assistance margins, and institutional growth. They benefit from the framing that adaptation is the primary response: a stable, financeable, project-based approach that generates institutional demand for their services. Their staffing and budgets depend on the volume of adaptation projects.
narrative_ontology:constraint_stakeholder(climate_response_imperative__adaptation_priority_reading, adaptation_finance_intermediaries, beneficiary,
    institutional, biographical, mobile, global).

% Build adaptation infrastructure (coastal defenses, water systems, climate-resilient agriculture infrastructure) under contracts funded by adaptation finance. Receive steady procurement streams from an expanding adaptation sector. Have no stake in whether mitigation succeeds or fails; their market depends on adaptation remaining the primary response.
narrative_ontology:constraint_stakeholder(climate_response_imperative__adaptation_priority_reading, engineering_contractors, beneficiary,
    powerful, biographical, mobile, global).

% Inherit a world with higher baseline temperatures, more extreme climate variability, and depleted adaptation capacity in vulnerable regions. The adaptation-priority framing defers emissions reductions that could have limited warming, leaving future generations to adapt to a warmer baseline than necessary. They have no voice in present-day negotiations.
narrative_ontology:constraint_stakeholder(climate_response_imperative__adaptation_priority_reading, future_generations, payer,
    powerless, civilizational, trapped, universal).

% Experience forced migration from climate impacts (sea-level rise, desertification, extreme weather) that adaptation infrastructure fails to prevent or manage. Adaptation projects often displace local populations themselves (dam building, coastal relocation). Have no meaningful participation in adaptation planning decisions that affect them.
narrative_ontology:constraint_stakeholder(climate_response_imperative__adaptation_priority_reading, climate_displaced_populations, payer,
    powerless, biographical, constrained, global).

% Climate scientists, environmental organizations, and developing-world advocates argue that adaptation cannot succeed at warming above 1.5–2°C without aggressive mitigation; the adaptation-priority framing marginalizes their evidence and recommendations by defining them as expensive, disruptive, and secondary. They contest the policy frame but lack the institutional power to reorder the agenda.
narrative_ontology:constraint_stakeholder(climate_response_imperative__adaptation_priority_reading, mitigation_advocates, excluded,
    organized, civilizational, constrained, global).

% Benefit implicitly from the adaptation-priority framing: emissions reduction in industrialized economies would require rapid decarbonization of energy, transport, and agriculture, which would constrain their operations. An adaptation-focused response allows continued high-emission business models while framing climate response as infrastructure investment in the Global South.
narrative_ontology:constraint_stakeholder(climate_response_imperative__adaptation_priority_reading, high_emissions_corporations, beneficiary,
    institutional, biographical, mobile, global).

% Produces evidence on climate impacts, mitigation feasibility, and adaptation limits. The adaptation-priority reading operationalizes a subset of that evidence (impacts on vulnerable regions, adaptation benefits) while marginalizing other evidence (emissions trajectories, adaptation limits at higher warming, feedback mechanisms). Observers can measure the frame's fidelity to the full body of evidence.
narrative_ontology:constraint_stakeholder(climate_response_imperative__adaptation_priority_reading, climate_science_community, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_response_imperative__adaptation_priority_reading, wealthy_coastal_nations).
narrative_ontology:fixing_cost_class(climate_response_imperative__adaptation_priority_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a global response to climate impacts by mobilizing adaptation finance, technology transfer, and infrastructure investment in vulnerable regions. Solves the collective-action problem of funding resilience in nations that lack domestic capital for climate adaptation while facing immediate hazards.
% TRANSFER_FUNCTION: Moves capital from wealthy nations to developing-vulnerable nations as adaptation finance (grants, concessional loans), generating fees for intermediaries and procurement for contractors. Simultaneously transfers responsibility for climate response from high-emitting nations (historical and present-day) to the nations bearing the worst immediate impacts.
% ABSENT_VOICES: Mitigation advocates (climate scientists, environmental organizations, developing-world youth movements, future-generation proxies) argue the adaptation-priority framing is scientifically inadequate and perpetuates the injustice of placing response costs on those least responsible for emissions. They are present in technical forums but structurally marginalized in policy priority-setting and funding allocation.
% DISAPPEARANCE_RATIONALE: If this framing vanished and global climate policy shifted to mitigation-priority (rapid emissions reductions) or degrowth (structural economic transformation), capital flows would reorient from adaptation infrastructure to clean energy transition and redistribution, institutional mandates would shift from adaptation finance to emissions accountability, and the climate-displaced population trajectory would change shape. The constraint shapes which solutions are funded and which are deferred.
% FOUNDING_PROBLEM: Wealthy nations historically emitted most carbon; today's impacts fall on vulnerable nations and populations with minimal historical responsibility. Adaptation was framed as the urgent response to solve immediate crises while wealthy nations pursued mitigation at their chosen pace.
% FOUNDING_PROBLEM_CORROBORATION: Wealthy-nation governments and adaptation finance institutions attest the founding problem as live and adaptation as the appropriate response. Climate scientists and developing-nation delegations attest that the founding problem was real but the solution is inadequate: adaptation alone cannot prevent catastrophic warming; the problem requires rapid mitigation, which the adaptation-priority framing indefinitely defers. IPCC Special Reports and scientific consensus documents (from outside the agenda-setting seat) support the contested verdict.
narrative_ontology:disappearance_verdict(climate_response_imperative__adaptation_priority_reading, world_rearranges).
narrative_ontology:founding_problem_status(climate_response_imperative__adaptation_priority_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_response_imperative__adaptation_priority_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(climate_response_imperative__adaptation_priority_reading, 'none', 1).
narrative_ontology:epsilon_provenance(climate_response_imperative__adaptation_priority_reading, 0.78, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_response_imperative__adaptation_priority_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_response_imperative__adaptation_priority_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_response_imperative__adaptation_priority_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness reaches 0.78 by interval end because the constraint transfers climate adaptation costs to nations bearing the least responsibility for emissions, funds those transfers through debt mechanisms that create long-term dependencies, and indefinitely defers the emissions reductions that would reduce future adaptation demand. The constraint has a real coordination function (mobilizing adaptation finance), which is why it is Tangled Rope, not pure Snare. But the extraction is substantial and active: wealthy nations must enforce the prioritization (defending adaptation-first framing in climate negotiations, allocating mitigation spending to adaptation, conditioning finance on recipient adoption of adaptation projects) to prevent mitigation advocates from reordering the agenda. Suppression is high (0.71) because the constraint's persistence depends on marginalizing climate science showing adaptation alone cannot prevent catastrophic warming, suppressing the political salience of emissions accountability, and constraining exit for developing nations (trapped in climate-exposed geography, dependent on adaptation finance, unable to fund mitigation themselves). Theater is moderate-high (0.48): adaptation projects are real and necessary, but a growing share of institutional activity defends the framing (downplaying mitigation research, funding adaptation studies over emissions analysis, celebrating adaptation wins while global emissions rise) rather than solving the underlying problem. The measurement series show extractiveness accumulating over time (0.62→0.78 across the interval) as climate impacts worsen, making adaptation more urgent and developing nations more dependent on adaptation finance at wealthier nations' terms. Suppression stabilizes (0.61→0.71, plateau) because marginalizing mitigation evidence reaches saturation—no institutional investment can further suppress climate science without explicit denial, which is increasingly costly.
 *
 * PERSPECTIVAL GAP:
 *   From the wealthy-nation / adaptation-finance seat: the constraint is genuine coordination—a pragmatic response to the climate crisis, immediate aid to vulnerable regions, infrastructure investment that saves lives. From the developing-nation / future-generation seat: the same constraint is enforced extraction—a perpetuation of historical injustice, a debt trap, an indefinite deferral of the responsibility that wealthy nations incurred through historical emissions. The engine computes these divergent classifications from the structural data: high directionality (d~0.9) for developing nations (trapped, powerless, bearing costs) versus moderate-high directionality (d~0.35–0.45) for wealthy nations (institutional power, arbitrage exit options, benefits from avoided mitigation costs). The perspectival gap is not a measurement error; it is the central structural fact—the constraint looks like Rope from one seat and Snare from another.
 *
 * DIRECTIONALITY LOGIC:
 *   Wealthy coastal nations and adaptation finance intermediaries occupy the beneficiary/agenda-setter positions: they set the agenda, finance adaptation on their terms, and avoid the domestic disruption that mitigation would require. Their d is low (0.2–0.3): they are subsidized by the constraint. Developing vulnerable nations are the structural targets: they bear the climate impacts, must pay for adaptation through debt, and are excluded from reordering the agenda. Their d is high (0.85–0.95): they are extracted from via immediate capital requirements, debt servicing, and indefinite deferral of mitigation that would reduce future adaptation demand. Future generations sit at extreme d (approaching 1.0): they inherit a warmer world and depleted adaptation resources, bearing costs from decisions made by present-day agenda-setters. The extracted surplus flows to wealthy-nation governments (avoided mitigation costs), adaptation finance intermediaries (institutional growth), and engineering contractors (procurement). High-emissions corporations benefit implicitly by avoiding the energy, transport, and agricultural transformation that rapid mitigation would require.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's founding problem (wealthy nations' historical emissions created unequal climate impacts; vulnerable nations lack capital for adaptation) is contested but alive. The constraint's mandate is simultaneously coordination (mobilize adaptation finance) and responsibility-deflection (defer mitigation, assign adaptation costs to victims). Mandatrophy appears in the form of mission creep: as climate impacts accelerate and adaptation needs grow, the constraint's institutional capacity (adaptation finance, engineering contracts) expands, locking in the framing that adaptation is the primary response and mitigation is optional. The theater ratio tracks this: 0.38→0.49 across the interval, as more institutional activity goes to defending the framing against growing climate science evidence that adaptation alone cannot prevent catastrophic warming. The constraint is not yet piton (the coordination function remains live, the institutional machinery is still building), but the pathway is visible: if global temperatures exceed adaptation limits (~2.5°C warming), adaptation projects will fail at scale, institutional machinery will face legitimacy crisis, and the constraint will either collapse or ossify into pure theater (Piton) as institutions defend their mandates through denial rather than functional adaptation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    adaptation_capacity_limits,
    'At what level of warming does adaptation-first response become physically impossible—where adaptation infrastructure cannot protect populations and retreat/relocation becomes the only option?',
    'Climate impact projections from IPCC reports and peer-reviewed studies; real-world adaptation project failure rates as warming accelerates; tipping-point models for ecosystems (coral bleaching, agricultural collapse, water scarcity).',
    'If adaptation capacity limits are reached (estimated 1.5–2°C warming by major scientific consensus), the adaptation-priority reading becomes inadequate as a governing framework. The constraint would face legitimacy crisis as mitigation-deferred warmth creates impacts adaptation cannot address. Classification could shift from Tangled Rope to Snare as the coordination function (adaptation) fails at scale and the extraction (responsibility deferral) persists.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(adaptation_capacity_limits, empirical, 'Physical limits on adaptation feasibility at higher warming levels').

omega_variable(
    mitigation_cost_vs_adaptation_cost,
    'Is the framing of rapid mitigation as economically disruptive accurate, or does it underestimate the economic costs of high-impact adaptation and repeated reconstruction in vulnerable regions?',
    'Comparative economic modeling: decarbonization costs in high-emission economies versus cumulative adaptation, reconstruction, and migration costs in vulnerable regions over 50+ years; accounting for adaptation infrastructure failures and repeated climate disasters.',
    'If adaptation cumulative costs exceed mitigation costs, the adaptation-priority framing becomes economically irrational even from wealthy nations'' narrow self-interest. The constraint''s beneficiary coalition would face cost-benefit pressure to shift toward mitigation priority, potentially dissolving the extraction structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mitigation_cost_vs_adaptation_cost, empirical, 'Comparative cost-benefit of mitigation versus infinite adaptation').

omega_variable(
    reading_observability_gap,
    'Is the adaptation-priority reading a genuine policy choice that observes and responds to climate science, or is it a cover story enabling wealthy nations to claim climate action while avoiding domestic emissions reduction?',
    'Track gap between adaptation finance growth and mitigation finance growth; compare pledged mitigation targets to actual emissions trajectories; examine whether adaptation-funded projects reduce region-level vulnerability or increase dependency on donor countries; measure growth of climate science showing adaptation inadequacy versus citations in policy documents.',
    'If the reading is primarily cover story rather than genuine response, it functions as Snare more than Tangled Rope: coordination function becomes secondary to extraction and responsibilty-shifting. Classification would shift accordingly. Divergence suggests the committer frame (reading-as-interpretive-choice) is hiding agenda-setting that is better described as pure extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_observability_gap, empirical, 'Whether adaptation-priority is governance or disguised extraction').

omega_variable(
    sibling_reading_foreclosure,
    'Does the adaptation-priority reading''s institutional entrenchment foreclose the mitigation-priority reading, or do the readings remain genuinely coexistent with both political pathways open?',
    'Track institutional resources allocated to each reading; examine whether mitigation-priority advocates have genuine policy voice or are structurally excluded (funding denied, research marginalized, delegates excluded from negotiations); measure whether policy reversals are possible (adaptation funds reallocation to mitigation, agenda reordering).',
    'If adaptation-priority has created institutional lock-in that forecloses mitigation-priority, the reading relationship should be reclassified from coexists_with to forecloses. If both remain live options, the coexistence persists and future policy shifts are structurally possible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure, empirical, 'Whether institutional entrenchment of one reading eliminates alternatives').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_imperative__adaptation_priority_reading, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_response_imperative__adaptation_priority_reading, theater_ratio, 0, 0.38).
narrative_ontology:measurement(clim_tr_t5, climate_response_imperative__adaptation_priority_reading, theater_ratio, 5, 0.41).
narrative_ontology:measurement(clim_tr_t10, climate_response_imperative__adaptation_priority_reading, theater_ratio, 10, 0.44).
narrative_ontology:measurement(clim_tr_t15, climate_response_imperative__adaptation_priority_reading, theater_ratio, 15, 0.46).
narrative_ontology:measurement(clim_tr_t20, climate_response_imperative__adaptation_priority_reading, theater_ratio, 20, 0.47).
narrative_ontology:measurement(clim_tr_t25, climate_response_imperative__adaptation_priority_reading, theater_ratio, 25, 0.48).
narrative_ontology:measurement(clim_tr_t30, climate_response_imperative__adaptation_priority_reading, theater_ratio, 30, 0.48).
narrative_ontology:measurement(clim_tr_t35, climate_response_imperative__adaptation_priority_reading, theater_ratio, 35, 0.49).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_response_imperative__adaptation_priority_reading, base_extractiveness, 0, 0.62).
narrative_ontology:measurement(clim_be_t5, climate_response_imperative__adaptation_priority_reading, base_extractiveness, 5, 0.66).
narrative_ontology:measurement(clim_be_t10, climate_response_imperative__adaptation_priority_reading, base_extractiveness, 10, 0.71).
narrative_ontology:measurement(clim_be_t15, climate_response_imperative__adaptation_priority_reading, base_extractiveness, 15, 0.74).
narrative_ontology:measurement(clim_be_t20, climate_response_imperative__adaptation_priority_reading, base_extractiveness, 20, 0.76).
narrative_ontology:measurement(clim_be_t25, climate_response_imperative__adaptation_priority_reading, base_extractiveness, 25, 0.77).
narrative_ontology:measurement(clim_be_t30, climate_response_imperative__adaptation_priority_reading, base_extractiveness, 30, 0.78).
narrative_ontology:measurement(clim_be_t35, climate_response_imperative__adaptation_priority_reading, base_extractiveness, 35, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_response_imperative__adaptation_priority_reading, suppression_requirement, 0, 0.61).
narrative_ontology:measurement(clim_su_t5, climate_response_imperative__adaptation_priority_reading, suppression_requirement, 5, 0.64).
narrative_ontology:measurement(clim_su_t10, climate_response_imperative__adaptation_priority_reading, suppression_requirement, 10, 0.67).
narrative_ontology:measurement(clim_su_t15, climate_response_imperative__adaptation_priority_reading, suppression_requirement, 15, 0.69).
narrative_ontology:measurement(clim_su_t20, climate_response_imperative__adaptation_priority_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(clim_su_t25, climate_response_imperative__adaptation_priority_reading, suppression_requirement, 25, 0.71).
narrative_ontology:measurement(clim_su_t30, climate_response_imperative__adaptation_priority_reading, suppression_requirement, 30, 0.71).
narrative_ontology:measurement(clim_su_t35, climate_response_imperative__adaptation_priority_reading, suppression_requirement, 35, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_response_imperative__adaptation_priority_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(climate_response_imperative__adaptation_priority_reading, 0.18).
narrative_ontology:affects_constraint(climate_response_imperative__adaptation_priority_reading, climate_response_imperative__mitigation_priority_reading).
narrative_ontology:affects_constraint(climate_response_imperative__adaptation_priority_reading, climate_response_imperative__degrowth_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the contested kernel climate_response_imperative. The three readings (adaptation-priority, mitigation-priority, degrowth) share a referent (anthropogenic climate change requiring human response) but differ fundamentally in how they construct the response priority and assign responsibility. Adaptation-priority frames response around immediate impacts and infrastructure investment; mitigation-priority frames response around cumulative emissions and warming prevention; degrowth frames response around structural economic transformation enabling both. The three readings create structural pressure on each other: adaptation success would reduce mitigation urgency (within wealthy nations' logic), mitigation success would reduce adaptation demand, degrowth would reframe the entire problem. Each reading authorizes different beneficiaries, victims, and policy pathways. The network edges link these as a family: adaptation-priority affects both siblings by setting the institutional agenda and resource flows, creating downstream pressure on whether mitigation and degrowth pathways remain live options.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(climate_response_imperative__adaptation_priority_reading, organized, 0.42).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
