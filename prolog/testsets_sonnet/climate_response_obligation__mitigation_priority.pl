% ============================================================================
% CONSTRAINT STORY: climate_response_obligation__mitigation_priority
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_response_obligation__mitigation_priority, []).

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
 *   constraint_id: climate_response_obligation__mitigation_priority
 *   human_readable: Rapid Decarbonization as Primary Climate Obligation (Mitigation-Priority Reading)
 *   domain: climate policy / political economy / intergenerational ethics
 *
 * SUMMARY:
 *   This story instantiates the mitigation-priority reading of the climate
 *   response obligation kernel: the claim that intergenerational justice and
 *   harm prevention require prioritizing rapid, deep decarbonization over
 *   adaptation-only or degrowth strategies. From the Rio Framework Convention
 *   (1992) through the Paris Agreement era, mitigation-priority framing has
 *   hardened from largely rhetorical commitment into a structure with real
 *   enforcement teeth — carbon pricing regimes, renewable mandates, coal
 *   phase-out schedules, carbon border adjustment mechanisms, and
 *   fossil-divestment campaigns targeting capital markets directly. The
 *   coordination function (preventing a genuine, severe, shared harm) is real
 *   and well-corroborated by physical science outside any beneficiary's
 *   self-interest. But the enforcement structure that has grown up to make
 *   the obligation binding now transfers real, present-day costs
 *   asymmetrically: onto fossil capital (stranded assets), fossil-dependent
 *   workers (displaced livelihoods), and industrializing Global South states
 *   (foreclosed cheap fossil-fueled development pathways) — while historical
 *   emitters, who accumulated the wealth using the fossil pathway now being
 *   foreclosed to others, retain disproportionate agenda-setting power over
 *   the pace and burden-sharing formula. The two sibling readings —
 *   adaptation_priority and degrowth_reading — are NOT evaluated here; they
 *   are separate constraints with their own ε, victims, and beneficiaries.
 *
 * KEY AGENTS:
 *   - future_generations: primary beneficiary, structurally voiceless, civilizational time horizon
 *   - climate_vulnerable_nations: beneficiary with limited leverage, generational time horizon
 *   - fossil_fuel_capital: primary payer via stranded-asset exposure, powerful but exit-constrained
 *   - fossil_dependent_workers: payer via displaced livelihoods, powerless and trapped
 *   - global_south_industrializing_states: payer via foreclosed development pathway, moderate power but excluded from setting enforcement terms
 *   - historical_emitter_governments: agenda-setter, institutional power, retains arbitrage-grade exit via financing and trade conditionality
 *   - climate_scientists_and_ipcc: analytical observer, provides the corroborating physical basis
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_obligation__mitigation_priority, 0.58).
domain_priors:suppression_score(climate_response_obligation__mitigation_priority, 0.44).
domain_priors:theater_ratio(climate_response_obligation__mitigation_priority, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_obligation__mitigation_priority, extractiveness, 0.58).
narrative_ontology:constraint_metric(climate_response_obligation__mitigation_priority, suppression_requirement, 0.44).
narrative_ontology:constraint_metric(climate_response_obligation__mitigation_priority, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_response_obligation__mitigation_priority, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(climate_response_obligation__mitigation_priority, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_obligation__mitigation_priority, tangled_rope).
narrative_ontology:human_readable(climate_response_obligation__mitigation_priority, "Rapid Decarbonization as Primary Climate Obligation (Mitigation-Priority Reading)").
narrative_ontology:topic_domain(climate_response_obligation__mitigation_priority, "climate policy / political economy / intergenerational ethics").

domain_priors:requires_active_enforcement(climate_response_obligation__mitigation_priority).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_obligation__mitigation_priority, 'e907b53b-7618-4856-9ba7-1321e53d693b').
narrative_ontology:cs_kernel_codification('e907b53b-7618-4856-9ba7-1321e53d693b', distributed).
narrative_ontology:cs_authority_grounding('e907b53b-7618-4856-9ba7-1321e53d693b', distributed).
narrative_ontology:cs_reading_relation('e907b53b-7618-4856-9ba7-1321e53d693b', climate_response_obligation__adaptation_priority, coexists_with).
narrative_ontology:cs_reading_relation('e907b53b-7618-4856-9ba7-1321e53d693b', climate_response_obligation__degrowth_reading, influences).
narrative_ontology:cs_axiom('e907b53b-7618-4856-9ba7-1321e53d693b', foundational, harm_prevention_lexically_prior_to_harm_accommodation).
narrative_ontology:cs_axiom_status(harm_prevention_lexically_prior_to_harm_accommodation, holdable).
narrative_ontology:cs_axiom_grounding('e907b53b-7618-4856-9ba7-1321e53d693b', harm_prevention_lexically_prior_to_harm_accommodation, deontological).
narrative_ontology:cs_axiom('e907b53b-7618-4856-9ba7-1321e53d693b', foundational, historical_cumulative_emitters_bear_proportionate_transition_cost).
narrative_ontology:cs_axiom_status(historical_cumulative_emitters_bear_proportionate_transition_cost, holdable).
narrative_ontology:cs_axiom_grounding('e907b53b-7618-4856-9ba7-1321e53d693b', historical_cumulative_emitters_bear_proportionate_transition_cost, empirically_contingent).
narrative_ontology:cs_axiom('e907b53b-7618-4856-9ba7-1321e53d693b', secondary, decarbonization_compatible_with_continued_material_growth).
narrative_ontology:cs_axiom_status(decarbonization_compatible_with_continued_material_growth, holdable).
narrative_ontology:cs_axiom_grounding('e907b53b-7618-4856-9ba7-1321e53d693b', decarbonization_compatible_with_continued_material_growth, instrumental).
narrative_ontology:cs_reference_frame('e907b53b-7618-4856-9ba7-1321e53d693b', common_but_differentiated_responsibilities_1992).
narrative_ontology:cs_drift_state('e907b53b-7618-4856-9ba7-1321e53d693b', post_paris_enforcement_era, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('e907b53b-7618-4856-9ba7-1321e53d693b', '').
narrative_ontology:cs_kernel_id(climate_response_obligation__mitigation_priority, climate_response_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_obligation__mitigation_priority, future_generations).
narrative_ontology:constraint_beneficiary(climate_response_obligation__mitigation_priority, climate_vulnerable_nations).
narrative_ontology:constraint_beneficiary(climate_response_obligation__mitigation_priority, renewable_energy_industry).
narrative_ontology:constraint_beneficiary(climate_response_obligation__mitigation_priority, low_lying_island_states).
narrative_ontology:constraint_victim(climate_response_obligation__mitigation_priority, fossil_fuel_capital).
narrative_ontology:constraint_victim(climate_response_obligation__mitigation_priority, fossil_dependent_workers).
narrative_ontology:constraint_victim(climate_response_obligation__mitigation_priority, global_south_industrializing_states).
narrative_ontology:constraint_victim(climate_response_obligation__mitigation_priority, energy_cost_burdened_households).
narrative_ontology:constraint_vindicates(climate_response_obligation__mitigation_priority, intergenerational_justice_doctrine).
narrative_ontology:constraint_vindicates(climate_response_obligation__mitigation_priority, polluter_pays_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Cannot participate in present-day negotiation over mitigation pace; inherit whatever level of warming current decisions lock in. Every ton of avoided emission is a direct transfer to them, but they have no seat, no vote, and no capacity to compensate current actors for costs borne on their behalf.
narrative_ontology:constraint_stakeholder(climate_response_obligation__mitigation_priority, future_generations, beneficiary,
    powerless, civilizational, trapped, global).

% Low-lying, drought-prone, or storm-exposed states with minimal historical emissions but maximal exposure to warming-driven harm. They press hardest for rapid mitigation in international fora, but have limited leverage over the large emitters whose action determines their outcomes.
narrative_ontology:constraint_stakeholder(climate_response_obligation__mitigation_priority, climate_vulnerable_nations, beneficiary,
    moderate, generational, trapped, global).

% Captures subsidies, mandates, and market share created by decarbonization policy. Lobbies for faster mitigation timelines that expand its addressable market; benefits directly from the same enforcement machinery (carbon pricing, renewable mandates, fossil phase-out rules) that imposes costs elsewhere.
narrative_ontology:constraint_stakeholder(climate_response_obligation__mitigation_priority, renewable_energy_industry, beneficiary,
    organized, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(climate_response_obligation__mitigation_priority, renewable_energy_industry, agenda_setter).

% Holds reserves and infrastructure whose value depends on continued extraction; rapid decarbonization timelines directly threaten to strand these assets. Has resources to lobby, litigate, and delay, but faces a structural endpoint if mitigation-priority policy is enforced as declared. Its historical emissions are cited to justify a disproportionate share of the transition burden.
narrative_ontology:constraint_stakeholder(climate_response_obligation__mitigation_priority, fossil_fuel_capital, payer,
    powerful, biographical, constrained, global).

% Employed in coal, oil, gas extraction and dependent industries in specific regions. Rapid phase-out schedules threaten their livelihoods on a timeline set by international climate diplomacy in which they have no direct representation; just-transition funding is often promised but arrives late or underfunded.
narrative_ontology:constraint_stakeholder(climate_response_obligation__mitigation_priority, fossil_dependent_workers, payer,
    powerless, biographical, trapped, regional).

% Seeking to industrialize using the same fossil-intensive pathways historical emitters used, but face pressure (financing conditions, carbon border tariffs, technology-transfer politics) to skip directly to costlier low-carbon development. They argue the mitigation-priority framing imposes present costs to correct a debt they did not incur, while historical emitters' obligations remain contested and underfunded.
narrative_ontology:constraint_stakeholder(climate_response_obligation__mitigation_priority, global_south_industrializing_states, payer,
    moderate, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(climate_response_obligation__mitigation_priority, global_south_industrializing_states, excluded).

% Bear higher near-term energy and consumer prices as carbon pricing, fuel taxes, and phased fossil retirement raise costs faster than wages or compensating transfers adjust. Political backlash from this group (fuel protests, energy-price elections) is a recurring constraint on mitigation-priority policy's actual pace.
narrative_ontology:constraint_stakeholder(climate_response_obligation__mitigation_priority, energy_cost_burdened_households, payer,
    powerless, immediate, trapped, national).

% Sets international mitigation targets, national carbon budgets, and enforcement instruments (carbon markets, border tariffs, subsidy regimes) while holding the largest share of cumulative historical emissions. Retains substantial ability to shape the pace and distribution of transition costs, and to shift compliance burdens onto later-industrializing states via trade and finance conditionality.
narrative_ontology:constraint_stakeholder(climate_response_obligation__mitigation_priority, historical_emitter_governments, agenda_setter,
    institutional, generational, arbitrage, global).

% Produce the physical basis (carbon budgets, warming trajectories) that grounds the mitigation-priority claim. Do not set policy or capture rents from it, but their assessments are cited by every other seat to justify its preferred pace and burden-sharing formula.
narrative_ontology:constraint_stakeholder(climate_response_obligation__mitigation_priority, climate_scientists_and_ipcc, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_response_obligation__mitigation_priority, diffuse).
narrative_ontology:fixing_cost_class(climate_response_obligation__mitigation_priority, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a genuinely global collective-action problem: greenhouse gas emissions anywhere warm the planet everywhere, so no single actor's restraint matters unless paired with comparable restraint elsewhere. Rapid decarbonization frameworks (carbon budgets, national pledges, sectoral phase-outs) attempt to solve free-riding on a shared atmospheric sink.
% TRANSFER_FUNCTION: Moves near-term economic cost — stranded fossil assets, displaced fossil-sector employment, higher consumer energy prices, foreclosed fossil-fueled industrialization pathways — from the present and from historically low-emitting late-developers onto the ledger of avoided future harm, credited to generations and nations not yet able to pay or vote.
% ABSENT_VOICES: Future generations cannot negotiate their own discount rate or burden allocation. Fossil-dependent workers in specific regions are rarely in the room when international mitigation timelines are set. Global South industrializing states are formally present in UNFCCC negotiations but structurally outmatched in financing and technology-transfer leverage relative to historical emitters who set the enforcement instruments.
% DISAPPEARANCE_RATIONALE: If the mitigation-priority obligation were abandoned tomorrow, fossil capital's stranded-asset risk would evaporate, carbon pricing and phase-out schedules would lose their justification, renewable energy subsidies and mandates would face immediate political pressure, and warming trajectories would shift onto a higher path — reallocating harm toward future generations and climate-vulnerable states who have no mechanism to object after the fact.
% FOUNDING_PROBLEM: Anthropogenic greenhouse gas accumulation causes warming with severe, long-lived, unevenly distributed harms; without coordinated restraint, each emitter's incentive is to free-ride on others' mitigation, producing a trajectory worse for everyone including the emitters' own descendants.
% FOUNDING_PROBLEM_CORROBORATION: The physical basis is corroborated outside any benefiting party by the IPCC assessment process, national academies of science across countries with divergent climate-policy interests, and independent paleoclimate and instrumental temperature records. Historical-responsibility accounting (cumulative emissions by nation) is corroborated by independent carbon-accounting bodies, not solely asserted by the nations who would benefit from shifting burden onto historical emitters.
narrative_ontology:disappearance_verdict(climate_response_obligation__mitigation_priority, world_rearranges).
narrative_ontology:founding_problem_status(climate_response_obligation__mitigation_priority, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_response_obligation__mitigation_priority, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(climate_response_obligation__mitigation_priority, 'none', 1).
narrative_ontology:epsilon_provenance(climate_response_obligation__mitigation_priority, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_response_obligation__mitigation_priority_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_response_obligation__mitigation_priority, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_response_obligation__mitigation_priority_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.58 and rising over the interval (0.25 in 1992 to 0.58 in 2025) because the mitigation-priority obligation began as largely aspirational (Rio, Kyoto's weak enforcement) and has progressively acquired binding transfer mechanisms — carbon pricing, phase-out mandates, carbon border tariffs — that concentrate real costs on fossil capital, fossil-sector labor, and late-industrializing states. Theater ratio starts high (0.55 in 1992, reflecting the gap between declared ambition and enforceable mechanism at Rio/Kyoto) and falls to 0.40 by 2025 as instruments have become materially binding rather than purely declaratory — this is the inverse of typical Goodhart drift and reflects a coordination structure MATURING rather than degrading into pure performance. Suppression rises correspondingly (0.20 to 0.44) as enforcement infrastructure — carbon border adjustment mechanisms, litigation against fossil expansion, divestment pressure on capital markets — has hardened. Accessibility collapse is moderate (0.35): meaningful alternative framings (adaptation-priority, degrowth) remain live and contested, so alternatives have not collapsed to the degree a mountain would show. Resistance is high (0.72): fossil capital, fossil-dependent labor constituencies, and industrializing states mount substantial organized and political resistance to the pace and burden allocation the obligation demands.
 *
 * PERSPECTIVAL GAP:
 *   From historical_emitter_governments' agenda-setting seat, the obligation functions as an enforceable but manageable coordination framework they largely wrote and retain leverage to adjust. From fossil_dependent_workers' or global_south_industrializing_states' payer seats, the same structure computes as an imposed, asymmetrically distributed cost whose timeline and burden-sharing formula they did not set and cannot easily renegotiate. The engine computes these divergent seat-level classifications from the structural power/exit/scope data; this story does not adjudicate which seat is 'correct' — both are structurally accurate descriptions of different positions inside the same constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Future generations and climate-vulnerable nations sit near the full-beneficiary end: they receive avoided harm without bearing the present transition cost, and their exit option (trapped, in the case of future generations — they cannot exit the world they inherit) does not diminish their beneficiary status because the constraint's whole function is to act on their behalf. Fossil fuel capital sits near the full-target end: constrained exit (cannot fully redeploy stranded assets), powerful but structurally on the paying side of the transfer the constraint enforces. Fossil-dependent workers and energy-cost-burdened households are powerless targets with trapped exit, which the derivation correctly pushes toward high d despite their small individual stakes relative to fossil capital. Global South industrializing states occupy an intermediate, ambiguous position — moderate power, constrained (not trapped) exit — because they retain some capacity to negotiate financing and technology-transfer terms, but structurally bear a burden (foreclosed fossil-development pathway) that historical emitters who created the atmospheric debt did not bear at equivalent development stages.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (accumulating atmospheric GHG concentration causing severe, long-lived, unevenly distributed harm) is unambiguously live — status is authored as 'live', not 'dead' or 'contested', because the physical mechanism has not been resolved by any policy action to date; warming continues to accumulate. This blocks a mandatrophy finding on the core coordination claim itself. What the metrics track instead is a distinct question: whether the ENFORCEMENT STRUCTURE built to serve that live problem has become partially decoupled from optimal burden allocation — historical emitters retaining agenda-setting power while shifting proportionate cost onto later-developing states is a burden-allocation critique, not evidence the underlying obligation has outlived its function. The tangled_rope classification (rather than pure rope) reflects exactly this: genuine coordination function (real, corroborated, still-needed) coexisting with asymmetric extraction (costs concentrated on fossil capital, fossil labor, and Global South states relative to their historical contribution) — both conditions must hold simultaneously for tangled_rope, and both are structurally present here.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    burden_allocation_fairness_formula,
    'Is the current de facto mitigation burden allocation (falling disproportionately on fossil capital, fossil-sector labor, and Global South industrializing states) proportionate to historical emissions responsibility, or does it allow historical emitters to retain disproportionate agenda-setting power while shifting adjustment costs onto others?',
    'Independent cumulative-emissions accounting compared against actual financial flows (climate finance, technology transfer, loss-and-damage payments) from historical emitters to late-industrializing states; track whether committed finance materializes at pledged levels.',
    'If burden allocation tracks historical responsibility, the tangled_rope''s extraction component is more defensible as proportionate cost recovery; if allocation is decoupled from historical responsibility, the extraction is better characterized as historical emitters using the coordination structure to externalize adjustment costs onto weaker parties.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(burden_allocation_fairness_formula, empirical, 'Whether mitigation costs track historical emissions responsibility or are shifted onto weaker parties.').

omega_variable(
    mitigation_reading_vs_sibling_readings,
    'Is mitigation-priority the correct lexical ordering of the climate response obligation, or do adaptation-priority or degrowth framings better serve intergenerational justice given realistic mitigation trajectories?',
    'This is the committer-level contest among sibling readings (adaptation_priority, degrowth_reading) of the same kernel. It is not resolved within this story; each reading is authored as a separate ε-invariant constraint. Resolution, if any, would come from observed outcomes across jurisdictions that have weighted these priorities differently, and from long-run comparison of realized warming, realized adaptation costs, and realized economic disruption under each dominant framing.',
    'If mitigation-priority is structurally correct, current extraction on fossil capital and labor is the necessary cost of preventing larger future harm. If adaptation_priority or degrowth_reading better serve the same underlying goal, mitigation-priority''s specific enforcement structure (carbon pricing, fossil phase-out mandates) may be misallocating present cost relative to the harm it prevents.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(mitigation_reading_vs_sibling_readings, conceptual, 'Committer-level contest among sibling kernel readings; not resolved within this single reading''s story.').

omega_variable(
    stranded_asset_compensation_question,
    'Does fossil capital''s stranded-asset loss represent a legitimate cost of correcting a negative externality it was permitted (or encouraged) to generate for decades, or an uncompensated taking that undermines the rule-of-law predictability investors relied on?',
    'Legal and economic analysis of comparable historical transitions (e.g., compensated vs. uncompensated regulatory takings in other environmental and public-health domains) and of whether fossil capital was on notice of climate risk at the time investments were made.',
    'If fossil capital was on adequate notice, stranding losses look more like ordinary regulatory risk realized; if not, the mitigation-priority enforcement apparatus looks more like uncompensated extraction from a class of investors, strengthening the tangled_rope over rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(stranded_asset_compensation_question, conceptual, 'Whether stranded-asset losses are legitimate externality correction or uncompensated taking.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_obligation__mitigation_priority, 1992, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t1992, climate_response_obligation__mitigation_priority, theater_ratio, 1992, 0.55).
narrative_ontology:measurement_basis(clim_tr_t1992, observed).
narrative_ontology:measurement(clim_tr_t1997, climate_response_obligation__mitigation_priority, theater_ratio, 1997, 0.5).
narrative_ontology:measurement_basis(clim_tr_t1997, observed).
narrative_ontology:measurement(clim_tr_t2005, climate_response_obligation__mitigation_priority, theater_ratio, 2005, 0.48).
narrative_ontology:measurement_basis(clim_tr_t2005, observed).
narrative_ontology:measurement(clim_tr_t2015, climate_response_obligation__mitigation_priority, theater_ratio, 2015, 0.42).
narrative_ontology:measurement_basis(clim_tr_t2015, observed).
narrative_ontology:measurement(clim_tr_t2020, climate_response_obligation__mitigation_priority, theater_ratio, 2020, 0.41).
narrative_ontology:measurement_basis(clim_tr_t2020, observed).
narrative_ontology:measurement(clim_tr_t2025, climate_response_obligation__mitigation_priority, theater_ratio, 2025, 0.4).
narrative_ontology:measurement_basis(clim_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(clim_be_t1992, climate_response_obligation__mitigation_priority, base_extractiveness, 1992, 0.25).
narrative_ontology:measurement_basis(clim_be_t1992, observed).
narrative_ontology:measurement(clim_be_t1997, climate_response_obligation__mitigation_priority, base_extractiveness, 1997, 0.3).
narrative_ontology:measurement_basis(clim_be_t1997, observed).
narrative_ontology:measurement(clim_be_t2005, climate_response_obligation__mitigation_priority, base_extractiveness, 2005, 0.38).
narrative_ontology:measurement_basis(clim_be_t2005, observed).
narrative_ontology:measurement(clim_be_t2015, climate_response_obligation__mitigation_priority, base_extractiveness, 2015, 0.48).
narrative_ontology:measurement_basis(clim_be_t2015, observed).
narrative_ontology:measurement(clim_be_t2020, climate_response_obligation__mitigation_priority, base_extractiveness, 2020, 0.53).
narrative_ontology:measurement_basis(clim_be_t2020, observed).
narrative_ontology:measurement(clim_be_t2025, climate_response_obligation__mitigation_priority, base_extractiveness, 2025, 0.58).
narrative_ontology:measurement_basis(clim_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t1992, climate_response_obligation__mitigation_priority, suppression_requirement, 1992, 0.2).
narrative_ontology:measurement_basis(clim_su_t1992, observed).
narrative_ontology:measurement(clim_su_t1997, climate_response_obligation__mitigation_priority, suppression_requirement, 1997, 0.25).
narrative_ontology:measurement_basis(clim_su_t1997, observed).
narrative_ontology:measurement(clim_su_t2005, climate_response_obligation__mitigation_priority, suppression_requirement, 2005, 0.3).
narrative_ontology:measurement_basis(clim_su_t2005, observed).
narrative_ontology:measurement(clim_su_t2015, climate_response_obligation__mitigation_priority, suppression_requirement, 2015, 0.36).
narrative_ontology:measurement_basis(clim_su_t2015, observed).
narrative_ontology:measurement(clim_su_t2020, climate_response_obligation__mitigation_priority, suppression_requirement, 2020, 0.4).
narrative_ontology:measurement_basis(clim_su_t2020, observed).
narrative_ontology:measurement(clim_su_t2025, climate_response_obligation__mitigation_priority, suppression_requirement, 2025, 0.44).
narrative_ontology:measurement_basis(clim_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_response_obligation__mitigation_priority, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(climate_response_obligation__mitigation_priority, 0.12).
narrative_ontology:affects_constraint(climate_response_obligation__mitigation_priority, climate_response_obligation__adaptation_priority).
narrative_ontology:affects_constraint(climate_response_obligation__mitigation_priority, climate_response_obligation__degrowth_reading).
narrative_ontology:affects_constraint(climate_response_obligation__mitigation_priority, carbon_border_adjustment_mechanism).
narrative_ontology:affects_constraint(climate_response_obligation__mitigation_priority, fossil_fuel_divestment_campaign).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the climate_response_obligation kernel: mitigation_priority (this story), adaptation_priority (accepts substantial warming, prioritizes resilience investment), and degrowth_reading (targets material throughput reduction over emissions-efficiency alone). Each reading has a distinct ε, distinct beneficiary/victim structure, and distinct claimed type — do not average or blend across them. The mitigation_priority reading has the most developed enforcement infrastructure of the three (carbon pricing, phase-out mandates, border tariffs) and consequently shows the clearest tangled_rope signature; the sibling readings should be authored independently with their own structural data.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
