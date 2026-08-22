% ============================================================================
% CONSTRAINT STORY: climate_response_imperative__degrowth_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_response_imperative__degrowth_reading, []).

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
 *   constraint_id: climate_response_imperative__degrowth_reading
 *   human_readable: Climate Response via Structural Degrowth (Global North)
 *   domain: climate_policy/political_economy/intergenerational_justice
 *
 * SUMMARY:
 *   This constraint instantiates ONE READING of the contested
 *   climate-response kernel: the degrowth reading holds that climate
 *   mitigation AND adaptation both require structural transformation of
 *   Global North consumption patterns, production systems, and economic
 *   institutions. The reading treats carbon-budget finiteness (from climate
 *   science) as the binding constraint and declares that technological
 *   innovation and market mechanisms at current consumption levels cannot
 *   solve the incompatibility. Present-generation Global North workers and
 *   consumers are the payers; future generations and Global South populations
 *   are the beneficiaries. The constraint is actively enforced via carbon
 *   pricing, supply-side production caps, working-time reduction, and demand
 *   destruction through both physical infrastructure change and narrative
 *   legitimacy. Sibling readings (mitigation-priority, adaptation-priority)
 *   dispute both the necessity of degrowth and the beneficiary/victim
 *   allocation—they treat technological substitution or localized adaptation
 *   as sufficient. This story claims tangled rope: genuine coordination
 *   function (solving intergenerational and North-South incompatibility)
 *   bundled with asymmetric extraction (present-generation sacrifice mandated
 *   without full reciprocal voice).
 *
 * KEY AGENTS:
 *   - Present-generation Global North workers: organized power, biographical horizon, constrained exit—face reduced working hours and consumption, dependent on transition support
 *   - Present-generation Global North consumers: moderate power, biographical horizon, constrained exit—must internalize reduced material throughput via prices, regulation, and narrative
 *   - Incumbent carbon-intensive industries: powerful, biographical horizon, constrained exit—face contraction or forced transition; capital is stranded
 *   - Future generations: powerless, civilizational horizon, trapped exit—inherit stabilized climate (under this reading) but have no voice in present transition
 *   - Global South populations: moderate power, generational horizon, constrained exit—benefit from reduced commodity demand and legitimized redistribution, but dependent on Global North's enforcement
 *   - Transition administrators (central banks, labor ministries, environmental agencies): institutional power, generational horizon—set and enforce the constraint; face coordination-failure risk across jurisdictions
 *   - Technology innovation sector: excluded by degrowth reading's rejection of carbon-removal scaling as primary strategy
 *   - Climate science community: institutional, analytical—provides epistemic authority but no enforcement power
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_imperative__degrowth_reading, 0.68).
domain_priors:suppression_score(climate_response_imperative__degrowth_reading, 0.71).
domain_priors:theater_ratio(climate_response_imperative__degrowth_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_imperative__degrowth_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(climate_response_imperative__degrowth_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(climate_response_imperative__degrowth_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_response_imperative__degrowth_reading, accessibility_collapse, 0.61).
narrative_ontology:constraint_metric(climate_response_imperative__degrowth_reading, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_imperative__degrowth_reading, tangled_rope).
narrative_ontology:human_readable(climate_response_imperative__degrowth_reading, "Climate Response via Structural Degrowth (Global North)").
narrative_ontology:topic_domain(climate_response_imperative__degrowth_reading, "climate_policy/political_economy/intergenerational_justice").

domain_priors:requires_active_enforcement(climate_response_imperative__degrowth_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_imperative__degrowth_reading, '96fd32eb-5fb9-4193-92cc-be8d9699f403').
narrative_ontology:cs_kernel_codification('96fd32eb-5fb9-4193-92cc-be8d9699f403', distributed).
narrative_ontology:cs_authority_grounding('96fd32eb-5fb9-4193-92cc-be8d9699f403', distributed).
narrative_ontology:cs_reading_relation('96fd32eb-5fb9-4193-92cc-be8d9699f403', climate_response_imperative__mitigation_priority_reading, coexists_with).
narrative_ontology:cs_reading_relation('96fd32eb-5fb9-4193-92cc-be8d9699f403', climate_response_imperative__adaptation_priority_reading, coexists_with).
narrative_ontology:cs_axiom('96fd32eb-5fb9-4193-92cc-be8d9699f403', foundational, growth_carbon_incompatibility_binding).
narrative_ontology:cs_axiom_status(growth_carbon_incompatibility_binding, holdable).
narrative_ontology:cs_axiom_grounding('96fd32eb-5fb9-4193-92cc-be8d9699f403', growth_carbon_incompatibility_binding, empirically_contingent).
narrative_ontology:cs_axiom('96fd32eb-5fb9-4193-92cc-be8d9699f403', foundational, structural_transformation_necessary).
narrative_ontology:cs_axiom_status(structural_transformation_necessary, holdable).
narrative_ontology:cs_axiom_grounding('96fd32eb-5fb9-4193-92cc-be8d9699f403', structural_transformation_necessary, deontological).
narrative_ontology:cs_reference_frame('96fd32eb-5fb9-4193-92cc-be8d9699f403', growth_dependent_high_income_economies).
narrative_ontology:cs_drift_state('96fd32eb-5fb9-4193-92cc-be8d9699f403', climate_tipping_point_proximity, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('96fd32eb-5fb9-4193-92cc-be8d9699f403', '').
narrative_ontology:cs_kernel_id(climate_response_imperative__degrowth_reading, climate_response_imperative).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_imperative__degrowth_reading, future_generations).
narrative_ontology:constraint_beneficiary(climate_response_imperative__degrowth_reading, global_south_populations).
narrative_ontology:constraint_victim(climate_response_imperative__degrowth_reading, present_global_north_workers).
narrative_ontology:constraint_victim(climate_response_imperative__degrowth_reading, present_global_north_consumers).
narrative_ontology:constraint_victim(climate_response_imperative__degrowth_reading, incumbent_carbon_intensive_industries).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Face reduced working hours, lower consumption capacity, and economic transition as the degrowth imperative reduces aggregate demand and redirects production from consumer goods to renewable infrastructure and redistribution. Their survival depends on effective transition support (retraining, income floors, care work recognition), but enforcement of that support is politically contested. Exit means emigration or sectoral exit, both constrained.
narrative_ontology:constraint_stakeholder(climate_response_imperative__degrowth_reading, present_global_north_workers, payer,
    organized, biographical, constrained, national).

% Must internalize the constraint through reduced material consumption: smaller homes, less frequent travel, fewer goods. The constraint operates partly through prices (carbon tax, scarcity), partly through cultural narrative (consumption as morally problematic), partly through supply-side physical impossibility (flight restrictions, single-occupancy vehicle bans). Resistance comes from preference divergence; constrained exit via relocation to lower-constraint jurisdictions is difficult.
narrative_ontology:constraint_stakeholder(climate_response_imperative__degrowth_reading, present_global_north_consumers, payer,
    moderate, biographical, constrained, regional).

% Face contraction or forced transition: fossil fuel extraction, high-throughput manufacturing, aviation, automotive. Capital in these sectors is stranded; workers are displaced. The constraint requires active enforcement—carbon pricing above competitive thresholds, supply-side regulation (production caps), demand destruction through narrative and physical infrastructure change. Exit via relocation encounters carbon-border adjustment and global coordination.
narrative_ontology:constraint_stakeholder(climate_response_imperative__degrowth_reading, incumbent_carbon_intensive_industries, payer,
    powerful, biographical, constrained, global).

% Inherit a habitable climate stabilized by present-generation sacrifice. Under the degrowth reading, they benefit from avoided catastrophic warming (< 1.5-2°C with high probability) without the climate-adaptation costs that the mitigation-priority or adaptation-priority readings would leave them bearing. They cannot negotiate or exit; they are locked into the present generation's choices.
narrative_ontology:constraint_stakeholder(climate_response_imperative__degrowth_reading, future_generations, beneficiary,
    powerless, civilizational, trapped, global).

% Benefit structurally from two mechanisms: (1) reduced Global North consumption reduces commodity demand pressure and price volatility, relieving resource-extraction and agricultural export dependency; (2) redistribution from Global North (reparations, climate finance, technology transfer) becomes politically legitimized by Global North's own sacrifice narrative, not charity. Their exit option is limited—remaining dependent on commodity exports or seeking alternative partnerships—but the degrowth framing narrows Global North's ability to escape that dependency through consumption growth.
narrative_ontology:constraint_stakeholder(climate_response_imperative__degrowth_reading, global_south_populations, beneficiary,
    moderate, generational, constrained, global).

% Institutional actors (central banks, labor ministries, environmental agencies, international bodies) that set and enforce the degrowth transition: carbon pricing, working-time reduction, industrial policy for renewable transition, redistribution mechanisms. They face acute tension between enforcing the constraint's terms and managing political resistance from payers. Their agenda-setting power depends on coordinated global action (no jurisdiction can enforce unilaterally without capital flight).
narrative_ontology:constraint_stakeholder(climate_response_imperative__degrowth_reading, transition_administrators, agenda_setter,
    institutional, generational, analytical, national).

% Watch the constraint's enforcement with mixed interest: benefit from reduced Global North commodity demand and political legitimacy for climate finance, but risk being pushed further into commodity dependence if Global North's degrowth is incomplete or exports its consumption to them. Their position is asymmetric—Global North's choice to degrow changes their structural options but does not give them enforceable say.
narrative_ontology:constraint_stakeholder(climate_response_imperative__degrowth_reading, global_south_governments, observer,
    institutional, generational, constrained, global).

% Provides epistemic authority for the constraint: establishes that mitigation is necessary, that carbon budget remaining is finite, that current NDC commitments are insufficient. Under the degrowth reading, science rules out reliance on unproven carbon removal and technological substitution at current consumption levels. Science community has no enforcement power but frames the urgency and legitimacy of the constraint.
narrative_ontology:constraint_stakeholder(climate_response_imperative__degrowth_reading, climate_science_community, observer,
    institutional, generational, analytical, global).

% Positioned outside the constraint under the degrowth reading because degrowth reading rejects the premise that technological innovation at current consumption levels can achieve mitigation targets. Clean-tech, carbon-removal, and geoengineering sectors see their domain redefined from solution to partial mitigation supplement. They would argue for their inclusion (CDR scaling, grid modernization, etc.); exclusion is what the degrowth reading's core claim produces.
narrative_ontology:constraint_stakeholder(climate_response_imperative__degrowth_reading, technology_innovation_sector, excluded,
    powerful, biographical, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_response_imperative__degrowth_reading, transition_administrators).
narrative_ontology:fixing_cost_class(climate_response_imperative__degrowth_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aligns present-generation effort and sacrifice with long-term climate stability: by reducing aggregate demand, redirecting production toward carbon elimination and resilience, and redistributing resources to Global South, the constraint solves the intergenerational coordination problem—present and future generations' interests no longer operate at pure conflict. It also solves the Global North–South coordination problem by making Global North's emission reductions materially credible (not dependent on externalized growth) and legitimizing resource transfer.
% TRANSFER_FUNCTION: Moves three flows: (1) consumption capacity from present Global North workers and consumers to future generations (via avoided climate damages); (2) resources from incumbent carbon-intensive industries to renewable energy and care infrastructure; (3) wealth/technology from Global North to Global South as part of decarbonization and adaptation finance. The constraint operates by physically reducing high-income-country throughput and directing the freed resources toward long-term stability.
% ABSENT_VOICES: Technology-forward sectors (clean-tech, carbon-removal firms, energy innovation) would argue the constraint unnecessarily restricts their domain and preempts solutions that might sustain consumption; they are excluded by the degrowth reading's core premise. Incumbent fossil-fuel workers would argue transition should preserve their livelihoods without reduced consumption—they have voice in labor negotiations but are not structural participants in the constraint's logic.
% DISAPPEARANCE_RATIONALE: If the degrowth-imperative constraint vanished, Global North economies would revert to growth-dependent consumption patterns, carbon budgets would exhaust faster, climate tipping points would be crossed with higher probability, and Global South would remain in subordinate commodity-export dependency. The constraint's absence means a fundamentally different climate trajectory and economic structure within 50 years—habitable versus uninhabitable regions, haves versus have-nots, are the rearrangement.
% FOUNDING_PROBLEM: The founding problem is the thermodynamic and intergenerational incompatibility between indefinite consumption growth in high-income countries and climate stabilization under a finite carbon budget. Technological innovation and market mechanisms, under the degrowth reading, cannot solve this incompatibility at the scale and speed required; structural transformation of consumption, production, and distribution is necessary.
% FOUNDING_PROBLEM_CORROBORATION: Climate science (IPCC AR6, paleoclimate data) establishes the carbon-budget and tipping-point facts from outside the degrowth advocacy community. Ecological economics (Georgescu-Roegen, Daly, Jackson) documents the biophysical incompatibility of indefinite growth with planetary boundaries. The mitigation-priority and adaptation-priority readings contest the founding problem's urgency or necessity—they argue technological substitution and localized adaptation are sufficient—but they do not dispute the underlying carbon-budget constraint. The corroboration is interdisciplinary and includes skeptical voices on the sufficiency claim.
narrative_ontology:disappearance_verdict(climate_response_imperative__degrowth_reading, world_rearranges).
narrative_ontology:founding_problem_status(climate_response_imperative__degrowth_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_response_imperative__degrowth_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(climate_response_imperative__degrowth_reading, 'none', 1).
narrative_ontology:epsilon_provenance(climate_response_imperative__degrowth_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_response_imperative__degrowth_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_response_imperative__degrowth_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_response_imperative__degrowth_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.42 to 0.68 over 30 years as the transition deepens: initial extractiveness is moderate (carbon pricing and early regulations fall on high-income consumers whose surplus is large), but as working-time reduction and production-capacity contraction deepen, extraction from workers becomes structural. Theater rises from 0.28 to 0.42 as initially genuine transition infrastructure (renewable energy buildout, care-work expansion) becomes layered with performative measures (green capitalism, offsetting, corporate sustainability theater) that slow real degrowth. Suppression rises from 0.55 to 0.71 as political resistance to consumption reduction intensifies and enforcement machinery (borders, production regulations, financial penalties) must be ratcheted up to hold the constraint. Accessibility collapse is moderate (0.61): alternatives exist (technology-priority reading, adaptation-only reading, continued growth) but carrying them becomes politically costly as climate impacts worsen and their inadequacy becomes visible. Resistance is high (0.74): workers, industries, and consumers mount active resistance through political mobilization, capital flight, informal economies, and legal challenges. The constraint is tangled rope, not snare, because (a) genuine coordination function exists—solving intergenerational and North-South incompatibility—and (b) extraction is high but not total—transition support, investment in worker retraining, and legitimacy claims are real, not pure theater (though theater increases over time).
 *
 * PERSPECTIVAL GAP:
 *   From the transition-administrator seat, this is genuine coordination: present generation accepts sacrifice to stabilize the system for future. From the payer seats (workers, consumers, industries), this is enforced extraction: they bear the costs while beneficiaries (future, Global South) are voiceless and cannot reciprocate. From the Global South beneficiary seat, this is asymmetric: they benefit structurally but have little voice in transition design—Global North degrows on its own terms. From the climate-science seat (observer), this is a binding necessity: carbon budget is finite, so growth must stop. From the technology-innovation seat (excluded), this is illegitimate foreclosure: degrowth preempts solutions that might sustain both climate and consumption. The engine computes these divergences from stakeholder power, exit options, and beneficiary/victim status; the authored claim (tangled rope) does not resolve them—the divergence itself is the measurement.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality for transition administrators is near 0.2 (beneficiary; they set the agenda and avoid worst-case losses). For present-generation workers it is near 0.75 (target; they pay via reduced working hours and consumption, and exit is constrained by job dependence on transition sectors). For incumbent industries it is near 0.8 (target; capital is stranded). For future generations it is at 0.0 (pure beneficiary; no costs, maximum benefit, no voice). For Global South it is near 0.15 (beneficiary of resource transfer and reduced commodity pressure, but with constrained agency and exit). Technology sector is near 1.0 in a different register—excluded entirely from the coordination, their domain is redefined by the reading. No override needed; the structural data produce the right directionality for each seat.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem—climate stabilization under a finite carbon budget and current consumption levels—is live and serious (confirmed by climate science). The degrowth reading's mandate is that structural transformation of Global North consumption is necessary. This mandate is contested by sibling readings: mitigation-priority reading says technological substitution suffices; adaptation-priority reading says localized resilience is the binding constraint. Mandatrophy would occur if the founding problem (climate destabilization risk) became dead (problem solved) while the degrowth constraint persisted anyway—e.g., if carbon budgets expanded due to unexpected CDR breakthroughs, or if climate impacts proved less severe than modeled, yet Global North persisted in degrowth anyway as ideology rather than necessity. Under current best estimates, the founding problem is live, so mandatrophy has not occurred. However, there is risk: if technological innovation accelerates and proves sufficient, or if Global North decides to live with adaptation costs rather than pay transition costs, the constraint's mandate dies but enforcement inertia might persist. The omega on CDR scalability and the founding-problem-corroboration statement document this risk.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    feasibility_of_global_degrowth_enforcement,
    'Can degrowth coordination be enforced globally when individual jurisdictions have incentive to defect (maintain growth, export carbon-intensive production, free-ride on others'' sacrifice)?',
    'Observation of 10-20 year coordination attempts: whether carbon-border adjustments, supply-chain governance, and international agreements actually prevent production relocation and consumption arbitrage, or whether enforcement degrades into selective enforcement favoring capital-mobile actors.',
    'If enforcement fails at scale, the constraint collapses into Prisoner''s Dilemma and effective extraction falls to near-zero (becomes a rope with hollow coordination, or a failed scaffold). If enforcement holds, the constraint stabilizes as tangled rope (coordinating long-term interests while extracting from present-generation consumption). Failure would vindicate the technology-priority reading; success would vindicate degrowth.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(feasibility_of_global_degrowth_enforcement, empirical, 'Whether global coordination on degrowth is structurally possible given incentives to defect.').

omega_variable(
    just_transition_delivery,
    'Can transition support (retraining, income floors, care work recognition, regional investment) genuinely neutralize the victim status of displaced workers, or does degrowth necessarily impose net losses on present-generation working classes regardless of transfer mechanisms?',
    'Post-transition empirical study of worker outcomes in economies that implement degrowth: income stability, employment quality, health and social outcomes, intergenerational mobility. Comparison with baseline projections of growth-as-usual followed by climate adaptation costs.',
    'If transition support succeeds, the constraint is tangled rope with asymmetric but manageable costs (victims are payers but not devastated). If it fails, the constraint becomes snare (victims bear costs with no realistic alternative, transition support is theater). This determination affects the legitimacy of the beneficiary claim for future generations—if present-generation loss is permanent, the intergenerational transfer is more extractive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(just_transition_delivery, empirical, 'Whether transition support can genuinely neutralize worker losses under degrowth.').

omega_variable(
    kernel_framing_alternative_readings,
    'Is the climate-response kernel fundamentally about WHAT FORM the response takes (degrowth vs. technology vs. adaptation), or is it about WHETHER response is necessary at all? Do the sibling readings share a common commitment to climate action, or do they represent fundamentally different commitments to climate science authority?',
    'Examine the sibling readings'' treatment of carbon budgets, tipping points, and IPCC findings: do they accept the same scientific framing and disagree on policy form, or do they dispute the science itself? If the latter, the kernel is not about policy form but about climate science authority.',
    'If sibling readings are policy-form differences within shared climate commitment, then reading_relations should be coexists_with (different parties hold different strategies). If sibling readings dispute climate science, then the kernel is not climate response but climate-science legitimacy, and this story is mis-framed or overpopulated. This omega documents the framing assumption and points to alternative decomposition if needed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_alternative_readings, conceptual, 'Whether the kernel is about response form or response necessity itself.').

omega_variable(
    carbon_removal_technological_frontier,
    'Does the degrowth reading''s exclusion of CDR and geoengineering reflect a structural necessity (carbon-removal cannot scale fast enough at plausible costs) or a normative choice (carbon removal is privileging present-generation consumption over ecological integrity)?',
    'Examine degrowth literature on whether CDR exclusion is empirically grounded (cost per ton, energy requirements, permanence) or axiomatically grounded (consumption is intrinsically destructive). Distinguish empirical scalability claims from normative objections to the consumption-offset frame.',
    'If CDR exclusion is empirical (scalability fails), the degrowth reading is compatible with supplementary CDR and can coexist with technology-priority readings on a shared carbon-budget constraint. If CDR exclusion is axiomatically grounded (consumption is the problem, not just its emissions), then the degrowth reading forecloses technology-priority reading—they share no common framework. This determines reading_relations type.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(carbon_removal_technological_frontier, conceptual, 'Whether CDR exclusion is empirical scalability or normative objection to consumption.').

omega_variable(
    global_south_agency_under_degrowth,
    'Does the degrowth reading''s allocation of Global South as beneficiary reflect actual Global South agency in shaping the transition, or impose a beneficiary status without voice (repeating colonial resource-flow patterns in reverse)?',
    'Examine degrowth policy proposals for Global South participation in setting terms, veto rights, ownership of transition infrastructure, and control of technology transfer. Compare against pre-degrowth climate-justice demands from Global South movements.',
    'If Global South has genuine co-authority over transition terms, the beneficiary status is legitimized. If degrowth is imposed by Global North on Global South as ''shared sacrifice'' without real voice, the constraint becomes snare-adjacent—Global South is structured as victim (dependency) rather than beneficiary (agency). This affects the constraint''s classification from a Global South analytical seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(global_south_agency_under_degrowth, empirical, 'Whether Global South holds actual agency in degrowth transition design or receives imposed beneficiary status.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_imperative__degrowth_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_response_imperative__degrowth_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(clim_tr_t10, climate_response_imperative__degrowth_reading, theater_ratio, 10, 0.33).
narrative_ontology:measurement(clim_tr_t20, climate_response_imperative__degrowth_reading, theater_ratio, 20, 0.38).
narrative_ontology:measurement(clim_tr_t30, climate_response_imperative__degrowth_reading, theater_ratio, 30, 0.42).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_response_imperative__degrowth_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(clim_be_t10, climate_response_imperative__degrowth_reading, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(clim_be_t20, climate_response_imperative__degrowth_reading, base_extractiveness, 20, 0.62).
narrative_ontology:measurement(clim_be_t30, climate_response_imperative__degrowth_reading, base_extractiveness, 30, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_response_imperative__degrowth_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(clim_su_t10, climate_response_imperative__degrowth_reading, suppression_requirement, 10, 0.62).
narrative_ontology:measurement(clim_su_t20, climate_response_imperative__degrowth_reading, suppression_requirement, 20, 0.67).
narrative_ontology:measurement(clim_su_t30, climate_response_imperative__degrowth_reading, suppression_requirement, 30, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_response_imperative__degrowth_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(climate_response_imperative__degrowth_reading, 0.18).
narrative_ontology:affects_constraint(climate_response_imperative__degrowth_reading, climate_response_imperative__mitigation_priority_reading).
narrative_ontology:affects_constraint(climate_response_imperative__degrowth_reading, climate_response_imperative__adaptation_priority_reading).

% DUAL FORMULATION NOTE:
% The climate_response_imperative kernel decomposes into three structurally distinct readings. Each reading instantiates a different constraint with different beneficiary/victim sets, different ε values, and different policy implications. The degrowth reading treats consumption reduction as structural necessity and allocates costs to present-generation Global North workers and consumers. The mitigation-priority reading treats technology as sufficient and allocates costs to future adaptation. The adaptation-priority reading treats localized resilience as primary and accepts residual climate impacts. These are not the same constraint viewed from different angles—they are different claims about WHAT must happen and WHO PAYS. The three stories are linked by network.affects_constraints to indicate they are siblings of the same kernel, not independent constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(climate_response_imperative__degrowth_reading, powerless, 0.0).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
