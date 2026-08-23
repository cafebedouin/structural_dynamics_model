% ============================================================================
% CONSTRAINT STORY: climate_response_obligation__degrowth_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_response_obligation__degrowth_reading, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: climate_response_obligation__degrowth_reading
 *   human_readable: Degrowth Sufficiency Constraint on Material Throughput
 *   domain: climate_policy/political_economy/intergenerational_ethics
 *
 * SUMMARY:
 *   The degrowth reading of the climate response obligation treats planetary
 *   boundaries as hard, non-negotiable limits that require absolute reduction
 *   of material throughput in the Global North. It is not a marginal
 *   efficiency constraint — it targets the growth imperative itself. The
 *   constraint coordinates by allocating a finite ecological budget
 *   (sufficiency corridors) and extracts from the structural drivers of
 *   overshoot: high-consumption lifestyles and capital's growth requirement.
 *   It claims Tangled Rope because it genuinely coordinates (prevents
 *   planetary collapse, distributes remaining space equitably) AND
 *   asymmetrically extracts (North pays, capital pays, South's development is
 *   conditional). The engine will compute per-seat types from this structural
 *   data.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_obligation__degrowth_reading, 0.62).
domain_priors:suppression_score(climate_response_obligation__degrowth_reading, 0.38).
domain_priors:theater_ratio(climate_response_obligation__degrowth_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_obligation__degrowth_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(climate_response_obligation__degrowth_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(climate_response_obligation__degrowth_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_response_obligation__degrowth_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(climate_response_obligation__degrowth_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_obligation__degrowth_reading, tangled_rope).
narrative_ontology:human_readable(climate_response_obligation__degrowth_reading, "Degrowth Sufficiency Constraint on Material Throughput").
narrative_ontology:topic_domain(climate_response_obligation__degrowth_reading, "climate_policy/political_economy/intergenerational_ethics").

domain_priors:requires_active_enforcement(climate_response_obligation__degrowth_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_obligation__degrowth_reading, '0dd9d2b9-7072-4a5a-a50e-cb20e4f0340b').
narrative_ontology:cs_kernel_codification('0dd9d2b9-7072-4a5a-a50e-cb20e4f0340b', distributed).
narrative_ontology:cs_authority_grounding('0dd9d2b9-7072-4a5a-a50e-cb20e4f0340b', practice).
narrative_ontology:cs_interpretation_layer_present('0dd9d2b9-7072-4a5a-a50e-cb20e4f0340b').
narrative_ontology:cs_reading_relation('0dd9d2b9-7072-4a5a-a50e-cb20e4f0340b', climate_response_obligation__mitigation_priority, coexists_with).
narrative_ontology:cs_reading_relation('0dd9d2b9-7072-4a5a-a50e-cb20e4f0340b', climate_response_obligation__adaptation_priority, coexists_with).
narrative_ontology:cs_axiom('0dd9d2b9-7072-4a5a-a50e-cb20e4f0340b', foundational, planetary_boundaries_are_non_negotiable).
narrative_ontology:cs_axiom_status(planetary_boundaries_are_non_negotiable, holdable).
narrative_ontology:cs_axiom_grounding('0dd9d2b9-7072-4a5a-a50e-cb20e4f0340b', planetary_boundaries_are_non_negotiable, empirically_contingent).
narrative_ontology:cs_axiom('0dd9d2b9-7072-4a5a-a50e-cb20e4f0340b', foundational, sufficiency_corridors_are_required_for_justice).
narrative_ontology:cs_axiom_status(sufficiency_corridors_are_required_for_justice, holdable).
narrative_ontology:cs_axiom_grounding('0dd9d2b9-7072-4a5a-a50e-cb20e4f0340b', sufficiency_corridors_are_required_for_justice, deontological).
narrative_ontology:cs_axiom('0dd9d2b9-7072-4a5a-a50e-cb20e4f0340b', foundational, capital_growth_imperative_is_incompatible_with_planetary_survival).
narrative_ontology:cs_axiom_status(capital_growth_imperative_is_incompatible_with_planetary_survival, holdable).
narrative_ontology:cs_axiom_grounding('0dd9d2b9-7072-4a5a-a50e-cb20e4f0340b', capital_growth_imperative_is_incompatible_with_planetary_survival, empirically_contingent).
narrative_ontology:cs_reference_frame('0dd9d2b9-7072-4a5a-a50e-cb20e4f0340b', planetary_integrity_within_biophysical_limits).
narrative_ontology:cs_drift_state('0dd9d2b9-7072-4a5a-a50e-cb20e4f0340b', post_paris_agreement_overshoot_trajectory, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('0dd9d2b9-7072-4a5a-a50e-cb20e4f0340b', '').
narrative_ontology:cs_kernel_id(climate_response_obligation__degrowth_reading, climate_response_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_obligation__degrowth_reading, planetary_systems).
narrative_ontology:constraint_beneficiary(climate_response_obligation__degrowth_reading, future_generations).
narrative_ontology:constraint_beneficiary(climate_response_obligation__degrowth_reading, global_south_equitable_development).
narrative_ontology:constraint_victim(climate_response_obligation__degrowth_reading, global_north_high_consumption_households).
narrative_ontology:constraint_victim(climate_response_obligation__degrowth_reading, capital_accumulation_mechanisms).
narrative_ontology:constraint_victim(climate_response_obligation__degrowth_reading, global_south_development_conditional).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(climate_response_obligation__degrowth_reading, degrowth_movements).
narrative_ontology:constraint_vindicates(climate_response_obligation__degrowth_reading, planetary_boundaries_hard_limits).
narrative_ontology:constraint_vindicates(climate_response_obligation__degrowth_reading, intergenerational_justice_as_non_negotiable).
narrative_ontology:constraint_vindicates(climate_response_obligation__degrowth_reading, sufficiency_over_efficiency_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Earth system processes (biosphere integrity, climate stability, biogeochemical flows) receive reduced extraction pressure when material throughput contracts within boundaries. They do not negotiate; their 'benefit' is the avoidance of crossing irreversible thresholds. No exit — the constraint exists to align human activity with their operating limits.
narrative_ontology:constraint_stakeholder(climate_response_obligation__degrowth_reading, planetary_systems, beneficiary,
    institutional, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(climate_response_obligation__degrowth_reading, planetary_systems).

% All people not yet born inherit the biophysical conditions this constraint aims to preserve. They cannot advocate, exit, or reciprocate. Their inclusion as beneficiary is a structural claim about temporal asymmetry: present choices determine their option space without their consent. The constraint's legitimacy rests partly on this asymmetry.
narrative_ontology:constraint_stakeholder(climate_response_obligation__degrowth_reading, future_generations, beneficiary,
    powerless, civilizational, trapped, universal).
narrative_ontology:stakeholder_non_agent(climate_response_obligation__degrowth_reading, future_generations).

% Households in high-income nations whose material lifestyles (per-capita energy, meat, transport, housing floor area, consumer goods) exceed global fair-share allocations. The constraint requires absolute reduction, not green substitution. Exit is constrained: lifestyle downgrade is socially stigmatized, infrastructure locks in high throughput (suburbs, car dependence), and political representation captures their preferences. They pay in foregone consumption, status, and convenience.
narrative_ontology:constraint_stakeholder(climate_response_obligation__degrowth_reading, global_north_high_consumption_households, payer,
    powerful, biographical, constrained, global).

% The profit imperative, financial return expectations, and growth-dependent debt structures that require expanding material throughput to service. Degrowth makes capital's reproductive logic the extraction target: profits shrink when throughput contracts, asset values reprice, and debt service becomes harder. Exit is constrained because the mechanism is structural — individual firms can pivot but the system-level growth requirement has no exit within capitalism. This is the constraint's primary extractive edge.
narrative_ontology:constraint_stakeholder(climate_response_obligation__degrowth_reading, capital_accumulation_mechanisms, payer,
    institutional, generational, constrained, global).

% The claim that Global South nations need ecological space for basic needs fulfillment (energy access, nutrition, housing, sanitation) — space that only opens if the Global North contracts first. They benefit from the constraint's distributive logic: sufficiency corridors allocate remaining planetary budget preferentially to unmet needs. But their agency is mediated by North-South power asymmetries; they cannot enforce the North's contraction.
narrative_ontology:constraint_stakeholder(climate_response_obligation__degrowth_reading, global_south_equitable_development, beneficiary,
    organized, generational, constrained, continental).
narrative_ontology:stakeholder_non_agent(climate_response_obligation__degrowth_reading, global_south_equitable_development).

% Global South governments and populations whose development trajectories are constrained by the same planetary budget the degrowth reading invokes. If the North does not contract sufficiently, the South's remaining carbon/material space is inadequate for dignified living standards. They pay twice: historically (colonial extraction created the overshoot) and prospectively (their development is capped by Northern inaction). Exit is constrained — no alternative planet, limited leverage over Northern consumption.
narrative_ontology:constraint_stakeholder(climate_response_obligation__degrowth_reading, global_south_development_conditional, payer,
    moderate, biographical, constrained, continental).

% International negotiators, IPCC authors, central bankers, and green-growth ministries who frame the response. They administer the constraint discourse: setting targets, designing mechanisms (carbon pricing, technology transfer), and legitimizing or marginalizing the degrowth reading. They are not primary beneficiaries or payers — their position is shaped by institutional survival, epistemic authority, and career incentives within the growth paradigm. Mobile exit: they can shift between mitigation, adaptation, and degrowth framings as political winds change.
narrative_ontology:constraint_stakeholder(climate_response_obligation__degrowth_reading, climate_policy_elites, agenda_setter,
    institutional, biographical, mobile, global).

% Social movements, ecological economists, and policy networks advocating sufficiency, commons, and post-growth institutions. They set the degrowth agenda and benefit from its validation (intellectual coherence, political relevance, identity affirmation). Identity-locked exit: professional and activist identity is fused to the reading; abandoning it dissolves the community and career. They are both authors and captives of the constraint.
narrative_ontology:constraint_stakeholder(climate_response_obligation__degrowth_reading, degrowth_movements, agenda_setter,
    organized, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(climate_response_obligation__degrowth_reading, degrowth_movements, beneficiary).

% OECD, IMF, World Bank, major central banks, and standard economics departments. They would object to the degrowth reading's core premises (growth decoupling is possible, green growth is sufficient, welfare requires rising GDP) but are structurally excluded from the constraint's internal logic — the reading treats their paradigm as the problem, not a stakeholder. Arbitrage exit: they control the dominant policy channels and can resource alternative framings at will.
narrative_ontology:constraint_stakeholder(climate_response_obligation__degrowth_reading, mainstream_economic_institutions, excluded,
    institutional, biographical, arbitrage, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allocates the finite planetary material/energy budget across present and future claimants so that biophysical thresholds are not crossed. Solves the tragedy of the atmospheric commons by imposing a collective cap and a distributive rule (sufficiency corridors) that prioritizes unmet needs over excess consumption.
% TRANSFER_FUNCTION: Moves ecological space (carbon budget, material footprint, biosphere capacity) from Global North high-consumption lifestyles and capital accumulation imperatives toward planetary integrity, future generations, and Global South needs fulfillment. The transfer is enforced through absolute caps, not prices — quantity rationing replaces market allocation.
% ABSENT_VOICES: Global South populations whose development is conditional on Northern contraction — they are not at the table when Northern sufficiency targets are negotiated. Working-class communities in the North whose livelihoods depend on high-throughput industries (auto, aviation, construction) and who are offered no just transition in the degrowth frame. Non-human species and ecosystems — represented only analytically through planetary boundaries, not as self-advocating agents.
% DISAPPEARANCE_RATIONALE: If the degrowth constraint vanished overnight, the default trajectory (green growth / techno-optimism) would resume: continued throughput growth with efficiency gains, betting on decoupling that has not materialized at scale. Planetary boundaries would be crossed with higher probability. Global South development space would shrink further. Capital accumulation would face no biophysical check. The world rearranges toward deeper overshoot.
% FOUNDING_PROBLEM: The founding problem is the empirical failure of green growth: decades of climate policy have not bent the global emissions curve downward while GDP grows. The constraint was built (intellectually, by ecological economists and degrowth scholars) to solve the coordination problem of living well within limits when the dominant paradigm refuses to accept limits.
% FOUNDING_PROBLEM_CORROBORATION: IPCC AR6 WGIII acknowledges that 'demand-side measures and new ways of end-use service provision can reduce global GHG emissions by 40-70% by 2050' (Ch.5) — corroboration from outside the degrowth beneficiary set. The International Resource Panel (UNEP) documents that material footprint per capita in high-income countries is 10x low-income, with no absolute decoupling. The founding problem (green growth failure) is attested by the very institutions the reading critiques.
narrative_ontology:disappearance_verdict(climate_response_obligation__degrowth_reading, world_rearranges).
narrative_ontology:founding_problem_status(climate_response_obligation__degrowth_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_response_obligation__degrowth_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron+rescue1', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(climate_response_obligation__degrowth_reading, 'none', 1).
narrative_ontology:epsilon_provenance(climate_response_obligation__degrowth_reading, 0.62, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_response_obligation__degrowth_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_response_obligation__degrowth_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_response_obligation__degrowth_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) reflects that the constraint takes from powerful structural agents (capital accumulation, Northern consumption) but does so for a coordination function (planetary survival, intergenerational justice, North-South equity) that is real and not merely cover. Suppression (0.38) is moderate: the constraint requires active enforcement (caps, rationing, bans on luxury throughput) but its legitimacy derives from biophysical reality, not pure coercion. Theater (0.22) is low: the sufficiency function is the primary operation, not a performance. Accessibility collapse (0.45) and resistance (0.58) reflect that alternatives (green growth) remain politically dominant but are empirically weakening. Measurements track the reading's intellectual/political trajectory from Limits to Growth (1972) through IPCC acknowledgment of demand-side mitigation (2020s) — extractiveness rises as the reading's structural claims gain empirical vindication.
 *
 * PERSPECTIVAL GAP:
 *   From the planetary_systems and future_generations seats, this is a Mountain (biophysical necessity). From global_north_high_consumption_households and capital_accumulation_mechanisms, it is a Snare (extractive, enforced, exit-constrained). From global_south_equitable_development, it is a Rope (coordination for fair shares). From climate_policy_elites, it is a Piton (theatrical engagement with a reading they cannot adopt without institutional suicide). The engine computes this divergence from power, exit, and beneficiary/victim structure — the single claimed_type does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries: planetary_systems (d≈0.0, universal scope, analytical exit), future_generations (d≈0.0, trapped, civilizational horizon), global_south_equitable_development (d≈0.15, constrained exit, organized power). Victims: global_north_high_consumption_households (d≈0.85, powerful but constrained exit, biographical horizon), capital_accumulation_mechanisms (d≈0.9, institutional, constrained exit, generational horizon), global_south_development_conditional (d≈0.7, moderate power, constrained exit). The asymmetry is structural: the same planetary budget constraint that protects the South's development space also caps it if the North fails — the South pays for Northern inaction. Agenda_setters (climate_policy_elites, degrowth_movements) sit at different d: elites mobile (d≈0.3), movements identity-locked (d≈0.4 — they benefit from validation but are trapped by the reading's demands).
 *
 * MANDATROPHY ANALYSIS:
 *   The mandate 'prevent dangerous climate change' has not atrophied — it has intensified. But the dominant institutional reading (mitigation_priority via green growth) shows mandatrophy: it claims to solve the problem while the problem worsens. The degrowth reading diagnoses this as mandatrophy of the green-growth paradigm and offers a successor constraint. Whether degrowth itself becomes a mandatrophy (a sufficiency regime that persists after boundaries are secured) is an open question — the sunset clause is absent, but the constraint's logic implies it should dissolve once throughput is within boundaries and needs are met.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    decoupling_feasibility,
    'Can absolute decoupling of GDP from material throughput occur at the speed and scale required to stay within planetary boundaries without sufficiency?',
    'Empirical tracking of global material footprint vs. GDP over 2025-2040 under aggressive green-growth policies (EU Green Deal, IRA, China''s ecological civilization). If footprint falls absolutely while GDP grows, the degrowth reading''s core empirical premise is falsified.',
    'If decoupling is feasible at scale, the degrowth reading''s extractiveness on capital_accumulation_mechanisms is gratuitous — the coordination function (planetary survival) could be achieved without the extraction. The constraint would reclassify toward snare. If not, the extraction is necessary coordination cost.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(decoupling_feasibility, empirical, 'The empirical contest at the heart of the kernel: green growth vs. degrowth.').

omega_variable(
    sufficiency_corridor_operationalization,
    'Can sufficiency corridors (per-capita resource floors and ceilings) be defined and governed without authoritarian allocation?',
    'Democratic deliberation experiments (citizen assemblies on consumption corridors), modeling of participatory rationing systems, historical analysis of wartime rationing with equity safeguards.',
    'If sufficiency requires authoritarian suppression, the constraint''s suppression metric understates its coercive reality and it trends toward snare. If democratic governance is viable, the coordination function holds with lower suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sufficiency_corridor_operationalization, conceptual, 'Whether the coordination mechanism itself generates extraction through governance.').

omega_variable(
    kernel_framing_underdetermination,
    'Does the kernel ''climate response obligation'' legitimately contain the degrowth reading, or does the reading exceed the kernel''s scope by targeting capital accumulation itself?',
    'Genealogical analysis of UNFCCC, IPCC, and Paris Agreement texts: do they commit to ''response'' narrowly (emissions) or broadly (systemic drivers)? Legal interpretation of ''common but differentiated responsibilities'' — does it entail throughput contraction?',
    'If the reading exceeds the kernel, it is a rival commitment system, not a reading — the cs_structure fields would misrepresent the relationship. If it falls within, the sibling relations and axioms are correctly authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Whether the degrowth reading is a legitimate interpretation of the shared kernel or a competing kernel.').

omega_variable(
    capital_accumulation_as_extractive_mechanism,
    'Is capital accumulation itself the extractive mechanism, or is it a neutral engine that can be directed toward sufficiency?',
    'Historical analysis of post-growth pilot regions (e.g., Kerala, Costa Rica, degrowth transitions in modeling), firm-level studies of profit-making under throughput caps, financial system modeling of debt without growth.',
    'If capital can operate within sufficiency (steady-state economics, not-for-profit ownership, commons-based production), the victim status of capital_accumulation_mechanisms is contested — the constraint extracts from a contingent form, not capital per se. If not, the extraction is structural.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(capital_accumulation_as_extractive_mechanism, conceptual, 'Whether the constraint''s extraction from capital is necessary or contingent on capital''s current form.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_obligation__degrowth_reading, 1972, 2050).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t1972, climate_response_obligation__degrowth_reading, theater_ratio, 1972, 0.05).
narrative_ontology:measurement(clim_tr_t1992, climate_response_obligation__degrowth_reading, theater_ratio, 1992, 0.08).
narrative_ontology:measurement(clim_tr_t2000, climate_response_obligation__degrowth_reading, theater_ratio, 2000, 0.12).
narrative_ontology:measurement(clim_tr_t2010, climate_response_obligation__degrowth_reading, theater_ratio, 2010, 0.16).
narrative_ontology:measurement(clim_tr_t2018, climate_response_obligation__degrowth_reading, theater_ratio, 2018, 0.19).
narrative_ontology:measurement(clim_tr_t2024, climate_response_obligation__degrowth_reading, theater_ratio, 2024, 0.21).
narrative_ontology:measurement(clim_tr_t2035, climate_response_obligation__degrowth_reading, theater_ratio, 2035, 0.22).
narrative_ontology:measurement(clim_tr_t2050, climate_response_obligation__degrowth_reading, theater_ratio, 2050, 0.22).

% Extraction over time
narrative_ontology:measurement(clim_be_t1972, climate_response_obligation__degrowth_reading, base_extractiveness, 1972, 0.15).
narrative_ontology:measurement(clim_be_t1992, climate_response_obligation__degrowth_reading, base_extractiveness, 1992, 0.22).
narrative_ontology:measurement(clim_be_t2000, climate_response_obligation__degrowth_reading, base_extractiveness, 2000, 0.31).
narrative_ontology:measurement(clim_be_t2010, climate_response_obligation__degrowth_reading, base_extractiveness, 2010, 0.43).
narrative_ontology:measurement(clim_be_t2018, climate_response_obligation__degrowth_reading, base_extractiveness, 2018, 0.52).
narrative_ontology:measurement(clim_be_t2024, climate_response_obligation__degrowth_reading, base_extractiveness, 2024, 0.58).
narrative_ontology:measurement(clim_be_t2035, climate_response_obligation__degrowth_reading, base_extractiveness, 2035, 0.62).
narrative_ontology:measurement(clim_be_t2050, climate_response_obligation__degrowth_reading, base_extractiveness, 2050, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t1972, climate_response_obligation__degrowth_reading, suppression_requirement, 1972, 0.1).
narrative_ontology:measurement(clim_su_t1992, climate_response_obligation__degrowth_reading, suppression_requirement, 1992, 0.15).
narrative_ontology:measurement(clim_su_t2000, climate_response_obligation__degrowth_reading, suppression_requirement, 2000, 0.22).
narrative_ontology:measurement(clim_su_t2010, climate_response_obligation__degrowth_reading, suppression_requirement, 2010, 0.28).
narrative_ontology:measurement(clim_su_t2018, climate_response_obligation__degrowth_reading, suppression_requirement, 2018, 0.33).
narrative_ontology:measurement(clim_su_t2024, climate_response_obligation__degrowth_reading, suppression_requirement, 2024, 0.36).
narrative_ontology:measurement(clim_su_t2035, climate_response_obligation__degrowth_reading, suppression_requirement, 2035, 0.38).
narrative_ontology:measurement(clim_su_t2050, climate_response_obligation__degrowth_reading, suppression_requirement, 2050, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_response_obligation__degrowth_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(climate_response_obligation__degrowth_reading, 0.18).
narrative_ontology:affects_constraint(climate_response_obligation__degrowth_reading, climate_response_obligation__mitigation_priority).
narrative_ontology:affects_constraint(climate_response_obligation__degrowth_reading, climate_response_obligation__adaptation_priority).

% DUAL FORMULATION NOTE:
% Part of the climate_response_obligation constraint family (kernel decomposition). This reading (degrowth) asserts planetary boundaries as hard caps requiring throughput contraction; mitigation_priority asserts rapid decarbonization within growth; adaptation_priority asserts resilience investment as primary. The three readings share the kernel (obligation to respond) but instantiate different constraints with different beneficiary/victim structures and ε values. degrowth_reading has higher ε (0.62) because it targets structural growth drivers; mitigation_priority has lower ε (targets emissions intensity); adaptation_priority has lowest ε (targets vulnerability, not drivers).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(climate_response_obligation__degrowth_reading, institutional, 0.35).
constraint_indexing:directionality_override(climate_response_obligation__degrowth_reading, organized, 0.25).
constraint_indexing:directionality_override(climate_response_obligation__degrowth_reading, powerful, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
