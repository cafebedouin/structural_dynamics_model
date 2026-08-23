% ============================================================================
% CONSTRAINT STORY: climate_response_action__degrowth_transformation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_response_action__degrowth_transformation, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: climate_response_action__degrowth_transformation
 *   human_readable: Degrowth Climate Response: Structural Economic Transformation Rejecting GDP Growth
 *   domain: climate_policy/political_economy/intergenerational_ethics
 *
 * SUMMARY:
 *   This constraint story models the degrowth_transformation reading of the
 *   climate_response_action kernel. It proposes that effective climate
 *   response requires rejecting GDP growth as an organizing principle and
 *   implementing structural economic transformation: universal basic
 *   services, working time reduction, democratic firm ownership, and planned
 *   throughput reduction. The arrangement coordinates human provisioning
 *   within planetary boundaries while redistributing throughput entitlement
 *   from Global North over-consumption to Global South development needs and
 *   future generations' climate stability. It faces extreme political
 *   feasibility barriers from incumbent capital, growth-dependent
 *   institutions, and identity-fused consumer populations. The claimed type
 *   is tangled_rope — genuine coordination function (solving climate
 *   collective action within biophysical limits) with asymmetric extraction
 *   (redistribution from wealthy/powerful to vulnerable/future).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_action__degrowth_transformation, 0.68).
domain_priors:suppression_score(climate_response_action__degrowth_transformation, 0.45).
domain_priors:theater_ratio(climate_response_action__degrowth_transformation, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_action__degrowth_transformation, extractiveness, 0.68).
narrative_ontology:constraint_metric(climate_response_action__degrowth_transformation, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(climate_response_action__degrowth_transformation, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_response_action__degrowth_transformation, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(climate_response_action__degrowth_transformation, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_action__degrowth_transformation, tangled_rope).
narrative_ontology:human_readable(climate_response_action__degrowth_transformation, "Degrowth Climate Response: Structural Economic Transformation Rejecting GDP Growth").
narrative_ontology:topic_domain(climate_response_action__degrowth_transformation, "climate_policy/political_economy/intergenerational_ethics").

domain_priors:requires_active_enforcement(climate_response_action__degrowth_transformation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_action__degrowth_transformation, 'dd1eecd7-b050-42ca-aa15-5e19647a05ff').
narrative_ontology:cs_kernel_codification('dd1eecd7-b050-42ca-aa15-5e19647a05ff', distributed).
narrative_ontology:cs_authority_grounding('dd1eecd7-b050-42ca-aa15-5e19647a05ff', practice).
narrative_ontology:cs_reading_relation('dd1eecd7-b050-42ca-aa15-5e19647a05ff', climate_response_action__mitigation_priority, forecloses).
narrative_ontology:cs_reading_relation('dd1eecd7-b050-42ca-aa15-5e19647a05ff', climate_response_action__adaptation_priority, influences).
narrative_ontology:cs_axiom('dd1eecd7-b050-42ca-aa15-5e19647a05ff', foundational, sufficiency_over_growth_as_organizing_principle).
narrative_ontology:cs_axiom_status(sufficiency_over_growth_as_organizing_principle, holdable).
narrative_ontology:cs_axiom_grounding('dd1eecd7-b050-42ca-aa15-5e19647a05ff', sufficiency_over_growth_as_organizing_principle, empirically_contingent).
narrative_ontology:cs_axiom('dd1eecd7-b050-42ca-aa15-5e19647a05ff', foundational, climate_justice_requires_atmospheric_space_redistribution).
narrative_ontology:cs_axiom_status(climate_justice_requires_atmospheric_space_redistribution, holdable).
narrative_ontology:cs_axiom_grounding('dd1eecd7-b050-42ca-aa15-5e19647a05ff', climate_justice_requires_atmospheric_space_redistribution, deontological).
narrative_ontology:cs_axiom('dd1eecd7-b050-42ca-aa15-5e19647a05ff', secondary, intergenerational_equity_requires_burden_shift_to_current_wealthy).
narrative_ontology:cs_axiom_status(intergenerational_equity_requires_burden_shift_to_current_wealthy, holdable).
narrative_ontology:cs_axiom_grounding('dd1eecd7-b050-42ca-aa15-5e19647a05ff', intergenerational_equity_requires_burden_shift_to_current_wealthy, deontological).
narrative_ontology:cs_reference_frame('dd1eecd7-b050-42ca-aa15-5e19647a05ff', growth_paradigm_hegemony).
narrative_ontology:cs_drift_state('dd1eecd7-b050-42ca-aa15-5e19647a05ff', post_paris_agreement_implementation_gap, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('dd1eecd7-b050-42ca-aa15-5e19647a05ff', '').
narrative_ontology:cs_kernel_id(climate_response_action__degrowth_transformation, climate_response_action).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_action__degrowth_transformation, global_south_populations).
narrative_ontology:constraint_beneficiary(climate_response_action__degrowth_transformation, future_generations).
narrative_ontology:constraint_beneficiary(climate_response_action__degrowth_transformation, climate_vulnerable_populations).
narrative_ontology:constraint_beneficiary(climate_response_action__degrowth_transformation, care_economy_workers).
narrative_ontology:constraint_victim(climate_response_action__degrowth_transformation, global_north_wealthy_consumers).
narrative_ontology:constraint_victim(climate_response_action__degrowth_transformation, fossil_fuel_capital).
narrative_ontology:constraint_victim(climate_response_action__degrowth_transformation, high_throughput_industrial_sectors).
narrative_ontology:constraint_victim(climate_response_action__degrowth_transformation, growth_dependent_financial_assets).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(climate_response_action__degrowth_transformation, global_north_working_class).
narrative_ontology:constraint_victim(climate_response_action__degrowth_transformation, care_economy_workers).
narrative_ontology:constraint_victim(climate_response_action__degrowth_transformation, mainstream_climate_institutions).
narrative_ontology:constraint_victim(climate_response_action__degrowth_transformation, global_north_working_class).
narrative_ontology:constraint_vindicates(climate_response_action__degrowth_transformation, planetary_boundaries_hard_constraint).
narrative_ontology:constraint_vindicates(climate_response_action__degrowth_transformation, intergenerational_equity_principle).
narrative_ontology:constraint_vindicates(climate_response_action__degrowth_transformation, climate_justice_as_redistribution).
narrative_ontology:constraint_vindicates(climate_response_action__degrowth_transformation, sufficiency_over_efficiency).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% High-consumption populations in OECD countries who would face reduced material throughput, lifestyle changes, and potential wealth redistribution. Exit options constrained by national borders, asset lock-in, and identity fusion with consumerist lifestyles. Bear direct costs of transformation through taxation, consumption caps, and reduced investment returns.
narrative_ontology:constraint_stakeholder(climate_response_action__degrowth_transformation, global_north_wealthy_consumers, payer,
    powerful, biographical, constrained, global).

% Extraction and combustion industries facing stranded assets and regulatory phase-out. Hold structural agenda-setting power through lobbying, media ownership, and political capture, but face existential threat to asset values. Exit constrained by sunk infrastructure and lack of viable alternative business models at current scale.
narrative_ontology:constraint_stakeholder(climate_response_action__degrowth_transformation, fossil_fuel_capital, payer,
    institutional, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(climate_response_action__degrowth_transformation, fossil_fuel_capital, agenda_setter).

% Cement, steel, aviation, shipping, industrial agriculture — sectors where throughput reduction directly threatens profit models. Organized through trade associations with regulatory influence. Exit options limited by capital intensity and lack of low-throughput alternatives at scale.
narrative_ontology:constraint_stakeholder(climate_response_action__degrowth_transformation, high_throughput_industrial_sectors, payer,
    organized, biographical, constrained, global).

% Pension funds, sovereign wealth funds, private equity requiring compound growth for solvency. Hold arbitrage-grade exit (capital mobility, asset switching) but face systemic risk if growth paradigm collapses. Extraction manifests as pressure for policy capture to maintain growth assumptions in valuation models.
narrative_ontology:constraint_stakeholder(climate_response_action__degrowth_transformation, growth_dependent_financial_assets, payer,
    institutional, biographical, arbitrage, global).

% Populations in Global South claiming atmospheric space and development rights. Benefit from redistribution of throughput quota, technology transfer, and climate finance. Structurally trapped by global economic governance, debt architecture, and border regimes. Limited exit from extractive supply chains.
narrative_ontology:constraint_stakeholder(climate_response_action__degrowth_transformation, global_south_populations, beneficiary,
    moderate, generational, trapped, global).

% All humans not yet born who inherit climate outcomes. Ultimate beneficiaries of avoided catastrophic warming and preserved biosphere. Completely excluded from current decision-making — no voice, no vote, no market power. Trapped in temporal asymmetry: bear consequences of today's choices with zero influence.
narrative_ontology:constraint_stakeholder(climate_response_action__degrowth_transformation, future_generations, beneficiary,
    powerless, civilizational, trapped, universal).
narrative_ontology:stakeholder_secondary_role(climate_response_action__degrowth_transformation, future_generations, excluded).

% Small island states, Arctic communities, Sahel populations, coastal megacity poor — already experiencing climate damages. Benefit from rapid throughput reduction minimizing further warming. Trapped geographically and economically; migration barriers compound vulnerability.
narrative_ontology:constraint_stakeholder(climate_response_action__degrowth_transformation, climate_vulnerable_populations, beneficiary,
    powerless, immediate, trapped, global).

% Healthcare, education, childcare, eldercare workers — predominantly women, often migrant. Benefit from universal basic services and working time reduction central to degrowth policy. Also pay transition costs through labor market restructuring. Exit constrained by professional licensing, care obligations, and gendered labor segmentation.
narrative_ontology:constraint_stakeholder(climate_response_action__degrowth_transformation, care_economy_workers, beneficiary,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(climate_response_action__degrowth_transformation, care_economy_workers, payer).

% Academics, NGOs, social movements (climate justice, post-growth, ecofeminist, Indigenous networks) proposing and lobbying for the transformation. Set intellectual and political agenda. Mobile exit (can shift focus, institutions, geographies) but identity-locked to the cause — professional and moral identity fused with the reading.
narrative_ontology:constraint_stakeholder(climate_response_action__degrowth_transformation, degrowth_policy_advocates, agenda_setter,
    organized, generational, mobile, global).

% IPCC, UNFCCC, IEA, central banks, major climate finance facilities. Currently aligned with mitigation_priority reading. Would face institutional obsolescence and mandate crisis under degrowth_transformation. Constrained exit — mandates, funding, and legitimacy tied to growth-compatible frameworks.
narrative_ontology:constraint_stakeholder(climate_response_action__degrowth_transformation, mainstream_climate_institutions, agenda_setter,
    institutional, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(climate_response_action__degrowth_transformation, mainstream_climate_institutions, payer).

% Workers in high-throughput sectors facing job transition; also potential beneficiaries of universal basic services, shorter hours, job guarantee. Politically contested — targeted by both degrowth advocates (just transition) and fossil fuel interests (job loss narratives). Exit constrained by skills specificity, geographic immobility, and pension dependence.
narrative_ontology:constraint_stakeholder(climate_response_action__degrowth_transformation, global_north_working_class, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(climate_response_action__degrowth_transformation, global_north_working_class, beneficiary).

% Interdisciplinary climate-economy scholars, Earth system scientists, political ecologists assessing the constraint's structural coherence, biophysical plausibility, and justice implications. No material stake; exit is analytical (can shift frameworks). Sees full structure including kernel contestation.
narrative_ontology:constraint_stakeholder(climate_response_action__degrowth_transformation, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective-action problem of staying within planetary boundaries by replacing growth-based allocation with sufficiency-based provisioning: universal basic services, working time reduction, and democratic resource governance coordinate human needs within biophysical limits without relying on price signals or technological substitution.
% TRANSFER_FUNCTION: Moves material throughput entitlement and financial claims from Global North wealthy consumers and fossil capital to Global South development space and future generations' climate stability. Transfers decision-making power from capital markets to democratic planning institutions. Transfers labor time from commodity production to care and regeneration.
% ABSENT_VOICES: Future generations (temporally excluded), non-human species (ontologically excluded), Global South subsistence producers not represented in climate governance, Indigenous land defenders criminalized for protecting carbon sinks, climate migrants denied legal status. Their absence enables the mitigation_priority reading to frame technological substitution as sufficient.
% DISAPPEARANCE_RATIONALE: If the degrowth transformation constraint vanished overnight, the mitigation_priority reading (green growth, carbon markets, technological substitution) would remain the dominant policy paradigm. Global North throughput would continue rising, carbon budgets would be exceeded, and climate damages would accelerate nonlinearly. The world rearranges toward catastrophic warming — but the rearrangement is the *absence* of this constraint, not its presence. The constraint's presence is what would prevent the rearrangement.
% FOUNDING_PROBLEM: The founding problem is the biophysical impossibility of decoupling GDP growth from resource throughput and emissions at the speed and scale required to stay within 1.5°C or 2°C carbon budgets, combined with the injustice of Global North historical emissions consuming the atmospheric commons owed to Global South development and future generations.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated by: IPCC AR6 WGIII (decoupling rates insufficient), Material Economics / IRP reports (no absolute decoupling at required scale), Global South negotiators at UNFCCC (climate justice demands), Indigenous movements (living alternatives to growth), ecological economists (Hickel, Kallis, Jackson, Raworth) — all outside the direct beneficiary set of any single reading. The mitigation_priority reading's corroboration comes from IEA, OECD, and green growth institutions — contested provenance.
narrative_ontology:disappearance_verdict(climate_response_action__degrowth_transformation, world_rearranges).
narrative_ontology:founding_problem_status(climate_response_action__degrowth_transformation, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_response_action__degrowth_transformation, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(climate_response_action__degrowth_transformation, 'none', 1).
narrative_ontology:epsilon_provenance(climate_response_action__degrowth_transformation, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_response_action__degrowth_transformation_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_response_action__degrowth_transformation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_response_action__degrowth_transformation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) reflects the massive throughput and wealth transfer required — not merely carbon pricing but structural redistribution of material entitlement. Suppression (0.45) is moderate: the constraint requires active enforcement (policy implementation, capital controls, border carbon adjustments) but its legitimacy rests on biophysical necessity and justice claims, not pure coercion. Theater ratio (0.25) is low initially (genuine mobilization) but rises as institutional capture attempts co-opt the language. Accessibility collapse (0.55) — alternatives (green growth) remain cognitively available but biophysically contested. Resistance (0.72) is high: the most powerful institutional and capital interests in history oppose this transformation.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats (Global North wealthy, fossil capital) experience this as extreme extraction — loss of lifestyle, asset stranding, power erosion. The beneficiary seats (Global South, future generations) experience it as long-overdue justice and survival. The agenda_setter seats diverge: degrowth advocates see coordination; mainstream institutions see existential threat to their mandate. The engine computes per-seat types from these structural positions — a claimed tangled_rope that may compute as snare from the fossil capital seat and rope from the Global South seat.
 *
 * DIRECTIONALITY LOGIC:
 *   Global North wealthy consumers and fossil capital are structural targets (d near 1.0) — they bear the throughput reduction and wealth transfer. Global South populations and future generations are structural beneficiaries (d near 0.0) — they receive throughput space and climate stability. Climate vulnerable populations are beneficiaries with trapped exit (d ~0.1). Care economy workers sit near symmetric (d ~0.5) — gain services/time but face labor restructuring. Mainstream climate institutions are agenda_setters who become payers under this reading (institutional obsolescence) — d shifts from ~0.2 to ~0.7. Degrowth advocates are agenda_setters with mobile exit but identity-locked commitment — their d is near beneficiary end for the constraint's success but they bear career risk.
 *
 * MANDATROPHY ANALYSIS:
 *   The mitigation_priority reading (green growth) shows mandatrophy signals: its founding problem (decouple growth from emissions) is empirically dead (no absolute decoupling at scale), yet the arrangement persists and extracts (carbon markets, offsets, techno-optimism delaying real action). The degrowth_transformation reading avoids mandatrophy by making its founding problem (biophysical limits + justice) live and its coordination function explicit. However, if implemented and the climate crisis resolves, the transformation would need a sunset clause or risk becoming a piton — the coordination function (staying within boundaries) would be achieved, but the throughput reduction institutions might persist theatrically.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_structure,
    'How does the degrowth_transformation reading''s structural classification change if the kernel''s other readings (mitigation_priority, adaptation_priority) are treated as coexisting constraints rather than mutually exclusive framings?',
    'Multi-constraint modeling: run engine with all three readings as separate constraints linked via network.affects_constraints. Compare per-seat classifications when seats participate in multiple constraints simultaneously vs. single-constraint analysis.',
    'If mitigation_priority and degrowth_transformation coexist as implemented policies (e.g., carbon pricing AND throughput caps), the extraction profile for each seat becomes the sum across constraints. This could reclassify degrowth_transformation from tangled_rope to snare for fossil capital (double extraction) or rope for Global South (double coordination). The kernel''s structural reality is the superposition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Committer frame structural ambiguity: single-reading vs. multi-reading superposition').

omega_variable(
    biophysical_feasibility_of_sufficiency_provisioning,
    'Can universal basic services and sufficiency provisioning actually meet human needs at lower throughput than current GDP growth trajectories, or does the coordination function collapse into managed scarcity?',
    'Biophysical modeling of need-satisfaction thresholds (Raworth''s Doughnut, O''Neill et al. 2018) vs. current provisioning systems. Empirical test: existing sufficiency-oriented economies (Costa Rica, Kerala, Cuba) — do they achieve high wellbeing at low throughput?',
    'If sufficiency provisioning fails biophysically, the coordination function is illusory — the constraint becomes a snare (extraction without real coordination). If it succeeds, the tangled_rope classification holds: genuine coordination with asymmetric extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(biophysical_feasibility_of_sufficiency_provisioning, empirical, 'Whether the claimed coordination function is biophysically realizable').

omega_variable(
    political_feasibility_of_enforcement,
    'Can the active enforcement required (capital controls, throughput caps, democratic planning) be implemented and sustained against the concentrated power of fossil capital and growth-dependent institutions, or does the constraint collapse into theater?',
    'Historical analogies: wartime mobilization, post-WWII reconstruction, COVID emergency powers. Political economy modeling of coalition possibilities: Global South + climate vulnerable + care workers + youth movements vs. fossil capital + financial assets + high-throughput sectors.',
    'If enforcement is impossible, the constraint is a scaffold that never deploys (theater_ratio → 1.0). If enforcement succeeds but creates new elite capture, it becomes a snare. If enforcement succeeds and remains democratic, tangled_rope holds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(political_feasibility_of_enforcement, empirical, 'Whether the constraint''s enforcement mechanism is politically realizable').

omega_variable(
    intergenerational_voice_representation,
    'How should future generations'' structural position (powerless, trapped, civilizational time horizon) be weighted in the engine''s directionality computation when they have zero exit and zero voice?',
    'Constitutionalize future generations'' interests (Wales Well-being of Future Generations Act, proposed UN Declaration). Test engine sensitivity: vary future_generations stakeholder power from powerless to analytical and observe classification shifts.',
    'If future generations are the primary beneficiary but have zero structural power, the constraint''s legitimacy rests entirely on proxy representation. This creates a principal-agent problem: do degrowth advocates truly represent future generations, or do they extract present-day political capital? Affects mandatrophy analysis.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(intergenerational_voice_representation, conceptual, 'Structural representation of voiceless ultimate beneficiaries').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_action__degrowth_transformation, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(degrowth_transformation_tr_t0, climate_response_action__degrowth_transformation, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(degrowth_transformation_tr_t0, projected).
narrative_ontology:measurement(degrowth_transformation_tr_t5, climate_response_action__degrowth_transformation, theater_ratio, 5, 0.15).
narrative_ontology:measurement_basis(degrowth_transformation_tr_t5, projected).
narrative_ontology:measurement(degrowth_transformation_tr_t10, climate_response_action__degrowth_transformation, theater_ratio, 10, 0.2).
narrative_ontology:measurement_basis(degrowth_transformation_tr_t10, projected).
narrative_ontology:measurement(degrowth_transformation_tr_t15, climate_response_action__degrowth_transformation, theater_ratio, 15, 0.22).
narrative_ontology:measurement_basis(degrowth_transformation_tr_t15, projected).
narrative_ontology:measurement(degrowth_transformation_tr_t20, climate_response_action__degrowth_transformation, theater_ratio, 20, 0.25).
narrative_ontology:measurement_basis(degrowth_transformation_tr_t20, projected).
narrative_ontology:measurement(degrowth_transformation_tr_t25, climate_response_action__degrowth_transformation, theater_ratio, 25, 0.28).
narrative_ontology:measurement_basis(degrowth_transformation_tr_t25, projected).
narrative_ontology:measurement(degrowth_transformation_tr_t30, climate_response_action__degrowth_transformation, theater_ratio, 30, 0.25).
narrative_ontology:measurement_basis(degrowth_transformation_tr_t30, projected).

% Extraction over time
narrative_ontology:measurement(degrowth_transformation_be_t0, climate_response_action__degrowth_transformation, base_extractiveness, 0, 0.35).
narrative_ontology:measurement_basis(degrowth_transformation_be_t0, projected).
narrative_ontology:measurement(degrowth_transformation_be_t5, climate_response_action__degrowth_transformation, base_extractiveness, 5, 0.45).
narrative_ontology:measurement_basis(degrowth_transformation_be_t5, projected).
narrative_ontology:measurement(degrowth_transformation_be_t10, climate_response_action__degrowth_transformation, base_extractiveness, 10, 0.55).
narrative_ontology:measurement_basis(degrowth_transformation_be_t10, projected).
narrative_ontology:measurement(degrowth_transformation_be_t15, climate_response_action__degrowth_transformation, base_extractiveness, 15, 0.62).
narrative_ontology:measurement_basis(degrowth_transformation_be_t15, projected).
narrative_ontology:measurement(degrowth_transformation_be_t20, climate_response_action__degrowth_transformation, base_extractiveness, 20, 0.68).
narrative_ontology:measurement_basis(degrowth_transformation_be_t20, projected).
narrative_ontology:measurement(degrowth_transformation_be_t25, climate_response_action__degrowth_transformation, base_extractiveness, 25, 0.7).
narrative_ontology:measurement_basis(degrowth_transformation_be_t25, projected).
narrative_ontology:measurement(degrowth_transformation_be_t30, climate_response_action__degrowth_transformation, base_extractiveness, 30, 0.68).
narrative_ontology:measurement_basis(degrowth_transformation_be_t30, projected).

% Suppression requirement over time
narrative_ontology:measurement(degrowth_transformation_su_t0, climate_response_action__degrowth_transformation, suppression_requirement, 0, 0.65).
narrative_ontology:measurement_basis(degrowth_transformation_su_t0, projected).
narrative_ontology:measurement(degrowth_transformation_su_t5, climate_response_action__degrowth_transformation, suppression_requirement, 5, 0.55).
narrative_ontology:measurement_basis(degrowth_transformation_su_t5, projected).
narrative_ontology:measurement(degrowth_transformation_su_t10, climate_response_action__degrowth_transformation, suppression_requirement, 10, 0.48).
narrative_ontology:measurement_basis(degrowth_transformation_su_t10, projected).
narrative_ontology:measurement(degrowth_transformation_su_t15, climate_response_action__degrowth_transformation, suppression_requirement, 15, 0.42).
narrative_ontology:measurement_basis(degrowth_transformation_su_t15, projected).
narrative_ontology:measurement(degrowth_transformation_su_t20, climate_response_action__degrowth_transformation, suppression_requirement, 20, 0.4).
narrative_ontology:measurement_basis(degrowth_transformation_su_t20, projected).
narrative_ontology:measurement(degrowth_transformation_su_t25, climate_response_action__degrowth_transformation, suppression_requirement, 25, 0.42).
narrative_ontology:measurement_basis(degrowth_transformation_su_t25, projected).
narrative_ontology:measurement(degrowth_transformation_su_t30, climate_response_action__degrowth_transformation, suppression_requirement, 30, 0.45).
narrative_ontology:measurement_basis(degrowth_transformation_su_t30, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_response_action__degrowth_transformation, resource_allocation).
narrative_ontology:boltzmann_floor_override(climate_response_action__degrowth_transformation, 0.12).
narrative_ontology:affects_constraint(climate_response_action__degrowth_transformation, climate_response_action__mitigation_priority).
narrative_ontology:affects_constraint(climate_response_action__degrowth_transformation, climate_response_action__adaptation_priority).
narrative_ontology:affects_constraint(climate_response_action__degrowth_transformation, global_carbon_budget_allocation).
narrative_ontology:affects_constraint(climate_response_action__degrowth_transformation, international_climate_finance_architecture).
narrative_ontology:affects_constraint(climate_response_action__degrowth_transformation, fossil_fuel_subsidy_regime).

% DUAL FORMULATION NOTE:
% This constraint is the degrowth_transformation reading of the climate_response_action kernel. It forecloses the mitigation_priority reading (growth-compatible decarbonization) within any single policy framework, as their core premises are logically contradictory. It influences the adaptation_priority reading by reducing the adaptation burden through deeper mitigation, but does not foreclose adaptation as a necessary complement. All three readings form a constraint family linked by mutual structural influence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(climate_response_action__degrowth_transformation, institutional, 0.7).
constraint_indexing:directionality_override(climate_response_action__degrowth_transformation, powerful, 0.85).
constraint_indexing:directionality_override(climate_response_action__degrowth_transformation, powerless, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
