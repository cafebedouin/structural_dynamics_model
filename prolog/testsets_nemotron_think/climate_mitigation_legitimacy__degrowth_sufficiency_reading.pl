% ============================================================================
% CONSTRAINT STORY: climate_mitigation_legitimacy__degrowth_sufficiency_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_mitigation_legitimacy__degrowth_sufficiency_reading, []).

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
 *   constraint_id: climate_mitigation_legitimacy__degrowth_sufficiency_reading
 *   human_readable: Degrowth Sufficiency Reading: Demand Reduction Obviates Generation Expansion
 *   domain: energy_policy/climate_mitigation/technology_governance
 *
 * SUMMARY:
 *   This constraint story instantiates the degrowth_sufficiency_reading of
 *   the contested kernel 'climate_mitigation_legitimacy'. The reading asserts
 *   that decarbonization can and should be achieved primarily through demand
 *   reduction and sufficiency measures, rendering large-scale generation
 *   expansion (both nuclear and utility-scale renewables) unnecessary. This
 *   positions the nuclear industry, utility-scale renewables developers,
 *   energy-intensive industries, and grid infrastructure capital as
 *   victims/payers — their growth-dependent business models are structurally
 *   incompatible with the reading's core premise. The constraint operates as
 *   a tangled rope: it performs a genuine coordination function (aligning
 *   societal energy use with carbon budgets) while asymmetrically extracting
 *   from incumbent energy sectors. Active enforcement (carbon pricing,
 *   rationing, efficiency mandates, zoning) is required to suppress
 *   generation expansion and lock in demand reduction.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_mitigation_legitimacy__degrowth_sufficiency_reading, 0.58).
domain_priors:suppression_score(climate_mitigation_legitimacy__degrowth_sufficiency_reading, 0.45).
domain_priors:theater_ratio(climate_mitigation_legitimacy__degrowth_sufficiency_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__degrowth_sufficiency_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__degrowth_sufficiency_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__degrowth_sufficiency_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__degrowth_sufficiency_reading, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__degrowth_sufficiency_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_mitigation_legitimacy__degrowth_sufficiency_reading, tangled_rope).
narrative_ontology:human_readable(climate_mitigation_legitimacy__degrowth_sufficiency_reading, "Degrowth Sufficiency Reading: Demand Reduction Obviates Generation Expansion").
narrative_ontology:topic_domain(climate_mitigation_legitimacy__degrowth_sufficiency_reading, "energy_policy/climate_mitigation/technology_governance").

domain_priors:requires_active_enforcement(climate_mitigation_legitimacy__degrowth_sufficiency_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_mitigation_legitimacy__degrowth_sufficiency_reading, '49eb445e-0a54-4ead-841f-e18a9f832a83').
narrative_ontology:cs_kernel_codification('49eb445e-0a54-4ead-841f-e18a9f832a83', distributed).
narrative_ontology:cs_authority_grounding('49eb445e-0a54-4ead-841f-e18a9f832a83', practice).
narrative_ontology:cs_interpretation_layer_present('49eb445e-0a54-4ead-841f-e18a9f832a83').
narrative_ontology:cs_reading_relation('49eb445e-0a54-4ead-841f-e18a9f832a83', climate_mitigation_legitimacy__baseload_necessity_reading, coexists_with).
narrative_ontology:cs_reading_relation('49eb445e-0a54-4ead-841f-e18a9f832a83', climate_mitigation_legitimacy__renewable_primacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('49eb445e-0a54-4ead-841f-e18a9f832a83', climate_mitigation_legitimacy__portfolio_pragmatism_reading, coexists_with).
narrative_ontology:cs_axiom('49eb445e-0a54-4ead-841f-e18a9f832a83', foundational, demand_reduction_suffices_for_decarbonization).
narrative_ontology:cs_axiom_status(demand_reduction_suffices_for_decarbonization, holdable).
narrative_ontology:cs_axiom_grounding('49eb445e-0a54-4ead-841f-e18a9f832a83', demand_reduction_suffices_for_decarbonization, empirically_contingent).
narrative_ontology:cs_axiom('49eb445e-0a54-4ead-841f-e18a9f832a83', foundational, large_scale_generation_expansion_is_unnecessary_and_harmful).
narrative_ontology:cs_axiom_status(large_scale_generation_expansion_is_unnecessary_and_harmful, holdable).
narrative_ontology:cs_axiom_grounding('49eb445e-0a54-4ead-841f-e18a9f832a83', large_scale_generation_expansion_is_unnecessary_and_harmful, empirically_contingent).
narrative_ontology:cs_reference_frame('49eb445e-0a54-4ead-841f-e18a9f832a83', sufficiency_first_mitigation_paradigm).
narrative_ontology:cs_drift_state('49eb445e-0a54-4ead-841f-e18a9f832a83', post_ipcc_ar6_wg3, gap(revival_pressure, substantial, false)).
narrative_ontology:cs_created_at('49eb445e-0a54-4ead-841f-e18a9f832a83', '').
narrative_ontology:cs_kernel_id(climate_mitigation_legitimacy__degrowth_sufficiency_reading, climate_mitigation_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__degrowth_sufficiency_reading, sufficiency_advocates).
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__degrowth_sufficiency_reading, energy_poor_communities).
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__degrowth_sufficiency_reading, local_resilience_practitioners).
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__degrowth_sufficiency_reading, post_growth_policy_networks).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__degrowth_sufficiency_reading, nuclear_industry).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__degrowth_sufficiency_reading, utility_scale_renewables_developers).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__degrowth_sufficiency_reading, energy_intensive_industries).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__degrowth_sufficiency_reading, grid_infrastructure_capital).
narrative_ontology:constraint_vindicates(climate_mitigation_legitimacy__degrowth_sufficiency_reading, energy_sufficiency_principle).
narrative_ontology:constraint_vindicates(climate_mitigation_legitimacy__degrowth_sufficiency_reading, demand_side_mitigation_primacy).
narrative_ontology:constraint_vindicates(climate_mitigation_legitimacy__degrowth_sufficiency_reading, growth_independent_wellbeing).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Advocate for policies that reduce aggregate energy demand through sufficiency measures (caps, quotas, lifestyle change, circular economy). Their professional and ideological identity is fused with the demand-reduction framing; exit means abandoning the core premise of their work. They gain legitimacy, funding, and policy influence when this reading prevails.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__degrowth_sufficiency_reading, sufficiency_advocates, agenda_setter,
    organized, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(climate_mitigation_legitimacy__degrowth_sufficiency_reading, sufficiency_advocates, beneficiary).

% Communities lacking reliable energy access who would benefit from redistribution of existing energy services rather than waiting for large-scale generation build-out. They have no meaningful exit from energy poverty and no organized voice in mitigation strategy debates. The reading's promise of 'sufficiency for all' positions them as primary beneficiaries, but they do not set the agenda.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__degrowth_sufficiency_reading, energy_poor_communities, beneficiary,
    powerless, biographical, trapped, global).

% Community energy groups, transition towns, and decentralized practitioners who build low-energy local systems. They benefit from policy frameworks that privilege demand reduction and distributed sufficiency over centralized expansion. Exit is constrained by sunk investment in local infrastructure and community bonds.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__degrowth_sufficiency_reading, local_resilience_practitioners, beneficiary,
    moderate, biographical, constrained, local).

% Academic, NGO, and policy networks advancing post-growth economics. They gain institutional relevance and funding when mitigation strategy centers demand reduction. Their identity is bound to the growth-critique frame; exit means leaving the field. They co-set the agenda with sufficiency advocates.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__degrowth_sufficiency_reading, post_growth_policy_networks, beneficiary,
    organized, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(climate_mitigation_legitimacy__degrowth_sufficiency_reading, post_growth_policy_networks, agenda_setter).

% Nuclear vendors, operators, and supply chains whose business model depends on large-scale, long-horizon generation deployment. This reading declares their core product unnecessary for decarbonization. They face stranded asset risk and loss of policy support. Exit is constrained by massive sunk capital and regulatory lock-in; they fight through lobbying and baseload_necessity_reading advocacy.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__degrowth_sufficiency_reading, nuclear_industry, payer,
    institutional, generational, constrained, global).

% Solar, wind, and storage developers at utility scale. This reading treats their expansion as growth-dependent and unnecessary, directly threatening their project pipelines and valuation models. They have more exit flexibility than nuclear (modular tech, shorter lead times) but are locked into growth-dependent finance structures. They advocate renewable_primacy_reading as counter-framing.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__degrowth_sufficiency_reading, utility_scale_renewables_developers, payer,
    powerful, biographical, constrained, global).

% Steel, cement, chemicals, aluminum — sectors whose competitiveness depends on abundant cheap energy. Demand reduction policies (rationing, efficiency mandates, carbon pricing) raise their costs or cap their output. They have constrained exit: relocation risks carbon leakage, efficiency gains have limits, and they wield significant political influence to resist.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__degrowth_sufficiency_reading, energy_intensive_industries, payer,
    powerful, biographical, constrained, global).

% Transmission operators, distribution utilities, and infrastructure investors whose asset base grows with generation expansion. A sufficiency-led pathway reduces the need for grid build-out, threatening regulated returns. Exit is constrained by regulatory compacts and the physical sunk network; they advocate portfolio_pragmatism_reading to maintain expansion logic.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__degrowth_sufficiency_reading, grid_infrastructure_capital, payer,
    institutional, generational, constrained, continental).

% Countries and coalitions (G77, LDCs, African Group) demanding energy access and development space. They are structurally excluded from the sufficiency framing which centers on reducing demand in already-high-consuming nations. Their objection — that sufficiency in the North cannot substitute for energy access in the South — is not heard in the reading's internal logic. They have no exit from the global energy inequality this reading does not address.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__degrowth_sufficiency_reading, global_south_development_blocks, excluded,
    organized, generational, trapped, global).

% Modelers, scenario builders, and assessment bodies (IPCC, IEA, national academies) who evaluate mitigation pathways. They observe the contest between readings and produce the evidence base each side cites. Their exit is analytical — they can shift methodological frames but remain bound to the epistemic standards of their institutions.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__degrowth_sufficiency_reading, climate_policy_analysts, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a society-wide reduction in energy demand through sufficiency policies (caps, quotas, standards, cultural shifts) so that decarbonization is achieved without expanding generation capacity. Solves the collective action problem of aligning individual consumption with a shared carbon budget.
% TRANSFER_FUNCTION: Moves the burden of decarbonization from capital-intensive supply-side deployment (nuclear, utility renewables, grid) to demand-side behavioral and structural change. Transfers cost and effort from energy industries and infrastructure capital to consumers, communities, and regulatory apparatus. Transfers political legitimacy from growth-dependent energy sectors to sufficiency advocates.
% ABSENT_VOICES: Global South development blocks (G77, LDCs, African Group) who need energy access expansion, not demand reduction. Industrial labor unions in energy-intensive sectors who face job losses without just transition guarantees. Rural and remote communities for whom decentralized sufficiency is impractical. These voices are excluded because the reading assumes a high-baseline consumption context where reduction is feasible.
% DISAPPEARANCE_RATIONALE: If the sufficiency reading vanished, mitigation policy would default to supply-side expansion (nuclear, renewables, grid) as the primary decarbonization lever. Capital flows, regulatory frameworks, and industrial strategy would reorient around generation build-out. The political coalition around demand reduction would dissolve. The world rearranges because the reading currently shapes a non-trivial share of policy discourse and funding (EU sufficiency targets, IPCC demand-side chapters, post-growth municipal programs).
% FOUNDING_PROBLEM: The recognition that supply-side decarbonization alone cannot meet carbon budgets in time due to deployment speed limits, material constraints, land use conflicts, and Jevons paradox rebound effects. The arrangement was built to argue that demand reduction is the necessary complement — or substitute — for generation expansion.
% FOUNDING_PROBLEM_CORROBORATION: IPCC AR6 WG3 Chapter 5 (demand, services, and social aspects) and the High-Level Panel on a Sustainable Ocean Economy corroborate that demand-side measures can deliver 40-70% of emissions reductions by 2050. However, the IEA Net Zero Roadmap and most national long-term strategies still center supply expansion. The founding problem (speed/scale limits of supply-side) is attested by bodies outside the sufficiency coalition, but its sufficiency (demand reduction alone suffices) remains contested.
narrative_ontology:disappearance_verdict(climate_mitigation_legitimacy__degrowth_sufficiency_reading, world_rearranges).
narrative_ontology:founding_problem_status(climate_mitigation_legitimacy__degrowth_sufficiency_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_mitigation_legitimacy__degrowth_sufficiency_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(climate_mitigation_legitimacy__degrowth_sufficiency_reading, 'none', 1).
narrative_ontology:epsilon_provenance(climate_mitigation_legitimacy__degrowth_sufficiency_reading, 0.58, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_mitigation_legitimacy__degrowth_sufficiency_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_mitigation_legitimacy__degrowth_sufficiency_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_mitigation_legitimacy__degrowth_sufficiency_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) reflects the substantial transfer from supply-side incumbents to demand-side coordination. Suppression (0.45) is moderate — the constraint requires policy enforcement to block generation projects and mandate demand reduction, but not total coercion. Theater ratio (0.25) is low: the coordination function (demand-side mitigation) is real and empirically grounded in IPCC literature. Accessibility collapse (0.52) is moderate: supply-side alternatives are not physically impossible but are politically and economically suppressed by the reading's logic. Resistance (0.72) is high: powerful institutional actors (nuclear, renewables, heavy industry, grid capital) actively contest this reading through lobbying, counter-scenarios, and political alliances. The claimed type is tangled_rope because the constraint coordinates a real collective action (demand reduction within carbon budget) while extracting from identifiable victims who lose revenue, asset value, and policy support.
 *
 * PERSPECTIVAL GAP:
 *   From the sufficiency advocate seat, the constraint is a genuine coordination mechanism (rope-like) solving the carbon budget alignment problem. From the nuclear/renewables/industry seats, it is an extractive suppression of their legitimate decarbonization contribution (snare-like). From the energy-poor community seat, it is a promise of redistribution that may not materialize without supply-side expansion. The engine computes this divergence from the declared power/exit/role structure. The reading's claim that renewables are also 'victims' (growth-dependent) creates a distinctive seat divergence: utility-scale renewables developers, usually framed as climate allies, experience this reading as extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Sufficiency advocates and post-growth networks are agenda_setters with identity_locked exit — their professional identity is fused to the reading. Energy-poor communities are beneficiaries but powerless and trapped — they gain from redistribution but have no voice. Local resilience practitioners are beneficiaries with constrained exit (sunk community investment). Nuclear industry, utility-scale renewables, energy-intensive industries, and grid capital are payers with constrained exit (massive sunk assets, regulatory lock-in). Global South development blocks are excluded — the reading's logic assumes high-baseline consumption contexts and does not address energy access needs. Climate policy analysts are observers with analytical exit. The engine computes directionality from these structural positions: payers sit at high d (target), beneficiaries at low d, agenda_setters near beneficiary end but with identity_lock amplifying their stake.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (supply-side speed/scale limits) remains live per IPCC and IEA evidence. However, the reading's proposed solution (demand reduction alone suffices) has not been demonstrated at scale. The constraint persists not because the coordination function is fulfilled but because the supply-side expansion it opposes continues to face delays and cost overruns, feeding the reading's relevance. No mandatrophy resolution — the arrangement has not outlived its function because the function (redirecting mitigation to demand-side) is arguably more urgent now. Theater ratio remains low because the coordination activity (sufficiency policy design, modeling, advocacy) is substantive, not performative.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_structure,
    'Is the degrowth_sufficiency_reading a distinct constraint from its sibling readings, or a parameter variation within a single mitigation strategy space?',
    'Test ε-invariance: if measuring the constraint via ''demand reduction potential'' yields low extraction but measuring via ''supply-side displacement'' yields high extraction, the ε-invariance principle requires decomposition. This story treats it as a distinct constraint with its own ε, beneficiaries, victims, and type.',
    'If the readings are parameter variations of one constraint, the engine should classify a single constraint with observer-dependent χ. If they are distinct constraints (this story''s premise), each gets its own ε and classification. The current corpus structure assumes distinct constraints linked by network.affects_constraints.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_structure, conceptual, 'Whether the four mitigation legitimacy readings are structurally distinct constraints or observational frames on one constraint.').

omega_variable(
    demand_reduction_feasibility,
    'Can demand reduction at the scale this reading requires (40-70% of emissions per IPCC) be achieved without authoritarian suppression or catastrophic welfare loss?',
    'Empirical evidence from sufficiency policy implementations (EU energy efficiency directives, carbon rationing pilots, pandemic demand shifts) and macroeconomic modeling of degrowth scenarios.',
    'If feasible with democratic governance, the reading''s coordination function is genuine and extraction is the price of coordination. If infeasible without authoritarian measures, suppression is underestimated and the constraint trends toward snare. If feasible only with welfare loss, the energy_poor_communities beneficiary claim is undermined.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(demand_reduction_feasibility, empirical, 'Whether the reading''s core empirical claim (demand reduction suffices) is realizable within its own normative constraints.').

omega_variable(
    renewables_as_victim_ambiguity,
    'Does classifying utility-scale renewables developers as victims/payers reflect structural reality or rhetorical strategy?',
    'Track whether renewables developers actually oppose sufficiency policies or whether they adapt (e.g., pivot to distributed generation, efficiency services). Observe lobbying positions on demand-side measures.',
    'If renewables developers actively resist sufficiency policies, the victim classification holds. If they adapt and capture sufficiency-adjacent markets, the extraction is lower and the constraint may be more rope-like. This affects the tangled_rope vs rope boundary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(renewables_as_victim_ambiguity, empirical, 'Whether the reading''s structural claim that renewables are growth-dependent victims matches their actual strategic behavior.').

omega_variable(
    global_south_exclusion_mechanism,
    'Is the exclusion of Global South development needs a structural feature of the sufficiency reading or a contingent framing gap?',
    'Analyze whether sufficiency literature and policy proposals (e.g., EU sufficiency targets, IPCC demand-side chapters) explicitly address energy access for the 750M without electricity and 2.6B without clean cooking. Track whether sufficiency advocates propose differentiated pathways.',
    'If structural, the reading carries an inherent justice deficit that undermines its coordination legitimacy at global scale — the constraint becomes a partial snare for the excluded. If contingent, the reading can be extended without changing its core logic.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(global_south_exclusion_mechanism, conceptual, 'Whether the reading''s silence on Global South energy access is a structural exclusion or an addressable gap.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_mitigation_legitimacy__degrowth_sufficiency_reading, 2015, 2050).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(climate_mitigation_legitimacy__degrowth_sufficiency_reading_tr_t2015, climate_mitigation_legitimacy__degrowth_sufficiency_reading, theater_ratio, 2015, 0.15).
narrative_ontology:measurement(climate_mitigation_legitimacy__degrowth_sufficiency_reading_tr_t2020, climate_mitigation_legitimacy__degrowth_sufficiency_reading, theater_ratio, 2020, 0.18).
narrative_ontology:measurement(climate_mitigation_legitimacy__degrowth_sufficiency_reading_tr_t2025, climate_mitigation_legitimacy__degrowth_sufficiency_reading, theater_ratio, 2025, 0.22).
narrative_ontology:measurement(climate_mitigation_legitimacy__degrowth_sufficiency_reading_tr_t2030, climate_mitigation_legitimacy__degrowth_sufficiency_reading, theater_ratio, 2030, 0.24).
narrative_ontology:measurement(climate_mitigation_legitimacy__degrowth_sufficiency_reading_tr_t2035, climate_mitigation_legitimacy__degrowth_sufficiency_reading, theater_ratio, 2035, 0.25).
narrative_ontology:measurement(climate_mitigation_legitimacy__degrowth_sufficiency_reading_tr_t2040, climate_mitigation_legitimacy__degrowth_sufficiency_reading, theater_ratio, 2040, 0.25).
narrative_ontology:measurement(climate_mitigation_legitimacy__degrowth_sufficiency_reading_tr_t2045, climate_mitigation_legitimacy__degrowth_sufficiency_reading, theater_ratio, 2045, 0.25).
narrative_ontology:measurement(climate_mitigation_legitimacy__degrowth_sufficiency_reading_tr_t2050, climate_mitigation_legitimacy__degrowth_sufficiency_reading, theater_ratio, 2050, 0.25).

% Extraction over time
narrative_ontology:measurement(climate_mitigation_legitimacy__degrowth_sufficiency_reading_be_t2015, climate_mitigation_legitimacy__degrowth_sufficiency_reading, base_extractiveness, 2015, 0.35).
narrative_ontology:measurement(climate_mitigation_legitimacy__degrowth_sufficiency_reading_be_t2020, climate_mitigation_legitimacy__degrowth_sufficiency_reading, base_extractiveness, 2020, 0.42).
narrative_ontology:measurement(climate_mitigation_legitimacy__degrowth_sufficiency_reading_be_t2025, climate_mitigation_legitimacy__degrowth_sufficiency_reading, base_extractiveness, 2025, 0.48).
narrative_ontology:measurement(climate_mitigation_legitimacy__degrowth_sufficiency_reading_be_t2030, climate_mitigation_legitimacy__degrowth_sufficiency_reading, base_extractiveness, 2030, 0.53).
narrative_ontology:measurement(climate_mitigation_legitimacy__degrowth_sufficiency_reading_be_t2035, climate_mitigation_legitimacy__degrowth_sufficiency_reading, base_extractiveness, 2035, 0.56).
narrative_ontology:measurement(climate_mitigation_legitimacy__degrowth_sufficiency_reading_be_t2040, climate_mitigation_legitimacy__degrowth_sufficiency_reading, base_extractiveness, 2040, 0.57).
narrative_ontology:measurement(climate_mitigation_legitimacy__degrowth_sufficiency_reading_be_t2045, climate_mitigation_legitimacy__degrowth_sufficiency_reading, base_extractiveness, 2045, 0.58).
narrative_ontology:measurement(climate_mitigation_legitimacy__degrowth_sufficiency_reading_be_t2050, climate_mitigation_legitimacy__degrowth_sufficiency_reading, base_extractiveness, 2050, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(climate_mitigation_legitimacy__degrowth_sufficiency_reading_su_t2015, climate_mitigation_legitimacy__degrowth_sufficiency_reading, suppression_requirement, 2015, 0.25).
narrative_ontology:measurement(climate_mitigation_legitimacy__degrowth_sufficiency_reading_su_t2020, climate_mitigation_legitimacy__degrowth_sufficiency_reading, suppression_requirement, 2020, 0.32).
narrative_ontology:measurement(climate_mitigation_legitimacy__degrowth_sufficiency_reading_su_t2025, climate_mitigation_legitimacy__degrowth_sufficiency_reading, suppression_requirement, 2025, 0.38).
narrative_ontology:measurement(climate_mitigation_legitimacy__degrowth_sufficiency_reading_su_t2030, climate_mitigation_legitimacy__degrowth_sufficiency_reading, suppression_requirement, 2030, 0.42).
narrative_ontology:measurement(climate_mitigation_legitimacy__degrowth_sufficiency_reading_su_t2035, climate_mitigation_legitimacy__degrowth_sufficiency_reading, suppression_requirement, 2035, 0.44).
narrative_ontology:measurement(climate_mitigation_legitimacy__degrowth_sufficiency_reading_su_t2040, climate_mitigation_legitimacy__degrowth_sufficiency_reading, suppression_requirement, 2040, 0.45).
narrative_ontology:measurement(climate_mitigation_legitimacy__degrowth_sufficiency_reading_su_t2045, climate_mitigation_legitimacy__degrowth_sufficiency_reading, suppression_requirement, 2045, 0.45).
narrative_ontology:measurement(climate_mitigation_legitimacy__degrowth_sufficiency_reading_su_t2050, climate_mitigation_legitimacy__degrowth_sufficiency_reading, suppression_requirement, 2050, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_mitigation_legitimacy__degrowth_sufficiency_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(climate_mitigation_legitimacy__degrowth_sufficiency_reading, 0.12).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__degrowth_sufficiency_reading, climate_mitigation_legitimacy__baseload_necessity_reading).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__degrowth_sufficiency_reading, climate_mitigation_legitimacy__renewable_primacy_reading).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__degrowth_sufficiency_reading, climate_mitigation_legitimacy__portfolio_pragmatism_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of four readings of the kernel 'climate_mitigation_legitimacy'. The ε-invariance principle requires separate stories because each reading has a distinct beneficiary/victim structure and extractiveness profile. The degrowth reading uniquely makes both nuclear and utility renewables victims; the renewable_primacy reading makes nuclear a victim but renewables a beneficiary; the baseload reading makes renewables a victim but nuclear a beneficiary; the portfolio reading attempts to make both beneficiaries. These are not observable variations — they are structurally distinct constraints with different coordination and extraction architectures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(climate_mitigation_legitimacy__degrowth_sufficiency_reading, organized, 0.15).
constraint_indexing:directionality_override(climate_mitigation_legitimacy__degrowth_sufficiency_reading, powerful, 0.82).
constraint_indexing:directionality_override(climate_mitigation_legitimacy__degrowth_sufficiency_reading, institutional, 0.78).
constraint_indexing:directionality_override(climate_mitigation_legitimacy__degrowth_sufficiency_reading, powerless, 0.25).
constraint_indexing:directionality_override(climate_mitigation_legitimacy__degrowth_sufficiency_reading, moderate, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
