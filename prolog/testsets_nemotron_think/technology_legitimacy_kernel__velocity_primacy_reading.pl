% ============================================================================
% CONSTRAINT STORY: technology_legitimacy_kernel__velocity_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_technology_legitimacy_kernel__velocity_primacy_reading, []).

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
 *   constraint_id: technology_legitimacy_kernel__velocity_primacy_reading
 *   human_readable: Velocity-Primacy Legitimacy Gate for Climate Mitigation Technologies
 *   domain: energy_policy/climate_mitigation/technology_governance
 *
 * SUMMARY:
 *   The velocity-primacy reading of technology legitimacy holds that a
 *   climate mitigation technology is legitimate if and only if it can deploy
 *   at scale within the remaining carbon budget timeline (2030/2050 targets).
 *   This reading gained dominance after Paris 2015 as the carbon budget math
 *   made near-term deployment the binding constraint. It operates as a de
 *   facto gate: renewables and storage pass; nuclear, CCS, and advanced
 *   thermal fail on construction timeline. The constraint coordinates global
 *   capital toward fast deployment but extracts asymmetrically — nuclear
 *   industry bears exclusion costs, grid operators bear intermittency
 *   integration costs, fossil communities bear accelerated stranding. The
 *   constraint requires active enforcement through taxonomies, permitting
 *   reform, and finance rules. Theater ratio rises as 'technology neutrality'
 *   rhetoric persists while policy mechanisms structurally favor
 *   velocity-aligned technologies.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(technology_legitimacy_kernel__velocity_primacy_reading, 0.68).
domain_priors:suppression_score(technology_legitimacy_kernel__velocity_primacy_reading, 0.55).
domain_priors:theater_ratio(technology_legitimacy_kernel__velocity_primacy_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(technology_legitimacy_kernel__velocity_primacy_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(technology_legitimacy_kernel__velocity_primacy_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(technology_legitimacy_kernel__velocity_primacy_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(technology_legitimacy_kernel__velocity_primacy_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(technology_legitimacy_kernel__velocity_primacy_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(technology_legitimacy_kernel__velocity_primacy_reading, tangled_rope).
narrative_ontology:human_readable(technology_legitimacy_kernel__velocity_primacy_reading, "Velocity-Primacy Legitimacy Gate for Climate Mitigation Technologies").
narrative_ontology:topic_domain(technology_legitimacy_kernel__velocity_primacy_reading, "energy_policy/climate_mitigation/technology_governance").

domain_priors:requires_active_enforcement(technology_legitimacy_kernel__velocity_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(technology_legitimacy_kernel__velocity_primacy_reading, 'e5b59d9d-3c98-4c6e-9ae4-808c413a28ef').
narrative_ontology:cs_kernel_codification('e5b59d9d-3c98-4c6e-9ae4-808c413a28ef', distributed).
narrative_ontology:cs_authority_grounding('e5b59d9d-3c98-4c6e-9ae4-808c413a28ef', expertise).
narrative_ontology:cs_interpretation_layer_present('e5b59d9d-3c98-4c6e-9ae4-808c413a28ef').
narrative_ontology:cs_reading_relation('e5b59d9d-3c98-4c6e-9ae4-808c413a28ef', technology_legitimacy_kernel__precautionary_reading, coexists_with).
narrative_ontology:cs_reading_relation('e5b59d9d-3c98-4c6e-9ae4-808c413a28ef', technology_legitimacy_kernel__reliability_primacy_reading, coexists_with).
narrative_ontology:cs_axiom('e5b59d9d-3c98-4c6e-9ae4-808c413a28ef', foundational, deployment_velocity_determines_legitimacy).
narrative_ontology:cs_axiom_status(deployment_velocity_determines_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('e5b59d9d-3c98-4c6e-9ae4-808c413a28ef', deployment_velocity_determines_legitimacy, empirically_contingent).
narrative_ontology:cs_axiom('e5b59d9d-3c98-4c6e-9ae4-808c413a28ef', secondary, intermittency_integration_costs_are_system_externalities).
narrative_ontology:cs_axiom_status(intermittency_integration_costs_are_system_externalities, holdable).
narrative_ontology:cs_axiom_grounding('e5b59d9d-3c98-4c6e-9ae4-808c413a28ef', intermittency_integration_costs_are_system_externalities, instrumental).
narrative_ontology:cs_reference_frame('e5b59d9d-3c98-4c6e-9ae4-808c413a28ef', carbon_budget_deployment_window).
narrative_ontology:cs_drift_state('e5b59d9d-3c98-4c6e-9ae4-808c413a28ef', post_paris_agreement_implementation, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('e5b59d9d-3c98-4c6e-9ae4-808c413a28ef', '2026-08-15T14:30:00Z').
narrative_ontology:cs_kernel_id(technology_legitimacy_kernel__velocity_primacy_reading, technology_legitimacy_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(technology_legitimacy_kernel__velocity_primacy_reading, renewable_developers).
narrative_ontology:constraint_beneficiary(technology_legitimacy_kernel__velocity_primacy_reading, battery_storage_industry).
narrative_ontology:constraint_beneficiary(technology_legitimacy_kernel__velocity_primacy_reading, demand_response_aggregators).
narrative_ontology:constraint_victim(technology_legitimacy_kernel__velocity_primacy_reading, nuclear_industry).
narrative_ontology:constraint_victim(technology_legitimacy_kernel__velocity_primacy_reading, grid_operators).
narrative_ontology:constraint_victim(technology_legitimacy_kernel__velocity_primacy_reading, fossil_fuel_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(technology_legitimacy_kernel__velocity_primacy_reading, battery_storage_industry).
narrative_ontology:constraint_vindicates(technology_legitimacy_kernel__velocity_primacy_reading, carbon_budget_urgency_doctrine).
narrative_ontology:constraint_vindicates(technology_legitimacy_kernel__velocity_primacy_reading, deployment_speed_as_primary_metric).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Wind and solar developers whose technologies deploy in 1-3 years align perfectly with the velocity criterion. They receive policy preference, streamlined permitting, and subsidy access. Their exit options are strong — they can deploy across jurisdictions and their core technology is the constraint's favored solution.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__velocity_primacy_reading, renewable_developers, beneficiary,
    organized, biographical, mobile, global).

% Storage developers benefit as essential complements to variable renewables, gaining legitimacy and market access. But they also bear cost pressure from the same velocity mandate — they must scale manufacturing at breakneck pace, absorbing supply chain risk and cost overruns that the constraint treats as their problem.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__velocity_primacy_reading, battery_storage_industry, beneficiary,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(technology_legitimacy_kernel__velocity_primacy_reading, battery_storage_industry, payer).

% Aggregators of flexible demand gain legitimacy as 'virtual power plants' that deploy instantly. They collect value from grid services markets created by the velocity constraint. Their exit is easy — software-based, low capital lock-in — but they depend on the constraint's market design persisting.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__velocity_primacy_reading, demand_response_aggregators, beneficiary,
    moderate, immediate, mobile, regional).

% Nuclear vendors and operators face de facto exclusion: new builds take 10-15 years, missing the 2030/2050 velocity window. They bear stranded R&D costs, workforce attrition, and supply chain decay. Their exit is constrained — massive sunk capital, regulatory obligations, and national security ties prevent pivot. They contest the constraint's legitimacy criterion but cannot escape its policy effects.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__velocity_primacy_reading, nuclear_industry, payer,
    institutional, generational, constrained, global).

% TSOs and DSOs must integrate rising variable generation while maintaining reliability. They bear the operational cost of intermittency — balancing markets, curtailment, grid reinforcement, inertia procurement. Their identity is fused to 'keeping the lights on'; they cannot exit the constraint because they are the system's fallback. The velocity mandate loads costs onto them without commensurate authority or compensation.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__velocity_primacy_reading, grid_operators, payer,
    institutional, biographical, identity_locked, national).

% Communities dependent on coal, gas, and oil extraction face accelerated phase-out under velocity logic. They would object to the legitimacy criterion that renders their assets stranded on a timeline incompatible with just transition. They are structurally excluded from the legitimacy conversation — their knowledge of energy density and reliability is treated as obstruction.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__velocity_primacy_reading, fossil_fuel_communities, excluded,
    organized, generational, trapped, regional).

% National governments, EU Commission, UNFCCC negotiators who set the legitimacy criterion through NDCs, taxonomy regulations, and subsidy rules. They justify velocity primacy via carbon budget math. They can arbitrage across jurisdictions — if one region relaxes, others tighten — and they control the enforcement machinery (permitting, finance, taxonomy).
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__velocity_primacy_reading, climate_policy_makers, agenda_setter,
    institutional, biographical, arbitrage, global).

% IPCC authors and climate modelers who produce the carbon budget numbers that ground the velocity constraint. They observe the constraint's operation but do not administer it. Their authority is epistemic, not operational — they define the problem space (remaining budget) but not the solution filter (velocity-only legitimacy).
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__velocity_primacy_reading, climate_scientists, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Directs global capital, policy attention, and engineering talent toward the mitigation technologies that can materially reduce emissions within the shrinking carbon budget — solving the 'too little, too late' problem by filtering for deployable solutions.
% TRANSFER_FUNCTION: Moves legitimacy, public finance, streamlined permitting, and market access from slow-deployment technologies (nuclear, CCS, novel thermal) to fast-deployment technologies (wind, solar, storage, demand flexibility). Transfers intermittency management costs from developers to grid operators. Transfers transition burden from policy timeline to fossil-fuel-dependent communities.
% ABSENT_VOICES: Nuclear engineering communities who argue that construction timelines can compress with regulatory reform and modular designs. Fossil fuel communities who would demand that legitimacy include transition justice metrics. Grid reliability engineers who would insist that legitimacy require demonstrated system adequacy, not just generation speed. These voices are excluded by the velocity criterion's definitional boundary.
% DISAPPEARANCE_RATIONALE: If the velocity-only legitimacy criterion vanished overnight, nuclear would re-enter serious policy consideration, grid operators would gain authority to set reliability standards that bind deployment pace, and transition timelines for fossil communities would extend. The entire architecture of climate finance (green taxonomies, MDB lending rules, subsidy allocation) would reorganize around a multi-criterion legitimacy framework.
% FOUNDING_PROBLEM: The Kyoto/Copenhagen era produced ambitious targets but negligible deployment; the Paris Agreement's 1.5°C pathway revealed that only technologies deployable at gigawatt-scale within 5-10 years can close the emissions gap before carbon budget exhaustion.
% FOUNDING_PROBLEM_CORROBORATION: IPCC AR6 WGIII (outside beneficiary set) corroborates the urgency of near-term deployment but explicitly notes that 'all mitigation options' are needed — not velocity-filtered ones. IEA Net Zero Roadmap (outside beneficiary set) includes nuclear and CCS as necessary, contradicting velocity-only legitimacy. The velocity reading's founding problem is corroborated on urgency but not on the exclusivity of its solution filter.
narrative_ontology:disappearance_verdict(technology_legitimacy_kernel__velocity_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(technology_legitimacy_kernel__velocity_primacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(technology_legitimacy_kernel__velocity_primacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(technology_legitimacy_kernel__velocity_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(technology_legitimacy_kernel__velocity_primacy_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(technology_legitimacy_kernel__velocity_primacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(technology_legitimacy_kernel__velocity_primacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(technology_legitimacy_kernel__velocity_primacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) is substantial: the constraint transfers legitimacy and finance from a broad technology portfolio to a narrow velocity-qualified set, while loading system integration costs onto grid operators. Suppression (0.55) is moderate but rising: nuclear is not banned but is structurally disadvantaged by permitting timelines, financing rules, and taxonomy exclusion. Theater ratio (0.42) reflects the gap between 'all tools needed' rhetoric and velocity-only policy operation. Accessibility collapse (0.58) is partial — nuclear remains technically possible but politically/financially marginalized. Resistance (0.71) is high from nuclear advocates, grid reliability institutions, and fossil-dependent regions. The claimed type is tangled_rope: genuine coordination (urgent deployment focus) plus asymmetric extraction (nuclear excluded, grid operators burdened).
 *
 * PERSPECTIVAL GAP:
 *   From the renewable developer seat, the constraint is a Rope — it solves the coordination problem of directing capital to the only technologies that can meet the timeline. From the nuclear industry seat, it is a Snare — the coordination story (carbon budget) is real but the velocity filter extracts their market access without technical justification. From the grid operator seat, it is a Tangled Rope — they get coordination benefit (more renewables to operate) but bear asymmetric extraction (integration costs without authority). The engine computes this divergence from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Renewable developers and storage industry are structural beneficiaries (d near 0.1-0.2): they collect legitimacy, finance, and market access. Nuclear industry is a structural target (d near 0.85): constrained exit, generational time horizon, institutional power but no velocity alignment. Grid operators are identity-locked targets (d near 0.9): they cannot exit the constraint because their institutional identity is 'system reliability,' and the constraint loads intermittency costs onto that identity. Fossil communities are trapped (d near 0.95): no exit, no voice. Policy makers are agenda-setters with arbitrage exit (d near 0.15). Climate scientists are analytical observers (d=0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's founding mandate (urgent deployment within carbon budget) remains live per IPCC. But the legitimacy filter has narrowed from 'technologies that can help' to 'technologies that deploy fast' — a mandatrophy drift where the solution criterion has hardened beyond what the founding problem strictly requires. The velocity reading captures real urgency but reifies a proxy (deployment speed) into a legitimacy gate, excluding options that could contribute post-2030. This is not pure extraction — the urgency is real — but the filter has become self-reinforcing: the more policy favors velocity, the more velocity-aligned technologies dominate, the more the constraint appears vindicated.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_structure_velocity_reading,
    'How does the velocity_primacy_reading''s beneficiary/victim structure differ from its sibling readings of the technology_legitimacy_kernel?',
    'Compare the three readings'' beneficiary and victim sets: velocity reading benefits renewables/storage, victims nuclear/grid-operators; precautionary reading benefits renewables, victims nuclear (waste); reliability reading benefits nuclear/hydro, victims variable renewables. The kernel contest is exactly which structural asymmetry gets encoded as ''legitimacy''.',
    'If the kernel is recognized as multi-reading, no single reading can claim its classification is the kernel''s classification. Each reading is a distinct constraint with its own ε. The velocity reading''s ε=0.68 applies only to its specific beneficiary/victim structure. Cross-reading comparison would reveal the kernel''s structural ambiguity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_structure_velocity_reading, conceptual, 'Committer structure: this constraint is one reading of a contested kernel; sibling readings produce different beneficiary/victim structures and thus different ε values.').

omega_variable(
    nuclear_exclusion_mechanism,
    'Is nuclear''s exclusion under the velocity criterion structural (policy/finance rules that penalize long lead times) or internalized (industry self-censors, accepts ''too slow'' framing)?',
    'Track nuclear project pipelines in jurisdictions with and without velocity-only taxonomies. If exclusion persists even where policy is neutral, internalization is significant. If exclusion correlates precisely with velocity-mandated policy instruments, structural mechanism dominates.',
    'If internalized, effective suppression is higher than measured — the industry carries the constraint''s exclusion logic even without active enforcement. If structural, suppression is policy-reversible. This affects whether nuclear''s victim status is contingent or locked.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(nuclear_exclusion_mechanism, empirical, 'Whether nuclear industry''s marginalization is enforced from outside or internalized as identity.').

omega_variable(
    intermittency_cost_allocation,
    'Are grid operators'' intermittency management costs a genuine system cost of high-renewable penetration, or an extractive transfer enabled by the velocity constraint''s refusal to internalize integration costs to developers?',
    'Compare jurisdictions with and without generator-side integration cost mandates (e.g., firm capacity requirements, balancing responsibility). If grid operator costs drop when integration costs are internalized, the velocity constraint''s current allocation is extractive transfer.',
    'If extractive transfer, the constraint''s extraction is higher than measured — grid operators are paying for a coordination externality that the constraint''s beneficiaries avoid. If genuine system cost, the constraint''s coordination function carries inherent extraction that no redesign can eliminate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intermittency_cost_allocation, empirical, 'Whether grid operator burden is necessary coordination cost or asymmetric extraction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(technology_legitimacy_kernel__velocity_primacy_reading, 2015, 2030).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(velprim_tr_t2015, technology_legitimacy_kernel__velocity_primacy_reading, theater_ratio, 2015, 0.15).
narrative_ontology:measurement(velprim_tr_t2018, technology_legitimacy_kernel__velocity_primacy_reading, theater_ratio, 2018, 0.22).
narrative_ontology:measurement(velprim_tr_t2021, technology_legitimacy_kernel__velocity_primacy_reading, theater_ratio, 2021, 0.3).
narrative_ontology:measurement(velprim_tr_t2024, technology_legitimacy_kernel__velocity_primacy_reading, theater_ratio, 2024, 0.38).
narrative_ontology:measurement(velprim_tr_t2027, technology_legitimacy_kernel__velocity_primacy_reading, theater_ratio, 2027, 0.42).
narrative_ontology:measurement(velprim_tr_t2030, technology_legitimacy_kernel__velocity_primacy_reading, theater_ratio, 2030, 0.48).

% Extraction over time
narrative_ontology:measurement(velprim_be_t2015, technology_legitimacy_kernel__velocity_primacy_reading, base_extractiveness, 2015, 0.35).
narrative_ontology:measurement(velprim_be_t2018, technology_legitimacy_kernel__velocity_primacy_reading, base_extractiveness, 2018, 0.45).
narrative_ontology:measurement(velprim_be_t2021, technology_legitimacy_kernel__velocity_primacy_reading, base_extractiveness, 2021, 0.55).
narrative_ontology:measurement(velprim_be_t2024, technology_legitimacy_kernel__velocity_primacy_reading, base_extractiveness, 2024, 0.62).
narrative_ontology:measurement(velprim_be_t2027, technology_legitimacy_kernel__velocity_primacy_reading, base_extractiveness, 2027, 0.68).
narrative_ontology:measurement(velprim_be_t2030, technology_legitimacy_kernel__velocity_primacy_reading, base_extractiveness, 2030, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(velprim_su_t2015, technology_legitimacy_kernel__velocity_primacy_reading, suppression_requirement, 2015, 0.25).
narrative_ontology:measurement(velprim_su_t2018, technology_legitimacy_kernel__velocity_primacy_reading, suppression_requirement, 2018, 0.35).
narrative_ontology:measurement(velprim_su_t2021, technology_legitimacy_kernel__velocity_primacy_reading, suppression_requirement, 2021, 0.45).
narrative_ontology:measurement(velprim_su_t2024, technology_legitimacy_kernel__velocity_primacy_reading, suppression_requirement, 2024, 0.52).
narrative_ontology:measurement(velprim_su_t2027, technology_legitimacy_kernel__velocity_primacy_reading, suppression_requirement, 2027, 0.55).
narrative_ontology:measurement(velprim_su_t2030, technology_legitimacy_kernel__velocity_primacy_reading, suppression_requirement, 2030, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(technology_legitimacy_kernel__velocity_primacy_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(technology_legitimacy_kernel__velocity_primacy_reading, 0.18).
narrative_ontology:affects_constraint(technology_legitimacy_kernel__velocity_primacy_reading, technology_legitimacy_kernel__precautionary_reading).
narrative_ontology:affects_constraint(technology_legitimacy_kernel__velocity_primacy_reading, technology_legitimacy_kernel__reliability_primacy_reading).
narrative_ontology:affects_constraint(technology_legitimacy_kernel__velocity_primacy_reading, grid_reliability_standards).
narrative_ontology:affects_constraint(technology_legitimacy_kernel__velocity_primacy_reading, climate_finance_taxonomy).
narrative_ontology:affects_constraint(technology_legitimacy_kernel__velocity_primacy_reading, nuclear_licensing_reform).

% DUAL FORMULATION NOTE:
% This constraint (velocity_primacy_reading) and its two siblings (precautionary_reading, reliability_primacy_reading) form the technology_legitimacy_kernel constraint family. They share the referent 'technology legitimacy for climate mitigation' but instantiate different ε values and different beneficiary/victim structures. The velocity reading's ε=0.68 reflects its asymmetric extraction from nuclear and grid operators. The precautionary reading would have lower ε for renewables but higher for nuclear. The reliability reading would invert the beneficiary/victim structure. All three are linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(technology_legitimacy_kernel__velocity_primacy_reading, institutional, 0.15).
constraint_indexing:directionality_override(technology_legitimacy_kernel__velocity_primacy_reading, organized, 0.2).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
