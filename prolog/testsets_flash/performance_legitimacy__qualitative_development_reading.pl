% ============================================================================
% CONSTRAINT STORY: performance_legitimacy__qualitative_development_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_performance_legitimacy__qualitative_development_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: performance_legitimacy__qualitative_development_reading
 *   human_readable: Performance Legitimacy: Qualitative Development Reading
 *   domain: political_economy/development_planning/state_capitalism
 *
 * SUMMARY:
 *   This constraint describes the 'qualitative development' reading of
 *   performance legitimacy, where the state's right to rule is justified by
 *   its ability to deliver innovation, sustainability, and efficiency gains,
 *   rather than just raw economic growth. This reading prioritizes industrial
 *   upgrading, environmental protection, and technological self-sufficiency,
 *   leading to significant structural shifts in the economy. It is a Tangled
 *   Rope because it genuinely coordinates a complex economic transition but
 *   also extracts heavily from traditional sectors and low-skilled labor
 *   through active enforcement of new policies.
 *
 * KEY AGENTS:
 *   - state_planning_agencies: Agenda setter (institutional/constrained) — drives the 'high-quality development' agenda.
 *   - high_tech_sectors: Primary beneficiary (organized/mobile) — receives preferential policies and investment.
 *   - traditional_manufacturing_sectors: Primary payer (powerful/constrained) — faces pressure to upgrade or decline.
 *   - property_dependent_local_governments: Payer (organized/trapped) — loses revenue from traditional growth models.
 *   - low_skilled_labor: Payer (powerless/identity_locked) — faces job displacement and retraining challenges.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(performance_legitimacy__qualitative_development_reading, 0.65).
domain_priors:suppression_score(performance_legitimacy__qualitative_development_reading, 0.75).
domain_priors:theater_ratio(performance_legitimacy__qualitative_development_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(performance_legitimacy__qualitative_development_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(performance_legitimacy__qualitative_development_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(performance_legitimacy__qualitative_development_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(performance_legitimacy__qualitative_development_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(performance_legitimacy__qualitative_development_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(performance_legitimacy__qualitative_development_reading, tangled_rope).
narrative_ontology:human_readable(performance_legitimacy__qualitative_development_reading, "Performance Legitimacy: Qualitative Development Reading").
narrative_ontology:topic_domain(performance_legitimacy__qualitative_development_reading, "political_economy/development_planning/state_capitalism").

domain_priors:requires_active_enforcement(performance_legitimacy__qualitative_development_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(performance_legitimacy__qualitative_development_reading, 'fecff1a1-1026-486e-9482-fc661e6637e4').
narrative_ontology:cs_kernel_codification('fecff1a1-1026-486e-9482-fc661e6637e4', formalized).
narrative_ontology:cs_authority_grounding('fecff1a1-1026-486e-9482-fc661e6637e4', lineage).
narrative_ontology:cs_interpretation_layer_present('fecff1a1-1026-486e-9482-fc661e6637e4').
narrative_ontology:cs_reading_relation('fecff1a1-1026-486e-9482-fc661e6637e4', performance_legitimacy__quantitative_growth_reading, influences).
narrative_ontology:cs_reading_relation('fecff1a1-1026-486e-9482-fc661e6637e4', performance_legitimacy__techno_nationalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('fecff1a1-1026-486e-9482-fc661e6637e4', performance_legitimacy__livelihood_security_reading, influences).
narrative_ontology:cs_axiom('fecff1a1-1026-486e-9482-fc661e6637e4', foundational, innovation_driven_development_is_supreme).
narrative_ontology:cs_axiom_status(innovation_driven_development_is_supreme, holdable).
narrative_ontology:cs_axiom_grounding('fecff1a1-1026-486e-9482-fc661e6637e4', innovation_driven_development_is_supreme, instrumental).
narrative_ontology:cs_axiom('fecff1a1-1026-486e-9482-fc661e6637e4', foundational, ecological_civilization_is_necessary).
narrative_ontology:cs_axiom_status(ecological_civilization_is_necessary, holdable).
narrative_ontology:cs_axiom_grounding('fecff1a1-1026-486e-9482-fc661e6637e4', ecological_civilization_is_necessary, empirically_contingent).
narrative_ontology:cs_reference_frame('fecff1a1-1026-486e-9482-fc661e6637e4', sustainable_innovation_paradigm).
narrative_ontology:cs_drift_state('fecff1a1-1026-486e-9482-fc661e6637e4', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('fecff1a1-1026-486e-9482-fc661e6637e4', '').
narrative_ontology:cs_kernel_id(performance_legitimacy__qualitative_development_reading, performance_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(performance_legitimacy__qualitative_development_reading, high_tech_sectors).
narrative_ontology:constraint_beneficiary(performance_legitimacy__qualitative_development_reading, state_backed_innovation_ecosystem).
narrative_ontology:constraint_beneficiary(performance_legitimacy__qualitative_development_reading, environmental_regulators).
narrative_ontology:constraint_victim(performance_legitimacy__qualitative_development_reading, traditional_manufacturing_sectors).
narrative_ontology:constraint_victim(performance_legitimacy__qualitative_development_reading, property_dependent_local_governments).
narrative_ontology:constraint_victim(performance_legitimacy__qualitative_development_reading, low_skilled_labor).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets policy priorities, allocates resources, and evaluates performance based on metrics of innovation, sustainability, and efficiency. Actively promotes industrial upgrading and green development, often at the expense of raw growth targets. Their legitimacy is tied to demonstrating 'high-quality development'.
narrative_ontology:constraint_stakeholder(performance_legitimacy__qualitative_development_reading, state_planning_agencies, agenda_setter,
    institutional, generational, constrained, national).

% Receives preferential policies, subsidies, and investment in R&D, venture capital, and M&A infrastructure. Benefits from the state's focus on innovation and industrial upgrading, which drives demand for their products and services.
narrative_ontology:constraint_stakeholder(performance_legitimacy__qualitative_development_reading, high_tech_sectors, beneficiary,
    organized, biographical, mobile, global).

% Includes state-owned venture capital funds, research institutes, and technology parks. Benefits from direct funding and policy support aimed at fostering indigenous innovation and achieving self-sufficiency in strategic technologies.
narrative_ontology:constraint_stakeholder(performance_legitimacy__qualitative_development_reading, state_backed_innovation_ecosystem, beneficiary,
    institutional, generational, constrained, national).

% Gains increased authority and resources as sustainability becomes a core component of 'high-quality development'. Enforces stricter environmental standards, leading to the closure or upgrading of polluting industries.
narrative_ontology:constraint_stakeholder(performance_legitimacy__qualitative_development_reading, environmental_regulators, beneficiary,
    institutional, generational, constrained, national).

% Faces pressure to upgrade, relocate, or shut down due to new environmental regulations and reduced state support for 'low-quality' growth. Bears the costs of industrial restructuring and often struggles to adapt to new policy directives.
narrative_ontology:constraint_stakeholder(performance_legitimacy__qualitative_development_reading, traditional_manufacturing_sectors, payer,
    powerful, biographical, constrained, national).

% Relied heavily on land sales and revenue from traditional industries for their budgets. The shift to 'high-quality development' means lower growth targets and stricter environmental controls, reducing their revenue streams and increasing fiscal pressure.
narrative_ontology:constraint_stakeholder(performance_legitimacy__qualitative_development_reading, property_dependent_local_governments, payer,
    organized, immediate, trapped, local).

% Faces job displacement as traditional industries decline and new high-tech sectors require different skill sets. Often lacks the means or opportunities for retraining, leading to unemployment or underemployment. Their identity is often tied to their traditional work.
narrative_ontology:constraint_stakeholder(performance_legitimacy__qualitative_development_reading, low_skilled_labor, payer,
    powerless, biographical, identity_locked, regional).

% Monitors policy shifts to identify new investment opportunities in high-tech and green sectors, while divesting from traditional industries facing headwinds. Their analytical stance allows them to arbitrage policy changes.
narrative_ontology:constraint_stakeholder(performance_legitimacy__qualitative_development_reading, international_investors, observer,
    powerful, immediate, arbitrage, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates national economic planning towards a unified vision of sustainable, innovation-driven growth, moving away from unbridled resource-intensive expansion. It aligns state and market actors towards specific strategic industries and environmental goals.
% TRANSFER_FUNCTION: Transfers resources, policy support, and legitimacy from traditional, high-polluting, and low-efficiency sectors to high-tech, green, and innovation-driven industries. It also transfers the burden of economic restructuring and job displacement to traditional sectors and low-skilled labor.
% ABSENT_VOICES: Local communities heavily dependent on traditional industries, small and medium enterprises unable to upgrade, and displaced low-skilled workers often lack effective channels to voice their concerns about the costs of 'high-quality development'. They would argue for more balanced growth and social safety nets.
% DISAPPEARANCE_RATIONALE: If this reading of performance legitimacy vanished, the state's development strategy would lose its guiding principle. Resource allocation would become chaotic, environmental regulations might relax, and the focus could revert to raw GDP growth, leading to a significant reorganization of economic priorities and power dynamics.
% FOUNDING_PROBLEM: The problem of unsustainable, imbalanced, and low-quality economic growth that led to severe environmental degradation, resource depletion, and a 'middle-income trap' risk, threatening long-term stability and global competitiveness.
% FOUNDING_PROBLEM_CORROBORATION: Independent environmental reports, international economic analyses, and academic studies corroborate the severity and persistence of the problems of environmental degradation and the need for industrial upgrading. While beneficiaries within the state apparatus attest to its live status, external observers also confirm the underlying issues this reading aims to address.
narrative_ontology:disappearance_verdict(performance_legitimacy__qualitative_development_reading, world_rearranges).
narrative_ontology:founding_problem_status(performance_legitimacy__qualitative_development_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(performance_legitimacy__qualitative_development_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(performance_legitimacy__qualitative_development_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(performance_legitimacy__qualitative_development_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(performance_legitimacy__qualitative_development_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(performance_legitimacy__qualitative_development_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.65) because the economic restructuring imposes significant costs on specific sectors and populations, often without direct compensation. Suppression (0.75) is also high, as the state actively enforces industrial policies, environmental regulations, and resource reallocation, limiting alternatives for affected parties. Theater ratio (0.40) reflects that while the goals of 'high-quality development' are genuine, some policies are performative, designed to signal commitment to the new agenda rather than achieve immediate, measurable results. The increasing trend in extractiveness and suppression over the interval reflects the deepening commitment to this development model and the intensifying enforcement required to achieve it.
 *
 * PERSPECTIVAL GAP:
 *   State planning agencies perceive this as a necessary and beneficial coordination effort for long-term national development. However, traditional manufacturing sectors, property-dependent local governments, and low-skilled labor experience it as a highly extractive and suppressive force, as their livelihoods and established economic models are disrupted. The engine's per-seat classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   State planning agencies, high-tech sectors, and the innovation ecosystem are beneficiaries (low d) as they gain resources, legitimacy, and policy support. Traditional manufacturing, local governments, and low-skilled labor are targets (high d) as they bear the costs of restructuring, job losses, and reduced revenue. Environmental regulators are beneficiaries as their mandate aligns with the new development goals.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is not experiencing mandatrophy; the founding problem of unsustainable growth is still live. Instead, it represents a re-mandating of state legitimacy towards a new development paradigm. The classification as Tangled Rope prevents mislabeling it as a pure Snare, acknowledging the genuine coordination function of steering a complex economy, while also capturing the significant extraction from those who bear the costs of this transition.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    qualitative_vs_quantitative_impact,
    'To what extent do the ''high-quality'' development metrics (innovation, sustainability) genuinely translate into broad-based welfare gains, versus primarily benefiting specific elite sectors?',
    'Longitudinal studies tracking income distribution, social mobility, and public health outcomes across different regions and social strata, disaggregated by sector.',
    'If benefits are concentrated, the constraint''s effective extractiveness is higher for the general population than current metrics suggest, potentially shifting its classification closer to a Snare for the majority. If broad-based, it reinforces the coordination aspect.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(qualitative_vs_quantitative_impact, empirical, 'Assessing the true distribution of benefits from ''high-quality development''.').

omega_variable(
    legitimacy_source_ambiguity,
    'Is the state''s legitimacy truly derived from delivering ''high-quality development'', or is this a post-hoc rationalization for maintaining control and directing resources to favored sectors?',
    'Analysis of public opinion surveys on government satisfaction, disaggregated by economic sector and region, alongside studies of policy implementation and enforcement patterns.',
    'If primarily a rationalization, the constraint''s claimed coordination function is weaker, and its suppressive elements are more central to its persistence, pushing it closer to a Snare. If genuine, it reinforces the Tangled Rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_source_ambiguity, conceptual, 'Distinguishing genuine legitimacy from rationalized control.').

omega_variable(
    kernel_reading_identification,
    'Is this constraint accurately identified as the ''qualitative_development_reading'' of the ''performance_legitimacy'' kernel, or does it conflate elements of other readings?',
    'Detailed textual analysis of policy documents and speeches, comparing specific policy instruments and stated goals against the core tenets of each sibling reading (quantitative_growth, techno_nationalist, livelihood_security).',
    'Misidentification would lead to an inaccurate assessment of its structural relations to other readings and its unique axiomatic grounding, potentially altering its computed position within the broader commitment system.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Verifying the precise instantiation of this kernel reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(performance_legitimacy__qualitative_development_reading, 2012, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(perf_tr_t2012, performance_legitimacy__qualitative_development_reading, theater_ratio, 2012, 0.25).
narrative_ontology:measurement(perf_tr_t2015, performance_legitimacy__qualitative_development_reading, theater_ratio, 2015, 0.3).
narrative_ontology:measurement(perf_tr_t2018, performance_legitimacy__qualitative_development_reading, theater_ratio, 2018, 0.35).
narrative_ontology:measurement(perf_tr_t2021, performance_legitimacy__qualitative_development_reading, theater_ratio, 2021, 0.38).
narrative_ontology:measurement(perf_tr_t2024, performance_legitimacy__qualitative_development_reading, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(perf_be_t2012, performance_legitimacy__qualitative_development_reading, base_extractiveness, 2012, 0.5).
narrative_ontology:measurement(perf_be_t2015, performance_legitimacy__qualitative_development_reading, base_extractiveness, 2015, 0.55).
narrative_ontology:measurement(perf_be_t2018, performance_legitimacy__qualitative_development_reading, base_extractiveness, 2018, 0.6).
narrative_ontology:measurement(perf_be_t2021, performance_legitimacy__qualitative_development_reading, base_extractiveness, 2021, 0.63).
narrative_ontology:measurement(perf_be_t2024, performance_legitimacy__qualitative_development_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(perf_su_t2012, performance_legitimacy__qualitative_development_reading, suppression_requirement, 2012, 0.6).
narrative_ontology:measurement(perf_su_t2015, performance_legitimacy__qualitative_development_reading, suppression_requirement, 2015, 0.65).
narrative_ontology:measurement(perf_su_t2018, performance_legitimacy__qualitative_development_reading, suppression_requirement, 2018, 0.7).
narrative_ontology:measurement(perf_su_t2021, performance_legitimacy__qualitative_development_reading, suppression_requirement, 2021, 0.73).
narrative_ontology:measurement(perf_su_t2024, performance_legitimacy__qualitative_development_reading, suppression_requirement, 2024, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(performance_legitimacy__qualitative_development_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(performance_legitimacy__qualitative_development_reading, 0.15).
narrative_ontology:affects_constraint(performance_legitimacy__qualitative_development_reading, performance_legitimacy__techno_nationalist_reading).
narrative_ontology:affects_constraint(performance_legitimacy__qualitative_development_reading, environmental_protection_regulations).
narrative_ontology:affects_constraint(performance_legitimacy__qualitative_development_reading, industrial_policy_subsidies).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'performance_legitimacy' kernel. Its structural focus on innovation and sustainability differentiates it from other readings like 'quantitative_growth' or 'livelihood_security', but it influences and is influenced by them within the broader political economy.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
