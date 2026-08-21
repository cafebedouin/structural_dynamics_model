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
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: performance_legitimacy__qualitative_development_reading
 *   human_readable: Performance Legitimacy: Qualitative Development Reading
 *   domain: political_economy/development_planning/state_capitalism
 *
 * SUMMARY:
 *   This constraint describes a reading of state performance legitimacy where
 *   the state's right to rule is justified by its ability to achieve
 *   'high-quality development' – emphasizing innovation, sustainability, and
 *   efficiency over raw GDP growth. This involves significant structural
 *   transformation, prioritizing high-tech sectors and environmental
 *   protection, often at the expense of traditional industries and
 *   property-dependent local governments. The constraint is claimed as a
 *   Tangled Rope, reflecting its genuine coordination function in reorienting
 *   the economy, but also its asymmetric extraction from sectors that bear
 *   the costs of this transformation.
 *
 * KEY AGENTS:
 *   - state_planning_agencies: Agenda setter (institutional/constrained) — drives the 'high-quality development' agenda.
 *   - high_tech_sectors: Primary beneficiary (powerful/mobile) — receives state support and preferential policies.
 *   - traditional_manufacturing_sectors: Primary payer (moderate/constrained) — bears costs of restructuring and environmental regulations.
 *   - property_dependent_local_governments: Payer (organized/trapped) — faces fiscal stress from reduced property-led growth.
 *   - low_skill_labor: Payer (powerless/identity_locked) — experiences job displacement and limited opportunities.
 *   - environmental_advocacy_groups: Observer (moderate/mobile) — monitors and advocates for sustainability goals.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(performance_legitimacy__qualitative_development_reading, 0.65).
domain_priors:suppression_score(performance_legitimacy__qualitative_development_reading, 0.7).
domain_priors:theater_ratio(performance_legitimacy__qualitative_development_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(performance_legitimacy__qualitative_development_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(performance_legitimacy__qualitative_development_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(performance_legitimacy__qualitative_development_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(performance_legitimacy__qualitative_development_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(performance_legitimacy__qualitative_development_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(performance_legitimacy__qualitative_development_reading, tangled_rope).
narrative_ontology:human_readable(performance_legitimacy__qualitative_development_reading, "Performance Legitimacy: Qualitative Development Reading").
narrative_ontology:topic_domain(performance_legitimacy__qualitative_development_reading, "political_economy/development_planning/state_capitalism").

domain_priors:requires_active_enforcement(performance_legitimacy__qualitative_development_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(performance_legitimacy__qualitative_development_reading, 'f90ee84b-4e42-4792-9d9d-484e832895c9').
narrative_ontology:cs_kernel_codification('f90ee84b-4e42-4792-9d9d-484e832895c9', formalized).
narrative_ontology:cs_authority_grounding('f90ee84b-4e42-4792-9d9d-484e832895c9', lineage).
narrative_ontology:cs_interpretation_layer_present('f90ee84b-4e42-4792-9d9d-484e832895c9').
narrative_ontology:cs_reading_relation('f90ee84b-4e42-4792-9d9d-484e832895c9', performance_legitimacy__quantitative_growth_reading, influences).
narrative_ontology:cs_reading_relation('f90ee84b-4e42-4792-9d9d-484e832895c9', performance_legitimacy__livelihood_security_reading, influences).
narrative_ontology:cs_reading_relation('f90ee84b-4e42-4792-9d9d-484e832895c9', performance_legitimacy__techno_nationalist_reading, coexists_with).
narrative_ontology:cs_axiom('f90ee84b-4e42-4792-9d9d-484e832895c9', foundational, innovation_driven_development_is_supreme).
narrative_ontology:cs_axiom_status(innovation_driven_development_is_supreme, holdable).
narrative_ontology:cs_axiom_grounding('f90ee84b-4e42-4792-9d9d-484e832895c9', innovation_driven_development_is_supreme, instrumental).
narrative_ontology:cs_axiom('f90ee84b-4e42-4792-9d9d-484e832895c9', foundational, environmental_sustainability_is_non_negotiable).
narrative_ontology:cs_axiom_status(environmental_sustainability_is_non_negotiable, holdable).
narrative_ontology:cs_axiom_grounding('f90ee84b-4e42-4792-9d9d-484e832895c9', environmental_sustainability_is_non_negotiable, empirically_contingent).
narrative_ontology:cs_reference_frame('f90ee84b-4e42-4792-9d9d-484e832895c9', sustainable_innovation_driven_state).
narrative_ontology:cs_drift_state('f90ee84b-4e42-4792-9d9d-484e832895c9', contemporary_global_economic_slowdown, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('f90ee84b-4e42-4792-9d9d-484e832895c9', '').
narrative_ontology:cs_kernel_id(performance_legitimacy__qualitative_development_reading, performance_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(performance_legitimacy__qualitative_development_reading, high_tech_sectors).
narrative_ontology:constraint_beneficiary(performance_legitimacy__qualitative_development_reading, state_backed_innovation_ecosystem).
narrative_ontology:constraint_victim(performance_legitimacy__qualitative_development_reading, traditional_manufacturing_sectors).
narrative_ontology:constraint_victim(performance_legitimacy__qualitative_development_reading, property_dependent_local_governments).
narrative_ontology:constraint_victim(performance_legitimacy__qualitative_development_reading, low_skill_labor).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These agencies set the strategic direction for economic development, prioritizing innovation, sustainability, and efficiency. They allocate resources, formulate industrial policies, and enforce environmental regulations, shifting focus from raw GDP growth to 'high-quality development' metrics. Their legitimacy depends on demonstrating progress in these areas.
narrative_ontology:constraint_stakeholder(performance_legitimacy__qualitative_development_reading, state_planning_agencies, agenda_setter,
    institutional, generational, constrained, national).

% These sectors (e.g., AI, biotech, advanced manufacturing) receive significant state support through subsidies, R&D funding, and preferential policies. They benefit from the prioritization of innovation and industrial upgrading, which drives their growth and profitability.
narrative_ontology:constraint_stakeholder(performance_legitimacy__qualitative_development_reading, high_tech_sectors, beneficiary,
    powerful, biographical, mobile, global).

% This includes state-owned venture capital funds, research institutes, and technology parks. They are instrumental in fostering innovation and commercializing new technologies, aligning with the 'high-quality development' agenda and receiving substantial public investment.
narrative_ontology:constraint_stakeholder(performance_legitimacy__qualitative_development_reading, state_backed_innovation_ecosystem, beneficiary,
    organized, generational, constrained, national).

% These sectors face increased pressure to upgrade, automate, or relocate due to stricter environmental regulations and reduced state support for 'low-quality' growth. Many struggle to adapt, leading to closures and job losses, bearing the costs of the structural transformation.
narrative_ontology:constraint_stakeholder(performance_legitimacy__qualitative_development_reading, traditional_manufacturing_sectors, payer,
    moderate, biographical, constrained, regional).

% Historically reliant on land sales and revenue from traditional industries, these local governments face fiscal stress as the central government de-emphasizes property-led growth and prioritizes environmental protection. Their ability to fund local services is constrained by the shift in development priorities.
narrative_ontology:constraint_stakeholder(performance_legitimacy__qualitative_development_reading, property_dependent_local_governments, payer,
    organized, immediate, trapped, local).

% Workers in traditional industries and those without skills for the high-tech sector face job displacement and limited opportunities. They bear the social costs of economic restructuring, often with inadequate social safety nets, and are identity-locked by their existing skills and geographic immobility.
narrative_ontology:constraint_stakeholder(performance_legitimacy__qualitative_development_reading, low_skill_labor, payer,
    powerless, biographical, identity_locked, local).

% These groups monitor the implementation of sustainability policies and advocate for stricter environmental protection. While their goals align with the 'high-quality development' narrative, they remain external observers, pushing for more rigorous enforcement.
narrative_ontology:constraint_stakeholder(performance_legitimacy__qualitative_development_reading, environmental_advocacy_groups, observer,
    moderate, generational, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(performance_legitimacy__qualitative_development_reading, state_backed_innovation_ecosystem).
narrative_ontology:fixing_cost_class(performance_legitimacy__qualitative_development_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates national economic policy towards a unified vision of 'high-quality development' by aligning industrial upgrading, environmental protection, and technological innovation, moving away from fragmented, growth-at-all-costs local initiatives.
% TRANSFER_FUNCTION: Transfers resources, policy focus, and political capital from traditional, high-polluting, and property-dependent sectors towards high-tech, green, and innovation-driven industries and their associated state-backed ecosystems. It also transfers social costs (job displacement, fiscal stress) to traditional sectors and local governments.
% ABSENT_VOICES: Small and medium-sized enterprises (SMEs) in traditional sectors, and displaced low-skill workers, often lack organized representation in policy discussions. They would advocate for more gradual transitions, retraining programs, and stronger social safety nets, but their concerns are often secondary to the national strategic goals.
% DISAPPEARANCE_RATIONALE: If this legitimacy constraint vanished, the state's development agenda would likely revert to prioritizing raw GDP growth, local governments would resume property-led development, and environmental regulations would weaken. The innovation ecosystem would lose its primary patron, and traditional industries might experience a temporary reprieve, leading to a significant reorientation of economic and political priorities.
% FOUNDING_PROBLEM: The previous model of rapid, quantitative growth led to severe environmental degradation, resource depletion, and an over-reliance on low-value-added manufacturing, creating an unsustainable and inefficient economic structure.
% FOUNDING_PROBLEM_CORROBORATION: Independent environmental reports, academic economic analyses, and international organizations corroborate the severity and persistence of the environmental and structural economic problems that necessitated the shift to 'high-quality development'. While some traditional sectors might dispute the urgency, the broad consensus among experts outside the direct beneficiaries supports the problem's live status.
narrative_ontology:disappearance_verdict(performance_legitimacy__qualitative_development_reading, world_rearranges).
narrative_ontology:founding_problem_status(performance_legitimacy__qualitative_development_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(performance_legitimacy__qualitative_development_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(performance_legitimacy__qualitative_development_reading, 'none', 1).
narrative_ontology:epsilon_provenance(performance_legitimacy__qualitative_development_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness (0.65) is substantial because the shift in development priorities imposes significant costs on traditional sectors and local governments, which are forced to adapt or decline. Suppression (0.70) is high due to the state's active enforcement of new industrial policies, environmental regulations, and resource reallocation, limiting alternatives for those negatively impacted. The theater ratio (0.25) is moderate; while there's genuine effort towards 'high-quality development', some rhetoric may mask the social and economic costs of the transition. The slight dip in extractiveness and suppression at the end of the interval reflects a potential stabilization or adaptation by some sectors after the initial shock of transformation.
 *
 * PERSPECTIVAL GAP:
 *   State planning agencies perceive this as a necessary and beneficial reorientation for long-term national prosperity, a genuine coordination effort. However, traditional manufacturing, local governments, and low-skill labor experience it as a highly extractive and suppressive force, as their livelihoods and fiscal bases are directly undermined by the new priorities. The engine's per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   State planning agencies and the high-tech/innovation ecosystem are beneficiaries, receiving resources and political backing (low d). Traditional manufacturing, property-dependent local governments, and low-skill labor are targets, bearing the costs of restructuring and policy shifts (high d). Environmental advocacy groups are observers, aligning with the goals but not directly subject to the constraint's extractive force.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the constraint as a pure Snare by acknowledging its genuine coordination function in addressing the unsustainability of past growth models. However, by classifying it as a Tangled Rope, it highlights the asymmetric extraction and suppression inherent in the structural transformation, ensuring that the costs borne by specific groups are not dismissed as mere 'transition friction'. The 'live' status of the founding problem, coupled with the 'world_rearranges' disappearance verdict, indicates that the mandate is still relevant, but the high extractiveness and suppression suggest that the implementation mechanism itself is problematic.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    true_cost_of_transition,
    'Are the social and economic costs borne by traditional sectors and low-skill labor adequately accounted for and mitigated by state policies?',
    'Comprehensive independent social impact assessments and longitudinal studies of displaced workers and affected regions, comparing actual outcomes with stated mitigation efforts.',
    'If costs are not adequately mitigated, the effective extractiveness and suppression of the constraint are higher than currently measured, potentially pushing the classification closer to a Snare for the victim seats. If mitigation is effective, the coordination aspect is stronger.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(true_cost_of_transition, empirical, 'Assesses whether the ''high-quality development'' transition is genuinely managed or merely imposed.').

omega_variable(
    sustainability_vs_growth_tradeoff,
    'To what extent does the emphasis on sustainability genuinely constrain economic growth, and is this tradeoff accepted by all relevant stakeholders?',
    'Analysis of economic data comparing growth rates in ''green'' vs. ''traditional'' sectors, coupled with surveys of business and public sentiment regarding environmental regulations and their economic impact.',
    'If the tradeoff is perceived as too severe or unfairly distributed, resistance could increase, challenging the constraint''s legitimacy. If sustainability gains are minimal despite economic costs, the theater ratio would increase.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sustainability_vs_growth_tradeoff, empirical, 'Examines the real-world impact and acceptance of the sustainability component of ''high-quality development''.').

omega_variable(
    qualitative_development_metrics_validity,
    'Are the metrics used to define and measure ''high-quality development'' (innovation, sustainability, efficiency) genuinely reflective of broad societal welfare, or do they primarily serve the interests of specific elite sectors?',
    'Independent expert review of the metric selection process, comparison with international standards for inclusive development, and public consultation processes to ensure broad societal input.',
    'If metrics are found to be biased, the claimed coordination function is weakened, and the constraint''s legitimacy is undermined, potentially increasing its effective extractiveness for non-beneficiaries.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(qualitative_development_metrics_validity, conceptual, 'Examines the conceptual validity and inclusiveness of ''high-quality development'' metrics.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(performance_legitimacy__qualitative_development_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(perf_tr_t0, performance_legitimacy__qualitative_development_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(perf_tr_t5, performance_legitimacy__qualitative_development_reading, theater_ratio, 5, 0.23).
narrative_ontology:measurement(perf_tr_t10, performance_legitimacy__qualitative_development_reading, theater_ratio, 10, 0.25).
narrative_ontology:measurement(perf_tr_t15, performance_legitimacy__qualitative_development_reading, theater_ratio, 15, 0.28).
narrative_ontology:measurement(perf_tr_t20, performance_legitimacy__qualitative_development_reading, theater_ratio, 20, 0.25).

% Extraction over time
narrative_ontology:measurement(perf_be_t0, performance_legitimacy__qualitative_development_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(perf_be_t5, performance_legitimacy__qualitative_development_reading, base_extractiveness, 5, 0.58).
narrative_ontology:measurement(perf_be_t10, performance_legitimacy__qualitative_development_reading, base_extractiveness, 10, 0.65).
narrative_ontology:measurement(perf_be_t15, performance_legitimacy__qualitative_development_reading, base_extractiveness, 15, 0.68).
narrative_ontology:measurement(perf_be_t20, performance_legitimacy__qualitative_development_reading, base_extractiveness, 20, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(perf_su_t0, performance_legitimacy__qualitative_development_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(perf_su_t5, performance_legitimacy__qualitative_development_reading, suppression_requirement, 5, 0.62).
narrative_ontology:measurement(perf_su_t10, performance_legitimacy__qualitative_development_reading, suppression_requirement, 10, 0.7).
narrative_ontology:measurement(perf_su_t15, performance_legitimacy__qualitative_development_reading, suppression_requirement, 15, 0.72).
narrative_ontology:measurement(perf_su_t20, performance_legitimacy__qualitative_development_reading, suppression_requirement, 20, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(performance_legitimacy__qualitative_development_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(performance_legitimacy__qualitative_development_reading, 0.15).
narrative_ontology:affects_constraint(performance_legitimacy__qualitative_development_reading, performance_legitimacy__quantitative_growth_reading).
narrative_ontology:affects_constraint(performance_legitimacy__qualitative_development_reading, performance_legitimacy__livelihood_security_reading).
narrative_ontology:affects_constraint(performance_legitimacy__qualitative_development_reading, performance_legitimacy__techno_nationalist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'performance_legitimacy' kernel, focusing on 'high-quality development'. It is structurally distinct from other readings (quantitative growth, livelihood security, techno-nationalism) due to its specific beneficiaries, victims, and policy priorities, but all are linked as components of the broader legitimacy framework.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
