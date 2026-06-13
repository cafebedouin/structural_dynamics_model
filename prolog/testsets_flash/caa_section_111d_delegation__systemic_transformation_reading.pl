% ============================================================================
% CONSTRAINT STORY: caa_section_111d_delegation__systemic_transformation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_caa_section_111d_delegation__systemic_transformation_reading, []).

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
    narrative_ontology:coordination_type/2,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: caa_section_111d_delegation__systemic_transformation_reading
 *   human_readable: CAA Section 111(d) Delegation: Systemic Transformation Reading
 *   domain: administrative_law/environmental_regulation/constitutional_interpretation
 *
 * SUMMARY:
 *   This constraint represents the 'systemic transformation' reading of
 *   Section 111(d) of the Clean Air Act, where the EPA is authorized to
 *   mandate state-level decarbonization pathways, including renewable
 *   substitution and early coal retirement. This interpretation allows for
 *   broad, grid-wide strategies to reduce emissions, shifting the energy
 *   generation mix. It is a reading of the 'caa_section_111d_delegation'
 *   kernel, distinct from the 'facility_constraint_reading'.
 *
 * KEY AGENTS:
 *   - epa: Agenda setter (institutional/analytical) — mandates and enforces decarbonization pathways.
 *   - renewable_energy_developers: Beneficiary (organized/arbitrage) — subsidized through regulatory compliance pathways.
 *   - environmental_advocacy_groups: Beneficiary (organized/analytical) — benefit from accelerated decarbonization.
 *   - coal_fired_power_plants: Victim (powerful/constrained) — face early retirement mandates and high compliance costs.
 *   - fossil_fuel_dependent_states: Victim (institutional/constrained) — bear high exit costs and economic disruption.
 *   - coal_mining_communities: Victim (powerless/trapped) — face job losses and economic decline.
 *   - supreme_court: Observer (institutional/analytical) — adjudicates the legality and scope of EPA's authority.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(caa_section_111d_delegation__systemic_transformation_reading, 0.65).
domain_priors:suppression_score(caa_section_111d_delegation__systemic_transformation_reading, 0.75).
domain_priors:theater_ratio(caa_section_111d_delegation__systemic_transformation_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(caa_section_111d_delegation__systemic_transformation_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(caa_section_111d_delegation__systemic_transformation_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(caa_section_111d_delegation__systemic_transformation_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(caa_section_111d_delegation__systemic_transformation_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(caa_section_111d_delegation__systemic_transformation_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(caa_section_111d_delegation__systemic_transformation_reading, tangled_rope).
narrative_ontology:human_readable(caa_section_111d_delegation__systemic_transformation_reading, "CAA Section 111(d) Delegation: Systemic Transformation Reading").
narrative_ontology:topic_domain(caa_section_111d_delegation__systemic_transformation_reading, "administrative_law/environmental_regulation/constitutional_interpretation").

domain_priors:requires_active_enforcement(caa_section_111d_delegation__systemic_transformation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(caa_section_111d_delegation__systemic_transformation_reading, 'cefcc7fb-636c-43c4-948f-0c6a86164a56').
narrative_ontology:cs_kernel_codification('cefcc7fb-636c-43c4-948f-0c6a86164a56', fixed_text).
narrative_ontology:cs_authority_grounding('cefcc7fb-636c-43c4-948f-0c6a86164a56', lineage).
narrative_ontology:cs_interpretation_layer_present('cefcc7fb-636c-43c4-948f-0c6a86164a56').
narrative_ontology:cs_reading_relation('cefcc7fb-636c-43c4-948f-0c6a86164a56', caa_section_111d_delegation__facility_constraint_reading, forecloses).
narrative_ontology:cs_axiom('cefcc7fb-636c-43c4-948f-0c6a86164a56', foundational, broad_delegation_for_environmental_protection).
narrative_ontology:cs_axiom_status(broad_delegation_for_environmental_protection, holdable).
narrative_ontology:cs_axiom_grounding('cefcc7fb-636c-43c4-948f-0c6a86164a56', broad_delegation_for_environmental_protection, deontological).
narrative_ontology:cs_axiom('cefcc7fb-636c-43c4-948f-0c6a86164a56', foundational, grid_wide_solutions_are_best_system).
narrative_ontology:cs_axiom_status(grid_wide_solutions_are_best_system, holdable).
narrative_ontology:cs_axiom_grounding('cefcc7fb-636c-43c4-948f-0c6a86164a56', grid_wide_solutions_are_best_system, empirically_contingent).
narrative_ontology:cs_reference_frame('cefcc7fb-636c-43c4-948f-0c6a86164a56', broad_regulatory_authority_for_environmental_protection).
narrative_ontology:cs_drift_state('cefcc7fb-636c-43c4-948f-0c6a86164a56', contemporary_major_questions_doctrine_era, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('cefcc7fb-636c-43c4-948f-0c6a86164a56', '').
narrative_ontology:cs_kernel_id(caa_section_111d_delegation__systemic_transformation_reading, caa_section_111d_delegation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(caa_section_111d_delegation__systemic_transformation_reading, epa).
narrative_ontology:constraint_beneficiary(caa_section_111d_delegation__systemic_transformation_reading, renewable_energy_developers).
narrative_ontology:constraint_beneficiary(caa_section_111d_delegation__systemic_transformation_reading, environmental_advocacy_groups).
narrative_ontology:constraint_victim(caa_section_111d_delegation__systemic_transformation_reading, coal_fired_power_plants).
narrative_ontology:constraint_victim(caa_section_111d_delegation__systemic_transformation_reading, fossil_fuel_dependent_states).
narrative_ontology:constraint_victim(caa_section_111d_delegation__systemic_transformation_reading, coal_mining_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The primary federal agency responsible for implementing environmental regulations. Under this reading, it gains significant authority to mandate broad decarbonization strategies across states, aligning with its mission to protect public health and the environment. Its authority is, however, constrained by judicial review.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__systemic_transformation_reading, epa, agenda_setter,
    institutional, generational, constrained, national).

% Benefit from regulatory mandates that create demand for renewable energy sources. They receive subsidies and preferential treatment in state compliance plans, leading to increased market share and profitability. Their exit options are high as they can pursue opportunities in other markets or states.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__systemic_transformation_reading, renewable_energy_developers, beneficiary,
    organized, biographical, arbitrage, national).

% Advocate for aggressive climate action and benefit from policies that accelerate decarbonization. They provide political support for EPA's actions and engage in litigation to defend the systemic transformation reading. Their 'exit' is primarily analytical, shifting focus to other policy levers if this one fails.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__systemic_transformation_reading, environmental_advocacy_groups, beneficiary,
    organized, generational, analytical, national).

% Face mandates for early retirement or costly retrofits to comply with new emission standards. This leads to significant financial losses, stranded assets, and potential bankruptcy. Their exit options are limited due to high capital investment and specialized infrastructure.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__systemic_transformation_reading, coal_fired_power_plants, payer,
    powerful, immediate, constrained, regional).

% States with economies heavily reliant on coal production and consumption face significant economic disruption, job losses, and the challenge of transitioning their energy infrastructure. They resist EPA mandates through legal challenges and political lobbying, but are ultimately bound by federal law.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__systemic_transformation_reading, fossil_fuel_dependent_states, payer,
    institutional, generational, constrained, national).

% Experience direct job losses and economic decline as coal-fired power plants shut down. These communities often lack alternative industries or retraining opportunities, leading to severe social and economic hardship. Their options are severely limited, often leading to out-migration or persistent poverty.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__systemic_transformation_reading, coal_mining_communities, payer,
    powerless, biographical, trapped, local).

% The ultimate arbiter of constitutional and administrative law. It reviews challenges to EPA's authority under Section 111(d), determining whether the systemic transformation reading is a legitimate exercise of delegated power or an overreach. Its decisions shape the long-term trajectory of environmental regulation.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__systemic_transformation_reading, supreme_court, observer,
    institutional, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(caa_section_111d_delegation__systemic_transformation_reading, epa).
narrative_ontology:fixing_cost_class(caa_section_111d_delegation__systemic_transformation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To coordinate a national, grid-wide transition away from fossil fuels to reduce greenhouse gas emissions, addressing a collective action problem where individual state actions are insufficient.
% TRANSFER_FUNCTION: Transfers economic value from the fossil fuel industry (through early retirement, reduced market share, compliance costs) to the renewable energy sector (through increased demand, subsidies, market opportunities) and to the public (through reduced pollution and climate benefits).
% ABSENT_VOICES: Future generations, who will bear the long-term costs of climate change if decarbonization is insufficient, are absent from the immediate policy debate. Their interests are represented by environmental groups but not directly voiced. Also, communities disproportionately affected by the energy transition, whose voices are often marginalized in national policy discussions.
% DISAPPEARANCE_RATIONALE: If this reading of Section 111(d) vanished, EPA's authority to mandate systemic decarbonization would cease. States would revert to less ambitious, facility-specific measures, or no action at all. The energy transition would slow significantly, and the economic landscape for both fossil fuels and renewables would shift dramatically, requiring a complete reorganization of climate policy and energy investment.
% FOUNDING_PROBLEM: The problem of regulating greenhouse gas emissions from existing power plants under the Clean Air Act, specifically how to interpret 'best system of emission reduction' to address climate change effectively.
% FOUNDING_PROBLEM_CORROBORATION: The problem of climate change and the need to regulate power plant emissions remains a live and pressing issue, corroborated by scientific consensus (IPCC, NASA, NOAA) and international agreements (Paris Agreement). The specific interpretation of 111(d) is contested, but the underlying problem is not.
narrative_ontology:disappearance_verdict(caa_section_111d_delegation__systemic_transformation_reading, world_rearranges).
narrative_ontology:founding_problem_status(caa_section_111d_delegation__systemic_transformation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(caa_section_111d_delegation__systemic_transformation_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(caa_section_111d_delegation__systemic_transformation_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(caa_section_111d_delegation__systemic_transformation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(caa_section_111d_delegation__systemic_transformation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(caa_section_111d_delegation__systemic_transformation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Tangled Rope because it genuinely coordinates a collective action problem (decarbonization) while simultaneously extracting significantly from specific parties (fossil fuel industry, coal-dependent states). Extractiveness is high (0.65) due to the substantial costs imposed on coal plants and states for early retirement and transition. Suppression (0.75) is also high, reflecting the EPA's active enforcement and the limited alternatives for compliance for affected entities. Theater ratio is low (0.1) as the EPA's actions are directly aimed at achieving environmental goals, with little performative maintenance.
 *
 * PERSPECTIVAL GAP:
 *   The EPA and environmental groups perceive this as a necessary and effective coordination mechanism for climate action, with benefits outweighing costs. Coal-fired power plants and fossil fuel-dependent states, however, experience it as a highly extractive and suppressive mandate that threatens their economic viability. The Supreme Court's perspective is analytical, focused on the legal boundaries of delegated authority.
 *
 * DIRECTIONALITY LOGIC:
 *   The EPA, renewable energy developers, and environmental advocacy groups are beneficiaries (low d) as they gain authority, market opportunities, and policy outcomes, respectively. Coal-fired power plants, fossil fuel-dependent states, and coal mining communities are victims (high d) due to mandated closures, economic disruption, and job losses. The Supreme Court is an analytical observer (d=0.5) as its role is to interpret the law, not directly benefit or suffer from the policy's implementation.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is not subject to mandatrophy in the traditional sense, as its mandate (decarbonization) is considered live and urgent by its proponents. However, the contest over its legal basis (major questions doctrine) and scope (systemic vs. facility) represents a challenge to its legitimacy, rather than an atrophy of its original function. The high resistance from victims indicates that the constraint's persistence is not due to inertia but active enforcement against strong opposition.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    delegation_doctrine_ambiguity,
    'Is this reading of Section 111(d) an unconstitutional delegation of legislative power to the EPA, or a legitimate exercise of agency authority under a broad statutory grant?',
    'Supreme Court ruling on the ''major questions doctrine'' as applied to this specific interpretation.',
    'If deemed unconstitutional, the constraint collapses, and EPA''s authority to mandate grid-wide decarbonization is nullified. If upheld, it solidifies EPA''s power and accelerates the energy transition.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(delegation_doctrine_ambiguity, conceptual, 'Ambiguity regarding the scope of EPA''s delegated authority under Section 111(d).').

omega_variable(
    systemic_vs_facility_scope,
    'Is Section 111(d) ''best system of emission reduction'' limited to measures implementable at individual facilities (the ''facility_constraint_reading''), or does it authorize grid-wide, generation-shifting strategies (this ''systemic_transformation_reading'')?',
    'Judicial interpretation, particularly by the Supreme Court, clarifying the statutory language and legislative intent.',
    'If the ''facility_constraint_reading'' prevails, EPA''s authority is severely curtailed, reducing extraction from fossil fuels and slowing decarbonization. If this ''systemic_transformation_reading'' prevails, EPA''s authority for broad decarbonization is affirmed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(systemic_vs_facility_scope, conceptual, 'Contest over the scope of EPA''s authority under Section 111(d) of the Clean Air Act.').

omega_variable(
    economic_impact_distribution,
    'What is the true economic cost and benefit distribution of this systemic transformation reading, particularly for energy consumers and fossil fuel-dependent regions?',
    'Comprehensive, independent economic modeling and longitudinal studies of energy prices, job displacement, and new job creation in affected regions.',
    'If costs are found to disproportionately burden vulnerable populations or regions without adequate transition support, it could fuel political resistance and legal challenges, potentially leading to policy adjustments or judicial invalidation. If benefits are widely distributed, it strengthens the policy''s legitimacy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(economic_impact_distribution, empirical, 'Uncertainty regarding the precise economic impacts and their distribution across different stakeholders.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(caa_section_111d_delegation__systemic_transformation_reading, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(caa__tr_t0, caa_section_111d_delegation__systemic_transformation_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(caa__tr_t5, caa_section_111d_delegation__systemic_transformation_reading, theater_ratio, 5, 0.1).
narrative_ontology:measurement(caa__tr_t10, caa_section_111d_delegation__systemic_transformation_reading, theater_ratio, 10, 0.1).
narrative_ontology:measurement(caa__tr_t15, caa_section_111d_delegation__systemic_transformation_reading, theater_ratio, 15, 0.1).

% Extraction over time
narrative_ontology:measurement(caa__be_t0, caa_section_111d_delegation__systemic_transformation_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(caa__be_t5, caa_section_111d_delegation__systemic_transformation_reading, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(caa__be_t10, caa_section_111d_delegation__systemic_transformation_reading, base_extractiveness, 10, 0.6).
narrative_ontology:measurement(caa__be_t15, caa_section_111d_delegation__systemic_transformation_reading, base_extractiveness, 15, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(caa__su_t0, caa_section_111d_delegation__systemic_transformation_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(caa__su_t5, caa_section_111d_delegation__systemic_transformation_reading, suppression_requirement, 5, 0.65).
narrative_ontology:measurement(caa__su_t10, caa_section_111d_delegation__systemic_transformation_reading, suppression_requirement, 10, 0.7).
narrative_ontology:measurement(caa__su_t15, caa_section_111d_delegation__systemic_transformation_reading, suppression_requirement, 15, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(caa_section_111d_delegation__systemic_transformation_reading, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'caa_section_111d_delegation' kernel, specifically the 'systemic_transformation_reading'. It stands in contrast to the 'facility_constraint_reading' which limits EPA's authority to individual facility-level measures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
