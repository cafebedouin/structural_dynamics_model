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
    narrative_ontology:affects_constraint/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: caa_section_111d_delegation__systemic_transformation_reading
 *   human_readable: CAA Section 111(d) 'Best System' Systemic Transformation Mandate
 *   domain: administrative_law/environmental_regulation/constitutional_interpretation
 *
 * SUMMARY:
 *   This constraint represents the 'systemic transformation' reading of
 *   Section 111(d) of the Clean Air Act, which interprets the EPA's authority
 *   to regulate greenhouse gas emissions from existing power plants as
 *   extending to grid-wide, generation-shifting strategies, including
 *   promoting renewable energy and retiring coal plants. This reading
 *   contrasts sharply with a 'facility-constraint' reading that limits EPA's
 *   authority to measures implementable at individual power plants. This
 *   constraint is a reading of the 'caa_section_111d_delegation' kernel.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(caa_section_111d_delegation__systemic_transformation_reading, 0.85).
domain_priors:suppression_score(caa_section_111d_delegation__systemic_transformation_reading, 0.9).
domain_priors:theater_ratio(caa_section_111d_delegation__systemic_transformation_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(caa_section_111d_delegation__systemic_transformation_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(caa_section_111d_delegation__systemic_transformation_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(caa_section_111d_delegation__systemic_transformation_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(caa_section_111d_delegation__systemic_transformation_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(caa_section_111d_delegation__systemic_transformation_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(caa_section_111d_delegation__systemic_transformation_reading, tangled_rope).
narrative_ontology:human_readable(caa_section_111d_delegation__systemic_transformation_reading, "CAA Section 111(d) 'Best System' Systemic Transformation Mandate").
narrative_ontology:topic_domain(caa_section_111d_delegation__systemic_transformation_reading, "administrative_law/environmental_regulation/constitutional_interpretation").

domain_priors:requires_active_enforcement(caa_section_111d_delegation__systemic_transformation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(caa_section_111d_delegation__systemic_transformation_reading, '0cf37370-b38a-4f6e-8b8f-144e5bed5a5d').
narrative_ontology:cs_kernel_codification('0cf37370-b38a-4f6e-8b8f-144e5bed5a5d', formalized).
narrative_ontology:cs_authority_grounding('0cf37370-b38a-4f6e-8b8f-144e5bed5a5d', lineage).
narrative_ontology:cs_interpretation_layer_present('0cf37370-b38a-4f6e-8b8f-144e5bed5a5d').
narrative_ontology:cs_reading_relation('0cf37370-b38a-4f6e-8b8f-144e5bed5a5d', caa_section_111d_delegation__facility_constraint_reading, forecloses).
narrative_ontology:cs_axiom('0cf37370-b38a-4f6e-8b8f-144e5bed5a5d', foundational, epa_broad_statutory_authority).
narrative_ontology:cs_axiom_status(epa_broad_statutory_authority, holdable).
narrative_ontology:cs_axiom_grounding('0cf37370-b38a-4f6e-8b8f-144e5bed5a5d', epa_broad_statutory_authority, conventional).
narrative_ontology:cs_axiom('0cf37370-b38a-4f6e-8b8f-144e5bed5a5d', foundational, climate_change_systemic_threat).
narrative_ontology:cs_axiom_status(climate_change_systemic_threat, holdable).
narrative_ontology:cs_axiom_grounding('0cf37370-b38a-4f6e-8b8f-144e5bed5a5d', climate_change_systemic_threat, empirically_contingent).
narrative_ontology:cs_reference_frame('0cf37370-b38a-4f6e-8b8f-144e5bed5a5d', broad_decarbonization_mandate).
narrative_ontology:cs_drift_state('0cf37370-b38a-4f6e-8b8f-144e5bed5a5d', contemporary_judicial_challenge, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('0cf37370-b38a-4f6e-8b8f-144e5bed5a5d', '').
narrative_ontology:cs_kernel_id(caa_section_111d_delegation__systemic_transformation_reading, caa_section_111d_delegation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(caa_section_111d_delegation__systemic_transformation_reading, epa).
narrative_ontology:constraint_beneficiary(caa_section_111d_delegation__systemic_transformation_reading, renewable_energy_developers).
narrative_ontology:constraint_beneficiary(caa_section_111d_delegation__systemic_transformation_reading, environmental_advocacy_groups).
narrative_ontology:constraint_beneficiary(caa_section_111d_delegation__systemic_transformation_reading, public_health_beneficiaries).
narrative_ontology:constraint_victim(caa_section_111d_delegation__systemic_transformation_reading, fossil_fuel_power_generators).
narrative_ontology:constraint_victim(caa_section_111d_delegation__systemic_transformation_reading, coal_mining_industry).
narrative_ontology:constraint_victim(caa_section_111d_delegation__systemic_transformation_reading, fossil_fuel_dependent_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% As the federal agency tasked with environmental protection, the EPA interprets and enforces Section 111(d) to mandate state-level decarbonization, viewing it as essential for public health and climate stability. They face legal challenges but are committed to this broad interpretation.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__systemic_transformation_reading, epa, agenda_setter,
    institutional, civilizational, analytical, national).

% Operate coal and natural gas power plants and face mandates to reduce emissions or retire facilities. They bear significant compliance costs, lost revenue, and stranded asset risks. Their exit options are limited by existing infrastructure and market dependence.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__systemic_transformation_reading, fossil_fuel_power_generators, payer,
    powerful, biographical, constrained, national).

% Supplies fuel to power generators and faces declining demand due to mandated shifts away from coal. Their industry is directly targeted by this interpretation, with limited alternative markets or economic diversification pathways.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__systemic_transformation_reading, coal_mining_industry, payer,
    organized, biographical, trapped, national).

% Develop and operate solar, wind, and other renewable energy projects. They benefit from regulatory mandates that create demand for their products and provide compliance pathways for states and utilities, leading to market growth and investment.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__systemic_transformation_reading, renewable_energy_developers, beneficiary,
    organized, biographical, mobile, national).

% States with economies heavily reliant on fossil fuel extraction and generation. They are mandated to develop decarbonization plans, facing economic disruption and political resistance from local industries and communities. Their options are to comply, litigate, or seek federal aid for transition.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__systemic_transformation_reading, fossil_fuel_dependent_states, payer,
    institutional, generational, constrained, national).

% Advocate for aggressive climate action and support EPA's broad interpretation of Section 111(d). They benefit from the policy's potential to reduce emissions and promote clean energy, aligning with their mission.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__systemic_transformation_reading, environmental_advocacy_groups, beneficiary,
    organized, generational, mobile, national).

% The ultimate arbiter of statutory and constitutional interpretation. Its rulings determine the legal validity and scope of EPA's authority under Section 111(d), directly impacting the persistence and enforcement of this constraint. It observes and adjudicates, rather than directly participating in the policy's implementation.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__systemic_transformation_reading, supreme_court, observer,
    institutional, civilizational, analytical, national).

% Individuals and communities who experience improved air quality and reduced health risks as a result of decreased emissions from power plants. They are diffuse beneficiaries, often unaware of the specific regulatory mechanisms but directly impacted by the outcomes.
narrative_ontology:constraint_stakeholder(caa_section_111d_delegation__systemic_transformation_reading, public_health_beneficiaries, beneficiary,
    powerless, biographical, trapped, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(caa_section_111d_delegation__systemic_transformation_reading, renewable_energy_developers).
narrative_ontology:fixing_cost_class(caa_section_111d_delegation__systemic_transformation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To coordinate a national, grid-wide strategy for reducing greenhouse gas emissions from existing power plants, facilitating a transition to cleaner energy sources and mitigating climate change impacts.
% TRANSFER_FUNCTION: Transfers economic costs (e.g., compliance, stranded assets) from the public (via health and environmental impacts) to fossil fuel industries and their consumers, while transferring economic benefits (e.g., subsidies, market growth) to renewable energy developers and their investors.
% ABSENT_VOICES: Future generations, who will bear the long-term consequences of climate change or benefit from successful decarbonization, are structurally absent from the immediate policy debates. Communities disproportionately affected by both fossil fuel pollution and the economic disruption of transition also often lack direct representation.
% DISAPPEARANCE_RATIONALE: If this systemic interpretation of Section 111(d) vanished, federal authority to mandate grid-wide decarbonization would be severely curtailed. States would likely pursue fragmented or less ambitious climate policies, leading to slower energy transition, continued reliance on fossil fuels, and exacerbated climate change impacts. The energy sector's investment landscape would shift dramatically, favoring fossil fuels over renewables in the absence of federal mandates.
% FOUNDING_PROBLEM: The Clean Air Act was established to address widespread air pollution threatening public health and welfare. Section 111(d) specifically aimed to regulate emissions from existing sources not covered by other provisions, a problem that evolved to include greenhouse gases and the systemic threat of climate change.
% FOUNDING_PROBLEM_CORROBORATION: Environmental scientists, public health organizations, and international climate bodies consistently attest that the problem of greenhouse gas emissions and climate change remains live and urgent. While fossil fuel industries and some political actors contest the severity or human causation of climate change, a broad scientific consensus and numerous independent reports corroborate the founding problem's ongoing relevance.
narrative_ontology:disappearance_verdict(caa_section_111d_delegation__systemic_transformation_reading, world_rearranges).
narrative_ontology:founding_problem_status(caa_section_111d_delegation__systemic_transformation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(caa_section_111d_delegation__systemic_transformation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(caa_section_111d_delegation__systemic_transformation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(caa_section_111d_delegation__systemic_transformation_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is high (0.85) because this interpretation mandates significant economic shifts, imposing substantial costs on fossil fuel industries and states dependent on them, while subsidizing renewables. Suppression is also high (0.90) as it involves federal mandates that compel states and utilities to adopt specific decarbonization pathways, with legal and financial penalties for non-compliance. Theater ratio is low (0.10) because the policy aims for direct, measurable environmental outcomes, not performative compliance. Resistance is high (0.80) due to intense political and legal opposition from affected industries and states. Accessibility collapse is high (0.75) as traditional fossil-fuel generation becomes increasingly economically and legally unviable under this interpretation.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the EPA and environmental advocates, this constraint is a necessary and effective mechanism for addressing climate change and public health, coordinating a national energy transition. From the perspective of fossil fuel industries and dependent states, it is an overreach of federal power, imposing extractive costs and suppressing their economic models.
 *
 * DIRECTIONALITY LOGIC:
 *   The EPA, renewable energy developers, environmental advocacy groups, and public health beneficiaries are the primary beneficiaries, gaining regulatory authority, market advantage, policy wins, and improved health outcomes, respectively. Fossil fuel power generators, the coal mining industry, and fossil fuel-dependent states are the primary victims, facing mandates, economic losses, and stranded assets. The Supreme Court acts as an observer/arbiter, determining the legal validity of this interpretation.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    statutory_interpretation_ambiguity,
    'Is the ''best system of emission reduction'' in Section 111(d) genuinely intended to authorize grid-wide, generation-shifting strategies, or is it limited to facility-specific measures?',
    'Supreme Court ruling on the scope of EPA''s authority under Section 111(d), or new legislative action clarifying congressional intent.',
    'If limited to facility-specific measures (the sibling reading), this constraint would be reclassified as a Snare (pure extraction from fossil fuels without a broad coordination function) or potentially a Piton if the facility-level measures are ineffective. If the systemic interpretation is upheld, it reinforces the Tangled Rope classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(statutory_interpretation_ambiguity, conceptual, 'Ambiguity in the scope of EPA''s statutory authority under the Clean Air Act.').

omega_variable(
    economic_transition_cost_uncertainty,
    'What are the true economic costs and benefits of a rapid, mandated grid-wide energy transition, and how are they distributed across different sectors and populations?',
    'Longitudinal economic studies tracking energy prices, job creation/loss, and investment patterns in transitioning regions over decades.',
    'If costs are significantly higher than projected and disproportionately borne by vulnerable populations, the extractiveness of this constraint would be higher, potentially pushing it closer to a Snare. If benefits (e.g., health, innovation) are greater and more equitably distributed, it would reinforce the coordination aspect of the Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(economic_transition_cost_uncertainty, empirical, 'Uncertainty regarding the full economic impact and equity of mandated energy transition.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of fossil fuel generation primarily structural (legal mandates, economic incentives) or is there an element of internalized suppression (e.g., states/utilities accepting decarbonization as an inevitable future)?',
    'Analysis of state-level policy responses and utility investment decisions in the absence of federal mandates, or post-mandate behavior if the constraint is weakened.',
    'If internalized suppression is significant, the constraint''s effective suppression is higher than the structural measure suggests, as actors self-regulate even without direct coercion. If purely structural, its persistence depends entirely on active enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in energy transition.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(caa_section_111d_delegation__systemic_transformation_reading, 2015, 2030).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(caa__tr_t2015, caa_section_111d_delegation__systemic_transformation_reading, theater_ratio, 2015, 0.15).
narrative_ontology:measurement(caa__tr_t2020, caa_section_111d_delegation__systemic_transformation_reading, theater_ratio, 2020, 0.12).
narrative_ontology:measurement(caa__tr_t2025, caa_section_111d_delegation__systemic_transformation_reading, theater_ratio, 2025, 0.1).
narrative_ontology:measurement(caa__tr_t2030, caa_section_111d_delegation__systemic_transformation_reading, theater_ratio, 2030, 0.1).

% Extraction over time
narrative_ontology:measurement(caa__be_t2015, caa_section_111d_delegation__systemic_transformation_reading, base_extractiveness, 2015, 0.6).
narrative_ontology:measurement(caa__be_t2020, caa_section_111d_delegation__systemic_transformation_reading, base_extractiveness, 2020, 0.7).
narrative_ontology:measurement(caa__be_t2025, caa_section_111d_delegation__systemic_transformation_reading, base_extractiveness, 2025, 0.78).
narrative_ontology:measurement(caa__be_t2030, caa_section_111d_delegation__systemic_transformation_reading, base_extractiveness, 2030, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(caa__su_t2015, caa_section_111d_delegation__systemic_transformation_reading, suppression_requirement, 2015, 0.7).
narrative_ontology:measurement(caa__su_t2020, caa_section_111d_delegation__systemic_transformation_reading, suppression_requirement, 2020, 0.78).
narrative_ontology:measurement(caa__su_t2025, caa_section_111d_delegation__systemic_transformation_reading, suppression_requirement, 2025, 0.85).
narrative_ontology:measurement(caa__su_t2030, caa_section_111d_delegation__systemic_transformation_reading, suppression_requirement, 2030, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(caa_section_111d_delegation__systemic_transformation_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(caa_section_111d_delegation__systemic_transformation_reading, us_energy_grid_stability_mandate).
narrative_ontology:affects_constraint(caa_section_111d_delegation__systemic_transformation_reading, state_renewable_portfolio_standards).
narrative_ontology:affects_constraint(caa_section_111d_delegation__systemic_transformation_reading, coal_plant_retirement_schedules).

% DUAL FORMULATION NOTE:
% This constraint is one of two primary readings of the CAA Section 111(d) delegation kernel, focusing on systemic, grid-wide transformation. The sibling 'facility_constraint_reading' posits a narrower, facility-specific interpretation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
