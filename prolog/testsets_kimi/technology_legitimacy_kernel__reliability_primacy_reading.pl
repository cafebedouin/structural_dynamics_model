% ============================================================================
% CONSTRAINT STORY: technology_legitimacy_kernel__reliability_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_technology_legitimacy_kernel__reliability_primacy_reading, []).

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
 *   constraint_id: technology_legitimacy_kernel__reliability_primacy_reading
 *   human_readable: Reliability-Primacy Technology Legitimacy Criterion
 *   domain: energy_policy/climate_governance
 *
 * SUMMARY:
 *   This constraint is the reliability_primacy reading of the
 *   technology_legitimacy_kernel: a contested policy mechanism that defines
 *   climate mitigation legitimacy through dispatchable baseload capability.
 *   It structurally advantages nuclear and firm low-carbon generation while
 *   disadvantaging intermittent renewables and imposing costs on ratepayers.
 *   As one reading of a three-way kernel dispute (velocity vs. precaution vs.
 *   reliability), it instantiates a specific Îµ-invariant constraint with its
 *   own beneficiary/victim structure.
 *
 * KEY AGENTS:
 *   - grid_regulators (agenda_setter, institutional): define and enforce the dispatchability standard
 *   - nuclear_industry (beneficiary, powerful): gains policy legitimacy and finance from the criterion
 *   - firm_low_carbon_operators (beneficiary, moderate): gain access but lack political weight
 *   - intermittent_renewable_developers (payer, moderate): bear compliance costs and exclusion
 *   - ratepayers (payer, powerless): pay reliability premiums with no grid exit
 *   - climate_velocity_advocates (excluded, moderate): excluded from legitimacy framework
 *   - energy_systems_analysts (observer, analytical): empirically assess necessity of firm baseload
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(technology_legitimacy_kernel__reliability_primacy_reading, 0.72).
domain_priors:suppression_score(technology_legitimacy_kernel__reliability_primacy_reading, 0.68).
domain_priors:theater_ratio(technology_legitimacy_kernel__reliability_primacy_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(technology_legitimacy_kernel__reliability_primacy_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(technology_legitimacy_kernel__reliability_primacy_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(technology_legitimacy_kernel__reliability_primacy_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(technology_legitimacy_kernel__reliability_primacy_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(technology_legitimacy_kernel__reliability_primacy_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(technology_legitimacy_kernel__reliability_primacy_reading, tangled_rope).
narrative_ontology:human_readable(technology_legitimacy_kernel__reliability_primacy_reading, "Reliability-Primacy Technology Legitimacy Criterion").
narrative_ontology:topic_domain(technology_legitimacy_kernel__reliability_primacy_reading, "energy_policy/climate_governance").

domain_priors:requires_active_enforcement(technology_legitimacy_kernel__reliability_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(technology_legitimacy_kernel__reliability_primacy_reading, '13bd4592-62e8-4e21-b0aa-e84dabacdda2').
narrative_ontology:cs_kernel_codification('13bd4592-62e8-4e21-b0aa-e84dabacdda2', formalized).
narrative_ontology:cs_authority_grounding('13bd4592-62e8-4e21-b0aa-e84dabacdda2', expertise).
narrative_ontology:cs_interpretation_layer_present('13bd4592-62e8-4e21-b0aa-e84dabacdda2').
narrative_ontology:cs_reading_relation('13bd4592-62e8-4e21-b0aa-e84dabacdda2', technology_legitimacy_kernel__velocity_primacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('13bd4592-62e8-4e21-b0aa-e84dabacdda2', technology_legitimacy_kernel__precautionary_reading, coexists_with).
narrative_ontology:cs_axiom('13bd4592-62e8-4e21-b0aa-e84dabacdda2', foundational, firm_baseload_required_for_stability).
narrative_ontology:cs_axiom_status(firm_baseload_required_for_stability, holdable).
narrative_ontology:cs_axiom_grounding('13bd4592-62e8-4e21-b0aa-e84dabacdda2', firm_baseload_required_for_stability, empirically_contingent).
narrative_ontology:cs_axiom('13bd4592-62e8-4e21-b0aa-e84dabacdda2', foundational, legitimacy_contingent_on_dispatchability).
narrative_ontology:cs_axiom_status(legitimacy_contingent_on_dispatchability, holdable).
narrative_ontology:cs_axiom_grounding('13bd4592-62e8-4e21-b0aa-e84dabacdda2', legitimacy_contingent_on_dispatchability, conventional).
narrative_ontology:cs_reference_frame('13bd4592-62e8-4e21-b0aa-e84dabacdda2', engineering_reliability_framework).
narrative_ontology:cs_drift_state('13bd4592-62e8-4e21-b0aa-e84dabacdda2', contemporary_renewable_expansion, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('13bd4592-62e8-4e21-b0aa-e84dabacdda2', '').
narrative_ontology:cs_kernel_id(technology_legitimacy_kernel__reliability_primacy_reading, technology_legitimacy_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(technology_legitimacy_kernel__reliability_primacy_reading, nuclear_industry).
narrative_ontology:constraint_beneficiary(technology_legitimacy_kernel__reliability_primacy_reading, firm_low_carbon_operators).
narrative_ontology:constraint_victim(technology_legitimacy_kernel__reliability_primacy_reading, intermittent_renewable_developers).
narrative_ontology:constraint_victim(technology_legitimacy_kernel__reliability_primacy_reading, ratepayers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Define and enforce dispatchability and baseload criteria that determine whether a technology counts as legitimate climate mitigation. They justify the standard using grid stability engineering and reliability economics, and are structurally bound to the reliability paradigm by their institutional mission and professional training.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__reliability_primacy_reading, grid_regulators, agenda_setter,
    institutional, generational, constrained, national).

% Benefits directly from the legitimacy criterion because nuclear generation provides firm baseload capacity. Gains access to climate finance, streamlined permitting, and policy preference relative to intermittent sources. Business model depends on long-term regulatory certainty and large sunk-cost projects.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__reliability_primacy_reading, nuclear_industry, beneficiary,
    powerful, generational, constrained, national).

% Includes geothermal, reservoir hydro, and other dispatchable low-carbon generators that satisfy the baseload criterion. They gain market access and policy recognition but lack the political influence and capital access of the nuclear industry.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__reliability_primacy_reading, firm_low_carbon_operators, beneficiary,
    moderate, biographical, constrained, regional).

% Develop wind and solar projects that are excluded from climate legitimacy unless paired with costly storage or firm backup. They bear compliance costs of retrofitting projects to meet dispatchability standards and lose market share and policy support to firm generators.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__reliability_primacy_reading, intermittent_renewable_developers, payer,
    moderate, biographical, constrained, national).

% Pay electricity rates that incorporate the higher capital costs of nuclear and firm low-carbon infrastructure mandated by reliability criteria. Most have no practical ability to exit the regulated grid and bear the financial risk of cost overruns on baseload projects.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__reliability_primacy_reading, ratepayers, payer,
    powerless, immediate, trapped, national).

% Argue that deployment speed and carbon displacement matter more than firmness, and that intermittent renewables should be prioritized. They are structurally excluded from the legitimacy framework when reliability criteria are treated as non-negotiable.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__reliability_primacy_reading, climate_velocity_advocates, excluded,
    moderate, biographical, constrained, global).

% Model grid integration pathways and empirically assess whether firm baseload is strictly necessary. They provide the evidence base for contesting or supporting the reliability criterion but do not directly set the agenda.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__reliability_primacy_reading, energy_systems_analysts, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ensures grid stability and reliability by limiting climate legitimacy to dispatchable, baseload-capable generation, thereby preventing high-penetration intermittent scenarios that could destabilize the power system.
% TRANSFER_FUNCTION: Moves policy legitimacy, investment capital, and regulatory approval toward nuclear and firm low-carbon generators, while moving compliance costs and market barriers to intermittent renewable developers and reliability surcharges to ratepayers.
% ABSENT_VOICES: Climate velocity advocates who prioritize deployment speed over firmness; precautionary advocates who question nuclear waste and accident legacy costs; pro-market technology-neutral voices who reject the dispatchability filter as arbitrary.
% DISAPPEARANCE_RATIONALE: Without this constraint, intermittent renewables would gain faster policy legitimacy without storage mandates, investment would shift toward variable generation and grid flexibility, planning would prioritize transmission and demand response over firm baseload, and nuclear projects would lose their privileged regulatory standing.
% FOUNDING_PROBLEM: Grid instability and blackouts observed in early high-renewable-penetration jurisdictions, combined with the engineering challenge of balancing variable wind and solar output with electricity demand in real time.
% FOUNDING_PROBLEM_CORROBORATION: Independent system operators and grid engineers attest to stability challenges from outside the nuclear beneficiary set, publishing reliability assessments in multiple jurisdictions. However, renewable integration researchers and demand-side management analysts contest the severity, arguing the problem is solvable through transmission, storage, and flexible demand.
narrative_ontology:disappearance_verdict(technology_legitimacy_kernel__reliability_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(technology_legitimacy_kernel__reliability_primacy_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(technology_legitimacy_kernel__reliability_primacy_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(technology_legitimacy_kernel__reliability_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(technology_legitimacy_kernel__reliability_primacy_reading, 0.72, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(technology_legitimacy_kernel__reliability_primacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(technology_legitimacy_kernel__reliability_primacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(technology_legitimacy_kernel__reliability_primacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.72) because the constraint deliberately filters technologies, concentrating policy and capital benefits on nuclear and firm generators while extracting compliance surplus from renewables and ratepayers. Suppression is substantial (0.68) because the criterion must be actively enforced through grid codes, interconnection standards, and procurement rules that exclude non-compliant technologies. Theater ratio (0.42) reflects growing performative maintenance: the baseload concept is increasingly contested by empirical grid studies, yet the narrative persists as a political anchor. Accessibility collapse (0.60) captures the marginalization of alternatives like supergrids and demand flexibility. Resistance (0.58) is moderate and rising as renewable cost declines make the constraint harder to justify.
 *
 * PERSPECTIVAL GAP:
 *   The nuclear industry and grid regulators experience this constraint as necessary coordination for physical system stability; intermittent renewable developers and ratepayers experience it as enforced extraction that limits technology choice and raises costs. The engine computes this divergence from the same structural data â the asymmetry is in the seat, not the facts.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (nuclear_industry, firm_low_carbon_operators) have constrained exit and collect policy rents, placing them near the low-d beneficiary end. Victims (intermittent_renewable_developers, ratepayers) bear costs and have constrained or trapped exit, placing them near the high-d target end. Regulators administering the standard sit closer to symmetric but with institutional power that dampens their experienced extraction. Excluded velocity advocates sit outside the direct flow but would register high d if incorporated.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled rope classification prevents mislabeling this as pure extraction (snare) because grid stability is a genuine coordination problem with physical roots; it also prevents mislabeling it as pure coordination (rope) because the beneficiary/victim asymmetry is structurally locked in by technology-specific criteria that favor incumbent industries. The mandatrophy risk here is not obsolescence but functional capture: the coordination function (stability) is used to legitimate an extraction function (technology picking).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reliability_physicality,
    'Is dispatchable baseload capability a permanent physical requirement of grid stability, or a transient constraint solvable through storage, transmission, and demand flexibility?',
    'Empirical demonstration of high-renewable-penetration grids maintaining stability without firm baseload, or systematic failure of such grids requiring firm capacity restoration.',
    'If transient, this constraint''s coordination function atrophies and extraction dominates, pushing classification toward snare; if permanent, the tangled rope characterization holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reliability_physicality, empirical, 'Whether baseload requirement is physical law or engineering paradigm').

omega_variable(
    kernel_reading_exclusivity,
    'Does adopting the reliability_primacy reading structurally foreclose the velocity_primacy reading in a single policy framework, or can they coexist as competing political positions?',
    'Comparative policy analysis of jurisdictions attempting to combine firmness targets with deployment speed targets without logical contradiction.',
    'If foreclosing, the reading_relations should upgrade from coexists_with to forecloses; if coexisting, the kernel permits hybrid readings and the family linkage remains cooperative.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_exclusivity, conceptual, 'Logical relationship between reliability and velocity readings').

omega_variable(
    ratepayer_cost_causality,
    'What fraction of ratepayer cost increases is causally attributable to the baseload legitimacy requirement versus other grid investment drivers such as aging infrastructure?',
    'Counterfactual grid cost modeling with and without the dispatchability mandate, isolating the marginal cost of the constraint.',
    'Determines whether ratepayers are genuine structural victims of this constraint or incidental payers in a complex cost stack.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ratepayer_cost_causality, empirical, 'Attribution of ratepayer costs to the legitimacy constraint').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(technology_legitimacy_kernel__reliability_primacy_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tech_leg_rel_tr_t0, technology_legitimacy_kernel__reliability_primacy_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(tech_leg_rel_tr_t8, technology_legitimacy_kernel__reliability_primacy_reading, theater_ratio, 8, 0.26).
narrative_ontology:measurement(tech_leg_rel_tr_t16, technology_legitimacy_kernel__reliability_primacy_reading, theater_ratio, 16, 0.31).
narrative_ontology:measurement(tech_leg_rel_tr_t24, technology_legitimacy_kernel__reliability_primacy_reading, theater_ratio, 24, 0.36).
narrative_ontology:measurement(tech_leg_rel_tr_t32, technology_legitimacy_kernel__reliability_primacy_reading, theater_ratio, 32, 0.4).
narrative_ontology:measurement(tech_leg_rel_tr_t40, technology_legitimacy_kernel__reliability_primacy_reading, theater_ratio, 40, 0.42).

% Extraction over time
narrative_ontology:measurement(tech_leg_rel_be_t0, technology_legitimacy_kernel__reliability_primacy_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(tech_leg_rel_be_t8, technology_legitimacy_kernel__reliability_primacy_reading, base_extractiveness, 8, 0.58).
narrative_ontology:measurement(tech_leg_rel_be_t16, technology_legitimacy_kernel__reliability_primacy_reading, base_extractiveness, 16, 0.63).
narrative_ontology:measurement(tech_leg_rel_be_t24, technology_legitimacy_kernel__reliability_primacy_reading, base_extractiveness, 24, 0.68).
narrative_ontology:measurement(tech_leg_rel_be_t32, technology_legitimacy_kernel__reliability_primacy_reading, base_extractiveness, 32, 0.71).
narrative_ontology:measurement(tech_leg_rel_be_t40, technology_legitimacy_kernel__reliability_primacy_reading, base_extractiveness, 40, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(tech_leg_rel_su_t0, technology_legitimacy_kernel__reliability_primacy_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(tech_leg_rel_su_t8, technology_legitimacy_kernel__reliability_primacy_reading, suppression_requirement, 8, 0.55).
narrative_ontology:measurement(tech_leg_rel_su_t16, technology_legitimacy_kernel__reliability_primacy_reading, suppression_requirement, 16, 0.6).
narrative_ontology:measurement(tech_leg_rel_su_t24, technology_legitimacy_kernel__reliability_primacy_reading, suppression_requirement, 24, 0.64).
narrative_ontology:measurement(tech_leg_rel_su_t32, technology_legitimacy_kernel__reliability_primacy_reading, suppression_requirement, 32, 0.67).
narrative_ontology:measurement(tech_leg_rel_su_t40, technology_legitimacy_kernel__reliability_primacy_reading, suppression_requirement, 40, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(technology_legitimacy_kernel__reliability_primacy_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(technology_legitimacy_kernel__reliability_primacy_reading, velocity_primacy_reading).
narrative_ontology:affects_constraint(technology_legitimacy_kernel__reliability_primacy_reading, precautionary_reading).

% DUAL FORMULATION NOTE:
% The technology_legitimacy_kernel decomposes into three structurally distinct readings because the natural-language concept 'legitimate climate mitigation technology' conflates competing criteria (reliability, velocity, precaution) that have different beneficiary/victim structures and empirical statuses. This story instantiates the reliability_primacy reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
