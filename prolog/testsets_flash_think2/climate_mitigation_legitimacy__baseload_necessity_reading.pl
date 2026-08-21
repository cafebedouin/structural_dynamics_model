% ============================================================================
% CONSTRAINT STORY: climate_mitigation_legitimacy__baseload_necessity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_mitigation_legitimacy__baseload_necessity_reading, []).

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
 *   constraint_id: climate_mitigation_legitimacy__baseload_necessity_reading
 *   human_readable: Baseload Necessity for Reliable Decarbonization
 *   domain: energy_policy/climate_mitigation/technology_governance
 *
 * SUMMARY:
 *   This constraint represents the 'baseload necessity' reading of the
 *   climate mitigation legitimacy kernel. It asserts that reliable
 *   decarbonization fundamentally requires dispatchable baseload power, which
 *   intermittent renewables cannot provide at scale. This framing positions
 *   technologies like nuclear and fossil fuels with CCS as essential, while
 *   classifying renewable-only pathways as inadequate for grid stability. The
 *   claimed type is Tangled Rope, reflecting a genuine coordination function
 *   (grid stability) coupled with asymmetric extraction (favoring specific
 *   technologies and their incumbents).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_mitigation_legitimacy__baseload_necessity_reading, 0.7).
domain_priors:suppression_score(climate_mitigation_legitimacy__baseload_necessity_reading, 0.75).
domain_priors:theater_ratio(climate_mitigation_legitimacy__baseload_necessity_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__baseload_necessity_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__baseload_necessity_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__baseload_necessity_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__baseload_necessity_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__baseload_necessity_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_mitigation_legitimacy__baseload_necessity_reading, tangled_rope).
narrative_ontology:human_readable(climate_mitigation_legitimacy__baseload_necessity_reading, "Baseload Necessity for Reliable Decarbonization").
narrative_ontology:topic_domain(climate_mitigation_legitimacy__baseload_necessity_reading, "energy_policy/climate_mitigation/technology_governance").

domain_priors:requires_active_enforcement(climate_mitigation_legitimacy__baseload_necessity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_mitigation_legitimacy__baseload_necessity_reading, 'c945f2a5-4b02-446c-bf5b-db2c412c08cc').
narrative_ontology:cs_kernel_codification('c945f2a5-4b02-446c-bf5b-db2c412c08cc', formalized).
narrative_ontology:cs_authority_grounding('c945f2a5-4b02-446c-bf5b-db2c412c08cc', expertise).
narrative_ontology:cs_interpretation_layer_present('c945f2a5-4b02-446c-bf5b-db2c412c08cc').
narrative_ontology:cs_reading_relation('c945f2a5-4b02-446c-bf5b-db2c412c08cc', climate_mitigation_legitimacy__renewable_primacy_reading, forecloses).
narrative_ontology:cs_reading_relation('c945f2a5-4b02-446c-bf5b-db2c412c08cc', climate_mitigation_legitimacy__portfolio_pragmatism_reading, influences).
narrative_ontology:cs_reading_relation('c945f2a5-4b02-446c-bf5b-db2c412c08cc', climate_mitigation_legitimacy__degrowth_sufficiency_reading, coexists_with).
narrative_ontology:cs_axiom('c945f2a5-4b02-446c-bf5b-db2c412c08cc', foundational, dispatchable_power_is_non_substitutable).
narrative_ontology:cs_axiom_status(dispatchable_power_is_non_substitutable, holdable).
narrative_ontology:cs_axiom_grounding('c945f2a5-4b02-446c-bf5b-db2c412c08cc', dispatchable_power_is_non_substitutable, empirically_contingent).
narrative_ontology:cs_axiom('c945f2a5-4b02-446c-bf5b-db2c412c08cc', foundational, grid_stability_is_paramount).
narrative_ontology:cs_axiom_status(grid_stability_is_paramount, holdable).
narrative_ontology:cs_axiom_grounding('c945f2a5-4b02-446c-bf5b-db2c412c08cc', grid_stability_is_paramount, conventional).
narrative_ontology:cs_reference_frame('c945f2a5-4b02-446c-bf5b-db2c412c08cc', traditional_grid_stability_paradigm).
narrative_ontology:cs_drift_state('c945f2a5-4b02-446c-bf5b-db2c412c08cc', contemporary_energy_transition, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('c945f2a5-4b02-446c-bf5b-db2c412c08cc', '').
narrative_ontology:cs_kernel_id(climate_mitigation_legitimacy__baseload_necessity_reading, climate_mitigation_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__baseload_necessity_reading, nuclear_power_industry).
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__baseload_necessity_reading, fossil_fuel_industry_with_ccs).
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__baseload_necessity_reading, grid_operators).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__baseload_necessity_reading, renewable_energy_developers).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__baseload_necessity_reading, climate_activists_renewable_only).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__baseload_necessity_reading, ratepayers_taxpayers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Responsible for ensuring energy security and grid stability while pursuing decarbonization goals. They interpret technical advice and public pressure to set energy policy, often favoring solutions that guarantee dispatchable power.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__baseload_necessity_reading, policy_makers_regulators, agenda_setter,
    institutional, generational, constrained, national).

% Benefits from policies that emphasize dispatchable baseload, as nuclear power is a primary source. They advocate for its role in decarbonization, securing investment and regulatory support for long-lived assets.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__baseload_necessity_reading, nuclear_power_industry, beneficiary,
    organized, civilizational, constrained, global).

% Benefits by positioning fossil fuels with carbon capture and storage (CCS) as a necessary baseload solution, extending the lifespan of their assets and delaying a full transition to renewables.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__baseload_necessity_reading, fossil_fuel_industry_with_ccs, beneficiary,
    organized, biographical, constrained, global).

% Prioritize grid stability and reliability. They advocate for dispatchable power sources to manage intermittency and ensure continuous supply, aligning with the baseload necessity argument.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__baseload_necessity_reading, grid_operators, beneficiary,
    institutional, biographical, constrained, national).

% Face policy and investment hurdles if their renewable-only pathways are deemed insufficient for baseload. They must either integrate storage solutions (increasing costs) or accept a secondary role in the energy mix.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__baseload_necessity_reading, renewable_energy_developers, payer,
    moderate, biographical, constrained, global).

% Advocate for rapid, full decarbonization primarily through renewables and storage. This constraint frames their preferred solutions as inadequate or unreliable, forcing them to compromise or fight against established policy narratives.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__baseload_necessity_reading, climate_activists_renewable_only, payer,
    organized, generational, identity_locked, global).

% Bear the costs of large-scale, capital-intensive baseload projects through higher electricity bills or taxes, without direct control over energy policy decisions or technology choices.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__baseload_necessity_reading, ratepayers_taxpayers, payer,
    powerless, immediate, trapped, national).

% Provide scientific assessments of climate change and decarbonization pathways, but their input is interpreted through various policy lenses, including the baseload necessity argument.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__baseload_necessity_reading, climate_scientists, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ensures a stable and reliable electricity grid during the transition to a decarbonized energy system by prioritizing dispatchable power sources that can meet continuous demand.
% TRANSFER_FUNCTION: Directs significant public and private investment towards large-scale, dispatchable baseload technologies (e.g., nuclear, fossil with CCS) and away from purely intermittent renewable energy projects, transferring associated costs to ratepayers and taxpayers.
% ABSENT_VOICES: Advocates for decentralized energy systems, proponents of aggressive demand-side management, and communities directly impacted by the construction of large baseload power plants (e.g., nuclear waste sites) are often marginalized in policy discussions driven by this narrative.
% DISAPPEARANCE_RATIONALE: If the premise that reliable decarbonization requires dispatchable baseload power vanished overnight, energy policy would dramatically shift. Investment would flood into renewables and storage, grid regulations would be re-evaluated to prioritize flexibility over dispatchability, and the political landscape of climate mitigation would fundamentally reorganize.
% FOUNDING_PROBLEM: The historical challenge of maintaining grid stability and energy security during the transition from fossil fuels, particularly given the intermittency of early renewable energy technologies and the perceived need for constant, on-demand power.
% FOUNDING_PROBLEM_CORROBORATION: Proponents (e.g., grid operators, nuclear industry) cite historical blackouts, grid stability studies, and the physical limitations of current large-scale storage as evidence the problem is live. Opponents (e.g., renewable advocates, some energy economists) attest that advancements in battery technology, smart grids, and demand-side management have substantially solved the problem, making the constraint's persistence a matter of policy choice rather than technical necessity.
narrative_ontology:disappearance_verdict(climate_mitigation_legitimacy__baseload_necessity_reading, world_rearranges).
narrative_ontology:founding_problem_status(climate_mitigation_legitimacy__baseload_necessity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_mitigation_legitimacy__baseload_necessity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(climate_mitigation_legitimacy__baseload_necessity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(climate_mitigation_legitimacy__baseload_necessity_reading, 0.7, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_mitigation_legitimacy__baseload_necessity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_mitigation_legitimacy__baseload_necessity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_mitigation_legitimacy__baseload_necessity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.7) stems from the capital-intensive nature of baseload technologies and the potential for lock-in, transferring significant costs to ratepayers. Suppression (0.75) is high because this narrative actively marginalizes and suppresses policy support for purely renewable pathways, requiring active enforcement through regulations and subsidies. The theater ratio (0.4) is moderate, as there is a genuine technical challenge in grid stability, but the 'necessity' argument also serves to protect incumbent industries and promote specific, often more expensive, technological solutions. The measurements show a general increase in extractiveness and suppression as the energy transition progresses and the debate intensifies, with a slight dip towards the end as renewable+storage solutions mature, but the core argument persists.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of baseload proponents (e.g., nuclear industry, grid operators), this constraint is a necessary coordination mechanism for grid reliability. From the perspective of renewable advocates, the same structure operates as an extractive mechanism that protects incumbent industries and slows the transition to cheaper, cleaner energy. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Policy makers and grid operators are agenda-setters and beneficiaries, as they maintain control and stability. The nuclear and fossil fuel (with CCS) industries are clear beneficiaries, receiving investment and policy support. Renewable energy developers and climate activists advocating for renewable-only solutions are payers/targets, as their preferred pathways are deemed insufficient. Ratepayers and taxpayers are ultimate payers, bearing the costs of these large-scale projects. Climate scientists act as observers, providing data that is then interpreted through this lens.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (reliable decarbonization) is still live, but its proposed solution (baseload necessity) is contested. The classification as a Tangled Rope prevents mislabeling it as a pure Rope (ignoring extraction) or a Snare (ignoring the genuine coordination problem of grid stability). The persistence of the constraint, despite technological advancements in renewables and storage, suggests a potential for mandatrophy if the 'necessity' becomes more theatrical than functional.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    technical_vs_policy_necessity,
    'Is the requirement for dispatchable baseload power a fundamental technical necessity for grid stability, or primarily a policy choice influenced by incumbent energy interests?',
    'Comparative analysis of grid stability outcomes and energy costs in jurisdictions that have aggressively pursued renewable-only pathways versus those that have prioritized baseload, controlling for geographic and demand differences.',
    'If primarily a policy choice, the constraint''s extractiveness and suppression are higher than technically justified, pushing its classification closer to a Snare. If a fundamental technical necessity, the coordination function is stronger, supporting a Tangled Rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technical_vs_policy_necessity, conceptual, 'Distinguishing technical necessity from policy-driven preference in energy infrastructure.').

omega_variable(
    renewable_storage_scalability,
    'Can renewable energy sources combined with advanced storage technologies truly provide reliable, dispatchable baseload power at the scale required for full decarbonization?',
    'Large-scale, long-duration energy storage demonstrations and grid integration projects, coupled with independent engineering and economic assessments of their technical feasibility and cost-effectiveness over multi-decade horizons.',
    'Empirical evidence of scalable, cost-effective renewable+storage solutions would significantly weaken the ''baseload necessity'' argument, reducing its suppression and extractiveness, potentially reclassifying it towards a Piton or even dissolving it. Lack of such evidence would reinforce its current classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(renewable_storage_scalability, empirical, 'Feasibility of renewable+storage for baseload power.').

omega_variable(
    cost_externalities_comparison,
    'What are the full lifecycle costs and externalities (e.g., waste, environmental impact, social equity) of baseload technologies (nuclear, CCS) compared to a fully renewable grid with storage?',
    'Comprehensive, independent lifecycle assessment (LCA) and social cost of carbon (SCC) analysis for different energy pathways, including all direct and indirect costs and benefits.',
    'If baseload technologies prove significantly more expensive or environmentally damaging when all externalities are accounted for, the perceived ''necessity'' would be undermined, reducing its legitimacy and increasing its effective extraction. Conversely, if renewables+storage prove more costly, the constraint''s coordination function would be strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cost_externalities_comparison, empirical, 'Comparative full cost and externality analysis of energy pathways.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_mitigation_legitimacy__baseload_necessity_reading, 2000, 2030).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t2000, climate_mitigation_legitimacy__baseload_necessity_reading, theater_ratio, 2000, 0.3).
narrative_ontology:measurement(clim_tr_t2005, climate_mitigation_legitimacy__baseload_necessity_reading, theater_ratio, 2005, 0.32).
narrative_ontology:measurement(clim_tr_t2010, climate_mitigation_legitimacy__baseload_necessity_reading, theater_ratio, 2010, 0.35).
narrative_ontology:measurement(clim_tr_t2015, climate_mitigation_legitimacy__baseload_necessity_reading, theater_ratio, 2015, 0.38).
narrative_ontology:measurement(clim_tr_t2020, climate_mitigation_legitimacy__baseload_necessity_reading, theater_ratio, 2020, 0.4).
narrative_ontology:measurement(clim_tr_t2025, climate_mitigation_legitimacy__baseload_necessity_reading, theater_ratio, 2025, 0.42).
narrative_ontology:measurement(clim_tr_t2030, climate_mitigation_legitimacy__baseload_necessity_reading, theater_ratio, 2030, 0.4).

% Extraction over time
narrative_ontology:measurement(clim_be_t2000, climate_mitigation_legitimacy__baseload_necessity_reading, base_extractiveness, 2000, 0.55).
narrative_ontology:measurement(clim_be_t2005, climate_mitigation_legitimacy__baseload_necessity_reading, base_extractiveness, 2005, 0.6).
narrative_ontology:measurement(clim_be_t2010, climate_mitigation_legitimacy__baseload_necessity_reading, base_extractiveness, 2010, 0.65).
narrative_ontology:measurement(clim_be_t2015, climate_mitigation_legitimacy__baseload_necessity_reading, base_extractiveness, 2015, 0.68).
narrative_ontology:measurement(clim_be_t2020, climate_mitigation_legitimacy__baseload_necessity_reading, base_extractiveness, 2020, 0.7).
narrative_ontology:measurement(clim_be_t2025, climate_mitigation_legitimacy__baseload_necessity_reading, base_extractiveness, 2025, 0.72).
narrative_ontology:measurement(clim_be_t2030, climate_mitigation_legitimacy__baseload_necessity_reading, base_extractiveness, 2030, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t2000, climate_mitigation_legitimacy__baseload_necessity_reading, suppression_requirement, 2000, 0.6).
narrative_ontology:measurement(clim_su_t2005, climate_mitigation_legitimacy__baseload_necessity_reading, suppression_requirement, 2005, 0.65).
narrative_ontology:measurement(clim_su_t2010, climate_mitigation_legitimacy__baseload_necessity_reading, suppression_requirement, 2010, 0.7).
narrative_ontology:measurement(clim_su_t2015, climate_mitigation_legitimacy__baseload_necessity_reading, suppression_requirement, 2015, 0.73).
narrative_ontology:measurement(clim_su_t2020, climate_mitigation_legitimacy__baseload_necessity_reading, suppression_requirement, 2020, 0.75).
narrative_ontology:measurement(clim_su_t2025, climate_mitigation_legitimacy__baseload_necessity_reading, suppression_requirement, 2025, 0.76).
narrative_ontology:measurement(clim_su_t2030, climate_mitigation_legitimacy__baseload_necessity_reading, suppression_requirement, 2030, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_mitigation_legitimacy__baseload_necessity_reading, resource_allocation).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__baseload_necessity_reading, energy_security_policy).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__baseload_necessity_reading, fossil_fuel_subsidies).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__baseload_necessity_reading, nuclear_power_regulation).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__baseload_necessity_reading, climate_mitigation_legitimacy__renewable_primacy_reading).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__baseload_necessity_reading, climate_mitigation_legitimacy__portfolio_pragmatism_reading).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__baseload_necessity_reading, climate_mitigation_legitimacy__degrowth_sufficiency_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of four distinct readings of the 'climate_mitigation_legitimacy' kernel, each representing a different structural claim about the requirements for effective climate action. They are linked to show their interdependencies and contestation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
