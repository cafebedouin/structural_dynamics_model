% ============================================================================
% CONSTRAINT STORY: technology_legitimacy_kernel__precautionary_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_technology_legitimacy_kernel__precautionary_reading, []).

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
 *   constraint_id: technology_legitimacy_kernel__precautionary_reading
 *   human_readable: Precautionary Legitimacy Threshold for Climate Mitigation Technologies
 *   domain: energy_policy/climate_mitigation/technology_governance
 *
 * SUMMARY:
 *   The precautionary reading of the technology legitimacy kernel asserts
 *   that climate mitigation technologies must have worst-case failure modes
 *   and legacy costs that are bounded and reversible within a human
 *   generation (~25-30 years). This reading emerged from the environmental
 *   movement's engagement with nuclear risk (Three Mile Island, Chernobyl,
 *   Fukushima) and chemical persistence (PCBs, PFAS), and was formalized in
 *   instruments like the Rio Declaration Principle 15 and the EU's
 *   precautionary principle. It operates as a legitimacy filter: technologies
 *   passing the filter (wind, solar, storage, efficiency) gain access to
 *   policy support, subsidies, and streamlined permitting; technologies
 *   failing it (nuclear fission, carbon capture with leakage risk,
 *   geoengineering) are excluded or burdened. The constraint has a genuine
 *   coordination function — it gives policymakers a defensible criterion for
 *   technology prioritization under uncertainty — but it also performs
 *   asymmetric extraction: the nuclear industry bears exclusion costs,
 *   ratepayers in phaseout jurisdictions bear higher system costs, and future
 *   generations bear climate damages if the filter slows decarbonization.
 *   Enforcement is active (permitting regimes, subsidy eligibility, phaseout
 *   laws). The reading's extraction has risen as renewable deployment scaled
 *   and the filter hardened into binding law (Germany's Energiewende, EU
 *   taxonomy debates, US IRA technology-neutral-but-effectively-renewable
 *   provisions).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(technology_legitimacy_kernel__precautionary_reading, 0.65).
domain_priors:suppression_score(technology_legitimacy_kernel__precautionary_reading, 0.55).
domain_priors:theater_ratio(technology_legitimacy_kernel__precautionary_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(technology_legitimacy_kernel__precautionary_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(technology_legitimacy_kernel__precautionary_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(technology_legitimacy_kernel__precautionary_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(technology_legitimacy_kernel__precautionary_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(technology_legitimacy_kernel__precautionary_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(technology_legitimacy_kernel__precautionary_reading, tangled_rope).
narrative_ontology:human_readable(technology_legitimacy_kernel__precautionary_reading, "Precautionary Legitimacy Threshold for Climate Mitigation Technologies").
narrative_ontology:topic_domain(technology_legitimacy_kernel__precautionary_reading, "energy_policy/climate_mitigation/technology_governance").

domain_priors:requires_active_enforcement(technology_legitimacy_kernel__precautionary_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(technology_legitimacy_kernel__precautionary_reading, '41005ba7-2b35-40f8-9281-94607fee9a6e').
narrative_ontology:cs_kernel_codification('41005ba7-2b35-40f8-9281-94607fee9a6e', distributed).
narrative_ontology:cs_authority_grounding('41005ba7-2b35-40f8-9281-94607fee9a6e', practice).
narrative_ontology:cs_interpretation_layer_present('41005ba7-2b35-40f8-9281-94607fee9a6e').
narrative_ontology:cs_reading_relation('41005ba7-2b35-40f8-9281-94607fee9a6e', technology_legitimacy_kernel__reliability_primacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('41005ba7-2b35-40f8-9281-94607fee9a6e', technology_legitimacy_kernel__velocity_primacy_reading, influences).
narrative_ontology:cs_axiom('41005ba7-2b35-40f8-9281-94607fee9a6e', foundational, irreversible_legacy_costs_disqualify).
narrative_ontology:cs_axiom_status(irreversible_legacy_costs_disqualify, holdable).
narrative_ontology:cs_axiom_grounding('41005ba7-2b35-40f8-9281-94607fee9a6e', irreversible_legacy_costs_disqualify, deontological).
narrative_ontology:cs_axiom('41005ba7-2b35-40f8-9281-94607fee9a6e', foundational, generational_reversibility_threshold).
narrative_ontology:cs_axiom_status(generational_reversibility_threshold, holdable).
narrative_ontology:cs_axiom_grounding('41005ba7-2b35-40f8-9281-94607fee9a6e', generational_reversibility_threshold, empirically_contingent).
narrative_ontology:cs_reference_frame('41005ba7-2b35-40f8-9281-94607fee9a6e', precautionary_governance_framework).
narrative_ontology:cs_drift_state('41005ba7-2b35-40f8-9281-94607fee9a6e', contemporary_net_zero_policy_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('41005ba7-2b35-40f8-9281-94607fee9a6e', '').
narrative_ontology:cs_kernel_id(technology_legitimacy_kernel__precautionary_reading, technology_legitimacy_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(technology_legitimacy_kernel__precautionary_reading, renewable_energy_developers).
narrative_ontology:constraint_beneficiary(technology_legitimacy_kernel__precautionary_reading, renewable_supply_chain).
narrative_ontology:constraint_beneficiary(technology_legitimacy_kernel__precautionary_reading, precautionary_policy_advocates).
narrative_ontology:constraint_victim(technology_legitimacy_kernel__precautionary_reading, nuclear_industry).
narrative_ontology:constraint_victim(technology_legitimacy_kernel__precautionary_reading, future_generations).
narrative_ontology:constraint_victim(technology_legitimacy_kernel__precautionary_reading, ratepayers_in_nuclear_phaseout_jurisdictions).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(technology_legitimacy_kernel__precautionary_reading, renewable_supply_chain).
narrative_ontology:constraint_vindicates(technology_legitimacy_kernel__precautionary_reading, precautionary_principle_in_climate_policy).
narrative_ontology:constraint_vindicates(technology_legitimacy_kernel__precautionary_reading, intergenerational_equity_doctrine).
narrative_ontology:constraint_vindicates(technology_legitimacy_kernel__precautionary_reading, reversibility_as_legitimacy_criterion).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Wind, solar, and storage developers gain policy preference, subsidy eligibility, and streamlined permitting because their decommissioning is reversible within a generation. They lobby to maintain and expand the legitimacy filter. Their exit is mobile — they can shift technologies or jurisdictions if the filter changes.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__precautionary_reading, renewable_energy_developers, beneficiary,
    organized, biographical, mobile, global).

% Manufacturers of turbines, panels, batteries, and grid equipment benefit from sustained demand driven by the filter. They also bear costs when the filter tightens (e.g., recycling mandates, critical mineral sourcing rules). Exit is constrained by capital intensity and geographic concentration of supply chains.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__precautionary_reading, renewable_supply_chain, beneficiary,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(technology_legitimacy_kernel__precautionary_reading, renewable_supply_chain, payer).

% NGOs, some regulators, and Green parties set the agenda by defining and defending the reversibility criterion. They benefit from the constraint's existence as a policy tool but do not directly collect financial rents. Their exit is analytical — they can revise their position if evidence shifts.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__precautionary_reading, precautionary_policy_advocates, agenda_setter,
    institutional, generational, analytical, global).

% Existing nuclear operators and vendors are excluded from 'legitimate mitigation' status, losing access to green finance, taxonomy inclusion, and policy support. Their assets are stranded or devalued; decommissioning liabilities accelerate. Exit is trapped — sunk capital is massive, regulatory license withdrawal is hard to reverse, and workforce specialization prevents pivot.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__precautionary_reading, nuclear_industry, payer,
    powerful, biographical, trapped, global).

% Bear the climate damages if the precautionary filter slows decarbonization enough to miss temperature targets (velocity drag). Also bear avoided legacy costs if the filter successfully prevents irreversible contamination. Their situation is identity_locked — they cannot exit the climate system or the intergenerational contract; their interests are represented only by proxies.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__precautionary_reading, future_generations, payer,
    powerless, civilizational, identity_locked, universal).

% Households and firms in jurisdictions that phase out nuclear under the precautionary filter (e.g., Germany post-2011, Belgium, Spain) pay higher electricity prices and system integration costs. Exit is constrained — they can reduce demand or self-generate but cannot easily change the policy regime.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__precautionary_reading, ratepayers_in_nuclear_phaseout_jurisdictions, payer,
    moderate, biographical, constrained, national).

% Transmission operators and reliability coordinators who warn that the filter excludes firm capacity needed for grid stability. They are excluded from the legitimacy determination despite operational expertise. Exit is constrained — they must operate the grid as given but their warnings are discounted in technology legitimacy proceedings.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__precautionary_reading, grid_reliability_engineers, excluded,
    organized, biographical, constrained, national).

% Planners in emerging economies who need affordable, scalable firm power and find the precautionary filter excludes nuclear and CCS options that fit their grid and finance conditions. They are excluded from the Western-dominated legitimacy discourse. Exit is constrained — they can reject the filter but lose access to climate finance tied to it.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__precautionary_reading, developing_nation_energy_planners, excluded,
    moderate, generational, constrained, global).

% IPCC and national assessment bodies that evaluate mitigation pathways. They observe the filter's effect on feasible pathway space but do not set the legitimacy criterion. Their exit is analytical — they report on consequences of different technology sets.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__precautionary_reading, climate_science_assessors, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a defensible, principle-based filter for climate mitigation technology prioritization under deep uncertainty about long-term failure modes, giving policymakers a criterion that internalizes intergenerational risk without requiring precise probabilistic quantification of tail events.
% TRANSFER_FUNCTION: Moves policy support, subsidies, green finance eligibility, and streamlined permitting from excluded technologies (nuclear, CCS, geoengineering) to included technologies (wind, solar, storage, efficiency). Moves system integration costs and potential velocity-drag climate damages to ratepayers and future generations. Moves avoided legacy contamination benefits to future generations if the filter works as intended.
% ABSENT_VOICES: Nuclear engineers and operators (excluded by criterion design), developing nation energy ministries (excluded from legitimacy-setting forums), communities hosting renewable extraction (critical minerals) whose legacy costs are not yet assessed by the filter, and future generations who cannot speak but bear both avoided and incurred irreversible costs.
% DISAPPEARANCE_RATIONALE: If the precautionary legitimacy filter vanished overnight, nuclear would re-enter green taxonomies and climate finance, CCS and geoengineering would gain legitimacy, renewable deployment would lose its preferential policy status, and the global mitigation portfolio would restructure around technology-neutral cost optimization. The energy transition's technology composition, finance flows, and geopolitical dependencies would reorganize substantially.
% FOUNDING_PROBLEM: The precautionary reading was built to solve the problem of technologies whose worst-case failures create irreversible, intergenerational legacies (nuclear meltdowns, radioactive waste, persistent chemical contamination) that cannot be remedied by the generation that benefits from the technology, under conditions where probabilistic risk assessment is unreliable for tail events.
% FOUNDING_PROBLEM_CORROBORATION: Environmental NGOs and the German Ethics Commission on Safe Energy Supply (2011) attest the problem is live (Fukushima, PFAS, climate tipping points). The nuclear industry, IAEA, and several national academies attest the problem is substantially solved for Generation III/IV designs (passive safety, waste vitrification, deep geological repositories) and the constraint now serves renewable rent-seeking. Independent systems analysts (MIT Energy Initiative, IEA) attest the problem is contested — modern nuclear risks are lower but not zero, and renewable legacy costs (critical mineral toxicity, blade waste) are emerging.
narrative_ontology:disappearance_verdict(technology_legitimacy_kernel__precautionary_reading, world_rearranges).
narrative_ontology:founding_problem_status(technology_legitimacy_kernel__precautionary_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(technology_legitimacy_kernel__precautionary_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(technology_legitimacy_kernel__precautionary_reading, 'none', 1).
narrative_ontology:epsilon_provenance(technology_legitimacy_kernel__precautionary_reading, 0.65, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(technology_legitimacy_kernel__precautionary_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(technology_legitimacy_kernel__precautionary_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(technology_legitimacy_kernel__precautionary_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.65) reflects the constraint's dual nature: it coordinates genuine precaution (low theater at origin) while extracting from nuclear incumbents and potentially from future generations via velocity drag. Suppression (0.55) is moderate — the filter operates through permitting and finance rather than direct bans, but the effect is exclusionary. Theater ratio (0.3) has crept up as the criterion is invoked to justify pre-determined technology choices rather than as an open assessment. Accessibility collapse (0.6) is significant: once the reversibility threshold is adopted, nuclear and CCS are structurally excluded from 'legitimate' status regardless of empirical updates. Resistance (0.5) comes from nuclear advocates, some labor unions, grid reliability engineers, and developing nations arguing for technology neutrality. The claimed type is tangled_rope: coordination function (precautionary governance) + asymmetric extraction (nuclear exclusion, cost shifting) + active enforcement.
 *
 * PERSPECTIVAL GAP:
 *   From the renewable developer seat, this is a rope: a genuine coordination mechanism that internalizes intergenerational risk. From the nuclear industry seat, it is a snare: a criterion designed to exclude them while renewables' own legacy costs (critical minerals, land use, recycling) are assessed more leniently. From the future-generations seat, the classification is contested: if the filter prevents nuclear deployment that would have avoided irreversible warming, it extracts from the very constituency it claims to protect. The engine computes this seat divergence from the structural data; the claimed_type does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Renewable developers are structural beneficiaries (d ~ 0.15): they gain market access, subsidies, and regulatory preference. Nuclear industry is a structural target (d ~ 0.85): excluded from legitimacy, stranded assets, decommissioning liabilities accelerated. Future generations are targets (d ~ 0.75) if velocity drag materializes, but beneficiaries (d ~ 0.25) if irreversible legacies are genuinely avoided — this ambiguity is the core omega. Ratepayers in phaseout jurisdictions are targets (d ~ 0.7). Policymakers are agenda_setters with analytical exit (d ~ 0.5). The directionality derivation from beneficiary/victim declarations plus exit options (nuclear: trapped by sunk capital; renewables: mobile with policy support; future generations: identity_locked to the climate outcome) produces the expected d gradient.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (nuclear/chemical irreversible legacies under uncertainty) is contested — some argue it is live (Fukushima, PFAS), others that it is dead for modern nuclear designs and the constraint now serves renewable rent-seeking. The constraint shows mandatrophy signals: rising theater_ratio, rising extractiveness, hardening enforcement after the coordination problem (early renewable deployment) was substantially solved. But the precautionary principle itself remains a live governance norm, so the constraint is not a pure piton — it retains a genuine coordination core even as extraction accumulates.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reversibility_threshold_ambiguity,
    'What counts as ''reversible within a generation'' — does it require full site restoration, or is monitored containment sufficient? Does nuclear waste vitrification and deep geological disposal count as bounded?',
    'International regulatory convergence on decommissioning standards (IAEA, NEA) and empirical data from completed renewable and nuclear decommissioning projects.',
    'If nuclear waste management meets the threshold, nuclear re-enters the legitimate set and the constraint''s extraction profile shifts; if renewables'' land-use legacy exceeds the threshold, the beneficiary set shrinks.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reversibility_threshold_ambiguity, conceptual, 'Whether the reversibility criterion structurally excludes nuclear or can accommodate advanced waste management.').

omega_variable(
    precautionary_vs_velocity_tension,
    'Does the precautionary reading''s exclusion of nuclear materially slow deployment velocity, and if so, does that velocity loss itself create irreversible climate harm that the precautionary reading fails to internalize?',
    'Integrated assessment modeling comparing net-zero pathways with and without nuclear under the precautionary legitimacy filter, with Monte Carlo sampling of deployment rates and learning curves.',
    'If velocity loss creates net irreversible harm, the precautionary reading extracts from future generations via a different channel — it becomes a snare for the very constituency it claims to protect.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(precautionary_vs_velocity_tension, empirical, 'Whether the precautionary criterion''s deployment drag creates the irreversible legacy costs it purports to avoid.').

omega_variable(
    kernel_reading_identity,
    'Is this constraint one reading of the technology_legitimacy_kernel, with sibling readings reliability_primacy_reading and velocity_primacy_reading?',
    'Committee deliberation record and SCOPE manifest decomposition confirm kernel structure.',
    'Confirms this story instantiates a single ε-invariant reading; sibling readings are separate constraints linked via network.affects_constraints.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Committee-frame kernel membership for this reading.').

omega_variable(
    reading_relation_to_reliability_primacy,
    'Does the precautionary reading foreclose, coexist with, or influence the reliability_primacy_reading?',
    'Analyze whether a single policy framework can simultaneously require reversibility-within-a-generation AND dispatchable-baseload-capability as legitimacy criteria.',
    'If they coexist, both readings remain live in different jurisdictions; if precautionary forecloses reliability, any framework adopting precautionary cannot also adopt reliability as a legitimacy gate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_relation_to_reliability_primacy, conceptual, 'Structural relationship from precautionary_reading to reliability_primacy_reading.').

omega_variable(
    reading_relation_to_velocity_primacy,
    'Does the precautionary reading foreclose, coexist with, or influence the velocity_primacy_reading?',
    'Analyze whether the precautionary filter''s exclusion of nuclear and slow-permitting renewables creates structural pressure on deployment timelines without logically eliminating the velocity criterion.',
    'If influences, the precautionary reading changes the resource/legitimacy conditions for velocity_primacy without foreclosing it — velocity advocates must work within a narrowed technology set.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_relation_to_velocity_primacy, conceptual, 'Structural relationship from precautionary_reading to velocity_primacy_reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(technology_legitimacy_kernel__precautionary_reading, 1990, 2030).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tech_legitimacy_precautionary_tr_t1990, technology_legitimacy_kernel__precautionary_reading, theater_ratio, 1990, 0.15).
narrative_ontology:measurement(tech_legitimacy_precautionary_tr_t2000, technology_legitimacy_kernel__precautionary_reading, theater_ratio, 2000, 0.18).
narrative_ontology:measurement(tech_legitimacy_precautionary_tr_t2010, technology_legitimacy_kernel__precautionary_reading, theater_ratio, 2010, 0.22).
narrative_ontology:measurement(tech_legitimacy_precautionary_tr_t2015, technology_legitimacy_kernel__precautionary_reading, theater_ratio, 2015, 0.25).
narrative_ontology:measurement(tech_legitimacy_precautionary_tr_t2020, technology_legitimacy_kernel__precautionary_reading, theater_ratio, 2020, 0.28).
narrative_ontology:measurement(tech_legitimacy_precautionary_tr_t2025, technology_legitimacy_kernel__precautionary_reading, theater_ratio, 2025, 0.29).
narrative_ontology:measurement(tech_legitimacy_precautionary_tr_t2030, technology_legitimacy_kernel__precautionary_reading, theater_ratio, 2030, 0.3).

% Extraction over time
narrative_ontology:measurement(tech_legitimacy_precautionary_be_t1990, technology_legitimacy_kernel__precautionary_reading, base_extractiveness, 1990, 0.35).
narrative_ontology:measurement(tech_legitimacy_precautionary_be_t2000, technology_legitimacy_kernel__precautionary_reading, base_extractiveness, 2000, 0.42).
narrative_ontology:measurement(tech_legitimacy_precautionary_be_t2010, technology_legitimacy_kernel__precautionary_reading, base_extractiveness, 2010, 0.52).
narrative_ontology:measurement(tech_legitimacy_precautionary_be_t2015, technology_legitimacy_kernel__precautionary_reading, base_extractiveness, 2015, 0.58).
narrative_ontology:measurement(tech_legitimacy_precautionary_be_t2020, technology_legitimacy_kernel__precautionary_reading, base_extractiveness, 2020, 0.62).
narrative_ontology:measurement(tech_legitimacy_precautionary_be_t2025, technology_legitimacy_kernel__precautionary_reading, base_extractiveness, 2025, 0.64).
narrative_ontology:measurement(tech_legitimacy_precautionary_be_t2030, technology_legitimacy_kernel__precautionary_reading, base_extractiveness, 2030, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(tech_legitimacy_precautionary_su_t1990, technology_legitimacy_kernel__precautionary_reading, suppression_requirement, 1990, 0.3).
narrative_ontology:measurement(tech_legitimacy_precautionary_su_t2000, technology_legitimacy_kernel__precautionary_reading, suppression_requirement, 2000, 0.38).
narrative_ontology:measurement(tech_legitimacy_precautionary_su_t2010, technology_legitimacy_kernel__precautionary_reading, suppression_requirement, 2010, 0.45).
narrative_ontology:measurement(tech_legitimacy_precautionary_su_t2015, technology_legitimacy_kernel__precautionary_reading, suppression_requirement, 2015, 0.5).
narrative_ontology:measurement(tech_legitimacy_precautionary_su_t2020, technology_legitimacy_kernel__precautionary_reading, suppression_requirement, 2020, 0.52).
narrative_ontology:measurement(tech_legitimacy_precautionary_su_t2025, technology_legitimacy_kernel__precautionary_reading, suppression_requirement, 2025, 0.54).
narrative_ontology:measurement(tech_legitimacy_precautionary_su_t2030, technology_legitimacy_kernel__precautionary_reading, suppression_requirement, 2030, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(technology_legitimacy_kernel__precautionary_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(technology_legitimacy_kernel__precautionary_reading, 0.12).
narrative_ontology:affects_constraint(technology_legitimacy_kernel__precautionary_reading, technology_legitimacy_kernel__reliability_primacy_reading).
narrative_ontology:affects_constraint(technology_legitimacy_kernel__precautionary_reading, technology_legitimacy_kernel__velocity_primacy_reading).
narrative_ontology:affects_constraint(technology_legitimacy_kernel__precautionary_reading, nuclear_phaseout_policy).
narrative_ontology:affects_constraint(technology_legitimacy_kernel__precautionary_reading, renewable_subsidy_regime).
narrative_ontology:affects_constraint(technology_legitimacy_kernel__precautionary_reading, eu_taxonomy_delegated_act).
narrative_ontology:affects_constraint(technology_legitimacy_kernel__precautionary_reading, german_energiewende_legislation).

% DUAL FORMULATION NOTE:
% This constraint is one member of the technology_legitimacy_kernel family (three readings). The precautionary reading centers intergenerational reversibility; the reliability reading centers grid stability; the velocity reading centers deployment speed. Each has distinct beneficiary/victim sets and extraction profiles. They are linked via affects_constraints because they compete for the same policy gate (what counts as 'legitimate mitigation') and each reading's adoption changes the legitimacy conditions for the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(technology_legitimacy_kernel__precautionary_reading, institutional, 0.5).
constraint_indexing:directionality_override(technology_legitimacy_kernel__precautionary_reading, organized, 0.7).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
