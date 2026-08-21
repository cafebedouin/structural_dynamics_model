% ============================================================================
% CONSTRAINT STORY: climate_mitigation_legitimacy__degrowth_sufficiency_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
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
 *   human_readable: Climate Mitigation Legitimacy: Degrowth Sufficiency Reading
 *   domain: energy_policy/climate_mitigation/technology_governance
 *
 * SUMMARY:
 *   This constraint represents the 'degrowth sufficiency' reading of climate
 *   mitigation legitimacy, asserting that decarbonization fundamentally
 *   requires demand reduction, rendering large-scale generation expansion
 *   unnecessary. This reading challenges conventional approaches that focus
 *   on replacing fossil fuels with large-scale nuclear or renewable energy.
 *   It redefines the problem as one of overconsumption and systemic growth,
 *   rather than merely a technological transition. The constraint is claimed
 *   as a Tangled Rope because it offers a coordination function (a coherent
 *   path to decarbonization) but also involves significant, actively enforced
 *   extraction from growth-dependent sectors and consumers.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_mitigation_legitimacy__degrowth_sufficiency_reading, 0.65).
domain_priors:suppression_score(climate_mitigation_legitimacy__degrowth_sufficiency_reading, 0.7).
domain_priors:theater_ratio(climate_mitigation_legitimacy__degrowth_sufficiency_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__degrowth_sufficiency_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__degrowth_sufficiency_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__degrowth_sufficiency_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__degrowth_sufficiency_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__degrowth_sufficiency_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_mitigation_legitimacy__degrowth_sufficiency_reading, tangled_rope).
narrative_ontology:human_readable(climate_mitigation_legitimacy__degrowth_sufficiency_reading, "Climate Mitigation Legitimacy: Degrowth Sufficiency Reading").
narrative_ontology:topic_domain(climate_mitigation_legitimacy__degrowth_sufficiency_reading, "energy_policy/climate_mitigation/technology_governance").

domain_priors:requires_active_enforcement(climate_mitigation_legitimacy__degrowth_sufficiency_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_mitigation_legitimacy__degrowth_sufficiency_reading, '932b628d-ec5e-4889-903c-f392fb46d19e').
narrative_ontology:cs_kernel_codification('932b628d-ec5e-4889-903c-f392fb46d19e', distributed).
narrative_ontology:cs_authority_grounding('932b628d-ec5e-4889-903c-f392fb46d19e', distributed).
narrative_ontology:cs_reading_relation('932b628d-ec5e-4889-903c-f392fb46d19e', climate_mitigation_legitimacy__baseload_necessity_reading, coexists_with).
narrative_ontology:cs_reading_relation('932b628d-ec5e-4889-903c-f392fb46d19e', climate_mitigation_legitimacy__renewable_primacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('932b628d-ec5e-4889-903c-f392fb46d19e', climate_mitigation_legitimacy__portfolio_pragmatism_reading, coexists_with).
narrative_ontology:cs_axiom('932b628d-ec5e-4889-903c-f392fb46d19e', foundational, decarbonization_requires_demand_reduction).
narrative_ontology:cs_axiom_status(decarbonization_requires_demand_reduction, holdable).
narrative_ontology:cs_axiom_grounding('932b628d-ec5e-4889-903c-f392fb46d19e', decarbonization_requires_demand_reduction, empirically_contingent).
narrative_ontology:cs_axiom('932b628d-ec5e-4889-903c-f392fb46d19e', foundational, large_scale_generation_unnecessary).
narrative_ontology:cs_axiom_status(large_scale_generation_unnecessary, holdable).
narrative_ontology:cs_axiom_grounding('932b628d-ec5e-4889-903c-f392fb46d19e', large_scale_generation_unnecessary, empirically_contingent).
narrative_ontology:cs_reference_frame('932b628d-ec5e-4889-903c-f392fb46d19e', ecological_limits_framework).
narrative_ontology:cs_drift_state('932b628d-ec5e-4889-903c-f392fb46d19e', contemporary, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('932b628d-ec5e-4889-903c-f392fb46d19e', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(climate_mitigation_legitimacy__degrowth_sufficiency_reading, climate_mitigation_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__degrowth_sufficiency_reading, degrowth_advocates).
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__degrowth_sufficiency_reading, local_resilience_movements).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__degrowth_sufficiency_reading, nuclear_industry).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__degrowth_sufficiency_reading, renewable_energy_developers).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__degrowth_sufficiency_reading, economic_growth_coalition).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__degrowth_sufficiency_reading, energy_consumers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Promote policies that prioritize demand reduction and energy system downsizing, framing large-scale generation as unnecessary and environmentally harmful. They benefit from the legitimacy of this framing in policy debates and resource allocation.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__degrowth_sufficiency_reading, degrowth_advocates, agenda_setter,
    organized, generational, identity_locked, global).

% Advocate for decentralized, low-energy systems and local food production, aligning with the demand reduction narrative. They benefit from policy support and public discourse that de-emphasizes large-scale infrastructure.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__degrowth_sufficiency_reading, local_resilience_movements, beneficiary,
    moderate, biographical, constrained, local).

% Faces delegitimization and reduced investment due to the narrative that large-scale generation is unnecessary. They bear the cost of diminished political support and capital access, despite offering carbon-free baseload power.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__degrowth_sufficiency_reading, nuclear_industry, payer,
    powerful, generational, constrained, national).

% While promoting clean energy, they are targeted by the 'sufficiency' argument against large-scale expansion, which limits their market for utility-scale projects. They bear the cost of reduced public funding and policy support for large projects.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__degrowth_sufficiency_reading, renewable_energy_developers, payer,
    powerful, biographical, constrained, global).

% Represents industries and governments whose models depend on continuous economic and energy growth. They bear the cost of policy shifts towards degrowth, which fundamentally challenges their operating assumptions and investment strategies.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__degrowth_sufficiency_reading, economic_growth_coalition, payer,
    institutional, generational, identity_locked, global).

% Are expected to reduce demand and accept potentially lower energy services. They bear the direct costs of lifestyle changes and the indirect costs of reduced economic activity, with limited ability to opt out of national energy policies.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__degrowth_sufficiency_reading, energy_consumers, payer,
    powerless, immediate, trapped, national).

% Analyze climate models and mitigation pathways, often finding that demand reduction is a necessary but insufficient component of decarbonization. They observe the policy debate without directly benefiting or paying from this specific constraint.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__degrowth_sufficiency_reading, climate_scientists, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates societal efforts towards decarbonization by prioritizing demand reduction and efficiency, aiming to align energy consumption with ecological limits and reduce the need for large-scale, capital-intensive energy projects.
% TRANSFER_FUNCTION: Transfers societal resources and political capital away from large-scale energy generation projects (both nuclear and renewables) towards demand-side management, efficiency programs, and local, small-scale energy solutions. It also transfers the burden of energy reduction onto consumers and growth-dependent industries.
% ABSENT_VOICES: Proponents of rapid, large-scale deployment of carbon-free energy (both nuclear and utility-scale renewables) are marginalized in this discourse, as their solutions are framed as unnecessary or undesirable. They would argue that demand reduction alone is too slow or insufficient to meet climate targets.
% DISAPPEARANCE_RATIONALE: If this reading vanished, the policy landscape for climate mitigation would shift dramatically. Investment would flow back into large-scale generation, public discourse would re-emphasize supply-side solutions, and the political capital of degrowth advocates would diminish, leading to a reorganization of climate strategy.
% FOUNDING_PROBLEM: The perceived failure of industrial society to address climate change through technological fixes alone, coupled with concerns about resource depletion and ecological overshoot, leading to a call for fundamental societal transformation.
% FOUNDING_PROBLEM_CORROBORATION: Ecological economists and some environmental scientists corroborate the underlying concerns about planetary boundaries and the limits to growth. However, mainstream economists and energy policy experts often contest the feasibility and desirability of a degrowth pathway for decarbonization, arguing it imposes unacceptable social costs.
narrative_ontology:disappearance_verdict(climate_mitigation_legitimacy__degrowth_sufficiency_reading, world_rearranges).
narrative_ontology:founding_problem_status(climate_mitigation_legitimacy__degrowth_sufficiency_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_mitigation_legitimacy__degrowth_sufficiency_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(climate_mitigation_legitimacy__degrowth_sufficiency_reading, 'none', 1).
narrative_ontology:epsilon_provenance(climate_mitigation_legitimacy__degrowth_sufficiency_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness (0.65) stems from the redirection of capital and policy away from established energy industries and the imposition of demand reduction on consumers. Suppression (0.70) is high because this reading actively delegitimizes and suppresses alternative, supply-side mitigation strategies, requiring active advocacy and policy enforcement to maintain its dominance in the discourse. The theater ratio (0.20) is relatively low, as the advocates of this reading are genuinely committed to its principles, but some performative aspects exist in framing all large-scale energy as 'unnecessary' even when it is carbon-free. Resistance (0.80) is high due to strong opposition from industries and political factions committed to economic growth and large-scale energy development.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of degrowth advocates, this is a necessary re-orientation towards ecological limits, a genuine coordination of human activity with planetary boundaries. From the perspective of the nuclear or large-scale renewable industries, it is an extractive constraint that unfairly targets their carbon-free solutions, suppressing viable pathways to decarbonization based on a contested premise about growth.
 *
 * DIRECTIONALITY LOGIC:
 *   Degrowth advocates and local resilience movements are beneficiaries, gaining legitimacy and policy traction. The nuclear industry, renewable energy developers (for large-scale projects), and the broader economic growth coalition are victims, facing reduced investment and political opposition. Energy consumers are also victims, bearing the costs of demand reduction. Climate scientists act as observers, providing data that may or may not align with this reading's policy prescriptions.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sufficiency_feasibility_empirical,
    'Is it empirically feasible to achieve rapid decarbonization solely through demand reduction and small-scale solutions, without significant large-scale generation expansion?',
    'Detailed, region-specific energy system modeling that integrates demand reduction targets with projected technological capabilities and social acceptance of lifestyle changes.',
    'If empirically infeasible, the constraint''s claimed coordination function collapses, revealing it as primarily extractive (snare) by suppressing necessary alternatives. If feasible, its legitimacy as a coordination mechanism strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sufficiency_feasibility_empirical, empirical, 'Empirical test of whether demand reduction alone can meet climate goals.').

omega_variable(
    growth_decoupling_conceptual,
    'Is economic growth fundamentally incompatible with decarbonization, or can growth be decoupled from emissions through technological innovation and efficiency?',
    'Long-term observation of GDP and emissions trajectories in advanced economies, coupled with analysis of the underlying drivers of decoupling (e.g., efficiency gains vs. offshoring emissions).',
    'If decoupling is robust, the foundational premise of this reading (that growth itself is the problem) is weakened, shifting the constraint towards a more contested or even a snare classification. If decoupling is shown to be illusory, the reading''s legitimacy is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(growth_decoupling_conceptual, conceptual, 'Whether economic growth can be reconciled with climate goals.').

omega_variable(
    social_acceptance_of_demand_reduction,
    'What is the actual level of social acceptance for the lifestyle changes and energy rationing implied by aggressive demand reduction policies?',
    'Public opinion surveys, behavioral economics studies, and pilot programs implementing demand reduction measures at scale.',
    'Low social acceptance would indicate that the constraint''s suppression is primarily external and coercive, rather than internalized, increasing its effective extractiveness and pushing it towards a Snare classification. High acceptance would support its coordination function.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(social_acceptance_of_demand_reduction, empirical, 'Public willingness to accept demand reduction measures.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_mitigation_legitimacy__degrowth_sufficiency_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_mitigation_legitimacy__degrowth_sufficiency_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(clim_tr_t5, climate_mitigation_legitimacy__degrowth_sufficiency_reading, theater_ratio, 5, 0.17).
narrative_ontology:measurement(clim_tr_t10, climate_mitigation_legitimacy__degrowth_sufficiency_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement(clim_tr_t15, climate_mitigation_legitimacy__degrowth_sufficiency_reading, theater_ratio, 15, 0.19).
narrative_ontology:measurement(clim_tr_t20, climate_mitigation_legitimacy__degrowth_sufficiency_reading, theater_ratio, 20, 0.2).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_mitigation_legitimacy__degrowth_sufficiency_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(clim_be_t5, climate_mitigation_legitimacy__degrowth_sufficiency_reading, base_extractiveness, 5, 0.58).
narrative_ontology:measurement(clim_be_t10, climate_mitigation_legitimacy__degrowth_sufficiency_reading, base_extractiveness, 10, 0.61).
narrative_ontology:measurement(clim_be_t15, climate_mitigation_legitimacy__degrowth_sufficiency_reading, base_extractiveness, 15, 0.63).
narrative_ontology:measurement(clim_be_t20, climate_mitigation_legitimacy__degrowth_sufficiency_reading, base_extractiveness, 20, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_mitigation_legitimacy__degrowth_sufficiency_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(clim_su_t5, climate_mitigation_legitimacy__degrowth_sufficiency_reading, suppression_requirement, 5, 0.63).
narrative_ontology:measurement(clim_su_t10, climate_mitigation_legitimacy__degrowth_sufficiency_reading, suppression_requirement, 10, 0.66).
narrative_ontology:measurement(clim_su_t15, climate_mitigation_legitimacy__degrowth_sufficiency_reading, suppression_requirement, 15, 0.68).
narrative_ontology:measurement(clim_su_t20, climate_mitigation_legitimacy__degrowth_sufficiency_reading, suppression_requirement, 20, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_mitigation_legitimacy__degrowth_sufficiency_reading, resource_allocation).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__degrowth_sufficiency_reading, climate_mitigation_legitimacy__baseload_necessity_reading).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__degrowth_sufficiency_reading, climate_mitigation_legitimacy__renewable_primacy_reading).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__degrowth_sufficiency_reading, climate_mitigation_legitimacy__portfolio_pragmatism_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of four readings of the 'climate_mitigation_legitimacy' kernel, each representing a distinct approach to decarbonization. This 'degrowth sufficiency' reading emphasizes demand reduction, while others focus on different supply-side solutions or pragmatic portfolios. All readings are linked to reflect their shared contested domain.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
