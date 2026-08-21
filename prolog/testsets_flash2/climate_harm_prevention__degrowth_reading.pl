% ============================================================================
% CONSTRAINT STORY: climate_harm_prevention__degrowth_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_harm_prevention__degrowth_reading, []).

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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: climate_harm_prevention__degrowth_reading
 *   human_readable: Degrowth Imperative for Climate Harm Prevention
 *   domain: climate_policy/political_economy/intergenerational_ethics
 *
 * SUMMARY:
 *   This constraint represents the 'degrowth' reading of climate harm
 *   prevention, arguing that legitimate climate response requires planned
 *   economic contraction in the Global North. It posits that mitigation
 *   efforts within a growth framework are physically and politically
 *   impossible. This reading is instantiated as a Snare due to its high
 *   extractiveness from Global North consumers and industries, and the high
 *   suppression required to enforce such a radical economic shift. The
 *   metrics reflect the perceived necessity and difficulty of implementing
 *   this constraint, with rising extractiveness and suppression over time as
 *   the climate crisis intensifies and resistance grows.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_harm_prevention__degrowth_reading, 0.85).
domain_priors:suppression_score(climate_harm_prevention__degrowth_reading, 0.7).
domain_priors:theater_ratio(climate_harm_prevention__degrowth_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_harm_prevention__degrowth_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(climate_harm_prevention__degrowth_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(climate_harm_prevention__degrowth_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_harm_prevention__degrowth_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(climate_harm_prevention__degrowth_reading, resistance, 0.9).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_harm_prevention__degrowth_reading, snare).
narrative_ontology:human_readable(climate_harm_prevention__degrowth_reading, "Degrowth Imperative for Climate Harm Prevention").
narrative_ontology:topic_domain(climate_harm_prevention__degrowth_reading, "climate_policy/political_economy/intergenerational_ethics").

domain_priors:requires_active_enforcement(climate_harm_prevention__degrowth_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_harm_prevention__degrowth_reading, '6ed8505c-8b2e-4a98-bf71-50ac8d6e309e').
narrative_ontology:cs_kernel_codification('6ed8505c-8b2e-4a98-bf71-50ac8d6e309e', distributed).
narrative_ontology:cs_authority_grounding('6ed8505c-8b2e-4a98-bf71-50ac8d6e309e', diffuse_epistemic).
narrative_ontology:cs_reading_relation('6ed8505c-8b2e-4a98-bf71-50ac8d6e309e', climate_harm_prevention__mitigation_priority, forecloses).
narrative_ontology:cs_reading_relation('6ed8505c-8b2e-4a98-bf71-50ac8d6e309e', climate_harm_prevention__adaptation_priority, coexists_with).
narrative_ontology:cs_axiom('6ed8505c-8b2e-4a98-bf71-50ac8d6e309e', foundational, economic_growth_is_ecologically_unsustainable).
narrative_ontology:cs_axiom_status(economic_growth_is_ecologically_unsustainable, holdable).
narrative_ontology:cs_axiom_grounding('6ed8505c-8b2e-4a98-bf71-50ac8d6e309e', economic_growth_is_ecologically_unsustainable, empirically_contingent).
narrative_ontology:cs_axiom('6ed8505c-8b2e-4a98-bf71-50ac8d6e309e', foundational, intergenerational_equity_requires_degrowth).
narrative_ontology:cs_axiom_status(intergenerational_equity_requires_degrowth, holdable).
narrative_ontology:cs_axiom_grounding('6ed8505c-8b2e-4a98-bf71-50ac8d6e309e', intergenerational_equity_requires_degrowth, deontological).
narrative_ontology:cs_reference_frame('6ed8505c-8b2e-4a98-bf71-50ac8d6e309e', planetary_boundaries_framework).
narrative_ontology:cs_drift_state('6ed8505c-8b2e-4a98-bf71-50ac8d6e309e', contemporary_policy_discourse, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('6ed8505c-8b2e-4a98-bf71-50ac8d6e309e', '').
narrative_ontology:cs_kernel_id(climate_harm_prevention__degrowth_reading, climate_harm_prevention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_harm_prevention__degrowth_reading, global_south_populations).
narrative_ontology:constraint_beneficiary(climate_harm_prevention__degrowth_reading, future_generations).
narrative_ontology:constraint_victim(climate_harm_prevention__degrowth_reading, global_north_consumers).
narrative_ontology:constraint_victim(climate_harm_prevention__degrowth_reading, extractive_industries).
narrative_ontology:constraint_victim(climate_harm_prevention__degrowth_reading, growth_oriented_economies).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Would bear the direct costs of planned economic contraction, including reduced consumption, altered lifestyles, and potential shifts in employment. Their current consumption patterns are seen as a primary driver of climate harm.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__degrowth_reading, global_north_consumers, payer,
    powerful, immediate, constrained, global).

% Would face severe restrictions or outright cessation of operations under a degrowth framework, as their business models are predicated on continuous resource extraction and economic expansion. Their political influence currently resists such changes.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__degrowth_reading, extractive_industries, payer,
    institutional, biographical, trapped, global).

% National and international economic systems built on the premise of continuous GDP growth would need fundamental restructuring. The identity of these economies is deeply intertwined with growth metrics.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__degrowth_reading, growth_oriented_economies, payer,
    institutional, generational, identity_locked, global).

% Would benefit from reduced climate impacts, greater ecological stability, and potentially a more equitable distribution of global resources. They currently bear a disproportionate share of climate harms despite lower historical emissions.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__degrowth_reading, global_south_populations, beneficiary,
    organized, generational, constrained, global).

% Are the ultimate beneficiaries of effective climate action, as their well-being and survival depend on a stable planetary environment. They have no direct voice in current policy decisions.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__degrowth_reading, future_generations, beneficiary,
    powerless, civilizational, trapped, universal).

% Provide the scientific basis for understanding climate change and the efficacy of various responses. Their models and data inform the urgency and scale of action required, often pointing to the inadequacy of current mitigation efforts.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__degrowth_reading, climate_scientists, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aims to coordinate global economic activity and resource use within planetary boundaries, ensuring ecological stability and intergenerational equity by explicitly rejecting the growth imperative.
% TRANSFER_FUNCTION: Transfers ecological space, resource availability, and reduced climate risk from Global North present consumption to Global South populations and future generations, by imposing planned economic contraction.
% ABSENT_VOICES: The voices of future generations are structurally absent from current political and economic decision-making, though their interests are represented by advocates. Non-human ecosystems also lack direct representation.
% DISAPPEARANCE_RATIONALE: If the degrowth imperative vanished, the current growth-oriented economic system would continue, leading to escalating climate harm and resource depletion, fundamentally altering the future world for the worse. The constraint's absence would mean a continuation of the status quo, which this reading views as a path to collapse.
% FOUNDING_PROBLEM: The existential threat of climate change, driven by unsustainable economic growth and resource consumption, particularly in the Global North, leading to ecological collapse and severe intergenerational and global inequity.
% FOUNDING_PROBLEM_CORROBORATION: Climate scientists, ecological economists, and indigenous communities globally corroborate the live status of the problem, citing IPCC reports, biodiversity loss data, and direct experience of climate impacts. This corroboration comes from outside the immediate beneficiaries of degrowth.
narrative_ontology:disappearance_verdict(climate_harm_prevention__degrowth_reading, world_rearranges).
narrative_ontology:founding_problem_status(climate_harm_prevention__degrowth_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_harm_prevention__degrowth_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(climate_harm_prevention__degrowth_reading, 'none', 1).
narrative_ontology:epsilon_provenance(climate_harm_prevention__degrowth_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_harm_prevention__degrowth_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_harm_prevention__degrowth_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_harm_prevention__degrowth_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.85) reflects the significant economic and lifestyle changes demanded from Global North populations and industries. Suppression (0.70) is high because implementing planned contraction against entrenched growth paradigms would require substantial political will and enforcement to overcome resistance from powerful economic interests. Theater ratio is low (0.10) because this reading is a direct, unvarnished call for radical change, with little room for performative gestures or 'greenwashing' within its framework. The increasing extractiveness and suppression over time reflect the growing urgency of the climate crisis and the escalating resistance to fundamental economic restructuring.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of Global North consumers and industries, this constraint is a severe imposition, threatening their economic models and way of life. From the perspective of Global South populations and future generations, it is a necessary and just rebalancing of ecological debt and resource distribution. The engine's per-seat classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Global South populations and future generations are the primary beneficiaries (d near 0.0), as they gain ecological stability and equity. Global North consumers, extractive industries, and growth-oriented economies are the primary targets (d near 1.0), as they bear the costs of contraction and restructuring. Climate scientists act as observers, providing the empirical basis for the constraint's necessity.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    political_feasibility_of_degrowth,
    'Is planned economic contraction in the Global North politically feasible within democratic frameworks, given the inherent resistance from powerful economic interests and consumer preferences?',
    'Empirical observation of policy implementation and public acceptance in nations attempting degrowth-aligned policies. Analysis of political economy models for non-growth transitions.',
    'If politically infeasible, the constraint''s suppression requirement would need to be even higher, or its claimed type would shift towards a Piton (performative but ineffective) or a Snare (requiring authoritarian enforcement). If feasible, it strengthens the claim of a viable path to climate stability.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(political_feasibility_of_degrowth, empirical, 'Uncertainty regarding the political viability of implementing degrowth policies.').

omega_variable(
    economic_contraction_impact_on_global_south,
    'How would planned economic contraction in the Global North impact the economic development and well-being of the Global South, considering current trade dependencies and historical injustices?',
    'Detailed economic modeling that accounts for global supply chains, trade relationships, and potential for new, equitable economic models. Consultation with Global South economists and policymakers.',
    'If degrowth in the North leads to unintended negative consequences for the South, the constraint''s beneficiary structure would be undermined, potentially shifting its classification towards a Tangled Rope or even a Snare for the Global South.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(economic_contraction_impact_on_global_south, empirical, 'Uncertainty about the full economic impacts of degrowth on the Global South.').

omega_variable(
    growth_vs_sustainability_framing,
    'Is economic growth fundamentally incompatible with ecological sustainability, or can ''green growth'' or ''decoupling'' achieve climate goals without contraction?',
    'Long-term empirical data on absolute decoupling of resource use and emissions from GDP growth at a global scale. Scientific consensus on planetary boundaries and resource limits.',
    'If green growth proves viable, the degrowth reading would be conceptually foreclosed, shifting the kernel''s dominant reading towards mitigation_priority. If green growth is shown to be insufficient, the degrowth reading''s necessity is reinforced.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(growth_vs_sustainability_framing, conceptual, 'Fundamental conceptual disagreement on the compatibility of growth and sustainability.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_harm_prevention__degrowth_reading, 2000, 2050).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t2000, climate_harm_prevention__degrowth_reading, theater_ratio, 2000, 0.2).
narrative_ontology:measurement(clim_tr_t2010, climate_harm_prevention__degrowth_reading, theater_ratio, 2010, 0.18).
narrative_ontology:measurement(clim_tr_t2020, climate_harm_prevention__degrowth_reading, theater_ratio, 2020, 0.15).
narrative_ontology:measurement(clim_tr_t2030, climate_harm_prevention__degrowth_reading, theater_ratio, 2030, 0.1).
narrative_ontology:measurement_basis(clim_tr_t2030, projected).
narrative_ontology:measurement(clim_tr_t2040, climate_harm_prevention__degrowth_reading, theater_ratio, 2040, 0.08).
narrative_ontology:measurement_basis(clim_tr_t2040, projected).
narrative_ontology:measurement(clim_tr_t2050, climate_harm_prevention__degrowth_reading, theater_ratio, 2050, 0.05).
narrative_ontology:measurement_basis(clim_tr_t2050, projected).

% Extraction over time
narrative_ontology:measurement(clim_be_t2000, climate_harm_prevention__degrowth_reading, base_extractiveness, 2000, 0.7).
narrative_ontology:measurement(clim_be_t2010, climate_harm_prevention__degrowth_reading, base_extractiveness, 2010, 0.75).
narrative_ontology:measurement(clim_be_t2020, climate_harm_prevention__degrowth_reading, base_extractiveness, 2020, 0.8).
narrative_ontology:measurement(clim_be_t2030, climate_harm_prevention__degrowth_reading, base_extractiveness, 2030, 0.85).
narrative_ontology:measurement_basis(clim_be_t2030, projected).
narrative_ontology:measurement(clim_be_t2040, climate_harm_prevention__degrowth_reading, base_extractiveness, 2040, 0.88).
narrative_ontology:measurement_basis(clim_be_t2040, projected).
narrative_ontology:measurement(clim_be_t2050, climate_harm_prevention__degrowth_reading, base_extractiveness, 2050, 0.9).
narrative_ontology:measurement_basis(clim_be_t2050, projected).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t2000, climate_harm_prevention__degrowth_reading, suppression_requirement, 2000, 0.5).
narrative_ontology:measurement(clim_su_t2010, climate_harm_prevention__degrowth_reading, suppression_requirement, 2010, 0.58).
narrative_ontology:measurement(clim_su_t2020, climate_harm_prevention__degrowth_reading, suppression_requirement, 2020, 0.65).
narrative_ontology:measurement(clim_su_t2030, climate_harm_prevention__degrowth_reading, suppression_requirement, 2030, 0.7).
narrative_ontology:measurement_basis(clim_su_t2030, projected).
narrative_ontology:measurement(clim_su_t2040, climate_harm_prevention__degrowth_reading, suppression_requirement, 2040, 0.75).
narrative_ontology:measurement_basis(clim_su_t2040, projected).
narrative_ontology:measurement(clim_su_t2050, climate_harm_prevention__degrowth_reading, suppression_requirement, 2050, 0.8).
narrative_ontology:measurement_basis(clim_su_t2050, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_harm_prevention__degrowth_reading, resource_allocation).
narrative_ontology:affects_constraint(climate_harm_prevention__degrowth_reading, mitigation_priority).
narrative_ontology:affects_constraint(climate_harm_prevention__degrowth_reading, adaptation_priority).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'climate_harm_prevention' kernel. It directly challenges the premises of 'mitigation_priority' and 'adaptation_priority' by rejecting the growth framework.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
