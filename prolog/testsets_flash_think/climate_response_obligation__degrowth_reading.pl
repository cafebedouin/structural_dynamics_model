% ============================================================================
% CONSTRAINT STORY: climate_response_obligation__degrowth_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_response_obligation__degrowth_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: climate_response_obligation__degrowth_reading
 *   human_readable: Degrowth Obligation for Planetary Boundaries
 *   domain: climate_policy/political_economy/intergenerational_ethics
 *
 * SUMMARY:
 *   This constraint represents the 'degrowth' reading of the broader climate
 *   response obligation, asserting that human economic activity must reduce
 *   material throughput to stay within planetary boundaries, prioritizing
 *   sufficiency over efficiency. It is a highly prescriptive and enforced
 *   constraint on current high-consumption patterns, with planetary systems
 *   and future generations as primary beneficiaries, and current
 *   growth-oriented economies and high-consumers as victims. The constraint
 *   is claimed as a Tangled Rope, acknowledging a genuine coordination
 *   function (avoiding ecological collapse) but with significant, actively
 *   enforced extraction from existing economic structures.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_obligation__degrowth_reading, 0.85).
domain_priors:suppression_score(climate_response_obligation__degrowth_reading, 0.9).
domain_priors:theater_ratio(climate_response_obligation__degrowth_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_obligation__degrowth_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(climate_response_obligation__degrowth_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(climate_response_obligation__degrowth_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_response_obligation__degrowth_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(climate_response_obligation__degrowth_reading, resistance, 0.95).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_obligation__degrowth_reading, tangled_rope).
narrative_ontology:human_readable(climate_response_obligation__degrowth_reading, "Degrowth Obligation for Planetary Boundaries").
narrative_ontology:topic_domain(climate_response_obligation__degrowth_reading, "climate_policy/political_economy/intergenerational_ethics").

domain_priors:requires_active_enforcement(climate_response_obligation__degrowth_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_obligation__degrowth_reading, '2104707e-f0db-4a9e-bdbe-a0f1ac0446be').
narrative_ontology:cs_kernel_codification('2104707e-f0db-4a9e-bdbe-a0f1ac0446be', implicit).
narrative_ontology:cs_authority_grounding('2104707e-f0db-4a9e-bdbe-a0f1ac0446be', expertise).
narrative_ontology:cs_interpretation_layer_present('2104707e-f0db-4a9e-bdbe-a0f1ac0446be').
narrative_ontology:cs_reading_relation('2104707e-f0db-4a9e-bdbe-a0f1ac0446be', climate_response_obligation__mitigation_priority, coexists_with).
narrative_ontology:cs_reading_relation('2104707e-f0db-4a9e-bdbe-a0f1ac0446be', climate_response_obligation__adaptation_priority, forecloses).
narrative_ontology:cs_axiom('2104707e-f0db-4a9e-bdbe-a0f1ac0446be', foundational, planetary_limits_are_absolute).
narrative_ontology:cs_axiom_status(planetary_limits_are_absolute, holdable).
narrative_ontology:cs_axiom_grounding('2104707e-f0db-4a9e-bdbe-a0f1ac0446be', planetary_limits_are_absolute, empirically_contingent).
narrative_ontology:cs_axiom('2104707e-f0db-4a9e-bdbe-a0f1ac0446be', foundational, sufficiency_is_ethical_imperative).
narrative_ontology:cs_axiom_status(sufficiency_is_ethical_imperative, holdable).
narrative_ontology:cs_axiom_grounding('2104707e-f0db-4a9e-bdbe-a0f1ac0446be', sufficiency_is_ethical_imperative, deontological).
narrative_ontology:cs_reference_frame('2104707e-f0db-4a9e-bdbe-a0f1ac0446be', planetary_boundaries_framework).
narrative_ontology:cs_drift_state('2104707e-f0db-4a9e-bdbe-a0f1ac0446be', contemporary_economic_paradigm, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('2104707e-f0db-4a9e-bdbe-a0f1ac0446be', '').
narrative_ontology:cs_kernel_id(climate_response_obligation__degrowth_reading, climate_response_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_obligation__degrowth_reading, planetary_systems).
narrative_ontology:constraint_beneficiary(climate_response_obligation__degrowth_reading, future_generations).
narrative_ontology:constraint_beneficiary(climate_response_obligation__degrowth_reading, global_south_nations).
narrative_ontology:constraint_victim(climate_response_obligation__degrowth_reading, global_north_high_consumers).
narrative_ontology:constraint_victim(climate_response_obligation__degrowth_reading, fossil_capital_industries).
narrative_ontology:constraint_victim(climate_response_obligation__degrowth_reading, growth_oriented_economies).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The ultimate beneficiary of reduced material throughput, experiencing less extraction pressure and greater stability. Its 'voice' is expressed through scientific data on ecological limits.
narrative_ontology:constraint_stakeholder(climate_response_obligation__degrowth_reading, planetary_systems, beneficiary,
    powerless, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(climate_response_obligation__degrowth_reading, planetary_systems).

% Benefit from a habitable planet and more equitable resource distribution, avoiding the ecological collapse predicted under business-as-usual scenarios. Their interests are represented by intergenerational ethics.
narrative_ontology:constraint_stakeholder(climate_response_obligation__degrowth_reading, future_generations, beneficiary,
    powerless, civilizational, analytical, global).

% Bear the primary cost of lifestyle changes, reduced consumption, and shifts away from material-intensive activities. Their current consumption patterns are directly targeted for reduction.
narrative_ontology:constraint_stakeholder(climate_response_obligation__degrowth_reading, global_north_high_consumers, payer,
    powerful, biographical, constrained, global).

% Benefit from the Global North's degrowth, which creates ecological space for their own sustainable development and reduces historical ecological debt. Their development is constrained if the North does not reduce first.
narrative_ontology:constraint_stakeholder(climate_response_obligation__degrowth_reading, global_south_nations, beneficiary,
    organized, generational, constrained, global).

% Face existential threats as their business model (based on continuous extraction and growth) is directly contradicted by the degrowth imperative. Their capital accumulation becomes an extractive mechanism to be dismantled.
narrative_ontology:constraint_stakeholder(climate_response_obligation__degrowth_reading, fossil_capital_industries, payer,
    institutional, immediate, trapped, global).

% Must fundamentally restructure away from GDP growth as the primary metric and goal, facing significant economic and political challenges in the transition. Their current operating logic is suppressed.
narrative_ontology:constraint_stakeholder(climate_response_obligation__degrowth_reading, growth_oriented_economies, payer,
    institutional, biographical, constrained, global).

% Propose and champion the policies and philosophical shifts required for degrowth, actively working to implement the constraint. They articulate the scientific and ethical basis for the obligation.
narrative_ontology:constraint_stakeholder(climate_response_obligation__degrowth_reading, degrowth_advocates, agenda_setter,
    organized, generational, mobile, global).

% Are largely excluded from the core framing of this reading, as their models often assume continuous growth or rely solely on efficiency gains. They would argue for technological solutions within existing economic paradigms.
narrative_ontology:constraint_stakeholder(climate_response_obligation__degrowth_reading, mainstream_economists, excluded,
    institutional, biographical, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To coordinate global human economic activity to operate within the biophysical limits of the planet, preventing ecological collapse and ensuring long-term habitability for all species and future generations.
% TRANSFER_FUNCTION: Transfers material and energy resources from current high-consumption societies (especially the Global North) to planetary sinks and future generations, by reducing overall throughput and prioritizing sufficiency over efficiency.
% ABSENT_VOICES: Mainstream economists focused on GDP growth, industries reliant on continuous expansion, and individuals whose identities are tied to high consumption. They would argue for technological solutions, green growth, or continued economic expansion, but are structurally excluded from the degrowth framing.
% DISAPPEARANCE_RATIONALE: If the obligation to reduce material throughput vanished, current economic systems would continue their growth trajectory, leading to accelerated ecological collapse, resource depletion, and a fundamentally different, likely uninhabitable, future world. The global economy would continue to operate as if planetary limits were not binding.
% FOUNDING_PROBLEM: The ecological overshoot and climate crisis caused by continuous economic growth and material extraction exceeding planetary regenerative capacities, leading to biodiversity loss, climate change, and resource depletion.
% FOUNDING_PROBLEM_CORROBORATION: Climate scientists, ecologists, and intergovernmental bodies (e.g., IPCC, IPBES) provide extensive corroboration through empirical data on planetary boundaries, biodiversity loss, and climate change impacts. Indigenous knowledge systems also corroborate the need for living within ecological limits.
narrative_ontology:disappearance_verdict(climate_response_obligation__degrowth_reading, world_rearranges).
narrative_ontology:founding_problem_status(climate_response_obligation__degrowth_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_response_obligation__degrowth_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(climate_response_obligation__degrowth_reading, 'none', 1).
narrative_ontology:epsilon_provenance(climate_response_obligation__degrowth_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_response_obligation__degrowth_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_response_obligation__degrowth_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_response_obligation__degrowth_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.85) is high because it demands fundamental shifts away from continuous growth, impacting deeply entrenched economic models and consumption habits. Suppression (0.90) is also high, as it requires active policies to limit growth and consumption, suppressing market-driven alternatives and challenging dominant economic paradigms. The theater ratio (0.10) is low, indicating that the constraint is seen as a direct, functional imperative rather than a performative one. Resistance (0.95) is very high due to the challenge it poses to established power structures and individual lifestyles. Accessibility collapse (0.80) is high because the degrowth reading argues that alternatives (e.g., infinite growth, purely technological fixes) are fundamentally collapsed by biophysical planetary limits.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of degrowth advocates and those representing planetary systems/future generations, this is a necessary coordination mechanism to ensure survival. From the perspective of high-consumers and growth-oriented industries, it is an extractive and suppressive force that threatens their prosperity and way of life. The engine's computation of per-seat classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Planetary systems and future generations are full beneficiaries (d=0.0) as the constraint directly subsidizes their long-term viability. Global South nations are conditional beneficiaries, gaining ecological space if the Global North degrows first. Global North high-consumers, fossil capital industries, and growth-oriented economies are full targets (d=1.0) as the constraint directly extracts from their current operations and lifestyles. Degrowth advocates act as agenda-setters, pushing for the constraint's implementation. Mainstream economists are excluded, as their frameworks are incompatible with the core tenets of degrowth.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    degrowth_political_feasibility,
    'Is a degrowth transition politically and socially feasible without severe societal disruption or authoritarian enforcement, given current global political economy structures?',
    'Empirical observation of degrowth policy implementation in democratic contexts and analysis of social acceptance and political resistance.',
    'If infeasible without disruption, the effective suppression and resistance metrics may be underestimated, or the constraint may be reclassified as a Snare due to the necessity of coercion. If feasible, it strengthens the Tangled Rope classification by demonstrating a viable coordination path.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(degrowth_political_feasibility, empirical, 'Uncertainty regarding the political and social feasibility of a degrowth transition.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression primarily structural (inherent planetary limits) or policy-enforced (human-made laws and regulations)?',
    'Analysis of policy effectiveness in reducing throughput versus the observed biophysical limits being breached. If limits are breached despite policy, structural suppression is dominant.',
    'If primarily structural, the constraint''s ''naturalness'' is higher, potentially pushing it closer to a Mountain (though still a Tangled Rope due to human agency). If primarily policy-enforced, it reinforces the constructed nature and the role of human agency in its persistence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, conceptual, 'Structural vs. internalized suppression mechanism in the context of planetary boundaries.').

omega_variable(
    degrowth_vs_green_growth_framing,
    'Is the ''sufficiency over efficiency'' framing a distinct structural imperative, or can its goals be achieved through ''green growth'' strategies that prioritize efficiency and technological innovation?',
    'Empirical evidence on whether efficiency gains consistently lead to absolute decoupling of economic growth from material throughput at a global scale, or if rebound effects negate these gains.',
    'If green growth achieves the goals, the degrowth reading''s high extractiveness and suppression might be overstated, and its ''forecloses'' relation to mitigation_priority might be weakened. If not, the degrowth reading''s structural distinctiveness and necessity are reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(degrowth_vs_green_growth_framing, empirical, 'Conceptual distinction between degrowth and green growth approaches to climate action.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_obligation__degrowth_reading, 1972, 2022).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t1972, climate_response_obligation__degrowth_reading, theater_ratio, 1972, 0.15).
narrative_ontology:measurement(clim_tr_t1982, climate_response_obligation__degrowth_reading, theater_ratio, 1982, 0.14).
narrative_ontology:measurement(clim_tr_t1992, climate_response_obligation__degrowth_reading, theater_ratio, 1992, 0.13).
narrative_ontology:measurement(clim_tr_t2002, climate_response_obligation__degrowth_reading, theater_ratio, 2002, 0.12).
narrative_ontology:measurement(clim_tr_t2012, climate_response_obligation__degrowth_reading, theater_ratio, 2012, 0.11).
narrative_ontology:measurement(clim_tr_t2022, climate_response_obligation__degrowth_reading, theater_ratio, 2022, 0.1).

% Extraction over time
narrative_ontology:measurement(clim_be_t1972, climate_response_obligation__degrowth_reading, base_extractiveness, 1972, 0.6).
narrative_ontology:measurement(clim_be_t1982, climate_response_obligation__degrowth_reading, base_extractiveness, 1982, 0.65).
narrative_ontology:measurement(clim_be_t1992, climate_response_obligation__degrowth_reading, base_extractiveness, 1992, 0.7).
narrative_ontology:measurement(clim_be_t2002, climate_response_obligation__degrowth_reading, base_extractiveness, 2002, 0.75).
narrative_ontology:measurement(clim_be_t2012, climate_response_obligation__degrowth_reading, base_extractiveness, 2012, 0.8).
narrative_ontology:measurement(clim_be_t2022, climate_response_obligation__degrowth_reading, base_extractiveness, 2022, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t1972, climate_response_obligation__degrowth_reading, suppression_requirement, 1972, 0.7).
narrative_ontology:measurement(clim_su_t1982, climate_response_obligation__degrowth_reading, suppression_requirement, 1982, 0.75).
narrative_ontology:measurement(clim_su_t1992, climate_response_obligation__degrowth_reading, suppression_requirement, 1992, 0.8).
narrative_ontology:measurement(clim_su_t2002, climate_response_obligation__degrowth_reading, suppression_requirement, 2002, 0.85).
narrative_ontology:measurement(clim_su_t2012, climate_response_obligation__degrowth_reading, suppression_requirement, 2012, 0.88).
narrative_ontology:measurement(clim_su_t2022, climate_response_obligation__degrowth_reading, suppression_requirement, 2022, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_response_obligation__degrowth_reading, global_infrastructure).
narrative_ontology:affects_constraint(climate_response_obligation__degrowth_reading, climate_response_obligation__mitigation_priority).
narrative_ontology:affects_constraint(climate_response_obligation__degrowth_reading, climate_response_obligation__adaptation_priority).
narrative_ontology:affects_constraint(climate_response_obligation__degrowth_reading, global_carbon_markets).
narrative_ontology:affects_constraint(climate_response_obligation__degrowth_reading, fossil_fuel_subsidies).
narrative_ontology:affects_constraint(climate_response_obligation__degrowth_reading, international_trade_agreements).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the 'climate_response_obligation' kernel, each representing a distinct approach to addressing the climate crisis. This 'degrowth_reading' emphasizes fundamental systemic change and reduced material throughput, contrasting with 'mitigation_priority' (rapid decarbonization within growth) and 'adaptation_priority' (focus on resilience).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
