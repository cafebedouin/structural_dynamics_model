% ============================================================================
% CONSTRAINT STORY: climate_response_legitimacy__degrowth_transformation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_response_legitimacy__degrowth_transformation, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: climate_response_legitimacy__degrowth_transformation
 *   human_readable: Degrowth Transformation for Legitimate Climate Response
 *   domain: climate_policy/political_economy/intergenerational_ethics
 *
 * SUMMARY:
 *   This constraint represents the 'degrowth transformation' reading of what
 *   constitutes a legitimate climate response. It posits that addressing the
 *   climate crisis legitimately requires a fundamental dismantling of the
 *   growth imperative in wealthy nations through structural economic changes,
 *   rather than relying solely on technological mitigation or adaptation.
 *   This involves significant extraction from existing economic models and
 *   active enforcement to overcome resistance from entrenched interests. The
 *   high extractiveness and suppression reflect the radical nature of the
 *   proposed transformation and the immense political and economic forces it
 *   would need to overcome.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_legitimacy__degrowth_transformation, 0.85).
domain_priors:suppression_score(climate_response_legitimacy__degrowth_transformation, 0.9).
domain_priors:theater_ratio(climate_response_legitimacy__degrowth_transformation, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_legitimacy__degrowth_transformation, extractiveness, 0.85).
narrative_ontology:constraint_metric(climate_response_legitimacy__degrowth_transformation, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(climate_response_legitimacy__degrowth_transformation, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_response_legitimacy__degrowth_transformation, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(climate_response_legitimacy__degrowth_transformation, resistance, 0.95).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_legitimacy__degrowth_transformation, tangled_rope).
narrative_ontology:human_readable(climate_response_legitimacy__degrowth_transformation, "Degrowth Transformation for Legitimate Climate Response").
narrative_ontology:topic_domain(climate_response_legitimacy__degrowth_transformation, "climate_policy/political_economy/intergenerational_ethics").

domain_priors:requires_active_enforcement(climate_response_legitimacy__degrowth_transformation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_legitimacy__degrowth_transformation, '2a7d04dc-b0bb-4306-81e1-79bec29e94a5').
narrative_ontology:cs_kernel_codification('2a7d04dc-b0bb-4306-81e1-79bec29e94a5', implicit).
narrative_ontology:cs_authority_grounding('2a7d04dc-b0bb-4306-81e1-79bec29e94a5', distributed).
narrative_ontology:cs_reading_relation('2a7d04dc-b0bb-4306-81e1-79bec29e94a5', climate_response_legitimacy__mitigation_priority, forecloses).
narrative_ontology:cs_reading_relation('2a7d04dc-b0bb-4306-81e1-79bec29e94a5', climate_response_legitimacy__adaptation_priority, forecloses).
narrative_ontology:cs_axiom('2a7d04dc-b0bb-4306-81e1-79bec29e94a5', foundational, economic_growth_is_ecologically_unsustainable).
narrative_ontology:cs_axiom_status(economic_growth_is_ecologically_unsustainable, holdable).
narrative_ontology:cs_axiom_grounding('2a7d04dc-b0bb-4306-81e1-79bec29e94a5', economic_growth_is_ecologically_unsustainable, empirically_contingent).
narrative_ontology:cs_axiom('2a7d04dc-b0bb-4306-81e1-79bec29e94a5', foundational, equity_requires_redistribution_not_growth).
narrative_ontology:cs_axiom_status(equity_requires_redistribution_not_growth, holdable).
narrative_ontology:cs_axiom_grounding('2a7d04dc-b0bb-4306-81e1-79bec29e94a5', equity_requires_redistribution_not_growth, deontological).
narrative_ontology:cs_reference_frame('2a7d04dc-b0bb-4306-81e1-79bec29e94a5', ecological_sustainability_and_equity).
narrative_ontology:cs_drift_state('2a7d04dc-b0bb-4306-81e1-79bec29e94a5', contemporary_growth_paradigm, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('2a7d04dc-b0bb-4306-81e1-79bec29e94a5', '').
narrative_ontology:cs_kernel_id(climate_response_legitimacy__degrowth_transformation, climate_response_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_legitimacy__degrowth_transformation, future_generations).
narrative_ontology:constraint_beneficiary(climate_response_legitimacy__degrowth_transformation, vulnerable_global_south_communities).
narrative_ontology:constraint_beneficiary(climate_response_legitimacy__degrowth_transformation, citizens_in_wealthy_nations_via_wellbeing).
narrative_ontology:constraint_victim(climate_response_legitimacy__degrowth_transformation, current_generation_in_wealthy_nations_via_income_reduction).
narrative_ontology:constraint_victim(climate_response_legitimacy__degrowth_transformation, growth_dependent_industries).
narrative_ontology:constraint_victim(climate_response_legitimacy__degrowth_transformation, fossil_fuel_corporations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(climate_response_legitimacy__degrowth_transformation, citizens_in_wealthy_nations_via_wellbeing).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Propose and articulate the necessity of dismantling the growth imperative, advocating for policies like universal basic services, working time reduction, and democratic firm ownership. They face significant political and economic opposition.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__degrowth_transformation, degrowth_advocates_and_ecological_economists, agenda_setter,
    organized, generational, constrained, global).

% Benefit from reduced global warming, preserved ecological systems, and a future less dependent on unsustainable technological fixes. They bear no direct costs but are entirely dependent on the actions of current generations.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__degrowth_transformation, future_generations, beneficiary,
    powerless, civilizational, trapped, universal).

% Benefit from a more equitable distribution of resources and a reduction in climate impacts that disproportionately affect them, without being forced into growth-dependent development models. They currently bear the brunt of climate change.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__degrowth_transformation, vulnerable_global_south_communities, beneficiary,
    powerless, generational, trapped, global).

% Potentially benefit from improved quality of life through universal basic services, reduced working hours, and greater democratic control over their economies, leading to less stress and more leisure. However, they also face income reduction and consumption limits.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__degrowth_transformation, citizens_in_wealthy_nations_via_wellbeing, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(climate_response_legitimacy__degrowth_transformation, citizens_in_wealthy_nations_via_wellbeing, payer).

% Bear the direct costs of structural economic transformation, including potential income reduction, changes in consumption patterns, and shifts in employment. Their resistance is a major political feasibility barrier.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__degrowth_transformation, current_generation_in_wealthy_nations_via_income_reduction, payer,
    powerful, biographical, constrained, national).

% Face existential threats from policies aimed at dismantling the growth imperative. Their business models are predicated on continuous expansion, making them strong opponents of degrowth policies.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__degrowth_transformation, growth_dependent_industries, payer,
    institutional, biographical, constrained, global).

% Are direct targets of any climate response that seeks to reduce energy consumption and transition away from fossil fuels. Their economic and political power makes them formidable opponents.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__degrowth_transformation, fossil_fuel_corporations, payer,
    institutional, biographical, constrained, global).

% Are largely excluded from the degrowth discourse as their foundational assumptions (e.g., GDP growth as a primary policy goal) are directly challenged. They would argue for technological solutions within a growth paradigm.
narrative_ontology:constraint_stakeholder(climate_response_legitimacy__degrowth_transformation, mainstream_economists_and_growth_oriented_politicians, excluded,
    institutional, biographical, identity_locked, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Realigns global economic activity and resource use with planetary ecological boundaries, coordinating human societies towards ecological sustainability and social equity rather than continuous material growth.
% TRANSFER_FUNCTION: Transfers wealth, resources, and decision-making power from growth-dependent sectors and individuals (primarily in wealthy nations) to public services, ecological restoration, and democratically controlled enterprises. It also transfers ecological burden from future generations to the current generation.
% ABSENT_VOICES: Mainstream economists, growth-oriented politicians, and industries heavily reliant on continuous growth are largely absent from the core degrowth conversation. They would argue that degrowth is economically unfeasible or undesirable, advocating for technological mitigation or adaptation within a growth framework.
% DISAPPEARANCE_RATIONALE: If the imperative for degrowth transformation were to vanish after implementation, the underlying drivers of ecological overshoot and inequality would likely reassert themselves, leading to a reversion to growth-dependent economic models and continued climate degradation. The global economy would reorganize around renewed growth targets.
% FOUNDING_PROBLEM: The climate crisis, ecological overshoot, and persistent social inequalities are fundamentally driven by the imperative for continuous economic growth, particularly in wealthy nations, leading to an unsustainable and unjust global system.
% FOUNDING_PROBLEM_CORROBORATION: IPCC reports on planetary boundaries, ecological economics research, and social justice movements consistently corroborate the existence and severity of the founding problem, linking it to the growth imperative. This corroboration comes from outside the direct beneficiaries of the proposed transformation.
narrative_ontology:disappearance_verdict(climate_response_legitimacy__degrowth_transformation, world_rearranges).
narrative_ontology:founding_problem_status(climate_response_legitimacy__degrowth_transformation, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_response_legitimacy__degrowth_transformation, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(climate_response_legitimacy__degrowth_transformation, 'none', 1).
narrative_ontology:epsilon_provenance(climate_response_legitimacy__degrowth_transformation, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_response_legitimacy__degrowth_transformation_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_response_legitimacy__degrowth_transformation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_response_legitimacy__degrowth_transformation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is high (0.85) because the proposed transformation demands a significant re-allocation of resources and a reduction in material consumption and income for many in wealthy nations. Suppression is very high (0.90) due to the anticipated resistance from powerful economic and political actors whose interests are tied to the growth paradigm. The theater ratio is low (0.10) because this is a genuine, radical proposal for systemic change, not a performative gesture. Accessibility collapse is moderate (0.70) as alternative climate responses (mitigation, adaptation) are seen as insufficient or illegitimate by this reading, but they still exist as policy options. Resistance is extremely high (0.95) given the challenge to fundamental economic and social structures.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of degrowth advocates, this transformation is a necessary and legitimate response to an existential crisis. From the perspective of growth-dependent industries and mainstream economists, it is an illegitimate, economically destructive, and politically unfeasible proposal. The engine's classification will highlight this divergence by showing high extraction from the current system, which is precisely the point of this reading.
 *
 * DIRECTIONALITY LOGIC:
 *   Future generations and vulnerable global south communities are clear beneficiaries (d=0.0) as they gain from a stable climate and more equitable resource distribution without bearing direct costs. Degrowth advocates are agenda-setters, pushing for the transformation. The current generation in wealthy nations, growth-dependent industries, and fossil fuel corporations are primary targets/payers (d=1.0), as they bear the costs of income reduction, consumption limits, and business model disruption. Citizens in wealthy nations are dual-positioned, potentially benefiting from well-being gains but paying through income reduction. Mainstream economists and growth-oriented politicians are excluded, as their paradigms are directly challenged.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    political_feasibility_of_degrowth,
    'Is the proposed degrowth transformation politically feasible within existing democratic or governance structures, given the high resistance from entrenched interests?',
    'Empirical observation of successful implementation of core degrowth policies (e.g., universal basic services, working time reduction) at national or regional scales, or the emergence of new political structures capable of enacting such changes.',
    'If politically infeasible, the constraint remains a theoretical ideal with no real-world impact, or it would require authoritarian enforcement, shifting its classification towards a Snare. If feasible, it could transition towards a Scaffold (if temporary) or a more stable Tangled Rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(political_feasibility_of_degrowth, empirical, 'Assesses the political viability of implementing degrowth policies against strong opposition.').

omega_variable(
    wellbeing_vs_income_tradeoff,
    'To what extent would the proposed structural economic transformation genuinely improve well-being for citizens in wealthy nations, offsetting the costs of income reduction and consumption limits?',
    'Longitudinal studies and social indicators tracking well-being, health, and social cohesion in regions implementing degrowth-aligned policies, compared to traditional growth-oriented economies.',
    'If well-being gains are substantial and widely distributed, it strengthens the ''beneficiary'' role for current citizens, potentially lowering the effective extraction from their seat. If not, it reinforces their ''payer'' role, increasing perceived extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(wellbeing_vs_income_tradeoff, empirical, 'Evaluates the net impact of degrowth policies on human well-being beyond economic metrics.').

omega_variable(
    sufficiency_of_decoupling_vs_degrowth,
    'Is technological decoupling of economic growth from emissions and resource use truly insufficient to address the climate crisis, as this reading claims, or could it provide a legitimate alternative?',
    'Empirical data on the absolute reduction of emissions and resource use achieved through decoupling efforts over time, compared against planetary boundaries and climate targets. This would involve assessing the ''mitigation_priority'' reading''s effectiveness.',
    'If decoupling proves sufficient, it would undermine a foundational axiom of the degrowth reading, potentially foreclosing it conceptually or shifting its classification to a less legitimate form (e.g., Snare if it persists through coercion despite viable alternatives).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sufficiency_of_decoupling_vs_degrowth, empirical, 'Contests the core premise that growth cannot be sufficiently decoupled from ecological impact.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_legitimacy__degrowth_transformation, 2020, 2070).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t2020, climate_response_legitimacy__degrowth_transformation, theater_ratio, 2020, 0.15).
narrative_ontology:measurement(clim_tr_t2030, climate_response_legitimacy__degrowth_transformation, theater_ratio, 2030, 0.12).
narrative_ontology:measurement(clim_tr_t2040, climate_response_legitimacy__degrowth_transformation, theater_ratio, 2040, 0.1).
narrative_ontology:measurement(clim_tr_t2050, climate_response_legitimacy__degrowth_transformation, theater_ratio, 2050, 0.08).
narrative_ontology:measurement(clim_tr_t2060, climate_response_legitimacy__degrowth_transformation, theater_ratio, 2060, 0.09).
narrative_ontology:measurement(clim_tr_t2070, climate_response_legitimacy__degrowth_transformation, theater_ratio, 2070, 0.1).

% Extraction over time
narrative_ontology:measurement(clim_be_t2020, climate_response_legitimacy__degrowth_transformation, base_extractiveness, 2020, 0.75).
narrative_ontology:measurement(clim_be_t2030, climate_response_legitimacy__degrowth_transformation, base_extractiveness, 2030, 0.8).
narrative_ontology:measurement(clim_be_t2040, climate_response_legitimacy__degrowth_transformation, base_extractiveness, 2040, 0.85).
narrative_ontology:measurement(clim_be_t2050, climate_response_legitimacy__degrowth_transformation, base_extractiveness, 2050, 0.88).
narrative_ontology:measurement(clim_be_t2060, climate_response_legitimacy__degrowth_transformation, base_extractiveness, 2060, 0.9).
narrative_ontology:measurement(clim_be_t2070, climate_response_legitimacy__degrowth_transformation, base_extractiveness, 2070, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t2020, climate_response_legitimacy__degrowth_transformation, suppression_requirement, 2020, 0.8).
narrative_ontology:measurement(clim_su_t2030, climate_response_legitimacy__degrowth_transformation, suppression_requirement, 2030, 0.85).
narrative_ontology:measurement(clim_su_t2040, climate_response_legitimacy__degrowth_transformation, suppression_requirement, 2040, 0.9).
narrative_ontology:measurement(clim_su_t2050, climate_response_legitimacy__degrowth_transformation, suppression_requirement, 2050, 0.92).
narrative_ontology:measurement(clim_su_t2060, climate_response_legitimacy__degrowth_transformation, suppression_requirement, 2060, 0.9).
narrative_ontology:measurement(clim_su_t2070, climate_response_legitimacy__degrowth_transformation, suppression_requirement, 2070, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_response_legitimacy__degrowth_transformation, resource_allocation).
narrative_ontology:affects_constraint(climate_response_legitimacy__degrowth_transformation, climate_response_legitimacy__mitigation_priority).
narrative_ontology:affects_constraint(climate_response_legitimacy__degrowth_transformation, climate_response_legitimacy__adaptation_priority).

% DUAL FORMULATION NOTE:
% This constraint is one reading ('degrowth_transformation') of the 'climate_response_legitimacy' kernel. Its ε value differs significantly from sibling readings ('mitigation_priority', 'adaptation_priority') due to its fundamental challenge to the growth imperative and its proposed structural economic transformation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
