% ============================================================================
% CONSTRAINT STORY: climate_response_imperative__degrowth_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_response_imperative__degrowth_reading, []).

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
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: climate_response_imperative__degrowth_reading
 *   human_readable: Climate Response Imperative: Degrowth Reading
 *   domain: climate_policy/political_economy/intergenerational_justice
 *
 * SUMMARY:
 *   This constraint represents the 'degrowth' reading of the climate response
 *   imperative, asserting that structural economic transformation in the
 *   Global North—including reduced consumption, redistribution, and
 *   post-growth institutions—is essential for both climate change mitigation
 *   and adaptation. It posits that current growth-oriented models are
 *   incompatible with ecological limits and intergenerational equity. This
 *   reading places present-day Global North populations in a victim role due
 *   to required consumption reductions, while future generations and Global
 *   South populations are primary beneficiaries. It explicitly rejects
 *   reliance on unproven carbon dioxide removal (CDR) technologies as a
 *   primary solution.
 *
 * KEY AGENTS:
 *   - global_north_consumers: Primary target (powerful/constrained) — bears reduced consumption
 *   - future_generations: Primary beneficiary (powerless/trapped) — benefits from ecological stability
 *   - global_south_populations: Primary beneficiary (organized/constrained) — benefits from reduced climate impacts and resource redistribution
 *   - fossil_fuel_industries: Primary target (institutional/constrained) — bears economic contraction and stranded assets
 *   - growth_dependent_economies: Primary target (institutional/constrained) — bears systemic transformation costs
 *   - degrowth_advocates: Agenda setter (organized/mobile) — promotes and designs degrowth policies
 *   - mainstream_economists: Excluded (institutional/analytical) — would object to post-growth models
 *   - climate_scientists: Observer (analytical/analytical) — provide empirical grounding for ecological limits
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_imperative__degrowth_reading, 0.65).
domain_priors:suppression_score(climate_response_imperative__degrowth_reading, 0.7).
domain_priors:theater_ratio(climate_response_imperative__degrowth_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_imperative__degrowth_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(climate_response_imperative__degrowth_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(climate_response_imperative__degrowth_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_response_imperative__degrowth_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(climate_response_imperative__degrowth_reading, resistance, 0.85).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_imperative__degrowth_reading, tangled_rope).
narrative_ontology:human_readable(climate_response_imperative__degrowth_reading, "Climate Response Imperative: Degrowth Reading").
narrative_ontology:topic_domain(climate_response_imperative__degrowth_reading, "climate_policy/political_economy/intergenerational_justice").

domain_priors:requires_active_enforcement(climate_response_imperative__degrowth_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_imperative__degrowth_reading, 'b5e613dc-779b-49f8-9b85-ca6352eebbc6').
narrative_ontology:cs_kernel_codification('b5e613dc-779b-49f8-9b85-ca6352eebbc6', distributed).
narrative_ontology:cs_authority_grounding('b5e613dc-779b-49f8-9b85-ca6352eebbc6', diffuse_epistemic).
narrative_ontology:cs_reading_relation('b5e613dc-779b-49f8-9b85-ca6352eebbc6', climate_response_imperative__mitigation_priority_reading, influences).
narrative_ontology:cs_reading_relation('b5e613dc-779b-49f8-9b85-ca6352eebbc6', climate_response_imperative__adaptation_priority_reading, influences).
narrative_ontology:cs_axiom('b5e613dc-779b-49f8-9b85-ca6352eebbc6', foundational, infinite_growth_on_finite_planet_impossible).
narrative_ontology:cs_axiom_status(infinite_growth_on_finite_planet_impossible, holdable).
narrative_ontology:cs_axiom_grounding('b5e613dc-779b-49f8-9b85-ca6352eebbc6', infinite_growth_on_finite_planet_impossible, empirically_contingent).
narrative_ontology:cs_axiom('b5e613dc-779b-49f8-9b85-ca6352eebbc6', foundational, ecological_justice_requires_degrowth).
narrative_ontology:cs_axiom_status(ecological_justice_requires_degrowth, holdable).
narrative_ontology:cs_axiom_grounding('b5e613dc-779b-49f8-9b85-ca6352eebbc6', ecological_justice_requires_degrowth, deontological).
narrative_ontology:cs_reference_frame('b5e613dc-779b-49f8-9b85-ca6352eebbc6', planetary_boundaries_framework).
narrative_ontology:cs_drift_state('b5e613dc-779b-49f8-9b85-ca6352eebbc6', contemporary, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('b5e613dc-779b-49f8-9b85-ca6352eebbc6', '').
narrative_ontology:cs_kernel_id(climate_response_imperative__degrowth_reading, climate_response_imperative).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_imperative__degrowth_reading, future_generations).
narrative_ontology:constraint_beneficiary(climate_response_imperative__degrowth_reading, global_south_populations).
narrative_ontology:constraint_beneficiary(climate_response_imperative__degrowth_reading, ecosystems).
narrative_ontology:constraint_victim(climate_response_imperative__degrowth_reading, global_north_consumers).
narrative_ontology:constraint_victim(climate_response_imperative__degrowth_reading, fossil_fuel_industries).
narrative_ontology:constraint_victim(climate_response_imperative__degrowth_reading, growth_dependent_economies).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Required to reduce consumption and accept changes to lifestyle and working time, directly bearing the costs of economic transformation. Their political power makes direct suppression difficult, but structural changes would limit their options.
narrative_ontology:constraint_stakeholder(climate_response_imperative__degrowth_reading, global_north_consumers, payer,
    powerful, biographical, constrained, global).

% Benefit from a stable climate, preserved ecosystems, and a more equitable distribution of resources. They have no voice in current policy decisions and are entirely dependent on present-day actions.
narrative_ontology:constraint_stakeholder(climate_response_imperative__degrowth_reading, future_generations, beneficiary,
    powerless, generational, trapped, universal).

% Benefit from reduced climate impacts, climate justice, and potential resource redistribution from the Global North. They bear the brunt of climate change impacts but have limited power to enforce the degrowth agenda on the Global North.
narrative_ontology:constraint_stakeholder(climate_response_imperative__degrowth_reading, global_south_populations, beneficiary,
    organized, generational, constrained, global).

% Face existential threats from reduced energy demand, stranded assets, and policy-driven phase-outs. They actively resist degrowth policies through lobbying and political influence.
narrative_ontology:constraint_stakeholder(climate_response_imperative__degrowth_reading, fossil_fuel_industries, payer,
    institutional, biographical, constrained, global).

% Must undergo fundamental restructuring away from GDP growth as a primary metric and goal, impacting employment, investment, and social welfare systems built around growth. This is a systemic cost.
narrative_ontology:constraint_stakeholder(climate_response_imperative__degrowth_reading, growth_dependent_economies, payer,
    institutional, generational, constrained, global).

% Propose, research, and advocate for degrowth policies and institutions. They actively work to shift public discourse and policy frameworks towards post-growth models.
narrative_ontology:constraint_stakeholder(climate_response_imperative__degrowth_reading, degrowth_advocates, agenda_setter,
    organized, generational, mobile, global).

% Operate within a paradigm that assumes continuous economic growth is necessary and desirable. They would largely reject the foundational premises of degrowth and are often excluded from policy discussions framed by degrowth advocates.
narrative_ontology:constraint_stakeholder(climate_response_imperative__degrowth_reading, mainstream_economists, excluded,
    institutional, biographical, analytical, global).

% Provide the scientific evidence for ecological limits and climate change impacts, which underpins the degrowth imperative. They observe the policy debate but do not directly set or pay for the constraint.
narrative_ontology:constraint_stakeholder(climate_response_imperative__degrowth_reading, climate_scientists, observer,
    analytical, civilizational, analytical, universal).

% Benefit from reduced resource extraction, pollution, and habitat destruction. They are non-agent entities that are direct beneficiaries of the degrowth agenda.
narrative_ontology:constraint_stakeholder(climate_response_imperative__degrowth_reading, ecosystems, beneficiary,
    powerless, civilizational, trapped, universal).
narrative_ontology:stakeholder_non_agent(climate_response_imperative__degrowth_reading, ecosystems).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_response_imperative__degrowth_reading, future_generations).
narrative_ontology:fixing_cost_class(climate_response_imperative__degrowth_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To coordinate a global, equitable response to climate change by aligning human economic activity with planetary ecological boundaries, ensuring long-term well-being for all.
% TRANSFER_FUNCTION: Transfers ecological space, resource availability, and a stable climate from present-day Global North over-consumers and extractive industries to future generations and Global South populations. It also transfers economic activity and investment from growth-oriented sectors to regenerative and care sectors.
% ABSENT_VOICES: Mainstream economists and proponents of 'green growth' or techno-optimistic solutions are largely excluded from the core degrowth discourse. They would argue that degrowth is unnecessary, economically damaging, or politically unfeasible, advocating for alternative pathways to climate stability.
% DISAPPEARANCE_RATIONALE: If the degrowth imperative (as a policy and social movement) vanished, the world would likely revert to business-as-usual growth patterns, accelerating climate change, exacerbating ecological crises, and deepening intergenerational and global inequalities. The trajectory of human civilization would fundamentally shift.
% FOUNDING_PROBLEM: The foundational problem is the inherent conflict between infinite economic growth on a finite planet, leading to ecological overshoot, climate breakdown, and unsustainable resource depletion, disproportionately impacting vulnerable populations and future generations.
% FOUNDING_PROBLEM_CORROBORATION: Climate scientists, ecologists, and intergovernmental bodies (e.g., IPCC reports) corroborate the live status of the ecological overshoot and climate crisis, providing empirical evidence that growth-as-usual is unsustainable. Indigenous communities and Global South activists corroborate the disproportionate impacts and the need for systemic change, from outside the direct beneficiaries of the degrowth agenda.
narrative_ontology:disappearance_verdict(climate_response_imperative__degrowth_reading, world_rearranges).
narrative_ontology:founding_problem_status(climate_response_imperative__degrowth_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_response_imperative__degrowth_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(climate_response_imperative__degrowth_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_response_imperative__degrowth_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_response_imperative__degrowth_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_response_imperative__degrowth_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Tangled Rope because it genuinely aims to coordinate a global response to climate change (benefiting future generations and the Global South) but requires significant, actively enforced extraction from Global North populations and growth-dependent industries. Extractiveness is high (0.65) due to the scale of economic restructuring and consumption reduction demanded. Suppression (0.70) is also high, reflecting the political and social resistance expected from such a radical shift, requiring active enforcement to overcome. The theater_ratio is low (0.20) as the degrowth agenda is direct and explicit about its goals, with little performative cover for other functions. Resistance is very high (0.85) due to the direct challenge to established economic paradigms and lifestyles.
 *
 * PERSPECTIVAL GAP:
 *   Global North consumers and growth-dependent economies would experience this as a highly extractive and suppressive Snare, as it directly targets their current economic models and consumption patterns. Future generations and Global South populations, however, would perceive it as a necessary Rope or even a Mountain, as it secures their long-term well-being and ecological stability. Degrowth advocates view it as a necessary, albeit challenging, coordination mechanism.
 *
 * DIRECTIONALITY LOGIC:
 *   Global North consumers and growth-dependent economies are clear targets (high d) due to mandated consumption reduction and economic restructuring. Fossil fuel industries are also targets (high d) as their business model is directly undermined. Future generations and Global South populations are beneficiaries (low d) as they gain from a stable climate and resource redistribution. Degrowth advocates act as agenda-setters, pushing for the constraint's implementation (low d).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is not subject to mandatrophy in the traditional sense, as its mandate (climate response) is perceived as increasingly urgent and 'live.' The challenge is not obsolescence but rather the political will and capacity to implement such a transformative agenda against entrenched interests. The classification as Tangled Rope prevents mislabeling it as a pure Snare (ignoring the coordination for future generations) or a pure Rope (ignoring the extraction from current populations).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    degrowth_feasibility,
    'Is a degrowth transition politically and economically feasible within democratic frameworks, or does it require authoritarian enforcement?',
    'Empirical observation of degrowth policy implementation in high-income nations and analysis of public acceptance and economic stability.',
    'If feasible, the suppression metric might be lower than currently estimated, reflecting voluntary participation. If not, the true suppression required would be higher, pushing the constraint closer to a Snare for Global North populations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(degrowth_feasibility, empirical, 'Feasibility of degrowth without authoritarian measures.').

omega_variable(
    degrowth_vs_other_readings,
    'This constraint is the ''degrowth_reading'' of the ''climate_response_imperative'' kernel. How would the classification change if a ''mitigation_priority_reading'' or ''adaptation_priority_reading'' were adopted?',
    'Analysis of policy outcomes under alternative framings: ''mitigation_priority'' would likely shift extraction towards carbon-intensive industries and rely more on technological solutions, potentially reducing extraction from general consumption. ''adaptation_priority'' would shift resources to resilience-building, potentially increasing extraction from all populations for infrastructure, but with different beneficiaries.',
    'The ''mitigation_priority_reading'' would likely be a Tangled Rope with different victims (carbon-intensive industries) and potentially lower overall extraction from general populations. The ''adaptation_priority_reading'' might be a Scaffold or Rope, focusing on collective action for resilience, with diffuse beneficiaries and victims.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(degrowth_vs_other_readings, conceptual, 'Impact of alternative readings of the climate response imperative.').

omega_variable(
    cdr_reliance_ambiguity,
    'Does the degrowth reading truly eliminate reliance on unproven Carbon Dioxide Removal (CDR) technologies, or does it merely reduce the scale of reliance?',
    'Detailed energy and material flow analysis of degrowth scenarios, including residual emissions and the need for negative emissions technologies.',
    'If some CDR reliance remains, the degrowth reading''s claim of ''eliminating reliance'' is partially theatrical, potentially increasing its theater_ratio and reducing its perceived efficacy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cdr_reliance_ambiguity, empirical, 'Degree to which degrowth eliminates reliance on unproven CDR technologies.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_imperative__degrowth_reading, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_response_imperative__degrowth_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(clim_tr_t5, climate_response_imperative__degrowth_reading, theater_ratio, 5, 0.15).
narrative_ontology:measurement(clim_tr_t10, climate_response_imperative__degrowth_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement(clim_tr_t15, climate_response_imperative__degrowth_reading, theater_ratio, 15, 0.2).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_response_imperative__degrowth_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(clim_be_t5, climate_response_imperative__degrowth_reading, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(clim_be_t10, climate_response_imperative__degrowth_reading, base_extractiveness, 10, 0.6).
narrative_ontology:measurement(clim_be_t15, climate_response_imperative__degrowth_reading, base_extractiveness, 15, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_response_imperative__degrowth_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(clim_su_t5, climate_response_imperative__degrowth_reading, suppression_requirement, 5, 0.6).
narrative_ontology:measurement(clim_su_t10, climate_response_imperative__degrowth_reading, suppression_requirement, 10, 0.65).
narrative_ontology:measurement(clim_su_t15, climate_response_imperative__degrowth_reading, suppression_requirement, 15, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_response_imperative__degrowth_reading, resource_allocation).
narrative_ontology:affects_constraint(climate_response_imperative__degrowth_reading, climate_response_imperative__mitigation_priority_reading).
narrative_ontology:affects_constraint(climate_response_imperative__degrowth_reading, climate_response_imperative__adaptation_priority_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'climate_response_imperative' kernel, focusing on degrowth. It is linked to sibling readings that prioritize mitigation via technology or adaptation via resilience, as these represent competing approaches to the same overarching problem.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
