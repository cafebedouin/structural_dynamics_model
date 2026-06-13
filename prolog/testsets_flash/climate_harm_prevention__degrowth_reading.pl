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
 *   constraint_id: climate_harm_prevention__degrowth_reading
 *   human_readable: Degrowth Imperative for Climate Harm Prevention
 *   domain: climate_policy/political_economy/intergenerational_ethics
 *
 * SUMMARY:
 *   This constraint represents the 'degrowth' reading of the broader
 *   'climate_harm_prevention' kernel. It posits that a legitimate and
 *   effective response to climate change necessitates planned economic
 *   contraction in the Global North, arguing that mitigation efforts within a
 *   growth-oriented framework are physically and politically impossible. This
 *   reading prioritizes the well-being of Global South nations and future
 *   generations, placing the burden of contraction on current Global North
 *   consumption and extractive industries. It is a highly contested
 *   perspective, demanding significant structural shifts.
 *
 * KEY AGENTS:
 *   - global_south_nations: Primary beneficiary (institutional/generational) — avoids worst climate impacts
 *   - future_generations: Primary beneficiary (analytical/civilizational) — inherits a livable planet
 *   - global_north_consumers: Primary victim (powerful/biographical) — bears costs of reduced consumption
 *   - extractive_industries: Primary victim (institutional/biographical) — faces existential threat to business model
 *   - growth_oriented_economies: Primary victim (institutional/generational) — fundamental paradigm challenged
 *   - degrowth_advocates: Agenda setter (organized/generational) — promotes and articulates the necessity of degrowth
 *   - mitigation_priority_advocates: Excluded (organized/generational) — argues for technological solutions within growth
 *   - adaptation_priority_advocates: Excluded (organized/generational) — argues for resilience building over deep mitigation
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
narrative_ontology:constraint_metric(climate_harm_prevention__degrowth_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(climate_harm_prevention__degrowth_reading, resistance, 0.9).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_harm_prevention__degrowth_reading, snare).
narrative_ontology:human_readable(climate_harm_prevention__degrowth_reading, "Degrowth Imperative for Climate Harm Prevention").
narrative_ontology:topic_domain(climate_harm_prevention__degrowth_reading, "climate_policy/political_economy/intergenerational_ethics").

domain_priors:requires_active_enforcement(climate_harm_prevention__degrowth_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_harm_prevention__degrowth_reading, 'c00d527d-d217-42e9-9c49-388f2a5206a4').
narrative_ontology:cs_kernel_codification('c00d527d-d217-42e9-9c49-388f2a5206a4', distributed).
narrative_ontology:cs_authority_grounding('c00d527d-d217-42e9-9c49-388f2a5206a4', diffuse_epistemic).
narrative_ontology:cs_reading_relation('c00d527d-d217-42e9-9c49-388f2a5206a4', climate_harm_prevention__mitigation_priority, forecloses).
narrative_ontology:cs_reading_relation('c00d527d-d217-42e9-9c49-388f2a5206a4', climate_harm_prevention__adaptation_priority, influences).
narrative_ontology:cs_axiom('c00d527d-d217-42e9-9c49-388f2a5206a4', foundational, absolute_decoupling_impossible).
narrative_ontology:cs_axiom_status(absolute_decoupling_impossible, holdable).
narrative_ontology:cs_axiom_grounding('c00d527d-d217-42e9-9c49-388f2a5206a4', absolute_decoupling_impossible, empirically_contingent).
narrative_ontology:cs_axiom('c00d527d-d217-42e9-9c49-388f2a5206a4', foundational, planetary_boundaries_are_fixed).
narrative_ontology:cs_axiom_status(planetary_boundaries_are_fixed, holdable).
narrative_ontology:cs_axiom_grounding('c00d527d-d217-42e9-9c49-388f2a5206a4', planetary_boundaries_are_fixed, empirically_contingent).
narrative_ontology:cs_reference_frame('c00d527d-d217-42e9-9c49-388f2a5206a4', ecological_limits_paradigm).
narrative_ontology:cs_drift_state('c00d527d-d217-42e9-9c49-388f2a5206a4', contemporary_growth_imperative, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('c00d527d-d217-42e9-9c49-388f2a5206a4', '').
narrative_ontology:cs_kernel_id(climate_harm_prevention__degrowth_reading, climate_harm_prevention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_harm_prevention__degrowth_reading, global_south_nations).
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

% These nations are disproportionately affected by climate change despite historically low emissions. This reading positions them as primary beneficiaries, as planned degrowth in the Global North would reduce climate impacts and potentially free up ecological space for their development. Their 'exit' from climate harm is constrained by global emissions.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__degrowth_reading, global_south_nations, beneficiary,
    institutional, generational, trapped, global).

% These are the ultimate beneficiaries of effective climate action, inheriting a more stable and livable planet. Their 'situation' is entirely dependent on the actions of present generations, making them a 'silent' beneficiary whose interests are represented by advocates.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__degrowth_reading, future_generations, beneficiary,
    analytical, civilizational, analytical, universal).

% These individuals would bear the direct costs of planned economic contraction through reduced consumption, changes in lifestyle, and potentially lower material living standards. Their 'exit' from this constraint is constrained by national policies and social norms.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__degrowth_reading, global_north_consumers, payer,
    powerful, biographical, constrained, global).

% Industries reliant on fossil fuels, mining, and other high-impact resource extraction face an existential threat under a degrowth paradigm. Their business models are directly targeted, and their 'exit' is effectively 'trapped' as the constraint seeks to eliminate their core activity.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__degrowth_reading, extractive_industries, payer,
    institutional, biographical, trapped, global).

% National and international economic systems built on the imperative of continuous growth would undergo fundamental transformation. This represents a direct challenge to their operating logic, forcing a re-evaluation of economic goals and metrics. Their 'exit' is constrained by global economic interdependence.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__degrowth_reading, growth_oriented_economies, payer,
    institutional, generational, constrained, global).

% These are the intellectual and political proponents of the degrowth paradigm. They actively articulate the necessity of economic contraction and propose policy frameworks for its implementation. Their 'power' lies in organizing and influencing public discourse and policy debates.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__degrowth_reading, degrowth_advocates, agenda_setter,
    organized, generational, mobile, global).

% These advocates believe climate harm can be prevented through technological innovation and emissions reduction within a framework of continued economic growth. From the degrowth reading's perspective, their approach is insufficient and fundamentally flawed, thus they are 'excluded' from the core solution space.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__degrowth_reading, mitigation_priority_advocates, excluded,
    organized, generational, mobile, global).

% These advocates prioritize building resilience to unavoidable climate impacts, often viewing deep mitigation as politically or economically infeasible. The degrowth reading considers their focus on adaptation as a distraction from the root cause and a tacit acceptance of higher warming trajectories, thus 'excluding' their approach as a primary solution.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__degrowth_reading, adaptation_priority_advocates, excluded,
    organized, generational, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_harm_prevention__degrowth_reading, global_south_nations).
narrative_ontology:fixing_cost_class(climate_harm_prevention__degrowth_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a global re-prioritization of ecological limits over economic growth, aiming to align human activity with planetary boundaries to prevent catastrophic climate harm.
% TRANSFER_FUNCTION: Transfers ecological space, resource availability, and a stable climate from Global North consumption and growth-oriented economies to Global South nations and future generations.
% ABSENT_VOICES: Advocates for 'green growth' and 'technological mitigation' are absent from the core framing of this constraint, as their solutions are deemed inadequate or impossible. They would argue for alternative paths that do not require economic contraction.
% DISAPPEARANCE_RATIONALE: If the degrowth imperative vanished, the world would continue on its current growth trajectory, leading to accelerated climate harm, increased resource depletion, and exacerbated inequalities. The fundamental challenge to the growth paradigm would disappear, and other climate response strategies would likely remain within the existing economic framework.
% FOUNDING_PROBLEM: The founding problem is the escalating climate crisis, driven by unsustainable economic growth and resource consumption, particularly in the Global North, leading to disproportionate harm to vulnerable populations and future generations.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is widely attested by the scientific community (IPCC reports), international bodies (UN), and numerous civil society organizations. While the proposed solution (degrowth) is contested, the existence and severity of the climate crisis are corroborated by sources outside the degrowth advocacy movement itself.
narrative_ontology:disappearance_verdict(climate_harm_prevention__degrowth_reading, world_rearranges).
narrative_ontology:founding_problem_status(climate_harm_prevention__degrowth_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_harm_prevention__degrowth_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(climate_harm_prevention__degrowth_reading, 'none', 1).

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
 *   The extractiveness (0.85) is high because this reading demands a fundamental restructuring of economic activity, directly extracting from current consumption patterns and established industries in the Global North. Suppression (0.70) is also high, reflecting the immense political and social pressure required to enforce such a paradigm shift against entrenched interests and cultural norms. The resistance (0.90) is extreme, as it challenges the foundational logic of modern economies. Accessibility collapse (0.80) is high because it argues that alternatives (like green growth) are physically or politically impossible, thus collapsing the perceived viability of other paths. Theater ratio (0.10) is low because this reading is direct and uncompromising; it does not rely on performative gestures but on a clear, if radical, structural demand.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of Global South nations and future generations, this constraint is a necessary (though perhaps difficult) Rope or even a Mountain of physical reality, ensuring their survival and well-being. For Global North consumers and growth-oriented economies, it is a Snare, demanding sacrifices that fundamentally alter their way of life and economic models. Degrowth advocates see it as a moral and ecological imperative, while those advocating for mitigation or adaptation within growth frameworks view it as an unfeasible or overly punitive approach.
 *
 * DIRECTIONALITY LOGIC:
 *   Global South nations and future generations are clear beneficiaries (d=0.0-0.1) as they are spared the worst climate harms. Global North consumers, extractive industries, and growth-oriented economies are direct targets (d=0.9-1.0) as they bear the costs of contraction and systemic change. Degrowth advocates act as agenda-setters, pushing for the implementation of this constraint. Mitigation and adaptation advocates are excluded, as their approaches are deemed insufficient or misguided by this reading.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is not subject to mandatrophy in the traditional sense, as its mandate (preventing climate harm) is perceived as increasingly urgent. However, the 'mandate' of economic growth itself is what this constraint seeks to resolve. The classification as a Snare reflects the significant, non-consensual extraction required from powerful actors, which is often masked by claims of 'necessity' or 'inevitability' by its proponents. The high resistance and suppression indicate that this is not a simple coordination problem but a fundamental conflict over resource allocation and societal priorities.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    degrowth_feasibility_empirical,
    'Is planned economic contraction in the Global North politically and logistically feasible without causing societal collapse or authoritarianism?',
    'Empirical observation of degrowth policies implemented at scale, assessing their social, economic, and political outcomes.',
    'If feasible, the constraint is a difficult but necessary Rope; if infeasible, it is a Snare that demands impossible sacrifices, leading to either failure or coercive enforcement.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(degrowth_feasibility_empirical, empirical, 'Feasibility of degrowth policies in the Global North.').

omega_variable(
    growth_decoupling_conceptual,
    'Is it conceptually possible to decouple economic growth from resource consumption and emissions sufficiently to meet climate targets, or is degrowth the only logical path?',
    'Theoretical and empirical analysis of ''green growth'' models versus absolute decoupling requirements, assessing the physical limits and political will for technological solutions.',
    'If decoupling is possible, the degrowth reading forecloses a viable alternative; if impossible, the degrowth reading is a Mountain of physical reality, and other readings are Snares.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(growth_decoupling_conceptual, conceptual, 'The fundamental debate over green growth vs. degrowth as a climate solution.').

omega_variable(
    climate_harm_prevention_kernel_reading,
    'This constraint is the ''degrowth_reading'' of the ''climate_harm_prevention'' kernel. How would the classification change if a ''mitigation_priority'' or ''adaptation_priority'' reading were adopted?',
    'Analyzing the structural properties (beneficiaries, victims, extractiveness, suppression) of the sibling readings.',
    'The ''mitigation_priority'' reading would likely be a Tangled Rope (coordinating technological transition with some extraction), while ''adaptation_priority'' might be a Rope or Scaffold (coordinating resilience building, potentially temporary). This ''degrowth_reading'' is more extractive and suppressive due to its demand for contraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(climate_harm_prevention_kernel_reading, conceptual, 'Impact of alternative readings of the climate_harm_prevention kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_harm_prevention__degrowth_reading, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_harm_prevention__degrowth_reading, base_extractiveness, 0, 0.7).
narrative_ontology:measurement(clim_be_t5, climate_harm_prevention__degrowth_reading, base_extractiveness, 5, 0.75).
narrative_ontology:measurement(clim_be_t10, climate_harm_prevention__degrowth_reading, base_extractiveness, 10, 0.8).
narrative_ontology:measurement(clim_be_t15, climate_harm_prevention__degrowth_reading, base_extractiveness, 15, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_harm_prevention__degrowth_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(clim_su_t5, climate_harm_prevention__degrowth_reading, suppression_requirement, 5, 0.6).
narrative_ontology:measurement(clim_su_t10, climate_harm_prevention__degrowth_reading, suppression_requirement, 10, 0.65).
narrative_ontology:measurement(clim_su_t15, climate_harm_prevention__degrowth_reading, suppression_requirement, 15, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_harm_prevention__degrowth_reading, resource_allocation).
narrative_ontology:affects_constraint(climate_harm_prevention__degrowth_reading, climate_harm_prevention__mitigation_priority).
narrative_ontology:affects_constraint(climate_harm_prevention__degrowth_reading, climate_harm_prevention__adaptation_priority).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'climate_harm_prevention' kernel, each representing a distinct approach to climate response. This 'degrowth_reading' fundamentally challenges the growth paradigm, distinguishing it from 'mitigation_priority' (technological solutions within growth) and 'adaptation_priority' (focus on resilience).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
