% ============================================================================
% CONSTRAINT STORY: climate_mitigation_imperative__systems_transition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_mitigation_imperative__systems_transition_reading, []).

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
 *   constraint_id: climate_mitigation_imperative__systems_transition_reading
 *   human_readable: Climate Mitigation Imperative: Systems Transition Reading
 *   domain: energy_policy/climate_mitigation/technology_governance
 *
 * SUMMARY:
 *   This constraint represents the 'systems transition' reading of the
 *   climate mitigation imperative, which asserts that effective climate
 *   action requires a fundamental transformation of energy governance towards
 *   decentralization and democratic control. From this perspective,
 *   technologies like nuclear power, which are seen as inherently centralized
 *   and extractive, are incompatible with true mitigation and become
 *   'victims' of the imperative. The constraint is not merely about carbon
 *   reduction but about the socio-technical system's structure. It is claimed
 *   as a Tangled Rope because it coordinates the transition towards
 *   distributed renewables while actively suppressing (extracting from)
 *   centralized energy incumbents.
 *
 * KEY AGENTS:
 *   - energy_democracy_advocates: Agenda-setter (institutional/analytical) — defines the terms of the transition
 *   - distributed_renewable_developers: Beneficiary (organized/mobile) — benefits from policy support for decentralized systems
 *   - local_energy_cooperatives: Beneficiary (organized/mobile) — benefits from policy support for community-led energy projects
 *   - nuclear_industry: Payer (institutional/constrained) — faces opposition and divestment pressure
 *   - fossil_fuel_incumbents: Payer (institutional/constrained) — faces accelerated phase-out pressure
 *   - centralized_grid_operators: Payer (institutional/constrained) — faces pressure to adapt to decentralized generation
 *   - policy_makers: Agenda-setter (institutional/constrained) — implements policies reflecting this imperative
 *   - climate_scientists: Observer (analytical/analytical) — provides data on climate impacts, but not necessarily on system governance
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_mitigation_imperative__systems_transition_reading, 0.65).
domain_priors:suppression_score(climate_mitigation_imperative__systems_transition_reading, 0.7).
domain_priors:theater_ratio(climate_mitigation_imperative__systems_transition_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_mitigation_imperative__systems_transition_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(climate_mitigation_imperative__systems_transition_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(climate_mitigation_imperative__systems_transition_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_mitigation_imperative__systems_transition_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(climate_mitigation_imperative__systems_transition_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_mitigation_imperative__systems_transition_reading, tangled_rope).
narrative_ontology:human_readable(climate_mitigation_imperative__systems_transition_reading, "Climate Mitigation Imperative: Systems Transition Reading").
narrative_ontology:topic_domain(climate_mitigation_imperative__systems_transition_reading, "energy_policy/climate_mitigation/technology_governance").

domain_priors:requires_active_enforcement(climate_mitigation_imperative__systems_transition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_mitigation_imperative__systems_transition_reading, 'c34dfac3-7d66-4baf-9ea5-770b6858309e').
narrative_ontology:cs_kernel_codification('c34dfac3-7d66-4baf-9ea5-770b6858309e', distributed).
narrative_ontology:cs_authority_grounding('c34dfac3-7d66-4baf-9ea5-770b6858309e', distributed).
narrative_ontology:cs_reading_relation('c34dfac3-7d66-4baf-9ea5-770b6858309e', climate_mitigation_imperative__portfolio_optimization_reading, coexists_with).
narrative_ontology:cs_reading_relation('c34dfac3-7d66-4baf-9ea5-770b6858309e', climate_mitigation_imperative__opportunity_cost_reading, coexists_with).
narrative_ontology:cs_axiom('c34dfac3-7d66-4baf-9ea5-770b6858309e', foundational, decentralization_is_mitigation).
narrative_ontology:cs_axiom_status(decentralization_is_mitigation, holdable).
narrative_ontology:cs_axiom_grounding('c34dfac3-7d66-4baf-9ea5-770b6858309e', decentralization_is_mitigation, instrumental).
narrative_ontology:cs_axiom('c34dfac3-7d66-4baf-9ea5-770b6858309e', foundational, democratic_control_is_essential).
narrative_ontology:cs_axiom_status(democratic_control_is_essential, holdable).
narrative_ontology:cs_axiom_grounding('c34dfac3-7d66-4baf-9ea5-770b6858309e', democratic_control_is_essential, deontological).
narrative_ontology:cs_reference_frame('c34dfac3-7d66-4baf-9ea5-770b6858309e', just_energy_transition_framework).
narrative_ontology:cs_drift_state('c34dfac3-7d66-4baf-9ea5-770b6858309e', contemporary, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('c34dfac3-7d66-4baf-9ea5-770b6858309e', '').
narrative_ontology:cs_kernel_id(climate_mitigation_imperative__systems_transition_reading, climate_mitigation_imperative).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__systems_transition_reading, distributed_renewable_developers).
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__systems_transition_reading, local_energy_cooperatives).
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__systems_transition_reading, energy_democracy_advocates).
narrative_ontology:constraint_victim(climate_mitigation_imperative__systems_transition_reading, nuclear_industry).
narrative_ontology:constraint_victim(climate_mitigation_imperative__systems_transition_reading, fossil_fuel_incumbents).
narrative_ontology:constraint_victim(climate_mitigation_imperative__systems_transition_reading, centralized_grid_operators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Actively promote policies and narratives that prioritize decentralized, democratically controlled energy systems as the core of climate mitigation. They define the terms of the 'systems transition' and lobby for its implementation.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__systems_transition_reading, energy_democracy_advocates, agenda_setter,
    institutional, generational, mobile, global).

% Benefit from policy frameworks and public investment that favor solar, wind, and other distributed energy resources. Their business models align with the systems transition imperative.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__systems_transition_reading, distributed_renewable_developers, beneficiary,
    organized, biographical, mobile, national).

% Are empowered by policies supporting community-owned and managed energy projects, directly benefiting from the decentralization aspect of the systems transition. They face fewer barriers to entry and growth.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__systems_transition_reading, local_energy_cooperatives, beneficiary,
    moderate, generational, constrained, local).

% Faces significant opposition, divestment campaigns, and regulatory hurdles from proponents of the systems transition reading, who view nuclear as a centralized, undemocratic, and risky technology incompatible with the desired future energy system. This leads to project cancellations and reduced investment.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__systems_transition_reading, nuclear_industry, payer,
    institutional, civilizational, constrained, global).

% Are directly targeted for phase-out and divestment by the systems transition imperative, facing accelerated pressure to cease operations and transition away from fossil fuels. Their assets are increasingly stranded.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__systems_transition_reading, fossil_fuel_incumbents, payer,
    institutional, biographical, constrained, global).

% Are pressured to fundamentally restructure their operations to accommodate distributed generation, smart grids, and local energy markets, moving away from a traditional top-down, centralized model. This requires significant capital expenditure and operational changes.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__systems_transition_reading, centralized_grid_operators, payer,
    institutional, generational, constrained, national).

% Are responsible for designing and implementing energy and climate policies. They are influenced by the systems transition narrative, leading to policies that favor renewables and decentralization, but also face lobbying from incumbent industries.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__systems_transition_reading, policy_makers, agenda_setter,
    institutional, generational, constrained, national).

% Provide the foundational scientific understanding of climate change and its impacts, which underpins the urgency of mitigation. While they do not directly advocate for specific energy system structures, their findings are interpreted by all parties to support their preferred mitigation pathways.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__systems_transition_reading, climate_scientists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_mitigation_imperative__systems_transition_reading, distributed_renewable_developers).
narrative_ontology:fixing_cost_class(climate_mitigation_imperative__systems_transition_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a societal-level transition towards a decentralized, democratically controlled energy system, aligning policy, investment, and public discourse around this vision to achieve climate mitigation and energy justice goals.
% TRANSFER_FUNCTION: Transfers political capital, investment, and social legitimacy away from centralized, incumbent energy systems (nuclear, fossil fuels) towards distributed renewable energy technologies and community-led energy initiatives.
% ABSENT_VOICES: Proponents of 'all-of-the-above' low-carbon strategies (e.g., those advocating for nuclear as a necessary baseload) are often marginalized in the discourse of this reading, as their solutions are deemed incompatible with the systemic transformation goal. Their arguments for nuclear's carbon-free benefits are suppressed by the focus on governance structure.
% DISAPPEARANCE_RATIONALE: If this imperative vanished, the momentum for decentralized energy systems would significantly diminish. Investment would likely flow back to more centralized, incumbent technologies, and the focus on democratic control and energy justice would wane, leading to a different, potentially less equitable, climate mitigation pathway.
% FOUNDING_PROBLEM: The founding problem was the dual crisis of climate change and energy injustice, rooted in centralized, fossil-fuel-dependent energy systems that perpetuated environmental damage, economic inequality, and lack of public control over essential resources.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is widely attested as live by environmental justice organizations, international climate bodies (e.g., IPCC reports highlighting equity concerns), and independent academic research on energy systems, all of whom corroborate the ongoing challenges of climate change and the structural issues of centralized energy systems, from outside the direct beneficiaries of distributed renewables.
narrative_ontology:disappearance_verdict(climate_mitigation_imperative__systems_transition_reading, world_rearranges).
narrative_ontology:founding_problem_status(climate_mitigation_imperative__systems_transition_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_mitigation_imperative__systems_transition_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(climate_mitigation_imperative__systems_transition_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_mitigation_imperative__systems_transition_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_mitigation_imperative__systems_transition_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_mitigation_imperative__systems_transition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) is substantial because it actively disfavors and seeks to dismantle existing centralized energy structures, imposing costs on incumbents. Suppression (0.70) is high due to the active policy and advocacy efforts to block new centralized projects and redirect investment. Theater ratio (0.20) is low because the movement is genuinely focused on systemic change, not just symbolic gestures. Accessibility collapse (0.40) is moderate as alternatives (centralized systems) are actively challenged but not entirely eliminated. Resistance (0.75) is high, reflecting the strong opposition from incumbent industries and their political allies.
 *
 * PERSPECTIVAL GAP:
 *   Energy democracy advocates and distributed renewable developers experience this as a necessary coordination mechanism for a just transition, leading to a beneficial outcome. The nuclear industry and fossil fuel incumbents, however, experience it as a highly extractive and suppressive force, threatening their business models and institutional power. Policy makers are caught between these perspectives, attempting to coordinate the transition while managing resistance.
 *
 * DIRECTIONALITY LOGIC:
 *   Energy democracy advocates and distributed renewable developers are beneficiaries (d near 0.0) as the constraint's operation directly supports their models. The nuclear industry, fossil fuel incumbents, and centralized grid operators are victims (d near 1.0) as the constraint actively works to diminish their role and extract resources from them. Policy makers, while agenda-setters, are also constrained by the imperative, balancing various pressures.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is not yet mandatrophic; its mandate (climate mitigation through systems transition) is very much live. The classification as Tangled Rope prevents mislabeling it as a pure Snare, acknowledging its genuine coordination function for decentralized systems, while also recognizing the asymmetric extraction from centralized incumbents. It also prevents mislabeling it as a pure Rope, which would ignore the significant costs imposed on the 'victims'.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint a genuine imperative for systems transition, or an ideological preference for specific technologies?',
    'Empirical analysis of long-term energy system resilience, equity, and democratic participation outcomes in systems pursuing decentralized vs. centralized mitigation pathways.',
    'If a genuine imperative, the classification holds; if an ideological preference, the constraint''s extractiveness and suppression metrics might be overstated, and its claimed coordination function weakened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'This constraint is the ''systems_transition_reading'' of the ''climate_mitigation_imperative'' kernel. Sibling readings (''portfolio_optimization_reading'', ''opportunity_cost_reading'') would shift the victim set and the definition of ''mitigation success''.').

omega_variable(
    centralization_extraction_link,
    'Is nuclear power inherently centralizing and extractive, or can it be integrated into decentralized, democratically controlled energy systems?',
    'Technological and governance innovation in small modular reactors (SMRs) and community-owned nuclear projects; empirical observation of their integration into local grids.',
    'If nuclear can be decentralized, its inclusion in the victim set of this reading would be challenged, potentially lowering the constraint''s measured extractiveness from the nuclear industry. If not, the current classification is reinforced.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(centralization_extraction_link, empirical, 'Ambiguity regarding the inherent centralizing nature of nuclear technology.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_mitigation_imperative__systems_transition_reading, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_mitigation_imperative__systems_transition_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(clim_tr_t5, climate_mitigation_imperative__systems_transition_reading, theater_ratio, 5, 0.15).
narrative_ontology:measurement(clim_tr_t10, climate_mitigation_imperative__systems_transition_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement(clim_tr_t15, climate_mitigation_imperative__systems_transition_reading, theater_ratio, 15, 0.2).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_mitigation_imperative__systems_transition_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(clim_be_t5, climate_mitigation_imperative__systems_transition_reading, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(clim_be_t10, climate_mitigation_imperative__systems_transition_reading, base_extractiveness, 10, 0.6).
narrative_ontology:measurement(clim_be_t15, climate_mitigation_imperative__systems_transition_reading, base_extractiveness, 15, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_mitigation_imperative__systems_transition_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(clim_su_t5, climate_mitigation_imperative__systems_transition_reading, suppression_requirement, 5, 0.6).
narrative_ontology:measurement(clim_su_t10, climate_mitigation_imperative__systems_transition_reading, suppression_requirement, 10, 0.65).
narrative_ontology:measurement(clim_su_t15, climate_mitigation_imperative__systems_transition_reading, suppression_requirement, 15, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_mitigation_imperative__systems_transition_reading, resource_allocation).
narrative_ontology:affects_constraint(climate_mitigation_imperative__systems_transition_reading, climate_mitigation_imperative__portfolio_optimization_reading).
narrative_ontology:affects_constraint(climate_mitigation_imperative__systems_transition_reading, climate_mitigation_imperative__opportunity_cost_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'climate_mitigation_imperative' kernel. It focuses on systemic transformation, contrasting with the 'portfolio_optimization_reading' (maximizing all low-carbon sources) and the 'opportunity_cost_reading' (fastest deployment per dollar).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
