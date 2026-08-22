% ============================================================================
% CONSTRAINT STORY: climate_mitigation_imperative__systems_transition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-20
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
 *   constraint_id: climate_mitigation_imperative__systems_transition_reading
 *   human_readable: Climate Mitigation Imperative — Systems Transition Reading
 *   domain: energy_policy/climate_mitigation/technology_governance
 *
 * SUMMARY:
 *   This constraint story instantiates the 'systems transition reading' of
 *   the contested kernel 'climate mitigation imperative.' The reading holds
 *   that mitigation is not merely a carbon accounting problem but a
 *   governance transformation: energy systems must be restructured toward
 *   decentralization and democratic control, and nuclear power is
 *   structurally incompatible with this transformation because its physics,
 *   economics, and institutional requirements perpetuate centralized,
 *   extractive relations. The constraint operates as a tangled rope: it
 *   coordinates a genuine collective action problem (aligning decarbonization
 *   with justice) while extracting from nuclear, centralized utilities, and
 *   large-scale project finance through policy exclusion and capital
 *   redirection. The constraint requires active enforcement (interconnection
 *   reform, community choice aggregation enabling legislation, nuclear
 *   subsidy termination) and has identifiable victims. The claim/metric
 *   independence is observed: the reading claims to be a rope (pure
 *   coordination for justice), but the authored metrics reveal substantial
 *   extraction and suppression — the engine will compute the divergence.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_mitigation_imperative__systems_transition_reading, 0.68).
domain_priors:suppression_score(climate_mitigation_imperative__systems_transition_reading, 0.62).
domain_priors:theater_ratio(climate_mitigation_imperative__systems_transition_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_mitigation_imperative__systems_transition_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(climate_mitigation_imperative__systems_transition_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(climate_mitigation_imperative__systems_transition_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_mitigation_imperative__systems_transition_reading, accessibility_collapse, 0.54).
narrative_ontology:constraint_metric(climate_mitigation_imperative__systems_transition_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_mitigation_imperative__systems_transition_reading, tangled_rope).
narrative_ontology:human_readable(climate_mitigation_imperative__systems_transition_reading, "Climate Mitigation Imperative — Systems Transition Reading").
narrative_ontology:topic_domain(climate_mitigation_imperative__systems_transition_reading, "energy_policy/climate_mitigation/technology_governance").

domain_priors:requires_active_enforcement(climate_mitigation_imperative__systems_transition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_mitigation_imperative__systems_transition_reading, '07b2db57-5f70-487a-a97d-d06f8f6a9bb9').
narrative_ontology:cs_kernel_codification('07b2db57-5f70-487a-a97d-d06f8f6a9bb9', distributed).
narrative_ontology:cs_authority_grounding('07b2db57-5f70-487a-a97d-d06f8f6a9bb9', extraction).
narrative_ontology:cs_interpretation_layer_present('07b2db57-5f70-487a-a97d-d06f8f6a9bb9').
narrative_ontology:cs_reading_relation('07b2db57-5f70-487a-a97d-d06f8f6a9bb9', climate_mitigation_imperative__portfolio_optimization_reading, influences).
narrative_ontology:cs_reading_relation('07b2db57-5f70-487a-a97d-d06f8f6a9bb9', climate_mitigation_imperative__opportunity_cost_reading, coexists_with).
narrative_ontology:cs_axiom('07b2db57-5f70-487a-a97d-d06f8f6a9bb9', foundational, decarbonization_requires_democratization).
narrative_ontology:cs_axiom_status(decarbonization_requires_democratization, holdable).
narrative_ontology:cs_axiom_grounding('07b2db57-5f70-487a-a97d-d06f8f6a9bb9', decarbonization_requires_democratization, deontological).
narrative_ontology:cs_axiom('07b2db57-5f70-487a-a97d-d06f8f6a9bb9', foundational, centralized_generation_perpetuates_extractive_relations).
narrative_ontology:cs_axiom_status(centralized_generation_perpetuates_extractive_relations, holdable).
narrative_ontology:cs_axiom_grounding('07b2db57-5f70-487a-a97d-d06f8f6a9bb9', centralized_generation_perpetuates_extractive_relations, empirically_contingent).
narrative_ontology:cs_reference_frame('07b2db57-5f70-487a-a97d-d06f8f6a9bb9', extractive_centralized_energy_order).
narrative_ontology:cs_drift_state('07b2db57-5f70-487a-a97d-d06f8f6a9bb9', contemporary_energy_transition, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('07b2db57-5f70-487a-a97d-d06f8f6a9bb9', '').
narrative_ontology:cs_kernel_id(climate_mitigation_imperative__systems_transition_reading, climate_mitigation_imperative).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__systems_transition_reading, distributed_renewables_developers).
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__systems_transition_reading, energy_communities).
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__systems_transition_reading, municipal_utilities).
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__systems_transition_reading, democratic_energy_advocates).
narrative_ontology:constraint_victim(climate_mitigation_imperative__systems_transition_reading, nuclear_industry).
narrative_ontology:constraint_victim(climate_mitigation_imperative__systems_transition_reading, centralized_grid_operators).
narrative_ontology:constraint_victim(climate_mitigation_imperative__systems_transition_reading, large_scale_gen_project_finance).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__systems_transition_reading, centralized_grid_operators).
narrative_ontology:constraint_victim(climate_mitigation_imperative__systems_transition_reading, energy_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Build and operate community solar, rooftop installations, and small-scale wind. Benefit from policy frameworks that prioritize distributed generation, simplified interconnection, and value-of-solar tariffs. Their business model depends on regulatory recognition of distributed energy as a public good, but they remain vulnerable to utility opposition and interconnection queue delays.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__systems_transition_reading, distributed_renewables_developers, beneficiary,
    organized, biographical, constrained, national).

% Local cooperatives, municipal aggregations, and neighborhood-scale energy governance bodies. They gain decision-making authority over energy infrastructure and revenue retention when the transition reading shapes policy. They also bear upfront capital costs and organizational labor of building democratic energy institutions. Their identity is constituted through the relational practice of collective energy governance — exit means dissolving the community's reason for existing.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__systems_transition_reading, energy_communities, beneficiary,
    moderate, generational, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(climate_mitigation_imperative__systems_transition_reading, energy_communities, payer).

% Publicly owned utilities that can expand into distributed energy services and retail choice when policy favors decentralization. They gain market share and political legitimacy as democratic alternatives to investor-owned utilities. However, they face stranded asset risk in existing centralized infrastructure and require state-level enabling legislation that is politically contested.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__systems_transition_reading, municipal_utilities, beneficiary,
    institutional, generational, constrained, regional).

% Civil society organizations, climate justice networks, and policy entrepreneurs who frame mitigation as a governance transformation. They set the intellectual and political agenda for the systems transition reading, mobilizing around energy democracy, just transition, and anti-extractivism. Their professional and ideological identity is fused with this framing — abandoning it would mean leaving the field that defines their work.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__systems_transition_reading, democratic_energy_advocates, agenda_setter,
    organized, civilizational, identity_locked, global).

% Reactor vendors, fuel cycle companies, and nuclear trade associations. They lose policy support, financing access, and social license when the systems transition reading prevails, because nuclear's centralized, capital-intensive, expertise-gated model is structurally incompatible with democratic decentralization. They can pivot to SMRs or export markets but remain locked into a technology paradigm the reading explicitly rejects.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__systems_transition_reading, nuclear_industry, payer,
    institutional, biographical, constrained, global).

% ISOs, RTOs, and transmission operators whose planning authority and revenue model assume centralized generation and top-down grid management. They are victimized when the reading mandates distribution-level planning authority and democratic oversight of grid investments. They also benefit from the coordination function of managing a decarbonizing grid — creating a genuine dual position where the same actors are coordinated and extracted from.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__systems_transition_reading, centralized_grid_operators, payer,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(climate_mitigation_imperative__systems_transition_reading, centralized_grid_operators, beneficiary).

% Banks, infrastructure funds, and development finance institutions that underwrite gigawatt-scale generation projects. Capital allocation shifts away from their pipeline when policy favors distributed, small-scale, community-owned assets. They have high exit optionality — capital is mobile and can redeploy to renewable portfolios — but the reading's governance demands (community ownership, democratic control) reduce the financializable surface area of the transition.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__systems_transition_reading, large_scale_gen_project_finance, payer,
    powerful, immediate, arbitrage, global).

% Technical experts who evaluate whether a decentralized, democratically governed system can meet reliability, resilience, and decarbonization targets simultaneously. They provide the epistemic infrastructure for both sides — their analyses are cited by transition advocates to prove feasibility and by nuclear advocates to prove infeasibility. They do not collect rents from either arrangement.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__systems_transition_reading, grid_reliability_engineers, observer,
    analytical, biographical, analytical, national).

% Communities historically burdened by energy extraction and pollution (fossil and nuclear). They would demand that democratic control include reparative justice, land sovereignty, and veto power over siting — but are often excluded from the policy tables where 'energy democracy' is defined by professionalized NGOs and municipal actors. Their exclusion is structural: the reading's advocacy infrastructure rarely includes them as decision-makers.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__systems_transition_reading, frontline_communities, excluded,
    powerless, generational, trapped, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the reorientation of energy infrastructure investment, planning authority, and revenue flows away from centralized, expert-gated, capital-intensive systems toward distributed, democratically governed, community-scale systems — solving the collective action problem of aligning decarbonization with energy justice.
% TRANSFER_FUNCTION: Moves planning authority, capital allocation, and operational revenue from centralized generators (nuclear, large hydro, investor-owned utilities) and their financial backers to distributed renewable developers, energy communities, and municipal utilities — while also transferring governance risk and organizational labor to the latter.
% ABSENT_VOICES: Frontline communities (especially Indigenous nations, fence-line communities near uranium mines and reactor sites, and energy-poor rural populations) are structurally excluded from defining 'democratic control' in mainstream transition advocacy. They would object to a transition that replicates extractive siting logics under a democratic banner. Global South energy sovereignty movements are also absent — the reading is largely articulated in Global North policy frameworks.
% DISAPPEARANCE_RATIONALE: If the systems transition reading vanished overnight, energy policy would default to the portfolio optimization reading (maximize all low-carbon sources) or opportunity cost reading (fastest deployment per dollar) — both of which legitimize nuclear and centralized renewables. The specific governance demands (community ownership, distribution-level planning authority, democratic oversight) would lose their policy foothold, and capital would flow back to large-scale, centralized projects.
% FOUNDING_PROBLEM: The founding problem is the twin crisis of climate breakdown and energy injustice: centralized fossil and nuclear systems have concentrated pollution, risk, and decision-making power in sacrifice zones while excluding affected communities from governance. The arrangement was built to solve this by making decarbonization inseparable from democratization — so that the transition does not reproduce the extractive relations of the energy system it replaces.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is corroborated by energy justice scholarship (e.g., Sovacool, Heffron, Jenkins), Indigenous environmental movements (e.g., Indigenous Environmental Network), and frontline community testimony at FERC and state PUC proceedings — all from outside the professional NGO and municipal utility beneficiaries. The nuclear industry and centralized utilities dispute the framing, arguing the problem is emissions only, not governance.
narrative_ontology:disappearance_verdict(climate_mitigation_imperative__systems_transition_reading, world_rearranges).
narrative_ontology:founding_problem_status(climate_mitigation_imperative__systems_transition_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_mitigation_imperative__systems_transition_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(climate_mitigation_imperative__systems_transition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(climate_mitigation_imperative__systems_transition_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness (0.68) reflects the reading's structural exclusion of nuclear and centralized generation from policy support and capital access — not merely competition but active foreclosure. Suppression (0.62) captures the enforcement machinery: interconnection barriers for nuclear, subsidy termination campaigns, planning authority restructuring. Theater ratio (0.38) acknowledges that the democratic governance function is real but increasingly performed through professionalized advocacy that substitutes procedural inclusion for substantive community power. Accessibility collapse (0.54) is moderate: alternatives (portfolio optimization, opportunity cost readings) remain intellectually and politically viable. Resistance (0.71) is high: nuclear industry, utilities, and financial actors mount coordinated opposition through lobbying, litigation, and narrative contests.
 *
 * PERSPECTIVAL GAP:
 *   From the democratic energy advocate seat, the constraint is genuine coordination for justice — the extraction from nuclear is a feature, not a bug, because nuclear's centralization IS the injustice. From the nuclear industry seat, the same constraint is a snare: a justice narrative weaponized to exclude a low-carbon technology. From the centralized grid operator seat, it is a tangled rope: they gain a role in the transition but lose their planning monopoly. From the frontline community seat (excluded), the constraint's democratic claims are suspect — the governance transformation may not reach them. The engine computes these per-seat classifications from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (distributed renewables developers, energy communities, municipal utilities, democratic energy advocates) receive planning authority, revenue streams, and political legitimacy — their d values derive toward the beneficiary end. Victims (nuclear industry, centralized grid operators, large-scale project finance) lose policy access, capital allocation, and social license — their d values derive toward the target end. Centralized grid operators sit in a genuine dual position: they are coordinated (given grid management authority in a decarbonizing system) and extracted from (stripped of top-down planning monopoly). Frontline communities are excluded — their structural position is trapped with no voice in the constraint's operation. The agenda_setter (democratic energy advocates) is identity-locked: their professional and ideological self-concept is constituted through this reading.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (climate + energy injustice) is live and worsening. The arrangement has not atrophied — its justification strengthens as climate impacts concentrate in frontline communities. However, mandatrophy risk exists in the professionalization of 'energy democracy': if the advocacy infrastructure captures the governance demands and converts them into procedural checkboxes (community benefit agreements without ownership, advisory boards without veto), the constraint becomes a piton — theatrical maintenance of democratic language while extraction continues through centralized renewable development. The theater ratio trajectory (rising from 0.22 to 0.38) tracks this risk.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    governance_vs_decarbonization_tradeoff,
    'Does the systems transition reading''s governance criterion (democratic control, decentralization) accelerate or delay net-zero achievement compared to a portfolio optimization approach?',
    'Integrated assessment modeling with governance constraints: compare decarbonization pathways with and without democratic decentralization requirements, measuring timeline, cost, and justice outcomes.',
    'If democratic decentralization delays net-zero by >5 years or increases cost >30%, the reading''s extraction from nuclear may be maladaptive for climate goals; if it accelerates or is neutral, the governance transformation is synergistic with decarbonization.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(governance_vs_decarbonization_tradeoff, empirical, 'Whether the governance transformation the reading demands is compatible with the speed and scale of required decarbonization.').

omega_variable(
    nuclear_incompatibility_mechanism,
    'Is nuclear''s incompatibility with democratic decentralization inherent to the technology (physics, safety, waste, proliferation) or contingent on its current institutional form (state-owned enterprises, regulated monopolies, expert-gated regulation)?',
    'Counterfactual institutional design: model nuclear deployment under community ownership, cooperative governance, and distributed siting (e.g., microreactors) — assess whether the technology''s physical characteristics necessitate centralization.',
    'If incompatibility is inherent, nuclear is structurally a victim of any democratic energy transition; if contingent, the reading''s exclusion of nuclear is a political choice, not a structural necessity — changing the classification from tangled_rope toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(nuclear_incompatibility_mechanism, conceptual, 'Whether nuclear''s centralization is technologically necessary or institutionally contingent.').

omega_variable(
    democratic_capture_risk,
    'To what extent has the ''energy democracy'' advocacy infrastructure been captured by professionalized NGOs, municipal utilities, and renewable developers — substituting procedural inclusion for substantive community power?',
    'Longitudinal tracking of community ownership shares, decision-making authority, and revenue retention in projects labeled ''energy democracy'' vs. ''community solar'' vs. ''utility-scale distributed'' — measuring the gap between rhetoric and governance reality.',
    'High capture would increase theater_ratio toward piton territory and reclassify the constraint as performative coordination masking continued extraction by professionalized actors; low capture would validate the reading''s coordination function as genuine.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(democratic_capture_risk, empirical, 'Whether the democratic governance the reading claims to coordinate has been hollowed out by professional capture.').

omega_variable(
    kernel_reading_boundary,
    'Where exactly does the structural disagreement between the systems_transition_reading and the portfolio_optimization_reading locate — in the objective function (what counts as mitigation), the constraint set (what technologies are eligible), or the governance model (who decides)?',
    'Formal decomposition of each reading''s optimization problem: identify which variables, constraints, and objective terms differ. Map to policy levers (technology eligibility, planning authority, ownership rules).',
    'If the disagreement is only in the governance model, the readings may coexist as complementary (portfolio optimization for technology mix + systems transition for governance); if in the objective function or constraint set, they are mutually exclusive policy frameworks.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_boundary, conceptual, 'The precise structural locus of disagreement between this reading and the portfolio optimization sibling.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_mitigation_imperative__systems_transition_reading, 2015, 2035).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(climate_mitigation_systems_transition_tr_t2015, climate_mitigation_imperative__systems_transition_reading, theater_ratio, 2015, 0.22).
narrative_ontology:measurement(climate_mitigation_systems_transition_tr_t2018, climate_mitigation_imperative__systems_transition_reading, theater_ratio, 2018, 0.25).
narrative_ontology:measurement(climate_mitigation_systems_transition_tr_t2021, climate_mitigation_imperative__systems_transition_reading, theater_ratio, 2021, 0.29).
narrative_ontology:measurement(climate_mitigation_systems_transition_tr_t2024, climate_mitigation_imperative__systems_transition_reading, theater_ratio, 2024, 0.33).
narrative_ontology:measurement(climate_mitigation_systems_transition_tr_t2027, climate_mitigation_imperative__systems_transition_reading, theater_ratio, 2027, 0.36).
narrative_ontology:measurement(climate_mitigation_systems_transition_tr_t2030, climate_mitigation_imperative__systems_transition_reading, theater_ratio, 2030, 0.38).
narrative_ontology:measurement(climate_mitigation_systems_transition_tr_t2033, climate_mitigation_imperative__systems_transition_reading, theater_ratio, 2033, 0.38).
narrative_ontology:measurement(climate_mitigation_systems_transition_tr_t2035, climate_mitigation_imperative__systems_transition_reading, theater_ratio, 2035, 0.38).

% Extraction over time
narrative_ontology:measurement(climate_mitigation_systems_transition_be_t2015, climate_mitigation_imperative__systems_transition_reading, base_extractiveness, 2015, 0.42).
narrative_ontology:measurement(climate_mitigation_systems_transition_be_t2018, climate_mitigation_imperative__systems_transition_reading, base_extractiveness, 2018, 0.48).
narrative_ontology:measurement(climate_mitigation_systems_transition_be_t2021, climate_mitigation_imperative__systems_transition_reading, base_extractiveness, 2021, 0.55).
narrative_ontology:measurement(climate_mitigation_systems_transition_be_t2024, climate_mitigation_imperative__systems_transition_reading, base_extractiveness, 2024, 0.62).
narrative_ontology:measurement(climate_mitigation_systems_transition_be_t2027, climate_mitigation_imperative__systems_transition_reading, base_extractiveness, 2027, 0.66).
narrative_ontology:measurement(climate_mitigation_systems_transition_be_t2030, climate_mitigation_imperative__systems_transition_reading, base_extractiveness, 2030, 0.68).
narrative_ontology:measurement(climate_mitigation_systems_transition_be_t2033, climate_mitigation_imperative__systems_transition_reading, base_extractiveness, 2033, 0.68).
narrative_ontology:measurement(climate_mitigation_systems_transition_be_t2035, climate_mitigation_imperative__systems_transition_reading, base_extractiveness, 2035, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(climate_mitigation_systems_transition_su_t2015, climate_mitigation_imperative__systems_transition_reading, suppression_requirement, 2015, 0.38).
narrative_ontology:measurement(climate_mitigation_systems_transition_su_t2018, climate_mitigation_imperative__systems_transition_reading, suppression_requirement, 2018, 0.44).
narrative_ontology:measurement(climate_mitigation_systems_transition_su_t2021, climate_mitigation_imperative__systems_transition_reading, suppression_requirement, 2021, 0.51).
narrative_ontology:measurement(climate_mitigation_systems_transition_su_t2024, climate_mitigation_imperative__systems_transition_reading, suppression_requirement, 2024, 0.57).
narrative_ontology:measurement(climate_mitigation_systems_transition_su_t2027, climate_mitigation_imperative__systems_transition_reading, suppression_requirement, 2027, 0.6).
narrative_ontology:measurement(climate_mitigation_systems_transition_su_t2030, climate_mitigation_imperative__systems_transition_reading, suppression_requirement, 2030, 0.62).
narrative_ontology:measurement(climate_mitigation_systems_transition_su_t2033, climate_mitigation_imperative__systems_transition_reading, suppression_requirement, 2033, 0.62).
narrative_ontology:measurement(climate_mitigation_systems_transition_su_t2035, climate_mitigation_imperative__systems_transition_reading, suppression_requirement, 2035, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_mitigation_imperative__systems_transition_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(climate_mitigation_imperative__systems_transition_reading, 0.08).
narrative_ontology:affects_constraint(climate_mitigation_imperative__systems_transition_reading, climate_mitigation_imperative__portfolio_optimization_reading).
narrative_ontology:affects_constraint(climate_mitigation_imperative__systems_transition_reading, climate_mitigation_imperative__opportunity_cost_reading).
narrative_ontology:affects_constraint(climate_mitigation_imperative__systems_transition_reading, nuclear_phaseout_policy).
narrative_ontology:affects_constraint(climate_mitigation_imperative__systems_transition_reading, distributed_energy_resource_integration).
narrative_ontology:affects_constraint(climate_mitigation_imperative__systems_transition_reading, energy_democracy_legislation).

% DUAL FORMULATION NOTE:
% This story is one of three readings of the climate_mitigation_imperative kernel. The portfolio_optimization_reading maximizes low-carbon deployment (nuclear as beneficiary). The opportunity_cost_reading minimizes cost-per-ton (nuclear as victim). This reading makes governance transformation the criterion (nuclear as victim, distributed renewables as beneficiaries). All three share the referent 'climate mitigation' but instantiate different constraints with different ε, beneficiaries, victims, and types. The network edges reflect structural influence: this reading's policy success reduces nuclear's policy space (affects nuclear_phaseout_policy) and expands distributed energy's policy space (affects distributed_energy_resource_integration, energy_democracy_legislation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(climate_mitigation_imperative__systems_transition_reading, institutional, 0.75).
constraint_indexing:directionality_override(climate_mitigation_imperative__systems_transition_reading, organized, 0.15).
constraint_indexing:directionality_override(climate_mitigation_imperative__systems_transition_reading, moderate, 0.45).
constraint_indexing:directionality_override(climate_mitigation_imperative__systems_transition_reading, powerful, 0.2).
constraint_indexing:directionality_override(climate_mitigation_imperative__systems_transition_reading, powerless, 0.9).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
