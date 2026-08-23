% ============================================================================
% CONSTRAINT STORY: climate_mitigation_imperative__systems_transition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
 *   human_readable: Climate Mitigation as Democratic Energy Transition
 *   domain: energy_policy/climate_mitigation/technology_governance
 *
 * SUMMARY:
 *   This constraint story instantiates the 'systems_transition_reading' of
 *   the contested kernel 'climate_mitigation_imperative'. The kernel is the
 *   claim that mitigation requires a specific kind of energy system
 *   transformation. This reading argues that mitigation is not merely a
 *   carbon accounting problem but a governance transformation:
 *   decentralization and democratic control are constitutive of effective
 *   mitigation, not optional add-ons. Nuclear power enters the victim set
 *   because its physical and institutional characteristics (large-scale,
 *   centralized, capital-intensive, expertocratic, long-lived, non-modular)
 *   are structurally incompatible with democratic energy governance — not
 *   because of radiation risk or waste per se. The constraint is a tangled
 *   rope: it coordinates genuine collective action (rapid equitable
 *   decarbonization) while asymmetrically extracting from incumbents whose
 *   business models depend on centralization. The claim/metric independence
 *   is maintained: the reading claims tangled_rope (genuine coordination +
 *   asymmetric extraction), while the metrics describe the constraint's
 *   measured operation.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_mitigation_imperative__systems_transition_reading, 0.58).
domain_priors:suppression_score(climate_mitigation_imperative__systems_transition_reading, 0.62).
domain_priors:theater_ratio(climate_mitigation_imperative__systems_transition_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_mitigation_imperative__systems_transition_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(climate_mitigation_imperative__systems_transition_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(climate_mitigation_imperative__systems_transition_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_mitigation_imperative__systems_transition_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(climate_mitigation_imperative__systems_transition_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_mitigation_imperative__systems_transition_reading, tangled_rope).
narrative_ontology:human_readable(climate_mitigation_imperative__systems_transition_reading, "Climate Mitigation as Democratic Energy Transition").
narrative_ontology:topic_domain(climate_mitigation_imperative__systems_transition_reading, "energy_policy/climate_mitigation/technology_governance").

domain_priors:requires_active_enforcement(climate_mitigation_imperative__systems_transition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_mitigation_imperative__systems_transition_reading, '7bc85eb4-5219-44de-b7c1-5e60cc7efde8').
narrative_ontology:cs_kernel_codification('7bc85eb4-5219-44de-b7c1-5e60cc7efde8', formalized).
narrative_ontology:cs_authority_grounding('7bc85eb4-5219-44de-b7c1-5e60cc7efde8', distributed).
narrative_ontology:cs_reading_relation('7bc85eb4-5219-44de-b7c1-5e60cc7efde8', climate_mitigation_imperative__portfolio_optimization_reading, coexists_with).
narrative_ontology:cs_reading_relation('7bc85eb4-5219-44de-b7c1-5e60cc7efde8', climate_mitigation_imperative__opportunity_cost_reading, coexists_with).
narrative_ontology:cs_axiom('7bc85eb4-5219-44de-b7c1-5e60cc7efde8', foundational, decentralization_constitutive_of_mitigation).
narrative_ontology:cs_axiom_status(decentralization_constitutive_of_mitigation, holdable).
narrative_ontology:cs_axiom_grounding('7bc85eb4-5219-44de-b7c1-5e60cc7efde8', decentralization_constitutive_of_mitigation, deontological).
narrative_ontology:cs_axiom('7bc85eb4-5219-44de-b7c1-5e60cc7efde8', foundational, democratic_control_necessary_for_just_transition).
narrative_ontology:cs_axiom_status(democratic_control_necessary_for_just_transition, holdable).
narrative_ontology:cs_axiom_grounding('7bc85eb4-5219-44de-b7c1-5e60cc7efde8', democratic_control_necessary_for_just_transition, deontological).
narrative_ontology:cs_axiom('7bc85eb4-5219-44de-b7c1-5e60cc7efde8', secondary, centralized_generation_incompatible_with_energy_democracy).
narrative_ontology:cs_axiom_status(centralized_generation_incompatible_with_energy_democracy, holdable).
narrative_ontology:cs_axiom_grounding('7bc85eb4-5219-44de-b7c1-5e60cc7efde8', centralized_generation_incompatible_with_energy_democracy, empirically_contingent).
narrative_ontology:cs_reference_frame('7bc85eb4-5219-44de-b7c1-5e60cc7efde8', post_paris_climate_governance).
narrative_ontology:cs_drift_state('7bc85eb4-5219-44de-b7c1-5e60cc7efde8', contemporary_polycrisis, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('7bc85eb4-5219-44de-b7c1-5e60cc7efde8', '').
narrative_ontology:cs_kernel_id(climate_mitigation_imperative__systems_transition_reading, climate_mitigation_imperative).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__systems_transition_reading, distributed_renewable_communities).
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__systems_transition_reading, energy_democracy_advocates).
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__systems_transition_reading, local_governments_pursuing_energy_autonomy).
narrative_ontology:constraint_victim(climate_mitigation_imperative__systems_transition_reading, nuclear_industry).
narrative_ontology:constraint_victim(climate_mitigation_imperative__systems_transition_reading, centralized_utilities).
narrative_ontology:constraint_victim(climate_mitigation_imperative__systems_transition_reading, fossil_fuel_incumbents).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__systems_transition_reading, grid_operators_and_system_planners).
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__systems_transition_reading, energy_consumers_and_vulnerable_households).
narrative_ontology:constraint_victim(climate_mitigation_imperative__systems_transition_reading, grid_operators_and_system_planners).
narrative_ontology:constraint_victim(climate_mitigation_imperative__systems_transition_reading, energy_consumers_and_vulnerable_households).
narrative_ontology:constraint_vindicates(climate_mitigation_imperative__systems_transition_reading, energy_democracy_principle).
narrative_ontology:constraint_vindicates(climate_mitigation_imperative__systems_transition_reading, decentralized_resilience_thesis).
narrative_ontology:constraint_vindicates(climate_mitigation_imperative__systems_transition_reading, just_transition_framework).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Community energy cooperatives, municipal utilities, and rooftop solar adopters who gain decision-making control and revenue retention when energy systems decentralize. They face constrained exit because their investments are place-based and depend on policy frameworks that enable local ownership. Their structural position improves when governance shifts toward democratic control.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__systems_transition_reading, distributed_renewable_communities, beneficiary,
    organized, generational, constrained, regional).

% Civil society organizations, climate justice networks, and policy entrepreneurs who frame mitigation as a governance transformation. They set the agenda through international forums, national legislation, and local campaigns. They benefit professionally and ideologically from the frame's adoption but can pivot to adjacent justice frames if this one collapses.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__systems_transition_reading, energy_democracy_advocates, agenda_setter,
    organized, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(climate_mitigation_imperative__systems_transition_reading, energy_democracy_advocates, beneficiary).

% Cities and regions pursuing 100% renewable targets with local ownership mandates. They gain fiscal resilience and political legitimacy from energy autonomy. Their exit is constrained by jurisdictional boundaries and dependence on higher-level policy enabling frameworks.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__systems_transition_reading, local_governments_pursuing_energy_autonomy, beneficiary,
    institutional, biographical, constrained, local).

% Reactors vendors, operators, fuel cycle firms, and their regulatory capture networks. The democratic transition frame structurally excludes nuclear by defining it as incompatible with decentralization and democratic control — not merely on cost or speed grounds. Their assets are long-lived, capital-intensive, and non-redeployable; exit means stranded assets and institutional dissolution.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__systems_transition_reading, nuclear_industry, payer,
    institutional, generational, trapped, global).

% Vertically integrated investor-owned utilities whose business model depends on centralized generation, rate-based returns, and regulatory capture. They face constrained exit: some pivot to distributed asset ownership, but their structural incentives and regulatory compacts resist full transformation. They bear transition costs while fighting to shape the transition's terms.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__systems_transition_reading, centralized_utilities, payer,
    institutional, generational, constrained, national).

% Oil, gas, and coal majors facing simultaneous pressure from carbon budgets and democratic energy claims. They bear extraction from both the carbon constraint and the governance transformation. Some invest in renewables but centrally; their exit is constrained by reserve stranding and shareholder expectations.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__systems_transition_reading, fossil_fuel_incumbents, payer,
    institutional, biographical, constrained, global).

% States that set NDCs, design energy policy, and allocate transition finance. They arbitrage between centralized and decentralized pathways, capturing legitimacy from democratic framing while often preserving centralized control. Their structural position lets them extract rents from both frames.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__systems_transition_reading, national_governments, agenda_setter,
    institutional, biographical, arbitrage, national).

% UNFCCC, IPCC, IEA, multilateral development banks. They observe and legitimize competing framings through assessment reports, finance criteria, and capacity building. Their analytical seat lets them hold multiple readings simultaneously; their agenda-setting power shapes which frames receive institutional uptake.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__systems_transition_reading, international_climate_institutions, observer,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(climate_mitigation_imperative__systems_transition_reading, international_climate_institutions, agenda_setter).

% TSOs, DSOs, and regional planning authorities who must technically integrate decentralized resources. They bear operational costs of transformation (balancing, stability, market redesign) but gain institutional relevance and expanded mandate. Their exit is constrained by the physical necessity of grid operation.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__systems_transition_reading, grid_operators_and_system_planners, payer,
    organized, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(climate_mitigation_imperative__systems_transition_reading, grid_operators_and_system_planners, beneficiary).

% Households, especially low-income and energy-poor, who stand to gain from democratic control (affordability, reliability, participation) but bear transition costs (rate increases, retrofit burdens) and have no meaningful exit from energy systems. Their structural position is most precarious: they are the constraint's moral justification but often its last material beneficiaries.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__systems_transition_reading, energy_consumers_and_vulnerable_households, beneficiary,
    powerless, immediate, trapped, local).
narrative_ontology:stakeholder_secondary_role(climate_mitigation_imperative__systems_transition_reading, energy_consumers_and_vulnerable_households, payer).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates rapid, equitable decarbonization by aligning climate mitigation with democratic governance of energy systems — solving the legitimacy and distributional failures of top-down techno-fixes.
% TRANSFER_FUNCTION: Moves decision-making authority, revenue streams, and asset ownership from centralized incumbents (nuclear, fossil, centralized utilities) to distributed communities, local governments, and cooperative structures. Transfers risk from ratepayers to shareholders.
% ABSENT_VOICES: Industrial workers in nuclear and fossil sectors whose communities face transition without guaranteed just-transition frameworks; Global South governments pressured to leapfrog to decentralized systems without finance or technology transfer; future generations who bear climate impacts but cannot contest present framings.
% DISAPPEARANCE_RATIONALE: If the democratic transition imperative vanished, mitigation would default to portfolio optimization (maximizing low-carbon gigawatts regardless of ownership) or opportunity-cost minimization (fastest $/ton) — both of which preserve centralized control and reproduce extractive energy relations. The governance transformation is the constraint's distinctive work.
% FOUNDING_PROBLEM: Top-down, centralized energy transitions (nuclear build-outs, large hydro, CCS) have repeatedly failed to deliver timely decarbonization while reinforcing the same power structures that created the climate crisis — the founding problem is the co-constitution of carbon intensity and centralized control.
% FOUNDING_PROBLEM_CORROBORATION: IPCC AR6 WGIII Chapter 5 (demand-side mitigation), IRENA World Energy Transitions Outlook (distributed renewables dominance), and climate justice movements (La Via Campesina, Indigenous Environmental Network, Global Campaign to Demand Climate Justice) attest from outside the direct beneficiary set that centralized pathways have underdelivered and that democratic governance is a live necessity.
narrative_ontology:disappearance_verdict(climate_mitigation_imperative__systems_transition_reading, world_rearranges).
narrative_ontology:founding_problem_status(climate_mitigation_imperative__systems_transition_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_mitigation_imperative__systems_transition_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(climate_mitigation_imperative__systems_transition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(climate_mitigation_imperative__systems_transition_reading, 0.58, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness (0.58) reflects the constraint's displacement of incumbent rent streams — not merely carbon pricing but ownership and governance transfer. Suppression (0.62) captures institutional barriers: regulatory frameworks favoring centralized generation, grid codes excluding distributed resources, finance architectures requiring scale. Theater (0.42) measures performative 'community engagement' in transitions that preserve centralized control, and 'just transition' rhetoric without asset redistribution. Accessibility collapse (0.48) — alternatives (centralized nuclear/CCS) remain technically available but are politically delegitimized by this frame. Resistance (0.71) — incumbents deploy regulatory capture, disinformation, and financial lobbying to block democratic transition policies. The measurement grid (7 time points, 3 metrics) shows rising extraction and suppression as the frame gains institutional traction, with theater rising as incumbents adopt democratic language without ceding control.
 *
 * PERSPECTIVAL GAP:
 *   From the energy democracy advocate seat, the constraint is a rope (genuine coordination solving legitimacy and distribution). From the nuclear industry seat, it is a snare (ideological exclusion masquerading as climate necessity). From national governments, it is a scaffold (transitional framing they can adopt or discard). The engine computes these divergences from the structural data; the authored claim (tangled_rope) states the author's structural judgment that the constraint genuinely coordinates AND extracts.
 *
 * DIRECTIONALITY LOGIC:
 *   Distributed renewable communities, energy democracy advocates, and local governments are structural beneficiaries (d ~0.2-0.3): they gain authority and revenue from the constraint's operation. Nuclear industry is a full target (d ~0.95): the frame structurally excludes it, its assets are stranded by the constraint's logic, and exit is nearly impossible. Centralized utilities and fossil incumbents are high-target (d ~0.7-0.85): they bear transition costs and fight to shape terms. National governments arbitrage (d ~0.4-0.6): they capture legitimacy from both frames. Grid operators are constrained payers who gain institutional relevance (d ~0.55). Vulnerable households are trapped beneficiaries/payers (d ~0.6): moral justification of the frame, last material beneficiaries. The engine will compute per-seat χ from these structural positions.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandate (climate mitigation) remains live and intensifying. The reading argues the founding problem (centralized control co-constituted with carbon intensity) is live — corroborated by IPCC and justice movements. Mandatrophy is not resolved: the constraint's function has not atrophied; rather, its scope is expanding as mitigation urgency grows. The risk is false scaffold: if democratic transition rhetoric is adopted while centralized control persists, the constraint becomes a piton — theatrical maintenance of democratic language over extractive practice.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_framing,
    'Does the climate_mitigation_imperative kernel admit of structurally distinct constraints (one per reading), or is it a single constraint evaluated differently by different observers?',
    'Apply ε-invariance test: if measuring the constraint via ''carbon reduced per dollar'' yields portfolio_optimization (low extraction) but measuring via ''governance transformation achieved'' yields systems_transition (high extraction from incumbents), then ε varies by observable — they are distinct constraints. The engine requires separate stories with separate ε values.',
    'If single constraint: readings are observer perspectives on one ε. If distinct constraints: each reading gets its own ε, stakeholders, classification, and network edges. The current authoring assumes distinct constraints per DP-001 (ε-invariance principle).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_framing, conceptual, 'Whether the kernel decomposes into multiple ε-invariant constraints or remains one constraint with observer-relative classification.').

omega_variable(
    nuclear_victim_status_boundary,
    'Is nuclear''s victim status in this reading structural (physical/institutional incompatibility with decentralization) or contingent (current cost/timeline disadvantage that SMRs or regulatory reform could resolve)?',
    'Track SMR deployment outcomes, regulatory framework evolution, and whether nuclear proponents adopt decentralized ownership models (community-owned SMRs, cooperative licensing). If nuclear structurally adapts to democratic governance, victim status was contingent; if the frame rejects nuclear even in adapted forms, victim status is structural.',
    'If structural: nuclear remains victim regardless of tech evolution — constraint is fundamentally about governance form. If contingent: victim set shrinks as nuclear adapts, reducing extraction and potentially shifting classification toward rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(nuclear_victim_status_boundary, empirical, 'Whether nuclear''s exclusion from democratic energy transition is a permanent structural feature or a contingent technological/economic assessment.').

omega_variable(
    coordination_extraction_separability,
    'Can the coordination function (rapid equitable decarbonization) be achieved without the extraction function (displacing centralized incumbents), or are they structurally inseparable?',
    'Natural experiment: jurisdictions pursuing decentralized renewables with incumbent buy-in (e.g., utility-led community solar, just transition agreements with nuclear communities). If decarbonization speed/equity holds without incumbent displacement, functions are separable; if displacement is necessary for pace/justice, they are coupled.',
    'If separable: measured extraction includes avoidable rent-stripping, weakening the tangled_rope claim toward rope. If inseparable: extraction is the price of coordination, strengthening tangled_rope and justifying active enforcement against incumbents.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_separability, conceptual, 'Whether the constraint''s coordination and extraction components can be disaggregated in practice.').

omega_variable(
    global_south_finance_voice,
    'Does the democratic transition frame adequately represent Global South energy sovereignty claims, or does it impose Northern decentralized models without finance/technology transfer?',
    'Analyze NDCs, climate finance flows, and technology transfer agreements for explicit democratic governance conditionalities. Survey Global South negotiators and civil society on whether the frame empowers or constrains their sovereignty.',
    'If the frame imposes Northern models: excluded_voices omega understates structural exclusion, suppression is higher for Global South agents, and the constraint may function as snare for those seats. If empowering: beneficiary set expands, coordination function strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(global_south_finance_voice, preference, 'Whether the systems_transition_reading genuinely centers Global South agency or replicates green colonialism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_mitigation_imperative__systems_transition_reading, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cmi_str_tr_t0, climate_mitigation_imperative__systems_transition_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(cmi_str_tr_t7, climate_mitigation_imperative__systems_transition_reading, theater_ratio, 7, 0.31).
narrative_ontology:measurement(cmi_str_tr_t14, climate_mitigation_imperative__systems_transition_reading, theater_ratio, 14, 0.37).
narrative_ontology:measurement(cmi_str_tr_t21, climate_mitigation_imperative__systems_transition_reading, theater_ratio, 21, 0.4).
narrative_ontology:measurement(cmi_str_tr_t28, climate_mitigation_imperative__systems_transition_reading, theater_ratio, 28, 0.41).
narrative_ontology:measurement(cmi_str_tr_t35, climate_mitigation_imperative__systems_transition_reading, theater_ratio, 35, 0.42).

% Extraction over time
narrative_ontology:measurement(cmi_str_be_t0, climate_mitigation_imperative__systems_transition_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(cmi_str_be_t7, climate_mitigation_imperative__systems_transition_reading, base_extractiveness, 7, 0.42).
narrative_ontology:measurement(cmi_str_be_t14, climate_mitigation_imperative__systems_transition_reading, base_extractiveness, 14, 0.49).
narrative_ontology:measurement(cmi_str_be_t21, climate_mitigation_imperative__systems_transition_reading, base_extractiveness, 21, 0.54).
narrative_ontology:measurement(cmi_str_be_t28, climate_mitigation_imperative__systems_transition_reading, base_extractiveness, 28, 0.56).
narrative_ontology:measurement(cmi_str_be_t35, climate_mitigation_imperative__systems_transition_reading, base_extractiveness, 35, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(cmi_str_su_t0, climate_mitigation_imperative__systems_transition_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(cmi_str_su_t7, climate_mitigation_imperative__systems_transition_reading, suppression_requirement, 7, 0.52).
narrative_ontology:measurement(cmi_str_su_t14, climate_mitigation_imperative__systems_transition_reading, suppression_requirement, 14, 0.57).
narrative_ontology:measurement(cmi_str_su_t21, climate_mitigation_imperative__systems_transition_reading, suppression_requirement, 21, 0.6).
narrative_ontology:measurement(cmi_str_su_t28, climate_mitigation_imperative__systems_transition_reading, suppression_requirement, 28, 0.61).
narrative_ontology:measurement(cmi_str_su_t35, climate_mitigation_imperative__systems_transition_reading, suppression_requirement, 35, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_mitigation_imperative__systems_transition_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(climate_mitigation_imperative__systems_transition_reading, 0.08).
narrative_ontology:affects_constraint(climate_mitigation_imperative__systems_transition_reading, climate_mitigation_imperative__portfolio_optimization_reading).
narrative_ontology:affects_constraint(climate_mitigation_imperative__systems_transition_reading, climate_mitigation_imperative__opportunity_cost_reading).

% DUAL FORMULATION NOTE:
% This constraint is one member of the climate_mitigation_imperative constraint family. The three readings instantiate distinct constraints with different ε values, victim/beneficiary structures, and classifications. Portfolio optimization reading: ε ~0.25, claimed mountain/rope (nuclear as necessary baseload). Opportunity cost reading: ε ~0.45, claimed tangled_rope (nuclear as opportunity cost). Systems transition reading (this): ε ~0.58, claimed tangled_rope (nuclear as governance incompatibility). All three coordinate around carbon reduction but differ on what mitigation *is* and who bears its costs.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(climate_mitigation_imperative__systems_transition_reading, institutional, 0.45).
constraint_indexing:directionality_override(climate_mitigation_imperative__systems_transition_reading, powerless, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
