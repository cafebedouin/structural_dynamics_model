% ============================================================================
% CONSTRAINT STORY: climate_mitigation_imperative__systems_transition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   human_readable: Climate Mitigation via Decentralized Energy Transition (Systems Reading)
 *   domain: energy_policy/climate_mitigation/governance
 *
 * SUMMARY:
 *   This constraint instantiates the systems-transition reading of the
 *   climate-mitigation kernel: the position that energy mitigation requires
 *   not just carbon reduction but transformation of energy systems toward
 *   decentralization and democratic control. Under this reading, incumbent
 *   nuclear operators and centralized utility monopolies are victims (their
 *   extractive centralization is incompatible with the transition mandate),
 *   and distributed renewable operators and community energy coalitions are
 *   beneficiaries. The constraint is structurally a tangled rope: it
 *   coordinates a genuine problem (carbon reduction + governance legitimacy)
 *   AND asymmetrically extracts from centralized operators. The claim is
 *   independent of the metrics: the policy landscape genuinely instantiates
 *   this reading as one live position among contested siblings
 *   (portfolio-optimization and opportunity-cost readings); the metrics
 *   measure how extractive and suppressive the systems-transition framing is
 *   in operation.
 *
 * KEY AGENTS:
 *   - Distributed renewable operators (moderate-organized): benefit from policy tailwinds and subsidy structures; build the alternative to centralized generation
 *   - Incumbent nuclear operators (institutional): bear costs from decommissioning mandates and loss of regulatory privilege; constrained exit (capital locked in existing fleet)
 *   - Community energy coalitions (moderate-organized): benefit from decentralization mandates and local-governance frameworks; geographically dispersed but networked
 *   - Centralized utility monopolies (institutional): bear costs from grid modernization and interconnection obligations; constrained exit (rate-base model dependent on scale)
 *   - Climate policy makers (institutional-analytical): set and enforce the constraint through subsidy allocation, grid codes, and framing of what 'mitigation' means
 *   - Grid modernization advocates (organized): benefit from mandates for smart grid and storage technology; business model depends on decentralized architecture
 *   - Consumer advocates (moderate): beneficiary-payer: gain energy choice but potentially bear transition costs
 *   - Global South energy access (excluded, powerless): their voice is excluded from the mitigation policy table; rapid electrification via any low-carbon path conflicts with the governance mandate
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_mitigation_imperative__systems_transition_reading, 0.68).
domain_priors:suppression_score(climate_mitigation_imperative__systems_transition_reading, 0.61).
domain_priors:theater_ratio(climate_mitigation_imperative__systems_transition_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_mitigation_imperative__systems_transition_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(climate_mitigation_imperative__systems_transition_reading, suppression_requirement, 0.61).
narrative_ontology:constraint_metric(climate_mitigation_imperative__systems_transition_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_mitigation_imperative__systems_transition_reading, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(climate_mitigation_imperative__systems_transition_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_mitigation_imperative__systems_transition_reading, tangled_rope).
narrative_ontology:human_readable(climate_mitigation_imperative__systems_transition_reading, "Climate Mitigation via Decentralized Energy Transition (Systems Reading)").
narrative_ontology:topic_domain(climate_mitigation_imperative__systems_transition_reading, "energy_policy/climate_mitigation/governance").

domain_priors:requires_active_enforcement(climate_mitigation_imperative__systems_transition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_mitigation_imperative__systems_transition_reading, 'd861fb9c-8556-42d3-89dc-af92d706a5ad').
narrative_ontology:cs_kernel_codification('d861fb9c-8556-42d3-89dc-af92d706a5ad', distributed).
narrative_ontology:cs_authority_grounding('d861fb9c-8556-42d3-89dc-af92d706a5ad', extraction).
narrative_ontology:cs_interpretation_layer_present('d861fb9c-8556-42d3-89dc-af92d706a5ad').
narrative_ontology:cs_reading_relation('d861fb9c-8556-42d3-89dc-af92d706a5ad', climate_mitigation_imperative__portfolio_optimization_reading, coexists_with).
narrative_ontology:cs_reading_relation('d861fb9c-8556-42d3-89dc-af92d706a5ad', climate_mitigation_imperative__opportunity_cost_reading, influences).
narrative_ontology:cs_axiom('d861fb9c-8556-42d3-89dc-af92d706a5ad', foundational, energy_democracy_coequal_with_decarbonization).
narrative_ontology:cs_axiom_status(energy_democracy_coequal_with_decarbonization, holdable).
narrative_ontology:cs_axiom_grounding('d861fb9c-8556-42d3-89dc-af92d706a5ad', energy_democracy_coequal_with_decarbonization, deontological).
narrative_ontology:cs_axiom('d861fb9c-8556-42d3-89dc-af92d706a5ad', foundational, centralized_generation_extractive_by_structure).
narrative_ontology:cs_axiom_status(centralized_generation_extractive_by_structure, holdable).
narrative_ontology:cs_axiom_grounding('d861fb9c-8556-42d3-89dc-af92d706a5ad', centralized_generation_extractive_by_structure, conventional).
narrative_ontology:cs_reference_frame('d861fb9c-8556-42d3-89dc-af92d706a5ad', equitable_democratic_energy_governance).
narrative_ontology:cs_drift_state('d861fb9c-8556-42d3-89dc-af92d706a5ad', contemporary_monopoly_carbon_inertia, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('d861fb9c-8556-42d3-89dc-af92d706a5ad', '').
narrative_ontology:cs_kernel_id(climate_mitigation_imperative__systems_transition_reading, climate_mitigation_imperative).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__systems_transition_reading, distributed_renewable_operators).
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__systems_transition_reading, community_energy_coalitions).
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__systems_transition_reading, grid_modernization_advocates).
narrative_ontology:constraint_victim(climate_mitigation_imperative__systems_transition_reading, incumbent_nuclear_operators).
narrative_ontology:constraint_victim(climate_mitigation_imperative__systems_transition_reading, centralized_utility_monopolies).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__systems_transition_reading, consumer_advocates).
narrative_ontology:constraint_victim(climate_mitigation_imperative__systems_transition_reading, incumbent_coal_operators).
narrative_ontology:constraint_victim(climate_mitigation_imperative__systems_transition_reading, consumer_advocates).
narrative_ontology:constraint_vindicates(climate_mitigation_imperative__systems_transition_reading, democratic_energy_governance_doctrine).
narrative_ontology:constraint_vindicates(climate_mitigation_imperative__systems_transition_reading, decentralized_resilience_hypothesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Solar, wind, and storage operators and companies whose business models align with distributed ownership and community control. They benefit from policy frameworks that mandate grid modernization, prioritize distributed deployment, allocate subsidies to renewables, and establish interconnection standards favorable to small-scale generation. Their exit is mobile (they can relocate to different jurisdictions or pivot to other energy sectors) but their growth depends on policy continuation.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__systems_transition_reading, distributed_renewable_operators, beneficiary,
    organized, biographical, mobile, national).

% Large utilities and state-backed nuclear operators whose existing fleets, capital investments, and regulatory privileges depend on centralized baseload generation models. Under the systems-transition reading, their generation model is framed as incompatible with democratic energy transition. They face decommissioning mandates, loss of rate-base assumptions, erosion of regulatory privilege, and stranded capital as policy shifts toward distributed alternatives. Their exit is constrained by sunk capital and the long timelines of nuclear decommissioning.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__systems_transition_reading, incumbent_nuclear_operators, payer,
    institutional, civilizational, constrained, national).

% Local and regional coalitions seeking energy autonomy and democratic ownership of energy generation. They advocate for microgrids, cooperative ownership structures, community choice aggregation, and local energy sovereignty. They benefit from policy frameworks that permit and incentivize community-scale deployment, permit cooperative ownership, and devolve decision-making authority to local entities. Their exit is mobile (they can exit communities or shift organizational form) but their influence depends on policy support.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__systems_transition_reading, community_energy_coalitions, beneficiary,
    moderate, generational, mobile, local).

% Regulated utility monopolies structured around centralized generation and transmission control. They bear costs from grid modernization requirements, mandatory interconnection of distributed resources, open-access obligations, and erosion of the regulatory model that assumed centralized capital intensity and concentrated control. Their exit is constrained by regulatory relationships, stranded assets, and the century-long infrastructure sunk in centralized models.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__systems_transition_reading, centralized_utility_monopolies, payer,
    institutional, civilizational, constrained, national).

% Government bodies, international agencies, and regulatory authorities setting and enforcing climate mitigation policy. Under the systems-transition reading, their mandate includes both carbon reduction and democratic transformation of energy systems. They enforce the constraint through subsidy allocation, grid codes, interconnection requirements, generation mix targets, and the rhetorical framing of what 'legitimate' mitigation includes. They have institutional power to set the agenda but analytical standing (they interpret what the climate science and energy-justice principles require).
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__systems_transition_reading, climate_policy_makers, agenda_setter,
    institutional, generational, analytical, national).

% Companies, think tanks, researchers, and technologists advocating for smart grid, distributed storage, demand response, and distributed control systems. They benefit from policy mandates requiring grid modernization and from the capital flows supporting distributed architecture (hardware, software, integration services). Their business models depend on decentralized architecture becoming the dominant paradigm. Their exit is mobile (they can serve different markets) but their growth depends on the regulatory direction continuing.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__systems_transition_reading, grid_modernization_advocates, beneficiary,
    organized, biographical, mobile, national).

% Coal-fired generation operators and coal-mining companies. They are not the primary target under the systems-transition reading (the primary target is the governance structure, not coal vs. renewables), but they are collateral payers: they depend on centralized generation models and simultaneously face pressure from carbon reduction AND from the decentralization mandate. Their exit is trapped (capital locked in coal assets, labor dependent on mines, capital immobility). They bear costs from both the carbon constraint and the governance constraint.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__systems_transition_reading, incumbent_coal_operators, payer,
    institutional, civilizational, trapped, national).

% Advocates for affordable energy access, consumer protection, and energy justice. They benefit from distributed energy options that offer price competition, consumer choice, and local control (beneficiary direction). They bear costs if grid modernization and transition infrastructure increase electricity prices during the buildout phase, or if new technologies impose costs on ratepayers (payer direction). Their interests are genuinely mixed; their exit is constrained by the essential nature of electricity.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__systems_transition_reading, consumer_advocates, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(climate_mitigation_imperative__systems_transition_reading, consumer_advocates, payer).

% Oil, gas, and coal companies and their lobbying associations. They are structurally excluded from the climate-mitigation-policy conversation (carbon constraint is non-negotiable under this reading). They have no formal seat at the table in energy-transition policy but maintain substantial informal influence through lobbying, political contributions, and capture of regulatory bodies. Their voice matters in shaping the timeline and pace of policy implementation even though they cannot argue for continued fossil fuels at the policy table.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__systems_transition_reading, excluded_fossil_fuel_interests, excluded,
    institutional, civilizational, trapped, global).

% Billions of people without reliable electricity access, predominantly in the Global South. Their interests in rapid electrification via any available low-carbon path (including nuclear or centralized hydro) compete with the systems-transition reading's mandate for decentralized, democratically controlled energy. They have minimal voice in climate policy design despite bearing the most material consequences of delayed electrification. Their exit is trapped (no alternative energy access path available; no voice in policy process).
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__systems_transition_reading, global_south_energy_access, excluded,
    powerless, generational, trapped, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_mitigation_imperative__systems_transition_reading, climate_policy_makers).
narrative_ontology:fixing_cost_class(climate_mitigation_imperative__systems_transition_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Combines two coordination problems: (1) carbon emissions require rapid phase-out of fossil fuels (technical climate problem); (2) centralized energy monopolies have historically extracted rents and excluded communities from control over energy systems (governance legitimacy problem). The constraint attempts to solve both simultaneously by mandating decentralization as the vehicle for decarbonization — distributed renewables are positioned as the solution to both problems at once.
% TRANSFER_FUNCTION: Moves regulatory favor, subsidy flows, grid investment capital, and decision-making authority from incumbent nuclear and utility operators toward distributed renewable operators, community coalitions, and grid modernization advocates. Simultaneously transfers control over energy infrastructure from centralized corporate entities toward communities and smaller operators.
% ABSENT_VOICES: Incumbent nuclear operators have minimal voice in the systems-transition framing (they are the problem to be overcome, not a party to be consulted). Fossil fuel companies are categorically excluded. Global South energy-access advocates are structurally excluded from the mitigation policy table — their voice that rapid electrification (via any low-carbon path) is a development priority would challenge the governance mandate's universality, but they have no seat in the policy conversation.
% DISAPPEARANCE_RATIONALE: If the systems-transition constraint vanished, climate mitigation policy would revert to purely carbon-minimization metrics (portfolio optimization without governance mandates). Incumbent utilities and nuclear operators would recover regulatory privilege and subsidy flows. Distributed renewable operators and community coalitions would lose policy tailwinds and momentum. The energy system would still decarbonize but governance would stay centralized and monopolistic rather than democratizing. The constraint's absence removes the governance-transformation component from mitigation, leaving only carbon reduction.
% FOUNDING_PROBLEM: Twofold founding problem: (1) atmospheric carbon requires rapid phase-out of fossil fuels and transition to low-carbon energy; (2) historical centralization of energy systems has created extractive monopolies that capture regulatory bodies, concentrate wealth and control, and exclude communities from decision-making over infrastructure that shapes their lives. The constraint's founding mandate is to solve both problems simultaneously — decarbonize while democratizing.
% FOUNDING_PROBLEM_CORROBORATION: Climate scientists and energy analysts attest the carbon problem is live and urgent (IPCC, national climate assessments). Energy-democracy advocates, community organizers, and some local government bodies attest the governance problem is live (structural exclusion of communities from energy decisions, monopoly control of supply, concentrated wealth in utility profits). Incumbent utilities and portfolio-optimization energy analysts attest that carbon urgency should override governance preferences; they argue the founding problem is carbon only, governance is a separate issue. Opportunity-cost analysts attest that speed-to-deployment should dominate both carbon and governance. Attestation is divided between the reading's beneficiaries (carbon + governance are co-primary problems) and those reading the kernel differently.
narrative_ontology:disappearance_verdict(climate_mitigation_imperative__systems_transition_reading, world_rearranges).
narrative_ontology:founding_problem_status(climate_mitigation_imperative__systems_transition_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_mitigation_imperative__systems_transition_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(climate_mitigation_imperative__systems_transition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(climate_mitigation_imperative__systems_transition_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is substantial (0.68) because the constraint asymmetrically burdens incumbent operators while favoring distributed alternatives — the burden is not just cost (decommissioning) but loss of regulatory privilege and control. Suppression is moderate (0.61) because the constraint persists through policy enforcement (subsidy allocation, grid codes, interconnection mandates) and through the framing of 'legitimate' mitigation, not through coercion of individual actors. Theater is elevated (0.42) because part of the enforcement energy goes to maintaining the narrative that nuclear is incompatible with democracy (a live contest, not settled fact), while part genuinely drives deployment of alternatives. The measurement series show extractiveness and suppression rising through the first 20 time points as policy enforcement intensifies (observed: 2010–2022), then plateauing (projected: 2025–2040) as the outcome stabilizes. Theater ratio rising slower but to the same plateau suggests the narrative-enforcement component is real but smaller than the structural enforcement (subsidy flows, grid codes).
 *
 * PERSPECTIVAL GAP:
 *   The constraint should compute differently from different seats: (1) From a distributed-renewable-operator seat (beneficiary + moderate power): the constraint is genuine coordination (carbon + governance both solved) with acceptable extraction flowing to policy makers and grid operators. Directionality near beneficiary (d low); effective extraction inverted into subsidy/opportunity. (2) From a nuclear-operator seat (payer + institutional power): the constraint is extraction masked as coordination — the governance mandate is pretextual, the real aim is to shut down nuclear competitors. Directionality near target (d high); effective extraction substantial. (3) From a climate-analyst seat (observer): the constraint is misguided because it privileges governance preferences over carbon speed — opportunity cost of delaying nuclear is higher climate risk. Type might classify as snare (governance extracted as cover story for distributed-renewable favoritism) or rope (if the governance objective is endorsed). The engine computes these divergences from the structural data; the commentary explains why they are real.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (distributed renewables, community coalitions, grid modernization advocates) have moderate-to-organized power and mobile-to-mobile exit: they can grow in the favorable policy environment but depend on policy continuation for scaling. Directionality near beneficiary end (d ≈ 0.2–0.3). Victims (nuclear and utility operators) have institutional power but constrained exit: they can relocate capital slowly and lobby politically, but their core business model is locked in. Directionality near target end (d ≈ 0.75–0.85). Policy makers have analytical standing and control the enforcement machinery: they are neither beneficiary nor victim but agenda-setter (role: agenda_setter, not a directionality seat). Consumer advocates are split (beneficiary + payer): energy choice (beneficiary direction) vs. transition costs (payer direction) offset — d ≈ 0.5. Global South energy access is trapped (powerless, no voice in the table, energy needs compete with governance preferences): directionality highest (d ≈ 0.95) if their exclusion from the mitigation framework itself becomes a cost they bear.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandate ('energy mitigation through democratic transition') is live (not dead) but contested. Some parties attest it is live and primary (energy-democracy advocates), others attest it is secondary to carbon speed (opportunity-cost readers), others attest it is pretextual (nuclear operators). The disappearance verdict is world_rearranges because if the governance mandate vanished, policy would revert to purely carbon optimization and incumbent operators would recover privilege. Theater-ratio measurement (0.42) reflects that governance narrative is real and enforced but not the sole enforcement mechanism — subsidy flows and grid codes are the hard enforcement. The constraint is not a piton (atrophied function): the governance transformation is an active, contested project, not theater masking a dead function. It is a tangled rope because it genuinely coordinates (carbon + governance) but the coordination is unequal (beneficiaries gain governance + carbon reduction; payers bear costs of both while losing control).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest_systems_vs_portfolio,
    'Is energy transition fundamentally a governance transformation (systems reading) or fundamentally a carbon-minimization optimization problem (portfolio reading)?',
    'This is a CONCEPTUAL omega that cannot be empirically resolved: it depends on what normative question the constraint is anchored to. The systems reading privileges democratic legitimacy as a primary goal co-equal with carbon reduction. The portfolio reading privileges carbon reduction as primary and governance as a secondary design parameter. No observation resolves which framing is more important.',
    'If the systems reading''s framing is authoritative, nuclear is victim-class (incompatible with democratic transition). If the portfolio reading''s framing is authoritative, nuclear is a carbon-minimization tool and beneficiary-adjacent. The classification diverges at the constraint definition level.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contest_systems_vs_portfolio, conceptual, 'Whether energy transition is framed as governance transformation or carbon optimization').

omega_variable(
    nuclear_centralization_necessity,
    'Is centralized generation (nuclear or otherwise) structurally necessary for reliable baseload supply, or can distributed systems with storage and demand response provide equivalent reliability at lower scale?',
    'Empirical: grid modeling and real-world trials of high-renewable, low-centralized-generation systems in comparable jurisdictions. Technical feasibility studies of storage and demand-management scaling.',
    'If centralization is necessary, nuclear becomes a genuine coordination good and the extraction framing weakens (victim status is undercut). If distributed systems can reliably replace centralized baseload, nuclear becomes pure extraction riding on a false coordination claim (victim status confirmed). The measurement distinguishes genuine tangled rope from snare misclassified as rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(nuclear_centralization_necessity, empirical, 'Whether centralized generation is structurally necessary for grid reliability').

omega_variable(
    democratic_control_operationalization,
    'What constitutes ''democratic control'' of energy systems operationally? Is it majority ownership, community governance boards, participatory budgeting, or open market entry for local actors?',
    'Definitional and comparative: jurisdictions with different operational models of ''democratic control'' (municipal utilities, energy cooperatives, community choice aggregation, open-access markets, regulated monopolies with stakeholder boards) can be compared on governance outcomes and energy delivery performance.',
    'Vague operationalization of ''democratic control'' leaves the constraint''s enforcement mechanisms contested: what counts as winning and what counts as enforcement is ambiguous. Tighter operationalization enables clearer beneficiary/victim classification and clearer measurement of enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(democratic_control_operationalization, conceptual, 'What operational definition of democratic energy control grounds the constraint').

omega_variable(
    global_south_energy_justice_conflict,
    'How does the systems-transition reading''s governance mandate apply in contexts where rapid electrification (potentially via nuclear or centralized generation) is necessary for development, equity, and poverty reduction?',
    'This is primarily a PREFERENCE omega (values-based): the constraint prioritizes wealthy-country energy democracy; Global South actors prioritize rapid access-at-any-governance-structure. The conflict is real and structural, not a measurement or factual dispute. No empirical resolution exists.',
    'If the Global South''s energy-access priority is authoritative, the constraint''s application becomes conditional on development context. If the systems-transition reading is universalized regardless of context, it becomes a form of imperialism (wealthy countries dictating governance preferences while denying developing economies the centralized deployment paths those countries used historically). The classification impact is on scope and victim characterization.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(global_south_energy_justice_conflict, preference, 'Whether the systems-transition mandate is universal or conditional on development context').

omega_variable(
    sibling_reading_relationship_portfolio_optimization,
    'How does this constraint''s systems-governance mandate relate to the portfolio-optimization reading''s carbon-minimization mandate? Do they coexist as live policy options or does one foreclose the other?',
    'This is the COMMITTER STRUCTURE omega (kernel-reading dynamics): the two readings are held by different policy coalitions (climate scientists vs. energy-systems analysts vs. energy-democracy advocates). No single framework holds both simultaneously in the current policy landscape. They coexist in the real world via institutional separation (different jurisdictions, different agencies, different time horizons) but in any single energy system, at any single time, choosing one reading shapes the outcomes of the other.',
    'Structural: the kernel (climate mitigation) is read differently depending on which coalition frames the problem. The constraint definitions are different because the STAKES are different: one stakes are carbon, the other stakes are control. The engine''s per-seat classification should reflect this: from a distributed-renewable operator''s seat, the systems reading is rope/coordination; from a utility-monopoly seat, it is snare/extraction. From a carbon-focused energy analyst seat, the reading might classify differently altogether.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_relationship_portfolio_optimization, conceptual, 'Kernel contest: systems-transition vs. portfolio-optimization vs. opportunity-cost readings of climate mitigation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_mitigation_imperative__systems_transition_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_mitigation_imperative__systems_transition_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(clim_tr_t0, observed).
narrative_ontology:measurement(clim_tr_t5, climate_mitigation_imperative__systems_transition_reading, theater_ratio, 5, 0.3).
narrative_ontology:measurement_basis(clim_tr_t5, observed).
narrative_ontology:measurement(clim_tr_t10, climate_mitigation_imperative__systems_transition_reading, theater_ratio, 10, 0.35).
narrative_ontology:measurement_basis(clim_tr_t10, observed).
narrative_ontology:measurement(clim_tr_t15, climate_mitigation_imperative__systems_transition_reading, theater_ratio, 15, 0.4).
narrative_ontology:measurement_basis(clim_tr_t15, observed).
narrative_ontology:measurement(clim_tr_t20, climate_mitigation_imperative__systems_transition_reading, theater_ratio, 20, 0.42).
narrative_ontology:measurement_basis(clim_tr_t20, observed).
narrative_ontology:measurement(clim_tr_t25, climate_mitigation_imperative__systems_transition_reading, theater_ratio, 25, 0.42).
narrative_ontology:measurement_basis(clim_tr_t25, projected).
narrative_ontology:measurement(clim_tr_t30, climate_mitigation_imperative__systems_transition_reading, theater_ratio, 30, 0.42).
narrative_ontology:measurement_basis(clim_tr_t30, projected).
narrative_ontology:measurement(clim_tr_t40, climate_mitigation_imperative__systems_transition_reading, theater_ratio, 40, 0.42).
narrative_ontology:measurement_basis(clim_tr_t40, projected).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_mitigation_imperative__systems_transition_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(clim_be_t0, observed).
narrative_ontology:measurement(clim_be_t5, climate_mitigation_imperative__systems_transition_reading, base_extractiveness, 5, 0.52).
narrative_ontology:measurement_basis(clim_be_t5, observed).
narrative_ontology:measurement(clim_be_t10, climate_mitigation_imperative__systems_transition_reading, base_extractiveness, 10, 0.58).
narrative_ontology:measurement_basis(clim_be_t10, observed).
narrative_ontology:measurement(clim_be_t15, climate_mitigation_imperative__systems_transition_reading, base_extractiveness, 15, 0.63).
narrative_ontology:measurement_basis(clim_be_t15, observed).
narrative_ontology:measurement(clim_be_t20, climate_mitigation_imperative__systems_transition_reading, base_extractiveness, 20, 0.66).
narrative_ontology:measurement_basis(clim_be_t20, observed).
narrative_ontology:measurement(clim_be_t25, climate_mitigation_imperative__systems_transition_reading, base_extractiveness, 25, 0.68).
narrative_ontology:measurement_basis(clim_be_t25, projected).
narrative_ontology:measurement(clim_be_t30, climate_mitigation_imperative__systems_transition_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement_basis(clim_be_t30, projected).
narrative_ontology:measurement(clim_be_t40, climate_mitigation_imperative__systems_transition_reading, base_extractiveness, 40, 0.68).
narrative_ontology:measurement_basis(clim_be_t40, projected).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_mitigation_imperative__systems_transition_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(clim_su_t0, observed).
narrative_ontology:measurement(clim_su_t5, climate_mitigation_imperative__systems_transition_reading, suppression_requirement, 5, 0.42).
narrative_ontology:measurement_basis(clim_su_t5, observed).
narrative_ontology:measurement(clim_su_t10, climate_mitigation_imperative__systems_transition_reading, suppression_requirement, 10, 0.5).
narrative_ontology:measurement_basis(clim_su_t10, observed).
narrative_ontology:measurement(clim_su_t15, climate_mitigation_imperative__systems_transition_reading, suppression_requirement, 15, 0.56).
narrative_ontology:measurement_basis(clim_su_t15, observed).
narrative_ontology:measurement(clim_su_t20, climate_mitigation_imperative__systems_transition_reading, suppression_requirement, 20, 0.6).
narrative_ontology:measurement_basis(clim_su_t20, observed).
narrative_ontology:measurement(clim_su_t25, climate_mitigation_imperative__systems_transition_reading, suppression_requirement, 25, 0.61).
narrative_ontology:measurement_basis(clim_su_t25, projected).
narrative_ontology:measurement(clim_su_t30, climate_mitigation_imperative__systems_transition_reading, suppression_requirement, 30, 0.61).
narrative_ontology:measurement_basis(clim_su_t30, projected).
narrative_ontology:measurement(clim_su_t40, climate_mitigation_imperative__systems_transition_reading, suppression_requirement, 40, 0.61).
narrative_ontology:measurement_basis(clim_su_t40, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_mitigation_imperative__systems_transition_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(climate_mitigation_imperative__systems_transition_reading, 0.18).
narrative_ontology:affects_constraint(climate_mitigation_imperative__systems_transition_reading, climate_mitigation_imperative__portfolio_optimization_reading).
narrative_ontology:affects_constraint(climate_mitigation_imperative__systems_transition_reading, climate_mitigation_imperative__opportunity_cost_reading).
narrative_ontology:affects_constraint(climate_mitigation_imperative__systems_transition_reading, nuclear_energy_governance_control).
narrative_ontology:affects_constraint(climate_mitigation_imperative__systems_transition_reading, distributed_energy_decentralization).

% DUAL FORMULATION NOTE:
% This constraint is part of the climate-mitigation-imperative kernel family. The kernel itself is a fixed commitment (reduce emissions + transform energy systems); different readings instantiate different constraints based on what 'transform' means. The systems-transition reading is one reading of this kernel. The portfolio-optimization and opportunity-cost readings are sibling constraints instantiating different normative priorities applied to the same kernel. All three share the carbon-reduction core; they diverge on what transformation means and who counts as a victim. See the kernel_context field in commentary for the full contest structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(climate_mitigation_imperative__systems_transition_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
