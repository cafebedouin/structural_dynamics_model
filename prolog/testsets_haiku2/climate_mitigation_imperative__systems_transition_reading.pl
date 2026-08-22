% ============================================================================
% CONSTRAINT STORY: climate_mitigation_imperative__systems_transition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
 *   human_readable: Climate Mitigation as Democratic Energy System Transition
 *   domain: energy_policy/climate_governance/technology_systems
 *
 * SUMMARY:
 *   The systems-transition reading frames climate mitigation as requiring not
 *   only carbon reduction but fundamental restructuring of energy governance
 *   toward decentralization and democratic control. Under this reading,
 *   nuclear power becomes incompatible with mitigation—not because of carbon
 *   accounting (where it scores well) but because it is structurally
 *   centralized, requires top-down capital deployment, and concentrates
 *   operational control in incumbent utilities. Distributed renewables,
 *   community energy cooperatives, and participatory governance become the
 *   beneficiary set. The constraint is thus a governance structure constraint
 *   masquerading as a carbon constraint—its real function is to redirect
 *   mitigation pathways toward systems that redistribute energy power. The
 *   constraint exhibits substantial extraction (incumbent utilities and
 *   nuclear operators are forcibly repositioned as obstacles rather than
 *   solutions) and moderate suppression (alternative energy pathways and
 *   voices prioritizing cost or speed are downplayed). Theater is rising: as
 *   the reading gains policy traction, more resources flow into performative
 *   'democratic energy' framing that does not deliver actual governance
 *   change.
 *
 * KEY AGENTS:
 *   - Distributed renewable developers (beneficiary, mobile, organized power — can exit to friendlier jurisdictions but gain from policy shift)
 *   - Community energy cooperatives (beneficiary, constrained power — depend on policy support but carry limited alternatives)
 *   - Energy democracy advocates (beneficiary, moderate power — move the framing forward but lack implementation authority)
 *   - Incumbent nuclear operators (victim, institutional power — trapped in regulatory and capital structures, lose policy legitimacy)
 *   - Centralized grid utilities (victim, institutional power — operations depend on centralized dispatch, face forced decentralization)
 *   - Fossil fuel workers in nuclear supply chains (victim, powerless, trapped — bear displacement costs from both fossil and nuclear phase-out)
 *   - Regulatory bodies (agenda setter, institutional power — operationalize the constraint but carry expanded mandate)
 *   - Global South energy access populations (excluded, powerless — deprioritized by governance focus, need electricity now)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_mitigation_imperative__systems_transition_reading, 0.68).
domain_priors:suppression_score(climate_mitigation_imperative__systems_transition_reading, 0.52).
domain_priors:theater_ratio(climate_mitigation_imperative__systems_transition_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_mitigation_imperative__systems_transition_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(climate_mitigation_imperative__systems_transition_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(climate_mitigation_imperative__systems_transition_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_mitigation_imperative__systems_transition_reading, accessibility_collapse, 0.64).
narrative_ontology:constraint_metric(climate_mitigation_imperative__systems_transition_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_mitigation_imperative__systems_transition_reading, tangled_rope).
narrative_ontology:human_readable(climate_mitigation_imperative__systems_transition_reading, "Climate Mitigation as Democratic Energy System Transition").
narrative_ontology:topic_domain(climate_mitigation_imperative__systems_transition_reading, "energy_policy/climate_governance/technology_systems").

domain_priors:requires_active_enforcement(climate_mitigation_imperative__systems_transition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_mitigation_imperative__systems_transition_reading, '153c87c0-b4b7-43be-b872-ad8d79ae0694').
narrative_ontology:cs_kernel_codification('153c87c0-b4b7-43be-b872-ad8d79ae0694', distributed).
narrative_ontology:cs_authority_grounding('153c87c0-b4b7-43be-b872-ad8d79ae0694', distributed).
narrative_ontology:cs_reading_relation('153c87c0-b4b7-43be-b872-ad8d79ae0694', climate_mitigation_imperative__portfolio_optimization_reading, coexists_with).
narrative_ontology:cs_reading_relation('153c87c0-b4b7-43be-b872-ad8d79ae0694', climate_mitigation_imperative__opportunity_cost_reading, influences).
narrative_ontology:cs_axiom('153c87c0-b4b7-43be-b872-ad8d79ae0694', foundational, democratic_energy_governance_structural_requirement).
narrative_ontology:cs_axiom_status(democratic_energy_governance_structural_requirement, holdable).
narrative_ontology:cs_axiom_grounding('153c87c0-b4b7-43be-b872-ad8d79ae0694', democratic_energy_governance_structural_requirement, deontological).
narrative_ontology:cs_axiom('153c87c0-b4b7-43be-b872-ad8d79ae0694', foundational, decentralization_energy_system_necessity).
narrative_ontology:cs_axiom_status(decentralization_energy_system_necessity, holdable).
narrative_ontology:cs_axiom_grounding('153c87c0-b4b7-43be-b872-ad8d79ae0694', decentralization_energy_system_necessity, deontological).
narrative_ontology:cs_axiom('153c87c0-b4b7-43be-b872-ad8d79ae0694', secondary, incumbent_centralization_incompatible_with_mitigation).
narrative_ontology:cs_axiom_status(incumbent_centralization_incompatible_with_mitigation, holdable).
narrative_ontology:cs_axiom_grounding('153c87c0-b4b7-43be-b872-ad8d79ae0694', incumbent_centralization_incompatible_with_mitigation, instrumental).
narrative_ontology:cs_reference_frame('153c87c0-b4b7-43be-b872-ad8d79ae0694', energy_systems_democratically_accountable_to_communities).
narrative_ontology:cs_drift_state('153c87c0-b4b7-43be-b872-ad8d79ae0694', contemporary_2020s_energy_policy, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('153c87c0-b4b7-43be-b872-ad8d79ae0694', '').
narrative_ontology:cs_kernel_id(climate_mitigation_imperative__systems_transition_reading, climate_mitigation_imperative).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__systems_transition_reading, distributed_renewable_developers).
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__systems_transition_reading, community_energy_cooperatives).
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__systems_transition_reading, energy_democracy_advocates).
narrative_ontology:constraint_victim(climate_mitigation_imperative__systems_transition_reading, incumbent_nuclear_operators).
narrative_ontology:constraint_victim(climate_mitigation_imperative__systems_transition_reading, centralized_grid_utilities).
narrative_ontology:constraint_victim(climate_mitigation_imperative__systems_transition_reading, fossil_fuel_workers_in_nuclear_supply_chains).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Develop solar, wind, and storage systems in modular, community-controllable configurations. Benefit from policy prioritizing decentralization and democratic ownership. Can relocate to more favorable jurisdictions if policy shifts but gain competitive advantage and institutional legitimacy when systems-transition framing dominates. Their technology pathway and business models align with the constraint's beneficiary position.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__systems_transition_reading, distributed_renewable_developers, beneficiary,
    organized, generational, mobile, national).

% Own and operate renewable systems with participatory governance. Depend on policy support for financing and regulatory approval. Cannot easily relocate; exit means dissolution. Benefit directly when policy centers democratic control as a mitigation criterion. Carry lived experience of community energy governance.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__systems_transition_reading, community_energy_cooperatives, beneficiary,
    moderate, generational, constrained, regional).

% Civil society organizations, movement leaders, academics, and advocates pushing for energy system decentralization and democratic control. Frame mitigation as inseparable from power redistribution. Move policy discourse and frame the constraint's core narrative. Can work across jurisdictions but depend on policy adoption for influence.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__systems_transition_reading, energy_democracy_advocates, beneficiary,
    moderate, biographical, mobile, global).

% Operate centralized nuclear systems; face classification as obstacle to mitigation under this reading. Experience stranded capital when policy prioritizes decentralization. Cannot easily exit (nuclear plants operate 40+ years); cannot adapt without fundamental business restructuring. Exit means technology and asset obsolescence.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__systems_transition_reading, incumbent_nuclear_operators, payer,
    institutional, civilizational, constrained, global).

% Own and operate centralized transmission and distribution grids; locked into business models dependent on centralized dispatch and operational control. The constraint requires decentralization of authority and participatory governance—a fundamental reshaping of their institutional role. Exit means technology obsolescence and loss of regulated utility status.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__systems_transition_reading, centralized_grid_utilities, payer,
    institutional, civilizational, constrained, national).

% Employment in uranium extraction, nuclear construction, fuel processing, and related supply-chain roles. Face displacement from both fossil fuel phase-out AND nuclear drawdown (under this reading). Carry minimal voice in energy mitigation debates; their material interest (jobs, community stability) conflicts with both low-carbon pathways. No exit: workers are geographically and skill-locked.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__systems_transition_reading, fossil_fuel_workers_in_nuclear_supply_chains, payer,
    powerless, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(climate_mitigation_imperative__systems_transition_reading, fossil_fuel_workers_in_nuclear_supply_chains, excluded).

% Government agencies tasked with climate mitigation; must operationalize the systems-transition reading into policy and regulation. Face expanded mandate: not just carbon accounting but governance criteria. Must define and enforce 'democratic control' and 'decentralization' in energy planning—substantially more complex than carbon-per-megawatt optimization. Exit is political (voters/legislatures may overturn the mandate).
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__systems_transition_reading, regulatory_bodies_with_carbon_mandate, agenda_setter,
    institutional, generational, analytical, national).

% Multinational energy companies with capital and technology to deploy large-scale centralized or hybrid solutions globally. Excluded from decision-making authority when the constraint prioritizes local democratic control. Can adapt by working with local partners but lose operational control and profit margins. Their voice is strategically marginalized by localism emphasis.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__systems_transition_reading, global_north_utilities_and_developers, excluded,
    institutional, generational, constrained, global).

% Billions with minimal or no reliable electricity access; climate mitigation deadlines and energy poverty are equally urgent. The systems-transition reading prioritizes governance transformation over speed and cost-minimization—potentially delaying their access to reliable power. Their survival needs are orthogonal to both governance and carbon-optimization frames. Voice structurally absent from energy policy debates between incumbent and renewable developers.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__systems_transition_reading, global_south_energy_access_populations, excluded,
    powerless, immediate, trapped, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(climate_mitigation_imperative__systems_transition_reading, distributed_renewable_developers).
narrative_ontology:fixing_cost_class(climate_mitigation_imperative__systems_transition_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Organizes climate mitigation strategy around energy system decentralization and participatory democratic governance. Solves the coordination problem of aligning energy infrastructure transformation with redistribution of decision-making power—moving from top-down utility planning to community-controlled, democratically-accountable systems. Coordinates beneficiary stakeholders (renewable developers, cooperatives, advocates) around a shared vision of what low-carbon energy should be.
% TRANSFER_FUNCTION: Transfers operational authority and capital flows from centralized utilities and incumbent nuclear operators to distributed renewable developers and community energy cooperatives. Transfers regulatory legitimacy from technical carbon-optimization to governance-inclusive mitigation criteria. Also transfers exclusion: developers and utilities unable or unwilling to adopt democratic governance structures are reclassified as obstacles rather than solution partners.
% ABSENT_VOICES: Fossil fuel and nuclear supply-chain workers whose interests are not represented in either the incumbent utility or renewable developer seats. Global South populations prioritizing energy access and speed over governance transformation. Technical climate scientists and energy economists focused on carbon-per-dollar optimization who would argue the constraint over-weights governance. Nuclear operators' workers and contractors with decades of expertise in large-scale power systems.
% DISAPPEARANCE_RATIONALE: If this constraint disappeared, energy mitigation would revert to carbon-only optimization criteria and allow centralized, incumbent-operator-led pathways. Incumbent utilities and nuclear operators would expand without democratic-governance requirements; decentralized renewables would lose policy priority and local participation mandates; energy decision-making would concentrate rather than distribute. The energy system would decarbonize (possibly faster) but would maintain or strengthen existing power concentrations—a rearrangement into a different governance structure with different beneficiaries and payers.
% FOUNDING_PROBLEM: Energy systems have historically been built by centralized utilities and governments with minimal democratic input from affected communities. Communities bear environmental and health costs without controlling energy decisions. Climate mitigation offers a historic opportunity to rebuild energy systems with decentralization and participatory governance as core requirements, not afterthoughts—to solve climate change while simultaneously democratizing energy power.
% FOUNDING_PROBLEM_CORROBORATION: Community energy cooperatives, Indigenous land defenders, environmental justice advocates, and Global South climate activists attest the founding problem is live and foundational to mitigation. Technical climate scientists and energy economists do not deny the governance problem exists but dispute whether it is necessary to solve simultaneously with carbon reduction. Independent sources (labor unions focused on just transition, Indigenous rights organizations, energy justice movement scholars) confirm the governance dimension as substantively contested and important. The strongest corroboration comes from outside the beneficiary seats: Indigenous communities whose energy systems were forcibly centralized by colonial and post-colonial states; labor movements defending worker voice in energy transitions; Global South nations asserting energy sovereignty against externally-imposed solutions.
narrative_ontology:disappearance_verdict(climate_mitigation_imperative__systems_transition_reading, world_rearranges).
narrative_ontology:founding_problem_status(climate_mitigation_imperative__systems_transition_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_mitigation_imperative__systems_transition_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
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
 *   Extractiveness is high (0.68 at end of interval) because the constraint systematically reclassifies incumbent energy institutions from solution-partners to obstacles, forcing them to surrender governance authority and revenue models. This is not a market outcome (prices or efficiency) but a structural redefinition of who counts as legitimate in mitigation. Suppression is moderate (0.52) because the constraint's enforcement depends on policy adoption and narrative legitimacy rather than direct coercion—utilities cannot be prevented from operating, but policy can starve them of expansion capital and authority. Theater rises from 0.28 to 0.41 over the interval because policy frameworks increasingly mouth 'democratic energy' language while maintaining centralized utility dominance (a substitution of narrative for actual governance change). Accessibility collapse is moderate-high (0.64): once the systems-transition framing takes hold, alternatives rooted in carbon-only optimization or cost-minimization appear illegitimate (collapsed accessibility), yet technical experts and Global South actors still resist it (incomplete collapse—0.64 not 0.85+). Resistance is high (0.72): nuclear operators, utilities, and cost-optimization advocates mount sustained challenge; incumbent institutions control media, technical standards, and regulatory capture—this is not a stable coordination but an active fight. The measurement series captures the constraint's initial adoption phase (rising extractiveness and suppression as policy adoption spreads) plateauing at 0.68/0.52, with theater continuing to rise—diagnostic of a constraint that is winning narrative but not yet implementing governance change.
 *
 * PERSPECTIVAL GAP:
 *   The gap is a structural fight over what counts as mitigation. Carbon-only technical optimizers and nuclear operators see the constraint as illegitimate expansion of criteria beyond the founding problem (climate). Systems-transition advocates and energy democracy movements see the constraint as finally centering the governance dimension that carbon-only optimization suppresses. No neutral framing exists; the gap is the constraint itself—which side's definition of mitigation becomes policy.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from beneficiary/victim positioning and exit options. Distributed renewables and cooperatives gain institutional legitimacy and policy capital (d near 0.0, full beneficiary); incumbent nuclear and utilities lose authority and face stranded capital (d near 1.0, full target). Energy democracy advocates move policy but do not capture extraction (secondary beneficiaries, d ~0.2). Regulatory bodies carry symmetric access to both sides' evidence (d ~0.5). Global South access populations are victimized by deprioritization (d ~0.8) but wield no structural power to change it (power atom: powerless). Fossil fuel workers in nuclear supply chains are targets of both fossil and nuclear drawdown with minimal voice (d ~0.85, trapped, no secondary role). This heterogeneous d distribution is the fingerprint of a tangled-rope constraint: genuine coordination (energy system transformation) mixed with asymmetric extraction (institutional power transfer without compensation or transitional justice).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is clear: energy systems were built without democratic input and concentrate power in incumbent institutions. The constraint addresses it directly: require democratic control. BUT: global carbon mitigation has a hard deadline (2050 or climate tipping); democratic transformation has no natural deadline. The founding problem (historical injustice in energy governance) and the pressing problem (carbon reduction within decades) create a mandatrophy trap: prioritizing governance transformation may slow carbon reduction below emergency-level requirements, but ignoring governance perpetuates the power structures the constraint identifies as problematic. The constraint exhibits tangled-rope precisely because it coordinates on one problem (governance) while extracting from another (carbon optimization). The classification resists mandatrophy by naming the structure explicitly: this is coordination FOR democratic energy PLUS extraction FROM incumbent utilities and carbon-fastest pathways. The classification holds both sides of the tension without resolving it—which is analytically correct. Mandatrophy resolution in policy would require either (a) decoupling governance and carbon optimization (separate constraints, separate timelines) or (b) demonstrating that decentralized, democratically-controlled systems CAN hit carbon targets on the required timeline (currently contested).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    governance_speed_tradeoff,
    'Is rapid decentralization of energy systems technically and economically feasible on the climate emergency timeline, or does democratization of energy governance slow deployment below carbon-reduction requirements?',
    'Real-world deployment data from jurisdictions pursuing systems-transition pathways (Denmark, Costa Rica community energy; Costa Rica renewables + democratic oversight) versus traditional centralized/incumbent models on carbon reduction per year and cost per ton. If decentralized systems hit carbon targets on timeline, tradeoff is managed; if slower, the constraint''s founding problem (governance) conflicts with the pressing problem (carbon).',
    'If decentralized governance slows carbon reduction, the constraint shifts from tangled-rope (genuine coordination + extraction) to snare (extraction disguised as coordination). If decentralized systems are speed-competitive, the constraint is justifiably tangled-rope with net coordination.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(governance_speed_tradeoff, empirical, 'Whether democratic energy governance is compatible with emergency-speed decarbonization.').

omega_variable(
    reading_substitution_ambiguity,
    'Is the systems-transition reading a coherent framing of climate mitigation, or is it a reclassification of energy justice as climate mitigation—conflating two different problems and using climate urgency to advance energy governance reforms?',
    'Genealogical analysis: did the systems-transition framing exist before climate emphasis rose (energy justice tradition), or did it emerge after climate became paramount (opportunistic frame capture)? Discourse analysis of whether advocates distinguish ''we need democratic energy'' from ''democratic energy is necessary for climate'' or collapse them.',
    'If the reading conflates energy justice and climate, the constraint is snare (extractive reclassification disguised as climate necessity). If democratic energy IS structurally necessary for climate targets, it remains tangled-rope. If they are independent problems requiring independent solutions, decompose into separate constraints.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_substitution_ambiguity, conceptual, 'Whether systems-transition is a climate reading or an energy-justice reading mislabeled as climate.').

omega_variable(
    incumbent_adaptation_suppression,
    'Could incumbent nuclear operators and centralized utilities adapt to democratic governance structures and participatory models, or does the constraint''s enforcement require their functional elimination?',
    'Test cases where utilities attempted cooperative models (Ökostrom Austria, Tenaska Energy, etc.). If adaptation succeeds, suppression is lower and constraint is coordination-friendly tangled rope. If incumbent institutions are structurally unable to adopt participatory governance (due to capital structure, fiduciary duty, or regulatory capture), suppression is structural and constraint slides toward snare.',
    'If utilities can adapt, the victim set should include adapted versions of incumbent operators (lower d, less extraction). If adaptation is structurally impossible, victims have no exit and suppression is near-total for those agents.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(incumbent_adaptation_suppression, empirical, 'Whether incumbent energy institutions can genuinely transform to participatory models or must be replaced.').

omega_variable(
    democratic_control_operationalization,
    'What counts as ''democratic control'' in energy systems? Is it community ownership, community voice in siting decisions, elected boards, or something stronger? Does the constraint operationalize this or leave it to interpretation?',
    'Policy implementation review: do regulations or frameworks specify what democratic control means? Do community co-ops practicing energy democracy align on the standard, or does each jurisdiction invent its own? If diverse operationalizations produce diverse outcomes (some genuinely participatory, some cosmetic), the constraint is underspecified.',
    'If the constraint is vague on governance implementation, it risks becoming a vessel for theater (narrative democratization without actual power transfer). Theater ratio would remain high even if the constraint nominally succeeds at policy level.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(democratic_control_operationalization, conceptual, 'Whether ''democratic control'' has a determinate meaning or is a flexible signifier prone to performative interpretation.').

omega_variable(
    committer_reading_distinctness,
    'How does the systems-transition reading DIFFER from the opportunity_cost_reading? Both emphasize distributed renewables; both critique nuclear. Is systems-transition genuinely distinct or is it opportunity-cost relabeled with governance language?',
    'Compare the two readings'' core claims: opportunity_cost_reading says ''nuclear is economically inefficient, renewables + storage are cheaper per ton.'' Systems-transition says ''nuclear requires centralized governance, renewables enable democratic control.'' If the readings diverge on what COUNTS (cost vs. governance) they are distinct; if both dismiss nuclear for overlapping reasons, systems-transition is redundant and the kernel is actually two-way (portfolio vs. opportunity+systems conflated).',
    'If systems-transition is genuinely distinct, this constraint story is properly authored. If it is redundant with opportunity-cost, decompose them and mark as the same reading with different framing. If they are conflated in real discourse, add an omega capturing the reading-disambiguation ambiguity to both stories.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_reading_distinctness, conceptual, 'Whether systems-transition and opportunity-cost are distinct kernel readings or one reading with two names.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_mitigation_imperative__systems_transition_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_mitigation_imperative__systems_transition_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(clim_tr_t0, observed).
narrative_ontology:measurement(clim_tr_t5, climate_mitigation_imperative__systems_transition_reading, theater_ratio, 5, 0.32).
narrative_ontology:measurement_basis(clim_tr_t5, observed).
narrative_ontology:measurement(clim_tr_t10, climate_mitigation_imperative__systems_transition_reading, theater_ratio, 10, 0.36).
narrative_ontology:measurement_basis(clim_tr_t10, observed).
narrative_ontology:measurement(clim_tr_t15, climate_mitigation_imperative__systems_transition_reading, theater_ratio, 15, 0.39).
narrative_ontology:measurement_basis(clim_tr_t15, observed).
narrative_ontology:measurement(clim_tr_t20, climate_mitigation_imperative__systems_transition_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement_basis(clim_tr_t20, observed).
narrative_ontology:measurement(clim_tr_t25, climate_mitigation_imperative__systems_transition_reading, theater_ratio, 25, 0.41).
narrative_ontology:measurement_basis(clim_tr_t25, projected).
narrative_ontology:measurement(clim_tr_t30, climate_mitigation_imperative__systems_transition_reading, theater_ratio, 30, 0.41).
narrative_ontology:measurement_basis(clim_tr_t30, projected).
narrative_ontology:measurement(clim_tr_t40, climate_mitigation_imperative__systems_transition_reading, theater_ratio, 40, 0.41).
narrative_ontology:measurement_basis(clim_tr_t40, projected).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_mitigation_imperative__systems_transition_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement_basis(clim_be_t0, observed).
narrative_ontology:measurement(clim_be_t5, climate_mitigation_imperative__systems_transition_reading, base_extractiveness, 5, 0.56).
narrative_ontology:measurement_basis(clim_be_t5, observed).
narrative_ontology:measurement(clim_be_t10, climate_mitigation_imperative__systems_transition_reading, base_extractiveness, 10, 0.61).
narrative_ontology:measurement_basis(clim_be_t10, observed).
narrative_ontology:measurement(clim_be_t15, climate_mitigation_imperative__systems_transition_reading, base_extractiveness, 15, 0.64).
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
narrative_ontology:measurement(clim_su_t0, climate_mitigation_imperative__systems_transition_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement_basis(clim_su_t0, observed).
narrative_ontology:measurement(clim_su_t5, climate_mitigation_imperative__systems_transition_reading, suppression_requirement, 5, 0.42).
narrative_ontology:measurement_basis(clim_su_t5, observed).
narrative_ontology:measurement(clim_su_t10, climate_mitigation_imperative__systems_transition_reading, suppression_requirement, 10, 0.46).
narrative_ontology:measurement_basis(clim_su_t10, observed).
narrative_ontology:measurement(clim_su_t15, climate_mitigation_imperative__systems_transition_reading, suppression_requirement, 15, 0.49).
narrative_ontology:measurement_basis(clim_su_t15, observed).
narrative_ontology:measurement(clim_su_t20, climate_mitigation_imperative__systems_transition_reading, suppression_requirement, 20, 0.51).
narrative_ontology:measurement_basis(clim_su_t20, observed).
narrative_ontology:measurement(clim_su_t25, climate_mitigation_imperative__systems_transition_reading, suppression_requirement, 25, 0.52).
narrative_ontology:measurement_basis(clim_su_t25, projected).
narrative_ontology:measurement(clim_su_t30, climate_mitigation_imperative__systems_transition_reading, suppression_requirement, 30, 0.52).
narrative_ontology:measurement_basis(clim_su_t30, projected).
narrative_ontology:measurement(clim_su_t40, climate_mitigation_imperative__systems_transition_reading, suppression_requirement, 40, 0.52).
narrative_ontology:measurement_basis(clim_su_t40, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_mitigation_imperative__systems_transition_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(climate_mitigation_imperative__systems_transition_reading, 0.12).
narrative_ontology:affects_constraint(climate_mitigation_imperative__systems_transition_reading, climate_mitigation_imperative__portfolio_optimization_reading).
narrative_ontology:affects_constraint(climate_mitigation_imperative__systems_transition_reading, climate_mitigation_imperative__opportunity_cost_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three kernel readings of climate_mitigation_imperative. The systems-transition reading frames mitigation as requiring democratic energy governance and decentralization. Sibling readings portfolio_optimization_reading and opportunity_cost_reading prioritize carbon-per-dollar and cost-per-ton respectively, creating a genuine three-way contest over what constitutes successful climate mitigation. All three are live positions in contemporary energy policy debates. The three stories are linked via network.affects_constraints and form a constraint family where each reading creates structural pressure on the others without logically foreclosing them.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(climate_mitigation_imperative__systems_transition_reading, powerless, 0.85).
constraint_indexing:directionality_override(climate_mitigation_imperative__systems_transition_reading, moderate, 0.42).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
