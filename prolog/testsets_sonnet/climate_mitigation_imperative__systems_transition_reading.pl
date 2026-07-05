% ============================================================================
% CONSTRAINT STORY: climate_mitigation_imperative__systems_transition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
 *   constraint_id: climate_mitigation_imperative__systems_transition_reading
 *   human_readable: Climate Mitigation as Energy-System Democratization Imperative (Systems Transition Reading)
 *   domain: energy_policy/climate_governance/technology_politics
 *
 * SUMMARY:
 *   This story instantiates the systems_transition_reading of the contested
 *   climate_mitigation_imperative kernel: the claim that mitigation is not
 *   merely a carbon-accounting or portfolio-optimization problem but a
 *   governance-transformation problem, requiring decentralized,
 *   democratically controlled energy systems, and that nuclear power —
 *   regardless of its carbon intensity per kWh — structurally perpetuates the
 *   centralized, extractive ownership patterns mitigation is supposed to
 *   dismantle. Under this reading nuclear enters the victim set (its
 *   operators, workforce, and dependent grid regions bear the cost of
 *   delegitimization and displacement) while distributed renewable
 *   developers, storage firms, and community energy cooperatives are
 *   beneficiaries who gain political and financial standing specifically
 *   because of the decentralization framing. This is a distinct constraint
 *   from the portfolio_optimization_reading (which treats nuclear as a
 *   necessary low-carbon baseload contributor) and the
 *   opportunity_cost_reading (which evaluates nuclear purely on deployment
 *   speed per dollar) — the three readings have different beneficiary/victim
 *   structures and different epsilon values because they are answering
 *   structurally different questions about what mitigation requires, not the
 *   same question measured differently. Per the epsilon-invariance principle,
 *   each is authored as its own story and linked via network edges rather
 *   than reconciled into one.
 *
 * KEY AGENTS:
 *   - grid_democratization_advocacy_networks: sets the political/regulatory agenda (organized/mobile) — administers the decentralization framing without directly profiting
 *   - distributed_solar_developers and storage_and_demand_response_firms: primary beneficiaries (organized/mobile) — capture financing and permitting priority specifically because of this reading's dominance
 *   - existing_nuclear_operators and nuclear_utility_workforces: primary targets (powerful-trapped and powerless-trapped respectively) — bear delegitimization and asset/employment risk regardless of their carbon performance
 *   - grid_regions_dependent_on_baseload_reliability and ratepayers_in_high_renewable_penetration_grids: diffuse payers (moderate/powerless, constrained/trapped) — bear reliability and cost risk from the transition pattern this reading prefers
 *   - climate_scientists_and_ipcc_modelers: analytical observer — can assess emissions-trajectory consequences without adjudicating the governance question
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_mitigation_imperative__systems_transition_reading, 0.58).
domain_priors:suppression_score(climate_mitigation_imperative__systems_transition_reading, 0.52).
domain_priors:theater_ratio(climate_mitigation_imperative__systems_transition_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_mitigation_imperative__systems_transition_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(climate_mitigation_imperative__systems_transition_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(climate_mitigation_imperative__systems_transition_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_mitigation_imperative__systems_transition_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(climate_mitigation_imperative__systems_transition_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_mitigation_imperative__systems_transition_reading, tangled_rope).
narrative_ontology:human_readable(climate_mitigation_imperative__systems_transition_reading, "Climate Mitigation as Energy-System Democratization Imperative (Systems Transition Reading)").
narrative_ontology:topic_domain(climate_mitigation_imperative__systems_transition_reading, "energy_policy/climate_governance/technology_politics").

domain_priors:requires_active_enforcement(climate_mitigation_imperative__systems_transition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_mitigation_imperative__systems_transition_reading, '168bcaa2-4216-40f4-bc4a-fae979274567').
narrative_ontology:cs_kernel_codification('168bcaa2-4216-40f4-bc4a-fae979274567', distributed).
narrative_ontology:cs_authority_grounding('168bcaa2-4216-40f4-bc4a-fae979274567', distributed).
narrative_ontology:cs_reading_relation('168bcaa2-4216-40f4-bc4a-fae979274567', climate_mitigation_imperative__portfolio_optimization_reading, coexists_with).
narrative_ontology:cs_reading_relation('168bcaa2-4216-40f4-bc4a-fae979274567', climate_mitigation_imperative__opportunity_cost_reading, influences).
narrative_ontology:cs_axiom('168bcaa2-4216-40f4-bc4a-fae979274567', foundational, ownership_structure_is_constitutive_of_mitigation_adequacy).
narrative_ontology:cs_axiom_status(ownership_structure_is_constitutive_of_mitigation_adequacy, holdable).
narrative_ontology:cs_axiom_grounding('168bcaa2-4216-40f4-bc4a-fae979274567', ownership_structure_is_constitutive_of_mitigation_adequacy, deontological).
narrative_ontology:cs_axiom('168bcaa2-4216-40f4-bc4a-fae979274567', foundational, centralized_generation_reproduces_extractive_power_regardless_of_carbon_output).
narrative_ontology:cs_axiom_status(centralized_generation_reproduces_extractive_power_regardless_of_carbon_output, holdable).
narrative_ontology:cs_axiom_grounding('168bcaa2-4216-40f4-bc4a-fae979274567', centralized_generation_reproduces_extractive_power_regardless_of_carbon_output, conventional).
narrative_ontology:cs_reference_frame('168bcaa2-4216-40f4-bc4a-fae979274567', centralized_utility_ownership_baseline).
narrative_ontology:cs_drift_state('168bcaa2-4216-40f4-bc4a-fae979274567', post_2015_paris_era_energy_democracy_movement, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('168bcaa2-4216-40f4-bc4a-fae979274567', '').
narrative_ontology:cs_kernel_id(climate_mitigation_imperative__systems_transition_reading, climate_mitigation_imperative).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__systems_transition_reading, distributed_solar_developers).
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__systems_transition_reading, community_energy_cooperatives).
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__systems_transition_reading, grid_democratization_advocacy_networks).
narrative_ontology:constraint_beneficiary(climate_mitigation_imperative__systems_transition_reading, storage_and_demand_response_firms).
narrative_ontology:constraint_victim(climate_mitigation_imperative__systems_transition_reading, nuclear_utility_workforces).
narrative_ontology:constraint_victim(climate_mitigation_imperative__systems_transition_reading, existing_nuclear_operators).
narrative_ontology:constraint_victim(climate_mitigation_imperative__systems_transition_reading, grid_regions_dependent_on_baseload_reliability).
narrative_ontology:constraint_victim(climate_mitigation_imperative__systems_transition_reading, ratepayers_in_high_renewable_penetration_grids).
narrative_ontology:constraint_vindicates(climate_mitigation_imperative__systems_transition_reading, energy_democracy_thesis).
narrative_ontology:constraint_vindicates(climate_mitigation_imperative__systems_transition_reading, centralized_grid_extraction_critique).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the political and regulatory agenda that frames mitigation as requiring ownership diffusion, not just decarbonization — pushes interconnection rules, community-ownership mandates, and permitting preferences that favor distributed generation over centralized nuclear licensing. Does not itself generate power or profit directly, but shapes which projects clear regulatory and financing hurdles.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__systems_transition_reading, grid_democratization_advocacy_networks, agenda_setter,
    organized, generational, mobile, national).

% Captures financing, permitting priority, and subsidy design once mitigation policy is read as a decentralization mandate rather than a carbon-per-dollar or portfolio-reliability calculation. Their business model depends on this reading remaining politically dominant.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__systems_transition_reading, distributed_solar_developers, beneficiary,
    organized, biographical, mobile, regional).

% Gains standing, grant eligibility, and governance seats in energy planning specifically because the constraint frames democratic control as the mitigation goal itself, not merely a side benefit of decarbonization. Genuinely exercises local control in the arrangement's favorable cases.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__systems_transition_reading, community_energy_cooperatives, beneficiary,
    moderate, generational, constrained, local).

% Benefits from a policy environment that treats grid flexibility and distributed balancing as necessary complements to renewables displacing baseload — a market that shrinks if nuclear baseload is treated as an acceptable mitigation pathway.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__systems_transition_reading, storage_and_demand_response_firms, beneficiary,
    organized, biographical, mobile, national).

% Operates plant assets with multi-decade sunk capital and licensing timelines that cannot pivot to a decentralization framing; under this reading, their contribution to decarbonization is discounted or actively opposed regardless of carbon output, because the constraint's success criterion is ownership structure, not emissions. Exit means early retirement or stranded assets, not relocation.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__systems_transition_reading, existing_nuclear_operators, payer,
    powerful, biographical, trapped, national).

% Employment, pensions, and community tax base are tied to plants that this reading of mitigation treats as illegitimate infrastructure regardless of their carbon performance. Retraining and relocation options are limited and geographically fixed to plant-adjacent communities.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__systems_transition_reading, nuclear_utility_workforces, payer,
    powerless, biographical, trapped, local).

% Faces reliability and price-volatility risk when dispatchable baseload capacity is retired or blocked from expansion in favor of distributed intermittent sources, without commensurate storage buildout keeping pace. Cannot easily exit the grid they are physically connected to.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__systems_transition_reading, grid_regions_dependent_on_baseload_reliability, payer,
    moderate, immediate, constrained, regional).

% Bears the cost of transition investments, curtailment inefficiencies, and reliability premiums that accompany a rapid shift toward distributed and democratically governed generation, whether or not they participated in or endorse the governance-transformation goal.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__systems_transition_reading, ratepayers_in_high_renewable_penetration_grids, payer,
    powerless, immediate, trapped, local).

% Would argue that grid-scale reliability and interregional transmission planning are structurally necessary regardless of ownership model, and that the decentralization framing undervalues their coordination function — but their technical planning voice is subordinated to the governance-transformation framing in policy forums that treat centralization itself as the problem.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__systems_transition_reading, utility_scale_transmission_operators, excluded,
    institutional, generational, constrained, continental).

% Model emissions pathways and note that multiple technology and governance mixes are compatible with mitigation targets; they can assess whether nuclear retirement under this reading accelerates or delays emissions reductions relative to the sibling readings, without adjudicating the governance question itself.
narrative_ontology:constraint_stakeholder(climate_mitigation_imperative__systems_transition_reading, climate_scientists_and_ipcc_modelers, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a genuine collective-action problem: aligning capital, permitting, and grid-interconnection rules across many small distributed generators and community owners who individually lack the scale to negotiate favorable terms, enabling a real transition away from fossil generation with broader local buy-in and governance participation.
% TRANSFER_FUNCTION: Moves regulatory priority, subsidy allocation, financing access, and political legitimacy away from centralized dispatchable generation (chiefly nuclear) and toward distributed renewable and storage assets and the organizations that build and govern them — regardless of the relative carbon performance or reliability cost of the assets displaced.
% ABSENT_VOICES: Nuclear utility workforces and transmission planning engineers rarely have seats in the advocacy coalitions that define what counts as a legitimate mitigation pathway; their objection — that decarbonization speed and grid reliability are being subordinated to an ownership-structure preference — is raised mainly in technical and labor forums outside the primary policy conversation.
% DISAPPEARANCE_RATIONALE: If this reading of the mitigation imperative disappeared, the distributed-generation and community-ownership coalition's privileged policy standing would erode markedly (subsidy design, permitting priority, and narrative framing would shift toward portfolio or least-cost readings) — a real rearrangement for that coalition. But the underlying physical mitigation problem and much decarbonization activity would continue under the sibling readings, so whether 'the world' rearranges depends on which world you are asking about: the governance-politics world clearly does; the emissions-trajectory world is contested.
% FOUNDING_PROBLEM: Built to address a double problem: (1) climate change requires large, fast energy-system transformation, and (2) prior large-scale energy infrastructure (especially nuclear and centralized fossil generation) has historically concentrated ownership, decision-making, and risk in ways that excluded affected communities and entrenched utility and state power.
% FOUNDING_PROBLEM_CORROBORATION: Community energy advocates and some energy-justice scholars outside the direct beneficiary firms attest that historical centralization did produce real democratic deficits and siting injustices, corroborating half the founding claim. Independent grid engineers and IPCC mitigation-pathway modelers, who are not beneficiaries of either nuclear or distributed-renewable buildout, attest that the emissions-reduction half of the founding problem does not require the ownership-structure transformation this reading insists on — multiple governance architectures reach similar decarbonization outcomes in their models, which contests the reading's claim that democratization is strictly necessary for mitigation rather than a separable value choice.
narrative_ontology:disappearance_verdict(climate_mitigation_imperative__systems_transition_reading, contested).
narrative_ontology:founding_problem_status(climate_mitigation_imperative__systems_transition_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_mitigation_imperative__systems_transition_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(climate_mitigation_imperative__systems_transition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(climate_mitigation_imperative__systems_transition_reading, 0.58, 'claude-sonnet-5', 'none', direct).

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
 *   Extraction (0.58) and suppression (0.52) are moderate-high and rising: the reading's political success increasingly depends on active exclusion of nuclear from favorable financing and permitting treatment, not on nuclear's demonstrated emissions performance, which requires enforcement machinery (permitting rules, subsidy design, narrative gatekeeping) that grows more assertive as the reading's coalition consolidates power. Accessibility collapse is set moderate (0.35) rather than high because, unlike a true mountain, alternative mitigation pathways (portfolio or opportunity-cost readings) remain live, visible, and actively argued in policy discourse — the reading has not achieved anything like natural-law status. Resistance is high (0.68) precisely because nuclear operators, grid engineers, and reliability-focused regulators actively contest the framing rather than accepting it as settled.
 *
 * DIRECTIONALITY LOGIC:
 *   Distributed generation developers and community cooperatives sit near the beneficiary end of directionality because the reading's political dominance directly determines their financing and legitimacy — they did not merely benefit incidentally, they organized to produce this framing. Nuclear operators and their workforce sit near the full-target end: trapped exit options (sunk capital, geographically fixed employment) combined with victim-group membership push d high regardless of their actual carbon contribution. Grid regions and ratepayers are payers by diffuse exposure rather than targeted extraction — they did not organize against or for the reading, but bear its downstream reliability and cost consequences, which is reflected in moderate/powerless power with constrained/trapped exit rather than organized victim status.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem genuinely had two components — decarbonization urgency and historical energy-governance injustice — and only the second has structurally required the decentralization/democratization solution; the first (decarbonization) is achievable under multiple governance architectures per independent modeling. Classifying this as tangled_rope rather than snare acknowledges the coordination function is real (distributed generation and cooperative governance do solve a genuine collective-action and equity problem) while the asymmetric cost imposed on nuclear-dependent communities — who are not shown to be net carbon villains — indicates active extraction riding on that coordination function, requiring the active enforcement (permitting bias, subsidy design, narrative gatekeeping) the schema requires for tangled_rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    governance_transformation_necessity_for_kernel,
    'Is democratic/decentralized governance of energy systems a genuinely necessary condition for adequate climate mitigation, or is it a separable value commitment being bundled into the mitigation imperative by this reading''s advocacy coalition?',
    'Compare emissions-trajectory outcomes across jurisdictions pursuing centralized low-carbon buildout (including nuclear) versus decentralized/cooperative buildout, controlling for deployment speed and grid reliability; also assess whether energy-democracy outcomes (community ownership, participatory governance) are achievable independent of the generation-technology mix chosen.',
    'If governance transformation is separable from decarbonization efficacy, this reading''s claim that nuclear is ''incompatible with mitigation'' collapses into a values preference riding on the mitigation imperative''s political urgency — strengthening the tangled_rope reading (coordination for equity, extraction from nuclear-dependent communities dressed as climate necessity). If inseparable, the coordination function is more fully justified and the constraint moves toward a genuine rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(governance_transformation_necessity_for_kernel, conceptual, 'Whether decentralization is a necessary or bundled-but-separable component of mitigation adequacy.').

omega_variable(
    kernel_reading_selection_mechanism,
    'Which of the three sibling readings (systems_transition, portfolio_optimization, opportunity_cost) becomes institutionally dominant in a given jurisdiction, and what determines the selection — technical merit, political coalition strength, or path-dependent prior investment?',
    'Comparative policy-process tracing across jurisdictions that have adopted each reading as their operative mitigation framework, identifying whether adoption correlates with prior energy-ownership structure, advocacy coalition strength, or independent technical assessment.',
    'If reading selection tracks advocacy coalition strength rather than technical assessment of mitigation efficacy, this strengthens the case that the systems_transition_reading (and its siblings) function partly as legitimating narratives for pre-existing political coalitions rather than as neutral derivations from the shared climate_mitigation_imperative kernel.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_mechanism, empirical, 'What determines which kernel reading becomes institutionally operative.').

omega_variable(
    nuclear_workforce_victim_attribution,
    'Is the cost borne by nuclear utility workforces and dependent communities properly attributed to this reading''s governance framing, or to independent economic pressures (market liberalization, gas price competition) that would harm the same workforce under any reading?',
    'Counterfactual analysis of nuclear plant retirement patterns and workforce outcomes in jurisdictions operating under different kernel readings, isolating the marginal effect of decentralization-favoring policy from broader market dynamics.',
    'If most nuclear workforce harm is attributable to market forces independent of this reading, the victim attribution here is overstated and the constraint''s extraction level should be revised downward; if the reading''s policy choices are a substantial independent driver of nuclear retirement beyond market forces, the current victim declaration and extraction level are well-supported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(nuclear_workforce_victim_attribution, empirical, 'Whether harm to nuclear-dependent communities is caused by this reading specifically or by independent market forces.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_mitigation_imperative__systems_transition_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_mitigation_imperative__systems_transition_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(clim_tr_t4, climate_mitigation_imperative__systems_transition_reading, theater_ratio, 4, 0.18).
narrative_ontology:measurement(clim_tr_t8, climate_mitigation_imperative__systems_transition_reading, theater_ratio, 8, 0.21).
narrative_ontology:measurement(clim_tr_t12, climate_mitigation_imperative__systems_transition_reading, theater_ratio, 12, 0.24).
narrative_ontology:measurement(clim_tr_t16, climate_mitigation_imperative__systems_transition_reading, theater_ratio, 16, 0.26).
narrative_ontology:measurement(clim_tr_t20, climate_mitigation_imperative__systems_transition_reading, theater_ratio, 20, 0.28).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_mitigation_imperative__systems_transition_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(clim_be_t4, climate_mitigation_imperative__systems_transition_reading, base_extractiveness, 4, 0.4).
narrative_ontology:measurement(clim_be_t8, climate_mitigation_imperative__systems_transition_reading, base_extractiveness, 8, 0.47).
narrative_ontology:measurement(clim_be_t12, climate_mitigation_imperative__systems_transition_reading, base_extractiveness, 12, 0.52).
narrative_ontology:measurement(clim_be_t16, climate_mitigation_imperative__systems_transition_reading, base_extractiveness, 16, 0.56).
narrative_ontology:measurement(clim_be_t20, climate_mitigation_imperative__systems_transition_reading, base_extractiveness, 20, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_mitigation_imperative__systems_transition_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(clim_su_t4, climate_mitigation_imperative__systems_transition_reading, suppression_requirement, 4, 0.36).
narrative_ontology:measurement(clim_su_t8, climate_mitigation_imperative__systems_transition_reading, suppression_requirement, 8, 0.41).
narrative_ontology:measurement(clim_su_t12, climate_mitigation_imperative__systems_transition_reading, suppression_requirement, 12, 0.45).
narrative_ontology:measurement(clim_su_t16, climate_mitigation_imperative__systems_transition_reading, suppression_requirement, 16, 0.49).
narrative_ontology:measurement(clim_su_t20, climate_mitigation_imperative__systems_transition_reading, suppression_requirement, 20, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_mitigation_imperative__systems_transition_reading, resource_allocation).
narrative_ontology:affects_constraint(climate_mitigation_imperative__systems_transition_reading, climate_mitigation_imperative__opportunity_cost_reading).
narrative_ontology:affects_constraint(climate_mitigation_imperative__systems_transition_reading, climate_mitigation_imperative__portfolio_optimization_reading).

% DUAL FORMULATION NOTE:
% This story is one of three siblings decomposing the natural-language claim 'the BGS conjecture'-style label 'climate mitigation requires transforming energy systems' into structurally distinct constraints sharing the climate_mitigation_imperative kernel. portfolio_optimization_reading treats nuclear as a necessary contributor within an all-source low-carbon portfolio (nuclear as beneficiary, not victim). opportunity_cost_reading evaluates nuclear strictly on deployment-speed-per-dollar without reference to governance structure. This reading (systems_transition_reading) uniquely makes democratic/decentralized ownership the success criterion, which is what moves nuclear into the victim set and distributed renewables into the beneficiary set — a structural delta the other two readings do not share. Each reading carries its own epsilon and stakeholder structure; they are linked here rather than merged, per the epsilon-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
