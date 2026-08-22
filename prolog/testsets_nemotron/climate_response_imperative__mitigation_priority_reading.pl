% ============================================================================
% CONSTRAINT STORY: climate_response_imperative__mitigation_priority_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_response_imperative__mitigation_priority_reading, []).

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
 *   constraint_id: climate_response_imperative__mitigation_priority_reading
 *   human_readable: Climate Mitigation Priority — Innovation and Market Mechanisms as Primary Response
 *   domain: climate/political_economy/intergenerational_justice
 *
 * SUMMARY:
 *   The mitigation-priority reading of the climate response imperative frames
 *   emissions reduction through technological innovation and market
 *   mechanisms as the primary legitimate response, treating adaptation as
 *   residual — necessary only where mitigation fails or lags. This reading
 *   has structured three decades of climate policy (UNFCCC, Kyoto, Paris),
 *   directing trillions toward mitigation deployment while adaptation finance
 *   remains chronically underfunded. The constraint is a genuine coordination
 *   mechanism (emissions must fall) that has become a vehicle for asymmetric
 *   extraction: Global North innovation sectors capture the value of the
 *   transition, while the costs of deferred adaptation and CDR failure risk
 *   are transferred to future generations and the Global South. The reading's
 *   persistence depends on active enforcement — carbon market rules,
 *   technology transfer barriers, IP regimes, and the 'net zero' accounting
 *   framework that equates future CDR with present mitigation.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_imperative__mitigation_priority_reading, 0.68).
domain_priors:suppression_score(climate_response_imperative__mitigation_priority_reading, 0.55).
domain_priors:theater_ratio(climate_response_imperative__mitigation_priority_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_imperative__mitigation_priority_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(climate_response_imperative__mitigation_priority_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(climate_response_imperative__mitigation_priority_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_response_imperative__mitigation_priority_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(climate_response_imperative__mitigation_priority_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_imperative__mitigation_priority_reading, tangled_rope).
narrative_ontology:human_readable(climate_response_imperative__mitigation_priority_reading, "Climate Mitigation Priority — Innovation and Market Mechanisms as Primary Response").
narrative_ontology:topic_domain(climate_response_imperative__mitigation_priority_reading, "climate/political_economy/intergenerational_justice").

domain_priors:requires_active_enforcement(climate_response_imperative__mitigation_priority_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_imperative__mitigation_priority_reading, 'bf50583c-c665-4a0b-bf27-9b2b0aad400e').
narrative_ontology:cs_kernel_codification('bf50583c-c665-4a0b-bf27-9b2b0aad400e', formalized).
narrative_ontology:cs_authority_grounding('bf50583c-c665-4a0b-bf27-9b2b0aad400e', lineage).
narrative_ontology:cs_interpretation_layer_present('bf50583c-c665-4a0b-bf27-9b2b0aad400e').
narrative_ontology:cs_reading_relation('bf50583c-c665-4a0b-bf27-9b2b0aad400e', climate_response_imperative__adaptation_priority_reading, coexists_with).
narrative_ontology:cs_reading_relation('bf50583c-c665-4a0b-bf27-9b2b0aad400e', climate_response_imperative__degrowth_reading, influences).
narrative_ontology:cs_axiom('bf50583c-c665-4a0b-bf27-9b2b0aad400e', foundational, mitigation_sufficiency_claim).
narrative_ontology:cs_axiom_status(mitigation_sufficiency_claim, holdable).
narrative_ontology:cs_axiom_grounding('bf50583c-c665-4a0b-bf27-9b2b0aad400e', mitigation_sufficiency_claim, empirically_contingent).
narrative_ontology:cs_axiom('bf50583c-c665-4a0b-bf27-9b2b0aad400e', foundational, market_mechanism_efficiency_axiom).
narrative_ontology:cs_axiom_status(market_mechanism_efficiency_axiom, holdable).
narrative_ontology:cs_axiom_grounding('bf50583c-c665-4a0b-bf27-9b2b0aad400e', market_mechanism_efficiency_axiom, instrumental).
narrative_ontology:cs_reference_frame('bf50583c-c665-4a0b-bf27-9b2b0aad400e', unfccc_kyoto_paris_regime).
narrative_ontology:cs_drift_state('bf50583c-c665-4a0b-bf27-9b2b0aad400e', post_paris_net_zero_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('bf50583c-c665-4a0b-bf27-9b2b0aad400e', '').
narrative_ontology:cs_kernel_id(climate_response_imperative__mitigation_priority_reading, climate_response_imperative).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_imperative__mitigation_priority_reading, global_north_innovation_sectors).
narrative_ontology:constraint_beneficiary(climate_response_imperative__mitigation_priority_reading, carbon_market_intermediaries).
narrative_ontology:constraint_beneficiary(climate_response_imperative__mitigation_priority_reading, large_scale_renewable_developers).
narrative_ontology:constraint_beneficiary(climate_response_imperative__mitigation_priority_reading, ccs_cdr_technology_proponents).
narrative_ontology:constraint_victim(climate_response_imperative__mitigation_priority_reading, future_generations).
narrative_ontology:constraint_victim(climate_response_imperative__mitigation_priority_reading, vulnerable_global_south_regions).
narrative_ontology:constraint_victim(climate_response_imperative__mitigation_priority_reading, low_income_communities_exposed_to_climate_impacts).
narrative_ontology:constraint_victim(climate_response_imperative__mitigation_priority_reading, indigenous_peoples_frontline_territories).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(climate_response_imperative__mitigation_priority_reading, large_scale_renewable_developers).
narrative_ontology:constraint_vindicates(climate_response_imperative__mitigation_priority_reading, technological_optimism_doctrine).
narrative_ontology:constraint_vindicates(climate_response_imperative__mitigation_priority_reading, market_efficiency_climate_claim).
narrative_ontology:constraint_vindicates(climate_response_imperative__mitigation_priority_reading, green_growth_narrative).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Capture massive public R&D subsidies, carbon credit revenues, and market-making opportunities from mitigation-focused policy. Their business models scale with the volume of mitigation deployment. They can pivot capital across sectors and geographies at will; climate policy risk is a portfolio variable, not an existential threat.
narrative_ontology:constraint_stakeholder(climate_response_imperative__mitigation_priority_reading, global_north_innovation_sectors, beneficiary,
    powerful, biographical, arbitrage, global).

% Extract fees from every ton of carbon traded, every verification contract, every registry service. Their revenue grows with mitigation ambition regardless of whether atmospheric concentrations actually fall. They exit by diversifying into adjacent financial products; their skill set is portable.
narrative_ontology:constraint_stakeholder(climate_response_imperative__mitigation_priority_reading, carbon_market_intermediaries, beneficiary,
    organized, biographical, mobile, global).

% Benefit from guaranteed demand, tax credits, and streamlined permitting under mitigation priority. Also bear grid integration costs and curtailment risk. They can relocate capital to favorable jurisdictions; their exit is constrained by stranded asset risk in specific projects but not by the sector itself.
narrative_ontology:constraint_stakeholder(climate_response_imperative__mitigation_priority_reading, large_scale_renewable_developers, beneficiary,
    powerful, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(climate_response_imperative__mitigation_priority_reading, large_scale_renewable_developers, payer).

% Depend entirely on the mitigation-priority framing to justify continued fossil infrastructure via 'net zero' pathways. Their business model requires policy to treat future CDR as equivalent to present mitigation. Exit means abandoning sunk R&D and pilot infrastructure; their professional identity is fused to the technology's necessity.
narrative_ontology:constraint_stakeholder(climate_response_imperative__mitigation_priority_reading, ccs_cdr_technology_proponents, beneficiary,
    organized, biographical, constrained, global).

% Inherit the atmospheric stock if mitigation underdelivers or CDR fails to scale. They bear the adaptation burden deferred by today's mitigation-only bets. They have no voice in current decisions, no exit from the planetary system, and no capacity to resist the arrangement.
narrative_ontology:constraint_stakeholder(climate_response_imperative__mitigation_priority_reading, future_generations, payer,
    powerless, civilizational, trapped, universal).

% Face accelerating climate impacts while adaptation finance remains a fraction of mitigation flows. Their mitigation potential is extracted via carbon markets; their adaptation needs are deferred. Exit is constrained by sovereign debt, technology transfer barriers, and geographic immobility.
narrative_ontology:constraint_stakeholder(climate_response_imperative__mitigation_priority_reading, vulnerable_global_south_regions, payer,
    moderate, generational, constrained, global).

% Bear the immediate costs of heat, flooding, and crop failure while mitigation investments flow to grid-scale projects far from their needs. They lack political voice to redirect resources and physical mobility to escape exposure.
narrative_ontology:constraint_stakeholder(climate_response_imperative__mitigation_priority_reading, low_income_communities_exposed_to_climate_impacts, payer,
    powerless, biographical, trapped, regional).

% Their territories host both extraction (mining for transition minerals) and carbon offset projects that restrict traditional land use. Their cosmologies treat land as non-commodifiable; the mitigation framework commodifies it. Exit means cultural erasure — identity is constituted through the relationship to territory.
narrative_ontology:constraint_stakeholder(climate_response_imperative__mitigation_priority_reading, indigenous_peoples_frontline_territories, payer,
    powerless, generational, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(climate_response_imperative__mitigation_priority_reading, indigenous_peoples_frontline_territories, excluded).

% Set mitigation targets, design carbon markets, allocate R&D budgets, and negotiate international frameworks. They benefit domestically from green industrial policy and internationally from maintaining the mitigation-as-primary framing that defers adaptation liability. They can shift policy emphasis between electoral cycles; their exit from the constraint is policy reversal.
narrative_ontology:constraint_stakeholder(climate_response_imperative__mitigation_priority_reading, global_north_governments, agenda_setter,
    institutional, biographical, mobile, national).

% Produce the assessment reports and negotiation frameworks that legitimize the mitigation priority. Their institutional survival depends on the climate regime's continued relevance; they have structural incentive to frame the problem in ways that sustain the regime. Exit means institutional obsolescence.
narrative_ontology:constraint_stakeholder(climate_response_imperative__mitigation_priority_reading, ipcc_and_unfccc_institutions, agenda_setter,
    institutional, generational, constrained, global).

% Demand adaptation finance, loss and damage compensation, and mitigation equity. They are admitted to conference spaces but structurally excluded from the decision-making that allocates trillions. Their exit is building parallel institutions (climate reparations frameworks, people's tribunals) — constrained by resource asymmetry.
narrative_ontology:constraint_stakeholder(climate_response_imperative__mitigation_priority_reading, climate_justice_movements, excluded,
    organized, biographical, constrained, global).

% Sees the full structure: a genuine coordination function (emissions must fall) fused with asymmetric extraction (innovation sectors capture the value, vulnerable populations bear the deferred costs). The coordination is real; the extraction is the gap between mitigation promise and delivery, financed by future adaptation burden.
narrative_ontology:constraint_stakeholder(climate_response_imperative__mitigation_priority_reading, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinate global emissions reduction to avoid catastrophic warming — a real collective action problem requiring technology deployment, finance mobilization, and behavioral change across all major economies.
% TRANSFER_FUNCTION: Moves public finance, carbon revenue, and regulatory favor from taxpayers and future generations to Global North innovation sectors, carbon market intermediaries, and large-scale renewable developers. Moves adaptation burden from present decision-makers to future generations and vulnerable regions.
% ABSENT_VOICES: Future generations (by definition absent), frontline indigenous communities (excluded by state-centric negotiation), and the global poor (excluded by resource asymmetry). They would object to the deferral of adaptation, the commodification of their territories, and the bet on unproven CDR.
% DISAPPEARANCE_RATIONALE: If the mitigation-priority framing vanished, climate finance would rebalance toward adaptation and loss & damage; carbon markets would collapse without the 'net zero' offset demand; Global North innovation policy would lose its climate justification; vulnerable regions would gain bargaining leverage for immediate adaptation resources. The world rearranges because trillions in expected flows and deferred liabilities are structured around this framing.
% FOUNDING_PROBLEM: The atmospheric commons was being treated as a free waste sink; a coordination mechanism was needed to internalize the carbon externality and drive emissions to net zero.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem (carbon externality) is attested by climate physics (IPCC WG1) and economic theory (Pigou, Nordhaus) — sources outside the beneficiary set. However, the STATUS of the founding problem is contested: mitigation-priority beneficiaries attest it requires their specific technological/market solution; climate justice movements attest the problem has mutated into a distributional crisis that the founding solution cannot address.
narrative_ontology:disappearance_verdict(climate_response_imperative__mitigation_priority_reading, world_rearranges).
narrative_ontology:founding_problem_status(climate_response_imperative__mitigation_priority_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_response_imperative__mitigation_priority_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(climate_response_imperative__mitigation_priority_reading, 'none', 1).
narrative_ontology:epsilon_provenance(climate_response_imperative__mitigation_priority_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_response_imperative__mitigation_priority_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_response_imperative__mitigation_priority_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_response_imperative__mitigation_priority_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.68) reflects the gap between mitigation promise and delivery: emissions have risen 60% since 1992 while mitigation finance has grown exponentially. The extraction is the value captured by innovation sectors per ton of promised-but-undelivered reduction. Suppression (0.55) is moderate: the constraint suppresses alternative framings (adaptation priority, degrowth) through institutional gatekeeping and epistemic authority, but does not fully close exits — vulnerable nations still push adaptation agendas in negotiations. Theater ratio (0.42) is rising: an increasing share of mitigation activity (offset markets, CDR modeling, 'net zero' pledges without near-term cuts) performs the appearance of action while emissions accumulate. Accessibility collapse (0.58) reflects the narrowing of policy imagination — 'mitigation first' has become the only legitimate frame in major institutions. Resistance (0.62) is significant: climate justice movements, Global South negotiating blocs, and frontline communities actively contest the framing, but their resistance is contained within the regime's procedural channels.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter/beneficiary seats (Global North governments, innovation sectors), the constraint appears as a Rope: genuine coordination solving a real collective action problem with net benefits. From the payer seats (future generations, vulnerable regions, frontline communities), it computes as a Snare: the coordination story is cover for extraction that persists by suppressing alternatives (adaptation finance, degrowth, climate reparations). The engine computes this divergence from the structural data — the declared beneficiaries, victims, exit options, and power positions. The claimed_type (tangled_rope) acknowledges the hybrid reality: the coordination function is real AND the extraction is asymmetric.
 *
 * DIRECTIONALITY LOGIC:
 *   Global North innovation sectors and carbon market intermediaries are structural beneficiaries (d near 0.0): they collect rents from the constraint's operation, control its rule-making, and hold arbitrage-grade exit. Future generations are full targets (d near 1.0): they bear the atmospheric stock risk and deferred adaptation burden with zero exit and zero voice. Vulnerable Global South regions are high-target (d ~0.8): they bear impacts and extraction (carbon colonialism) with constrained exit. Indigenous peoples are identity-locked targets: their territories are both extraction sites and offset zones; exit means cultural erasure. Global North governments and UN institutions are agenda-setters with mobile/constrained exit — they administer the constraint and could change it, but their institutional interests align with its persistence. The analytical observer sees the full asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (carbon externality) remains live — emissions must fall. But the specific arrangement built to solve it (market-mediated, innovation-led mitigation priority) has developed mandatrophy: its mandate has expanded from 'reduce emissions' to 'channel all climate response through market mechanisms that benefit specific sectors,' while the adaptation half of the problem has been systematically deferred. The constraint now extracts more than it coordinates. The founding problem status is 'live' but the founding solution has become extractive — this is the tangled_rope signature.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mitigation_adaptation_separability,
    'Are the mitigation coordination function and the adaptation extraction transfer structurally separable, or has the constraint fused them such that challenging the extraction dismantles the coordination?',
    'Natural experiment: if a jurisdiction pursues aggressive adaptation without mitigation priority (or vice versa), does the coordination function hold? Historical counterfactual: would a 1990s adaptation-first framework have achieved comparable emissions outcomes?',
    'If separable, the constraint is a clean tangled_rope — coordination extractable from extraction. If fused, the constraint may be a snare wearing a rope''s skin: the coordination story is structurally necessary to the extraction, not merely contingently attached.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mitigation_adaptation_separability, conceptual, 'Whether the genuine coordination and asymmetric extraction can be disaggregated.').

omega_variable(
    cdr_scaling_feasibility,
    'Will carbon dioxide removal technologies scale to the gigaton levels assumed in mitigation-priority pathways, or is the CDR bet a structural extraction mechanism that transfers mitigation burden to future generations?',
    'Empirical tracking of CDR deployment vs. modeled pathways; engineering assessment of energy, land, and water requirements at scale; monitoring of ''net zero'' pledge reliance on future CDR.',
    'If CDR fails to scale, the mitigation-priority reading''s extraction from future generations is confirmed as structural — the constraint promised mitigation but delivered deferral. If CDR scales, the extraction decreases but the coordination-extraction boundary shifts to CDR deployment communities.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cdr_scaling_feasibility, empirical, 'Whether the technological bet underpinning the mitigation priority''s intergenerational transfer will pay out.').

omega_variable(
    committer_frame_location,
    'This constraint is one reading (mitigation_priority_reading) of the contested kernel ''climate_response_imperative''. Where exactly does the structural disagreement with sibling readings locate?',
    'Map each sibling reading''s beneficiary/victim sets, claimed coordination functions, and founding problem status. The disagreement locates at: (1) victim set definition (future generations included/excluded), (2) coordination function scope (mitigation-only vs. mitigation+adaptation vs. structural transformation), (3) founding problem status (live/contested/dead).',
    'If the disagreement is at victim set definition, the kernel''s ε is reading-indexed (per OQ-26). If at coordination function scope, the kernel contains multiple constraints (per DP-001). If at founding problem status, the mandatrophy resolution differs across readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_frame_location, conceptual, 'Structural locus of disagreement between mitigation_priority_reading and its sibling readings adaptation_priority_reading and degrowth_reading.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of alternative framings (adaptation priority, degrowth) structural (institutional gatekeeping, funding control, epistemic authority) or internalized (climate policy community has absorbed the mitigation-priority frame as cognitive default)?',
    'Track policy imagination range in major institutions over time: when adaptation/degrowth proposals enter formal negotiation texts, are they substantively engaged or procedurally marginalized? Survey climate policymakers'' mental models of ''legitimate climate response.''',
    'If structural, suppression is contestable through institutional reform. If internalized, the constraint''s effective suppression exceeds its structural measure — the target (alternative framings) carries the suppression cognitively even when formal barriers lower.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression of alternative climate response framings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_imperative__mitigation_priority_reading, 1992, 2035).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(climate_response_mitigation_priority_tr_t1992, climate_response_imperative__mitigation_priority_reading, theater_ratio, 1992, 0.15).
narrative_ontology:measurement(climate_response_mitigation_priority_tr_t1997, climate_response_imperative__mitigation_priority_reading, theater_ratio, 1997, 0.18).
narrative_ontology:measurement(climate_response_mitigation_priority_tr_t2005, climate_response_imperative__mitigation_priority_reading, theater_ratio, 2005, 0.22).
narrative_ontology:measurement(climate_response_mitigation_priority_tr_t2009, climate_response_imperative__mitigation_priority_reading, theater_ratio, 2009, 0.28).
narrative_ontology:measurement(climate_response_mitigation_priority_tr_t2015, climate_response_imperative__mitigation_priority_reading, theater_ratio, 2015, 0.33).
narrative_ontology:measurement(climate_response_mitigation_priority_tr_t2021, climate_response_imperative__mitigation_priority_reading, theater_ratio, 2021, 0.38).
narrative_ontology:measurement(climate_response_mitigation_priority_tr_t2024, climate_response_imperative__mitigation_priority_reading, theater_ratio, 2024, 0.4).
narrative_ontology:measurement(climate_response_mitigation_priority_tr_t2030, climate_response_imperative__mitigation_priority_reading, theater_ratio, 2030, 0.41).
narrative_ontology:measurement(climate_response_mitigation_priority_tr_t2035, climate_response_imperative__mitigation_priority_reading, theater_ratio, 2035, 0.42).

% Extraction over time
narrative_ontology:measurement(climate_response_mitigation_priority_be_t1992, climate_response_imperative__mitigation_priority_reading, base_extractiveness, 1992, 0.25).
narrative_ontology:measurement(climate_response_mitigation_priority_be_t1997, climate_response_imperative__mitigation_priority_reading, base_extractiveness, 1997, 0.3).
narrative_ontology:measurement(climate_response_mitigation_priority_be_t2005, climate_response_imperative__mitigation_priority_reading, base_extractiveness, 2005, 0.38).
narrative_ontology:measurement(climate_response_mitigation_priority_be_t2009, climate_response_imperative__mitigation_priority_reading, base_extractiveness, 2009, 0.42).
narrative_ontology:measurement(climate_response_mitigation_priority_be_t2015, climate_response_imperative__mitigation_priority_reading, base_extractiveness, 2015, 0.5).
narrative_ontology:measurement(climate_response_mitigation_priority_be_t2021, climate_response_imperative__mitigation_priority_reading, base_extractiveness, 2021, 0.58).
narrative_ontology:measurement(climate_response_mitigation_priority_be_t2024, climate_response_imperative__mitigation_priority_reading, base_extractiveness, 2024, 0.63).
narrative_ontology:measurement(climate_response_mitigation_priority_be_t2030, climate_response_imperative__mitigation_priority_reading, base_extractiveness, 2030, 0.66).
narrative_ontology:measurement(climate_response_mitigation_priority_be_t2035, climate_response_imperative__mitigation_priority_reading, base_extractiveness, 2035, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(climate_response_mitigation_priority_su_t1992, climate_response_imperative__mitigation_priority_reading, suppression_requirement, 1992, 0.2).
narrative_ontology:measurement(climate_response_mitigation_priority_su_t1997, climate_response_imperative__mitigation_priority_reading, suppression_requirement, 1997, 0.25).
narrative_ontology:measurement(climate_response_mitigation_priority_su_t2005, climate_response_imperative__mitigation_priority_reading, suppression_requirement, 2005, 0.3).
narrative_ontology:measurement(climate_response_mitigation_priority_su_t2009, climate_response_imperative__mitigation_priority_reading, suppression_requirement, 2009, 0.35).
narrative_ontology:measurement(climate_response_mitigation_priority_su_t2015, climate_response_imperative__mitigation_priority_reading, suppression_requirement, 2015, 0.42).
narrative_ontology:measurement(climate_response_mitigation_priority_su_t2021, climate_response_imperative__mitigation_priority_reading, suppression_requirement, 2021, 0.48).
narrative_ontology:measurement(climate_response_mitigation_priority_su_t2024, climate_response_imperative__mitigation_priority_reading, suppression_requirement, 2024, 0.52).
narrative_ontology:measurement(climate_response_mitigation_priority_su_t2030, climate_response_imperative__mitigation_priority_reading, suppression_requirement, 2030, 0.54).
narrative_ontology:measurement(climate_response_mitigation_priority_su_t2035, climate_response_imperative__mitigation_priority_reading, suppression_requirement, 2035, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_response_imperative__mitigation_priority_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(climate_response_imperative__mitigation_priority_reading, 0.18).
narrative_ontology:affects_constraint(climate_response_imperative__mitigation_priority_reading, climate_response_imperative__adaptation_priority_reading).
narrative_ontology:affects_constraint(climate_response_imperative__mitigation_priority_reading, climate_response_imperative__degrowth_reading).
narrative_ontology:affects_constraint(climate_response_imperative__mitigation_priority_reading, carbon_market_architecture).
narrative_ontology:affects_constraint(climate_response_imperative__mitigation_priority_reading, global_adaptation_finance_regime).
narrative_ontology:affects_constraint(climate_response_imperative__mitigation_priority_reading, cdr_deployment_governance).

% DUAL FORMULATION NOTE:
% This constraint decomposes the colloquial 'climate action' label into the mitigation_priority_reading of the climate_response_imperative kernel. The sibling readings (adaptation_priority_reading, degrowth_reading) are separate constraints with their own ε, stakeholder structures, and classifications. They are linked via affects_constraints because the mitigation priority reading structurally influences the others: it sets the financing architecture, defines the 'net zero' accounting that CDR governance depends on, and determines the adaptation finance residual. The dual formulation is: the kernel is the imperative; the readings are the constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(climate_response_imperative__mitigation_priority_reading, institutional, 0.15).
constraint_indexing:directionality_override(climate_response_imperative__mitigation_priority_reading, organized, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
