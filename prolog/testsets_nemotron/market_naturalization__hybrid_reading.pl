% ============================================================================
% CONSTRAINT STORY: market_naturalization__hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_market_naturalization__hybrid_reading, []).

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
 *   constraint_id: market_naturalization__hybrid_reading
 *   human_readable: Market Dominance — Hybrid Reading (Lapsed + Actively Maintained)
 *   domain: economic/political/institutional
 *
 * SUMMARY:
 *   This constraint models the hybrid reading of market naturalization:
 *   market dominance structures are neither purely lapsed historical closures
 *   (which would require no active maintenance) nor purely active extraction
 *   machines (which would require constant enforcement). Instead, they
 *   combine both — some dominance mechanisms (network effects, scale
 *   economies, path-dependent standards) persist without active intervention,
 *   while others (IP enforcement, regulatory capture, strategic litigation,
 *   subsidy regimes) require continuous active maintenance. The beneficiary
 *   structure is mixed: incumbent capital holders and dominant platform
 *   operators benefit from both the lapsed and active components; regulatory
 *   capture networks benefit primarily from the active component. Victims
 *   include new entrants blocked by both types of barriers, labor in
 *   precarious sectors, consumers in captive markets, and small producers.
 *   The constraint is claimed as tangled_rope because it possesses both a
 *   genuine coordination function (market infrastructure, standards, scale
 *   efficiencies) AND asymmetric extraction (rent capture, barrier
 *   maintenance) — the two are structurally entangled.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(market_naturalization__hybrid_reading, 0.52).
domain_priors:suppression_score(market_naturalization__hybrid_reading, 0.48).
domain_priors:theater_ratio(market_naturalization__hybrid_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(market_naturalization__hybrid_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(market_naturalization__hybrid_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(market_naturalization__hybrid_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(market_naturalization__hybrid_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(market_naturalization__hybrid_reading, resistance, 0.41).

% --- Constraint claim ---
narrative_ontology:constraint_claim(market_naturalization__hybrid_reading, tangled_rope).
narrative_ontology:human_readable(market_naturalization__hybrid_reading, "Market Dominance — Hybrid Reading (Lapsed + Actively Maintained)").
narrative_ontology:topic_domain(market_naturalization__hybrid_reading, "economic/political/institutional").

domain_priors:requires_active_enforcement(market_naturalization__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(market_naturalization__hybrid_reading, '25124fdc-c79e-44f6-95b9-3d549186ce36').
narrative_ontology:cs_kernel_codification('25124fdc-c79e-44f6-95b9-3d549186ce36', distributed).
narrative_ontology:cs_authority_grounding('25124fdc-c79e-44f6-95b9-3d549186ce36', extraction).
narrative_ontology:cs_interpretation_layer_present('25124fdc-c79e-44f6-95b9-3d549186ce36').
narrative_ontology:cs_reading_relation('25124fdc-c79e-44f6-95b9-3d549186ce36', market_naturalization__lapsed_alternative_reading, coexists_with).
narrative_ontology:cs_reading_relation('25124fdc-c79e-44f6-95b9-3d549186ce36', market_naturalization__beneficiary_maintained_reading, coexists_with).
narrative_ontology:cs_axiom('25124fdc-c79e-44f6-95b9-3d549186ce36', foundational, market_dominance_is_hybrid_lapsed_active).
narrative_ontology:cs_axiom_status(market_dominance_is_hybrid_lapsed_active, holdable).
narrative_ontology:cs_axiom_grounding('25124fdc-c79e-44f6-95b9-3d549186ce36', market_dominance_is_hybrid_lapsed_active, empirically_contingent).
narrative_ontology:cs_axiom('25124fdc-c79e-44f6-95b9-3d549186ce36', foundational, lapsed_mechanisms_provide_cover_for_active_extraction).
narrative_ontology:cs_axiom_status(lapsed_mechanisms_provide_cover_for_active_extraction, holdable).
narrative_ontology:cs_axiom_grounding('25124fdc-c79e-44f6-95b9-3d549186ce36', lapsed_mechanisms_provide_cover_for_active_extraction, empirically_contingent).
narrative_ontology:cs_reference_frame('25124fdc-c79e-44f6-95b9-3d549186ce36', postwar_embedded_liberalism).
narrative_ontology:cs_drift_state('25124fdc-c79e-44f6-95b9-3d549186ce36', neoliberal_hegemony_peak, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('25124fdc-c79e-44f6-95b9-3d549186ce36', '').
narrative_ontology:cs_kernel_id(market_naturalization__hybrid_reading, market_naturalization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(market_naturalization__hybrid_reading, incumbent_capital_holders).
narrative_ontology:constraint_beneficiary(market_naturalization__hybrid_reading, regulatory_capture_networks).
narrative_ontology:constraint_beneficiary(market_naturalization__hybrid_reading, dominant_platform_operators).
narrative_ontology:constraint_victim(market_naturalization__hybrid_reading, new_entrants).
narrative_ontology:constraint_victim(market_naturalization__hybrid_reading, labor_in_precarious_sectors).
narrative_ontology:constraint_victim(market_naturalization__hybrid_reading, consumers_in_captive_markets).
narrative_ontology:constraint_victim(market_naturalization__hybrid_reading, small_producers).
narrative_ontology:constraint_vindicates(market_naturalization__hybrid_reading, market_efficiency_doctrine).
narrative_ontology:constraint_vindicates(market_naturalization__hybrid_reading, natural_monopoly_theory).
narrative_ontology:constraint_vindicates(market_naturalization__hybrid_reading, comparative_advantage_orthodoxy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold controlling positions in dominant firms across sectors. Benefit from both lapsed mechanisms (accumulated scale, network effects, brand moats) and active maintenance (lobbying for favorable regulation, IP enforcement, tax structures). Capital mobility gives them arbitrage-grade exit: they can reallocate across sectors and jurisdictions. Their structural position is near-full beneficiary.
narrative_ontology:constraint_stakeholder(market_naturalization__hybrid_reading, incumbent_capital_holders, beneficiary,
    institutional, generational, arbitrage, global).

% Operate the core digital infrastructure (search, social, cloud, marketplace). Benefit from lapsed network effects and scale economies, while actively maintaining dominance through API control, data accumulation, and strategic acquisition. They set the rules of participation for entire ecosystems. Exit is arbitrage-grade: they own the platform others depend on.
narrative_ontology:constraint_stakeholder(market_naturalization__hybrid_reading, dominant_platform_operators, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(market_naturalization__hybrid_reading, dominant_platform_operators, agenda_setter).

% Revolving-door networks of regulators, lobbyists, industry lawyers, and think tanks. They benefit from the active maintenance component: writing and enforcing rules that entrench incumbents. Their position depends on the dominance structure persisting — if dominance lapsed, their capture value would collapse. Exit is mobile (revolving door) but not arbitrage: they need the captured institution to exist.
narrative_ontology:constraint_stakeholder(market_naturalization__hybrid_reading, regulatory_capture_networks, beneficiary,
    institutional, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(market_naturalization__hybrid_reading, regulatory_capture_networks, agenda_setter).

% Firms attempting to enter dominated markets. Face both lapsed barriers (scale requirements, network effects, switching costs) and active barriers (patent thickets, regulatory compliance costs, exclusionary contracts). Constrained exit: they can pivot to adjacent markets or niche segments, but the dominant structure shapes the entire opportunity landscape. Bear extraction through foregone market access and compliance costs.
narrative_ontology:constraint_stakeholder(market_naturalization__hybrid_reading, new_entrants, payer,
    moderate, biographical, constrained, global).

% Workers in sectors where dominant firms set labor standards (gig platforms, warehouse logistics, franchise chains). Bear extraction through suppressed wages, algorithmic management, and eroded bargaining power. Lapsed mechanisms (monopsony power from scale) combine with active maintenance (anti-union lobbying, classification battles, arbitration clauses). Exit is trapped: sector-specific skills, geographic immobility, and systemic monopsony limit options.
narrative_ontology:constraint_stakeholder(market_naturalization__hybrid_reading, labor_in_precarious_sectors, payer,
    powerless, immediate, trapped, national).

% End users facing limited choice in essential services (broadband, pharmaceuticals, banking, utilities). Bear extraction through higher prices, lower quality, and reduced innovation. Lapsed barriers (infrastructure sunk costs) combine with active barriers (regulatory capture, exclusive territories, switching costs). Exit is constrained: collective action (municipal broadband, policy advocacy) is possible but high-friction.
narrative_ontology:constraint_stakeholder(market_naturalization__hybrid_reading, consumers_in_captive_markets, payer,
    organized, biographical, constrained, national).

% Independent farmers, manufacturers, creators selling into dominated supply chains. Bear extraction through monopsony purchasing power, platform fees, and contractual terms. Lapsed mechanisms (distribution concentration) combine with active maintenance (category management, slotting fees, algorithmic visibility control). Exit is constrained: direct-to-consumer channels exist but require scale they lack.
narrative_ontology:constraint_stakeholder(market_naturalization__hybrid_reading, small_producers, payer,
    moderate, biographical, constrained, regional).

% Antitrust and regulatory bodies investigating dominance abuse. They see the full structure: lapsed efficiencies, active extraction, and the entanglement between them. Their analytical seat computes per-seat classifications from the structural data. They can impose remedies that alter the constraint's enforcement but face political capture and resource asymmetry.
narrative_ontology:constraint_stakeholder(market_naturalization__hybrid_reading, competition_authorities, observer,
    institutional, generational, analytical, national).

% Scholars challenging the naturalization narrative (post-Keynesian, institutional, Marxian, ecological economics). They provide the counter-reading that dominance is constructed, not natural. Their seat is purely analytical: they neither collect nor pay, but their frameworks structure the contest over the kernel's meaning.
narrative_ontology:constraint_stakeholder(market_naturalization__hybrid_reading, heterodox_economists, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the genuine coordination problem of complex production at scale: matching supply and demand across vast networks, standardizing interfaces, aggregating capital for long-horizon investment, and reducing transaction costs through trusted intermediaries. These functions are real and socially necessary.
% TRANSFER_FUNCTION: Moves economic surplus from new entrants, labor, consumers, and small producers to incumbent capital holders, dominant platform operators, and regulatory capture networks — via supra-competitive pricing, suppressed wages, platform fees, regulatory rents, and foregone innovation. The transfer rides on both lapsed mechanisms (scale, networks) and active enforcement (IP, regulation, contracts).
% ABSENT_VOICES: Future generations who inherit the ecological and institutional costs of entrenched dominance; workers in the Global South whose labor markets are shaped by dominant buyer power but who have no voice in Northern regulatory frameworks; small producers in informal economies excluded from dominant supply chains entirely. They would object to the naturalization of arrangements that extract from them, but they are structurally excluded from the conversation.
% DISAPPEARANCE_RATIONALE: If market dominance structures vanished overnight, production would not cease but would reorganize: decentralized coordination mechanisms (federated protocols, cooperative platforms, public infrastructure) would scale; surplus would redistribute from incumbents to entrants, labor, and consumers; innovation would shift from moat-building to problem-solving. The world would rearrange substantially — the constraint is not a natural law.
% FOUNDING_PROBLEM: Coordinating industrial-scale production and distribution across geographically dispersed populations with high capital requirements and long time horizons, while minimizing transaction costs and ensuring reliable quality.
% FOUNDING_PROBLEM_CORROBORATION: Mainstream economics (Chicago School, neoclassical IO) attests the founding problem is live and the current arrangements are the efficient solution. Heterodox economists (Galbraith, Baran & Sweezy, contemporary post-Keynesian and institutional scholars) attest the founding problem was substantially solved by mid-century and the persisting arrangements are extractive. Historical analysis of the 1940s-1970s shows robust growth with far less dominance concentration — corroboration from outside the beneficiary set exists.
narrative_ontology:disappearance_verdict(market_naturalization__hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(market_naturalization__hybrid_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(market_naturalization__hybrid_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(market_naturalization__hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(market_naturalization__hybrid_reading, 0.52, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(market_naturalization__hybrid_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(market_naturalization__hybrid_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(market_naturalization__hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52) reflects moderate but rising rent extraction above coordination costs. Suppression (0.48) is moderate: alternatives exist but are structurally disadvantaged, not eliminated. Theater ratio (0.38) indicates significant but not dominant performative maintenance — the coordination function is real but increasingly overlain by rent-seeking. Accessibility collapse (0.62) is moderately high: once the dominance structure is understood, alternatives are cognitively and institutionally difficult to pursue. Resistance (0.41) is moderate: antitrust, regulatory reform, and competitive challenges exist but are fragmented and often co-opted. The rising trend in extractiveness and theater from 1980-2024 captures the shift from post-war embedded liberalism to neoliberal market naturalization, where lapsed elements (scale, networks) were leveraged into actively maintained extraction regimes.
 *
 * PERSPECTIVAL GAP:
 *   From the incumbent seat, the arrangement appears as rope or mountain: coordination infrastructure they built, natural efficiencies they exploit. From the new entrant and labor seats, it appears as snare: active barriers, suppressed alternatives. From the analytical observer seat, it appears as tangled_rope: the coordination and extraction are genuinely entangled — scale economies are real AND the IP regime that locks them in is actively enforced. The engine computes this divergence from the structural data; the authored claim (tangled_rope) represents the analytical seat's assessment.
 *
 * DIRECTIONALITY LOGIC:
 *   Incumbent capital holders and dominant platform operators are structural beneficiaries (d near 0.0-0.2): they collect rents from both lapsed and active components, with arbitrage-grade exit options (capital mobility, political access). Regulatory capture networks are partial beneficiaries (d ~0.3): they extract via revolving doors and captured rulemaking but depend on the dominance structure for their position. New entrants, precarious labor, captive consumers, and small producers are targets (d 0.7-0.9): they bear extraction costs with constrained or trapped exit. The mixed beneficiary structure reflects the hybrid reading's core claim: some beneficiaries hold position via lapsed mechanisms (low active cost), others via active maintenance (high enforcement cost).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (coordinating complex production at scale) is partially live (scale coordination still needed) but partially dead (the specific dominance structures that solved it have outlived their coordination function and now primarily extract). Mandatrophy is unresolved: the arrangement persists because the lapsed elements provide cover for the active extraction, and no coalition has both the power and incentive to disentangle them. The hybrid reading captures this irresolution — it is neither pure coordination (rope) nor pure extraction (snare) nor degraded inertia (piton).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a single reading of the market_naturalization kernel, and which structural elements distinguish it from the lapsed_alternative_reading and beneficiary_maintained_reading?',
    'Cross-reading structural comparison: map each reading''s beneficiary/victim sets, enforcement requirements, and extractiveness profiles to identify non-overlapping claims.',
    'If readings share identical structural profiles, they are not distinct constraints. Distinct profiles validate the kernel decomposition; overlapping profiles require further disambiguation or merger.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Commitment-system kernel decomposition: this constraint instantiates the hybrid_reading of market_naturalization; sibling readings are separate constraint stories.').

omega_variable(
    lapsed_vs_active_boundary,
    'Where does the boundary fall between lapsed elements (requiring no active maintenance) and actively maintained elements (requiring enforcement) within market dominance arrangements?',
    'Domain-by-domain audit: for each sector (tech platforms, energy, finance, pharmaceuticals), trace which dominance mechanisms persist without active intervention (network effects, scale economies) vs. which require continuous lobbying, regulatory capture, or strategic litigation.',
    'If the lapsed share dominates, the constraint trends toward piton/mountain; if the active share dominates, it trends toward snare/tangled_rope. The hybrid classification depends on this boundary being genuinely mixed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(lapsed_vs_active_boundary, empirical, 'Structural boundary between lapsed and actively maintained components of market dominance.').

omega_variable(
    naturalness_ambiguity,
    'Is the ''naturalness'' of market dominance a genuine feature of competitive dynamics, or a constructed narrative that benefits identifiable actors?',
    'Counterfactual policy simulation: remove active maintenance mechanisms (IP regimes, regulatory barriers, subsidy structures) and measure dominance persistence. If dominance collapses, naturalness is constructed.',
    'If naturalness is constructed, false_summit_mountain signature may fire on adjacent mountain-claimed constraints; this hybrid reading''s extractiveness would be re-weighted upward.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(naturalness_ambiguity, conceptual, 'Natural-law vs. constructed-beneficiary ambiguity for market dominance arrangements.').

omega_variable(
    domain_variance_extractiveness,
    'How much does extractiveness vary across domains (tech, energy, finance, pharma, retail), and does a single constraint story adequately capture this variance?',
    'Per-domain ε measurement: author separate constraint stories for each major domain if variance exceeds 0.25 in extractiveness or shifts claimed_type.',
    'High variance would require domain-decomposed stories linked via network.affects_constraints, each with its own ε and claimed_type.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(domain_variance_extractiveness, empirical, 'Cross-domain variance in market dominance extractiveness and classification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(market_naturalization__hybrid_reading, 1980, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(market_naturalization__hybrid_reading_tr_t1980, market_naturalization__hybrid_reading, theater_ratio, 1980, 0.22).
narrative_ontology:measurement(market_naturalization__hybrid_reading_tr_t1990, market_naturalization__hybrid_reading, theater_ratio, 1990, 0.28).
narrative_ontology:measurement(market_naturalization__hybrid_reading_tr_t2000, market_naturalization__hybrid_reading, theater_ratio, 2000, 0.33).
narrative_ontology:measurement(market_naturalization__hybrid_reading_tr_t2010, market_naturalization__hybrid_reading, theater_ratio, 2010, 0.37).
narrative_ontology:measurement(market_naturalization__hybrid_reading_tr_t2020, market_naturalization__hybrid_reading, theater_ratio, 2020, 0.39).
narrative_ontology:measurement(market_naturalization__hybrid_reading_tr_t2024, market_naturalization__hybrid_reading, theater_ratio, 2024, 0.41).

% Extraction over time
narrative_ontology:measurement(market_naturalization__hybrid_reading_be_t1980, market_naturalization__hybrid_reading, base_extractiveness, 1980, 0.35).
narrative_ontology:measurement(market_naturalization__hybrid_reading_be_t1990, market_naturalization__hybrid_reading, base_extractiveness, 1990, 0.42).
narrative_ontology:measurement(market_naturalization__hybrid_reading_be_t2000, market_naturalization__hybrid_reading, base_extractiveness, 2000, 0.47).
narrative_ontology:measurement(market_naturalization__hybrid_reading_be_t2010, market_naturalization__hybrid_reading, base_extractiveness, 2010, 0.53).
narrative_ontology:measurement(market_naturalization__hybrid_reading_be_t2020, market_naturalization__hybrid_reading, base_extractiveness, 2020, 0.58).
narrative_ontology:measurement(market_naturalization__hybrid_reading_be_t2024, market_naturalization__hybrid_reading, base_extractiveness, 2024, 0.61).

% Suppression requirement over time
narrative_ontology:measurement(market_naturalization__hybrid_reading_su_t1980, market_naturalization__hybrid_reading, suppression_requirement, 1980, 0.38).
narrative_ontology:measurement(market_naturalization__hybrid_reading_su_t1990, market_naturalization__hybrid_reading, suppression_requirement, 1990, 0.41).
narrative_ontology:measurement(market_naturalization__hybrid_reading_su_t2000, market_naturalization__hybrid_reading, suppression_requirement, 2000, 0.44).
narrative_ontology:measurement(market_naturalization__hybrid_reading_su_t2010, market_naturalization__hybrid_reading, suppression_requirement, 2010, 0.47).
narrative_ontology:measurement(market_naturalization__hybrid_reading_su_t2020, market_naturalization__hybrid_reading, suppression_requirement, 2020, 0.51).
narrative_ontology:measurement(market_naturalization__hybrid_reading_su_t2024, market_naturalization__hybrid_reading, suppression_requirement, 2024, 0.53).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(market_naturalization__hybrid_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(market_naturalization__hybrid_reading, 0.15).
narrative_ontology:affects_constraint(market_naturalization__hybrid_reading, market_naturalization__lapsed_alternative_reading).
narrative_ontology:affects_constraint(market_naturalization__hybrid_reading, market_naturalization__beneficiary_maintained_reading).
narrative_ontology:affects_constraint(market_naturalization__hybrid_reading, intellectual_property_regime).
narrative_ontology:affects_constraint(market_naturalization__hybrid_reading, regulatory_capture_network).
narrative_ontology:affects_constraint(market_naturalization__hybrid_reading, platform_governance_architecture).

% DUAL FORMULATION NOTE:
% Kernel decomposition: market_naturalization splits into three readings. The hybrid reading acknowledges both lapsed mechanisms (network effects, scale economies, path dependence) and active maintenance (IP enforcement, regulatory capture, strategic litigation). The lapsed reading treats dominance as mountain/piton (natural law or degraded inertia). The beneficiary_maintained reading treats it as snare/tangled_rope with purely active extraction. All three share the referent 'market dominance structures' but differ in ε, beneficiary/victim structure, and enforcement profile.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(market_naturalization__hybrid_reading, institutional, 0.15).
constraint_indexing:directionality_override(market_naturalization__hybrid_reading, powerful, 0.75).
constraint_indexing:directionality_override(market_naturalization__hybrid_reading, moderate, 0.65).
constraint_indexing:directionality_override(market_naturalization__hybrid_reading, organized, 0.55).
constraint_indexing:directionality_override(market_naturalization__hybrid_reading, powerless, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
