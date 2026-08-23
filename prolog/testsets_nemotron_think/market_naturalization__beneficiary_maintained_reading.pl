% ============================================================================
% CONSTRAINT STORY: market_naturalization__beneficiary_maintained_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_market_naturalization__beneficiary_maintained_reading, []).

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
 *   constraint_id: market_naturalization__beneficiary_maintained_reading
 *   human_readable: Market Naturalization — Beneficiary Maintained Reading
 *   domain: political_economy/economic_history/institutional_analysis
 *
 * SUMMARY:
 *   This constraint story instantiates the 'beneficiary_maintained_reading'
 *   of the market_naturalization kernel. The reading asserts that what
 *   presents itself as the natural, efficient outcome of competitive markets
 *   — concentration, dominance, supra-normal returns — is in fact actively
 *   constructed and defended by the identifiable class that benefits from it.
 *   The coordination story (efficiency, scale, innovation) is the cover; the
 *   operational reality is rent extraction enforced through political
 *   capture, regulatory moats, and structural barriers. The constraint is not
 *   a law of economics but a political-economic arrangement maintained by
 *   continuous investment in suppression.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(market_naturalization__beneficiary_maintained_reading, 0.82).
domain_priors:suppression_score(market_naturalization__beneficiary_maintained_reading, 0.85).
domain_priors:theater_ratio(market_naturalization__beneficiary_maintained_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(market_naturalization__beneficiary_maintained_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(market_naturalization__beneficiary_maintained_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(market_naturalization__beneficiary_maintained_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(market_naturalization__beneficiary_maintained_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(market_naturalization__beneficiary_maintained_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(market_naturalization__beneficiary_maintained_reading, snare).
narrative_ontology:human_readable(market_naturalization__beneficiary_maintained_reading, "Market Naturalization — Beneficiary Maintained Reading").
narrative_ontology:topic_domain(market_naturalization__beneficiary_maintained_reading, "political_economy/economic_history/institutional_analysis").

domain_priors:requires_active_enforcement(market_naturalization__beneficiary_maintained_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(market_naturalization__beneficiary_maintained_reading, 'b621c215-0550-4d58-b34d-548926f30c71').
narrative_ontology:cs_kernel_codification('b621c215-0550-4d58-b34d-548926f30c71', distributed).
narrative_ontology:cs_authority_grounding('b621c215-0550-4d58-b34d-548926f30c71', extraction).
narrative_ontology:cs_interpretation_layer_present('b621c215-0550-4d58-b34d-548926f30c71').
narrative_ontology:cs_reading_relation('b621c215-0550-4d58-b34d-548926f30c71', market_naturalization__lapsed_alternative_reading, influences).
narrative_ontology:cs_reading_relation('b621c215-0550-4d58-b34d-548926f30c71', market_naturalization__hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('b621c215-0550-4d58-b34d-548926f30c71', foundational, market_dominance_requires_active_defense).
narrative_ontology:cs_axiom_status(market_dominance_requires_active_defense, holdable).
narrative_ontology:cs_axiom_grounding('b621c215-0550-4d58-b34d-548926f30c71', market_dominance_requires_active_defense, empirically_contingent).
narrative_ontology:cs_axiom('b621c215-0550-4d58-b34d-548926f30c71', foundational, incumbent_capital_holders_capture_regulatory_process).
narrative_ontology:cs_axiom_status(incumbent_capital_holders_capture_regulatory_process, holdable).
narrative_ontology:cs_axiom_grounding('b621c215-0550-4d58-b34d-548926f30c71', incumbent_capital_holders_capture_regulatory_process, empirically_contingent).
narrative_ontology:cs_axiom('b621c215-0550-4d58-b34d-548926f30c71', secondary, natural_market_narrative_is_constructed_legitimation).
narrative_ontology:cs_axiom_status(natural_market_narrative_is_constructed_legitimation, holdable).
narrative_ontology:cs_axiom_grounding('b621c215-0550-4d58-b34d-548926f30c71', natural_market_narrative_is_constructed_legitimation, empirically_contingent).
narrative_ontology:cs_reference_frame('b621c215-0550-4d58-b34d-548926f30c71', natural_market_equilibrium).
narrative_ontology:cs_drift_state('b621c215-0550-4d58-b34d-548926f30c71', contemporary_antitrust_revival, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('b621c215-0550-4d58-b34d-548926f30c71', '').
narrative_ontology:cs_kernel_id(market_naturalization__beneficiary_maintained_reading, market_naturalization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(market_naturalization__beneficiary_maintained_reading, incumbent_capital_holders).
narrative_ontology:constraint_beneficiary(market_naturalization__beneficiary_maintained_reading, dominant_firms).
narrative_ontology:constraint_beneficiary(market_naturalization__beneficiary_maintained_reading, financial_intermediaries).
narrative_ontology:constraint_victim(market_naturalization__beneficiary_maintained_reading, potential_entrants).
narrative_ontology:constraint_victim(market_naturalization__beneficiary_maintained_reading, competitors).
narrative_ontology:constraint_victim(market_naturalization__beneficiary_maintained_reading, consumers).
narrative_ontology:constraint_victim(market_naturalization__beneficiary_maintained_reading, workers).
narrative_ontology:constraint_victim(market_naturalization__beneficiary_maintained_reading, small_businesses).
narrative_ontology:constraint_vindicates(market_naturalization__beneficiary_maintained_reading, market_efficiency_doctrine).
narrative_ontology:constraint_vindicates(market_naturalization__beneficiary_maintained_reading, natural_monopoly_theory).
narrative_ontology:constraint_vindicates(market_naturalization__beneficiary_maintained_reading, shareholder_primacy_norm).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Own controlling stakes in dominant firms across sectors. Fund think tanks, lobbyists, and political campaigns to shape competition policy, trade rules, and regulatory frameworks. Use capital mobility to threaten disinvestment if policy turns unfavorable. Directly benefit from supra-competitive returns sustained by barriers they help erect.
narrative_ontology:constraint_stakeholder(market_naturalization__beneficiary_maintained_reading, incumbent_capital_holders, agenda_setter,
    institutional, generational, arbitrage, global).

% Enjoy market power that yields persistent economic rents. Deploy profits to acquire potential rivals, lobby for regulatory moats, and shape industry standards. Their CEOs and boards rotate through policy advisory roles. Constrained exit: they cannot easily shed the political dependencies that sustain their position without losing the rents.
narrative_ontology:constraint_stakeholder(market_naturalization__beneficiary_maintained_reading, dominant_firms, beneficiary,
    institutional, biographical, constrained, global).

% Investment banks, private equity, and asset managers extract fees from consolidation transactions, manage capital flows that discipline dissenting firms, and advise on regulatory strategy. Mobile exit: they can shift allocation across sectors and geographies, but their business model depends on the overall architecture of concentrated markets.
narrative_ontology:constraint_stakeholder(market_naturalization__beneficiary_maintained_reading, financial_intermediaries, beneficiary,
    organized, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(market_naturalization__beneficiary_maintained_reading, financial_intermediaries, agenda_setter).

% Face artificially elevated entry barriers: regulatory compliance costs scaled for incumbents, patent thickets, predatory pricing threats, and capital market exclusion. Many never form; those that attempt entry face coordinated retaliation. Trapped: the structure itself prevents their emergence as viable agents.
narrative_ontology:constraint_stakeholder(market_naturalization__beneficiary_maintained_reading, potential_entrants, payer,
    powerless, immediate, trapped, national).

% Surviving fringe firms pay the 'competition tax' — accepting lower margins, ceding prime segments, or selling to dominants. Exit is constrained: selling out realizes the extraction; fighting drains resources; niche survival depends on not threatening the core arrangement.
narrative_ontology:constraint_stakeholder(market_naturalization__beneficiary_maintained_reading, competitors, payer,
    moderate, biographical, constrained, national).

% Pay monopoly prices, accept reduced quality and choice, and lose privacy/data to dominant platforms. Class actions and consumer advocacy exist but face collective action problems and captured enforcement. Constrained exit: switching costs, network effects, and universal adoption of extractive terms limit meaningful choice.
narrative_ontology:constraint_stakeholder(market_naturalization__beneficiary_maintained_reading, consumers, payer,
    organized, biographical, constrained, global).

% Face monopsony power in labor markets: wage suppression, non-compete enforcement, algorithmic management, and eroded bargaining power. Union density has been systematically dismantled by the same political architecture. Constrained exit: geographic mobility is costly; skill specificity locks them into dominated sectors.
narrative_ontology:constraint_stakeholder(market_naturalization__beneficiary_maintained_reading, workers, payer,
    powerless, biographical, constrained, national).

% Squeezed between dominant suppliers and dominant buyers/platforms. Pay rents through platform fees, supplier price discrimination, and regulatory compliance disproportionate to scale. Constrained exit: local embeddedness and relationship capital make pivot impossible; acquisition by dominants is the only liquidity event.
narrative_ontology:constraint_stakeholder(market_naturalization__beneficiary_maintained_reading, small_businesses, payer,
    moderate, biographical, constrained, regional).

% Tasked with enforcing antitrust but operate within doctrinal frameworks (consumer welfare standard) shaped by the beneficiary class. Chronic under-resourcing, revolving-door capture, and judicial hostility limit effectiveness. Analytical seat: they see the structure but their mandate and tools are calibrated to the cover story.
narrative_ontology:constraint_stakeholder(market_naturalization__beneficiary_maintained_reading, competition_authorities, observer,
    institutional, generational, analytical, national).

% Produce evidence that concentration harms growth, innovation, and equality. Excluded from mainstream journals, central bank advisory roles, and policy circles dominated by neoclassical orthodoxy. Identity-locked: their professional identity is constituted through opposition to the dominant paradigm; exit means abandoning their intellectual project.
narrative_ontology:constraint_stakeholder(market_naturalization__beneficiary_maintained_reading, heterodox_economists, excluded,
    moderate, generational, identity_locked, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Claims to coordinate efficient resource allocation through scale economies, risk pooling, and market discipline — the 'natural' outcome of competitive processes.
% TRANSFER_FUNCTION: Moves monopoly rents, supra-competitive profits, political influence, and regulatory capture proceeds from consumers, workers, competitors, and potential entrants to incumbent capital holders, dominant firms, and financial intermediaries.
% ABSENT_VOICES: Potential entrants who never form (structural non-existence), workers in monopsony labor markets (organized out of the conversation), consumers in captured regulatory proceedings (represented by captured agents), Global South economies subjected to IP and investment regimes they did not negotiate — all would object if present.
% DISAPPEARANCE_RATIONALE: If active defense of market dominance vanished overnight: barriers to entry would collapse, patent thickets would be challenged, predatory pricing would be prosecuted, labor markets would rebalance, platform fees would face competition, and the political architecture sustaining concentration would lose its material base. The economic structure would reorganize toward lower concentration, lower rents, and distributed bargaining power.
% FOUNDING_PROBLEM: Post-Gilded Age legitimation crisis: concentrated capital needed a theoretical and legal framework to survive democratic and antitrust challenges. The 'natural market' narrative — that dominance reflects efficiency, not power — was constructed by corporate lawyers, neoclassical economists, and captured courts to convert political vulnerability into scientific inevitability.
% FOUNDING_PROBLEM_CORROBORATION: Historical institutionalists (Skowronek on state-building, Hacker & Pierson on drift, Blyth on austerity) document the deliberate construction of market naturalization as a legitimation project. No credible source outside the beneficiary class (corporate law firms, Chicago School economics, business-funded think tanks) argues the founding legitimation crisis remains live. The arrangement persists because it pays, not because the problem it solved still exists.
narrative_ontology:disappearance_verdict(market_naturalization__beneficiary_maintained_reading, world_rearranges).
narrative_ontology:founding_problem_status(market_naturalization__beneficiary_maintained_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(market_naturalization__beneficiary_maintained_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(market_naturalization__beneficiary_maintained_reading, 'none', 1).
narrative_ontology:epsilon_provenance(market_naturalization__beneficiary_maintained_reading, 0.82, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(market_naturalization__beneficiary_maintained_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(market_naturalization__beneficiary_maintained_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(market_naturalization__beneficiary_maintained_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.82) reflects the sum of monopoly rents, monopsony wage suppression, platform fees, and the enforcement costs borne by the whole economy — all flowing to a narrow beneficiary class. Suppression (0.85) is high because the arrangement requires active maintenance: lobbying, litigation, regulatory capture, judicial appointments, think tank funding, and media narrative control. Theater (0.48) captures the performative 'competition' — merger reviews that approve 99% of deals, efficiency defenses that never face empirical test, innovation rhetoric that masks rent-seeking. Accessibility collapse (0.78) measures how completely alternatives (decentralized markets, public utility models, cooperative ownership, antimonopoly enforcement) become unthinkable once the naturalization frame is internalized. Resistance (0.62) is real but fragmented: antitrust revival, labor organizing, platform regulation, Global South resistance — all face the same coordinated suppression.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat, the arrangement looks like 'protecting the gains from innovation' — a coordination function they built and maintain. From the payer seats, the same structure operates as enforced extraction with no coordination benefit they can access. The engine computes this divergence from the structural data; the authored claim (snare) reflects the payer-seat reality while the cover story (rope/mountain) reflects the agenda-setter projection.
 *
 * DIRECTIONALITY LOGIC:
 *   Incumbent capital holders and dominant firms are structural beneficiaries (d ~ 0.1-0.2): they collect the rents, set the rules, and hold arbitrage-grade exit. Financial intermediaries sit slightly higher (d ~ 0.3) — they benefit but depend on the architecture. Potential entrants are fully trapped (d ~ 0.95): they never emerge. Competitors, consumers, workers, and small businesses are constrained payers (d ~ 0.7-0.85): they bear costs with limited exit. Competition authorities are analytical observers (d ~ 0.5) but their tools are calibrated to the cover story. Heterodox economists are identity-locked excluded (d ~ 0.9): their professional existence depends on the constraint's persistence as an object of critique.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding legitimation crisis (Gilded Age concentration vs. democracy) is dead — the political threat that motivated the naturalization project has been neutralized by the project itself. Yet the arrangement not only persists but intensifies (rising extractiveness, rising suppression). This is mandatrophy: the mandate (legitimate concentrated capital) outlived its function (survive antitrust), and the machinery now runs purely for extraction. The classification as snare — not rope, not tangled_rope — captures this: the coordination story is entirely cover; the suppression is the point.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    naturalness_vs_construction_ambiguity,
    'Is the observed market concentration a genuine emergent property of competitive processes (mountain) or a constructed outcome of deliberate political-economic choices (snare)?',
    'Counterfactual historical analysis: trace specific policy choices (antitrust non-enforcement, IP regime expansion, financial deregulation, trade rules) and measure their causal contribution to concentration vs. ''natural'' scale economies. If concentration reverses when active maintenance is removed (e.g., post-breakup, post-deregulation reversal), the constructed reading is vindicated.',
    'If natural, the constraint is a mountain (ε ≈ 0) and the beneficiary_maintained_reading is false. If constructed, the constraint is a snare (ε high) and the lapsed_alternative_reading is false. The hybrid_reading survives either resolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(naturalness_vs_construction_ambiguity, empirical, 'Whether market concentration is a natural law or a constructed arrangement maintained by beneficiaries.').

omega_variable(
    coordination_extraction_boundary,
    'Does the ''efficiency'' coordination function have any independent existence, or is it entirely a cover story for extraction?',
    'Decompose claimed efficiencies (scale economies, scope economies, risk pooling, innovation incentives) and test whether they: (a) exist at the observed scale of concentration, (b) require the specific institutional form (investor-owned corporation, patent monopoly, platform dominance), (c) survive when extraction is removed (e.g., via mandated interoperability, public utility regulation, cooperative conversion).',
    'If coordination function is real and non-trivial, the constraint may be tangled_rope (hybrid). If coordination is entirely cover, snare classification is confirmed. This boundary determines whether any ''baby'' is thrown out with the ''bathwater'' in remedial policy.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_boundary, conceptual, 'Whether the constraint''s claimed coordination function is structurally real or purely performative cover.').

omega_variable(
    suppression_mechanism_composition,
    'What proportion of the measured suppression is structural (legal barriers, capital requirements, network effects) vs. internalized (ideological capture, professional identity, cognitive frames that make alternatives unthinkable)?',
    'Post-reform suppression trajectory: in jurisdictions or sectors where structural barriers are lowered (e.g., telecom unbundling, open banking mandates), measure whether suppression persists via internalized frames. If market actors continue self-censoring, innovating only within dominant paradigms, or treating concentration as inevitable, the internalized component is large.',
    'If suppression is substantially internalized, the constraint''s effective suppression is higher than structural measures suggest — the target carries the suppression after structural exit. This raises the snare''s persistence and lowers the efficacy of purely structural remedies.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_composition, empirical, 'Structural vs. internalized composition of suppression in the market naturalization constraint.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(market_naturalization__beneficiary_maintained_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mnmbr_tr_t0, market_naturalization__beneficiary_maintained_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(mnmbr_tr_t10, market_naturalization__beneficiary_maintained_reading, theater_ratio, 10, 0.3).
narrative_ontology:measurement(mnmbr_tr_t20, market_naturalization__beneficiary_maintained_reading, theater_ratio, 20, 0.38).
narrative_ontology:measurement(mnmbr_tr_t30, market_naturalization__beneficiary_maintained_reading, theater_ratio, 30, 0.43).
narrative_ontology:measurement(mnmbr_tr_t40, market_naturalization__beneficiary_maintained_reading, theater_ratio, 40, 0.46).
narrative_ontology:measurement(mnmbr_tr_t50, market_naturalization__beneficiary_maintained_reading, theater_ratio, 50, 0.48).

% Extraction over time
narrative_ontology:measurement(mnmbr_be_t0, market_naturalization__beneficiary_maintained_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(mnmbr_be_t10, market_naturalization__beneficiary_maintained_reading, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(mnmbr_be_t20, market_naturalization__beneficiary_maintained_reading, base_extractiveness, 20, 0.61).
narrative_ontology:measurement(mnmbr_be_t30, market_naturalization__beneficiary_maintained_reading, base_extractiveness, 30, 0.71).
narrative_ontology:measurement(mnmbr_be_t40, market_naturalization__beneficiary_maintained_reading, base_extractiveness, 40, 0.78).
narrative_ontology:measurement(mnmbr_be_t50, market_naturalization__beneficiary_maintained_reading, base_extractiveness, 50, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(mnmbr_su_t0, market_naturalization__beneficiary_maintained_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(mnmbr_su_t10, market_naturalization__beneficiary_maintained_reading, suppression_requirement, 10, 0.62).
narrative_ontology:measurement(mnmbr_su_t20, market_naturalization__beneficiary_maintained_reading, suppression_requirement, 20, 0.71).
narrative_ontology:measurement(mnmbr_su_t30, market_naturalization__beneficiary_maintained_reading, suppression_requirement, 30, 0.78).
narrative_ontology:measurement(mnmbr_su_t40, market_naturalization__beneficiary_maintained_reading, suppression_requirement, 40, 0.82).
narrative_ontology:measurement(mnmbr_su_t50, market_naturalization__beneficiary_maintained_reading, suppression_requirement, 50, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(market_naturalization__beneficiary_maintained_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(market_naturalization__beneficiary_maintained_reading, 0.15).
narrative_ontology:affects_constraint(market_naturalization__beneficiary_maintained_reading, market_naturalization__lapsed_alternative_reading).
narrative_ontology:affects_constraint(market_naturalization__beneficiary_maintained_reading, market_naturalization__hybrid_reading).
narrative_ontology:affects_constraint(market_naturalization__beneficiary_maintained_reading, intellectual_property_regime).
narrative_ontology:affects_constraint(market_naturalization__beneficiary_maintained_reading, financial_deregulation_constraint).
narrative_ontology:affects_constraint(market_naturalization__beneficiary_maintained_reading, labor_market_monopsony_constraint).

% DUAL FORMULATION NOTE:
% The market_naturalization kernel decomposes into three readings with distinct ε values: beneficiary_maintained (ε=0.82, snare), lapsed_alternative (ε≈0.15, piton/mountain), hybrid (ε≈0.45, tangled_rope). They are linked because the lapsed elements provide cover for the active maintenance; the hybrid reading captures their interaction. This story is the high-extraction pole of the family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(market_naturalization__beneficiary_maintained_reading, institutional, 0.15).
constraint_indexing:directionality_override(market_naturalization__beneficiary_maintained_reading, organized, 0.35).
constraint_indexing:directionality_override(market_naturalization__beneficiary_maintained_reading, powerless, 0.92).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
