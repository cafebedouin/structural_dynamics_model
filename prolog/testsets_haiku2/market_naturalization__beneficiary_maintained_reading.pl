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
 *   constraint_id: market_naturalization__beneficiary_maintained_reading
 *   human_readable: Market Dominance as Actively Defended Arrangement
 *   domain: political_economy/institutional_analysis
 *
 * SUMMARY:
 *   This constraint story instantiates the beneficiary-maintained reading of
 *   the market_naturalization kernel: market dominance is actively defended
 *   by incumbent capital holders through legal, political, and economic
 *   mechanisms. This reading asserts that dominant market position is not a
 *   natural outcome of superior efficiency or merit, but rather an
 *   arrangement constructed and maintained through specific enforcement
 *   machinery—patent litigation, regulatory capture, predatory pricing, union
 *   suppression, intellectual property regimes, and narrative production that
 *   naturalizes capital's dominance as inevitable. The claim and metrics are
 *   deliberately held in tension: the constraint is CLAIMED as tangled_rope
 *   (genuine coordination of markets, plus asymmetric extraction by dominant
 *   actors) while the authored metrics model high extractiveness and
 *   suppression—capturing the contest between the beneficiary framing
 *   (markets coordinate efficiently) and the critical reading (markets are
 *   engineered to maintain capital dominance). The engine computes whether
 *   this tension is resolved as real coordination or as cover story.
 *
 * KEY AGENTS:
 *   - incumbent_capital_holders: primary beneficiary and agenda-setter; accumulate supernormal profits from enforced dominance
 *   - alternative_market_entrants: powerless victims; face structural barriers engineered by incumbent dominance
 *   - displaced_labor: powerless victims; lose regional opportunity and generational pathway from consolidation and automation funded by monopoly rents
 *   - competing_institutional_forms: moderate payer; cooperatives, public firms, worker-owned models systematically suppressed by capital-favorable regulatory capture
 *   - state_regulatory_apparatus: institutional agenda-setter; provides enforcement infrastructure (property law, IP regimes, underfunded antitrust) that makes dominance defensible
 *   - labor_unions_and_collective_organizations: excluded; would provide countervailing power if institutional space permitted
 *   - subaltern_observers: analytical seat; document the active mechanisms of enforcement rather than natural competitive outcome
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(market_naturalization__beneficiary_maintained_reading, 0.78).
domain_priors:suppression_score(market_naturalization__beneficiary_maintained_reading, 0.82).
domain_priors:theater_ratio(market_naturalization__beneficiary_maintained_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(market_naturalization__beneficiary_maintained_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(market_naturalization__beneficiary_maintained_reading, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(market_naturalization__beneficiary_maintained_reading, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(market_naturalization__beneficiary_maintained_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(market_naturalization__beneficiary_maintained_reading, resistance, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(market_naturalization__beneficiary_maintained_reading, tangled_rope).
narrative_ontology:human_readable(market_naturalization__beneficiary_maintained_reading, "Market Dominance as Actively Defended Arrangement").
narrative_ontology:topic_domain(market_naturalization__beneficiary_maintained_reading, "political_economy/institutional_analysis").

domain_priors:requires_active_enforcement(market_naturalization__beneficiary_maintained_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(market_naturalization__beneficiary_maintained_reading, '923dbb02-3a31-4cbc-951e-e9bcdf96e367').
narrative_ontology:cs_kernel_codification('923dbb02-3a31-4cbc-951e-e9bcdf96e367', formalized).
narrative_ontology:cs_authority_grounding('923dbb02-3a31-4cbc-951e-e9bcdf96e367', extraction).
narrative_ontology:cs_interpretation_layer_present('923dbb02-3a31-4cbc-951e-e9bcdf96e367').
narrative_ontology:cs_reading_relation('923dbb02-3a31-4cbc-951e-e9bcdf96e367', market_naturalization__lapsed_alternative_reading, forecloses).
narrative_ontology:cs_reading_relation('923dbb02-3a31-4cbc-951e-e9bcdf96e367', market_naturalization__hybrid_reading, influences).
narrative_ontology:cs_axiom('923dbb02-3a31-4cbc-951e-e9bcdf96e367', foundational, market_dominance_requires_active_maintenance).
narrative_ontology:cs_axiom_status(market_dominance_requires_active_maintenance, holdable).
narrative_ontology:cs_axiom_grounding('923dbb02-3a31-4cbc-951e-e9bcdf96e367', market_dominance_requires_active_maintenance, empirically_contingent).
narrative_ontology:cs_axiom('923dbb02-3a31-4cbc-951e-e9bcdf96e367', secondary, capital_accumulation_justification_is_efficiency_claim).
narrative_ontology:cs_axiom_status(capital_accumulation_justification_is_efficiency_claim, overridden).
narrative_ontology:cs_axiom_grounding('923dbb02-3a31-4cbc-951e-e9bcdf96e367', capital_accumulation_justification_is_efficiency_claim, empirically_contingent).
narrative_ontology:cs_reference_frame('923dbb02-3a31-4cbc-951e-e9bcdf96e367', competitive_market_efficiency_doctrine).
narrative_ontology:cs_drift_state('923dbb02-3a31-4cbc-951e-e9bcdf96e367', contemporary_2025, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('923dbb02-3a31-4cbc-951e-e9bcdf96e367', '2026-06-12T14:32:51Z').
narrative_ontology:cs_kernel_id(market_naturalization__beneficiary_maintained_reading, market_naturalization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(market_naturalization__beneficiary_maintained_reading, incumbent_capital_holders).
narrative_ontology:constraint_victim(market_naturalization__beneficiary_maintained_reading, alternative_market_entrants).
narrative_ontology:constraint_victim(market_naturalization__beneficiary_maintained_reading, displaced_labor).
narrative_ontology:constraint_victim(market_naturalization__beneficiary_maintained_reading, competing_institutional_forms).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(market_naturalization__beneficiary_maintained_reading, consumer_base).
narrative_ontology:constraint_beneficiary(market_naturalization__beneficiary_maintained_reading, academic_and_policy_discourse).
narrative_ontology:constraint_victim(market_naturalization__beneficiary_maintained_reading, consumer_base).
narrative_ontology:constraint_vindicates(market_naturalization__beneficiary_maintained_reading, capital_accumulation_doctrine).
narrative_ontology:constraint_vindicates(market_naturalization__beneficiary_maintained_reading, competitive_efficiency_narrative).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Control the dominant institutional infrastructure (supply chains, regulatory capture, intellectual property regimes, network effects). Actively maintain market dominance through legal action, political influence, strategic underselling to competitors, control of essential inputs, and narrative production. Collect supernormal profits from market position. Their enforcement machinery—from patent litigation to lobbying—consumes substantial resources but is treated as 'business necessity' or 'competition' rather than as maintenance of an extractive arrangement.
narrative_ontology:constraint_stakeholder(market_naturalization__beneficiary_maintained_reading, incumbent_capital_holders, agenda_setter,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(market_naturalization__beneficiary_maintained_reading, incumbent_capital_holders, beneficiary).

% Face structural barriers to entry: incumbents control essential inputs, regulatory approval timelines are engineered to favor incumbents, network effects lock customers in, and predatory pricing eliminates new competitors before they reach scale. They bear the full cost of attempting entry (capital, legal defense, market education) while the market is actively shaped against them. Exit from the attempt means abandonment of invested capital; they cannot compete on merit because the competitive arena itself is engineered.
narrative_ontology:constraint_stakeholder(market_naturalization__beneficiary_maintained_reading, alternative_market_entrants, payer,
    powerless, biographical, trapped, regional).

% Loses employment, regional economic opportunity, and intergenerational pathway when incumbents consolidate production, offshore operations, or automate using capital accumulated from monopoly positions. Their labor market is constrained: local opportunities shrink, retraining costs are unsubsidized, and relocation demands break community bonds. They bear the externalized cost of maintaining dominant market structure.
narrative_ontology:constraint_stakeholder(market_naturalization__beneficiary_maintained_reading, displaced_labor, payer,
    powerless, biographical, constrained, local).

% Cooperative enterprises, public firms, worker-owned models, and state-directed alternatives are actively suppressed by capital's domination of regulatory frameworks, access to credit, and narrative framing. These organizational forms are not market failures but competing institutional arrangements; their exclusion requires active defense of the incumbent capital model. The institutional capture that maintains this suppression is substantial: tax code favors shareholder primacy, corporate law embeds fiduciary duties to capital, and policy discourse treats capitalism as natural inevitability rather than as one contestable institutional arrangement.
narrative_ontology:constraint_stakeholder(market_naturalization__beneficiary_maintained_reading, competing_institutional_forms, payer,
    moderate, generational, identity_locked, national).

% Receive standardized, branded products and services with genuine reliability and quality. They also carry indirect costs: monopoly pricing, reduced innovation from reduced competition, planned obsolescence maintained by intellectual property barriers, and ecosystem lock-in. Their choice set is bounded by what incumbent infrastructure permits to exist; they experience the dominant arrangement as 'the market' rather than as a constructed exclusion.
narrative_ontology:constraint_stakeholder(market_naturalization__beneficiary_maintained_reading, consumer_base, beneficiary,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(market_naturalization__beneficiary_maintained_reading, consumer_base, payer).

% Would mobilize collective countervailing power to negotiate wages, working conditions, and capital distribution if permitted institutional space. They are systematically excluded from meaningful governance of firms and markets through anti-union law, right-to-work statutes, outsourcing, and gig-economy restructuring. Their exclusion from the constraint's rule-setting is precisely what maintains market dominance as an arrangement benefiting capital.
narrative_ontology:constraint_stakeholder(market_naturalization__beneficiary_maintained_reading, labor_unions_and_collective_organizations, excluded,
    organized, generational, constrained, national).

% Provides the legal and enforcement infrastructure that makes incumbent dominance possible and defensible: property rights law, patent and intellectual property regimes, corporate law that privileges shareholder interests, antitrust enforcement that is underfunded or captured, trade agreements that lock in capital-favorable terms. The state enforces the rules that maintain market dominance while presenting itself as a neutral arbiter. Regulatory capture—where incumbent firms write the rules they are regulated by—is a structural feature, not a deviation.
narrative_ontology:constraint_stakeholder(market_naturalization__beneficiary_maintained_reading, state_regulatory_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).

% Economics, business schools, and policy institutions are substantially funded and shaped by incumbent capital. Market-efficiency narratives, competitive-dynamics models that treat monopoly as temporary deviation, and theories that naturalize capital accumulation are produced and reproduced in these spaces. The discourse benefits from the same capital that benefits from the market arrangement being naturalized as inevitable. This is not corruption but structural alignment: institutions funded by capital tend to produce theories legitimating capital's arrangement.
narrative_ontology:constraint_stakeholder(market_naturalization__beneficiary_maintained_reading, academic_and_policy_discourse, beneficiary,
    institutional, generational, identity_locked, global).

% Historians, heterodox economists, political ecologists, and structural analysts see the dominance not as natural market outcome but as constructed through active maintenance. They document the enforcement—litigation, lobbying, narrative production, regulatory capture—that sustains it. They are analytically positioned outside the arrangement but lack structural power to alter it.
narrative_ontology:constraint_stakeholder(market_naturalization__beneficiary_maintained_reading, subaltern_observers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(market_naturalization__beneficiary_maintained_reading, incumbent_capital_holders).
narrative_ontology:fixing_cost_class(market_naturalization__beneficiary_maintained_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Market coordination (price discovery, allocation, production incentives) under conditions where dominant actors set terms unilaterally. Coordinated efficiency is genuine—the market does allocate resources—but the distribution of gains and losses is not determined by neutral competitive process; it is engineered by those with dominant structural position.
% TRANSFER_FUNCTION: Transfers supernormal profits (rents and enforcement costs paid as consumer prices) from alternative market entrants, displaced labor, competing institutional forms, and the general public to incumbent capital holders. Transfers also flow from labor (via suppressed wages under threat of outsourcing) to capital. The magnitude of transfer is sustained by active legal and political defense of the dominant position.
% ABSENT_VOICES: Labor unions, worker-owned enterprises, cooperative structures, public firms, and subaltern institutional innovators are structurally excluded from meaningful participation in setting market rules. They would testify that market dominance is maintained through specific legal mechanisms (right-to-work laws, restrictions on unionization, corporate law hierarchy, intellectual property regimes) rather than emerging from neutral competition. Regulatory authorities in jurisdictions outside incumbent control (EU competition authorities, non-capitalist states) would attest to the active mechanisms of maintenance.
% DISAPPEARANCE_RATIONALE: If active defense of market dominance—patent litigation, regulatory capture, predatory pricing, union suppression, intellectual property enforcement—suddenly ceased, the market structure would shift within years: competing organizational forms would emerge, labor would recover collective bargaining capacity, new entrants would reach scale, and capital accumulation patterns would reorganize around genuine competitive constraint rather than structural dominance. The fact that dominance persists only under constant enforcement shows it is not natural outcome but constructed arrangement.
% FOUNDING_PROBLEM: In early competitive markets, larger firms accumulated capital and could invest in efficiency; the founding narrative treats this as natural selection. The reading reveals the founding problem differently: once capital accumulation reached a certain threshold, incumbent firms could capture regulatory space, suppress entry, and maintain dominance without superior product or service. The constraint exists to solve the problem of preventing that dominance from being challenged—not to solve any consumer or productive problem.
% FOUNDING_PROBLEM_CORROBORATION: Incumbent capital and mainstream economics attest the founding problem is solved: competitive markets work when government does not interfere. Alternative institutional theorists, labor historians, competition authorities in jurisdictions with tighter enforcement, and subaltern economic movements attest the founding problem was inverted—that the constraint exists precisely to prevent competitive challenge to accumulated dominance. Corroboration from outside benefiting parties: regulatory enforcement reports documenting predatory practices, union testimony on wage suppression under threat of outsourcing, case law on patent abuse, and structural analyses of barriers to entry.
narrative_ontology:disappearance_verdict(market_naturalization__beneficiary_maintained_reading, world_rearranges).
narrative_ontology:founding_problem_status(market_naturalization__beneficiary_maintained_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(market_naturalization__beneficiary_maintained_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(market_naturalization__beneficiary_maintained_reading, 'none', 1).
narrative_ontology:epsilon_provenance(market_naturalization__beneficiary_maintained_reading, 0.78, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is high (0.78 at present) and rising over the interval because dominant actors maintain position by capturing regulatory space, controlling essential inputs, and preventing competitive challenge—activities that do not create consumer value but transfer value from alternatives to incumbents. The temporal trajectory (1980: 0.54 → 2025: 0.78) shows extraction accumulating as financial consolidation, supply-chain integration, and institutional capture deepen. Suppression is higher still (0.82) because maintenance of dominance requires active exclusion: patent enforcement, union-busting legislation, antitrust capture, lobbying against entry-permitting regulation. Theater ratio (0.52 at present) indicates that roughly half of enforcement activity is now performative (justifying dominance as natural competition, reframing rent collection as innovation funding) rather than substantive coordination. Accessibility collapse (0.71 at structural level, 0.82 at endpoint) reflects how thoroughly the market is engineered to prevent alternatives from emerging: barriers to entry, network effects, intellectual property, regulatory timelines, and access to credit are all shaped by those already dominant. Resistance (declining from 0.71 to 0.62 at class level, 0.58 to 0.48 at structural level) shows how labor and alternative institutions are worn down by sustained suppression—not because their challenges lack merit, but because the asymmetry of institutional power is vast. The grid measurements capture the leveled structure: suppression is HIGHEST at the structural level (0.88 at endpoint)—the system itself is shaped to defend dominance—while individual resistance is dampened most. This is not distributed coercion but hierarchically concentrated: the system-level architecture defends what individual actors cannot resist.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seat (incumbent capital) and the payer seats should compute very differently from this structural data. From the beneficiary position: markets are competitive, dominance is temporary (destroyed by innovation and new entry), and enforcement is merely defending legitimate property rights. From the constrained-exit payer positions: the market is engineered, dominance is persistent (defended by capital's control of regulatory and legal machinery), and enforcement is extracting rents by preventing alternatives. Both cannot be simultaneously true of the same market; the engine computes which framing the structural data supports by testing whether the metrics and beneficiary/victim structure are consistent with coordinate vs. hierarchical extraction. If the constraint computes as tangled_rope or snare across seats, the critical reading's claim is supported. If it computes as rope across all seats despite high extraction metrics, the cover-story mechanism is flagged.
 *
 * DIRECTIONALITY LOGIC:
 *   Incumbent capital holders have high directionality toward full target-position (d near 1.0) relative to this constraint because they bear the enforcement cost of maintaining dominance—constant litigation, lobbying, regulatory monitoring, and narrative production are their burden. This is counterintuitive but structurally correct: the beneficiary must continuously defend the arrangement or it would collapse. However, they collect the supernormal profits FROM that enforcement, so they are simultaneously the beneficiary collecting the extraction. This is captured in the stakeholder's dual role: agenda_setter + beneficiary. The derivation would compute high d from their role as agenda-setter (executor of the constraint) but also recognize them as beneficiary (collector of rents), producing a mixed directionality. Alternative entrants face high d toward full target (d near 1.0) with no offsetting benefit—they are purely extractive targets. Displaced labor faces high d toward full target (d near 1.0) with constrained exit and no collection. Competing institutional forms face high d toward target with identity_locked exit (they are ideologically committed to an alternative model that the constraint actively suppresses). The asymmetry is stark: the beneficiary-executor bears enforcement cost but collects supernormal gains; the victims bear both prevention and cost with no collection.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (competitive markets rewarding efficiency) no longer matches the function (maintaining incumbent dominance against competitive threat). The constraint persists not because the founding problem is live but because the arrangement benefits identifiable parties (incumbent capital) who control the enforcement machinery. By 2025, the founding problem is dead: markets are not competitive arenas where superior performance is rewarded; they are hierarchical structures where dominant position is engineered. Yet the constraint persists with HIGHER enforcement intensity (suppression up from 0.58 to 0.82, theater up from 0.31 to 0.52). This is the signature of mandatrophy: the constraint's founding justification has been replaced by pure extraction, yet enforcement is intensifying because the beneficiaries have captured the institutions that could change the rules. The theater-ratio rise indicates that as the founding problem weakens, more enforcement is devoted to narrative production (reframing rent collection as innovation incentive) rather than to substantive coordination. This reading declares mandatrophy resolved: the constraint is no longer justified by market efficiency; it is justified by capital's interest in maintaining position.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    active_vs_structural_maintenance,
    'Is market dominance maintained by active, deliberate decisions by incumbent capital, or by structural arrangements that persist regardless of incumbent intention (structural path dependence)?',
    'Comparative analysis: jurisdictions where incumbent capital lacks political power but structural dominance persists (e.g., state-directed economies where former incumbents lose decision-making authority) vs. jurisdictions where structural barriers have been deliberately dismantled (e.g., breakups, antitrust enforcement, open standards regimes). If dominance collapses when active defense is removed, this reading is correct; if dominance persists despite loss of incumbent control, the structural reading explains it better.',
    'If active, the constraint is a snare maintained by identifiable beneficiaries who could choose to stop—remedies would target incumbent decision-making. If structural, the constraint persists regardless of incumbent intent—remedies would need to rebuild institutions, not just discipline incumbents. This reading claims active; the competing readings claim structural or mixed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(active_vs_structural_maintenance, empirical, 'Whether incumbent capital deliberately maintains dominance or it persists through structural inertia.').

omega_variable(
    suppression_mechanism_internalization,
    'Is the measured suppression (0.82) primarily structural (legal barriers, regulatory capture, access-to-capital restrictions) or primarily internalized (workers and entrants have absorbed the narrative that dominance is natural and do not attempt challenge)?',
    'Post-suppression-removal trajectories: when legal barriers are removed (e.g., union legalization, antitrust enforcement, intellectual property reform), do challenges immediately mount (structural suppression) or do they lag as people rebuild capacity and expectation (internalized suppression)? The lag time and the magnitude of post-removal mobilization would measure the internalization.',
    'If suppression is primarily structural, removing legal mechanisms would release substantial resistance. If primarily internalized, the constraint''s persistence depends on continuous narrative production and institutional design rather than legal force alone. The grid shows suppression highest at structural level (0.88) and lower at individual level (0.62 at endpoint), suggesting structural enforcement is more complete than internalized.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'The relative contribution of structural versus internalized suppression to the measured total.').

omega_variable(
    rent_magnitude_and_counterfactual,
    'What is the magnitude of supernormal profit incumbent capital extracts relative to what profit would be under genuinely competitive conditions? Is the difference large enough to justify the enforcement cost, or would incumbent capital accumulate faster under actual competition?',
    'Economic modeling of competitive counterfactual: what would happen to incumbent capital''s returns if entry barriers were removed, intellectual property were converted to open standards, and regulatory capture were ended. Empirical comparison with jurisdictions where these conditions partially hold (EU market structure under tighter competition law; open-source software markets; jurisdictions with strong labor law).',
    'If supernormal profit vastly exceeds competitive profit, this reading is correct: capital defends dominance because the rents are worth defending. If competitive conditions would produce equal or higher capital returns through lower enforcement cost and larger market, the reading is incorrect—capital would not actively defend dominance.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(rent_magnitude_and_counterfactual, empirical, 'Whether the extractive rents from dominance justify the enforcement cost incurred.').

omega_variable(
    reading_divergence_from_hybrid_and_lapsed,
    'Does this beneficiary_maintained reading foreclose the hybrid_reading and lapsed_alternative_reading, or do all three readings remain logically live within their respective analytical frameworks?',
    'The reading_relations fields in cs_structure declare forecloses/coexists_with/influences. If this reading CLAIMS active maintenance is the core fact, does that logically rule out the hybrid reading (which claims both lapsed and active elements coexist)? Does it rule out the lapsed reading (which claims dominance persists from inertia, not active defense)?',
    'If forecloses: only one reading can be right, and the structural analysis must choose. If coexists_with: different parties or analytical traditions hold different readings, and the divergence is not a logic error but a political question. If influences: this reading creates pressure on the others (shifts the institutional conditions they operate in) but does not eliminate them.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_divergence_from_hybrid_and_lapsed, conceptual, 'The logical relationship between this reading and its kernel siblings.').

omega_variable(
    narrative_production_as_enforcement,
    'Is the rising theater_ratio (0.31 to 0.52 over the interval) evidence that enforcement is shifting toward narrative production and away from substantive coercion, or is it evidence that narrative production WAS ALWAYS the dominant enforcement mechanism, with legal/political enforcement as the visible substrate?',
    'Historical analysis of enforcement machinery: What percentage of incumbent capital''s enforcement budget goes to litigation vs. lobbying vs. public relations vs. regulatory consultation? What percentage of regulatory compliance among subordinate actors is driven by legal threat vs. internalized belief that dominance is natural/necessary? Comparison with periods when narrative coherence was stronger (theater_ratio lower): what actual enforcement was occurring then?',
    'If theater_ratio rise indicates narrative substitution for coercion, the constraint is transitioning toward cultural maintenance (Piton risk). If narrative was always the dominant enforcement mechanism, the structural coercion is masked but not replaced—legal and political machinery remain the real drivers. This reading assumes the latter: capital actively defends dominance through multiple channels, and narrative production is a tool OF enforcement, not a replacement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(narrative_production_as_enforcement, empirical, 'Whether rising theater_ratio indicates functional substitution or structural masking.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(market_naturalization__beneficiary_maintained_reading, 1980, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mark_tr_t1980, market_naturalization__beneficiary_maintained_reading, theater_ratio, 1980, 0.31).
narrative_ontology:measurement_basis(mark_tr_t1980, observed).
narrative_ontology:measurement(mark_tr_t1990, market_naturalization__beneficiary_maintained_reading, theater_ratio, 1990, 0.38).
narrative_ontology:measurement_basis(mark_tr_t1990, observed).
narrative_ontology:measurement(mark_tr_t2000, market_naturalization__beneficiary_maintained_reading, theater_ratio, 2000, 0.43).
narrative_ontology:measurement_basis(mark_tr_t2000, observed).
narrative_ontology:measurement(mark_tr_t2010, market_naturalization__beneficiary_maintained_reading, theater_ratio, 2010, 0.48).
narrative_ontology:measurement_basis(mark_tr_t2010, observed).
narrative_ontology:measurement(mark_tr_t2018, market_naturalization__beneficiary_maintained_reading, theater_ratio, 2018, 0.5).
narrative_ontology:measurement_basis(mark_tr_t2018, observed).
narrative_ontology:measurement(mark_tr_t2025, market_naturalization__beneficiary_maintained_reading, theater_ratio, 2025, 0.52).
narrative_ontology:measurement_basis(mark_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(mark_be_t1980, market_naturalization__beneficiary_maintained_reading, base_extractiveness, 1980, 0.54).
narrative_ontology:measurement_basis(mark_be_t1980, observed).
narrative_ontology:measurement(mark_be_t1990, market_naturalization__beneficiary_maintained_reading, base_extractiveness, 1990, 0.62).
narrative_ontology:measurement_basis(mark_be_t1990, observed).
narrative_ontology:measurement(mark_be_t2000, market_naturalization__beneficiary_maintained_reading, base_extractiveness, 2000, 0.68).
narrative_ontology:measurement_basis(mark_be_t2000, observed).
narrative_ontology:measurement(mark_be_t2010, market_naturalization__beneficiary_maintained_reading, base_extractiveness, 2010, 0.74).
narrative_ontology:measurement_basis(mark_be_t2010, observed).
narrative_ontology:measurement(mark_be_t2018, market_naturalization__beneficiary_maintained_reading, base_extractiveness, 2018, 0.76).
narrative_ontology:measurement_basis(mark_be_t2018, observed).
narrative_ontology:measurement(mark_be_t2025, market_naturalization__beneficiary_maintained_reading, base_extractiveness, 2025, 0.78).
narrative_ontology:measurement_basis(mark_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(mark_su_t1980, market_naturalization__beneficiary_maintained_reading, suppression_requirement, 1980, 0.58).
narrative_ontology:measurement_basis(mark_su_t1980, observed).
narrative_ontology:measurement(mark_su_t1990, market_naturalization__beneficiary_maintained_reading, suppression_requirement, 1990, 0.66).
narrative_ontology:measurement_basis(mark_su_t1990, observed).
narrative_ontology:measurement(mark_su_t2000, market_naturalization__beneficiary_maintained_reading, suppression_requirement, 2000, 0.72).
narrative_ontology:measurement_basis(mark_su_t2000, observed).
narrative_ontology:measurement(mark_su_t2010, market_naturalization__beneficiary_maintained_reading, suppression_requirement, 2010, 0.78).
narrative_ontology:measurement_basis(mark_su_t2010, observed).
narrative_ontology:measurement(mark_su_t2018, market_naturalization__beneficiary_maintained_reading, suppression_requirement, 2018, 0.8).
narrative_ontology:measurement_basis(mark_su_t2018, observed).
narrative_ontology:measurement(mark_su_t2025, market_naturalization__beneficiary_maintained_reading, suppression_requirement, 2025, 0.82).
narrative_ontology:measurement_basis(mark_su_t2025, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=1980, tn=2025
narrative_ontology:measurement(mark_grid_01, market_naturalization__beneficiary_maintained_reading, accessibility_collapse(class), 1980, 0.71).
narrative_ontology:measurement(mark_grid_02, market_naturalization__beneficiary_maintained_reading, accessibility_collapse(class), 2025, 0.8).
narrative_ontology:measurement(mark_grid_03, market_naturalization__beneficiary_maintained_reading, accessibility_collapse(individual), 1980, 0.58).
narrative_ontology:measurement(mark_grid_04, market_naturalization__beneficiary_maintained_reading, accessibility_collapse(individual), 2025, 0.68).
narrative_ontology:measurement(mark_grid_05, market_naturalization__beneficiary_maintained_reading, accessibility_collapse(organizational), 1980, 0.62).
narrative_ontology:measurement(mark_grid_06, market_naturalization__beneficiary_maintained_reading, accessibility_collapse(organizational), 2025, 0.76).
narrative_ontology:measurement(mark_grid_07, market_naturalization__beneficiary_maintained_reading, accessibility_collapse(structural), 1980, 0.74).
narrative_ontology:measurement(mark_grid_08, market_naturalization__beneficiary_maintained_reading, accessibility_collapse(structural), 2025, 0.82).
narrative_ontology:measurement(mark_grid_09, market_naturalization__beneficiary_maintained_reading, resistance(class), 1980, 0.71).
narrative_ontology:measurement(mark_grid_10, market_naturalization__beneficiary_maintained_reading, resistance(class), 2025, 0.62).
narrative_ontology:measurement(mark_grid_11, market_naturalization__beneficiary_maintained_reading, resistance(individual), 1980, 0.52).
narrative_ontology:measurement(mark_grid_12, market_naturalization__beneficiary_maintained_reading, resistance(individual), 2025, 0.38).
narrative_ontology:measurement(mark_grid_13, market_naturalization__beneficiary_maintained_reading, resistance(organizational), 1980, 0.68).
narrative_ontology:measurement(mark_grid_14, market_naturalization__beneficiary_maintained_reading, resistance(organizational), 2025, 0.54).
narrative_ontology:measurement(mark_grid_15, market_naturalization__beneficiary_maintained_reading, resistance(structural), 1980, 0.58).
narrative_ontology:measurement(mark_grid_16, market_naturalization__beneficiary_maintained_reading, resistance(structural), 2025, 0.48).
narrative_ontology:measurement(mark_grid_17, market_naturalization__beneficiary_maintained_reading, stakes_inflation(class), 1980, 0.38).
narrative_ontology:measurement(mark_grid_18, market_naturalization__beneficiary_maintained_reading, stakes_inflation(class), 2025, 0.58).
narrative_ontology:measurement(mark_grid_19, market_naturalization__beneficiary_maintained_reading, stakes_inflation(individual), 1980, 0.42).
narrative_ontology:measurement(mark_grid_20, market_naturalization__beneficiary_maintained_reading, stakes_inflation(individual), 2025, 0.63).
narrative_ontology:measurement(mark_grid_21, market_naturalization__beneficiary_maintained_reading, stakes_inflation(organizational), 1980, 0.51).
narrative_ontology:measurement(mark_grid_22, market_naturalization__beneficiary_maintained_reading, stakes_inflation(organizational), 2025, 0.74).
narrative_ontology:measurement(mark_grid_23, market_naturalization__beneficiary_maintained_reading, stakes_inflation(structural), 1980, 0.54).
narrative_ontology:measurement(mark_grid_24, market_naturalization__beneficiary_maintained_reading, stakes_inflation(structural), 2025, 0.72).
narrative_ontology:measurement(mark_grid_25, market_naturalization__beneficiary_maintained_reading, suppression(class), 1980, 0.58).
narrative_ontology:measurement(mark_grid_26, market_naturalization__beneficiary_maintained_reading, suppression(class), 2025, 0.81).
narrative_ontology:measurement(mark_grid_27, market_naturalization__beneficiary_maintained_reading, suppression(individual), 1980, 0.48).
narrative_ontology:measurement(mark_grid_28, market_naturalization__beneficiary_maintained_reading, suppression(individual), 2025, 0.62).
narrative_ontology:measurement(mark_grid_29, market_naturalization__beneficiary_maintained_reading, suppression(organizational), 1980, 0.61).
narrative_ontology:measurement(mark_grid_30, market_naturalization__beneficiary_maintained_reading, suppression(organizational), 2025, 0.83).
narrative_ontology:measurement(mark_grid_31, market_naturalization__beneficiary_maintained_reading, suppression(structural), 1980, 0.66).
narrative_ontology:measurement(mark_grid_32, market_naturalization__beneficiary_maintained_reading, suppression(structural), 2025, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(market_naturalization__beneficiary_maintained_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(market_naturalization__beneficiary_maintained_reading, 0.18).
narrative_ontology:affects_constraint(market_naturalization__beneficiary_maintained_reading, market_naturalization__lapsed_alternative_reading).
narrative_ontology:affects_constraint(market_naturalization__beneficiary_maintained_reading, market_naturalization__hybrid_reading).
narrative_ontology:affects_constraint(market_naturalization__beneficiary_maintained_reading, labor_suppression_as_feature).
narrative_ontology:affects_constraint(market_naturalization__beneficiary_maintained_reading, intellectual_property_as_barrier).
narrative_ontology:affects_constraint(market_naturalization__beneficiary_maintained_reading, regulatory_capture_in_energy).

% DUAL FORMULATION NOTE:
% The market_naturalization kernel decomposes into three structurally distinct readings: this beneficiary_maintained reading (active defense, identifiable beneficiaries, high extraction), the lapsed_alternative_reading (dominance persists from inertia without active maintenance), and the hybrid_reading (both active and lapsed elements). Each reading has its own ε, beneficiary/victim structure, and temporal trajectory. This reading instantiates the interpretation where incumbent capital continuously defends position; the sibling readings test whether that defense is necessary or whether dominance would persist regardless. The three stories are linked via affects_constraints to permit comparative analysis and mismatch detection when empirical findings show active enforcement lower/higher than the reading claims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(market_naturalization__beneficiary_maintained_reading, institutional, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
