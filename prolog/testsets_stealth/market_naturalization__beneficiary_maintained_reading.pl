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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
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
 *   human_readable: Actively Defended Market Dominance (Beneficiary-Maintained Reading)
 *   domain: political economy / economic history / institutional analysis
 *
 * SUMMARY:
 *   Since roughly 1980, market concentration has risen across sector after
 *   sector while the public account of the pattern holds that winners simply
 *   earned their positions. This story authors the beneficiary-maintained
 *   account of that standing arrangement: dominance persists because
 *   identifiable capital holders actively defend it, through lobbying,
 *   regulatory capture, antitrust doctrine shaped toward permissiveness,
 *   intellectual-property thickening, non-compete enforcement, and an
 *   epistemic apparatus that portrays concentrated outcomes as natural
 *   selection. The epsilon referent is the standing concentrated-market
 *   arrangement as it actually operates, assessed by this reading's lights;
 *   the reading's endorsed alternative (restored competitive entry) is not
 *   the referent. Claim and metrics are authored independently: the claimed
 *   type states what this reading holds to be structurally true, and the
 *   metrics state what the historical record descriptively shows.
 *
 * KEY AGENTS:
 *   - - incumbent_capital_holders: Primary beneficiary and agenda-setter (powerful/arbitrage) — funds and directs the defense apparatus; capital exits any jurisdiction that threatens it
 *   - - rent_defense_professionals: Secondary beneficiary (organized/constrained) — paid from the rent stream to manufacture legitimacy
 *   - - captured_competition_authorities: Administering seat (institutional/constrained) — runs enforcement under inherited permissive doctrine; careers pass through the revolving door
 *   - - would_be_entrants: Primary target (moderate/constrained) — bears the suppressed-entry cost; largely voiceless because their firms never form
 *   - - small_and_rival_firms: Target (organized/constrained) — bears squeeze, predation, and tilted standards
 *   - - monopsony_wage_workers: Most exposed target (powerless/trapped) — bears wage suppression with the fewest outside options
 *   - - captive_consumers: Net target with incidental benefit (moderate/constrained) — pays above-benchmark prices inside a system that also delivers real coordination goods
 *   - - competition_law_scholars: Analytical observer (analytical/analytical) — sees the full structure, holds no vote
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(market_naturalization__beneficiary_maintained_reading, 0.78).
domain_priors:suppression_score(market_naturalization__beneficiary_maintained_reading, 0.77).
domain_priors:theater_ratio(market_naturalization__beneficiary_maintained_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(market_naturalization__beneficiary_maintained_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(market_naturalization__beneficiary_maintained_reading, suppression_requirement, 0.77).
narrative_ontology:constraint_metric(market_naturalization__beneficiary_maintained_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(market_naturalization__beneficiary_maintained_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(market_naturalization__beneficiary_maintained_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(market_naturalization__beneficiary_maintained_reading, tangled_rope).
narrative_ontology:human_readable(market_naturalization__beneficiary_maintained_reading, "Actively Defended Market Dominance (Beneficiary-Maintained Reading)").
narrative_ontology:topic_domain(market_naturalization__beneficiary_maintained_reading, "political economy / economic history / institutional analysis").

domain_priors:requires_active_enforcement(market_naturalization__beneficiary_maintained_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(market_naturalization__beneficiary_maintained_reading, '2f82e583-c72c-453d-ba53-777f57d08992').
narrative_ontology:cs_kernel_codification('2f82e583-c72c-453d-ba53-777f57d08992', distributed).
narrative_ontology:cs_authority_grounding('2f82e583-c72c-453d-ba53-777f57d08992', extraction).
narrative_ontology:cs_interpretation_layer_present('2f82e583-c72c-453d-ba53-777f57d08992').
narrative_ontology:cs_reading_relation('2f82e583-c72c-453d-ba53-777f57d08992', market_naturalization__lapsed_alternative_reading, forecloses).
narrative_ontology:cs_reading_relation('2f82e583-c72c-453d-ba53-777f57d08992', market_naturalization__hybrid_reading, influences).
narrative_ontology:cs_axiom('2f82e583-c72c-453d-ba53-777f57d08992', foundational, dominance_requires_active_defense).
narrative_ontology:cs_axiom_status(dominance_requires_active_defense, holdable).
narrative_ontology:cs_axiom_grounding('2f82e583-c72c-453d-ba53-777f57d08992', dominance_requires_active_defense, empirically_contingent).
narrative_ontology:cs_axiom('2f82e583-c72c-453d-ba53-777f57d08992', foundational, naturalization_is_beneficiary_funded).
narrative_ontology:cs_axiom_status(naturalization_is_beneficiary_funded, holdable).
narrative_ontology:cs_axiom_grounding('2f82e583-c72c-453d-ba53-777f57d08992', naturalization_is_beneficiary_funded, empirically_contingent).
narrative_ontology:cs_reference_frame('2f82e583-c72c-453d-ba53-777f57d08992', incumbent_maintained_market_order).
narrative_ontology:cs_drift_state('2f82e583-c72c-453d-ba53-777f57d08992', contemporary_antitrust_revival_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('2f82e583-c72c-453d-ba53-777f57d08992', '').
narrative_ontology:cs_kernel_id(market_naturalization__beneficiary_maintained_reading, market_naturalization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(market_naturalization__beneficiary_maintained_reading, incumbent_capital_holders).
narrative_ontology:constraint_beneficiary(market_naturalization__beneficiary_maintained_reading, rent_defense_professionals).
narrative_ontology:constraint_victim(market_naturalization__beneficiary_maintained_reading, would_be_entrants).
narrative_ontology:constraint_victim(market_naturalization__beneficiary_maintained_reading, small_and_rival_firms).
narrative_ontology:constraint_victim(market_naturalization__beneficiary_maintained_reading, monopsony_wage_workers).
narrative_ontology:constraint_victim(market_naturalization__beneficiary_maintained_reading, captive_consumers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(market_naturalization__beneficiary_maintained_reading, captive_consumers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Own and control the dominant firms across concentrated sectors. Fund lobbying campaigns, trade associations, think tanks, and candidate committees; place alumni in regulator posts; litigate against challengers and acquire nascent rivals. Wealth compounds fastest when competitive pressure stays low, and capital moves freely across borders and asset classes, so no single jurisdiction's policy can trap them.
narrative_ontology:constraint_stakeholder(market_naturalization__beneficiary_maintained_reading, incumbent_capital_holders, agenda_setter,
    powerful, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(market_naturalization__beneficiary_maintained_reading, incumbent_capital_holders, beneficiary).

% Lobbyists, antitrust attorneys, economist-consultants, and public-relations firms whose practices depend on incumbent clients. They produce the studies, testimony, and commentary that portray concentrated outcomes as the product of merit and efficiency. Their client base and career ladders sit inside the incumbent ecosystem; retraining for other work is possible but costly mid-career.
narrative_ontology:constraint_stakeholder(market_naturalization__beneficiary_maintained_reading, rent_defense_professionals, beneficiary,
    organized, biographical, constrained, national).

% Staff and run the agencies that review mergers and police conduct. Appointees frequently arrive from, and return to, incumbent-side law firms; agency budgets depend on legislative committees responsive to incumbent donors. They administer the rules faithfully under the doctrines they inherit, and individual careers advance through the revolving door.
narrative_ontology:constraint_stakeholder(market_naturalization__beneficiary_maintained_reading, captured_competition_authorities, agenda_setter,
    institutional, biographical, constrained, national).

% Founders and firms that never get started or never scale because distribution channels, platform gatekeeping, predatory-pricing episodes, and acquire-or-crush dynamics close the paths incumbents themselves once used. Many redirect ambition to adjacent niches or abandon entrepreneurship entirely; their objection registers mostly in the aggregate statistics of declining startup rates.
narrative_ontology:constraint_stakeholder(market_naturalization__beneficiary_maintained_reading, would_be_entrants, payer,
    moderate, biographical, constrained, national).

% Operate at the edges of concentrated markets: suppliers squeezed on terms, regional retailers facing below-cost pricing campaigns, niche producers watching standards bodies tilt specifications toward incumbent patent portfolios. Leaving for another market means writing off sector-specific capital and relationships.
narrative_ontology:constraint_stakeholder(market_naturalization__beneficiary_maintained_reading, small_and_rival_firms, payer,
    organized, biographical, constrained, national).

% Work in labor markets where one or two employers dominate hiring: hospital systems, meatpacking towns, single-company regions. Non-compete clauses, no-poach agreements, and geographic concentration hold their outside options down; moving means leaving a region, a specialty, or both.
narrative_ontology:constraint_stakeholder(market_naturalization__beneficiary_maintained_reading, monopsony_wage_workers, payer,
    powerless, biographical, trapped, regional).

% Buy from markets with few providers: broadband, airlines, pharmaceuticals, banking. Prices and fees run above competitive benchmarks while service quality stagnates. They receive real benefits from the underlying market system such as variety, standards, and working payment rails, and can sometimes switch providers, but the choice set is bounded by the same consolidation they pay for.
narrative_ontology:constraint_stakeholder(market_naturalization__beneficiary_maintained_reading, captive_consumers, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(market_naturalization__beneficiary_maintained_reading, captive_consumers, beneficiary).

% Study concentration, markups, and entry from universities and independent institutes. They document the gap between competitive benchmarks and observed outcomes and propose doctrinal reform; some now hold enforcement posts, but as a class they hold no vote in the rule-making conversation and depend on public or philanthropic funding.
narrative_ontology:constraint_stakeholder(market_naturalization__beneficiary_maintained_reading, competition_law_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(market_naturalization__beneficiary_maintained_reading, incumbent_capital_holders).
narrative_ontology:fixing_cost_class(market_naturalization__beneficiary_maintained_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The market-institutional framework the arrangement rides on genuinely coordinates exchange at scale: property rights, contract enforcement, standardized accounting, payment rails, and interoperability standards are solved once instead of per-transaction, and incumbent-led standard setting does resolve real technical coordination problems.
% TRANSFER_FUNCTION: Moves economic surplus (supra-competitive margins, monopsony wage gaps, data and platform tolls) from workers, consumers, and blocked entrants to incumbent capital holders, and additionally moves public resources (enforcement forbearance, subsidies, favorable doctrine) from taxpayers to the same seats through captured policy channels.
% ABSENT_VOICES: Would-be entrants whose businesses were never started have no seat and no voice; future generations inherit consolidated industries they did not consent to; residents of company-dominated towns lack representation in the rule-making conversation, which is staffed by incumbent-funded expertise and revolving-door alumni.
% DISAPPEARANCE_RATIONALE: If the defense apparatus vanished overnight, entry would surge into gated markets, margins would compress toward competitive benchmarks, wages in concentrated labor markets would rise as non-competes and no-poach terms lost their enforcers, and the epistemic infrastructure portraying concentration as natural would lose its funding base within a budget cycle.
% FOUNDING_PROBLEM: The underlying market institutions were built to solve the coordination problems of industrial-scale exchange: trust between strangers, capital formation, standardization, and dispute resolution. The dominance-defense layer was later built to solve the incumbents' own problem: protecting accumulated positions from competitive erosion and political challenge.
% FOUNDING_PROBLEM_CORROBORATION: Empirical industrial-organization research documenting rising markups and declining business dynamism, economic historians of the Gilded Age and its trust-busting response, and competition authorities in jurisdictions outside the captured orbit all corroborate the shifted-function reading from outside the benefiting parties. No source outside the incumbent-funded expert ecosystem attests that current dominance levels reflect unaided competitive selection.
narrative_ontology:disappearance_verdict(market_naturalization__beneficiary_maintained_reading, world_rearranges).
narrative_ontology:founding_problem_status(market_naturalization__beneficiary_maintained_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(market_naturalization__beneficiary_maintained_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(market_naturalization__beneficiary_maintained_reading, 'none', 1).
narrative_ontology:epsilon_provenance(market_naturalization__beneficiary_maintained_reading, 0.78, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness is high (0.78 at interval end) because the surplus transfer is decoupled from any service rendered: markups rose while measured innovation intensity and business dynamism fell. Suppression is nearly as high (0.77) because the arrangement's persistence depends on actively closing alternatives — gating distribution channels, enforcing non-competes, and disciplining agencies — not on participant preference. Theater ratio is moderate and rising (0.42): the coordination functions are real, but a growing share of activity defends the position rather than performs it (merger-defense economics, 'superstar firm' celebration, compliance pageantry). Accessibility collapse sits at 0.5: alternatives do not vanish once the arrangement is understood — antitrust revival, cooperative ownership, and public options remain conceivable — but each carries heavy mobilization cost against funded opposition. Resistance is substantial (0.6): labor organizing waves, neo-Brandeisian enforcement, and state-level non-compete bans are real counter-pressure. The three temporal series run on ONE shared grid (t=0,8,16,24,32,40) with every metric authored at every point; trajectories are monotonic ratchets rather than cycles — no oscillation phase drives the pattern, so no intermittent-reinforcement mechanism is claimed. The suppression_requirement series is authored deliberately: the story traces enforcement-capacity maturation (lobbying spend growth, revolving-door deepening, non-compete proliferation), which is exactly the dynamic that scalar base_properties.suppression alone cannot show.
 *
 * PERSPECTIVAL GAP:
 *   From the incumbent seat the same structure reads as earned reward and legitimate property defense — an experience of coordination it funds, believes in, and would describe as the price of a functioning market. From the worker and entrant seats it reads as closed doors and held-down options. The captured-authority seat experiences faithful administration of inherited doctrine, not choice. The engine computes these divergent per-seat classifications from power, exit, and directional data; the authored claim does not adjudicate between them.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries anchor the low-d end: incumbent capital holders receive the surplus transfer directly, and rent-defense professionals are paid out of the same stream. Declared victims anchor the high-d end: entrants, rival firms, and consumers bear the transfer with constrained exits, and monopsonized workers bear it with effectively no exit, sitting nearest the full-target end. Captive consumers carry a secondary beneficiary role — they do receive genuine coordination goods — which damps but does not invert their target position. One override is authored: the institutional power atom is set to d=0.35 because captured_competition_authorities are neither declared beneficiary nor victim, so the structural derivation would fall back to a neutral-administrator default; revolving-door compensation places them measurably nearer the beneficiary end than neutrality. Suppression is authored as a raw structural property and is not scaled by power or scope; only extractiveness is scaled, by directionality and spatial scope, in the engine's computation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding coordination problem (industrial-scale exchange) remains live, so this is not a case of a mandate outliving its function — the arrangement is not inertial performance but actively serviced structure, which is why no mandatrophy resolution is declared and no sunset clause exists. The classification work this story performs is preventive on the other flank: a 'natural market order' framing invites mountain treatment (nothing to enforce, nobody benefiting), and had this story been authored with emerges_naturally and no beneficiary declarations, a false summit would pass uncertified. Declaring the beneficiary class, the victim classes, and active enforcement keeps the asymmetry visible to every downstream consumer. The R5 mismatch consumer finds status=live paired with verdict=world_rearranges — no zombie flag — because the arrangement's persistence tracks ongoing enforcement effort, not leftover inertia.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This constraint instantiates the beneficiary_maintained_reading of the market_naturalization kernel; what structurally changes if the lapsed_alternative_reading or hybrid_reading is adopted instead?',
    'Compile the sibling stories and compare computed classifications across the family: the lapsed reading should yield a near-zero-active-extraction, inertia-carried profile; the hybrid an intermediate one. Divergence in victim sets and effective extraction across readings localizes the disagreement.',
    'Under the lapsed reading the victim classes thin out and enforcement facts become vestigial; under the hybrid reading part of the measured transfer reclassifies as inertial residue rather than collected gain. This reading''s victim set and high epsilon stand or fall with the maintenance premise.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer-frame position: one of three readings of the market_naturalization kernel; sibling adoption changes victim set, epsilon, and type.').

omega_variable(
    maintenance_vs_selection_residual,
    'How much of observed dominance is actively maintained versus a residual of genuine superior efficiency that would persist even undefended?',
    'Natural experiments where defense capacity lapses: forced breakups, state-level non-compete bans, exogenous shocks to lobbying capacity, and cross-jurisdiction variation in enforcement intensity, tracking entry and margin response.',
    'A large efficiency residual would shift weight toward the hybrid profile and lower effective extraction; a small residual confirms this reading''s premise that maintenance is load-bearing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(maintenance_vs_selection_residual, empirical, 'Decomposition of observed dominance into maintained position versus efficiency residual.').

omega_variable(
    rent_efficiency_decomposition,
    'Can the supra-competitive margin be decomposed into positional rents versus returns to genuine innovation and scale economies?',
    'Event studies around antitrust actions and breakup threats, markup decomposition against quality-adjusted output, and comparison of incumbent margins before and after credible entry threats materialize.',
    'Recalibrates the extractiveness input directly: the larger the rent share, the higher the defensible epsilon; the larger the efficiency share, the more the arrangement resembles costly coordination.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(rent_efficiency_decomposition, empirical, 'Rent-versus-return split inside the measured margin.').

omega_variable(
    doctrine_instrumentality_ambiguity,
    'Is the consumer-welfare standard a neutral analytic framework that happens to govern, or an instrument selected and maintained because it licenses concentration?',
    'Doctrinal history tracing the standard''s adoption timing against incumbent interests, plus counterfactual analysis of enforcement outcomes under alternative frameworks on the same merger record.',
    'If instrumental, part of the suppression measure attaches to the doctrine itself as enforced machinery rather than to market structure; if neutral, the doctrine belongs to the vindicated-propositions column and extraction attribution narrows to conduct.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(doctrine_instrumentality_ambiguity, conceptual, 'Whether the governing antitrust framework is analytic or instrumental.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(market_naturalization__beneficiary_maintained_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mark_tr_t0, market_naturalization__beneficiary_maintained_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(mark_tr_t8, market_naturalization__beneficiary_maintained_reading, theater_ratio, 8, 0.25).
narrative_ontology:measurement(mark_tr_t16, market_naturalization__beneficiary_maintained_reading, theater_ratio, 16, 0.3).
narrative_ontology:measurement(mark_tr_t24, market_naturalization__beneficiary_maintained_reading, theater_ratio, 24, 0.35).
narrative_ontology:measurement(mark_tr_t32, market_naturalization__beneficiary_maintained_reading, theater_ratio, 32, 0.38).
narrative_ontology:measurement(mark_tr_t40, market_naturalization__beneficiary_maintained_reading, theater_ratio, 40, 0.42).

% Extraction over time
narrative_ontology:measurement(mark_be_t0, market_naturalization__beneficiary_maintained_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(mark_be_t8, market_naturalization__beneficiary_maintained_reading, base_extractiveness, 8, 0.6).
narrative_ontology:measurement(mark_be_t16, market_naturalization__beneficiary_maintained_reading, base_extractiveness, 16, 0.65).
narrative_ontology:measurement(mark_be_t24, market_naturalization__beneficiary_maintained_reading, base_extractiveness, 24, 0.7).
narrative_ontology:measurement(mark_be_t32, market_naturalization__beneficiary_maintained_reading, base_extractiveness, 32, 0.74).
narrative_ontology:measurement(mark_be_t40, market_naturalization__beneficiary_maintained_reading, base_extractiveness, 40, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(mark_su_t0, market_naturalization__beneficiary_maintained_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(mark_su_t8, market_naturalization__beneficiary_maintained_reading, suppression_requirement, 8, 0.56).
narrative_ontology:measurement(mark_su_t16, market_naturalization__beneficiary_maintained_reading, suppression_requirement, 16, 0.62).
narrative_ontology:measurement(mark_su_t24, market_naturalization__beneficiary_maintained_reading, suppression_requirement, 24, 0.68).
narrative_ontology:measurement(mark_su_t32, market_naturalization__beneficiary_maintained_reading, suppression_requirement, 32, 0.73).
narrative_ontology:measurement(mark_su_t40, market_naturalization__beneficiary_maintained_reading, suppression_requirement, 40, 0.77).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(market_naturalization__beneficiary_maintained_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(market_naturalization__beneficiary_maintained_reading, lapsed_alternative_reading).
narrative_ontology:affects_constraint(market_naturalization__beneficiary_maintained_reading, hybrid_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the market_naturalization label decomposes into three structurally distinct stories sharing one referent (the standing concentrated-market arrangement) but instantiating different constraints with different maintenance structures, victim sets, and epsilon values. This member carries the identifiable-beneficiary, active-suppression structure; the lapsed_alternative_reading member carries an inertia-carried structure with negligible active extraction; the hybrid member mixes both. Family members are linked via affects_constraints so contamination and evidence propagate across the family; each file remains epsilon-invariant on its own.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(market_naturalization__beneficiary_maintained_reading, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
