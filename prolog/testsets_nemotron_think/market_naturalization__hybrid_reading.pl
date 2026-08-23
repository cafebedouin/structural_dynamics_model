% ============================================================================
% CONSTRAINT STORY: market_naturalization__hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
 *   human_readable: Market Naturalization (Hybrid Reading): Lapsed and Actively Maintained Dominance
 *   domain: political_economy/economic_history/institutional_analysis
 *
 * SUMMARY:
 *   This constraint story represents the HYBRID READING of the
 *   market_naturalization kernel: the claim that market dominance outcomes
 *   are 'natural' combines lapsed historical elements (where alternatives
 *   genuinely atrophied without active suppression) with actively maintained
 *   elements (where incumbents invest in preserving the narrative). The
 *   constraint is the ideological-institutional arrangement that presents
 *   concentration as efficiency. Extractiveness rose from 1980-2010 as
 *   concentration increased across sectors, then plateaued. Theater ratio
 *   increased as the coordination function (genuine market efficiency in some
 *   domains) became a smaller share of the narrative's work. Suppression
 *   requirement grew as heterodox challenges and empirical evidence (rising
 *   markups, declining dynamism) required more active exclusion.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(market_naturalization__hybrid_reading, 0.45).
domain_priors:suppression_score(market_naturalization__hybrid_reading, 0.55).
domain_priors:theater_ratio(market_naturalization__hybrid_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(market_naturalization__hybrid_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(market_naturalization__hybrid_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(market_naturalization__hybrid_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(market_naturalization__hybrid_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(market_naturalization__hybrid_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(market_naturalization__hybrid_reading, tangled_rope).
narrative_ontology:human_readable(market_naturalization__hybrid_reading, "Market Naturalization (Hybrid Reading): Lapsed and Actively Maintained Dominance").
narrative_ontology:topic_domain(market_naturalization__hybrid_reading, "political_economy/economic_history/institutional_analysis").

domain_priors:requires_active_enforcement(market_naturalization__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(market_naturalization__hybrid_reading, 'bad21b1a-1aa6-44aa-b5e1-d5464733682c').
narrative_ontology:cs_kernel_codification('bad21b1a-1aa6-44aa-b5e1-d5464733682c', distributed).
narrative_ontology:cs_authority_grounding('bad21b1a-1aa6-44aa-b5e1-d5464733682c', extraction).
narrative_ontology:cs_interpretation_layer_present('bad21b1a-1aa6-44aa-b5e1-d5464733682c').
narrative_ontology:cs_reading_relation('bad21b1a-1aa6-44aa-b5e1-d5464733682c', market_naturalization__lapsed_alternative_reading, coexists_with).
narrative_ontology:cs_reading_relation('bad21b1a-1aa6-44aa-b5e1-d5464733682c', market_naturalization__beneficiary_maintained_reading, coexists_with).
narrative_ontology:cs_axiom('bad21b1a-1aa6-44aa-b5e1-d5464733682c', foundational, market_outcomes_reflect_efficiency_in_some_domains).
narrative_ontology:cs_axiom_status(market_outcomes_reflect_efficiency_in_some_domains, holdable).
narrative_ontology:cs_axiom_grounding('bad21b1a-1aa6-44aa-b5e1-d5464733682c', market_outcomes_reflect_efficiency_in_some_domains, empirically_contingent).
narrative_ontology:cs_axiom('bad21b1a-1aa6-44aa-b5e1-d5464733682c', foundational, naturalization_claim_serves_extraction_in_other_domains).
narrative_ontology:cs_axiom_status(naturalization_claim_serves_extraction_in_other_domains, holdable).
narrative_ontology:cs_axiom_grounding('bad21b1a-1aa6-44aa-b5e1-d5464733682c', naturalization_claim_serves_extraction_in_other_domains, empirically_contingent).
narrative_ontology:cs_axiom('bad21b1a-1aa6-44aa-b5e1-d5464733682c', secondary, the_kernel_label_obscures_sectoral_variation).
narrative_ontology:cs_axiom_status(the_kernel_label_obscures_sectoral_variation, holdable).
narrative_ontology:cs_axiom_grounding('bad21b1a-1aa6-44aa-b5e1-d5464733682c', the_kernel_label_obscures_sectoral_variation, conventional).
narrative_ontology:cs_reference_frame('bad21b1a-1aa6-44aa-b5e1-d5464733682c', classical_competitive_equilibrium).
narrative_ontology:cs_drift_state('bad21b1a-1aa6-44aa-b5e1-d5464733682c', contemporary_concentration_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('bad21b1a-1aa6-44aa-b5e1-d5464733682c', '').
narrative_ontology:cs_kernel_id(market_naturalization__hybrid_reading, market_naturalization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(market_naturalization__hybrid_reading, incumbent_firms).
narrative_ontology:constraint_beneficiary(market_naturalization__hybrid_reading, industry_associations).
narrative_ontology:constraint_beneficiary(market_naturalization__hybrid_reading, orthodox_economists).
narrative_ontology:constraint_victim(market_naturalization__hybrid_reading, competitors).
narrative_ontology:constraint_victim(market_naturalization__hybrid_reading, consumers).
narrative_ontology:constraint_victim(market_naturalization__hybrid_reading, workers).
narrative_ontology:constraint_victim(market_naturalization__hybrid_reading, new_entrants).
narrative_ontology:constraint_vindicates(market_naturalization__hybrid_reading, competitive_markets_are_efficient).
narrative_ontology:constraint_vindicates(market_naturalization__hybrid_reading, market_outcomes_are_natural).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Dominant firms in concentrated markets benefit from the narrative that their position reflects superior efficiency rather than barrier construction. They fund research, lobby for favorable regulation, and shape industry standards. Their exit from the constraint is trivial — they would abandon the naturalization claim if it ceased to serve them, but they actively maintain it where it protects rents.
narrative_ontology:constraint_stakeholder(market_naturalization__hybrid_reading, incumbent_firms, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(market_naturalization__hybrid_reading, incumbent_firms, beneficiary).

% Trade associations and lobbying groups produce the 'market discipline' rhetoric that naturalizes concentration. They coordinate messaging across firms, fund academic centers, and cultivate regulatory relationships. Their position depends on member dues from incumbents; exit means losing relevance and funding.
narrative_ontology:constraint_stakeholder(market_naturalization__hybrid_reading, industry_associations, agenda_setter,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(market_naturalization__hybrid_reading, industry_associations, beneficiary).

% Mainstream economics departments, journals, and advisory roles gain prestige and resources from being the designated interpreters of 'market efficiency.' Their professional identity is fused with the naturalization framework — challenging it threatens career capital and disciplinary coherence. Exit requires reconstructing professional identity.
narrative_ontology:constraint_stakeholder(market_naturalization__hybrid_reading, orthodox_economists, beneficiary,
    organized, biographical, identity_locked, global).

% Firms attempting to enter or expand in concentrated markets face barriers justified as 'market discipline' — scale requirements, switching costs, regulatory capture framed as consumer protection. The naturalization claim makes their exclusion appear as market verdict rather than constructed barrier. Exit means abandoning the market or accepting subordinate position.
narrative_ontology:constraint_stakeholder(market_naturalization__hybrid_reading, competitors, payer,
    moderate, biographical, constrained, national).

% Consumers face higher prices, reduced choice, and quality stagnation in naturalized-dominance markets. The claim that 'this is what markets produce' obscures the political construction of the outcome. Individual exit is nearly impossible — coordinated action (consumer movements, regulation) is required but suppressed by the same narrative.
narrative_ontology:constraint_stakeholder(market_naturalization__hybrid_reading, consumers, payer,
    powerless, immediate, trapped, global).

% Labor markets in concentrated industries exhibit suppressed wages and reduced bargaining power, framed as 'market-clearing.' The naturalization claim prevents collective bargaining from being seen as correcting a distortion rather than interfering with efficiency. Exit means job loss or geographic mobility with high personal cost.
narrative_ontology:constraint_stakeholder(market_naturalization__hybrid_reading, workers, payer,
    powerless, biographical, constrained, national).

% Potential competitors face capital requirements, network effects, and regulatory moats justified as 'natural barriers to entry.' The naturalization narrative makes their failure appear as market judgment. Exit is forced — they never enter or are acquired/killed before establishing.
narrative_ontology:constraint_stakeholder(market_naturalization__hybrid_reading, new_entrants, payer,
    powerless, immediate, trapped, national).

% Antitrust and competition agencies investigate whether dominance reflects efficiency or exclusion. They are structurally positioned to challenge naturalization but often adopt its framework (consumer welfare standard). Their enforcement varies by political administration and intellectual fashion.
narrative_ontology:constraint_stakeholder(market_naturalization__hybrid_reading, competition_authorities, observer,
    institutional, generational, analytical, national).

% Economists working in institutional, post-Keynesian, Marxian, or ecological traditions who challenge the naturalization framework. They are excluded from top journals, policy advisory roles, and central bank positions. Their exclusion is maintained by the same professional structures that benefit orthodox economists.
narrative_ontology:constraint_stakeholder(market_naturalization__hybrid_reading, heterodox_economists, excluded,
    moderate, biographical, identity_locked, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Markets do coordinate production and allocation across dispersed knowledge — the price system, competition, and profit/loss signals solve a genuine information problem. The naturalization claim captures this real coordination function.
% TRANSFER_FUNCTION: The arrangement transfers rents from competitors, consumers, workers, and potential entrants to incumbent firms and their intellectual/regulatory allies, by framing constructed market power as natural market outcome. The transfer operates through pricing power, suppressed wages, barrier maintenance, and regulatory capture — all justified as 'efficiency.'
% ABSENT_VOICES: Workers in the global south whose labor markets are shaped by dominant-firm supply chains; future generations who inherit concentrated market structures; small businesses and cooperatives that never form because the naturalization narrative makes alternatives appear utopian. These voices are absent because the constraint operates at the level of what is thinkable as 'economic reality.'
% DISAPPEARANCE_RATIONALE: If the naturalization claim vanished, competition policy would shift from consumer-welfare-only to structural dominance analysis; industrial policy would treat concentration as a policy choice not a market verdict; labor bargaining would gain legitimacy; heterodox economics would enter mainstream discourse. The institutional architecture of the last 40 years would require reconstruction.
% FOUNDING_PROBLEM: Post-1970s stagflation created a crisis of confidence in managed economies. The naturalization framework offered a clean intellectual solution: attribute all good outcomes to markets, all bad outcomes to interference. This solved the legitimacy problem for capital after the Keynesian consensus collapsed.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem (stagflation crisis of the 1970s) is historically dead — that macroeconomic context no longer exists. This is attested by economic historians (e.g., Tooze, Blyth) and central bank archives, not by the beneficiaries of the current arrangement. The arrangement persists despite the founding problem's disappearance.
narrative_ontology:disappearance_verdict(market_naturalization__hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(market_naturalization__hybrid_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(market_naturalization__hybrid_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(market_naturalization__hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(market_naturalization__hybrid_reading, 0.45, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(market_naturalization__hybrid_reading_tests).
:- end_tests(market_naturalization__hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Claimed type is tangled_rope because the constraint has a genuine coordination core (markets do coordinate) but layers asymmetric extraction on top (incumbents capture the 'natural' label). Beneficiaries are declared (incumbents, associations, orthodox economists) and victims declared (competitors, consumers, workers, entrants). Active enforcement is required — the narrative is maintained through funding, appointments, journal gatekeeping, regulatory capture. The metrics reflect the hybrid nature: moderate extractiveness (not pure snare), moderate suppression (not all alternatives crushed), moderate theater (some function remains).
 *
 * PERSPECTIVAL GAP:
 *   From the incumbent/agenda-setter seat, the constraint looks like a rope: they provide coordination (markets work) and the naturalization claim is just truth-telling. From the payer seats (competitors, workers, consumers), it looks like a snare: the 'natural' label suppresses alternatives and extracts rents. The engine computes this divergence from the structural power/exit asymmetries declared above. The hybrid reading explicitly holds that BOTH perspectives are partially right — hence tangled_rope.
 *
 * DIRECTIONALITY LOGIC:
 *   Incumbents and associations are agenda_setters with institutional/organized power and arbitrage/constrained exit — they shape the constraint and can exit it if it stops serving them (d near 0.0-0.2). Orthodox economists are beneficiaries but identity_locked — their professional self-concept is fused to the framework, making exit existentially costly (d ~0.3). Competitors, consumers, workers, new_entrants are payers with powerless/moderate power and trapped/constrained exit — they bear the extraction with minimal escape (d ~0.7-0.9). Competition authorities are observers with analytical exit (d ~0.5). Heterodox economists are excluded — they would object but are kept out of the conversation (d not computed for excluded).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (1970s stagflation crisis) is dead, but the arrangement persists and has deepened. This is classic mandatrophy: the mandate ('markets are natural/efficient') outlived its founding crisis. The hybrid reading captures this by distinguishing lapsed elements (where the mandate simply persists by inertia — piton-like) from actively maintained elements (where incumbents invest in the narrative — snare-like). The mandate is not resolved; it has metastasized.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the hybrid_reading a distinct structural position, or a descriptive summary of the other two readings operating in different domains?',
    'Sector-level analysis: if the same firms/institutions operate BOTH lapsed and maintained elements simultaneously (e.g., a conglomerate with legacy utilities AND active platform plays), the hybrid is a unitary constraint. If sectors cleanly separate, it is a composite.',
    'If unitary, this constraint has a single ε and classification. If composite, it should decompose into sector-specific constraint stories linked by network.affects_constraints.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the hybrid reading is one constraint or a family of sectoral constraints.').

omega_variable(
    lapsed_vs_maintained_boundary,
    'What distinguishes domains where dominance is lapsed from domains where it is actively maintained?',
    'Historical-institutional tracing: identify the last active maintenance action in each sector. If none in 20+ years, lapsed. If ongoing lobbying/funding/gatekeeping, maintained.',
    'Determines whether extractiveness and suppression metrics should be uniform or sector-differentiated. Affects whether the constraint is one tangled_rope or a mixed family.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(lapsed_vs_maintained_boundary, empirical, 'The empirical boundary between lapsed and maintained naturalization.').

omega_variable(
    orthodox_economist_capture_mechanism,
    'Is the identity_locked position of orthodox economists caused by active gatekeeping (journal edits, hiring, funding) or by genuine intellectual conviction that the framework is correct?',
    'Sociology of economics: track career trajectories of heterodox PhDs, citation networks, journal rejection rates for critical work, funding flows from incumbent-linked foundations.',
    'If active gatekeeping, the constraint''s suppression is higher and more intentional. If genuine conviction, suppression is more internalized and the constraint is more piton-like in the intellectual sphere.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(orthodox_economist_capture_mechanism, empirical, 'Mechanism of intellectual capture in the economics profession.').

omega_variable(
    naturalization_as_coordination_cover,
    'Does the genuine coordination function of markets (price signals, competition) REQUIRE the naturalization claim, or is the claim a separable ideological overlay?',
    'Counterfactual institutional design: can we have competitive markets WITHOUT the claim that ALL outcomes are natural? Historical cases (ordoliberalism, postwar mixed economies) suggest yes.',
    'If separable, the coordination function is a rope and the naturalization claim is a separable snare/tangled_rope — supporting decomposition. If inseparable, the tangled_rope classification holds as a unified constraint.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(naturalization_as_coordination_cover, conceptual, 'Whether market coordination and market naturalization are structurally separable.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(market_naturalization__hybrid_reading, 1980, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mnh_tr_t1980, market_naturalization__hybrid_reading, theater_ratio, 1980, 0.2).
narrative_ontology:measurement(mnh_tr_t1990, market_naturalization__hybrid_reading, theater_ratio, 1990, 0.25).
narrative_ontology:measurement(mnh_tr_t2000, market_naturalization__hybrid_reading, theater_ratio, 2000, 0.3).
narrative_ontology:measurement(mnh_tr_t2010, market_naturalization__hybrid_reading, theater_ratio, 2010, 0.38).
narrative_ontology:measurement(mnh_tr_t2020, market_naturalization__hybrid_reading, theater_ratio, 2020, 0.4).
narrative_ontology:measurement(mnh_tr_t2024, market_naturalization__hybrid_reading, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(mnh_be_t1980, market_naturalization__hybrid_reading, base_extractiveness, 1980, 0.25).
narrative_ontology:measurement(mnh_be_t1990, market_naturalization__hybrid_reading, base_extractiveness, 1990, 0.3).
narrative_ontology:measurement(mnh_be_t2000, market_naturalization__hybrid_reading, base_extractiveness, 2000, 0.35).
narrative_ontology:measurement(mnh_be_t2010, market_naturalization__hybrid_reading, base_extractiveness, 2010, 0.42).
narrative_ontology:measurement(mnh_be_t2020, market_naturalization__hybrid_reading, base_extractiveness, 2020, 0.45).
narrative_ontology:measurement(mnh_be_t2024, market_naturalization__hybrid_reading, base_extractiveness, 2024, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(mnh_su_t1980, market_naturalization__hybrid_reading, suppression_requirement, 1980, 0.35).
narrative_ontology:measurement(mnh_su_t1990, market_naturalization__hybrid_reading, suppression_requirement, 1990, 0.4).
narrative_ontology:measurement(mnh_su_t2000, market_naturalization__hybrid_reading, suppression_requirement, 2000, 0.48).
narrative_ontology:measurement(mnh_su_t2010, market_naturalization__hybrid_reading, suppression_requirement, 2010, 0.52).
narrative_ontology:measurement(mnh_su_t2020, market_naturalization__hybrid_reading, suppression_requirement, 2020, 0.55).
narrative_ontology:measurement(mnh_su_t2024, market_naturalization__hybrid_reading, suppression_requirement, 2024, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(market_naturalization__hybrid_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(market_naturalization__hybrid_reading, 0.15).
narrative_ontology:affects_constraint(market_naturalization__hybrid_reading, market_naturalization__lapsed_alternative_reading).
narrative_ontology:affects_constraint(market_naturalization__hybrid_reading, market_naturalization__beneficiary_maintained_reading).

% DUAL FORMULATION NOTE:
% This hybrid_reading decomposes the market_naturalization kernel into mixed lapsed/maintained dynamics. The lapsed_alternative_reading treats the kernel as piton (pure inertia). The beneficiary_maintained_reading treats it as snare (pure extraction). This reading argues the kernel's ε varies by sector — some domains are piton, some snare, and the kernel label obscures this. All three readings share the kernel_id market_naturalization and are linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(market_naturalization__hybrid_reading, organized, 0.3).
constraint_indexing:directionality_override(market_naturalization__hybrid_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
