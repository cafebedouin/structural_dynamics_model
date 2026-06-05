% ============================================================================
% CONSTRAINT STORY: market_naturalization__hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: market_naturalization__hybrid_reading
 *   human_readable: Market Dominance: Hybrid Reading (Lapsed + Active Maintenance)
 *   domain: political_economy/institutional_analysis
 *
 * SUMMARY:
 *   Market dominance combines lapsed elements with active maintenance. This
 *   hybrid reading claims that incumbent market power reflects BOTH the
 *   genuine difficulty of restoring alternatives that once existed and were
 *   lapsed (infrastructure lock-in, consumer habit formation, coordination
 *   cost escalation) AND the active suppression of new alternatives that
 *   could theoretically compete (regulatory capture, standards gatekeeping,
 *   capital access restriction). The reading is neither pure naturalization
 *   (the Mountain/lapsed_alternative reading) nor pure extraction (the
 *   beneficiary_maintained reading). Instead, it identifies a mixed
 *   structural landscape where some barriers to entry are immutable path
 *   dependencies and others are chosen institutional arrangements. The
 *   extractiveness value (0.45) reflects this mix: not low enough to be pure
 *   coordination (Rope), not high enough to be pure snare, but high enough to
 *   require active suppression infrastructure to maintain. The theater ratio
 *   (0.58) shows that while some regulatory activity is performative (formal
 *   risk assessment applied selectively), significant genuine coordination
 *   infrastructure persists (settled payment systems, established liability
 *   rules). This is the definitional structure of Tangled Rope: real
 *   coordination function entangled with asymmetric extraction.
 *
 * KEY AGENTS:
 *   - Incumbent Market Holders: Primary beneficiary (institutional/arbitrage) — benefits from inherited infrastructure, consumer lock-in, and regulatory alignment. Benefits are partly lapsed (coordination systems built decades ago), partly actively maintained (ongoing regulatory discretion in their favor).
 *   - Regulatory Capture Beneficiaries: Secondary beneficiary (institutional/constrained) — professional classes, certification bodies, and government agencies that derive authority and revenue from maintaining incumbent dominance. Actively defend the rules.
 *   - Excluded Market Entrants: Primary victim (powerless/trapped) — cannot access capital, distribution, or regulatory legitimacy required to compete. Face both lapsed barriers (infrastructure cannot be replicated) and active barriers (deliberately maintained licensing and capital standards).
 *   - Displaced Alternative Systems: Secondary victim (analytical/analytical) — historical systems (local production networks, mutual credit, cooperative distribution) that were lapsed through natural economic transition in some regions but actively suppressed in others. Some could theoretically be restored; doing so would require overcoming both path-dependency costs and active incumbent resistance.
 *   - Adjacent Sector Workers: Tertiary agent (moderate/constrained) — benefit from coordination infrastructure (stable employment in regulated sectors) but experience extraction (suppressed wage growth, limited sector mobility due to licensing barriers).
 *   - Platform Alternative Movement: Emergent agent (organized/mobile) — technically capable competitors building parallel systems (crypto exchanges, direct-to-consumer platforms). See the constraint as temporary — dominance depends on lapsed infrastructure they can bypass; regulatory barriers to them remain low, so exit is mobile rather than trapped.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(market_naturalization__hybrid_reading, 0.45).
domain_priors:suppression_score(market_naturalization__hybrid_reading, 0.52).
domain_priors:theater_ratio(market_naturalization__hybrid_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(market_naturalization__hybrid_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(market_naturalization__hybrid_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(market_naturalization__hybrid_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(market_naturalization__hybrid_reading, tangled_rope).
narrative_ontology:human_readable(market_naturalization__hybrid_reading, "Market Dominance: Hybrid Reading (Lapsed + Active Maintenance)").
narrative_ontology:topic_domain(market_naturalization__hybrid_reading, "political_economy/institutional_analysis").

domain_priors:requires_active_enforcement(market_naturalization__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(market_naturalization__hybrid_reading, '1d700601-66ae-4958-91cc-1e8146aaa97f').
narrative_ontology:cs_kernel_codification('1d700601-66ae-4958-91cc-1e8146aaa97f', distributed).
narrative_ontology:cs_authority_grounding('1d700601-66ae-4958-91cc-1e8146aaa97f', extraction).
narrative_ontology:cs_reading_relation('1d700601-66ae-4958-91cc-1e8146aaa97f', market_naturalization__lapsed_alternative_reading, coexists_with).
narrative_ontology:cs_reading_relation('1d700601-66ae-4958-91cc-1e8146aaa97f', market_naturalization__beneficiary_maintained_reading, coexists_with).
narrative_ontology:cs_axiom('1d700601-66ae-4958-91cc-1e8146aaa97f', foundational, lapsed_infrastructure_irreversible).
narrative_ontology:cs_axiom_status(lapsed_infrastructure_irreversible, holdable).
narrative_ontology:cs_axiom_grounding('1d700601-66ae-4958-91cc-1e8146aaa97f', lapsed_infrastructure_irreversible, empirically_contingent).
narrative_ontology:cs_axiom('1d700601-66ae-4958-91cc-1e8146aaa97f', foundational, incumbent_maintenance_is_chosen).
narrative_ontology:cs_axiom_status(incumbent_maintenance_is_chosen, holdable).
narrative_ontology:cs_axiom_grounding('1d700601-66ae-4958-91cc-1e8146aaa97f', incumbent_maintenance_is_chosen, empirically_contingent).
narrative_ontology:cs_reference_frame('1d700601-66ae-4958-91cc-1e8146aaa97f', mixed_constraint_structure).
narrative_ontology:cs_drift_state('1d700601-66ae-4958-91cc-1e8146aaa97f', contemporary_platform_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('1d700601-66ae-4958-91cc-1e8146aaa97f', '').
narrative_ontology:cs_kernel_id(market_naturalization__hybrid_reading, market_naturalization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(market_naturalization__hybrid_reading, incumbent_market_holders).
narrative_ontology:constraint_beneficiary(market_naturalization__hybrid_reading, regulatory_capture_beneficiaries).
narrative_ontology:constraint_victim(market_naturalization__hybrid_reading, excluded_market_entrants).
narrative_ontology:constraint_victim(market_naturalization__hybrid_reading, displaced_alternative_systems).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EXCLUDED MARKET ENTRANT (SNARE) — Cannot access established distribution networks, regulatory frameworks, or capital pools designed for incumbents. Faces material barriers (licensing, network lock-in) that appear natural but are actively maintained. Maximum extraction experienced — no exit options within the structured market.
constraint_indexing:constraint_classification(market_naturalization__hybrid_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: ADJACENT SECTOR WORKER (TANGLED ROPE) — Experiences coordination benefits (stable employment, training infrastructure inherited from prior period) alongside extraction (suppressed wage growth, limited mobility). The constraint has both genuine coordination (inherited systems that work) and active suppression (barriers to higher-wage sectors maintained through licensing and social closure).
constraint_indexing:constraint_classification(market_naturalization__hybrid_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INCUMBENT MARKET HOLDER (ROPE) — Benefits from coordination infrastructure (established supply chains, consumer habits, regulatory alignment) that required active maintenance to build but now functions with reduced friction. Experiences the constraint as coordination — the market structure enables their operations. Net beneficiary with agency to modify terms.
constraint_indexing:constraint_classification(market_naturalization__hybrid_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: PLATFORM ALTERNATIVE MOVEMENT (SCAFFOLD) — Digital-first entrants (crypto exchanges, direct-to-consumer platforms, cooperative marketplaces) are building parallel verification and distribution systems. These alternatives are not blocked by incumbents (regulation remains permissive) but operate at lower volume. The constraint is temporary from this perspective — alternative pathways are mature enough to exist, making the incumbent dominance a transitional phenomenon with declining extraction as network effects shift.
constraint_indexing:constraint_classification(market_naturalization__hybrid_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: REGULATORY FRAMEWORK (PITON) — Licensing requirements, capital standards, and disclosure rules that once served coordination functions (preventing fraud, ensuring solvency) now primarily function to maintain incumbent market share. The rules persist through regulatory inertia even as their protective rationale has weakened. Theater ratio is high because the regulatory logic is invoked but enforcement is selective (incumbents get discretion; entrants face strict application).
constraint_indexing:constraint_classification(market_naturalization__hybrid_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: NATURAL LAW VIEW / RISK ESSENTIALIST (MOUNTAIN) — From a civilizational perspective, market dominance by large, established firms reflects immutable constraints on managing complex systems: smaller entrants cannot absorb tail risks, coordination costs scale nonlinearly, and distributed systems are inherently unstable. This view treats the regulatory barriers and capital requirements as natural expressions of these immutable constraints. However, the hybrid reading reveals this as a false summit — some barriers are lapsed (no longer necessary), others are actively maintained (chosen to protect incumbents).
constraint_indexing:constraint_classification(market_naturalization__hybrid_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(market_naturalization__hybrid_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(market_naturalization__hybrid_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(market_naturalization__hybrid_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(market_naturalization__hybrid_reading, TR),
    TR >= 0.70.

:- end_tests(market_naturalization__hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.45): Moderate-high. The hybrid reading locates extractiveness at the precise point where lapsed barriers and active maintenance contribute equally. If the constraint were pure lapse (no active maintenance), extractiveness would be lower (~0.25-0.30) because the barriers would be immutable but cost-neutral — incumbents would benefit without needing to extract. If pure maintenance (no lapse), extractiveness would be higher (~0.60-0.70) because incumbents would be actively paying to exclude alternatives. At 0.45, both mechanisms operate. Suppression (0.52): Moderate-high. Suppression reflects barriers to entry — some structural (cannot be reversed: infrastructure sunk costs, consumer habits), some institutional (can be reversed: licensing rules, capital standards, rating agency gatekeeping). The hybrid reading claims ~50% of suppression is structural lapse, ~50% active maintenance. Suppression is not maximal (≤0.05 for Mountain) because some alternatives ARE technically feasible (platforms are emerging, crypto markets exist) and some regulatory barriers ARE being challenged (some jurisdictions allow lighter-touch entrants). Theater ratio (0.58): Moderate-high. Regulatory frameworks maintain significant performative content — risk assessment and disclosure rules are invoked to justify exclusion, but enforcement is selective. However, genuine coordination infrastructure (settlement networks, insurance backstops, interbank clearing) provides real function. Theater rises over the interval (0.35 → 0.58) as regulatory justification becomes more elaborate while underlying enforcement becomes more selective — a classic Goodhart drift where the risk-management rationale is invoked but not uniformly applied.
 *
 * PERSPECTIVAL GAP:
 *   The kernel contest is lodged in this perspectival gap. The beneficiary_maintained reading sees active incumbent defense (Snare at high power, or Rope at institutional power). The lapsed_alternative reading sees genuine infeasibility (Mountain from the natural law perspective). The hybrid reading sees both: some barriers are immutable lapsed constraints, others are chosen maintenance. This generates the Tangled Rope classification — real coordination (lapsed systems that work and cannot be easily replaced) entangled with real extraction (active maintenance of rules that would need active choice to preserve). The perspectival gap is NOT resolvable by more data about beneficiary intent — even well-intentioned incumbents benefit from lapsed alternatives and face incentives to maintain exclusion. The gap reflects genuine structural ambiguity: the dominance IS partially lapsed and partially maintained; the reading chooses to hold both facts simultaneously rather than collapse to one or the other.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective experiences the constraint through a different d value derived from beneficiary/victim status and exit options. Incumbent market holders (institutional/arbitrage) have low d (~0.15) — they benefit from the structure and can arbitrage alternatives if dominance erodes. Excluded entrants (powerless/trapped) have high d (~0.95) — they bear full extraction cost and cannot exit. Adjacent sector workers (moderate/constrained) have mid-range d (~0.65) — they experience both coordination benefits (stable employment) and extraction costs (wage suppression, limited mobility), with constrained exit options (leaving the sector is possible but costly). Platform alternatives (organized/mobile) have low-mid d (~0.35) — they can organize (organized power), can exit (mobile), and partly benefit from incumbent dominance by positioning themselves as alternatives. The Tangled Rope classification emerges because the beneficiary structure (incumbent + regulatory capture) coexists with the victim structure (entrants + displaced systems) and both are structural, not contingent.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    lapsed_vs_maintained_decomposition,
    'What proportion of current market dominance reflects lapsed alternatives (genuinely difficult to restore) versus actively maintained barriers (chosen to exclude)?',
    'Historical counterfactual analysis: identify specific regulatory rules, infrastructure investments, and consumer habit patterns. For each, assess whether reversal would be technically feasible and what cost would be incurred. Distinguish between structural path-dependency (cannot be reversed without catastrophic transition cost) and institutional choice (can be reversed but would benefit excluded entrants).',
    'If dominance is >70% lapsed: constraint should be classified as Rope or Piton (coordination with inertia). If dominance is >70% maintained: constraint should be classified as Snare or Tangled Rope (active extraction). The hybrid reading claims ~50/50 split — this omega tests that claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(lapsed_vs_maintained_decomposition, empirical, 'Proportion of dominance from lapsed alternatives vs active maintenance').

omega_variable(
    regulatory_selectivity_mechanism,
    'Are licensing, capital, and disclosure requirements applied uniformly to all market participants, or selectively in ways that advantage incumbents?',
    'Comparative regulatory analysis: audit application rates, enforcement intensity, exemption patterns, and grandfathering rules across incumbent and entrant cohorts. Measure timeline-to-approval, required capital multiples, and audit frequency by firm size and vintage. Statistical test for differential impact.',
    'If uniform application: market dominance reflects genuine risk management (Rope classification higher). If selective: dominance reflects regulatory capture and active extraction (Snare/Tangled Rope higher). This determines whether theater_ratio reflects protection or maintenance.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(regulatory_selectivity_mechanism, empirical, 'Whether regulatory requirements are applied uniformly or selectively').

omega_variable(
    alternative_system_feasibility,
    'Are alternatives to the incumbent-dominated market technically feasible at scale, or would scaling necessarily require the incumbent infrastructure?',
    'Technology assessment and supply-chain analysis: identify what infrastructure alternatives would need to replicate (cooling systems, settlement networks, insurance backstops, real-time monitoring). Cost estimate for parallel infrastructure at 10%, 50%, 90% scale penetration. Distinguish between technical infeasibility (impossible) and economic infeasibility (possible but unaffordable given current capital constraints).',
    'If technically infeasible at scale: lapsed alternatives are genuinely lapsed; dominance reflects immutable constraints (Mountain candidate). If economically infeasible: dominance reflects capital barriers maintained by incumbent gatekeeping (Snare/Tangled Rope). This affects whether the natural law perspective (Mountain) is a false summit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_system_feasibility, empirical, 'Technical and economic feasibility of alternative-system scaling').

omega_variable(
    hybrid_reading_false_dichotomy,
    'Does the hybrid reading''s claim that some alternatives are ''genuinely lapsed'' and others ''actively suppressed'' rest on a false dichotomy? Could the same lapsed alternative be simultaneously infeasible to restore AND suppressed if restored?',
    'Logical analysis: identify a specific alternative system (e.g., distributed manufacturing, local trade clearing), assess whether technical barriers to restoration are independent from or entangled with suppression mechanisms. Example: if decentralized production networks were suppressed by transport infrastructure investment (lapsed choice), is restoration infeasible because transport infrastructure is too entrenched (genuine lapse) or because incumbents now benefit from that entrenched infrastructure and would block competing transport investments (ongoing suppression)?',
    'If lapsed and suppression are entangled: the distinction between readings collapses — beneficiary_maintained and hybrid become empirically indistinguishable. If separable: the hybrid reading is coherent and empirically testable. This affects whether the kernel contest is meaningful or merely linguistic.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(hybrid_reading_false_dichotomy, conceptual, 'Whether lapsed and suppressed are false dichotomy or empirically distinct').

omega_variable(
    beneficiary_structure_heterogeneity,
    'Is the set of beneficiaries (incumbent market holders, regulatory capture beneficiaries) internally aligned in their interest to maintain dominance, or do internal conflicts suggest different readings apply to different market segments?',
    'Institutional analysis: map the beneficiary coalition. Identify conflicts of interest (small incumbents vs large, manufacturers vs financial intermediaries, domestic vs multinational). Assess whether coalitions are stable or shifting. If shifting, identify which segments support the lapsed reading (benefit from perceived inevitability) vs beneficiary_maintained reading (actively defend via lobbying, standards capture).',
    'If beneficiaries are heterogeneous: the constraint may decompose into separate stories per market segment or institutional coalition, each with different ε values. If unified: the hybrid reading''s single beneficiary structure is appropriate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_structure_heterogeneity, empirical, 'Beneficiary coalition internal alignment and segment variation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(market_naturalization__hybrid_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mkt_nat_hybrid_tr_t0, market_naturalization__hybrid_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(mkt_nat_hybrid_tr_t5, market_naturalization__hybrid_reading, theater_ratio, 5, 0.48).
narrative_ontology:measurement(mkt_nat_hybrid_tr_t10, market_naturalization__hybrid_reading, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(mkt_nat_hybrid_be_t0, market_naturalization__hybrid_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(mkt_nat_hybrid_be_t5, market_naturalization__hybrid_reading, base_extractiveness, 5, 0.38).
narrative_ontology:measurement(mkt_nat_hybrid_be_t10, market_naturalization__hybrid_reading, base_extractiveness, 10, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(mkt_nat_hybrid_su_t0, market_naturalization__hybrid_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(mkt_nat_hybrid_su_t5, market_naturalization__hybrid_reading, suppression_requirement, 5, 0.48).
narrative_ontology:measurement(mkt_nat_hybrid_su_t10, market_naturalization__hybrid_reading, suppression_requirement, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(market_naturalization__hybrid_reading, resource_allocation).
narrative_ontology:affects_constraint(market_naturalization__hybrid_reading, market_naturalization__lapsed_alternative_reading).
narrative_ontology:affects_constraint(market_naturalization__hybrid_reading, market_naturalization__beneficiary_maintained_reading).

% DUAL FORMULATION NOTE:
% market_naturalization is a kernel with three distinct readings: beneficiary_maintained, hybrid, and lapsed_alternative. Each reading instantiates a different constraint with potentially different ε values. This file models the hybrid reading — market dominance as mixed lapsed infrastructure + active maintenance. The sibling readings model alternative framings of the same kernel. All three readings are live in political economy discourse; none forecloses the others within the academic tradition. They coexist as competing analytical framings held by different institutional and theoretical coalitions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
