% ============================================================================
% CONSTRAINT STORY: governance_overfitting
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_governance_overfitting, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: governance_overfitting
 *   human_readable: Hyper-Specific Compliance Lock-in
 *   domain: political/technological
 *
 * SUMMARY:
 *   Governance overfitting occurs when a regulatory framework becomes so
 *   precisely calibrated to past edge cases that it crystallizes into a
 *   barrier against novel coordination mechanisms. The constraint exhibits
 *   the full spectrum of indexical classification depending on structural
 *   position. A regulator designed to prevent the 2008 financial crisis
 *   through specific capital requirements, stress tests, and counterparty
 *   disclosure rules now blocks decentralized finance mechanisms that solve
 *   different coordination problems. A governance framework built to address
 *   1990s telecommunications monopolies now constrains platform economics and
 *   algorithmic coordination. The specificity that was once protective
 *   becomes extractive: incumbents benefit from the regulatory moats that
 *   precise rules create, while innovators face either compliance maps that
 *   distort their mechanisms or regulatory limbo. The extractiveness has
 *   grown over the interval (0.28 → 0.52) as regulatory accretion adds layer
 *   upon layer of specific rules, each justified by a real past failure,
 *   until the cumulative effect becomes less about preventing failures and
 *   more about protecting incumbents. Theater has risen (0.42 → 0.68) as
 *   regulatory compliance becomes increasingly performative: firms hire
 *   specialized compliance officers not to change actual operations, but to
 *   map novel activities into pre-existing regulatory categories. The
 *   legitimacy narrative (protection of the public) persists even as the
 *   actual mechanism (incumbent protection) becomes transparent to informed
 *   observers.
 *
 * KEY AGENTS:
 *   - Novel Coordinators (Startups, DeFi Protocols, Emergent Platforms): Primary victim (powerless/trapped) — cannot exit the compliance framework without abandoning coordination; must conform mechanisms to regulatory categories designed for incumbents
 *   - Regulatory Incumbents (Legacy Finance, Incumbent Telecommunications, Traditional Platforms): Primary beneficiary (institutional/arbitrage) — benefit from specificity that codifies incumbent market position; experience framework as coordination mechanism that structures competitive access
 *   - Reform Coalition (Tech Associations, Startup Advocates, International Bodies, Some Regulators): Secondary actor (organized/constrained) — benefit from coordination norms around modernization but constrained by incumbent veto and political risk
 *   - Legacy Regulatory Structure (Regulatory Agencies, Rule-Making Bodies, Compliance Infrastructure): Institutional actor (institutional/arbitrage) — maintains specific ruleset through inertia; sees own process as degraded (piton perspective)
 *   - Interstate Regulatory Arbitrage (Lighter-Touch Jurisdictions, Regulatory Sandboxes, Charter Cities): Temporary escape route (powerful/mobile) — provides temporary relief while global standards evolve
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional arrangements as inherent governance limits
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(governance_overfitting, 0.52).
domain_priors:suppression_score(governance_overfitting, 0.58).
domain_priors:theater_ratio(governance_overfitting, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(governance_overfitting, extractiveness, 0.52).
narrative_ontology:constraint_metric(governance_overfitting, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(governance_overfitting, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(governance_overfitting, tangled_rope).
narrative_ontology:human_readable(governance_overfitting, "Hyper-Specific Compliance Lock-in").
narrative_ontology:topic_domain(governance_overfitting, "political/technological").

domain_priors:requires_active_enforcement(governance_overfitting).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(governance_overfitting, regulatory_incumbents).
narrative_ontology:constraint_beneficiary(governance_overfitting, legacy_industry_players).
narrative_ontology:constraint_victim(governance_overfitting, novel_coordination_mechanisms).
narrative_ontology:constraint_victim(governance_overfitting, emergent_actors).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NOVEL COORDINATOR (SNARE) — Cannot exit the compliance framework without abandoning coordination entirely. New mechanisms must map to pre-existing regulatory categories designed for incumbent actors. d≈0.92, f(d)≈1.40, σ=1.0 → χ≈0.73.
constraint_indexing:constraint_classification(governance_overfitting, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: REGULATORY INCUMBENT (ROPE) — Benefits from specificity that codifies incumbent advantages. Experiences compliance framework as coordination mechanism that structures market access. d≈0.08, f(d)≈-0.11, σ=1.0 → χ≈-0.06. Net beneficiary.
constraint_indexing:constraint_classification(governance_overfitting, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: REFORM COALITION (TANGLED ROPE) — Organized agents (tech associations, startup advocates, international bodies) benefit from coordination norms around regulatory modernization but are constrained by veto power of incumbents and political risk of framework rewrite. d≈0.58, f(d)≈0.72, σ=1.1 → χ≈0.43.
constraint_indexing:constraint_classification(governance_overfitting, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 4: LEGACY REGULATORY STRUCTURE (PITON) — The specific compliance regime persists through institutional inertia despite loss of original function. Regulators continue applying 1990s categorizations to 2020s technologies. Theater dominates: compliance reports are filed and archived; actual risk is decoupled from specificity. theater_ratio=0.68 (≥0.70 threshold approaches). χ≤0.25 due to low institutional agent's f(d). Maintained because alternatives haven't politically replaced it.
constraint_indexing:constraint_classification(governance_overfitting, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: INTERSTATE REGULATORY ARBITRAGE (SCAFFOLD) — Jurisdictions with lighter-touch frameworks (EU regulatory sandboxes, Singapore fintech hubs, blockchain-friendly states) serve as temporary escape routes while the global regulatory landscape evolves toward a less-overfitted standard. d≈0.45, f(d)≈0.48, σ=1.1 → χ≈0.27. Scaffold because the multi-jurisdictional fragmentation has a built-in sunset: convergence toward a common standard (either through race-to-the-bottom or race-to-the-top) will resolve the arbitrage window.
constraint_indexing:constraint_classification(governance_overfitting, scaffold,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilization-scale view, regulatory overfitting appears as an irreducible property of complex governance: any rule system precise enough to handle N edge cases will fail on novel edge cases N+1. The analytical observer risks seeing this as immutable law. However, structural data (ε=0.52, suppression=0.58, theater=0.68) contradicts mountain classification — the specific overfitting is contingent institutional arrangement, not natural law.
constraint_indexing:constraint_classification(governance_overfitting, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(governance_overfitting_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(governance_overfitting, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(governance_overfitting, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(governance_overfitting, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(governance_overfitting, TR),
    TR >= 0.70.

:- end_tests(governance_overfitting_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The incumbent's ability to maintain regulatory moats through specific rules is substantial but not complete — novel actors can sometimes reframe their mechanisms to fit existing categories or lobby for modification. The rise from 0.28 to 0.52 over the interval reflects increasing regulatory accretion: each new rule creates new interpretation costs for innovators. Suppression (0.58): Moderate-high. Barriers include compliance costs, specialized legal expertise requirements, publication delays for novel mechanisms, and career risk for regulators who approve novel mechanisms. However, suppression is not total — jurisdictional arbitrage, industry lobbying, and technological change (which sometimes outpaces regulation) create partial exits. Theater ratio (0.68): High. Compliance activities are increasingly performative: regulatory filings are processed without substantive review of novel mechanisms; compliance is about category-mapping rather than actual risk assessment. The rise from 0.42 to 0.68 reflects that as rules accumulate, regulators and firms both invest more in theatrical compliance rather than functional assessment. Claimed type (Tangled Rope): The constraint exhibits genuine coordination function (preventing financial crises, protecting consumers, enabling market access) combined with asymmetric extraction (incumbents protected, innovators blocked).
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates high perspectival divergence. Regulatory incumbents see Rope — the specificity solves the real problem of coordinating complex finance. Novel coordinators see Snare — they cannot exit without abandoning their mechanism. The reform coalition sees Tangled Rope — genuine coordination function shadowed by incumbent extraction. The regulatory structure itself sees Piton — maintaining performative compliance despite loss of function. Interstate arbitrage sees Scaffold — the multi-jurisdictional fragmentation is temporary while standards harmonize. The analytical observer risks seeing Mountain — governance precision as immutable law — but the structural data reveals this as false summit: the overfitting is contingent institutional arrangement maintained by incumbent veto, not by natural limits on governance.
 *
 * DIRECTIONALITY LOGIC:
 *   Novel coordinators: Victim + trapped → d≈0.92, f(d)≈1.40. Near-maximum extraction. Regulatory incumbents: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.11. Net beneficiary. Reform coalition: Mixed + constrained → d≈0.58, f(d)≈0.72. Moderate extraction due to organized power mitigated by veto constraints. Legacy regulatory structure: Institutional + arbitrage (piton perspective) → d≈0.08, f(d)≈-0.11 (but theater gate overrides chi calculation). Interstate arbitrage: Powerful + mobile → d≈0.45, f(d)≈0.48. Low-to-moderate extraction due to structural mobility. Analytical observer: analytical → d≈0.72, f(d)≈1.15. Mountain classification derived from naturalization, engine catches false summit.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by revealing how identical regulatory specificity appears as coordination mechanism (Rope) to incumbents and as extraction mechanism (Snare) to innovators. The ambiguity is not about whether the type is 'correct' but about whose structural position we adopt. The tangled_rope classification reflects the objective reality: genuine coordination function (crisis prevention) plus genuine extraction (incumbent protection). The regulatory structure is NOT purely extractive (it does solve real problems) and NOT purely coordinative (it does disadvantage novel mechanisms). The rise in theater and extractiveness over the interval shows degradation of the coordination function relative to the extraction function — the rules were justified by crisis prevention, but the crisis-prevention mechanism has atrophied while the incumbent-protection mechanism has become more explicitly extractive. This is the diagnostic signature of a constraint becoming a Piton: the primary function decays while performative maintenance persists.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    specificity_optimality_boundary,
    'At what level of regulatory specificity does precision in handling known edge cases become counterproductive for novel coordination mechanisms?',
    'Empirical analysis of coordination failure rates: track novel mechanisms that fail compliance vs those that succeed by reframing into incumbent categories; measure time-to-regulatory-approval as function of mechanism novelty',
    'If boundary is quantifiable: regulatory frameworks can be redesigned with explicit precision-flexibility trade-offs. If boundary is path-dependent: overfitting is structural and requires wholesale framework rebuild rather than incremental adjustment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(specificity_optimality_boundary, empirical, 'Optimal level of regulatory specificity before counterproductivity').

omega_variable(
    incumbent_veto_sustainability,
    'Can regulatory incumbents sustain veto power over modernization indefinitely, or do technological shifts (distributed systems, cryptographic proofs, etc.) eventually bypass the specificity trap?',
    'Historical analysis of regulatory capture duration; tracking of technologies that successfully operated outside traditional compliance frameworks; identification of technological thresholds that reduce enforceability of specific rules',
    'If incumbents can sustain veto: overfitting persists, becomes a structural Snare for innovators. If technological shifts erode veto power: scaffold perspective is correct, sunset is real, constraint degrades to irrelevance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(incumbent_veto_sustainability, empirical, 'Whether incumbent veto power over regulatory modernization is sustainable').

omega_variable(
    international_convergence_mechanism,
    'Will international regulatory harmonization move toward the most-stringent overfitted standard (race-to-the-top) or toward a principles-based framework (race-to-flexibility)?',
    'Monitoring of international regulatory bodies (FATF, ISO, IOSCO); tracking convergence in fintech, AI, and data governance frameworks; identification of path-dependent lock-in in international negotiations',
    'If stringent: overfitting will become globally universal, converting Scaffold to Snare for all jurisdictions. If principles-based: scaffold sunset is accelerated, constraint degrades rapidly.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(international_convergence_mechanism, preference, 'Direction of international regulatory convergence').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(governance_overfitting, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(govfit_tr_t0, governance_overfitting, theater_ratio, 0, 0.42).
narrative_ontology:measurement(govfit_tr_t7, governance_overfitting, theater_ratio, 7, 0.58).
narrative_ontology:measurement(govfit_tr_t15, governance_overfitting, theater_ratio, 15, 0.68).

% Extraction over time
narrative_ontology:measurement(govfit_be_t0, governance_overfitting, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(govfit_be_t7, governance_overfitting, base_extractiveness, 7, 0.42).
narrative_ontology:measurement(govfit_be_t15, governance_overfitting, base_extractiveness, 15, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(governance_overfitting, enforcement_mechanism).
narrative_ontology:affects_constraint(governance_overfitting, regulatory_capture).
narrative_ontology:affects_constraint(governance_overfitting, innovation_barrier_licensing).
narrative_ontology:affects_constraint(governance_overfitting, international_regulatory_fragmentation).

% DUAL FORMULATION NOTE:
% Governance overfitting is downstream of specific regulatory decisions (banking regulation post-2008, telecom deregulation, platform governance rules) but represents a distinct structural constraint. The upstream constraints have their own ε values reflecting empirical status of specific policy domains; governance overfitting has ε=0.52 reflecting the meta-level rigidity of any rule system too precisely tuned to past edge cases.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(governance_overfitting, organized, 0.58).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
