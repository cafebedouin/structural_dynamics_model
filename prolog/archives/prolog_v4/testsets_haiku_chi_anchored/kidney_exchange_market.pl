% ============================================================================
% CONSTRAINT STORY: kidney_exchange_market
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kidney_exchange_market, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: kidney_exchange_market
 *   human_readable: Kidney Exchange Cycles and Chains
 *   domain: social/technological/biological
 *
 * SUMMARY:
 *   Kidney exchange solves the 'incompatible donor' problem: when a willing
 *   live donor is biologically incompatible with their intended recipient,
 *   traditional transplant pathways fail despite willing participation.
 *   Kidney exchange cycles and chains enable compatible transplants by
 *   coordinating multiple incompatible pairs so that each patient receives a
 *   compatible kidney. The constraint mechanism is pure coordination —
 *   participants benefit from inclusion in the matching algorithm with no
 *   coercive overhead. Suppression (rule complexity, chain-breaking
 *   protocols, waitlist dependency) exists to prevent commodification and
 *   ensure ethical fairness, not to extract rents. The system exhibits low
 *   extractiveness (0.28) and low theater (0.40), indicating genuine
 *   coordination dominance. Theater emerges primarily from regulatory
 *   compliance documentation and clinical protocol rehearsal, not from
 *   performative gatekeeping. The core tension is equity: prioritizing
 *   exchange-eligible pairs may disadvantage waitlist patients without living
 *   donors, creating an asymmetry that pushes the classification toward
 *   tangled_rope from the waitlist perspective while remaining pure rope from
 *   pair and transplant center perspectives.
 *
 * KEY AGENTS:
 *   - Incompatible Patient-Donor Pairs: Primary beneficiary (powerless/mobile) — gain access to transplant that would otherwise be blocked
 *   - Waitlist Patients (non-paired): Secondary beneficiary and potential victim (moderate/constrained) — benefit from altruistic chain-initiation but may experience relative disadvantage if exchange pairs are prioritized
 *   - Altruistic Donors: Enablers (moderate/mobile) — initiate chains without expecting return; motivated by reciprocal altruism or genuine charity
 *   - Transplant Centers: Institutional beneficiary (institutional/arbitrage) — expand transplant volume, improve utilization metrics, gain prestige
 *   - OPTN / Regulatory Authority: Institutional enforcer (organized/constrained) — maintains compatibility rules, chain-breaking protocols, prevents commodification; views current oversight as sunset-limited (xenotransplant/artificial kidney will reduce dependency)
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees pure coordination solving biological incompatibility problem
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kidney_exchange_market, 0.28).
domain_priors:suppression_score(kidney_exchange_market, 0.35).
domain_priors:theater_ratio(kidney_exchange_market, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kidney_exchange_market, extractiveness, 0.28).
narrative_ontology:constraint_metric(kidney_exchange_market, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(kidney_exchange_market, theater_ratio, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kidney_exchange_market, rope).
narrative_ontology:human_readable(kidney_exchange_market, "Kidney Exchange Cycles and Chains").
narrative_ontology:topic_domain(kidney_exchange_market, "social/technological/biological").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kidney_exchange_market, incompatible_patient_donor_pairs).
narrative_ontology:constraint_beneficiary(kidney_exchange_market, waitlist_patients).
narrative_ontology:constraint_beneficiary(kidney_exchange_market, transplant_centers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INCOMPATIBLE PATIENT-DONOR PAIR (ROPE) — Without exchange, faces dialysis or death on waitlist. Exchange provides coordinated access to compatible kidney. No coercion beyond biological constraint. d≈0.35, f(d)≈0.35, σ=1.0 → χ≈0.10. Pure coordination benefit.
constraint_indexing:constraint_classification(kidney_exchange_market, rope,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 2: ALTRUISTIC DONOR (ROPE) — Volunteer participation in chain-starting donation. No extraction; pure coordination enabling others. Motivated by reciprocal altruism or genuine charity. d≈0.15, f(d)≈-0.01, σ=1.0 → χ≈-0.00. Net neutral or slightly beneficial.
constraint_indexing:constraint_classification(kidney_exchange_market, rope,
    context(agent_power(moderate),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: TRANSPLANT CENTER NETWORK (ROPE) — Benefits from expanded transplant volume, improved utilization metrics, and scientific prestige. Coordinates logistics and clinical protocols. No coercive overhead; centers voluntarily participate in matching algorithm. d≈0.20, f(d)≈0.10, σ=1.0 → χ≈0.03. Light positive extraction (prestige/funding), but primarily coordination.
constraint_indexing:constraint_classification(kidney_exchange_market, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: REGULATORY AUTHORITY / OPTN (SCAFFOLD) — Maintains compatibility rules, chain-breaking protocols, and ethical oversight to prevent commodification. Sunset logic: as biological engineering (xenotransplantation, artificial kidneys) matures, exchange market dependency declines. Oversight suppression (rule complexity, chain restrictions) is justified by sunset trajectory. d≈0.45, f(d)≈0.45, σ=1.0 → χ≈0.13.
constraint_indexing:constraint_classification(kidney_exchange_market, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: WAITLIST PATIENT WITHOUT DONOR (TANGLED ROPE) — Experiences both coordination (expanded donor pool through altruistic-initiated chains) and asymmetric extraction (de facto priority given to compatible pairs in exchange pools, extended waitlist time for non-paired patients). d≈0.65, f(d)≈0.95, σ=1.0 → χ≈0.27. Mixed benefit/cost structure.
constraint_indexing:constraint_classification(kidney_exchange_market, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (ROPE) — From civilizational scope, kidney exchange is a pure coordination mechanism solving a collective action problem (incompatibility) without coercion. The constraint emerges from biological incompatibility, not from extraction logic. Algorithm design, not power asymmetry, is the core technical challenge. d≈0.50, f(d)≈0.65, σ=1.2 → χ≈0.22. Coordination emphasis.
constraint_indexing:constraint_classification(kidney_exchange_market, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kidney_exchange_market_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(kidney_exchange_market, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(kidney_exchange_market, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(kidney_exchange_market_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.28): Low-to-moderate, reflecting the core thesis that kidney exchange is pure coordination with modest distributional asymmetry. The value increased over the measurement interval (0.08 → 0.28) due to: (1) higher chain-length optimization creating de facto priority for exchange-eligible pairs, (2) waitlist experience of relative disadvantage as exchange volume grows, (3) accumulation of regulatory complexity that burdens non-pair participants. Suppression (0.35): Moderate. Significant barriers to direct commodification (legal prohibition, OPTN rules on chain-breaking, compatibility-only matching) prevent pure market extraction. However, coordination overhead is non-trivial: matching algorithm complexity, multi-center logistics, informed consent requirements around chain dependency, and rule-based restrictions on donor/recipient swaps. Theater (0.40): Moderate-low. Clinical protocols and regulatory compliance create some performative content, but the core matching function is genuine — participants actually do receive compatible kidneys through the algorithm, and the algorithm solves a real technical problem. Theater increased over interval as compliance documentation expanded relative to matching innovation.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits strong perspectival alignment across most actors (all classify as Rope) but a critical gap emerges from the waitlist patient perspective. Incompatible pairs, altruistic donors, centers, regulators, and the analytical observer all see coordination (Rope or Scaffold). Waitlist patients without living donors see mixed coordination and extraction (Tangled Rope) — they benefit from altruistic chain-initiation but experience systematic disadvantage relative to exchange-eligible pairs. The gap reflects a genuine distributional asymmetry: the exchange mechanism solves the incompatibility problem but creates a de facto two-tier system (paired vs unpaired). From the regulatory/analytical perspective, this is justified by the coordination function and sunset logic. From the waitlist patient perspective, it appears as hidden extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Incompatible pairs: Beneficiary + mobile → d≈0.35, f(d)≈0.35. Positive but not maximal beneficiary status; can access dialysis alternative (lower d than pure trapped). Waitlist patients: Victim + constrained → d≈0.65, f(d)≈0.95. Asymmetric positioning: benefit from altruistic chains but cannot opt into exchange pairs without living donor. Altruistic donors: Beneficiary (reverse direction) + mobile → d≈0.15, f(d)≈-0.01. Net beneficiary from reciprocal altruism and social prestige, no extraction. Centers: Beneficiary + arbitrage → d≈0.20, f(d)≈0.10. Light extraction (prestige/volume metrics), but primarily coordination. OPTN: Enforcer + constrained → d≈0.45, f(d)≈0.45. Constraint-relative power; sees sunset justification for current suppression. Analytical observer: d≈0.50, f(d)≈0.65. Neutral perspective; sees pure coordination structure.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint avoids mandatrophy by maintaining clear separation between coordination function and extraction asymmetry. The base classification is Rope because: (1) all beneficiaries gain from the coordination mechanism (algorithm actually solves incompatibility), (2) suppression is rule-based (no/commodification boundaries) not power-based (no rent-extraction via market power), (3) theater is low (matching is genuine function, not performative gatekeeping). The waitlist patient's tangled_rope perspective identifies real asymmetry but does not constitute mandatrophy because the asymmetry is distributive (relative priority) not structural (coercive extraction). The Rope classification remains primary because: removing the exchange mechanism harms all patients (pairs revert to dialysis, waitlist patients lose altruistic chains). The asymmetry is a design choice (prioritize pairs or equalize across waitlist), not an inherent extraction logic. Sunset logic via xenotransplant/artificial kidney confirms coordination thesis: as biological dependency declines, regulatory suppression (chain rules) will correspondingly decline — characteristic of Scaffold, not Snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    altruistic_chain_sustainability,
    'How dependent is the entire system on the continued availability of altruistic (non-directed) donors to initiate chains?',
    'Time-series analysis of chain-initiation rates and altruistic donor recruitment trends; sensitivity analysis of matching algorithm performance under different altruistic donor availability scenarios',
    'If altruistic donors decline: system reverts to 2-way and 3-way cycles with reduced throughput; extraction pressure increases on waitlist patients. If stable or growing: pure coordination thesis confirmed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(altruistic_chain_sustainability, empirical, 'Dependency on altruistic chain-starters').

omega_variable(
    paired_vs_unpaired_equity,
    'Does prioritizing exchange pairs create systematic disadvantage for waitlist patients without living donors?',
    'Comparative transplant rate analysis (pairs vs non-pairs); survival time and quality-of-life outcomes for waitlist patients across pre-exchange and post-exchange periods',
    'If systematic disadvantage confirmed: tangled_rope classification dominates; extraction component significant. If negligible: rope classification reinforced across all perspectives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(paired_vs_unpaired_equity, empirical, 'Equity impact of exchange prioritization').

omega_variable(
    xenotransplant_sunset_timeline,
    'What is the realistic timeline for xenotransplantation (pig-to-human) or artificial kidney technology to reduce dependence on biological exchange mechanisms?',
    'Clinical trial pipeline analysis; regulatory approval trajectories; cost-competitiveness curves vs biological transplant',
    'If 10-15 year horizon: scaffold classification valid (suppression justified by sunset). If 30+ years or stalled: current exchange rules are indefinite extraction, not temporary coordination.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(xenotransplant_sunset_timeline, empirical, 'Xenotransplant and artificial kidney development timeline').

omega_variable(
    chain_length_optimization_extraction,
    'Does the matching algorithm''s optimization for longer chains create extraction pressure on early-link participants (obligating participation in chains they didn''t voluntarily join)?',
    'Analysis of chain-breaking rates and expressed preferences; comparison of utility maximization vs fairness-weighted algorithms',
    'If extraction confirmed: suppression component (chain dependency) increases; snare perspective emerges for some participants. If fairness-optimized: rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(chain_length_optimization_extraction, conceptual, 'Whether chain-length optimization creates hidden extraction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kidney_exchange_market, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(kidney_tr_t0, kidney_exchange_market, theater_ratio, 0, 0.25).
narrative_ontology:measurement(kidney_tr_t10, kidney_exchange_market, theater_ratio, 10, 0.32).
narrative_ontology:measurement(kidney_tr_t20, kidney_exchange_market, theater_ratio, 20, 0.4).

% Extraction over time
narrative_ontology:measurement(kidney_be_t0, kidney_exchange_market, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(kidney_be_t10, kidney_exchange_market, base_extractiveness, 10, 0.18).
narrative_ontology:measurement(kidney_be_t20, kidney_exchange_market, base_extractiveness, 20, 0.28).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kidney_exchange_market, resource_allocation).
narrative_ontology:affects_constraint(kidney_exchange_market, deceased_donor_allocation).
narrative_ontology:affects_constraint(kidney_exchange_market, living_donor_altruism_sustainability).

% DUAL FORMULATION NOTE:
% Kidney exchange is downstream of the incompatibility problem (biological constraint) and upstream of broader transplant allocation policy. Exchange cycles/chains represent the coordination mechanism response; deceased donor allocation policy and altruistic donor recruitment represent parallel systems with different constraint structures. Exchange affects both through altered waitlist composition and priority dynamics.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
