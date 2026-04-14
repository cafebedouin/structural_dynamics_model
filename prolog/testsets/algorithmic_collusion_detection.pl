% ============================================================================
% CONSTRAINT STORY: algorithmic_collusion_detection
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_algorithmic_collusion_detection, []).

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
 *   constraint_id: algorithmic_collusion_detection
 *   human_readable: Algorithmic Collusion Detection and Market Surveillance
 *   domain: antitrust/technology/finance
 *
 * SUMMARY:
 *   Algorithmic collusion detection represents a structural tension between
 *   legitimate market coordination and selective enforcement asymmetry.
 *   Detection systems solve a genuine problem — identifying coordinated
 *   pricing behavior that would otherwise remain hidden in complex digital
 *   markets. Yet the same infrastructure enables platforms to enforce opaque
 *   rules against competitors while exempting their own behavior from
 *   equivalent scrutiny. The constraint exhibits tangled rope
 *   characteristics: real coordination function (matching, contract
 *   enforcement, fraud prevention) coupled with asymmetric extraction
 *   (selective enforcement, unilateral rule changes, data asymmetry). The
 *   extractiveness has increased over the interval (0.28 → 0.52) as
 *   algorithms have become more sophisticated and detection has expanded into
 *   increasingly subjective behavioral signals. Theater ratio has also risen
 *   (0.35 → 0.58), reflecting that formal antitrust proceedings increasingly
 *   substitute for actual market repair.
 *
 * KEY AGENTS:
 *   - Dominant Platform Operator: Primary beneficiary (institutional/arbitrage) — controls detection system, exempts own behavior, extracts data and fees
 *   - Market Entrants: Primary victim (powerless/trapped) — cannot exit surveillance, face opaque rules, no appeal mechanism
 *   - Mid-Market Firms: Secondary victim (moderate/constrained) — benefit from platform infrastructure but face extractive terms and unilateral penalty changes
 *   - Regulatory Agency: Secondary beneficiary/victim (organized/constrained) — ostensibly detects collusion but often captured by dominant platforms; rotates staff to industry
 *   - Traditional Legal Framework: Institutional observer (institutional/arbitrage) — classical antitrust doctrine persists despite losing explanatory power in digital markets
 *   - Analytical Observer: Cross-structural perspective (analytical/analytical) — sees genuine coordination coupled with selective extraction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(algorithmic_collusion_detection, 0.52).
domain_priors:suppression_score(algorithmic_collusion_detection, 0.65).
domain_priors:theater_ratio(algorithmic_collusion_detection, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(algorithmic_collusion_detection, extractiveness, 0.52).
narrative_ontology:constraint_metric(algorithmic_collusion_detection, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(algorithmic_collusion_detection, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(algorithmic_collusion_detection, tangled_rope).
narrative_ontology:human_readable(algorithmic_collusion_detection, "Algorithmic Collusion Detection and Market Surveillance").
narrative_ontology:topic_domain(algorithmic_collusion_detection, "antitrust/technology/finance").

domain_priors:requires_active_enforcement(algorithmic_collusion_detection).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(algorithmic_collusion_detection, dominant_platform_operators).
narrative_ontology:constraint_beneficiary(algorithmic_collusion_detection, regulatory_capture_agencies).
narrative_ontology:constraint_victim(algorithmic_collusion_detection, market_entrants).
narrative_ontology:constraint_victim(algorithmic_collusion_detection, price_discovery_mechanism).
narrative_ontology:constraint_victim(algorithmic_collusion_detection, small_competitors).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EXCLUDED MARKET ENTRANT (SNARE) — Small competitors cannot exit the surveillance regime. Detection algorithms are opaque, decision criteria unknown, and appeal mechanisms are controlled by the same platform. Trapped with no structural alternative: either comply with opaque rules or exit the market entirely. Maximum extraction with no coordination benefit.
constraint_indexing:constraint_classification(algorithmic_collusion_detection, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: MID-MARKET FIRM (TANGLED ROPE) — Faces substantial costs to exit (reputational, technical integration, customer switching). The platform provides genuine coordination: real-time market data, standardized interfaces, scale economics. But the terms are extractive — asymmetric pricing power, unilateral rule changes, opaque penalties. Genuine coordination function coupled with asymmetric extraction.
constraint_indexing:constraint_classification(algorithmic_collusion_detection, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: DOMINANT PLATFORM OPERATOR (ROPE) — Experiences the detection system as pure coordination: matching buyers and sellers, enforcing contract terms, preventing fraud. The surveillance infrastructure solves genuine market-matching problems. Net beneficiary — collects fees and captures data. Arbitrage exit options enable this perspective (can exit and redeploy capital elsewhere).
constraint_indexing:constraint_classification(algorithmic_collusion_detection, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: REGULATORY AGENCY (TANGLED ROPE) — Ostensibly coordinates competition by detecting collusion. But agency staff often rotate to industry positions, creating identity fusion with regulated firms. Detection algorithms are often co-designed with incumbent platforms, embedding their commercial interests. Genuine coordination function (preventing true collusion) coupled with capture-driven extraction (protecting market leaders).
constraint_indexing:constraint_classification(algorithmic_collusion_detection, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: TRADITIONAL LEGAL FRAMEWORK (PITON) — Classical antitrust law (per se rules, rule of reason tests, merger review) persists despite becoming largely ornamental in digital markets. The legal framework cannot keep pace with algorithmic sophistication. Enforcement theaters (consent decrees, settlement agreements) occupy resources while core problems remain unaddressed. Theater ratio high because legal proceedings are decoupled from technical realities.
constraint_indexing:constraint_classification(algorithmic_collusion_detection, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — Sees the constraint as genuinely hybrid. Detection algorithms solve a real coordination problem (matching + contract enforcement in complex markets) but simultaneously enable collusion detection asymmetry: large firms can model detection thresholds and stay just below them, while small firms face unpredictable penalties. The analytical view integrates both functions and their asymmetry.
constraint_indexing:constraint_classification(algorithmic_collusion_detection, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(algorithmic_collusion_detection_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(algorithmic_collusion_detection, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(algorithmic_collusion_detection, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(algorithmic_collusion_detection, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(algorithmic_collusion_detection, TR),
    TR >= 0.70.

:- end_tests(algorithmic_collusion_detection_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The detection system provides real value (prevents fraud, enables transaction matching) but extracts through: unilateral rule-setting, selective enforcement, data concentration, and penalty asymmetry. The trajectory from 0.28 to 0.52 over 10 years reflects increasing sophistication enabling more extraction while maintaining coordination appearance. Not pure extraction (Snare levels 0.60+) because legitimate coordination functions persist. Suppression (0.65): High. Victims face opacity (algorithms unexplained), immobility (platform switching costs), enforcement arbitrariness (rules change unilaterally), and power asymmetry (appeal mechanisms controlled by platforms). Suppression is structural, not just informational — firms cannot escape through better compliance because rules are opaque and enforcement is discretionary. Theater ratio (0.58): Moderate-high. Antitrust enforcement against platforms produces visible consent decrees and settlement announcements, yet core extraction mechanisms (data concentration, algorithmic opacity, rule asymmetry) persist unchanged. Legal theater substitutes for actual structural remedy.
 *
 * PERSPECTIVAL GAP:
 *   The excluded entrant sees a snare (opaque, inescapable, asymmetrically enforced). The platform operator sees a rope (coordination mechanism for market-matching). The mid-market firm sees both — tangled rope (genuine coordination benefits coupled with extractive terms). The regulator sees a rope but is often captured into seeing it from the platform's angle — demonstrating regulatory capture through identity fusion rather than overt coercion. The traditional legal framework sees a piton — classical antitrust concepts are maintained performatively while digital realities outpace them. The analytical observer integrates all perspectives and concludes tangled rope: the system is genuinely hybrid, not collapsible to either pure coordination or pure extraction. The gap between entrant (snare) and operator (rope) perspectives is the defining diagnostic: if they had identical perspectives, one would be wrong.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) are derived from beneficiary/victim status and exit options. The platform operator (beneficiary + arbitrage) derives low d → experiences negative extraction (benefits). The market entrant (victim + trapped) derives high d → experiences maximum extraction (bears costs). The mid-market firm (victim + constrained) derives moderate-high d → experiences high but not maximum extraction (costs exceed benefits but not insurmountably). The regulator (structurally ambiguous: ostensible beneficiary through anti-collusion mission, actual victim through capture) requires directionality override: derive d from the captured institutional perspective rather than the nominal function. The analytical observer (neutral, analytical exit) derives d reflecting true structural position: seeing both coordination and extraction, neither side of asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by acknowledging that tangled rope is the structurally accurate classification when genuine coordination (matching, enforcement) coexists with asymmetric extraction (unilateral rules, selective enforcement, data concentration). The mandatrophy would emerge if attempting to classify this as pure rope (coordination with no extraction) — that would naturalize the asymmetry as costless. It would equally emerge if classifying as pure snare — that would ignore the genuine coordination benefits that participants do extract. Tangled rope correctly integrates both functions: the system genuinely coordinates while asymmetrically extracting. The challenge is not classification but remediation: can coordination be preserved while extraction is reduced? Open protocols, algorithmic transparency, and multi-participant governance would shift toward rope; current trajectory toward snare for entrants suggests capture is winning.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    detection_asymmetry_threshold,
    'At what algorithmic sophistication level do collusion detection systems become primarily tools for selective enforcement rather than market protection?',
    'Comparative analysis of false positive/negative rates across firm sizes; correlation between detection accuracy and firm market share; audit of enforcement consistency across scale categories',
    'If threshold is low (easily exploitable): classification shifts toward Snare for smaller firms. If threshold is high (genuine technical barrier): more Rope/Scaffold perspectives justified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(detection_asymmetry_threshold, empirical, 'Threshold for detection asymmetry enabling selective enforcement').

omega_variable(
    algorithmic_opacity_irreducibility,
    'Is algorithmic opacity in collusion detection technically necessary (irreducible to preserve statistical validity) or strategically chosen (obscuring commercial bias)?',
    'Technical audit comparing detection performance under full transparency vs proprietary approaches; analysis of open-source alternatives (StatArb, anomaly detection frameworks) and their accuracy; benchmarking against published academic models',
    'If necessary: supports mountain perspective (technical natural law). If chosen: supports snare/tangled_rope perspectives (institutional capture). High impact on suppression assessment.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(algorithmic_opacity_irreducibility, empirical, 'Whether algorithmic opacity is technically necessary or strategically chosen').

omega_variable(
    regulatory_capture_mechanism,
    'Does collusion detection serve principally to protect incumbent platforms from new competitive threats, or does it genuinely preserve price discovery across market scales?',
    'Historical pattern analysis: correlation between detection intensity and market entry pressure; audit of detection rules and their effect on entry barriers; comparison of enforcement action frequency before/after market concentration increases',
    'If primarily protects incumbents: tangled_rope classification with higher directionality toward victims (higher chi). If genuinely protects price discovery: rope classification from regulatory perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_capture_mechanism, empirical, 'Whether detection primarily protects incumbents or price discovery').

omega_variable(
    coordination_benefit_distribution,
    'Do small firms capture any genuine benefits from the collusion detection and platform infrastructure, or are all benefits concentrated in the platform operator and regulators?',
    'Cost-benefit analysis for entry cohorts: transaction costs, data access quality, penalty frequency and magnitude; comparative analysis of transaction costs on centralized vs decentralized platforms; measurement of information asymmetry (platform vs participants)',
    'If benefits distributed: supports rope/tangled_rope classification for mid-market firms. If concentrated: supports snare classification. Critical for determining whether this is coordination with extraction or pure extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_benefit_distribution, empirical, 'Distribution of coordination benefits across firm sizes').

omega_variable(
    temporal_detection_rule_stability,
    'How frequently do collusion detection rules and penalty thresholds change unilaterally without participant input, and how do changes correlate with enforcement outcomes?',
    'Audit of rule change frequency and scope; analysis of announcement timing relative to enforcement actions; measurement of predictability: can firms anticipate rule changes or are they arbitrary?',
    'High instability and unilateral changes increase suppression (victims cannot predict compliance requirements). Correlations with enforcement outcomes indicate strategic rule adjustment. Supports higher extraction classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(temporal_detection_rule_stability, empirical, 'Frequency and predictability of unilateral rule changes').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(algorithmic_collusion_detection, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(algcoll_tr_t0, algorithmic_collusion_detection, theater_ratio, 0, 0.35).
narrative_ontology:measurement(algcoll_tr_t5, algorithmic_collusion_detection, theater_ratio, 5, 0.48).
narrative_ontology:measurement(algcoll_tr_t10, algorithmic_collusion_detection, theater_ratio, 10, 0.58).
narrative_ontology:measurement(algcoll_tr_t2, algorithmic_collusion_detection, theater_ratio, 2, 0.4).
narrative_ontology:measurement(algcoll_tr_t7, algorithmic_collusion_detection, theater_ratio, 7, 0.54).

% Extraction over time
narrative_ontology:measurement(algcoll_be_t0, algorithmic_collusion_detection, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(algcoll_be_t5, algorithmic_collusion_detection, base_extractiveness, 5, 0.4).
narrative_ontology:measurement(algcoll_be_t10, algorithmic_collusion_detection, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(algcoll_be_t2, algorithmic_collusion_detection, base_extractiveness, 2, 0.33).
narrative_ontology:measurement(algcoll_be_t7, algorithmic_collusion_detection, base_extractiveness, 7, 0.47).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(algorithmic_collusion_detection, resource_allocation).
narrative_ontology:affects_constraint(algorithmic_collusion_detection, price_discovery_asymmetry).
narrative_ontology:affects_constraint(algorithmic_collusion_detection, platform_data_concentration).
narrative_ontology:affects_constraint(algorithmic_collusion_detection, regulatory_capture_institutional).

% DUAL FORMULATION NOTE:
% Algorithmic collusion detection exists as distinct constraints depending on observable: (1) the fraud-detection function (genuine coordination, low ε) and (2) the selective-enforcement mechanism (extraction, high ε). The decomposition reflects that 'algorithmic collusion detection' conflates two structurally distinct claims. This story integrates both as a single tangled_rope; alternative formulation would separate them into constraint family with network edges.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(algorithmic_collusion_detection, organized, 0.68).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
