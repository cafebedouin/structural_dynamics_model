% ============================================================================
% CONSTRAINT STORY: seedance_export_restriction
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_seedance_export_restriction, []).

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
 *   constraint_id: seedance_export_restriction
 *   human_readable: US Export Restrictions on ByteDance's SeeDance AI
 *   domain: political/technological/economic
 *
 * SUMMARY:
 *   The US export restriction on ByteDance's SeeDance AI exemplifies a
 *   geopolitically-driven constraint that simultaneously serves coordination
 *   (preventing proliferation of frontier AI to adversarial states, aligning
 *   with domestic industrial policy) and extraction (protecting US market
 *   dominance, imposing unilateral costs on global research, concentrating AI
 *   development in a single hegemonic power). The restriction operates
 *   through multiple enforcement mechanisms: EAR/ITAR licensing controls, GPU
 *   cluster access restrictions, international pressure on allied nations,
 *   and de facto standards-setting via export permits. Different structural
 *   actors perceive the constraint completely differently: ByteDance's
 *   international operations see a Snare (unilateral trap with no exit). The
 *   US AI industry sees a Rope (coordination that protects shared supply
 *   chains and competitive position). The global research ecosystem sees
 *   Snare (exclusion from frontier technology). International trading
 *   partners see Tangled Rope (coordination benefits paired with extraction
 *   costs). The legacy export control bureaucracy (EAR/ITAR) increasingly
 *   appears as Piton: the performative machinery persists despite limited
 *   efficacy against determined state actors. Emerging multilateral AI
 *   governance frameworks (OECD, EU, UN) are building an alternative Scaffold
 *   with a real sunset clause—as global governance matures, unilateral US
 *   restrictions become redundant and lose legitimacy. The restriction's
 *   extractiveness has increased from 0.35 to 0.62 over four years as the US
 *   tightened enforcement and expanded controlled technologies. Theater ratio
 *   rose from 0.42 to 0.65 as compliance audits, licensing denials, and port
 *   inspections multiplied without proportional impact on actual SeeDance
 *   deployment—the constraint increasingly performs its role rather than
 *   achieving it. This is the diagnostic signature of a Tangled Rope
 *   degrading toward Piton: the coordination function (preventing
 *   proliferation) is becoming obscured by performative enforcement theater,
 *   while the extraction function (market protection) remains structurally
 *   intact.
 *
 * KEY AGENTS:
 *   - ByteDance International Operations: Primary victim (powerless/trapped) — faces unilateral constraints with no exit mechanism; cannot deploy SeeDance outside US-approved channels
 *   - US AI Industry (OpenAI, Google, Meta, Anthropic): Primary beneficiary (institutional/arbitrage) — gains competitive advantage via exclusion of Chinese alternatives and preferential GPU access
 *   - US National Security Establishment (NSC, DoD, CFIUS, BIS): Primary beneficiary (institutional/arbitrage) — maintains strategic AI asymmetry vs China/Russia; captures surveillance/partnership revenue
 *   - Global AI Research Ecosystem: Secondary victim (powerless/trapped) — loses access to competitive open-source alternative; research fragmented by export restrictions
 *   - ByteDance US Shareholders: Mixed actor (moderate/constrained) — lose valuation from IP restriction but benefit from reduced regulatory scrutiny on TikTok
 *   - International Trade Coalitions (EU, allies, WTO): Mixed actor (organized/constrained) — experience both coordination (supply chain standardization) and extraction (US unilateral enforcement)
 *   - Legacy Export Control Bureaucracy (EAR/ITAR/BIS): Institutional actor (institutional/arbitrage) — maintains theater through compliance enforcement; enforcement efficacy declining (Piton)
 *   - Emerging Multilateral AI Governance (OECD, EU AI Act, UN): Organized actor (organized/constrained) — building alternative coordination pathway with sunset logic to replace unilateral controls
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(seedance_export_restriction, 0.62).
domain_priors:suppression_score(seedance_export_restriction, 0.78).
domain_priors:theater_ratio(seedance_export_restriction, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(seedance_export_restriction, extractiveness, 0.62).
narrative_ontology:constraint_metric(seedance_export_restriction, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(seedance_export_restriction, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(seedance_export_restriction, tangled_rope).
narrative_ontology:human_readable(seedance_export_restriction, "US Export Restrictions on ByteDance's SeeDance AI").
narrative_ontology:topic_domain(seedance_export_restriction, "political/technological/economic").

domain_priors:requires_active_enforcement(seedance_export_restriction).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(seedance_export_restriction, us_ai_industry).
narrative_ontology:constraint_beneficiary(seedance_export_restriction, us_national_security_establishment).
narrative_ontology:constraint_victim(seedance_export_restriction, bytedance_international_operations).
narrative_ontology:constraint_victim(seedance_export_restriction, global_ai_research_ecosystem).
narrative_ontology:constraint_victim(seedance_export_restriction, bytedance_shareholders).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% ByteDance's non-US subsidiaries and overseas partnerships face unilateral US-imposed constraints on SeeDance deployment. No exit mechanism exists short of capitulation (licensing to US companies) or technological abandonment. Trapped in a unilateral regulatory regime. d≈0.92, f(d)≈1.38, σ=1.2 → χ≈1.04.
constraint_indexing:constraint_classification(seedance_export_restriction, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% The restriction fragments the global research community, preventing access to SeeDance for academic and private research outside US-authorized channels. The epistemic commons loses a competitive open-source alternative. No mechanism for appeal or negotiated access. d≈0.90, f(d)≈1.35, σ=1.2 → χ≈1.03.
constraint_indexing:constraint_classification(seedance_export_restriction, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% US-domiciled shareholders lose valuation from SeeDance IP restriction but also benefit from ByteDance's diversified revenue streams (TikTok, Douyin) and reduced regulatory scrutiny. Constrained by US fiduciary law but not fully trapped. Mixed experience of extraction and coordination. d≈0.68, f(d)≈1.05, σ=1.0 → χ≈0.65.
constraint_indexing:constraint_classification(seedance_export_restriction, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% US-based AI companies (OpenAI, Google, Meta, Anthropic) benefit from reduced competition and preferential access to export-controlled GPU clusters, advanced training infrastructure, and talent migration from restricted contexts. The restriction functions as coordination mechanism (protecting shared supply chains, aligning standards) while enabling arbitrage (exclusive access to frontier models). d≈0.10, f(d)≈0.05, σ=1.0 → χ≈0.03.
constraint_indexing:constraint_classification(seedance_export_restriction, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% Intelligence agencies, DoD, and CFIUS perceive the restriction as pure coordination: aligning technology control with geopolitical strategy, maintaining strategic asymmetry vs China/Russia, ensuring domestic AI capacity for military applications. The mechanism generates extraction revenue (licensing fees, corporate partnerships, surveillance access) but frames it as coordination. d≈0.08, f(d)≈-0.08, σ=1.0 → χ≈-0.05.
constraint_indexing:constraint_classification(seedance_export_restriction, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% WTO members, EU regulators, and allied trade blocs experience the restriction as both coordination (global supply chain standardization via US hegemony) and extraction (US unilateral enforcement of market-access conditions). Some allies benefit from exclusion of Chinese tech; others lose access to competitive alternatives. d≈0.50, f(d)≈0.65, σ=1.1 → χ≈0.47.
constraint_indexing:constraint_classification(seedance_export_restriction, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% The EAR/ITAR/BIS system was designed for Cold War weapons technology; application to AI training data and algorithms is largely performative. Enforcement relies on theatrical compliance audits, port inspections, and license denials that have minimal impact on determined state actors. Theater_ratio=0.65 reflects substantial procedural theater masking low functional capacity. d≈0.05, f(d)≈-0.12, σ=1.0 → χ≈-0.08.
constraint_indexing:constraint_classification(seedance_export_restriction, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% OECD AI Principles, EU AI Act, and emerging multilateral AI treaties represent alternative coordination pathways with built-in sunset logic: as global AI governance matures, unilateral US export controls become redundant and lose legitimacy. These frameworks aim to replace ad-hoc restrictions with rule-based standards. Theater≤0.70, beneficiaries (democratic states seeking coordination over hegemonic control), sunset clause (norms maturation in 5-10 years). d≈0.35, f(d)≈0.30, σ=1.1 → χ≈0.23.
constraint_indexing:constraint_classification(seedance_export_restriction, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% From a civilizational view, the restriction exhibits genuine coordination (preventing proliferation of frontier AI to adversarial states, standardizing export norms) AND genuine extraction (using state power to create market advantage, imposing costs on global research, concentrating AI development in a single hegemonic state). This is the core mandatrophy: the restriction is not 'merely' coordination or 'merely' extraction—it is structurally both. d≈0.72, f(d)≈1.15, σ=1.2 → χ≈0.85.
constraint_indexing:constraint_classification(seedance_export_restriction, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(seedance_export_restriction_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(seedance_export_restriction, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(seedance_export_restriction, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(seedance_export_restriction, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(seedance_export_restriction, TR),
    TR >= 0.70.

:- end_tests(seedance_export_restriction_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62): Moderate-high. The restriction directly transfers market value from ByteDance (restricted deployment) to US AI companies (reduced competition, preferential resource access). The extraction is substantial but not total—ByteDance can still operate in China and through licensing arrangements; US AI companies face cost increases from supply-chain restrictions; global research loses but isn't completely blocked. The 0.62 value reflects that the constraint extracts significant rents while maintaining some permeable boundaries. Suppression (0.78): High. Multiple enforcement mechanisms (licensing controls, GPU embargoes, international pressure, de facto standards) create substantial barriers to alternatives. Victims face high switching costs and few exit routes. However, suppression is not absolute—determined actors (states, large firms) can develop workarounds, reverse-engineer, or negotiate licensing. Theater ratio (0.65): Moderate-high. Compliance enforcement (audits, port inspections, licensing denials) has become increasingly performative as actual SeeDance deployment rates show minimal decline and alternative pathways emerge (Chinese licensing, open-source variants). The restriction persists through institutional inertia and rhetorical commitment to national security, but functional efficacy has decayed—the theater supports the extraction more than it prevents the threat.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is maximal. ByteDance operations see Snare (complete trap, high d=0.92). US AI industry sees Rope (coordination protecting their position, low d=0.10). Global research sees Snare (exclusion from frontier tech, high d=0.90). National security sees Rope (coordination against adversarial development, low d=0.08). International coalitions see Tangled Rope (coordination benefits plus extraction costs, mid d=0.50). The analytical observer sees the constraint as genuinely both coordination and extraction—not a misclassification but a structural feature of geopolitical power asymmetry. This perspectival spread demonstrates that the constraint is NOT a natural law (would be invariant) but an institutional arrangement (varies by structural position).
 *
 * DIRECTIONALITY LOGIC:
 *   ByteDance (victim + trapped): d≈0.92 → f(d)≈1.38. Maximum extraction exposure; no exit mechanism. US AI industry (beneficiary + arbitrage): d≈0.10 → f(d)≈0.05. Net beneficiary; can move capital freely within permitted contexts. Global research (victim + trapped): d≈0.90 → f(d)≈1.35. High extraction; academic freedom constrained by export rules. National security (beneficiary + arbitrage): d≈0.08 → f(d)≈-0.08. Net beneficiary; maintains strategic advantage; can arbitrage between allowed and denied markets. Multilateral coalitions (mixed): d≈0.50 → f(d)≈0.65. Moderate extraction; some partners benefit (security), others lose (research access). The directionality splits reveal that this is a classic asymmetric extraction mechanism masked as coordination.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVED VIA STRUCTURAL DECOMPOSITION: The mandatrophy (coordination vs extraction) is resolved by recognizing that the constraint SIMULTANEOUSLY serves both functions with different actors experiencing it differently. There is no single 'true' classification—the presheaf of perspectives reveals the constraint's nature. From the beneficiary's view (US industry, NSC): Rope (pure coordination for their interests). From the victim's view (ByteDance, research): Snare (pure extraction against their interests). From the international view: Tangled Rope (genuine coordination mixed with asymmetric extraction). The analytical perspective confirms: the coordination function (preventing proliferation) is real and necessary. The extraction function (market concentration) is equally real and contingent. The restriction is NOT 'really' one or the other—it is structurally both, and the conflict between these functions is irreducible. This is what a genuine Tangled Rope looks like under perspectival decomposition. The theater ratio (0.65) indicates that performative enforcement is increasingly substituting for functional efficacy—the constraint is drifting toward Piton if the actual security benefits fail to materialize and only the market protection extraction persists. Monitor whether efficacy declines faster than institutional commitment; if so, the classification will degrade to Piton within 5-10 years.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    national_security_vs_trade_arbitrage,
    'Does the restriction serve genuine national security (preventing adversarial AI development) or is it primarily protecting US market dominance in AI?',
    'Comparative analysis of threat assessment vs competitive market dynamics. If restrictions target only China/Russia and exclude allies despite similar capabilities, suggests arbitrage. If applied uniformly to any state outside Five Eyes, suggests security.',
    'If purely security: classification shifts toward Rope (coordination). If primarily arbitrage: classification shifts toward Snare (extraction). Current ambiguity sustains the Tangled Rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(national_security_vs_trade_arbitrage, conceptual, 'Whether restriction serves security or market protection').

omega_variable(
    byteance_technology_transfer_risk,
    'Is SeeDance genuinely a national security risk (capable of enabling CCP espionage/AI militarization) or is the threat assessment artificially inflated to justify market protection?',
    'Technical analysis of SeeDance architecture vs known CCP AI capabilities; assessment by external cryptography/security experts; comparison with threat posed by other Chinese AI systems not restricted.',
    'If genuine risk: restriction justified as coordination mechanism (preventing proliferation). If inflated: restriction appears as pure extraction using security theater. Confidence in mandatrophy depends on this resolution.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(byteance_technology_transfer_risk, empirical, 'Actual security risk posed by SeeDance technology').

omega_variable(
    enforcement_efficacy_vs_theater,
    'Do US export controls actually prevent SeeDance deployment or do they merely redirect it through intermediate jurisdictions (proxies, licensing, reverse-engineering)?',
    'Track actual SeeDance deployment rates pre- and post-restriction across non-US jurisdictions; assess reverse-engineering efforts and licensing workarounds; compare with historical Cold War export control efficacy data.',
    'If effective: extraction is real (SeeDance genuinely blocked). If theater: restriction is performative; victims find workarounds and extraction is minimized. Theater_ratio assessment depends on this.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_efficacy_vs_theater, empirical, 'Whether enforcement actually blocks SeeDance or is easily circumvented').

omega_variable(
    multilateral_governance_exit_timeline,
    'How quickly will emerging multilateral AI governance frameworks replace unilateral US export controls?',
    'Track adoption rates of OECD AI Principles, EU AI Act harmonization, and UN AI governance proposals. Assess voluntary compliance vs coercive enforcement. Project convergence timeline.',
    'If sunset occurs (5-10 years): Scaffold classification confirmed. If multilateral frameworks fail: Tangled Rope persists indefinitely, extraction hardens into Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(multilateral_governance_exit_timeline, empirical, 'Timeline for multilateral AI governance maturation and US export control redundancy').

omega_variable(
    china_response_escalation,
    'Will Chinese retaliation (counter-restrictions on US tech, espionage intensification, alternative AI development) increase or decrease the net security benefit of the restriction?',
    'Monitor Chinese government statements, retaliatory trade measures, advanced semiconductor theft, and AI capability development. Model game-theoretic outcomes of restriction vs cooperation.',
    'If escalation outweighs security gains: restriction becomes counterproductive (negative coordination). If China complies: restriction validates security rationale.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(china_response_escalation, conceptual, 'Net security impact of Chinese retaliation vs restriction benefits').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(seedance_export_restriction, 0, 4).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seed_tr_t0, seedance_export_restriction, theater_ratio, 0, 0.42).
narrative_ontology:measurement(seed_tr_t2, seedance_export_restriction, theater_ratio, 2, 0.53).
narrative_ontology:measurement(seed_tr_t4, seedance_export_restriction, theater_ratio, 4, 0.65).

% Extraction over time
narrative_ontology:measurement(seed_be_t0, seedance_export_restriction, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(seed_be_t2, seedance_export_restriction, base_extractiveness, 2, 0.48).
narrative_ontology:measurement(seed_be_t4, seedance_export_restriction, base_extractiveness, 4, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(seedance_export_restriction, enforcement_mechanism).
narrative_ontology:affects_constraint(seedance_export_restriction, tiktok_regulatory_pressure).
narrative_ontology:affects_constraint(seedance_export_restriction, semiconductor_export_controls).
narrative_ontology:affects_constraint(seedance_export_restriction, chinese_ai_capability_development).
narrative_ontology:affects_constraint(seedance_export_restriction, us_gpu_cluster_distribution).

% DUAL FORMULATION NOTE:
% SeeDance restriction is downstream of US-China AI competition dynamics and upstream of broader semiconductor/tech decoupling. The restriction impacts both direct ByteDance operations and broader US-allied supply-chain alignment. Sister constraints (TikTok regulation, GPU controls) share the same structural tension between coordination and extraction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(seedance_export_restriction, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
