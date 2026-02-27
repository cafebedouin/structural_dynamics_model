% ============================================================================
% CONSTRAINT STORY: edelman_2026_developing_volatility
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_edelman_2026_developing_volatility, []).

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
 *   constraint_id: edelman_2026_developing_volatility
 *   human_readable: The Developing Market Trust Surge and Disinformation Extraction
 *   domain: economic/technological/geopolitical
 *
 * SUMMARY:
 *   The Edelman 2026 Trust Index reveals a structural paradox in developing
 *   markets: high institutional trust (India 74, UAE 80, Nigeria 72) coexists
 *   with severe exposure to foreign disinformation (75% in UAE cite
 *   disinformation concerns) and amplified displacement anxiety from AI
 *   narratives. This constraint operates as a pure extraction snare where the
 *   social asset being extracted is the high-trust baseline itself.
 *   Developing market populations, initially advantaged by institutional
 *   legitimacy and belief in institutions and technology, become vulnerable
 *   to disinformation precisely because trust is high — citizens assume
 *   information from credible-seeming sources is authentic. Foreign
 *   disinformation actors, coordinated or uncoordinated, exploit this trust
 *   asymmetry: false narratives about government instability, currency
 *   collapse, job loss to AI, and social division spread faster in high-trust
 *   environments because they initially benefit from presumptions of
 *   authenticity. The snare's suppression mechanism (0.72) operates through
 *   information monopolies, algorithmic amplification of polarizing content,
 *   limited media literacy infrastructure in some regions, and the structural
 *   barrier of language diversity (disinformation tailored to local languages
 *   is harder to detect by global fact-checkers). Over the 6-year interval
 *   from 2020-2026, extractiveness has risen from 0.35 to 0.58, and
 *   theater_ratio has increased from 0.42 to 0.64, indicating that
 *   institutional trust measurement (the Edelman index itself) has become
 *   increasingly performative — annual trust scores are published while the
 *   underlying resilience to disinformation penetration has degraded. The
 *   constraint is durable because high trust creates the extraction
 *   opportunity, while extraction (disinformation infiltration, anxiety
 *   amplification) eventually erodes trust, but with a lag. This lag allows
 *   disinformation actors to extract value (political influence, capital
 *   flight acceleration, destabilization) before trust collapse becomes
 *   evident in survey metrics.
 *
 * KEY AGENTS:
 *   - Developing Market Populations (India, UAE, Nigeria, others): Primary victims (powerless/trapped) — high trust baseline creates vulnerability; cannot exit information ecosystems; bear full cost of disinformation damage
 *   - Foreign Disinformation Actors (state and non-state): Primary beneficiaries (powerful/mobile) — exploit trust asymmetry; derive political influence, capital flow acceleration, destabilization benefits; retain arbitrage to exit when one region becomes saturated
 *   - Displaced Workers and AI-Anxiety Cohorts: Secondary victims (moderate/constrained) — face genuine labor market vulnerability; amplified by disinformation narratives; trapped by lack of skill-transfer infrastructure
 *   - Domestic Institutional Trust and Governance: Secondary victims (institutional/constrained) — their legitimacy is the extracted asset; cannot easily rebuild trust once disinformation takes hold; face capital flight risk
 *   - Tech and AI Companies Operating Regionally: Mixed (powerful/mobile) — extract through AI service expansion while claiming to solve disinformation; benefit from displacement narratives; provide some coordination infrastructure (moderation, fact-checking tools) as cover
 *   - Civic and Development Organizations: Organized (organized/constrained) — pursue coordination solutions (media literacy, fact-checking networks); see this as solvable collective action problem, not structural extraction
 *   - Trust Index Measurement Systems (Edelman, others): Institutional (institutional/arbitrage) — maintain measurement ritual despite decoupling from actual institutional resilience; piton perspective reveals theater masking underlying snare
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(edelman_2026_developing_volatility, 0.58).
domain_priors:suppression_score(edelman_2026_developing_volatility, 0.72).
domain_priors:theater_ratio(edelman_2026_developing_volatility, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(edelman_2026_developing_volatility, extractiveness, 0.58).
narrative_ontology:constraint_metric(edelman_2026_developing_volatility, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(edelman_2026_developing_volatility, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(edelman_2026_developing_volatility, snare).
narrative_ontology:human_readable(edelman_2026_developing_volatility, "The Developing Market Trust Surge and Disinformation Extraction").
narrative_ontology:topic_domain(edelman_2026_developing_volatility, "economic/technological/geopolitical").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(edelman_2026_developing_volatility, foreign_disinformation_actors).
narrative_ontology:constraint_beneficiary(edelman_2026_developing_volatility, ai_displacement_narrators).
narrative_ontology:constraint_beneficiary(edelman_2026_developing_volatility, capital_flight_enablers).
narrative_ontology:constraint_victim(edelman_2026_developing_volatility, developing_market_populations).
narrative_ontology:constraint_victim(edelman_2026_developing_volatility, local_institutional_trust).
narrative_ontology:constraint_victim(edelman_2026_developing_volatility, domestic_economic_stability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DEVELOPING MARKET CONSUMER (SNARE) — High trust baseline (India 74, UAE 80, Nigeria 72) creates vulnerability to disinformation. Citizens cannot exit: they must navigate information ecosystems within their national boundaries. Foreign disinformation exploits trust asymmetry (75% in UAE cite disinformation concerns). d≈0.92, f(d)≈1.39, σ=0.9 → χ≈0.73. Trapped by geography and language; extraction is high and suppression (0.72) reflects information monopoly dynamics.
constraint_indexing:constraint_classification(edelman_2026_developing_volatility, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: DISPLACED WORKERS / AI-ANXIETY COHORTS (SNARE) — Moderate power but constrained exit. Workers facing AI displacement fears are partially trapped by labor market structures, credit-dependent consumption, and lack of skill-transfer infrastructure. Disinformation narratives amplify job-loss anxiety, making exit (retraining, relocation) appear blocked. d≈0.78, f(d)≈1.11, σ=0.9 → χ≈0.57. Suppression (0.72) holds them in place through manufactured fear and limited alternative pathways.
constraint_indexing:constraint_classification(edelman_2026_developing_volatility, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: LOCAL INSTITUTIONS / GOVERNANCE (SNARE) — Developing market governments, central banks, and regulatory bodies experience disinformation as an extraction mechanism. High public trust (74-80) is the asset being extracted: disinformation erodes institutional legitimacy without offering exit. Capital flight accelerates when trust collapses, creating a negative feedback loop. d≈0.85, f(d)≈1.24, σ=0.9 → χ≈0.64. Suppression (0.72) reflects inability to control information flows or build countervailing trust narratives quickly enough.
constraint_indexing:constraint_classification(edelman_2026_developing_volatility, snare,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: TECH/AI COMPANIES IN DEVELOPING MARKETS (TANGLED ROPE) — Powerful actors with global mobility (arbitrage across markets) who extract through AI services while simultaneously claiming to solve disinformation. These firms benefit from high trust (easier user adoption) and from displacement narratives (justifies AI implementation). Simultaneously, they have coordination function (provide tools, infrastructure). d≈0.48, f(d)≈0.60, σ=1.1 → χ≈0.38. Effective extraction moderate; requires active coordination (platform governance, moderation) to maintain extraction floor.
constraint_indexing:constraint_classification(edelman_2026_developing_volatility, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: CIVIC/DEVELOPMENT COALITION (ROPE) — Organized NGOs, civil society, media literacy initiatives, and multilateral development agencies see this as a coordination problem. They benefit from trust-building infrastructure and see disinformation as a solvable collective action problem, not extraction. Low-overhead coordination: media literacy, fact-checking networks, digital literacy programs. d≈0.30, f(d)≈0.20, σ=1.2 → χ≈0.10. Pure coordination orientation; beneficiaries from coordination mechanisms.
constraint_indexing:constraint_classification(edelman_2026_developing_volatility, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: TRUST INDEX MEASUREMENT SYSTEMS (PITON) — Organizations like Edelman Trust Barometer produce annual trust metrics (74, 80, 72 indices) that have become performative. The measurement ritual persists, but trust index scores have become decoupled from actual institutional resilience — high scores coexist with capital flight, disinformation penetration (75% in UAE), and displacement anxiety. theater_ratio=0.64 reflects that trust measurement is partly theater masking extraction. d≈0.10, f(d)≈-0.04, σ=1.2 → χ≈-0.03. Negative effective extraction: indices appear to benefit systems, but they obscure the disinformation snare.
constraint_indexing:constraint_classification(edelman_2026_developing_volatility, piton,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (SNARE) — From a civilizational/global perspective, the constraint is pure extraction of high-trust social capital by foreign and coordinated disinformation actors, amplified by AI-driven anxiety narratives. The high trust baseline in developing markets is the extractable asset. Suppression (0.72) operates through information asymmetry, platform algorithms, and coordinated inauthentic behavior networks. d≈0.70, f(d)≈1.04, σ=1.2 → χ≈0.72. The snare is structural and durable so long as information flows remain asymmetric.
constraint_indexing:constraint_classification(edelman_2026_developing_volatility, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(edelman_2026_developing_volatility_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(edelman_2026_developing_volatility, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(edelman_2026_developing_volatility, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(edelman_2026_developing_volatility, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(edelman_2026_developing_volatility, TR),
    TR >= 0.70.

:- end_tests(edelman_2026_developing_volatility_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High. The constraint operates by extracting social trust capital and converting it into political influence, economic flows (capital flight), and destabilization. The extraction is not total (0.58 rather than 0.75) because developing markets retain some institutional resilience, fact-checking capacity, and counter-narrative infrastructure. However, the trend is increasing (0.35 → 0.58 over 6 years), indicating the snare is deepening. Suppression (0.72): High. Multiple layers suppress exit and alternatives: (1) Information monopolies in some regions (limited competitive news sources); (2) Algorithmic amplification of engaging (often polarizing) disinformation; (3) Language barriers that slow global fact-checking networks; (4) Limited digital literacy infrastructure in some developing markets; (5) Structural economic constraints that make citizens vulnerable to displacement anxiety narratives. Theater ratio (0.64): Moderate-high and increasing. Trust measurement (Edelman index) has become increasingly performative — annual indices are published and reported as objective facts, but they mask the underlying disinformation penetration and trust degradation. The theater increases because trust scores remain relatively high (through survey methodology lag, respondent bias toward socially desirable answers, or genuine trust rebound after single events) while actual institutional vulnerability to disinformation grows. Claimed type (Snare): The constraint exhibits all snare markers. Base extraction ≥ 0.46 (0.58 > 0.46 ✓), suppression ≥ 0.60 (0.72 > 0.60 ✓), chi calculation yields 0.72 for primary victims (powerless/trapped at global scope) meeting χ ≥ 0.66 ✓. Disinformation actors retain arbitrage (can exit one market for another); victims are trapped within information ecosystems.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap in this constraint is severe and reflects the fundamental asymmetry of the snare. Developing market consumers and domestic institutions (victims, powerless/institutional) see a snare — they are trapped, extraction is high, suppression is real, and they have no clear exit. Tech companies operating regionally (powerful/mobile) see tangled rope or even rope — they extract but also provide coordination infrastructure; they have full arbitrage mobility and experience the constraint as manageable. Civic organizations (organized/constrained) see rope — pure coordination problem with available solutions. The trust index measurement system (piton) sees its own process as degraded theater — the ritual persists, but it no longer captures actual institutional resilience. The analytical observer (civilizational scope) sees snare — high trust in developing markets is a structural vulnerability being systematically extracted by foreign actors, and the extraction is durable because trust itself enables it. The gap reveals that the 'trust surge' in developing markets is not a strength but a vulnerability being mined. High trust is the extracted asset.
 *
 * DIRECTIONALITY LOGIC:
 *   Developing market populations: Victim + trapped → d≈0.92, f(d)≈1.39. Maximum extraction. Citizens cannot leave; must consume information within national boundaries; information quality controlled by others. Disinformation actors: Beneficiary + arbitrage → d≈0.05, f(d)≈-0.12. Net beneficiaries. Can move operations between markets as one saturates. Displaced workers: Victim + constrained → d≈0.78, f(d)≈1.11. Significant extraction; not fully trapped (some retraining possible) but labor market barriers are high. Domestic institutions: Victim + constrained → d≈0.85, f(d)≈1.24. High extraction; constrained by inability to rapidly rebuild trust or control information flows. Tech companies: Mixed beneficiary + mobile → d≈0.48, f(d)≈0.60. Moderate effective extraction; benefits from growth opportunities but mobile arbitrage reduces commitment. Civic organizations: Beneficiary (of coordination function) + constrained → d≈0.30, f(d)≈0.20. Low extraction from coordination perspective. Trust index systems: Beneficiary (maintain institutional relevance) + arbitrage → d≈0.10, f(d)≈-0.04. Piton classification from theater gate, not directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy resolved: This constraint's extractiveness (0.58) exceeds 0.46, triggering mandatrophy scrutiny. The snare classification is confirmed through the structural decomposition: (1) PRIMARY BENEFICIARIES EXIST: Foreign disinformation actors, AI displacement narrators, capital flight enablers all derive measurable benefit. (2) PRIMARY VICTIMS EXIST: Developing market populations (trapped), domestic institutions (constrained), displaced workers all bear documented costs. (3) EXTRACTION MECHANISM CLEAR: Trust asymmetry + information monopolies + algorithmic amplification create durable extraction pathway. (4) SUPPRESSION ≥ 0.60: Citizens cannot exit; alternatives are suppressed by information control, algorithm design, and lack of competing infrastructure. (5) NO GENUINE COORDINATION FUNCTION: Unlike tangled rope, this constraint offers no meaningful coordination benefit to victims. The 'solutions' offered (AI tools, fact-checking, digital literacy) are secondary and insufficient to constitute coordination floor. (6) BENEFICIARIES DO NOT OVERLAP WITH VICTIMS: Foreign disinformation actors and displaced workers are structurally distinct populations, confirming asymmetric extraction rather than symmetric coordination with winners/losers. Mandatrophy therefore does NOT apply — snare is the correct classification across all perspectives. The constraint is not being mislabeled as coordination when it is extraction, nor vice versa. The piton perspective (trust index theater) reveals that measurement systems obscure the snare, but that is a secondary observation about institutional inertia, not a contradiction of the core classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    trust_baseline_sustainability,
    'Is the high trust baseline in developing markets (India 74, UAE 80, Nigeria 72) a structural feature of institutional maturity or a temporary phenomenon vulnerable to single-shock degradation?',
    'Time series analysis of trust metrics across decades; correlation between trust levels and institutional capacity; shock response modeling (financial crisis, political event, disinformation campaign intensity thresholds)',
    'If structural: snare persists even after single disinformation campaigns because underlying trust rebounds. If fragile: trust collapse could happen rapidly once critical threshold is breached, converting snare to total institutional failure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(trust_baseline_sustainability, empirical, 'Whether high developing-market trust is durable or shock-vulnerable').

omega_variable(
    disinformation_actor_coordination,
    'Do foreign disinformation operations in developing markets (75% penetration in UAE) operate as uncoordinated actors (multiple state/private entities competing) or as a coordinated extraction mechanism (shared infrastructure, synchronized targeting)?',
    'Network analysis of disinformation narratives; temporal correlation of campaign targeting; source infrastructure mapping; intelligence analysis of state/non-state actor relationships',
    'If uncoordinated: snare could be disrupted by targeting one source or coalition. If coordinated: extraction is more durable and requires multilateral counter-action; single-nation defenses insufficient.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(disinformation_actor_coordination, empirical, 'Whether disinformation operations are coordinated or competitive').

omega_variable(
    ai_displacement_narrative_exogeneity,
    'Are AI-displacement fears in developing markets genuine responses to technological change or partially manufactured/amplified by disinformation actors who benefit from anxiety narratives?',
    'Comparative narrative analysis (organic vs coordinated social media campaigns); correlation between disinformation campaign timing and AI-anxiety sentiment peaks; interview-based attribution studies; bot detection on displacement-related content',
    'If exogenous (genuine displacement risk): snare classification holds but reflects real labor market vulnerability. If partially manufactured: disinformation actors are using AI-anxiety as a tool to entrench snare, making the constraint more durable (emotional amplification layer).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ai_displacement_narrative_exogeneity, empirical, 'Whether AI-anxiety is organic or narratively amplified').

omega_variable(
    capital_flight_causation,
    'Does disinformation-driven trust collapse directly cause capital flight from developing markets, or is capital flight driven by independent macroeconomic factors (interest rates, currency risk) and disinformation is correlated but not causal?',
    'Econometric analysis of trust metrics vs capital flight timing; instrumental variable analysis; event studies around major disinformation campaigns; wealth/asset flow analysis pre/post trust degradation',
    'If causal: disinformation is an extraction mechanism that converts social trust into actual economic extraction (capital outflow). If correlated but not causal: snare operates on perception/sentiment, not on real economic flows.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(capital_flight_causation, empirical, 'Whether disinformation causes capital flight or is merely correlated').

omega_variable(
    platform_algorithm_complicity,
    'Do recommendation algorithms on major social platforms (Meta, TikTok, YouTube, X) passively distribute disinformation or actively optimize for engagement with polarizing disinformation content, thereby amplifying the snare?',
    'Algorithm transparency audits; comparative feed analysis (algorithmic vs chronological ranking); network analysis of disinformation amplification; platform policy review and enforcement data',
    'If passive: disinformation spreads but could be countered by platform policy changes. If actively optimized for engagement: platforms are structural components of the snare, making suppression (0.72) more durable.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(platform_algorithm_complicity, empirical, 'Whether platform algorithms passively distribute or actively amplify disinformation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(edelman_2026_developing_volatility, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(edel_tr_t0, edelman_2026_developing_volatility, theater_ratio, 0, 0.42).
narrative_ontology:measurement(edel_tr_t3, edelman_2026_developing_volatility, theater_ratio, 3, 0.53).
narrative_ontology:measurement(edel_tr_t6, edelman_2026_developing_volatility, theater_ratio, 6, 0.64).

% Extraction over time
narrative_ontology:measurement(edel_be_t0, edelman_2026_developing_volatility, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(edel_be_t3, edelman_2026_developing_volatility, base_extractiveness, 3, 0.48).
narrative_ontology:measurement(edel_be_t6, edelman_2026_developing_volatility, base_extractiveness, 6, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(edelman_2026_developing_volatility, information_standard).
narrative_ontology:affects_constraint(edelman_2026_developing_volatility, ai_displacement_labor_markets).
narrative_ontology:affects_constraint(edelman_2026_developing_volatility, platform_algorithmic_amplification).
narrative_ontology:affects_constraint(edelman_2026_developing_volatility, capital_flight_volatility).

% DUAL FORMULATION NOTE:
% This constraint is downstream of three structural conditions: (1) AI-displacement labor market vulnerability, which generates genuine anxiety that disinformation amplifies. (2) Platform algorithm design that optimizes for engagement (including polarizing disinformation). (3) Macroeconomic capital flight dynamics that disinformation accelerates. Each upstream constraint has lower ε but together they enable the developing market trust snare. The relationship is not sequential causation but structural coupling: each upstream constraint provides a failure point that disinformation exploits.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(edelman_2026_developing_volatility, institutional, 0.82).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
