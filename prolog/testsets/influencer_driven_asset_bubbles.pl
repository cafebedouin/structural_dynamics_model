% ============================================================================
% CONSTRAINT STORY: influencer_driven_asset_bubbles
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_influencer_driven_asset_bubbles, []).

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
 *   constraint_id: influencer_driven_asset_bubbles
 *   human_readable: Influencer-Driven Asset Bubbles
 *   domain: financial_markets/social_dynamics
 *
 * SUMMARY:
 *   Influencer-driven asset bubbles represent a structural constraint where
 *   social proof amplification mechanisms create asymmetric extraction
 *   between early-mover influencers and later-entering retail investors. The
 *   constraint emerges from the intersection of three structural elements:
 *   (1) algorithmic amplification of engaging content regardless of accuracy,
 *   (2) information asymmetry between influencers with large platforms and
 *   dispersed retail participants, and (3) identity fusion dynamics where
 *   participation becomes a marker of social belonging or investment
 *   sophistication. The bubble cycle exhibits a clear temporal progression:
 *   low extractiveness during the accumulation phase (0-3 time points), rapid
 *   escalation as FOMO drives retail adoption (3-6 time points), and eventual
 *   collapse when new supply of retail capital exhausts. Theater ratio tracks
 *   the performative content of market justifications: early bubble
 *   narratives cite fundamental value changes; late-stage narratives become
 *   purely narrative speculation with minimal connection to underlying
 *   assets. This constraint classifies as Snare from most perspectives
 *   because the suppression mechanisms (algorithmic promotion, social proof
 *   cascades, identity lock) are primarily coercive rather than coordinative.
 *
 * KEY AGENTS:
 *   - Early Influencers and Insiders: Primary beneficiary (institutional/arbitrage) — capture disproportionate gains during accumulation and early promotion phases; able to exit with arbitrage windows before collapse
 *   - Retail Investors: Primary victim (powerless/trapped) — enter during high-extraction phases; lack information asymmetry compensation and face material barriers to exit once committed
 *   - Late Adopters: Secondary victim (moderate/constrained) — face social pressure and FOMO; structurally constrained but not fully trapped; exit options exist but carry reputational costs
 *   - Market Integrity: Tertiary victim (powerless/trapped) — abstract collective victim incapable of organizing or exiting; bears long-term costs of reduced trust and regulatory burden
 *   - Regulatory Coalition: Organized actor (organized/mobile) — sees mixed coordination and extraction; can exit through regulatory intervention but faces institutional capture
 *   - Platform Infrastructure: Implicit beneficiary (institutional/arbitrage) — profits from engagement and transaction volume during bubble phases; largely escapes extraction cost allocation
 *   - Analytical Observer: Self-regarding actor (analytical/identity_locked) — structurally capable of modeling bubbles but identity-locked by participation in attention economy; cannot fully exit analytical frame without abandoning primary analytical tools
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(influencer_driven_asset_bubbles, 0.68).
domain_priors:suppression_score(influencer_driven_asset_bubbles, 0.72).
domain_priors:theater_ratio(influencer_driven_asset_bubbles, 0.81).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(influencer_driven_asset_bubbles, extractiveness, 0.68).
narrative_ontology:constraint_metric(influencer_driven_asset_bubbles, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(influencer_driven_asset_bubbles, theater_ratio, 0.81).

% --- Constraint claim ---
narrative_ontology:constraint_claim(influencer_driven_asset_bubbles, snare).
narrative_ontology:human_readable(influencer_driven_asset_bubbles, "Influencer-Driven Asset Bubbles").
narrative_ontology:topic_domain(influencer_driven_asset_bubbles, "financial_markets/social_dynamics").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(influencer_driven_asset_bubbles, early_influencers_and_insiders).
narrative_ontology:constraint_victim(influencer_driven_asset_bubbles, retail_investors).
narrative_ontology:constraint_victim(influencer_driven_asset_bubbles, late_adopters).
narrative_ontology:constraint_victim(influencer_driven_asset_bubbles, market_integrity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RETAIL INVESTOR (SNARE) — Trapped by information asymmetry, FOMO-driven market dynamics, and lack of exit capacity once committed. Bears full extraction cost as bubble inflates and collapses. No meaningful coordination benefit; the constraint exists only to extract wealth upward.
constraint_indexing:constraint_classification(influencer_driven_asset_bubbles, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: LATE ADOPTER (SNARE) — Structurally constrained by social pressure, availability cascades, and algorithmic amplification. Can exit but faces significant costs: social exclusion, fear of missing gains, reputational damage within peer groups. High extraction with limited coordination benefit.
constraint_indexing:constraint_classification(influencer_driven_asset_bubbles, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: EARLY INFLUENCER (ROPE) — Benefits from first-mover advantage, audience growth, and arbitrage opportunities. Experiences the constraint as coordination: influencers aggregate audiences, provide information (albeit skewed), and enable market participation. Net beneficiary; extraction runs toward this agent.
constraint_indexing:constraint_classification(influencer_driven_asset_bubbles, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: REGULATORY COALITION (TANGLED ROPE) — Organized agents (SEC, financial regulators, social platforms) see this as a hybrid: genuine coordination problem (how do markets incorporate distributed information?) coupled with asymmetric extraction (bad-faith influencer coordination, insider trading). Mobile exit options but constrained by institutional capture and regulatory arbitrage.
constraint_indexing:constraint_classification(influencer_driven_asset_bubbles, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: MARKET STRUCTURE APPARATUS (PITON) — Listing standards, circuit breakers, disclosure requirements persist through institutional inertia. These mechanisms are largely performative for social-media-driven bubbles — they were designed for institutional trading, not for virality-driven coordination. Theater ratio high; functional verification low.
constraint_indexing:constraint_classification(influencer_driven_asset_bubbles, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (SNARE with IDENTITY_LOCKED COMPONENT) — The analytical observer faces cognitive capture through the very architecture being analyzed. Platform algorithms that create bubbles also structure the observer's information access. Identity-locked because the analytical framework itself is constituted through participation in the attention economy. Sees snare but cannot fully escape it.
constraint_indexing:constraint_classification(influencer_driven_asset_bubbles, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(identity_locked),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(influencer_driven_asset_bubbles_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(influencer_driven_asset_bubbles, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(influencer_driven_asset_bubbles, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(influencer_driven_asset_bubbles, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(influencer_driven_asset_bubbles, TR),
    TR >= 0.70.

:- end_tests(influencer_driven_asset_bubbles_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The constraint systematically transfers wealth from later-entering cohorts to earlier-entering cohorts through information asymmetry exploitation and coordinated amplification. Early influencers (and their insiders) achieve 10-100x returns while median retail participants experience 40-80% losses. This is classic extraction: value transfer without production. The value is not created by influencer activity — it's transferred from later buyers to earlier buyers through amplification and coordination. Suppression (0.72): Very high. Suppression operates through multiple mechanisms: (1) algorithmic amplification that creates availability cascades (most visible assets are promoted assets), (2) social proof dynamics that make exit costly in identity terms, (3) information asymmetry that prevents accurate risk assessment, (4) platform architecture that obscures influencer financial interests in promoted assets. Exiting is theoretically possible but practically suppressed through psychological and structural mechanisms. Theater ratio (0.81): Very high and escalating. Early-stage bubble justifications cite market fundamentals, technology adoption curves, and comparative valuations — these carry performative content but some connection to real analysis. Late-stage justifications are pure narrative: 'this asset is going up because everyone knows it's going up.' Market-making mechanisms (circuit breakers, listing standards, disclosure requirements) are performative theater — designed for institutional markets, they provide minimal protection against virality-driven bubbles. The theatrical ratio increases over the measurement interval as the constraint matures from coordination problem (early, ε=0.15) to pure extraction (late, ε=0.68).
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximum perspectival divergence. Early influencers and insiders experience coordination (Rope perspective) — they're aggregating audiences and enabling participation in emerging asset classes. Retail investors experience pure extraction (Snare perspective) — they're entering at peak extraction, with no coordination benefit. Regulatory observers see a hybrid (Tangled Rope perspective) — genuine market participation coordination coupled with bad-faith insider coordination. The analytical observer faces a critical identity-lock problem: the tools used to analyze bubbles (social network analysis, sentiment tracking, algorithmic auditing) are themselves structured by the attention economy being analyzed, creating a U₄ paradox. The market apparatus (Piton perspective) sees performative compliance — circuit breakers and disclosure requirements generate the ritual of market protection while remaining blind to virality-driven extraction. This perspectival gap is not a classification error; it's the diagnostic signature of a Snare: different agents genuinely perceive the constraint as serving different functions depending on their structural position.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values reflect each agent's structural relationship to the extraction flow. Early influencers with arbitrage exit options derive d ≈ 0.08 (full beneficiary), producing negative experienced extractiveness — the constraint subsidizes them. Retail investors who are trapped derive d ≈ 0.95 (full target), producing maximum experienced extractiveness through the sigmoid f(d). Regulatory observers who are organized but constrained by institutional capture derive d ≈ 0.50-0.60 (mixed position), producing moderate-high experienced extractiveness. The analytical observer with identity-locked exit options derives d ≈ 0.78 (victim with cognitive entanglement), producing high extractiveness that includes the additional cost of analytical capture. Platform infrastructure nominally derives d as a beneficiary (d ≈ 0.15) but faces under-specification in the beneficiaries array — this is an omega variable, as platform complicity is empirically uncertain and policy-dependent.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by showing that Snare is the only classification consistent with the base properties and all perspectives. The early-influencer Rope perspective is a genuinely-experienced perspective (they do benefit from coordination), but it does not override the Snare classification for the system as a whole because: (1) the extracted value is not produced by the coordination (value is transferred from later to earlier, not created), (2) suppression mechanisms operate primarily on victims, not beneficiaries, and (3) the constraint would not exist but for the extraction incentive — if removed, information coordination would still occur (through other mechanisms), but the extraction cascade would not. The Rope perspective is a local beneficiary perception, not a structural refutation of Snare classification. The Tangled Rope perspective for regulators is also legitimate but subordinate — they experience mixed effects because they are organizationally outside the direct extraction channel. The mandatrophy is resolved: Snare is the canonical type; other perspectives represent sub-structures within the Snare, not alternative classifications.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    intention_detection_threshold,
    'At what point does influencer information-sharing become coordinated market manipulation vs. authentic community building?',
    'Analysis of influencer communication patterns: private coordination channels, timing of disclosures, post-hoc analysis of influencer asset holding and sale timing relative to public promotion',
    'If high threshold: most bubbles appear as coordination failure (Rope from more perspectives). If low threshold: bubbles appear as deliberate extraction schemes (Snare confirmed from all perspectives).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intention_detection_threshold, empirical, 'Distinguishing coordinated manipulation from community building').

omega_variable(
    algorithmic_amplification_agency,
    'How much of the bubble extraction results from influencer intent vs. algorithmic feedback loops that amplify engagement regardless of influencer strategy?',
    'Comparative analysis of bubble dynamics on platforms with different recommendation algorithms; A/B testing of algorithm dampening vs. amplification; influencer intent analysis (private messages, deleted content)',
    'If primarily algorithmic: suppression mechanisms shift from human intent to platform architecture (changes victim classification). If primarily intentional: snare classification confirmed with high confidence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(algorithmic_amplification_agency, empirical, 'Relative contribution of influencer intent vs. algorithmic amplification').

omega_variable(
    retail_investor_exit_feasibility,
    'Do retail investors actually face material barriers to exiting bubble positions, or is their ''trappedness'' primarily a result of cognitive biases and identity lock?',
    'Analysis of transaction costs, liquidity windows, holding period distributions, and post-bubble retrospective interviews with participants about why they didn''t exit earlier',
    'If material barriers dominant: exit_options correctly classified as ''trapped''. If cognitive/identity factors dominant: exit_options should be ''identity_locked'' and perspectives require recalibration.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(retail_investor_exit_feasibility, empirical, 'Whether retail investor trappedness is material or cognitive').

omega_variable(
    market_integrity_victim_agency,
    'Can ''market integrity'' organize and exercise collective power, or is it a purely abstract victim incapable of structural response?',
    'Historical analysis of market reform movements following bubbles; effectiveness of circuit breakers and circuit-breaker-like mechanisms in different jurisdictions; social coordination among dispersed retail participants',
    'If market integrity can organize: victim classification changes from powerless to organized (changes classification from Snare toward Tangled Rope). If purely abstract: powerless classification confirmed.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(market_integrity_victim_agency, conceptual, 'Whether market integrity can exercise collective agency').

omega_variable(
    platform_complicity_threshold,
    'At what level of platform knowledge and algorithmic optimization for engagement do platforms become beneficiaries (and thus complicit in extraction) vs. neutral infrastructure?',
    'Internal platform documentation regarding algorithm design for engagement and retention; platform revenue flows during bubbles; correlation between platform feature updates and bubble intensity',
    'If platforms are knowing beneficiaries: they belong in beneficiaries array and perspectives should include platform-as-institutional-beneficiary. If neutral: early_influencers_and_insiders is the primary beneficiary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(platform_complicity_threshold, empirical, 'Platform complicity in bubble extraction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(influencer_driven_asset_bubbles, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(influencer_bubble_tr_t0, influencer_driven_asset_bubbles, theater_ratio, 0, 0.45).
narrative_ontology:measurement(influencer_bubble_tr_t3, influencer_driven_asset_bubbles, theater_ratio, 3, 0.65).
narrative_ontology:measurement(influencer_bubble_tr_t6, influencer_driven_asset_bubbles, theater_ratio, 6, 0.81).

% Extraction over time
narrative_ontology:measurement(influencer_bubble_be_t0, influencer_driven_asset_bubbles, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(influencer_bubble_be_t3, influencer_driven_asset_bubbles, base_extractiveness, 3, 0.42).
narrative_ontology:measurement(influencer_bubble_be_t6, influencer_driven_asset_bubbles, base_extractiveness, 6, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(influencer_driven_asset_bubbles, information_standard).
narrative_ontology:affects_constraint(influencer_driven_asset_bubbles, retail_investor_financial_vulnerability).
narrative_ontology:affects_constraint(influencer_driven_asset_bubbles, platform_algorithmic_amplification).
narrative_ontology:affects_constraint(influencer_driven_asset_bubbles, regulatory_arbitrage_in_asset_markets).

% DUAL FORMULATION NOTE:
% Influencer-driven bubbles decompose into three structurally distinct constraints: (1) information_standard coordination (how market participants aggregate dispersed signals), (2) algorithmic_amplification mechanism (how platforms structure visibility and attention), (3) retail_investor_vulnerability (how participation structures create trappedness). This story focuses on the combined constraint; downstream stories model individual mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(influencer_driven_asset_bubbles, institutional, 0.22).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
