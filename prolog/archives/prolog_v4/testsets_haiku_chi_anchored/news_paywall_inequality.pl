% ============================================================================
% CONSTRAINT STORY: news_paywall_inequality
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_news_paywall_inequality, []).

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
 *   constraint_id: news_paywall_inequality
 *   human_readable: Information Asymmetry due to News Paywalls
 *   domain: social/media/economics
 *
 * SUMMARY:
 *   News paywalls create a structural tension between the economic necessity
 *   of funding quality journalism and the democratic requirement for
 *   universal information access. Paywall-based revenue is framed by
 *   publishers as a coordination mechanism — solving the collective action
 *   problem of digital journalism sustainability — but operates as asymmetric
 *   extraction from low-income readers who are priced out of quality
 *   information. The constraint exhibits multiple legitimate classifications
 *   depending on observer position: publishers see coordination (Rope),
 *   low-income readers see pure extraction (Snare), organized equity
 *   advocates see mixed coordination-extraction (Tangled Rope), and
 *   alternative funding movements see a temporary problem with a sunset
 *   (Scaffold). The theater ratio (0.48) reflects that paywall enforcement is
 *   partly performative: technical barriers (cookie walls, metering,
 *   registration friction) provide more theatrical friction than actual
 *   exclusion, as readers bypass paywalls through incognito browsing, link
 *   sharing, aggregators, and library access. Extractiveness has risen from
 *   0.30 to 0.52 over the measurement interval as publishers have hardened
 *   paywalls in response to digital ad collapse, indicating that the
 *   constraint is intensifying rather than resolving through market
 *   mechanisms.
 *
 * KEY AGENTS:
 *   - Low-Income Readers: Primary victim (powerless/trapped) — excluded from quality journalism by price, forced into low-quality information diet; no meaningful exit option
 *   - Premium Publishers (NYT, WSJ, FT, etc.): Primary beneficiary (institutional/arbitrage) — capture subscription revenue and reader data; justify paywalls as necessary for journalism funding
 *   - Affluent Readers: Secondary beneficiary/partial victim (moderate/constrained) — can afford subscriptions but face paywall friction; gain access but bear coordination cost
 *   - Information Equity Movement (nonprofits, literacy advocates, open-access journalists): Organized victim (organized/mobile) — organize to build alternatives and demand access; benefit from public demand but constrained by publisher market power
 *   - Traditional Journalism Industry: Institutional actor (institutional/arbitrage) — maintains paywall model as degraded sustenance mechanism for print-era economics; theater reflects partial effectiveness
 *   - Public Media Coalition (NPR, BBC, Guardian philanthropy, ProPublica, nonprofit newsrooms): Organized beneficiary (organized/mobile) — building post-paywall funding models with genuine coordination benefits; have exit options and sunset vision
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent paywall economics as inherent law of information
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(news_paywall_inequality, 0.52).
domain_priors:suppression_score(news_paywall_inequality, 0.65).
domain_priors:theater_ratio(news_paywall_inequality, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(news_paywall_inequality, extractiveness, 0.52).
narrative_ontology:constraint_metric(news_paywall_inequality, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(news_paywall_inequality, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(news_paywall_inequality, tangled_rope).
narrative_ontology:human_readable(news_paywall_inequality, "Information Asymmetry due to News Paywalls").
narrative_ontology:topic_domain(news_paywall_inequality, "social/media/economics").

domain_priors:requires_active_enforcement(news_paywall_inequality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(news_paywall_inequality, premium_publishers).
narrative_ontology:constraint_beneficiary(news_paywall_inequality, affluent_readers).
narrative_ontology:constraint_victim(news_paywall_inequality, low_income_readers).
narrative_ontology:constraint_victim(news_paywall_inequality, epistemic_public_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LOW-INCOME READER (SNARE) — Cannot afford premium subscriptions; trapped in low-quality information diet. Faces full extraction: quality journalism is gatekept, forcing reliance on free but unreliable sources, social media algorithms, or no news at all. No meaningful exit option; cost of political disengagement is high. d≈0.92, f(d)≈1.40, σ=1.0 → χ≈0.73.
constraint_indexing:constraint_classification(news_paywall_inequality, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: PREMIUM PUBLISHER (ROPE) — Experiences paywall as coordination mechanism: solves the collective action problem of sustainable journalism funding. Subscription revenue enables quality reporting. Coordination benefit (quality journalism) offsets perceived suppression cost. d≈0.08, f(d)≈-0.10, σ=1.2 → χ≈-0.05. Net beneficiary; negative effective extraction.
constraint_indexing:constraint_classification(news_paywall_inequality, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: AFFLUENT READER (TANGLED ROPE) — Can afford premium subscriptions but faces paywall friction (multiple subscriptions for different outlets, registration delays). Benefits from paywalled quality journalism but also constrained by paywall enforcement. Hybrid: gains information access (coordination benefit) but pays extraction cost. d≈0.50, f(d)≈0.65, σ=1.0 → χ≈0.34.
constraint_indexing:constraint_classification(news_paywall_inequality, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: INFORMATION EQUITY MOVEMENT (TANGLED ROPE) — Organized actors (news literacy nonprofits, public media advocates, open-access journalists) see paywalls as both solving a coordination problem (funding journalism) and creating asymmetric extraction. Movement benefits from the public's demand for information access (coordination function) but is constrained by publisher market power. Has exit options (build alternative platforms, promote public media) but these are resource-intensive. d≈0.55, f(d)≈0.75, σ=1.0 → χ≈0.39.
constraint_indexing:constraint_classification(news_paywall_inequality, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: TRADITIONAL JOURNALISM MODEL (PITON) — Paywalls are a degraded sustenance mechanism for print-era journalism economics. Historically, classified ads and subscriber revenue funded journalism; paywalls replicate that model in digital form. But the function has atrophied: most digital paywalls capture < 10% of revenue compared to print subscriptions' 30-40%. Theater ratio = 0.48 reflects that paywall enforcement (cookie walls, metering) is partly performative—sophisticated readers bypass via incognito mode, article links, or aggregators—while the underlying economic problem (digital ad collapse) remains unsolved. The model persists through inertia, not effectiveness.
constraint_indexing:constraint_classification(news_paywall_inequality, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: PUBLIC MEDIA COALITION (SCAFFOLD) — Public broadcasters, nonprofit newsrooms, and open-access platforms see paywalls as a temporary coordination failure with a sunset. Alternative funding models (philanthropic, membership, public funding, collaborative journalism networks) are creating post-paywall pathways. These platforms have low suppression and genuine coordination benefits without extraction. As these mature over generational timescales, traditional paywall extraction loses force. Has sunset clause: open-access norms and funded journalism alternatives are building to replace paywall-dependent models.
constraint_indexing:constraint_classification(news_paywall_inequality, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, paywalls might appear as an immutable law of information economics: quality journalism requires funding, funding requires payment, payment requires exclusion. But the structural data (ε=0.52, suppression=0.65, theater=0.48) contradicts the mountain classification. The constraint is contingent on specific institutional arrangements (copyright enforcement, subscription technology, digital ad market collapse) rather than fundamental. The engine will compute this as a false summit, revealing that 'paying for information is inevitable' naturalizes what is actually a policy choice.
constraint_indexing:constraint_classification(news_paywall_inequality, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(news_paywall_inequality_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(news_paywall_inequality, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(news_paywall_inequality, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(news_paywall_inequality, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(news_paywall_inequality, TR),
    TR >= 0.70.

:- end_tests(news_paywall_inequality_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderately high. Publishers capture significant economic rent from subscriber willingness to pay; low-income readers bear the full cost of exclusion. But extractiveness is not maximal (would be 0.70+) because: (1) paywalls do solve a real coordination problem (funding quality journalism), (2) free/low-cost alternatives exist (public media, free outlets, libraries, social media), and (3) extraction is enforced through price/technology, not pure coercion. The upward trend (0.30→0.52) reflects publishers hardening paywalls in response to digital ad collapse—the constraint is intensifying. Suppression (0.65): High. Low-income readers face multiple barriers: subscription cost, registration friction, technical exclusion, and reduced alternative quality. But not total suppression—library access, free tier articles, and aggregator links provide some workarounds. Theater ratio (0.48): Moderate. Paywall enforcement mechanisms (cookie walls, metering, incognito blocking, registration delays) are substantially performative—sophisticated readers bypass them with effort. Actual enforcement effectiveness is lower than technical sophistication suggests. Theater has increased over time (0.25→0.48) as publishers add theatrical barriers to supplement price-based exclusion.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates stark perspectival divergence: Publishers see coordination (Rope)—paywalls fund journalism and solve advertiser collapse. Low-income readers see extraction (Snare)—they are excluded from quality information with no exit option. Affluent readers see mixed coordination-extraction (Tangled Rope)—they gain access but pay friction costs. Organized equity advocates see constrained opposition to hybrid extraction (Tangled Rope)—the system solves publishers' funding problem but not the public's access problem. Public media builders see a temporary problem with alternatives (Scaffold)—philanthropic and public funding models are maturing to replace paywalls over generational timescales. The traditional journalism industry sees its own degraded model (Piton)—paywall revenue is 10-15% of what print subscriptions achieved, suggesting the model is sustained by inertia. The analytical observer risks naturalizing paywalls as inevitable (Mountain)—the false summit is the claim that 'someone has to pay for journalism, so exclusion is necessary,' which ignores that public funding, philanthropy, and membership models already demonstrate alternatives.
 *
 * DIRECTIONALITY LOGIC:
 *   Low-income readers: Victim + trapped → d≈0.92, f(d)≈1.40. Maximum extraction; no alternative. Premium publishers: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiary; negative effective extraction. Affluent readers: Beneficiary + constrained → d≈0.50, f(d)≈0.65. Symmetric mixed experience—benefit from access but constrained by friction and cost. Information equity advocates: Victim + mobile → d≈0.55, f(d)≈0.75. Organized actors can build alternatives but are constrained by publisher dominance. Public media coalition: Organized + mobile → d≈0.40, f(d)≈0.40. Low effective extraction; coalition has agency and clear exit pathway through alternative funding. Traditional journalism: Institutional + arbitrage → d≈0.08, f(d)≈-0.10. Piton classification comes from theater ratio gate (0.48 is moderate but not high enough for piton; however, the degraded function and inertial maintenance are piton signatures). Analytical observer: analytical → d≈0.72, f(d)≈1.15. Mountain classification is perspectival false summit—naturalizes policy choice as law.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by distinguishing between two competing claims: (1) 'Paywalls are a necessary coordination mechanism for journalism funding' (Rope framing), and (2) 'Paywalls are an extractive gatekeeping mechanism that creates information inequality' (Snare framing). The Tangled Rope classification (ε=0.52, suppression=0.65, requires_active_enforcement=true, both beneficiaries and victims) captures that BOTH claims are structurally true: paywalls do provide coordination value (they fund journalism that wouldn't exist on ads alone), AND they create asymmetric extraction (low-income readers are excluded). The mandatrophy is resolved by refusing to collapse to a single type. The constraint is hybrid: its coordination function is genuine but limited (paywalls fund some journalism quality but not all, as evidenced by theater_ratio showing enforcement is partly performative); its extraction function is real (information access is gatekept by price). The Scaffold perspective (public media/philanthropy building post-paywall models) and Piton perspective (traditional journalism's degraded sustenance mechanism) together suggest the constraint is transitional: paywalls are hybrid today, but alternative funding pathways are maturing. The false summit (Mountain perspective) is the claim that paywalls are inevitable—policy and institutional choice, not law of nature.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sustainable_journalism_funding_path,
    'What alternative funding model (philanthropy, membership, public funding, syndication, or hybrid) can sustain quality journalism without information-based extraction?',
    'Case study analysis of nonprofit newsrooms, public broadcasters, and membership-based outlets; longitudinal tracking of journalism quality and sustainability metrics across funding models; economic modeling of scalability',
    'If viable alternatives exist and scale: paywall extraction is a policy choice, not necessity (Scaffold or Rope). If alternatives remain niche: paywalls may be unavoidable (Mountain or Snare).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sustainable_journalism_funding_path, empirical, 'Whether non-paywall models can sustainably fund quality journalism').

omega_variable(
    information_inequality_causal_chain,
    'Does paywall-induced information asymmetry causally degrade low-income readers'' decision-making on issues requiring detailed knowledge (healthcare, finance, education, politics)?',
    'Comparative analysis: decision outcomes for paywall-exposed vs paywall-free readers controlling for income; experimental exposure to quality journalism vs free sources; longitudinal tracking of knowledge gaps and decision quality',
    'If causal degradation is severe: victims perspective (Snare) is confirmed and extraction severity is high. If information gaps close over time through alternative sources: extraction severity may be overstated (closer to Rope for public).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(information_inequality_causal_chain, empirical, 'Whether paywalls causally degrade low-income readers'' decision-making').

omega_variable(
    paywall_enforcement_technological_cost,
    'What proportion of paywall infrastructure cost goes to actual content exclusion vs authentication theater (cookie walls, metering, registration delays, incognito blocking)?',
    'Technical audit of paywall systems; comparison of enforcement costs across publishers; measurement of actual exclusion rates vs attempted exclusion; user behavior analysis on bypass methods',
    'If enforcement cost is high relative to exclusion effectiveness: theater_ratio may be understated, suggesting Piton classification is stronger. If enforcement is highly effective: theater_ratio estimate is correct, and Tangled Rope classification is appropriate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(paywall_enforcement_technological_cost, empirical, 'What fraction of paywall cost is theater vs functional exclusion').

omega_variable(
    public_media_alternative_viability,
    'Can publicly funded or philanthropically funded journalism scale to replace market-based paywall models while maintaining editorial independence and quality?',
    'Comparative institutional analysis of NPR, BBC, Guardian (philanthropic), ProPublica, nonprofit state bureaus; measurement of editorial independence metrics, revenue stability, and reach',
    'If public/philanthropic models can scale: Scaffold perspective is confirmed and sunset timeline is real (15-30 years). If they remain niche: alternatives are aspirational, and paywalls remain extractive long-term (Snare/Tangled Rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(public_media_alternative_viability, empirical, 'Whether public/philanthropic journalism can scale as paywall alternative').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(news_paywall_inequality, 0, 14).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(paywall_tr_t0, news_paywall_inequality, theater_ratio, 0, 0.25).
narrative_ontology:measurement(paywall_tr_t7, news_paywall_inequality, theater_ratio, 7, 0.37).
narrative_ontology:measurement(paywall_tr_t14, news_paywall_inequality, theater_ratio, 14, 0.48).

% Extraction over time
narrative_ontology:measurement(paywall_be_t0, news_paywall_inequality, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(paywall_be_t7, news_paywall_inequality, base_extractiveness, 7, 0.42).
narrative_ontology:measurement(paywall_be_t14, news_paywall_inequality, base_extractiveness, 14, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(news_paywall_inequality, information_standard).
narrative_ontology:affects_constraint(news_paywall_inequality, digital_divide_information_access).
narrative_ontology:affects_constraint(news_paywall_inequality, journalism_business_model_collapse).
narrative_ontology:affects_constraint(news_paywall_inequality, algorithmic_misinformation_amplification).

% DUAL FORMULATION NOTE:
% News paywalls are downstream of journalism business model collapse (digital ad collapse forced publishers to pursue subscription revenue) but represent a distinct structural constraint. The upstream constraint has ε reflecting empirical uncertainty about ad-supported sustainability; the paywall constraint has ε=0.52 reflecting the policy choice to extract from readers rather than explore alternative funding. Paywalls also interact with algorithmic misinformation amplification: low-income readers excluded from paywalled quality sources have higher exposure to free but unreliable information on social platforms. These are separate constraints linked through information economics and equity.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
