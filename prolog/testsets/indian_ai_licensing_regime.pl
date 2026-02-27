% ============================================================================
% CONSTRAINT STORY: indian_ai_licensing_regime
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_indian_ai_licensing_regime, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: indian_ai_licensing_regime
 *   human_readable: India's Sovereign AI Licensing and Data Localization Mandate
 *   domain: technological/political
 *
 * SUMMARY:
 *   India's AI Licensing Regime, enacted following the 2026 Delhi AI Expo
 *   under the 'AI for All' initiative, represents a nation-state attempt to
 *   maintain technological sovereignty while developing domestic AI capacity.
 *   The regime imposes mandatory licensing for all AI model deployments in
 *   India, requires data localization (training data and inference logs must
 *   reside on Indian infrastructure), mandates model weights escrow to
 *   government repositories, and subjects all deployments to algorithmic
 *   audit. This constraint exhibits Tangled Rope structure: it combines
 *   genuine coordination functions (protecting domestic innovation from
 *   multinational capture, solving the collective action problem of
 *   developing AI infrastructure at scale) with significant asymmetric
 *   extraction (licensing fees, surveillance access, technology transfer
 *   requirements imposed on multinationals). The regime is neither pure
 *   coordination (Rope) nor pure extraction (Snare) — it serves both
 *   functions simultaneously, with different agents experiencing different
 *   balances. The extractiveness has increased over 12 months (0.35 → 0.58)
 *   as licensing terms have become stricter and compliance costs have
 *   accumulated. Theater ratio has also increased (0.48 → 0.62), indicating
 *   that much licensing activity is performative review rather than
 *   functional technical oversight. This combination signals potential
 *   Mandatrophy drift: if theater continues rising without corresponding
 *   functional capacity (audit effectiveness, harm prevention), the regime
 *   risks degrading from Tangled Rope toward Piton (institutional inertia
 *   maintaining a degraded apparatus).
 *
 * KEY AGENTS:
 *   - Indian Government / Ministry of Technology: Primary beneficiary (institutional/arbitrage) — controls licensing authority, captures fees, gains surveillance access, directs strategic AI development
 *   - Domestic AI Startups: Secondary beneficiary (organized/constrained) — receive market protection and preferential licensing, but constrained by data localization and compliance costs
 *   - Multinational AI Firms (OpenAI, Google, Meta, etc.): Primary victims (powerful/constrained) — face licensing costs, data localization requirements, model transparency mandates; cannot exit Indian market without abandoning 1.4B users
 *   - Indian Consumers / Developers: Secondary victims (powerless/trapped) — restricted access to frontier models, cannot migrate to international platforms due to data localization enforcement, face higher prices
 *   - Licensing Board Bureaucracy: Institutional actor maintaining performative compliance apparatus (institutional/arbitrage) — persists through inertia despite capacity constraints
 *   - Open-Source Community: Alternative pathway (organized/mobile) — creates substitute for licensed models, building sunset trajectory through decentralized AI infrastructure
 *   - Licensed Domestic Champions (Jio, TCS, Indian unicorns): Beneficiaries with market protection (powerful/arbitrage) — shape licensing terms and receive preferential treatment
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(indian_ai_licensing_regime, 0.58).
domain_priors:suppression_score(indian_ai_licensing_regime, 0.68).
domain_priors:theater_ratio(indian_ai_licensing_regime, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(indian_ai_licensing_regime, extractiveness, 0.58).
narrative_ontology:constraint_metric(indian_ai_licensing_regime, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(indian_ai_licensing_regime, theater_ratio, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(indian_ai_licensing_regime, tangled_rope).
narrative_ontology:human_readable(indian_ai_licensing_regime, "India's Sovereign AI Licensing and Data Localization Mandate").
narrative_ontology:topic_domain(indian_ai_licensing_regime, "technological/political").

domain_priors:requires_active_enforcement(indian_ai_licensing_regime).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(indian_ai_licensing_regime, indian_ai_startups).
narrative_ontology:constraint_beneficiary(indian_ai_licensing_regime, domestic_data_infrastructure).
narrative_ontology:constraint_beneficiary(indian_ai_licensing_regime, government_revenue_stream).
narrative_ontology:constraint_victim(indian_ai_licensing_regime, multinational_ai_firms).
narrative_ontology:constraint_victim(indian_ai_licensing_regime, consumer_access_diversity).
narrative_ontology:constraint_victim(indian_ai_licensing_regime, developer_mobility).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CONSUMER/DEVELOPER END-USER (SNARE) — Trapped within Indian borders. Licensing regime restricts access to frontier AI models, data portability, and competitive pricing. Cannot exit to international platforms without violating data localization laws. Suppression is absolute: technical barriers (IP blocking), legal barriers (criminal penalties for unauthorized model export), and economic barriers (licensing fees make alternatives unaffordable) all converge. Experiences pure extraction through access restrictions and pricing power granted to licensed incumbents.
constraint_indexing:constraint_classification(indian_ai_licensing_regime, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: DOMESTIC AI SECTOR (TANGLED ROPE) — Indian startups and researchers benefit from protected market access and preferential licensing terms. But also constrained: must comply with data residency requirements (expensive), cannot freely access international training data (legal barriers), and face government monitoring/control through licensing conditions. Mixed experience: genuine coordination benefit (protection from multinationals) combined with asymmetric extraction (surveillance, licensing fees, regulatory compliance burden). Active enforcement required through licensing board compliance mechanisms.
constraint_indexing:constraint_classification(indian_ai_licensing_regime, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: GOVERNMENT LICENSING AUTHORITY (ROPE) — Experiences the constraint as a coordination mechanism: licensing regime solves the collective action problem of domestic AI sector development (without it, multinationals would capture the market). Government benefits from: licensing fee revenue, surveillance access to training data pipelines, strategic control over AI development trajectory, and political credit for 'AI for All' sovereignty narrative. High arbitrage exit: government can modify regulations unilaterally, and the constraint enforces compliance through state power rather than coercion. Low experienced extraction because government holds regulatory authority.
constraint_indexing:constraint_classification(indian_ai_licensing_regime, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: MULTINATIONAL AI FIRMS (SNARE) — Constrained exit: cannot serve Indian market without licensing (abandoning 1.4B-user market is costly); cannot license without submitting to data localization, model transparency, and government audit. Licensing conditions include mandatory model weights escrow, training data residency, and algorithmic audit access. Effective extraction: licensing fees, compliance costs, and competitive disadvantage relative to domestic licensees. Suppression is high: legal barriers (licensing is mandatory, not optional), technical barriers (firewalls), and economic barriers (cost of compliance exceeds margins in many use cases). However, multinationals retain some arbitrage power (can license for premium tiers, retain international operations outside India).
constraint_indexing:constraint_classification(indian_ai_licensing_regime, snare,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: INTERNATIONAL OPEN-SOURCE COMMUNITY (SCAFFOLD) — Open-source AI frameworks (PyTorch, Hugging Face, OpenAI API alternatives) create parallel pathways that bypass licensing entirely. This perspective sees India's regime as a temporary coordination problem being solved by distributed development and international standards. Exit option: developers migrate code to jurisdictions with lighter regulation. Theater is moderate (0.62): the licensing regime performs sovereignty but is increasingly bypassed through decentralized ML infrastructure. Sunset logic applies: as open-source models mature and international federated learning protocols develop, centralized licensing loses enforcement power. The scaffold expects this regime to degrade within 5-10 years as technology diffuses.
constraint_indexing:constraint_classification(indian_ai_licensing_regime, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: LICENSING BOARD BUREAUCRACY (PITON) — The licensing board (Ministry of Technology's AI Licensing Authority) maintains a performative compliance apparatus: application review processes, quarterly audits, model transparency reports. But the actual enforcement function is degraded: licensing board lacks technical capacity to audit frontier models, relies on self-reported compliance, and faces massive review backlogs (estimated 18-month licensing decision timelines). The bureaucracy persists through institutional inertia (government commitment to 'AI for All' narrative) and because alternatives (blanket bans, no regulation) appear worse. Theater ratio (0.62) reflects that much licensing activity is paperwork circulation with limited functional verification. Theater has increased over the interval as application volume exceeded review capacity.
constraint_indexing:constraint_classification(indian_ai_licensing_regime, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: REGIONAL TECH HUBS (TANGLED ROPE) — States like Karnataka (Bangalore), Maharashtra (Mumbai), Telangana (Hyderabad) benefit from licensing regime because it concentrates AI development domestically and prevents brain drain to Silicon Valley. But also constrained: must implement licensing compliance infrastructure locally, compete with central government for regulatory authority, and cannot offer exemptions to attract multinational R&D centers. Mixed: coordination (protection of regional talent and investment) combined with extraction (license fees, state-level regulatory compliance burden). Active enforcement through state-level AI councils and monitoring.
constraint_indexing:constraint_classification(indian_ai_licensing_regime, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 8: LICENSED DOMESTIC CHAMPIONS (ROPE) — Government-favored licensees (telecom giants like Jio, IT giants like TCS, newly created Indian AI unicorns) experience the constraint as pure coordination and market protection. They benefit from: protected market access, preferential licensing terms, government contracts, and access to government-funded training data. Exit option (arbitrage) is high: as licensees, they can negotiate licensing terms with government, can operate internationally, and have capital to absorb compliance costs. Low experienced extraction because they shape the rules.
constraint_indexing:constraint_classification(indian_ai_licensing_regime, rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 9: ANALYTICAL OBSERVER / DATA SOVEREIGNTY AS NATURAL LAW (MOUNTAIN) — From a civilizational perspective, some jurisdictional control over data flows is inherent to nation-state sovereignty: countries must retain ability to govern flows of strategic information. This perspective treats data localization as an immutable requirement of statehood, similar to border control. Emerges naturally from geopolitical realities. However, this classification is likely a false summit: the structural data (high suppression, high theater, measured extractiveness) reveals that India's regime is a contingent institutional choice, not an invariant law. The regime could be organized differently (lighter-touch regulation, open data markets) while preserving legitimate sovereignty interests.
constraint_indexing:constraint_classification(indian_ai_licensing_regime, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(indian_ai_licensing_regime_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(indian_ai_licensing_regime, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(indian_ai_licensing_regime, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(indian_ai_licensing_regime, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(indian_ai_licensing_regime, TR),
    TR >= 0.70.

:- end_tests(indian_ai_licensing_regime_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high and rising. The regime extracts from multiple sources: licensing fees from multinationals (estimated ₹500Cr+ annually), data access from training pipelines (government gains surveillance), and compliance costs from all operators (shifting economic value to compliance infrastructure). The increase from 0.35 to 0.58 reflects tightening licensing conditions and expanded audit requirements. This is not low extraction (Rope threshold ≤0.45) — the asymmetry is substantial. Suppression (0.68): High. Multiple barriers prevent exit: legal (licensing mandatory, violators face criminal penalties and fines), technical (IP blocking and data residency enforcement through infrastructure controls), and economic (compliance costs are substantial). However, suppression is not absolute (0.95+) because open-source alternatives and international platforms create partial exit options despite restrictions. Theater ratio (0.62): Moderate-high and rising. Licensing board's technical capacity lags demand: 18-month decision timelines, insufficient staff expertise in frontier models, reliance on self-reported compliance data. Much licensing activity is paperwork review rather than functional technical audit. Yet the regime is not purely theatrical (theater > 0.85) because some licensees do face genuine oversight and some harmful deployments are caught. The increase from 0.48 to 0.62 reflects growing gap between regulatory ambition and actual capacity as application volumes outpaced hiring.
 *
 * PERSPECTIVAL GAP:
 *   This constraint generates a perspectival gap across all eight agent perspectives. The government sees Rope (coordination mechanism protecting domestic capacity). Domestic startups see Tangled Rope (protection combined with compliance burden). Multinational firms see Snare (trapped, high extraction). Consumers see Snare (access restricted, prices inflated). The licensing board sees Piton (performative apparatus maintained through inertia). Open-source community sees Scaffold (temporary regulatory barrier being bypassed by technology diffusion). Licensed champions see Rope (market protection, low extraction). The analytical observer risks seeing Mountain (data sovereignty as natural law) — but the structural data (rising theater, rising extractiveness, capacity constraints) reveals this as a false summit. The regime could be designed differently (lighter-touch oversight, performance-based rather than prescriptive regulation) while preserving genuine sovereignty interests. The perspectival gap is driven by differential exit options: those with arbitrage power (government, licensed champions) see coordination; those with trapped or constrained exit (consumers, multinationals) see extraction; those building alternatives (open-source) see temporary friction.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from structural position and exit options. Government has high arbitrage (can modify regulations unilaterally) plus beneficiary status (revenue, control) → d ≈ 0.05 (full beneficiary) → f(d) ≈ -0.12 → negative χ (experiences benefit). Domestic startups have constrained exit (cannot easily switch to international infrastructure due to compliance requirements) plus mixed beneficiary/victim status (protected but burdened) → d ≈ 0.35-0.45 → f(d) ≈ 0.40-0.60 → moderate χ (mixed extraction). Multinational firms have constrained exit (cannot serve market without licensing, cannot exit market without business loss) plus clear victim status (extraction of fees, data, compliance burden) → d ≈ 0.70-0.80 → f(d) ≈ 1.05-1.20 → high χ (strong extraction experienced). Consumers have trapped exit (cannot legally access international platforms if data residency violated) plus victim status → d ≈ 0.85-0.95 → f(d) ≈ 1.30-1.42 → very high χ (maximum extraction experienced). The regime's χ varies significantly across agents (from -0.12 for government to +1.35 for trapped consumers), which is the defining feature of Tangled Rope classification: different agents experience the same structural constraint as having radically different extractiveness.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED: This constraint avoids the false-positive snare classification by explicitly identifying the coordination functions (domestic capacity development, collective action solution) alongside the extraction mechanisms. The Tangled Rope classification with explicit beneficiaries + victims + enforcement requirement prevents misclassification as either pure Rope ('it's just coordination') or pure Snare ('it's just extraction'). The regime genuinely coordinates domestic AI development and genuinely extracts from multinationals and consumers simultaneously — these are not contradictory, but rather the defining feature of Tangled Rope. The mandatrophy resolution mechanism is the presence of 'requires_active_enforcement: true' plus multiple beneficiary and victim declarations. However, the rising theater_ratio (0.48 → 0.62) creates a secondary mandatrophy risk: if theater continues to rise without functional capacity improvement, the regime could degrade from Tangled Rope toward Piton (institutional inertia replacing coordination function). This degradation pathway would represent a different mandatrophy: misclassifying a degraded Piton as a healthy Tangled Rope. The measurements track this risk through theater_ratio trajectory — if theater exceeds 0.75 in future intervals without corresponding audit effectiveness improvements, reclassification to Piton would be indicated.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    licensing_effectiveness_threshold,
    'Does the licensing regime actually prevent harmful AI deployment, or does it primarily create compliance theater while allowing licensed players to operate with minimal oversight?',
    'Comparative analysis: licensing approval timelines vs actual harm incidents; audit report effectiveness vs self-reported compliance rates; technical audit depth vs model complexity',
    'If effective: regime is Rope or Tangled Rope (coordination + some extraction justified by safety). If theater: regime is Piton (degraded bureaucracy) or Snare (pure extraction masquerading as safety).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(licensing_effectiveness_threshold, empirical, 'Whether licensing regime provides genuine oversight or merely bureaucratic theater').

omega_variable(
    data_sovereignty_vs_innovation_tradeoff,
    'Does data localization requirement genuinely enhance Indian AI competitiveness, or does it impose costs that prevent domestic firms from competing with international players who can access global training datasets?',
    'Longitudinal comparison: model performance metrics for Indian domestic models vs international baseline; cost analysis of localized vs distributed training infrastructure; startup funding flows and exit valuations pre/post regulation',
    'If localization enhances competitiveness: regime is Rope (genuine coordination benefit). If localization reduces competitiveness: regime is Snare (extraction from domestic innovators disguised as protection).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(data_sovereignty_vs_innovation_tradeoff, empirical, 'Whether data localization mandate supports or hinders AI competitiveness').

omega_variable(
    consumer_welfare_impact,
    'Does licensing regime improve or degrade consumer access to AI services? Do end-users benefit from domestic alternatives, or do they face restricted choice and higher prices?',
    'Market analysis: price comparison (licensed Indian models vs international alternatives pre-2026); consumer access metrics (geographic coverage, model diversity); adoption rates for licensed vs unlicensed services',
    'If consumer access improved: regime is Rope. If consumer access degraded: regime is Snare from consumer perspective. This drives whether suppression (0.68) is justified by safety benefits or represents pure extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(consumer_welfare_impact, empirical, 'Whether licensing regime improves or restricts consumer AI access').

omega_variable(
    enforcement_capacity_vs_regulatory_reach,
    'Can the Indian licensing board actually enforce compliance at scale, or is the regime''s enforcement capacity permanently captured by larger players who can afford compliance costs?',
    'Regulatory capacity audit: review staff expertise, budget per pending application, average time to licensing decision, enforcement action frequency; comparative analysis with other regulatory bodies (telecom, pharmaceuticals)',
    'If enforcement capacity scales with demand: regime remains Tangled Rope. If capacity is permanently limited: regime degrades to Piton (theater) and Snare (large players can flout rules while small players bear compliance burden).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(enforcement_capacity_vs_regulatory_reach, empirical, 'Whether licensing board can enforce compliance at necessary scale').

omega_variable(
    open_source_substitutability,
    'Can open-source AI models (Meta''s Llama, Stability AI''s Stable Diffusion, community models) adequately substitute for licensed proprietary models, creating an exit option for Indian developers despite licensing restrictions?',
    'Technical feasibility analysis: model performance parity, ease of deployment in India''s data localization environment, cost comparison; adoption metrics for open-source alternatives among Indian developers',
    'If open-source is adequate substitute: regime is Scaffold (sunset logic applies as technology matures). If open-source is inadequate: regime is Snare (users are trapped regardless of licensing status).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(open_source_substitutability, empirical, 'Whether open-source models provide viable alternative to licensed systems').

omega_variable(
    geopolitical_leverage_motivation,
    'Is India''s licensing regime primarily motivated by AI safety/sovereignty concerns, or is it a geopolitical tool to extract concessions from U.S./Chinese AI firms and increase Indian government revenue?',
    'Government document analysis: stated regulatory goals vs actual licensing conditions; revenue tracking and allocation; licensing decision patterns (preferential terms for domestic actors vs arm''s-length treatment); diplomatic signals during international AI governance forums',
    'If safety/sovereignty motivated: regime is Rope (legitimate coordination). If geopolitical extraction motivated: regime is Snare (pure extraction from multinationals) or Tangled Rope (extraction layered over coordination benefit). This determines whether extractiveness (0.58) is justified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(geopolitical_leverage_motivation, conceptual, 'Whether licensing regime is driven by safety/sovereignty or geopolitical/revenue extraction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(indian_ai_licensing_regime, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(indai_tr_t0, indian_ai_licensing_regime, theater_ratio, 0, 0.48).
narrative_ontology:measurement(indai_tr_t6, indian_ai_licensing_regime, theater_ratio, 6, 0.55).
narrative_ontology:measurement(indai_tr_t12, indian_ai_licensing_regime, theater_ratio, 12, 0.62).

% Extraction over time
narrative_ontology:measurement(indai_be_t0, indian_ai_licensing_regime, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(indai_be_t6, indian_ai_licensing_regime, base_extractiveness, 6, 0.47).
narrative_ontology:measurement(indai_be_t12, indian_ai_licensing_regime, base_extractiveness, 12, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(indian_ai_licensing_regime, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(indian_ai_licensing_regime, 0.35).
narrative_ontology:affects_constraint(indian_ai_licensing_regime, multinational_ai_market_access).
narrative_ontology:affects_constraint(indian_ai_licensing_regime, open_source_ai_development_diffusion).
narrative_ontology:affects_constraint(indian_ai_licensing_regime, data_sovereignty_architecture).
narrative_ontology:affects_constraint(indian_ai_licensing_regime, developer_migration_flows).

% DUAL FORMULATION NOTE:
% The Indian AI Licensing Regime can be decomposed into two structurally distinct constraints: (1) Data Localization Requirement (ε ≈ 0.35, primarily Rope — solves infrastructure coordination problem) and (2) Licensing/Surveillance Authority (ε ≈ 0.68, Snare — extraction from multinationals). These decompose because the extractiveness values differ significantly (2x difference) and they have different resolution mechanisms. Data localization could exist without licensing authority (infrastructure coordination without extraction); licensing authority could exist without localization (surveillance without spatial requirement). The 'AI for All' framing conflates them, but they are structurally separable. This story treats them as unified because they are implemented as one regime, but future analysis should consider whether the coordination function (localization) could be separated from the extraction function (licensing/surveillance).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(indian_ai_licensing_regime, powerful, 0.78).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
