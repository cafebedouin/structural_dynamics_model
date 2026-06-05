% ============================================================================
% CONSTRAINT STORY: personal_data_market_concentration
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_personal_data_market_concentration, []).

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
 *   constraint_id: personal_data_market_concentration
 *   human_readable: Personal Data Market Concentration and Asymmetric Extraction
 *   domain: digital_economy/data_rights/platform_governance
 *
 * SUMMARY:
 *   Personal data market concentration represents a structural constraint
 *   where dominant digital platforms (Google, Meta, Amazon, Apple, ByteDance)
 *   consolidate control over information flows necessary for economic and
 *   social participation. This constraint exhibits the full range of DR
 *   classifications depending on observer position and structural
 *   relationship to the extraction mechanism. From the perspective of data
 *   subjects who have no genuine exit option (all major social, commercial,
 *   and informational functions are platform-mediated), the constraint
 *   appears as a snare: high extraction, high suppression, no escape. From
 *   the perspective of competing firms facing asymmetric data access, it
 *   appears as snare: structural disadvantage blocking market entry. From the
 *   platform's own perspective, it appears as tangled rope: genuine
 *   coordination function (user data drives algorithmic relevance) alongside
 *   asymmetric extraction (data collection far exceeds user understanding or
 *   consent). From the regulatory perspective, it appears as tangled rope:
 *   coordination goals (privacy protection, transparency) alongside
 *   extraction mechanisms (compliance costs creating competitive moats,
 *   regulatory timelines mismatched to innovation speed). From the
 *   perspective of digitally-native populations, identity fusion with
 *   platforms creates a novel form of snare: structurally mobile but
 *   functionally trapped because exit would require abandoning
 *   identity-constitutive peer relationships. The constraint's theater ratio
 *   (0.55) reflects the performative consent mechanism (terms-of-service
 *   theater) maintained despite evidence that informed consent at scale is
 *   impossible. Rising suppression (0.58 → 0.72) models the intensifying
 *   enforcement mechanisms: algorithmic steering, account suspensions, API
 *   restrictions, exclusive agreements. Rising extractiveness (0.45 → 0.68)
 *   models the accumulation of data collection scope and the monetization
 *   efficiency of larger datasets. The scaffold perspective identifies data
 *   interoperability mandates (DMA, GDPR portability rights, federated
 *   standards) as structured exit pathways with sunset logic — if implemented
 *   effectively, they would lower switching costs and disrupt concentration.
 *   The mountain perspective risks naturalizing network effects as inevitable
 *   laws when they are actually outcomes of policy choices regarding data
 *   ownership, platform liability, and interoperability requirements.
 *
 * KEY AGENTS:
 *   - Data Subjects: Primary victims (powerless/trapped + identity_locked) — participation in economic and social life is mediated through platforms with no genuine alternatives; identity fusion with platforms deepens entrapment
 *   - Competing Firms: Secondary victims (moderate/constrained) — cannot match dominant platforms' data-driven personalization without equivalent user data access; high exit cost makes alternatives unreachable
 *   - Dominant Platforms: Primary beneficiaries (institutional/arbitrage) — capture asymmetric value from user data; coordinate genuine functions alongside extraction; active enforcement through terms-of-service and algorithmic control
 *   - Regulatory Agencies: Mixed actor (organized/constrained) — coordinate legitimate goals (privacy protection, transparency) but suffer from resource asymmetry, regulatory timelines mismatched to innovation, and revolving-door capture
 *   - Digitally-Native Population: Vulnerable subpopulation (powerless/identity_locked) — structurally mobile but functionally trapped through identity fusion; accept platform extraction as inevitable
 *   - Data Interoperability Coalition: Organized agents (organized/constrained) — EU regulators, open-standards bodies, competitive platforms creating alternative protocols (ActivityPub, DMA mandates, data portability) with sunset clauses
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent policy choices as inevitable laws of digital economies
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(personal_data_market_concentration, 0.68).
domain_priors:suppression_score(personal_data_market_concentration, 0.72).
domain_priors:theater_ratio(personal_data_market_concentration, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(personal_data_market_concentration, extractiveness, 0.68).
narrative_ontology:constraint_metric(personal_data_market_concentration, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(personal_data_market_concentration, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(personal_data_market_concentration, snare).
narrative_ontology:human_readable(personal_data_market_concentration, "Personal Data Market Concentration and Asymmetric Extraction").
narrative_ontology:topic_domain(personal_data_market_concentration, "digital_economy/data_rights/platform_governance").

domain_priors:requires_active_enforcement(personal_data_market_concentration).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(personal_data_market_concentration, dominant_platforms).
narrative_ontology:constraint_victim(personal_data_market_concentration, data_subjects).
narrative_ontology:constraint_victim(personal_data_market_concentration, competing_firms).
narrative_ontology:constraint_victim(personal_data_market_concentration, regulatory_capacity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DATA SUBJECT (SNARE) — Participation in economic and social life requires platform access. No genuine exit exists; all major coordination functions (employment search, social connection, financial services, location-based commerce) are platform-mediated. Suppression operates through both structural (no alternatives) and internalized (identity fusion with platform identity) mechanisms. Data extraction is the hidden cost of essential services. Maximum experienced extraction — trapped agent with no exit option.
constraint_indexing:constraint_classification(personal_data_market_concentration, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: COMPETING FIRM (SNARE) — Cannot match dominant platform's data-driven personalization without access to equivalent user data. High exit cost (replication of entire infrastructure, user base, trust) makes alternatives structurally unreachable. Asymmetric data access creates permanent competitive disadvantage. Suppression operates through technical (API restrictions, data access denials) and legal (terms-of-service, exclusive agreements) mechanisms. Effective extraction through reduced market share and acquisition pressure.
constraint_indexing:constraint_classification(personal_data_market_concentration, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: DOMINANT PLATFORM (TANGLED ROPE) — Genuinely coordinates user attention and data exchange: user behaviors generate real-time signals that drive algorithmic relevance; data aggregation produces network externalities. Genuine coordination function exists. SIMULTANEOUS extraction: data collection far exceeds what users consent to or understand; data monetization through advertising and targeting is asymmetrically favorable to the platform; control over user interface and ranking creates structural dependence. Active enforcement through terms-of-service, algorithmic steering, and account suspension. This is pure tangled rope: coordination + extraction + enforcement.
constraint_indexing:constraint_classification(personal_data_market_concentration, tangled_rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: REGULATORY COALITION (TANGLED ROPE) — GDPR, DMA, state privacy laws coordinate legitimate goals: preventing predatory data use, ensuring transparency, enabling user consent. Genuine coordination function. SIMULTANEOUS extraction: regulatory compliance costs disproportionately burden smaller competitors (compliance expense is fixed, scale-dependent), creating moats that protect dominant platforms; regulatory timelines (months to years) cannot match platform innovation speed; enforcement actions are reactive, not preventive. Active enforcement required to maintain regulatory structure itself. Constrained by resource limitations (regulatory budgets, technical expertise) and captured by regulatory revolving-door dynamics (personnel movement between platforms and agencies).
constraint_indexing:constraint_classification(personal_data_market_concentration, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: DATA-AS-COMMODITY NARRATIVE (PITON) — The framing that 'users exchange data for free services' persists as institutional theater despite structural evidence that users cannot meaningfully consent to data collection at scale, cannot calculate the asymmetry, and cannot exit. The consent mechanism is performative: terms-of-service acceptance is per-click theater with no material negotiation. The commodity fiction persists through regulatory inertia (data protection frameworks still assume informed consent) and platform insistence on legitimacy through transparency theater. High theater ratio (0.55 understates the performativity). The narrative maintains institutional authority despite functional degradation.
constraint_indexing:constraint_classification(personal_data_market_concentration, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: DIGITALLY-NATIVE POPULATION (SNARE + identity_locked) — For generation Z and younger cohorts, platform identity is not separable from social identity. Exiting a dominant social platform (Instagram, TikTok, Discord) is experienced as identity annihilation, not merely losing a service. Structurally mobile (could technically delete accounts) but functionally trapped because exit requires abandoning peer relationships, social coordination structures, and identity-constitutive practices. Suppression mechanism is internalized: the platform's extraction is accepted as inevitable ('the price of digital life'). This represents a novel form of entrapment: cognitive rather than material barriers.
constraint_indexing:constraint_classification(personal_data_market_concentration, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(identity_locked),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, network effects and economies of scale in data are treated as immutable laws of digital economies: larger platforms are inherently more valuable; data concentration is the inevitable equilibrium of free-service business models; exit is 'always available' to users who don't want to participate. This perspective risks naturalizing what is actually a contingent policy choice: data ownership allocation, platform liability rules, and interoperability requirements could be different. The false-summit detector will flag this as naturalized policy disguised as natural law.
constraint_indexing:constraint_classification(personal_data_market_concentration, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 8: DATA INTEROPERABILITY COALITION (SCAFFOLD) — Emerging frameworks (data portability mandates, federated standards, open protocols) represent structured exit pathways with sunset clauses. DMA interoperability requirements in EU, data portability rights in GDPR, emerging ActivityPub standards for social protocols create alternatives to locked-in ecosystems. Low theater because interoperability is mechanically testable. Extractiveness drops if interoperability succeeds because platforms lose unique access to proprietary user data and switching costs fall. Organized agents (EU regulators, open-standards bodies, competitive platforms) see this as a temporary coordination failure being solved by regulatory forcing. Estimated sunset: 5-10 years as open protocols mature and user data becomes portably accessible.
constraint_indexing:constraint_classification(personal_data_market_concentration, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(personal_data_market_concentration_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(personal_data_market_concentration, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(personal_data_market_concentration, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(personal_data_market_concentration, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(personal_data_market_concentration, TR),
    TR >= 0.70.

:- end_tests(personal_data_market_concentration_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The dominant platforms capture asymmetric value from user data through collection scope far exceeding user understanding (comprehensive behavioral tracking, shadow profiles, cross-device tracking), monetization through advertising targeting and algorithmic ranking, and structural dependence forcing continuation of platform use despite extraction. Measurement reflects: data collection expanding faster than transparency (theater_ratio rising), new data types (biometric, location, social graph) enabling novel targeting, and platform optimization toward maximum data collection. The 0.68 value (not higher) reflects that some users do benefit from platform services (free communication, content discovery, e-commerce access) — there is genuine coordination function occurring alongside extraction. Suppression (0.72): High. Structural barriers to exit are extensive: alternative platforms lack equivalent user bases (network effects), lack equivalent feature parity (years of development investment), lack equivalent data about users (creation barrier). Technical barriers: API access restrictions, data export format limitations, data-portability non-compliance. Legal barriers: exclusive agreements (Google-Apple), terms-of-service control, liability immunity (Section 230 in US). Psychological barriers: for digitally-native populations, identity fusion makes exit feel like identity annihilation. Theater ratio (0.55): Moderate-high. The consent mechanism (terms-of-service acceptance) is performative: accept-or-refuse binary offers no negotiation; users cannot understand scope of data collection; data monetization occurs without user visibility; the transparency theater (privacy dashboards, data access tools) provides illusion of control without meaningful power. The 0.55 reflects that not all platform operations are theater — some genuine coordination occurs — but the data extraction mechanism operates largely through consent theater rather than informed agreement. Rising trajectory (0.42 → 0.55) models increasing reliance on theatrical mechanisms as actual consent becomes harder to obtain (regulatory pressure, user awareness, media coverage).
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximum perspectival divergence. The dominant platform's tangled-rope classification (coordination + extraction) stands in direct opposition to the data subject's snare classification (pure extraction with no exit). The platform sees genuine coordination function: user behaviors generate real signals driving algorithmic relevance; data aggregation produces valuable insights that improve service utility; network effects create legitimate coordination value. The data subject sees only extraction: the platform collects data far beyond what is disclosed; monetization benefits only the platform; switching costs are prohibitively high; consent is theatrical. The regulatory perspective (tangled rope) attempts to harmonize these views through enforcement but itself becomes extractive: compliance costs create competitive moats protecting incumbents; regulatory timelines cannot match innovation speed. The digitally-native population (identity_locked) perceives the platform as structurally inescapable because their identity has been constituted through platform participation — even if switching costs dropped to zero, exit would require identity transformation they cannot imagine from within. The mountain perspective (natural law) risks collapsing this perspectival structure by naturalizing the concentration outcome as inevitable network-effect equilibrium, when the actual cause is contingent policy choices (data ownership, liability rules, interoperability blocking). The scaffold perspective (interoperability coalition) attempts to transform the constraint by lowering switching costs — shifting data subjects from trapped to mobile, competing firms from constrained to mobile, platforms from pure beneficiaries to partial beneficiaries. If successful, this would push multiple perspectives from snare toward tangled rope or rope.
 *
 * DIRECTIONALITY LOGIC:
 *   The dominant platform benefits from concentration, operates with minimal external constraints (regulatory penalties are small relative to revenue), and has arbitrage options at every decision point — classic beneficiary directionality yielding low d (0.15-0.20). Data subjects are victims of both extraction and cognitive capture through identity fusion; structurally mobile (could delete accounts) but functionally trapped; facing suppression both external (no alternatives) and internal (identity constitution) — victim directionality yielding high d (0.75-0.85). Competing firms are pure victims in this constraint: they benefit from the platform ecosystem's existence but are structurally excluded from the data access required to compete on equal footing — high d (0.80-0.85). Regulatory agencies occupy an unstable middle position: they have mandate and authority (moderate power) but face resource constraints and capture dynamics; they can issue regulations but cannot enforce them in real-time against platform innovation — d around 0.45-0.55. The directionality derivation should flow from these structural facts without needing overrides, provided beneficiary/victim declarations are accurate and exit_options are differentiated appropriately.
 *
 * MANDATROPHY ANALYSIS:
 *   SNARE WITH SECONDARY TANGLED-ROPE READING: The constraint resolves the mandatrophy through perspectival analysis. The snare classification from the data-subject perspective is accurate — trapped agents with no exit option facing asymmetric extraction with no coordination benefit. The tangled-rope classification from the platform perspective is also accurate — genuine coordination function exists (user data driving algorithmic relevance) alongside asymmetric extraction. These are not contradictory; they are different positions within the same constraint structure. The mandatrophy resolves through observing that the two perspectives are in contradiction because the distribution of benefits is asymmetric: the platform genuinely coordinates and extracts; the user genuinely benefits from coordination but bears disproportionate extraction cost. The synthesis is tangled rope at the systemic level (coordination + extraction + enforcement) with snare at the subject level (from the perspective of the powerless agent with no exit option). This is precisely what tangled rope describes: a constraint that would be rope (pure coordination) if distribution were symmetric, but becomes tangled rope because benefits and costs are asymmetrically distributed and enforcement is required to maintain this asymmetry. The high suppression (0.72) and high extractiveness (0.68) confirm snare characteristics; the presence of genuine coordination function (network effects, algorithmic relevance, data utility) confirms tangled rope rather than pure snare. The scaffold perspective offers a potential resolution: interoperability that converts trapped subjects to mobile subjects would shift the constraint from snare toward rope or light tangled rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    consent_measurement_impossibility,
    'Can users meaningfully calculate the economic value of their data and the asymmetry of exchange at scale?',
    'Randomized pricing experiments offering explicit payment for data collection; measurement of user understanding of data use patterns; comparison of stated willingness-to-accept vs. revealed preference',
    'If impossible at scale: consent mechanism is fundamentally degraded and the entire ''exchange'' framing is false. Classification remains snare. If possible with simplified interfaces: scaffold perspective becomes more plausible — clearer consent mechanisms could convert extraction to coordination.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(consent_measurement_impossibility, empirical, 'Whether informed data consent is feasible at platform scale').

omega_variable(
    interoperability_cost_structure,
    'What is the true cost of switching platforms for users and firms when data interoperability is implemented?',
    'Pre/post measurement of switching costs in regimes with and without data portability (EU vs. non-EU); measurement of account deletion and account creation rates; measurement of multi-homing behavior before/after interoperability implementation',
    'If switching costs remain high post-interoperability: scaffold sunset fails and constraint remains snare. If switching costs drop substantially: interoperability is effective and scaffold perspective is structurally sound.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interoperability_cost_structure, empirical, 'Whether data interoperability actually reduces platform switching costs').

omega_variable(
    network_effects_necessity,
    'Are network effects in social platforms inevitable laws of digital economies, or contingent outcomes of design choices and data access rules?',
    'Historical comparison: did early social platforms (Friendster, Myspace) exhibit network effects before data concentration? Do federated social protocols (Mastodon, Bluesky) show different equilibria? Can network effects be decoupled from data concentration?',
    'If inevitable laws: mountain perspective is correct and concentration is natural equilibrium. If contingent: mountain is false-summit and concentration is maintained by policy choices (data ownership, interoperability blocking, liability exemptions). Current evidence suggests contingency.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(network_effects_necessity, conceptual, 'Whether network effects are inevitable or contingent on institutional design').

omega_variable(
    regulatory_capacity_asymmetry,
    'Can regulatory agencies with annual budgets of millions match platform innovation cycles and data science capabilities costing billions?',
    'Comparative timeline analysis: regulatory investigation duration vs. platform product iteration speed; cost accounting for regulatory compliance vs. platform R&D spending; measurement of false-positive and false-negative rates in automated regulatory auditing',
    'If regulatory capacity remains structurally inadequate: tangled-rope regulatory perspective is accurate and represents real enforcement limits. If regulatory capacity can be enhanced: organized agents could potentially shift this to rope (pure coordination) rather than mixed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(regulatory_capacity_asymmetry, empirical, 'Whether regulatory agencies can match platform innovation speeds').

omega_variable(
    identity_lock_persistence,
    'For digitally-native populations, is identity fusion with platforms sufficiently strong to prevent exit even if switching costs drop to near-zero?',
    'Longitudinal interviews with Gen-Z users in high-interoperability regions; measurement of actual platform switching behavior when alternatives are objectively available; assessment of whether identity-reframing occurs when switching occurs',
    'If identity lock persists post-interoperability: even functionally mobile agents remain trapped. Snare classification from identity_locked perspective remains accurate. If identity lock dissolves with availability of alternatives: exit barriers are primarily material, not cognitive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_persistence, empirical, 'Whether identity fusion prevents platform exit even when switching costs drop').

omega_variable(
    data_extraction_quantity_ceiling,
    'Is there a diminishing-marginal-value point where additional data collection provides platforms no additional extraction benefit?',
    'Platform earnings analysis: correlation between user-data-collection-scope and platform advertising revenue; analysis of whether more comprehensive data collection produces measurably higher targeting precision; measurement of ROI on incremental data collection',
    'If ceiling exists: extraction could be capped by regulation below platform optimization point. If no ceiling: platforms structurally require total data collection for maximum extraction. Current evidence suggests weak ceiling but strong marginal incentives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(data_extraction_quantity_ceiling, empirical, 'Whether additional data collection provides diminishing returns to platforms').

omega_variable(
    false_summit_natural_law_claim,
    'Is network-effect inevitability a genuine natural law or a naturalized policy choice?',
    'Historical and comparative analysis: different regimes produced different equilibria (AT&T breakup produced innovation in telecom; EU data protection shifted platform strategies); identification of specific policy levers that could disrupt concentration; modeling of alternative institutional arrangements',
    'If naturalized policy: mountain classification is false summit. If genuine law: mountain is correct. Current evidence (DMA proving effective at forcing interoperability, federated protocols showing viability) suggests policy contingency.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_natural_law_claim, conceptual, 'Whether network-effect concentration is natural law or policy-contingent').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(personal_data_market_concentration, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pdmc_theater_t0, personal_data_market_concentration, theater_ratio, 0, 0.42).
narrative_ontology:measurement(pdmc_theater_t5, personal_data_market_concentration, theater_ratio, 5, 0.48).
narrative_ontology:measurement(pdmc_theater_t10, personal_data_market_concentration, theater_ratio, 10, 0.55).

% Extraction over time
narrative_ontology:measurement(pdmc_extract_t0, personal_data_market_concentration, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(pdmc_extract_t5, personal_data_market_concentration, base_extractiveness, 5, 0.58).
narrative_ontology:measurement(pdmc_extract_t10, personal_data_market_concentration, base_extractiveness, 10, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(pdmc_suppress_t0, personal_data_market_concentration, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(pdmc_suppress_t5, personal_data_market_concentration, suppression_requirement, 5, 0.68).
narrative_ontology:measurement(pdmc_suppress_t10, personal_data_market_concentration, suppression_requirement, 10, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(personal_data_market_concentration, resource_allocation).
narrative_ontology:boltzmann_floor_override(personal_data_market_concentration, 0.12).
narrative_ontology:affects_constraint(personal_data_market_concentration, algorithmic_opacity_in_targeting).
narrative_ontology:affects_constraint(personal_data_market_concentration, digital_attention_economy).
narrative_ontology:affects_constraint(personal_data_market_concentration, platform_liability_exemption).

% DUAL FORMULATION NOTE:
% Personal data market concentration is downstream of three structurally distinct constraints: (1) algorithmic opacity (users cannot understand or verify targeting logic), (2) attention-economy extractiveness (user time is the commodity being traded for platform access), and (3) liability immunity (platforms face no direct liability for data misuse). Each has distinct ε and classification. Data concentration affects all three. In particular, interoperability mandates (scaffold perspective) address concentration directly but leave opacity and liability immunity in place — addressing concentration alone will not fully resolve the extraction mechanism.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(personal_data_market_concentration, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
